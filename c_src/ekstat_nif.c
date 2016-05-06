// -*- mode: c; tab-width: 8; indent-tabs-mode: 1; st-rulers: [70] -*-
// vim: ts=8 sw=8 ft=c noet
/*
 * CDDL HEADER START
 *
 * The contents of this file are subject to the terms of the
 * Common Development and Distribution License (the "License").
 * You may not use this file except in compliance with the License.
 *
 * You can obtain a copy of the license at usr/src/OPENSOLARIS.LICENSE
 * or http://www.opensolaris.org/os/licensing.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL HEADER in each
 * file and include the License file at usr/src/OPENSOLARIS.LICENSE.
 * If applicable, add the following below this CDDL HEADER, with the
 * fields enclosed by brackets "[]" replaced with your own identifying
 * information: Portions Copyright [yyyy] [name of copyright owner]
 *
 * CDDL HEADER END
 */

/*
 * Copyright (c) 1999, 2010, Oracle and/or its affiliates. All rights reserved.
 * Copyright (c) 2013 David Hoeppner. All rights reserved.
 * Copyright 2013 Nexenta Systems, Inc.  All rights reserved.
 * Copyright (c) 2013, Joyent, Inc. All rights reserved.
 * Copyright (c) 2014, Pagoda Box, Inc. All rights reserved.
 */

#include "ekstat.h"
#include "async_nif.h"

ErlNifResourceType	*ekstat_RESOURCE;

/* Global init for async_nif. */
ASYNC_NIF_INIT(ekstat);

/* Atoms (initialized in on_load) */
static ERL_NIF_TERM	ATOM_CLOSED;
static ERL_NIF_TERM	ATOM_REGERROR;

#define CHECK(expr, label)								\
	if (EKSTAT_OK != (ret = (expr))) {						\
		DPRINTF("CHECK(\"%s\") failed \"%s\" at %s:%d in %s()\n",		\
			#expr, ekstat_strerror(ret), __FILE__, __LINE__, __func__);	\
		err = __strerror_term(env, ret);					\
		goto label;								\
	}

#define FAIL_ERR(e, label)					\
	do {							\
		err = __strerror_term(env, (e));		\
		goto label;					\
	} while(0)

/**
 * Convenience function to generate {error, {errno, Reason}}
 * Erlang terms to return to callers.
 *
 * env    NIF environment
 * err    code of last error
 */
static ERL_NIF_TERM
__strerror_term(ErlNifEnv *env, int err)
{
	/* We return the errno value as well as the message here because the
	   error message provided by strerror() for differ across platforms
	   and/or may be localized to any given language (i18n).  Use the errno
	   atom rather than the message when matching in Erlang.  You've been
	   warned. */
	return enif_make_tuple2(
		env,
		ATOM_ERROR,
		enif_make_tuple2(
			env,
			enif_make_atom(env, erl_errno_id(err)),
			enif_make_string(env, ekstat_strerror(err), ERL_NIF_LATIN1)
		)
	);
}

/**
 * Opens a kstat control structure.
 *
 */
ASYNC_NIF_DECL(
	ekstat_open,
	{ // struct

	},
	{ // pre
		UNUSED(argv);
	},
	{ // work
		kstat_ctl_t	*control;
		ekstat_t	*handle;
		ERL_NIF_TERM	term;
		int		ret;
		ERL_NIF_TERM	err;

		while ((control = kstat_open()) == NULL) {
			if (errno == EAGAIN) {
				(void) poll(NULL, 0, 200);
			} else {
				FAIL_ERR(errno, err1);
			}
		}

		handle = (ekstat_t *)(enif_alloc_resource(ekstat_RESOURCE, sizeof(*handle)));
		handle->rwlock = enif_rwlock_create("ekstat_handle");
		(void) enif_rwlock_rwlock(handle->rwlock);
		handle->reader = new_kstat_reader(control);
		CHECK(load_kstat_reader(handle->reader, NULL), err2);
		term = enif_make_resource(env, handle);
		(void) enif_release_resource(handle);
		(void) enif_rwlock_rwunlock(handle->rwlock);

		ASYNC_NIF_REPLY(enif_make_tuple(env, 2, ATOM_OK, term));
		return;

	err2:
		(void) enif_rwlock_rwunlock(handle->rwlock);
	err1:
		ASYNC_NIF_REPLY(err);
		return;
	},
	{ // post

	}
);

/**
 * Closes a kstat control structure.
 *
 * argv[0]	reference to the kstat handle resource
 */
ASYNC_NIF_DECL(
	ekstat_close,
	{ // struct
		ekstat_t	*handle;
	},
	{ // pre
		if (argc != 1) {
			ASYNC_NIF_RETURN_BADARG();
		}
		if (!enif_get_resource(env, argv[0], ekstat_RESOURCE, (void **)(&args->handle))) {
			ASYNC_NIF_RETURN_BADARG();
		}
		if (!args->handle->reader) {
			ASYNC_NIF_RETURN_BADARG();
		}
		(void) enif_keep_resource((void *)(args->handle));
	},
	{ // work
		if (!args->handle->rwlock) {
			ASYNC_NIF_REPLY(enif_make_tuple(env, 2, ATOM_ERROR, ATOM_CLOSED));
			return;
		}
		(void) enif_rwlock_rwlock(args->handle->rwlock);
		(void) kstat_close(args->handle->reader->control);
		args->handle->reader->control = NULL;
		(void) free_kstat_reader(args->handle->reader);
		args->handle->reader = NULL;
		(void) enif_rwlock_rwunlock(args->handle->rwlock);
		(void) enif_rwlock_destroy(args->handle->rwlock);
		args->handle->rwlock = NULL;

		ASYNC_NIF_REPLY(ATOM_OK);
		return;
	},
	{ // post
		(void) enif_release_resource((void *)(args->handle));
	}
);

/**
 * Clear a kstat control structure's statistics.
 *
 * argv[0]	reference to the kstat handle resource
 */
ASYNC_NIF_DECL(
	ekstat_clear,
	{ // struct
		ekstat_t	*handle;
		unsigned int	cleared;
	},
	{ // pre
		if (argc != 1) {
			ASYNC_NIF_RETURN_BADARG();
		}
		if (!enif_get_resource(env, argv[0], ekstat_RESOURCE, (void **)(&args->handle))) {
			ASYNC_NIF_RETURN_BADARG();
		}
		if (!args->handle->reader) {
			ASYNC_NIF_RETURN_BADARG();
		}
		args->cleared = 0;
		(void) enif_keep_resource((void *)(args->handle));
	},
	{ // work
		int		ret;
		ERL_NIF_TERM	err;

		if (!args->handle->rwlock) {
			ASYNC_NIF_REPLY(enif_make_tuple(env, 2, ATOM_ERROR, ATOM_CLOSED));
			return;
		}
		(void) enif_rwlock_rwlock(args->handle->rwlock);
		CHECK(clear_kstat_reader(args->handle->reader, &args->cleared), err1);
		(void) enif_rwlock_rwunlock(args->handle->rwlock);

		ASYNC_NIF_REPLY(enif_make_tuple(env, 2, ATOM_OK, enif_make_uint(env, args->cleared)));
		return;

	err1:
		(void) enif_rwlock_rwunlock(args->handle->rwlock);
		ASYNC_NIF_REPLY(err);
		return;
	},
	{ // post
		(void) enif_release_resource((void *)(args->handle));
	}
);

/**
 * Update a kstat control structure's statistics.
 *
 * argv[0]	reference to the kstat handle resource
 */
ASYNC_NIF_DECL(
	ekstat_update,
	{ // struct
		ekstat_t	*handle;
		unsigned int	loaded;
	},
	{ // pre
		if (argc != 1) {
			ASYNC_NIF_RETURN_BADARG();
		}
		if (!enif_get_resource(env, argv[0], ekstat_RESOURCE, (void **)(&args->handle))) {
			ASYNC_NIF_RETURN_BADARG();
		}
		if (!args->handle->reader) {
			ASYNC_NIF_RETURN_BADARG();
		}
		args->loaded = 0;
		(void) enif_keep_resource((void *)(args->handle));
	},
	{ // work
		int		ret;
		ERL_NIF_TERM	err;

		if (!args->handle->rwlock) {
			ASYNC_NIF_REPLY(enif_make_tuple(env, 2, ATOM_ERROR, ATOM_CLOSED));
			return;
		}
		(void) enif_rwlock_rwlock(args->handle->rwlock);
		CHECK(update_kstat_reader(args->handle->reader), err1);
		CHECK(clear_kstat_reader(args->handle->reader, NULL), err1);
		CHECK(load_kstat_reader(args->handle->reader, &args->loaded), err1);
		(void) enif_rwlock_rwunlock(args->handle->rwlock);

		ASYNC_NIF_REPLY(enif_make_tuple(env, 2, ATOM_OK, enif_make_uint(env, args->loaded)));
		return;

	err1:
		(void) enif_rwlock_rwunlock(args->handle->rwlock);
		ASYNC_NIF_REPLY(err);
		return;
	},
	{ // post
		(void) enif_release_resource((void *)(args->handle));
	}
);

/**
 * Read a kstat control structure's cached statistics.
 *
 * argv[0]	reference to the kstat handle resource
 * argv[1]	Class
 * argv[2]	Module
 * argv[3]	Instance
 * argv[4]	Name
 * argv[5]	Statistic
 */
ASYNC_NIF_DECL(
	ekstat_read,
	{ // struct
		ekstat_t	*handle;
		char		*ks_class;
		char		*ks_module;
		char		*ks_instance;
		char		*ks_name;
		char		*ks_statistic;
	},
	{ // pre
		int		ret;
		ERL_NIF_TERM	err;

		if (argc < 1 || argc > 6) {
			ASYNC_NIF_RETURN_BADARG();
		}
		if (!enif_get_resource(env, argv[0], ekstat_RESOURCE, (void **)(&args->handle))) {
			ASYNC_NIF_RETURN_BADARG();
		}
		if (!args->handle->reader) {
			ASYNC_NIF_RETURN_BADARG();
		}

		args->ks_class = NULL;
		args->ks_module = NULL;
		args->ks_instance = NULL;
		args->ks_name = NULL;
		args->ks_statistic = NULL;

		if (argc > 1) {
			CHECK(pattern_term_to_string(env, argv[1], &args->ks_class), err1);
		}
		if (argc > 2) {
			CHECK(pattern_term_to_string(env, argv[2], &args->ks_module), err2);
		}
		if (argc > 3) {
			CHECK(pattern_term_to_string(env, argv[3], &args->ks_instance), err3);
		}
		if (argc > 4) {
			CHECK(pattern_term_to_string(env, argv[4], &args->ks_name), err4);
		}
		if (argc > 5) {
			CHECK(pattern_term_to_string(env, argv[5], &args->ks_statistic), err5);
		}

		goto work1;

	err5:
		if (args->ks_statistic) {
			(void) free(args->ks_statistic);
		}
	err4:
		if (args->ks_name) {
			(void) free(args->ks_name);
		}
	err3:
		if (args->ks_instance) {
			(void) free(args->ks_instance);
		}
	err2:
		if (args->ks_module) {
			(void) free(args->ks_module);
		}
	err1:
		if (args->ks_class) {
			(void) free(args->ks_class);
		}
		return err;
	work1:
		(void) enif_keep_resource((void *)(args->handle));
	},
	{ // work
		char		*errbuf;
		kstat_folder_t	*folder;
		ekstat_acc_t	*acc;
		int		ret;
		ERL_NIF_TERM	err;
		ERL_NIF_TERM	term;

		if (!args->handle->rwlock) {
			ASYNC_NIF_REPLY(enif_make_tuple(env, 2, ATOM_ERROR, ATOM_CLOSED));
			return;
		}

		(void) enif_rwlock_rlock(args->handle->rwlock);

		errbuf = NULL;

		folder = (kstat_folder_t *)(malloc(sizeof (kstat_folder_t)));
		if (folder == NULL) {
			if (errno) {
				FAIL_ERR(errno, workerr1);
			}
			FAIL_ERR(ENOMEM, workerr1);
		}
		folder->selector = new_kstat_reader_selector(&errbuf, args->ks_class, args->ks_module, args->ks_instance, args->ks_name, args->ks_statistic);
		if (folder->selector == NULL) {
			if (errbuf) {
				err = enif_make_tuple2(
					env,
					ATOM_ERROR,
					enif_make_tuple2(
						env,
						ATOM_REGERROR,
						enif_make_string(env, errbuf, ERL_NIF_LATIN1)
					)
				);
				goto workerr2;
			}
			if (errno) {
				FAIL_ERR(errno, workerr2);
			}
			FAIL_ERR(ENOMEM, workerr2);
		}
		acc = (ekstat_acc_t *)(malloc(sizeof (ekstat_acc_t)));
		if (acc == NULL) {
			if (errno) {
				FAIL_ERR(errno, workerr3);
			}
			FAIL_ERR(ENOMEM, workerr3);
		}
		acc->list = enif_make_list(env, 0);
		acc->ksi = NULL;
		folder->data = (void *)(env);
		folder->fold = ekstat_folder;
		CHECK(fold_kstat_reader(args->handle->reader, folder, (void **)(&acc)), workerr4);
		term = acc->list;
		(void) free(acc);
		acc = NULL;
		folder->data = NULL;
		(void) free_kstat_reader_selector(folder->selector);
		folder->selector = NULL;
		(void) free(folder);
		folder = NULL;
		(void) enif_rwlock_runlock(args->handle->rwlock);

		ASYNC_NIF_REPLY(term);
		return;

	workerr4:
		(void) free(acc);
		acc = NULL;
		folder->data = NULL;
	workerr3:
		(void) free_kstat_reader_selector(folder->selector);
		folder->selector = NULL;
	workerr2:
		(void) free(folder);
		folder = NULL;
	workerr1:
		(void) enif_rwlock_runlock(args->handle->rwlock);
		ASYNC_NIF_REPLY(err);
		return;
	},
	{ // post
		(void) enif_release_resource((void *)(args->handle));
	}
);

static void
ekstat_dtor_nif(ErlNifEnv *env, void *obj)
{
	ekstat_t	*handle;

	handle = (ekstat_t *)(obj);
	if (handle == NULL) {
		return;
	}
	if (handle->rwlock) {
		(void) enif_rwlock_rwlock(handle->rwlock);
	}
	if (handle->reader) {
		if (handle->reader->control) {
			(void) kstat_close(handle->reader->control);
			handle->reader->control = NULL;
		}
		(void) free_kstat_reader(handle->reader);
		handle->reader = NULL;
	}
	if (handle->rwlock) {
		(void) enif_rwlock_rwunlock(handle->rwlock);
		(void) enif_rwlock_destroy(handle->rwlock);
		handle->rwlock = NULL;
	}
}

static int
ekstat_load_nif(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
	ErlNifResourceFlags	flags;
	ekstat_priv_data_t	*priv;

	UNUSED(load_info);

	priv = enif_alloc(sizeof(*priv));
	if (!priv) {
		return (ENOMEM);
	}
	(void) memset(priv, 0, sizeof(*priv));

	/* Note: !!! the first element of our priv_data struct *must* be the
	   pointer to the async_nif's private data which we set here. */
	ASYNC_NIF_LOAD(ekstat, env, priv->async_nif_priv);
	if (!priv) {
		return (ENOMEM);
	}
	*priv_data = priv;

	ATOM_CLOSED = enif_make_atom(env, "closed");
	ATOM_REGERROR = enif_make_atom(env, "regerror");

	flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
	ekstat_RESOURCE = enif_open_resource_type(
		env,
		NULL,			// module_str: MUST be NULL
		"ekstat_resource",	// name
		ekstat_dtor_nif,	// dtor
		flags,
		NULL
	);

	return (0);
}

static int
ekstat_reload_nif(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM info)
{
	UNUSED(env);
	UNUSED(priv_data);
	UNUSED(info);
	return (0); // TODO: implement
}

static int
ekstat_upgrade_nif(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
	UNUSED(priv_data);
	UNUSED(old_priv_data);
	UNUSED(load_info);
	ASYNC_NIF_UPGRADE(ekstat, env); // TODO: implement
	return 0;
}

static void
ekstat_unload_nif(ErlNifEnv *env, void *priv_data)
{
	ekstat_priv_data_t	*priv;

	priv = (ekstat_priv_data_t *)priv_data;
	ASYNC_NIF_UNLOAD(ekstat, env, priv->async_nif_priv);
	(void) enif_free(priv);
	return;
}

static ErlNifFunc nif_funcs [] = {
	{"open_nif",	1,	ekstat_open},
	{"close_nif",	2,	ekstat_close},
	{"clear_nif",	2,	ekstat_clear},
	{"update_nif",	2,	ekstat_update},
	{"read_nif",	2,	ekstat_read},
	{"read_nif",	3,	ekstat_read},
	{"read_nif",	4,	ekstat_read},
	{"read_nif",	5,	ekstat_read},
	{"read_nif",	6,	ekstat_read},
	{"read_nif",	7,	ekstat_read}
};

/* driver entry point */
ERL_NIF_INIT(
	ekstat,
	nif_funcs,
	& ekstat_load_nif,
	& ekstat_reload_nif,
	& ekstat_upgrade_nif,
	& ekstat_unload_nif
)
