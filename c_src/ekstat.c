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

		while ((control = kstat_open()) == NULL) {
			if (errno == EAGAIN) {
				(void) poll(NULL, 0, 200);
			} else {
				(void) printf("ERROR\n");
				return;
			}
		}

		handle = (ekstat_t *)(enif_alloc_resource(ekstat_RESOURCE, sizeof(*handle)));
		handle->rwlock = enif_rwlock_create("ekstat_handle");
		(void) enif_rwlock_rwlock(handle->rwlock);
		handle->context = new_ekstat_context(control);
		term = enif_make_resource(env, handle);
		(void) enif_release_resource(handle);
		(void) enif_rwlock_rwunlock(handle->rwlock);

		ASYNC_NIF_REPLY(enif_make_tuple(env, 2, ATOM_OK, term));
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
		if (!args->handle->context) {
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
		(void) kstat_close(args->handle->context->control);
		args->handle->context->control = NULL;
		(void) free_ekstat_context(args->handle->context);
		args->handle->context = NULL;
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
		if (!args->handle->context) {
			ASYNC_NIF_RETURN_BADARG();
		}
		args->cleared = 0;
		(void) enif_keep_resource((void *)(args->handle));
	},
	{ // work
		if (!args->handle->rwlock) {
			ASYNC_NIF_REPLY(enif_make_tuple(env, 2, ATOM_ERROR, ATOM_CLOSED));
			return;
		}
		(void) enif_rwlock_rwlock(args->handle->rwlock);
		args->cleared = clear_ekstat_context(args->handle->context);
		(void) enif_rwlock_rwunlock(args->handle->rwlock);

		ASYNC_NIF_REPLY(enif_make_tuple(env, 2, ATOM_OK, enif_make_uint(env, args->cleared)));
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
		unsigned int	updated;
	},
	{ // pre
		if (argc != 1) {
			ASYNC_NIF_RETURN_BADARG();
		}
		if (!enif_get_resource(env, argv[0], ekstat_RESOURCE, (void **)(&args->handle))) {
			ASYNC_NIF_RETURN_BADARG();
		}
		if (!args->handle->context) {
			ASYNC_NIF_RETURN_BADARG();
		}
		args->updated = 0;
		(void) enif_keep_resource((void *)(args->handle));
	},
	{ // work
		if (!args->handle->rwlock) {
			ASYNC_NIF_REPLY(enif_make_tuple(env, 2, ATOM_ERROR, ATOM_CLOSED));
			return;
		}
		(void) enif_rwlock_rwlock(args->handle->rwlock);
		(void) clear_ekstat_context(args->handle->context);
		args->updated = load_ekstat_context(args->handle->context);
		(void) enif_rwlock_rwunlock(args->handle->rwlock);

		ASYNC_NIF_REPLY(enif_make_tuple(env, 2, ATOM_OK, enif_make_uint(env, args->updated)));
		return;
	},
	{ // post
		(void) enif_release_resource((void *)(args->handle));
	}
);

typedef struct ekstat_term_s {
	ERL_NIF_TERM	term;
} ekstat_term_t;

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
		ks_selector_t	*selector;
	},
	{ // pre
		int		position;
		int		result;
		ks_pattern_t	*pattern;

		if (argc < 1 || argc > 6) {
			ASYNC_NIF_RETURN_BADARG();
		}
		if (!enif_get_resource(env, argv[0], ekstat_RESOURCE, (void **)(&args->handle))) {
			ASYNC_NIF_RETURN_BADARG();
		}
		if (!args->handle->context) {
			ASYNC_NIF_RETURN_BADARG();
		}

		args->selector = new_ks_selector(args->handle->context);
		if (args->selector == NULL) {
			ASYNC_NIF_RETURN_BADARG();
		}

		position = 1;
		while (argc > position) {
			switch (position) {
			case 1:
				pattern = &args->selector->ks_class;
				break;
			case 2:
				pattern = &args->selector->ks_module;
				break;
			case 3:
				pattern = &args->selector->ks_instance;
				break;
			case 4:
				pattern = &args->selector->ks_name;
				break;
			case 5:
				pattern = &args->selector->ks_statistic;
				break;
			default:
				ASYNC_NIF_RETURN_BADARG();
			}

			result = parse_selector_pattern(env, argv[position], pattern);
			if (result != EKSTAT_OK) {
				(void) free_ks_selector(args->selector);
				if (result == EKSTAT_ENOMEM) {
					return enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM);
				} else {
					ASYNC_NIF_RETURN_BADARG();
				}
			}
			position++;
		}
		(void) enif_keep_resource((void *)(args->handle));
	},
	{ // work
		ekstat_folder_t	folder;
		ekstat_term_t	*list;
		int		result;

		list = (ekstat_term_t *)(malloc(sizeof(*list)));
		if (list == NULL) {
			ASYNC_NIF_REPLY(enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM));
			return;
		}

		if (!args->handle->rwlock) {
			ASYNC_NIF_REPLY(enif_make_tuple(env, 2, ATOM_ERROR, ATOM_CLOSED));
			return;
		}
		(void) enif_rwlock_rlock(args->handle->rwlock);
		list->term = enif_make_list(env, 0);
		folder.env = (void *)(env);
		folder.selector = args->selector;
		folder.accumulator = (void *)(list);
		folder.data = NULL;
		folder.prepare = ekstat_read_prepare;
		folder.fold = ekstat_read_fold;
		folder.finalize = ekstat_read_finalize;
		result = fold_ekstat_context(args->handle->context, &folder);
		(void) enif_rwlock_runlock(args->handle->rwlock);

		if (result == EKSTAT_OK) {
			ASYNC_NIF_REPLY(enif_make_tuple(env, 2, ATOM_OK, list->term));
		} else {
			if (result == EKSTAT_ENOMEM) {
				ASYNC_NIF_REPLY(enif_make_tuple2(env, ATOM_ERROR, ATOM_ENOMEM));
			} else {
				ASYNC_NIF_REPLY(ATOM_ERROR);
			}
		}

		(void) free(list);
		return;
	},
	{ // post
		(void) free_ks_selector(args->selector);
		(void) enif_release_resource((void *)(args->handle));
	}
);

static int
ekstat_read_prepare(ekstat_folder_t *folder)
{
	ErlNifEnv	*env;
	ekstat_term_t	*statistics;

	env = (ErlNifEnv *)(folder->env);
	statistics = (ekstat_term_t *)(malloc(sizeof(*statistics)));
	if (statistics == NULL) {
		return (EKSTAT_ENOMEM);
	}
	statistics->term = enif_make_list(env, 0);
	folder->data = (void *)(statistics);
	return (EKSTAT_OK);
}

static int
ekstat_read_fold(ekstat_folder_t *folder, ks_nvpair_t *nvpair)
{
	ErlNifEnv	*env;
	ekstat_term_t	*statistics;
	ERL_NIF_TERM	term;
	ERL_NIF_TERM	key;
	ERL_NIF_TERM	value;

	env = (ErlNifEnv *)(folder->env);
	statistics = (ekstat_term_t *)(folder->data);
	value = 0;

	switch (nvpair->data_type) {
	case KSTAT_DATA_CHAR:
		value = enif_make_string(env, nvpair->value.c, ERL_NIF_LATIN1);
		break;
	case KSTAT_DATA_INT32:
		value = enif_make_int(env, nvpair->value.i32);
		break;
	case KSTAT_DATA_UINT32:
		value = enif_make_uint(env, nvpair->value.ui32);
		break;
	case KSTAT_DATA_INT64:
		value = enif_make_int64(env, nvpair->value.i64);
		break;
	case KSTAT_DATA_UINT64:
		value = enif_make_uint64(env, nvpair->value.ui64);
		break;
	case KSTAT_DATA_STRING:
		value = enif_make_string(env, KSTAT_NAMED_STR_PTR(nvpair), ERL_NIF_LATIN1);
		break;
	case KSTAT_DATA_HRTIME:
		if (nvpair->value.ui64 == 0) {
			value = enif_make_int(env, 0);
		} else {
			value = enif_make_double(env, nvpair->value.ui64 / 1000000000.0);
		}
		break;
	default:
		assert(B_FALSE);
	}

	key = enif_make_string(env, nvpair->name, ERL_NIF_LATIN1);
	term = enif_make_list_cell(env, enif_make_tuple(env, 2, key, value), statistics->term);
	statistics->term = term;

	return (EKSTAT_OK);
}

static int
ekstat_read_finalize(ekstat_folder_t *folder, ks_instance_t *ksi)
{
	ErlNifEnv	*env;
	ekstat_term_t	*statistics;
	ekstat_term_t	*list;
	ERL_NIF_TERM	instance;
	ERL_NIF_TERM	term;

	env = (ErlNifEnv *)(folder->env);
	statistics = (ekstat_term_t *)(folder->data);
	list = (ekstat_term_t *)(folder->accumulator);
	instance = enif_make_tuple(
		env,
		5,
		enif_make_string(env, ksi->ks_class, ERL_NIF_LATIN1),
		enif_make_string(env, ksi->ks_module, ERL_NIF_LATIN1),
		enif_make_int(env, ksi->ks_instance),
		enif_make_string(env, ksi->ks_name, ERL_NIF_LATIN1),
		statistics->term
	);

	term = enif_make_list_cell(env, instance, list->term);
	list->term = term;

	(void) free(statistics);
	folder->data = NULL;

	return (EKSTAT_OK);
}

static int
parse_selector_pattern(ErlNifEnv *env, ERL_NIF_TERM arg, ks_pattern_t *pattern)
{
	if (enif_is_atom(env, arg)) {
		unsigned	size;
		char		*atom;
		int		len;

		(void) enif_get_atom_length(env, arg, &size, ERL_NIF_LATIN1);
		atom = (char *)(malloc(sizeof (char) * (size + 1)));
		if (atom == NULL) {
			return (EKSTAT_ENOMEM);
		}
		len = enif_get_atom(env, arg, atom, size + 1, ERL_NIF_LATIN1);
		if (!len) {
			(void) free(atom);
			return (EKSTAT_BADARG);
		}
		if (strncmp(atom, "_", len) != 0) {
			(void) free(atom);
			return (EKSTAT_BADARG);
		}
		pattern->pstr = "*";
		pattern->needs_free = B_FALSE;
		(void) free(atom);
		return (EKSTAT_OK);
	} else if (enif_is_list(env, arg)) {
		unsigned	size;
		char		*string;
		int		len;

		(void) enif_get_list_length(env, arg, &size);
		string = (char *)(malloc(sizeof (char) * (size + 1)));
		if (string == NULL) {
			return (EKSTAT_ENOMEM);
		}
		len = enif_get_string(env, arg, string, size + 1, ERL_NIF_LATIN1);
		if (!len) {
			(void) free(string);
			return (EKSTAT_BADARG);
		}
		pattern->pstr = string;
		pattern->needs_free = B_TRUE;
		return (EKSTAT_OK);
	} else if (enif_is_number(env, arg)) {
		ErlNifSInt64	integer;
		char		*string;

		if (!enif_get_int64(env, arg, &integer)) {
			return (EKSTAT_BADARG);
		}
		(void) asprintf(&string, "%d", integer);
		if (string == NULL) {
			return (EKSTAT_ENOMEM);
		}
		pattern->pstr = string;
		pattern->needs_free = B_TRUE;
		return (EKSTAT_OK);
	} else {
		return (EKSTAT_BADARG);
	}
}

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
	if (handle->context) {
		if (handle->context->control) {
			(void) kstat_close(handle->context->control);
			handle->context->control = NULL;
		}
		(void) free_ekstat_context(handle->context);
		handle->context = NULL;
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
ERL_NIF_INIT(ekstat,
	nif_funcs,
	& ekstat_load_nif,
	& ekstat_reload_nif,
	& ekstat_upgrade_nif,
	& ekstat_unload_nif)
