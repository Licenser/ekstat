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

static int	ekstat_folder_finalize(kstat_folder_t *folder, void **accumulator);
static int	ekstat_folder_prepare(kstat_folder_t *folder, ks_instance_t *ksi, void **accumulator);
static int	ekstat_folder_fold(kstat_folder_t *folder, ks_instance_t *ksi, ks_nvpair_t *nvpair, void **accumulator);

static int	pattern_atom_to_string(ErlNifEnv *env, ERL_NIF_TERM arg, char **pstr);
static int	pattern_list_to_string(ErlNifEnv *env, ERL_NIF_TERM arg, char **pstr);
static int	pattern_number_to_string(ErlNifEnv *env, ERL_NIF_TERM arg, char **pstr);

char *
ekstat_strerror(int errnum)
{
	return (strerror(errnum));
}

static int
ekstat_folder_finalize(kstat_folder_t *folder, void **accumulator)
{
	ErlNifEnv	*env;
	ekstat_acc_t	*acc;
	ERL_NIF_TERM	instance;
	ERL_NIF_TERM	term;

	env = (ErlNifEnv *)(folder->data);
	acc = (ekstat_acc_t *)(*accumulator);
	instance = enif_make_tuple(
		env,
		5,
		enif_make_string(env, acc->ksi->ks_class, ERL_NIF_LATIN1),
		enif_make_string(env, acc->ksi->ks_module, ERL_NIF_LATIN1),
		enif_make_int(env, acc->ksi->ks_instance),
		enif_make_string(env, acc->ksi->ks_name, ERL_NIF_LATIN1),
		acc->statistics
	);
	term = enif_make_list_cell(env, instance, acc->list);
	acc->list = term;
	acc->ksi = NULL;

	return (EKSTAT_OK);
}

static int
ekstat_folder_prepare(kstat_folder_t *folder, ks_instance_t *ksi, void **accumulator)
{
	ErlNifEnv	*env;
	ekstat_acc_t	*acc;

	env = (ErlNifEnv *)(folder->data);
	acc = (ekstat_acc_t *)(*accumulator);
	acc->statistics = enif_make_list(env, 0);
	acc->ksi = ksi;

	return (EKSTAT_OK);
}

static int
ekstat_folder_fold(kstat_folder_t *folder, ks_instance_t *ksi, ks_nvpair_t *nvpair, void **accumulator)
{
	ErlNifEnv	*env;
	ekstat_acc_t	*acc;
	ERL_NIF_TERM	key;
	ERL_NIF_TERM	value;
	ERL_NIF_TERM	term;

	env = (ErlNifEnv *)(folder->data);
	acc = (ekstat_acc_t *)(*accumulator);

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
	term = enif_make_list_cell(env, enif_make_tuple(env, 2, key, value), acc->statistics);
	acc->statistics = term;

	return (EKSTAT_OK);
}

int
ekstat_folder(kstat_folder_t *folder, ks_instance_t *ksi, ks_nvpair_t *nvpair, void **accumulator)
{
	/* finalize */
	if (ksi == NULL && nvpair == NULL) {
		return (ekstat_folder_finalize(folder, accumulator));
	}

	/* prepare */
	if (nvpair == NULL) {
		return (ekstat_folder_prepare(folder, ksi, accumulator));
	}

	/* fold */
	return (ekstat_folder_fold(folder, ksi, nvpair, accumulator));
}

static int
pattern_atom_to_string(ErlNifEnv *env, ERL_NIF_TERM arg, char **pstr)
{
	unsigned	size;
	char		*atom;
	int		len;

	(void) enif_get_atom_length(env, arg, &size, ERL_NIF_LATIN1);
	atom = (char *)(malloc(sizeof (char) * (size + 1)));
	if (atom == NULL) {
		if (errno) {
			return (errno);
		}
		return (ENOMEM);
	}
	len = enif_get_atom(env, arg, atom, size + 1, ERL_NIF_LATIN1);
	if (!len) {
		(void) free(atom);
		return (-1);
	}
	if (strncmp(atom, "_", len) != 0) {
		(void) free(atom);
		return (-1);
	}
	*pstr = NULL;
	(void) free(atom);
	return (EKSTAT_OK);
}

static int
pattern_list_to_string(ErlNifEnv *env, ERL_NIF_TERM arg, char **pstr)
{
	unsigned	size;
	char		*string;
	int		len;

	(void) enif_get_list_length(env, arg, &size);
	string = (char *)(malloc(sizeof (char) * (size + 1)));
	if (string == NULL) {
		if (errno) {
			return (errno);
		}
		return (ENOMEM);
	}
	len = enif_get_string(env, arg, string, size + 1, ERL_NIF_LATIN1);
	if (!len) {
		(void) free(string);
		return (-1);
	}
	*pstr = string;
	return (EKSTAT_OK);
}

static int
pattern_number_to_string(ErlNifEnv *env, ERL_NIF_TERM arg, char **pstr)
{
	ErlNifSInt64	integer;
	char		*string;

	if (!enif_get_int64(env, arg, &integer)) {
		return (-1);
	}
	(void) asprintf(&string, "%d", integer);
	if (string == NULL) {
		if (errno) {
			return (errno);
		}
		return (ENOMEM);
	}
	*pstr = string;
	return (EKSTAT_OK);
}

int
pattern_term_to_string(ErlNifEnv *env, ERL_NIF_TERM arg, char **pstr)
{
	if (enif_is_atom(env, arg)) {
		return (pattern_atom_to_string(env, arg, pstr));
	} else if (enif_is_list(env, arg)) {
		return (pattern_list_to_string(env, arg, pstr));
	} else if (enif_is_number(env, arg)) {
		return (pattern_number_to_string(env, arg, pstr));
	}
	return (-1);
}
