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
 * Copyright 2006 Sun Microsystems, Inc.  All rights reserved.
 * Copyright 2013 David Hoeppner.  All rights reserved.
 * Copyright 2013 Nexenta Systems, Inc.  All rights reserved.
 * Copyright 2014 Pagoda Box, Inc.  All rights reserved.
 */

#ifndef _EKSTAT_H
#define	_EKSTAT_H

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <errno.h>
#include <sys/param.h>
#include <poll.h>
#include <erl_nif.h>
#include <erl_driver.h>

#include "common.h"
#include "kstat_reader.h"

#define EKSTAT_OK	0

typedef struct ekstat_priv_data_s {
	void		*async_nif_priv;	// Note: must be first element in struct
} ekstat_priv_data_t;

typedef struct ekstat_s {
	kstat_reader_t	*reader;
	ErlNifRWLock	*rwlock;
} ekstat_t;

typedef struct ekstat_acc_s {
	ERL_NIF_TERM	list;
	ERL_NIF_TERM	statistics;
	ks_instance_t	*ksi;
} ekstat_acc_t;

extern char	*ekstat_strerror(int errnum);
extern int	ekstat_folder(kstat_folder_t *folder, ks_instance_t *ksi, ks_nvpair_t *nvpair, void **accumulator);
extern int	pattern_term_to_string(ErlNifEnv *env, ERL_NIF_TERM arg, char **pstr);

#endif /* _EKSTAT_H */
