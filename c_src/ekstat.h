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

#if !(__STDC_VERSION__ >= 199901L || defined(__GNUC__))
# undef  DEBUG
# define DEBUG		0
# define DPRINTF	(void)	/* Vararg macros may be unsupported */
#elif DEBUG
#include <stdio.h>
#include <stdarg.h>
#define DPRINTF(fmt, ...)								\
	do {										\
		fprintf(stderr, "%s:%d " fmt "\n", __FILE__, __LINE__, __VA_ARGS__);    \
		fflush(stderr);								\
	} while(0)
#define DPUTS(arg)		DPRINTF("%s", arg)
#else
#define DPRINTF(fmt, ...)	((void) 0)
#define DPUTS(arg)		((void) 0)
#endif

typedef struct ekstat_priv_data_s {
	void			*async_nif_priv;	// Note: must be first element in struct
} ekstat_priv_data_t;

typedef struct ekstat_s {
	ekstat_context_t	*context;
	ErlNifRWLock		*rwlock;
} ekstat_t;

static int	ekstat_read_prepare(ekstat_folder_t *folder);
static int	ekstat_read_fold(ekstat_folder_t *folder, ks_nvpair_t *nvpair);
static int	ekstat_read_finalize(ekstat_folder_t *folder, ks_instance_t *ksi);

static int	parse_selector_pattern(ErlNifEnv *env, ERL_NIF_TERM arg, ks_pattern_t *pattern);

#endif /* _EKSTAT_H */
