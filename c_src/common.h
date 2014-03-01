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

#ifndef _COMMON_H
#define	_COMMON_H

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <kstat.h>
#include <langinfo.h>
#include <libgen.h>
#include <limits.h>
#include <locale.h>
#include <signal.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <time.h>
#include <unistd.h>
#include <sys/list.h>
#include <sys/time.h>
#include <sys/types.h>
#include <regex.h>

#define EKSTAT_BADARG	-1
#define EKSTAT_OK	0
#define EKSTAT_ENOMEM	ENOMEM

#define	KSTAT_DATA_HRTIME	(KSTAT_DATA_STRING + 1)

typedef union ks_value {
	char		c[16];
	int32_t		i32;
	uint32_t	ui32;
	struct {
		union {
			char	*ptr;
			char	__pad[8];
		} addr;
		uint32_t	len;
	} str;

	int64_t		i64;
	uint64_t	ui64;
} ks_value_t;

typedef struct ekstat_context_s {
	kstat_ctl_t	*control;
	list_t		cache;
	int		last_error;
	char		*last_error_string;
} ekstat_context_t;

typedef struct ks_instance {
	list_node_t		ks_next;
	char			ks_name[KSTAT_STRLEN];
	char			ks_module[KSTAT_STRLEN];
	char			ks_class[KSTAT_STRLEN];
	int			ks_instance;
	uchar_t			ks_type;
	hrtime_t		ks_snaptime;
	list_t			ks_nvlist;
	ekstat_context_t	*context;
} ks_instance_t;

typedef struct ks_nvpair {
	list_node_t	nv_next;
	char		name[KSTAT_STRLEN];
	uchar_t		data_type;
	ks_value_t	value;
} ks_nvpair_t;

typedef struct ks_pattern {
	boolean_t		needs_free;
	char			*pstr;
	regex_t			preg;
	ekstat_context_t	*context;
} ks_pattern_t;

typedef struct ks_selector {
	ks_pattern_t	ks_class;
	ks_pattern_t	ks_module;
	ks_pattern_t	ks_instance;
	ks_pattern_t	ks_name;
	ks_pattern_t	ks_statistic;
} ks_selector_t;

/* Typedef for raw kstat reader functions */
typedef void	(*kstat_raw_reader_t)(kstat_t *, ks_instance_t *);

typedef struct ekstat_folder_s	ekstat_folder_t;

/* Typedefs for fold functions */
typedef int	(*ekstat_prepare_cb)(ekstat_folder_t *folder);
typedef int	(*ekstat_fold_cb)(ekstat_folder_t *folder, ks_nvpair_t *nvpair);
typedef int	(*ekstat_finalize_cb)(ekstat_folder_t *folder, ks_instance_t *ksi);

struct ekstat_folder_s {
	void			*env;
	ks_selector_t		*selector;
	void			*accumulator;
	void			*data;
	ekstat_prepare_cb	prepare;
	ekstat_fold_cb		fold;
	ekstat_finalize_cb	finalize;
};

extern ekstat_context_t		*new_ekstat_context(kstat_ctl_t *control);
extern int			clear_ekstat_context(ekstat_context_t *context);
extern int			load_ekstat_context(ekstat_context_t *context);
extern int			fold_ekstat_context(ekstat_context_t *context, ekstat_folder_t *folder);
extern void			free_ekstat_context(ekstat_context_t *context);

extern ks_selector_t		*new_ks_selector(ekstat_context_t *context);
extern void			free_ks_selector(ks_selector_t *selector);

#endif /* _COMMON_H */
