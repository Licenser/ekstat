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

#ifndef _KSTAT_READER_H
#define	_KSTAT_READER_H

#ifdef KSR_INTERNAL
/*
 * Structures needed by the kstat reader functions.
 */
#include <sys/var.h>
#include <sys/utsname.h>
#include <sys/sysinfo.h>
#include <sys/flock.h>
#include <sys/dnlc.h>
#include <regex.h>
#include <nfs/nfs.h>
#include <nfs/nfs_clnt.h>

#ifdef __sparc
#include <vm/hat_sfmmu.h>
#include <sys/simmstat.h>
#include <sys/sysctrl.h>
#include <sys/fhc.h>
#endif

#include "common.h"

#endif
#define	KSTAT_DATA_HRTIME	(KSTAT_DATA_STRING + 1)
#define KSR_OK			0

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

#ifdef KSR_INTERNAL

#define nvpair_insert(I, N, V, T)					\
	if (KSR_OK != (ret = (ksr_nvpair_insert(I, N, V, T)))) {	\
		return (ret);						\
	}

#define	SAVE_HRTIME(I, S, N)				\
{							\
	ks_value_t v;					\
	v.ui64 = S->N;					\
	nvpair_insert(I, #N, &v, KSTAT_DATA_UINT64);	\
}

#define	SAVE_INT32(I, S, N)				\
{							\
	ks_value_t v;					\
	v.i32 = S->N;					\
	nvpair_insert(I, #N, &v, KSTAT_DATA_INT32);	\
}

#define	SAVE_UINT32(I, S, N)				\
{							\
	ks_value_t v;					\
	v.ui32 = S->N;					\
	nvpair_insert(I, #N, &v, KSTAT_DATA_UINT32);	\
}

#define	SAVE_INT64(I, S, N)             		\
{							\
	ks_value_t v;					\
	v.i64 = S->N;					\
	nvpair_insert(I, #N, &v, KSTAT_DATA_INT64);	\
}

#define	SAVE_UINT64(I, S, N)				\
{							\
	ks_value_t v;					\
	v.ui64 = S->N;					\
	nvpair_insert(I, #N, &v, KSTAT_DATA_UINT64);	\
}

/*
 * We dont want const "strings" because we free
 * the instances later.
 */
#define	SAVE_STRING(I, S, N)				\
{							\
	ks_value_t v;					\
	v.str.addr.ptr = ksr_safe_strdup(S->N);		\
	v.str.len = strlen(S->N);			\
	nvpair_insert(I, #N, &v, KSTAT_DATA_STRING);	\
}

#define	SAVE_HRTIME_X(I, N, V)				\
{							\
	ks_value_t v;					\
	v.ui64 = V;					\
	nvpair_insert(I, N, &v, KSTAT_DATA_HRTIME);	\
}

#define	SAVE_INT32_X(I, N, V)				\
{							\
	ks_value_t v;					\
	v.i32 = V;					\
	nvpair_insert(I, N, &v, KSTAT_DATA_INT32);	\
}

#define	SAVE_UINT32_X(I, N, V)				\
{							\
	ks_value_t v;					\
	v.ui32 = V;					\
	nvpair_insert(I, N, &v, KSTAT_DATA_UINT32);	\
}

#define	SAVE_UINT64_X(I, N, V)				\
{							\
	ks_value_t v;					\
	v.ui64 = V;					\
	nvpair_insert(I, N, &v, KSTAT_DATA_UINT64);	\
}

#define	SAVE_STRING_X(I, N, V)				\
{							\
	ks_value_t v;					\
	v.str.addr.ptr = ksr_safe_strdup(V);		\
	v.str.len = (V) ? strlen(V) : 0;		\
	nvpair_insert(I, N, &v, KSTAT_DATA_STRING);	\
}

#define	SAVE_CHAR_X(I, N, V)				\
{							\
	ks_value_t v;					\
	(void) asprintf(&v.str.addr.ptr, "%c", V);	\
	v.str.len = 1;					\
	nvpair_insert(I, N, &v, KSTAT_DATA_STRING);	\
}
#endif

typedef struct ks_instance {
	list_node_t	ks_next;
	char		ks_name[KSTAT_STRLEN];
	char		ks_module[KSTAT_STRLEN];
	char		ks_class[KSTAT_STRLEN];
	int		ks_instance;
	uchar_t		ks_type;
	hrtime_t	ks_snaptime;
	list_t		ks_nvlist;
} ks_instance_t;

typedef struct ks_nvpair {
	list_node_t	nv_next;
	char		name[KSTAT_STRLEN];
	uchar_t		data_type;
	ks_value_t	value;
} ks_nvpair_t;

typedef struct ks_pattern {
	char		*pstr;
	regex_t		*preg;
} ks_pattern_t;

typedef struct ks_selector {
	ks_pattern_t	*class;
	ks_pattern_t	*module;
	ks_pattern_t	*instance;
	ks_pattern_t	*name;
	ks_pattern_t	*statistic;
} ks_selector_t;

#ifdef KSR_INTERNAL
static int		ksr_compare_instances(ks_instance_t *, ks_instance_t *);
static int		ksr_nvpair_insert(ks_instance_t *, char *, ks_value_t *, uchar_t);
static boolean_t	ksr_match(const char *, ks_pattern_t *);
static ks_pattern_t	*ksr_new_pattern(char **errbuf, char *pstr);
static ks_selector_t	*ksr_new_selector(char **errbuf, char *class, char *module, char *instance, char *name, char *statistic);
static void		ksr_free_pattern(ks_pattern_t *pattern);
static void		ksr_free_selector(ks_selector_t *selector);
static char		*ksr_safe_strdup(char *str);

/* Raw kstat readers */
static int	ksr_save_cpu_stat(kstat_t *, ks_instance_t *);
static int	ksr_save_var(kstat_t *, ks_instance_t *);
static int	ksr_save_ncstats(kstat_t *, ks_instance_t *);
static int	ksr_save_sysinfo(kstat_t *, ks_instance_t *);
static int	ksr_save_vminfo(kstat_t *, ks_instance_t *);
static int	ksr_save_nfs(kstat_t *, ks_instance_t *);
#ifdef __sparc
static int	ksr_save_sfmmu_global_stat(kstat_t *, ks_instance_t *);
static int	ksr_save_sfmmu_tsbsize_stat(kstat_t *, ks_instance_t *);
static int	ksr_save_simmstat(kstat_t *, ks_instance_t *);
/* Helper function for save_temperature() */
static char	*ksr_short_array_to_string(short *, int);
static int	ksr_save_temperature(kstat_t *, ks_instance_t *);
static int	ksr_save_temp_over(kstat_t *, ks_instance_t *);
static int	ksr_save_ps_shadow(kstat_t *, ks_instance_t *);
static int	ksr_save_fault_list(kstat_t *, ks_instance_t *);
#endif

/* Named kstat readers */
static int	ksr_save_named(kstat_t *, ks_instance_t *);
static int	ksr_save_intr(kstat_t *, ks_instance_t *);
static int	ksr_save_io(kstat_t *, ks_instance_t *);
static int	ksr_save_timer(kstat_t *, ks_instance_t *);

/* Extra named kstat readers */
static int	ksr_save_hrtimes(kstat_t *kp, ks_instance_t *ksi);
static int	ksr_save_error(int errnum, ks_instance_t *ksi);

/* Typedef for raw kstat reader functions */
typedef int	(*kstat_raw_reader_t)(kstat_t *, ks_instance_t *);

static struct {
	kstat_raw_reader_t fn;
	char *name;
} ks_raw_lookup[] = {
	/* Function name		kstat name		*/
	{ksr_save_cpu_stat,		"cpu_stat:cpu_stat"},
	{ksr_save_var,			"unix:var"},
	{ksr_save_ncstats,		"unix:ncstats"},
	{ksr_save_sysinfo,		"unix:sysinfo"},
	{ksr_save_vminfo,		"unix:vminfo"},
	{ksr_save_nfs,			"nfs:mntinfo"},
#ifdef __sparc
	{ksr_save_sfmmu_global_stat,	"unix:sfmmu_global_stat"},
	{ksr_save_sfmmu_tsbsize_stat,	"unix:sfmmu_tsbsize_stat"},
	{ksr_save_simmstat,		"unix:simm-status"},
	{ksr_save_temperature,		"unix:temperature"},
	{ksr_save_temp_over,		"unix:temperature override"},
	{ksr_save_ps_shadow,		"unix:ps_shadow"},
	{ksr_save_fault_list,		"unix:fault_list"},
#endif
	{NULL, NULL},
};

static kstat_raw_reader_t	ksr_lookup_raw_kstat_fn(char *module, char *name);
#endif

/* Public */
typedef struct kstat_reader_s {
	kid_t		kid;
	kstat_ctl_t	*control;
	list_t		cache;
} kstat_reader_t;

typedef struct kstat_folder_s	kstat_folder_t;

/* Typedefs for fold functions */
typedef int	(*kstat_fold_cb)(kstat_folder_t *folder, ks_instance_t *ksi, ks_nvpair_t *nvpair, void **accumulator);

struct kstat_folder_s {
	ks_selector_t	*selector;
	kstat_fold_cb	fold;
	void		*data;
};

extern kstat_reader_t	*new_kstat_reader(kstat_ctl_t *control);
extern int		clear_kstat_reader(kstat_reader_t *reader, unsigned int *count);
extern int		update_kstat_reader(kstat_reader_t *reader);
extern int		load_kstat_reader(kstat_reader_t *reader, unsigned int *count);
extern int		fold_kstat_reader(kstat_reader_t *reader, kstat_folder_t *folder, void **accumulator);
extern void		free_kstat_reader(kstat_reader_t *reader);
extern ks_selector_t	*new_kstat_reader_selector(char **errbuf, char *class, char *module, char *instance, char *name, char *statistic);
extern void		free_kstat_reader_selector(ks_selector_t *selector);

#endif /* _KSTAT_READER_H */
