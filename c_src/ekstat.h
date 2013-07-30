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
 * Copyright 2013 Pagoda Box, Inc.  All rights reserved.
 */

#ifndef _STAT_EKSTAT_H
#define	_STAT_EKSTAT_H

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

#include "erl_nif.h"

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

#define	SAVE_HRTIME(R, I, S, N)				\
{							\
	ks_value_t v;					\
	v.ui64 = S->N;					\
	nvpair_insert(R, I, #N, &v, KSTAT_DATA_UINT64);	\
}

#define	SAVE_INT32(R, I, S, N)				\
{							\
	ks_value_t v;					\
	v.i32 = S->N;					\
	nvpair_insert(R, I, #N, &v, KSTAT_DATA_INT32);	\
}

#define	SAVE_UINT32(R, I, S, N)				\
{							\
	ks_value_t v;					\
	v.ui32 = S->N;					\
	nvpair_insert(R, I, #N, &v, KSTAT_DATA_UINT32);	\
}

#define	SAVE_INT64(R, I, S, N)             		\
{							\
	ks_value_t v;					\
	v.i64 = S->N;					\
	nvpair_insert(R, I, #N, &v, KSTAT_DATA_INT64);	\
}

#define	SAVE_UINT64(R, I, S, N)				\
{							\
	ks_value_t v;					\
	v.ui64 = S->N;					\
	nvpair_insert(R, I, #N, &v, KSTAT_DATA_UINT64);	\
}

/*
 * We dont want const "strings" because we free
 * the instances later.
 */
#define	SAVE_STRING(R, I, S, N)				\
{							\
	ks_value_t v;					\
	v.str.addr.ptr = ks_safe_strdup(R, S->N);		\
	v.str.len = strlen(S->N);			\
	nvpair_insert(R, I, #N, &v, KSTAT_DATA_STRING);	\
}

#define	SAVE_HRTIME_X(R, I, N, V)			\
{							\
	ks_value_t v;					\
	v.ui64 = V;					\
	nvpair_insert(R, I, N, &v, KSTAT_DATA_HRTIME);	\
}

#define	SAVE_INT32_X(R, I, N, V)			\
{							\
	ks_value_t v;					\
	v.i32 = V;					\
	nvpair_insert(R, I, N, &v, KSTAT_DATA_INT32);	\
}

#define	SAVE_UINT32_X(R, I, N, V)			\
{							\
	ks_value_t v;					\
	v.ui32 = V;					\
	nvpair_insert(R, I, N, &v, KSTAT_DATA_UINT32);	\
}

#define	SAVE_UINT64_X(R, I, N, V)			\
{							\
	ks_value_t v;					\
	v.ui64 = V;					\
	nvpair_insert(R, I, N, &v, KSTAT_DATA_UINT64);	\
}

#define	SAVE_STRING_X(R, I, N, V)			\
{							\
	ks_value_t v;					\
	v.str.addr.ptr = ks_safe_strdup(R, V);		\
	v.str.len = (V) ? strlen(V) : 0;		\
	nvpair_insert(R, I, N, &v, KSTAT_DATA_STRING);	\
}

#define	SAVE_CHAR_X(R, I, N, V)				\
{							\
	ks_value_t v;					\
	(void) asprintf(&v.str.addr.ptr, "%c", V);	\
	v.str.len = 1;					\
	nvpair_insert(R, I, N, &v, KSTAT_DATA_STRING);	\
}

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
	regex_t		preg;
} ks_pattern_t;

typedef struct ks_selector {
	ks_pattern_t	ks_class;
	ks_pattern_t	ks_module;
	ks_pattern_t	ks_instance;
	ks_pattern_t	ks_name;
	ks_pattern_t	ks_statistic;
} ks_selector_t;

typedef struct ks_returner {
	ErlNifEnv	*env;
	boolean_t	ready;
	ERL_NIF_TERM	term;
} ks_returner_t;

static void	nvpair_insert(ks_returner_t *, ks_instance_t *, char *, ks_value_t *, uchar_t);
static int	compare_instances(ks_instance_t *, ks_instance_t *);
static char	*ks_safe_strdup(ks_returner_t *, char *);
static boolean_t	ks_match(ks_returner_t *, const char *, ks_pattern_t *);
static ks_selector_t	*new_selector(ks_returner_t *);

/* Raw kstat readers */
static void	save_cpu_stat(ks_returner_t *, kstat_t *, ks_instance_t *);
static void	save_var(ks_returner_t *, kstat_t *, ks_instance_t *);
static void	save_ncstats(ks_returner_t *, kstat_t *, ks_instance_t *);
static void	save_sysinfo(ks_returner_t *, kstat_t *, ks_instance_t *);
static void	save_vminfo(ks_returner_t *, kstat_t *, ks_instance_t *);
static void	save_nfs(ks_returner_t *, kstat_t *, ks_instance_t *);
#ifdef __sparc
static void	save_sfmmu_global_stat(ks_returner_t *, kstat_t *, ks_instance_t *);
static void	save_sfmmu_tsbsize_stat(ks_returner_t *, kstat_t *, ks_instance_t *);
static void	save_simmstat(ks_returner_t *, kstat_t *, ks_instance_t *);
/* Helper function for save_temperature() */
static char	*short_array_to_string(short *, int);
static void	save_temperature(ks_returner_t *, kstat_t *, ks_instance_t *);
static void	save_temp_over(ks_returner_t *, kstat_t *, ks_instance_t *);
static void	save_ps_shadow(ks_returner_t *, kstat_t *, ks_instance_t *);
static void	save_fault_list(ks_returner_t *, kstat_t *, ks_instance_t *);
#endif

/* Named kstat readers */
static void	save_named(ks_returner_t *, kstat_t *, ks_instance_t *);
static void	save_intr(ks_returner_t *, kstat_t *, ks_instance_t *);
static void	save_io(ks_returner_t *, kstat_t *, ks_instance_t *);
static void	save_timer(ks_returner_t *, kstat_t *, ks_instance_t *);

/* Typedef for raw kstat reader functions */
typedef void	(*kstat_raw_reader_t)(ks_returner_t *, kstat_t *, ks_instance_t *);

static struct {
	kstat_raw_reader_t fn;
	char *name;
} ks_raw_lookup[] = {
	/* Function name		kstat name		*/
	{save_cpu_stat,			"cpu_stat:cpu_stat"},
	{save_var,			"unix:var"},
	{save_ncstats,			"unix:ncstats"},
	{save_sysinfo,			"unix:sysinfo"},
	{save_vminfo,			"unix:vminfo"},
	{save_nfs,			"nfs:mntinfo"},
#ifdef __sparc
	{save_sfmmu_global_stat,	"unix:sfmmu_global_stat"},
	{save_sfmmu_tsbsize_stat,	"unix:sfmmu_tsbsize_stat"},
	{save_simmstat,			"unix:simm-status"},
	{save_temperature,		"unix:temperature"},
	{save_temp_over,		"unix:temperature override"},
	{save_ps_shadow,		"unix:ps_shadow"},
	{save_fault_list,		"unix:fault_list"},
#endif
	{NULL, NULL},
};

static kstat_raw_reader_t	lookup_raw_kstat_fn(char *, char *);

static ERL_NIF_TERM	ks_nvpair_term(ks_returner_t *, ks_nvpair_t *);
static ERL_NIF_TERM	ks_instance_term(ks_returner_t *, ks_instance_t *, ERL_NIF_TERM);

/*
 * Erlang NIF functions
 */
typedef struct ks_handle_t {
	kid_t		ks_id;
	kstat_ctl_t	*ks_ctl;
	/* Sorted list of kstat instances */
	list_t		instances_list;
} ks_handle_t;

#define	EKSTAT_ATOM(A)			enif_make_atom(ret->env, A)
#define EKSTAT_DOUBLE(A)		enif_make_double(ret->env, A)
#define EKSTAT_INT(A)			enif_make_int(ret->env, A)
#define EKSTAT_INT64(A)			enif_make_int64(ret->env, A)
#define EKSTAT_STRING(A)		enif_make_string(ret->env, A, ERL_NIF_LATIN1)
#define	EKSTAT_TUPLE2(A, B)		enif_make_tuple2(ret->env, A, B)
#define	EKSTAT_TUPLE3(A, B, C)		enif_make_tuple3(ret->env, A, B, C)
#define	EKSTAT_TUPLE4(A, B, C, D)	enif_make_tuple4(ret->env, A, B, C, D)
#define	EKSTAT_TUPLE5(A, B, C, D, E)	enif_make_tuple5(ret->env, A, B, C, D, E)
#define EKSTAT_UINT(A)			enif_make_uint(ret->env, A)
#define EKSTAT_UINT64(A)		enif_make_uint64(ret->env, A)

#define EKSTAT_ERROR(A)			EKSTAT_TUPLE2(EKSTAT_ATOM("error"), EKSTAT_STRING(A))
#define EKSTAT_OK(A)			EKSTAT_TUPLE2(EKSTAT_ATOM("ok"), A)
#define EKSTAT_RETURN(A)		ks_returner_term(ret, A)

static void	handle_dtor(ErlNifEnv *, void *);
static int	load(ErlNifEnv *, void **, ERL_NIF_TERM);

static ERL_NIF_TERM	open_nif(ErlNifEnv *, int, const ERL_NIF_TERM[]);
static ERL_NIF_TERM	update_nif(ErlNifEnv *, int, const ERL_NIF_TERM[]);
static ERL_NIF_TERM	read_nif(ErlNifEnv *, int, const ERL_NIF_TERM[]);

static ErlNifFunc	nif_funcs[] = {
	{"open", 0, open_nif},
	{"update", 1, update_nif},
	{"read", 1, read_nif},
	{"read", 2, read_nif},
	{"read", 3, read_nif},
	{"read", 4, read_nif},
	{"read", 5, read_nif},
	{"read", 6, read_nif}
};

#endif /* _STAT_EKSTAT_H */
