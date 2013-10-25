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
 * Copyright (c) 2013, Pagoda Box, Inc. All rights reserved.
 */

/*
 * Display kernel statistics
 *
 * This is a reimplementation of the perl kstat command originally found
 * under usr/src/cmd/kstat/kstat.pl
 *
 * Incompatibilities:
 *	- perl regular expressions replaced with extended REs bracketed by '/'
 *
 * Flags added:
 *	-C	similar to the -p option but value is separated by a colon
 *	-h	display help
 *	-j	json format
 */

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

#include "ekstat.h"

/*
 * Inserts an instance in the per selector list.
 */
static void
nvpair_insert(ks_returner_t *ret, ks_instance_t *ksi, char *name, ks_value_t *value, uchar_t data_type)
{
	ks_nvpair_t *instance;
	ks_nvpair_t *tmp;

	if (ret->ready != B_TRUE) {
		instance = (ks_nvpair_t *)(malloc(sizeof (ks_nvpair_t)));
		if (instance == NULL) {
			ret->term = EKSTAT_ERROR("ks_nvpair_t malloc");
			ret->ready = B_TRUE;
		} else {
			(void) strlcpy(instance->name, name, KSTAT_STRLEN);
			(void) memcpy(&instance->value, value, sizeof (ks_value_t));
			instance->data_type = data_type;

			tmp = list_head(&ksi->ks_nvlist);
			while (tmp != NULL && strcasecmp(instance->name, tmp->name) < 0)
			tmp = list_next(&ksi->ks_nvlist, tmp);

			list_insert_before(&ksi->ks_nvlist, tmp, instance);
		}
	}
}

/*
 * Sort compare function.
 */
static int
compare_instances(ks_instance_t *l_arg, ks_instance_t *r_arg)
{
	int	cval;
	int	rval;

	cval = strcasecmp(l_arg->ks_class, r_arg->ks_class);
	if (cval == 0) {
		rval = strcasecmp(l_arg->ks_module, r_arg->ks_module);
		if (rval == 0) {
			if (l_arg->ks_instance == r_arg->ks_instance) {
				return (strcasecmp(l_arg->ks_name, r_arg->ks_name));
			} else if (l_arg->ks_instance < r_arg->ks_instance) {
				return (-1);
			} else {
				return (1);
			}
		} else {
			return (rval);
		}
	} else {
		return (cval);
	}
}

static char *
ks_safe_strdup(ks_returner_t *ret, char *str)
{
	char	*dup;

	if (str == NULL) {
		return (NULL);
	}

	while ((dup = strdup(str)) == NULL) {
		if (errno == EAGAIN) {
			(void) poll(NULL, 0, 200);
		} else {
			ret->term = EKSTAT_ERROR("strdup");
			ret->ready = B_TRUE;
			return (NULL);
		}
	}

	return (dup);
}

/*
 * Allocates a new all-matching selector.
 */
static ks_selector_t *
new_selector(ks_returner_t *ret)
{
	ks_selector_t	*selector;

	selector = (ks_selector_t *)malloc(sizeof (ks_selector_t));
	if (selector == NULL) {
		ret->term = EKSTAT_ERROR("ks_selector_t malloc");
		ret->ready = B_TRUE;
		return (void *)(NULL);
	}

	selector->ks_class.pstr = "*";
	selector->ks_module.pstr = "*";
	selector->ks_instance.pstr = "*";
	selector->ks_name.pstr = "*";
	selector->ks_statistic.pstr = "*";

	selector->ks_class.free = B_FALSE;
	selector->ks_module.free = B_FALSE;
	selector->ks_instance.free = B_FALSE;
	selector->ks_name.free = B_FALSE;
	selector->ks_statistic.free = B_FALSE;

	return (selector);
}

static void
free_pattern(ks_pattern_t *pattern)
{
	if (pattern == NULL) {
		return;
	}
	if (pattern->pstr != NULL) {
		if (pattern->free == B_TRUE) {
			free(pattern->pstr);
		}
		pattern->pstr = NULL;
	}
	(void) regfree(&pattern->preg);
}

static void
free_selector(ks_selector_t *selector)
{
	(void) free_pattern(&selector->ks_class);
	(void) free_pattern(&selector->ks_module);
	(void) free_pattern(&selector->ks_instance);
	(void) free_pattern(&selector->ks_name);
	(void) free_pattern(&selector->ks_statistic);
	free(selector);
}

/*
 * This function was taken from the perl kstat module code - please
 * see for further comments there.
 */
static kstat_raw_reader_t
lookup_raw_kstat_fn(char *module, char *name)
{
	char		key[KSTAT_STRLEN * 2];
	register char 	*f, *t;
	int		n = 0;

	for (f = module, t = key; *f != '\0'; f++, t++) {
		while (*f != '\0' && isdigit(*f))
			f++;
		*t = *f;
	}
	*t++ = ':';

	for (f = name; *f != '\0'; f++, t++) {
		while (*f != '\0' && isdigit(*f))
			f++;
		*t = *f;
	}
	*t = '\0';

	while (ks_raw_lookup[n].fn != NULL) {
		if (strncmp(ks_raw_lookup[n].name, key, strlen(key)) == 0)
			return (ks_raw_lookup[n].fn);
		n++;
	}

	return (0);
}

/*
 * Match a string against a shell glob or extended regular expression.
 */
static boolean_t
ks_match(ks_returner_t *ret, const char *str, ks_pattern_t *pattern)
{
	int	regcode;
	char	*regstr;
	char	*errbuf;
	size_t	bufsz;

	if (ret->ready == B_TRUE) {
		return B_FALSE;
	}

	if (pattern->pstr != NULL && gmatch(pattern->pstr, "/*/") != 0) {
		/* All regex patterns are strdup'd copies */
		regstr = pattern->pstr + 1;
		*(strrchr(regstr, '/')) = '\0';

		regcode = regcomp(&pattern->preg, regstr,
		    REG_EXTENDED | REG_NOSUB);
		if (regcode != 0) {
			bufsz = regerror(regcode, NULL, NULL, 0);
			if (bufsz != 0) {
				errbuf = malloc(bufsz);
				if (errbuf == NULL) {
					ret->term = EKSTAT_ERROR("regex buffer malloc");
					ret->ready = B_TRUE;
					free(pattern->pstr);
					pattern->pstr = NULL;
					return B_FALSE;
				}
				(void) regerror(regcode, NULL, errbuf, bufsz);
				ret->term = EKSTAT_ERROR(errbuf);
				ret->ready = B_TRUE;
				free(errbuf);
			}
			free(pattern->pstr);
			pattern->pstr = NULL;
			return B_FALSE;
		}

		free(pattern->pstr);
		pattern->pstr = NULL;
	}

	if (pattern->pstr == NULL) {
		return (regexec(&pattern->preg, str, 0, NULL, 0) == 0);
	}

	return ((gmatch(str, pattern->pstr) != 0));
}

static void
save_cpu_stat(ks_returner_t *ret, kstat_t *kp, ks_instance_t *ksi)
{
	cpu_stat_t	*stat;
	cpu_sysinfo_t	*sysinfo;
	cpu_syswait_t	*syswait;
	cpu_vminfo_t	*vminfo;

	stat = (cpu_stat_t *)(kp->ks_data);
	sysinfo = &stat->cpu_sysinfo;
	syswait = &stat->cpu_syswait;
	vminfo  = &stat->cpu_vminfo;

	SAVE_UINT32_X(ret, ksi, "idle", sysinfo->cpu[CPU_IDLE]);
	SAVE_UINT32_X(ret, ksi, "user", sysinfo->cpu[CPU_USER]);
	SAVE_UINT32_X(ret, ksi, "kernel", sysinfo->cpu[CPU_KERNEL]);
	SAVE_UINT32_X(ret, ksi, "wait", sysinfo->cpu[CPU_WAIT]);
	SAVE_UINT32_X(ret, ksi, "wait_io", sysinfo->wait[W_IO]);
	SAVE_UINT32_X(ret, ksi, "wait_swap", sysinfo->wait[W_SWAP]);
	SAVE_UINT32_X(ret, ksi, "wait_pio", sysinfo->wait[W_PIO]);
	SAVE_UINT32(ret, ksi, sysinfo, bread);
	SAVE_UINT32(ret, ksi, sysinfo, bwrite);
	SAVE_UINT32(ret, ksi, sysinfo, lread);
	SAVE_UINT32(ret, ksi, sysinfo, lwrite);
	SAVE_UINT32(ret, ksi, sysinfo, phread);
	SAVE_UINT32(ret, ksi, sysinfo, phwrite);
	SAVE_UINT32(ret, ksi, sysinfo, pswitch);
	SAVE_UINT32(ret, ksi, sysinfo, trap);
	SAVE_UINT32(ret, ksi, sysinfo, intr);
	SAVE_UINT32(ret, ksi, sysinfo, syscall);
	SAVE_UINT32(ret, ksi, sysinfo, sysread);
	SAVE_UINT32(ret, ksi, sysinfo, syswrite);
	SAVE_UINT32(ret, ksi, sysinfo, sysfork);
	SAVE_UINT32(ret, ksi, sysinfo, sysvfork);
	SAVE_UINT32(ret, ksi, sysinfo, sysexec);
	SAVE_UINT32(ret, ksi, sysinfo, readch);
	SAVE_UINT32(ret, ksi, sysinfo, writech);
	SAVE_UINT32(ret, ksi, sysinfo, rcvint);
	SAVE_UINT32(ret, ksi, sysinfo, xmtint);
	SAVE_UINT32(ret, ksi, sysinfo, mdmint);
	SAVE_UINT32(ret, ksi, sysinfo, rawch);
	SAVE_UINT32(ret, ksi, sysinfo, canch);
	SAVE_UINT32(ret, ksi, sysinfo, outch);
	SAVE_UINT32(ret, ksi, sysinfo, msg);
	SAVE_UINT32(ret, ksi, sysinfo, sema);
	SAVE_UINT32(ret, ksi, sysinfo, namei);
	SAVE_UINT32(ret, ksi, sysinfo, ufsiget);
	SAVE_UINT32(ret, ksi, sysinfo, ufsdirblk);
	SAVE_UINT32(ret, ksi, sysinfo, ufsipage);
	SAVE_UINT32(ret, ksi, sysinfo, ufsinopage);
	SAVE_UINT32(ret, ksi, sysinfo, inodeovf);
	SAVE_UINT32(ret, ksi, sysinfo, fileovf);
	SAVE_UINT32(ret, ksi, sysinfo, procovf);
	SAVE_UINT32(ret, ksi, sysinfo, intrthread);
	SAVE_UINT32(ret, ksi, sysinfo, intrblk);
	SAVE_UINT32(ret, ksi, sysinfo, idlethread);
	SAVE_UINT32(ret, ksi, sysinfo, inv_swtch);
	SAVE_UINT32(ret, ksi, sysinfo, nthreads);
	SAVE_UINT32(ret, ksi, sysinfo, cpumigrate);
	SAVE_UINT32(ret, ksi, sysinfo, xcalls);
	SAVE_UINT32(ret, ksi, sysinfo, mutex_adenters);
	SAVE_UINT32(ret, ksi, sysinfo, rw_rdfails);
	SAVE_UINT32(ret, ksi, sysinfo, rw_wrfails);
	SAVE_UINT32(ret, ksi, sysinfo, modload);
	SAVE_UINT32(ret, ksi, sysinfo, modunload);
	SAVE_UINT32(ret, ksi, sysinfo, bawrite);
#ifdef	STATISTICS	/* see header file */
	SAVE_UINT32(ret, ksi, sysinfo, rw_enters);
	SAVE_UINT32(ret, ksi, sysinfo, win_uo_cnt);
	SAVE_UINT32(ret, ksi, sysinfo, win_uu_cnt);
	SAVE_UINT32(ret, ksi, sysinfo, win_so_cnt);
	SAVE_UINT32(ret, ksi, sysinfo, win_su_cnt);
	SAVE_UINT32(ret, ksi, sysinfo, win_suo_cnt);
#endif

	SAVE_INT32(ret, ksi, syswait, iowait);
	SAVE_INT32(ret, ksi, syswait, swap);
	SAVE_INT32(ret, ksi, syswait, physio);

	SAVE_UINT32(ret, ksi, vminfo, pgrec);
	SAVE_UINT32(ret, ksi, vminfo, pgfrec);
	SAVE_UINT32(ret, ksi, vminfo, pgin);
	SAVE_UINT32(ret, ksi, vminfo, pgpgin);
	SAVE_UINT32(ret, ksi, vminfo, pgout);
	SAVE_UINT32(ret, ksi, vminfo, pgpgout);
	SAVE_UINT32(ret, ksi, vminfo, swapin);
	SAVE_UINT32(ret, ksi, vminfo, pgswapin);
	SAVE_UINT32(ret, ksi, vminfo, swapout);
	SAVE_UINT32(ret, ksi, vminfo, pgswapout);
	SAVE_UINT32(ret, ksi, vminfo, zfod);
	SAVE_UINT32(ret, ksi, vminfo, dfree);
	SAVE_UINT32(ret, ksi, vminfo, scan);
	SAVE_UINT32(ret, ksi, vminfo, rev);
	SAVE_UINT32(ret, ksi, vminfo, hat_fault);
	SAVE_UINT32(ret, ksi, vminfo, as_fault);
	SAVE_UINT32(ret, ksi, vminfo, maj_fault);
	SAVE_UINT32(ret, ksi, vminfo, cow_fault);
	SAVE_UINT32(ret, ksi, vminfo, prot_fault);
	SAVE_UINT32(ret, ksi, vminfo, softlock);
	SAVE_UINT32(ret, ksi, vminfo, kernel_asflt);
	SAVE_UINT32(ret, ksi, vminfo, pgrrun);
	SAVE_UINT32(ret, ksi, vminfo, execpgin);
	SAVE_UINT32(ret, ksi, vminfo, execpgout);
	SAVE_UINT32(ret, ksi, vminfo, execfree);
	SAVE_UINT32(ret, ksi, vminfo, anonpgin);
	SAVE_UINT32(ret, ksi, vminfo, anonpgout);
	SAVE_UINT32(ret, ksi, vminfo, anonfree);
	SAVE_UINT32(ret, ksi, vminfo, fspgin);
	SAVE_UINT32(ret, ksi, vminfo, fspgout);
	SAVE_UINT32(ret, ksi, vminfo, fsfree);
}

static void
save_var(ks_returner_t *ret, kstat_t *kp, ks_instance_t *ksi)
{
	struct var	*var = (struct var *)(kp->ks_data);

	assert(kp->ks_data_size == sizeof (struct var));

	SAVE_INT32(ret, ksi, var, v_buf);
	SAVE_INT32(ret, ksi, var, v_call);
	SAVE_INT32(ret, ksi, var, v_proc);
	SAVE_INT32(ret, ksi, var, v_maxupttl);
	SAVE_INT32(ret, ksi, var, v_nglobpris);
	SAVE_INT32(ret, ksi, var, v_maxsyspri);
	SAVE_INT32(ret, ksi, var, v_clist);
	SAVE_INT32(ret, ksi, var, v_maxup);
	SAVE_INT32(ret, ksi, var, v_hbuf);
	SAVE_INT32(ret, ksi, var, v_hmask);
	SAVE_INT32(ret, ksi, var, v_pbuf);
	SAVE_INT32(ret, ksi, var, v_sptmap);
	SAVE_INT32(ret, ksi, var, v_maxpmem);
	SAVE_INT32(ret, ksi, var, v_autoup);
	SAVE_INT32(ret, ksi, var, v_bufhwm);
}

static void
save_ncstats(ks_returner_t *ret, kstat_t *kp, ks_instance_t *ksi)
{
	struct ncstats	*ncstats = (struct ncstats *)(kp->ks_data);

	assert(kp->ks_data_size == sizeof (struct ncstats));

	SAVE_INT32(ret, ksi, ncstats, hits);
	SAVE_INT32(ret, ksi, ncstats, misses);
	SAVE_INT32(ret, ksi, ncstats, enters);
	SAVE_INT32(ret, ksi, ncstats, dbl_enters);
	SAVE_INT32(ret, ksi, ncstats, long_enter);
	SAVE_INT32(ret, ksi, ncstats, long_look);
	SAVE_INT32(ret, ksi, ncstats, move_to_front);
	SAVE_INT32(ret, ksi, ncstats, purges);
}

static void
save_sysinfo(ks_returner_t *ret, kstat_t *kp, ks_instance_t *ksi)
{
	sysinfo_t	*sysinfo = (sysinfo_t *)(kp->ks_data);

	assert(kp->ks_data_size == sizeof (sysinfo_t));

	SAVE_UINT32(ret, ksi, sysinfo, updates);
	SAVE_UINT32(ret, ksi, sysinfo, runque);
	SAVE_UINT32(ret, ksi, sysinfo, runocc);
	SAVE_UINT32(ret, ksi, sysinfo, swpque);
	SAVE_UINT32(ret, ksi, sysinfo, swpocc);
	SAVE_UINT32(ret, ksi, sysinfo, waiting);
}

static void
save_vminfo(ks_returner_t *ret, kstat_t *kp, ks_instance_t *ksi)
{
	vminfo_t	*vminfo = (vminfo_t *)(kp->ks_data);

	assert(kp->ks_data_size == sizeof (vminfo_t));

	SAVE_UINT64(ret, ksi, vminfo, freemem);
	SAVE_UINT64(ret, ksi, vminfo, swap_resv);
	SAVE_UINT64(ret, ksi, vminfo, swap_alloc);
	SAVE_UINT64(ret, ksi, vminfo, swap_avail);
	SAVE_UINT64(ret, ksi, vminfo, swap_free);
	SAVE_UINT64(ret, ksi, vminfo, updates);
}

static void
save_nfs(ks_returner_t *ret, kstat_t *kp, ks_instance_t *ksi)
{
	struct mntinfo_kstat *mntinfo = (struct mntinfo_kstat *)(kp->ks_data);

	assert(kp->ks_data_size == sizeof (struct mntinfo_kstat));

	SAVE_STRING(ret, ksi, mntinfo, mik_proto);
	SAVE_UINT32(ret, ksi, mntinfo, mik_vers);
	SAVE_UINT32(ret, ksi, mntinfo, mik_flags);
	SAVE_UINT32(ret, ksi, mntinfo, mik_secmod);
	SAVE_UINT32(ret, ksi, mntinfo, mik_curread);
	SAVE_UINT32(ret, ksi, mntinfo, mik_curwrite);
	SAVE_INT32(ret, ksi, mntinfo, mik_timeo);
	SAVE_INT32(ret, ksi, mntinfo, mik_retrans);
	SAVE_UINT32(ret, ksi, mntinfo, mik_acregmin);
	SAVE_UINT32(ret, ksi, mntinfo, mik_acregmax);
	SAVE_UINT32(ret, ksi, mntinfo, mik_acdirmin);
	SAVE_UINT32(ret, ksi, mntinfo, mik_acdirmax);
	SAVE_UINT32_X(ret, ksi, "lookup_srtt", mntinfo->mik_timers[0].srtt);
	SAVE_UINT32_X(ret, ksi, "lookup_deviate", mntinfo->mik_timers[0].deviate);
	SAVE_UINT32_X(ret, ksi, "lookup_rtxcur", mntinfo->mik_timers[0].rtxcur);
	SAVE_UINT32_X(ret, ksi, "read_srtt", mntinfo->mik_timers[1].srtt);
	SAVE_UINT32_X(ret, ksi, "read_deviate", mntinfo->mik_timers[1].deviate);
	SAVE_UINT32_X(ret, ksi, "read_rtxcur", mntinfo->mik_timers[1].rtxcur);
	SAVE_UINT32_X(ret, ksi, "write_srtt", mntinfo->mik_timers[2].srtt);
	SAVE_UINT32_X(ret, ksi, "write_deviate", mntinfo->mik_timers[2].deviate);
	SAVE_UINT32_X(ret, ksi, "write_rtxcur", mntinfo->mik_timers[2].rtxcur);
	SAVE_UINT32(ret, ksi, mntinfo, mik_noresponse);
	SAVE_UINT32(ret, ksi, mntinfo, mik_failover);
	SAVE_UINT32(ret, ksi, mntinfo, mik_remap);
	SAVE_STRING(ret, ksi, mntinfo, mik_curserver);
}

#ifdef __sparc
static void
save_sfmmu_global_stat(ks_returner_t *ret, kstat_t *kp, ks_instance_t *ksi)
{
	struct sfmmu_global_stat *sfmmug =
	    (struct sfmmu_global_stat *)(kp->ks_data);

	assert(kp->ks_data_size == sizeof (struct sfmmu_global_stat));

	SAVE_INT32(ret, ksi, sfmmug, sf_tsb_exceptions);
	SAVE_INT32(ret, ksi, sfmmug, sf_tsb_raise_exception);
	SAVE_INT32(ret, ksi, sfmmug, sf_pagefaults);
	SAVE_INT32(ret, ksi, sfmmug, sf_uhash_searches);
	SAVE_INT32(ret, ksi, sfmmug, sf_uhash_links);
	SAVE_INT32(ret, ksi, sfmmug, sf_khash_searches);
	SAVE_INT32(ret, ksi, sfmmug, sf_khash_links);
	SAVE_INT32(ret, ksi, sfmmug, sf_swapout);
	SAVE_INT32(ret, ksi, sfmmug, sf_tsb_alloc);
	SAVE_INT32(ret, ksi, sfmmug, sf_tsb_allocfail);
	SAVE_INT32(ret, ksi, sfmmug, sf_tsb_sectsb_create);
	SAVE_INT32(ret, ksi, sfmmug, sf_scd_1sttsb_alloc);
	SAVE_INT32(ret, ksi, sfmmug, sf_scd_2ndtsb_alloc);
	SAVE_INT32(ret, ksi, sfmmug, sf_scd_1sttsb_allocfail);
	SAVE_INT32(ret, ksi, sfmmug, sf_scd_2ndtsb_allocfail);
	SAVE_INT32(ret, ksi, sfmmug, sf_tteload8k);
	SAVE_INT32(ret, ksi, sfmmug, sf_tteload64k);
	SAVE_INT32(ret, ksi, sfmmug, sf_tteload512k);
	SAVE_INT32(ret, ksi, sfmmug, sf_tteload4m);
	SAVE_INT32(ret, ksi, sfmmug, sf_tteload32m);
	SAVE_INT32(ret, ksi, sfmmug, sf_tteload256m);
	SAVE_INT32(ret, ksi, sfmmug, sf_tsb_load8k);
	SAVE_INT32(ret, ksi, sfmmug, sf_tsb_load4m);
	SAVE_INT32(ret, ksi, sfmmug, sf_hblk_hit);
	SAVE_INT32(ret, ksi, sfmmug, sf_hblk8_ncreate);
	SAVE_INT32(ret, ksi, sfmmug, sf_hblk8_nalloc);
	SAVE_INT32(ret, ksi, sfmmug, sf_hblk1_ncreate);
	SAVE_INT32(ret, ksi, sfmmug, sf_hblk1_nalloc);
	SAVE_INT32(ret, ksi, sfmmug, sf_hblk_slab_cnt);
	SAVE_INT32(ret, ksi, sfmmug, sf_hblk_reserve_cnt);
	SAVE_INT32(ret, ksi, sfmmug, sf_hblk_recurse_cnt);
	SAVE_INT32(ret, ksi, sfmmug, sf_hblk_reserve_hit);
	SAVE_INT32(ret, ksi, sfmmug, sf_get_free_success);
	SAVE_INT32(ret, ksi, sfmmug, sf_get_free_throttle);
	SAVE_INT32(ret, ksi, sfmmug, sf_get_free_fail);
	SAVE_INT32(ret, ksi, sfmmug, sf_put_free_success);
	SAVE_INT32(ret, ksi, sfmmug, sf_put_free_fail);
	SAVE_INT32(ret, ksi, sfmmug, sf_pgcolor_conflict);
	SAVE_INT32(ret, ksi, sfmmug, sf_uncache_conflict);
	SAVE_INT32(ret, ksi, sfmmug, sf_unload_conflict);
	SAVE_INT32(ret, ksi, sfmmug, sf_ism_uncache);
	SAVE_INT32(ret, ksi, sfmmug, sf_ism_recache);
	SAVE_INT32(ret, ksi, sfmmug, sf_recache);
	SAVE_INT32(ret, ksi, sfmmug, sf_steal_count);
	SAVE_INT32(ret, ksi, sfmmug, sf_pagesync);
	SAVE_INT32(ret, ksi, sfmmug, sf_clrwrt);
	SAVE_INT32(ret, ksi, sfmmug, sf_pagesync_invalid);
	SAVE_INT32(ret, ksi, sfmmug, sf_kernel_xcalls);
	SAVE_INT32(ret, ksi, sfmmug, sf_user_xcalls);
	SAVE_INT32(ret, ksi, sfmmug, sf_tsb_grow);
	SAVE_INT32(ret, ksi, sfmmug, sf_tsb_shrink);
	SAVE_INT32(ret, ksi, sfmmug, sf_tsb_resize_failures);
	SAVE_INT32(ret, ksi, sfmmug, sf_tsb_reloc);
	SAVE_INT32(ret, ksi, sfmmug, sf_user_vtop);
	SAVE_INT32(ret, ksi, sfmmug, sf_ctx_inv);
	SAVE_INT32(ret, ksi, sfmmug, sf_tlb_reprog_pgsz);
	SAVE_INT32(ret, ksi, sfmmug, sf_region_remap_demap);
	SAVE_INT32(ret, ksi, sfmmug, sf_create_scd);
	SAVE_INT32(ret, ksi, sfmmug, sf_join_scd);
	SAVE_INT32(ret, ksi, sfmmug, sf_leave_scd);
	SAVE_INT32(ret, ksi, sfmmug, sf_destroy_scd);
}
#endif

#ifdef __sparc
static void
save_sfmmu_tsbsize_stat(ks_returner_t *ret, kstat_t *kp, ks_instance_t *ksi)
{
	struct sfmmu_tsbsize_stat *sfmmut;

	assert(kp->ks_data_size == sizeof (struct sfmmu_tsbsize_stat));
	sfmmut = (struct sfmmu_tsbsize_stat *)(kp->ks_data);

	SAVE_INT32(ret, ksi, sfmmut, sf_tsbsz_8k);
	SAVE_INT32(ret, ksi, sfmmut, sf_tsbsz_16k);
	SAVE_INT32(ret, ksi, sfmmut, sf_tsbsz_32k);
	SAVE_INT32(ret, ksi, sfmmut, sf_tsbsz_64k);
	SAVE_INT32(ret, ksi, sfmmut, sf_tsbsz_128k);
	SAVE_INT32(ret, ksi, sfmmut, sf_tsbsz_256k);
	SAVE_INT32(ret, ksi, sfmmut, sf_tsbsz_512k);
	SAVE_INT32(ret, ksi, sfmmut, sf_tsbsz_1m);
	SAVE_INT32(ret, ksi, sfmmut, sf_tsbsz_2m);
	SAVE_INT32(ret, ksi, sfmmut, sf_tsbsz_4m);
}
#endif

#ifdef __sparc
static void
save_simmstat(ks_returner_t *ret, kstat_t *kp, ks_instance_t *ksi)
{
	uchar_t	*simmstat;
	char	*simm_buf;
	char	*list = NULL;
	int	i;

	assert(kp->ks_data_size == sizeof (uchar_t) * SIMM_COUNT);

	for (i = 0, simmstat = (uchar_t *)(kp->ks_data); i < SIMM_COUNT - 1;
	    i++, simmstat++) {
		if (list == NULL) {
			(void) asprintf(&simm_buf, "%d,", *simmstat);
		} else {
			(void) asprintf(&simm_buf, "%s%d,", list, *simmstat);
			free(list);
		}
		list = simm_buf;
	}

	(void) asprintf(&simm_buf, "%s%d", list, *simmstat);
	SAVE_STRING_X(ret, ksi, "status", simm_buf);
	free(list);
	free(simm_buf);
}
#endif

#ifdef __sparc
/*
 * Helper function for save_temperature().
 */
static char *
short_array_to_string(short *shortp, int len)
{
	char	*list = NULL;
	char	*list_buf;

	for (; len > 1; len--, shortp++) {
		if (list == NULL) {
			(void) asprintf(&list_buf, "%hd,", *shortp);
		} else {
			(void) asprintf(&list_buf, "%s%hd,", list, *shortp);
			free(list);
		}
		list = list_buf;
	}

	(void) asprintf(&list_buf, "%s%hd", list, *shortp);
	free(list);
	return (list_buf);
}

static void
save_temperature(ks_returner_t *ret, kstat_t *kp, ks_instance_t *ksi)
{
	struct temp_stats *temps = (struct temp_stats *)(kp->ks_data);
	char	*buf;

	assert(kp->ks_data_size == sizeof (struct temp_stats));

	SAVE_UINT32(ret, ksi, temps, index);

	buf = short_array_to_string(temps->l1, L1_SZ);
	SAVE_STRING_X(ret, ksi, "l1", buf);
	free(buf);

	buf = short_array_to_string(temps->l2, L2_SZ);
	SAVE_STRING_X(ret, ksi, "l2", buf);
	free(buf);

	buf = short_array_to_string(temps->l3, L3_SZ);
	SAVE_STRING_X(ret, ksi, "l3", buf);
	free(buf);

	buf = short_array_to_string(temps->l4, L4_SZ);
	SAVE_STRING_X(ret, ksi, "l4", buf);
	free(buf);

	buf = short_array_to_string(temps->l5, L5_SZ);
	SAVE_STRING_X(ret, ksi, "l5", buf);
	free(buf);

	SAVE_INT32(ret, ksi, temps, max);
	SAVE_INT32(ret, ksi, temps, min);
	SAVE_INT32(ret, ksi, temps, state);
	SAVE_INT32(ret, ksi, temps, temp_cnt);
	SAVE_INT32(ret, ksi, temps, shutdown_cnt);
	SAVE_INT32(ret, ksi, temps, version);
	SAVE_INT32(ret, ksi, temps, trend);
	SAVE_INT32(ret, ksi, temps, override);
}
#endif

#ifdef __sparc
static void
save_temp_over(ks_returner_t *ret, kstat_t *kp, ks_instance_t *ksi)
{
	short	*sh = (short *)(kp->ks_data);
	char	*value;

	assert(kp->ks_data_size == sizeof (short));

	(void) asprintf(&value, "%hu", *sh);
	SAVE_STRING_X(ret, ksi, "override", value);
	free(value);
}
#endif

#ifdef __sparc
static void
save_ps_shadow(ks_returner_t *ret, kstat_t *kp, ks_instance_t *ksi)
{
	uchar_t	*uchar = (uchar_t *)(kp->ks_data);

	assert(kp->ks_data_size == SYS_PS_COUNT);

	SAVE_CHAR_X(ret, ksi, "core_0", *uchar++);
	SAVE_CHAR_X(ret, ksi, "core_1", *uchar++);
	SAVE_CHAR_X(ret, ksi, "core_2", *uchar++);
	SAVE_CHAR_X(ret, ksi, "core_3", *uchar++);
	SAVE_CHAR_X(ret, ksi, "core_4", *uchar++);
	SAVE_CHAR_X(ret, ksi, "core_5", *uchar++);
	SAVE_CHAR_X(ret, ksi, "core_6", *uchar++);
	SAVE_CHAR_X(ret, ksi, "core_7", *uchar++);
	SAVE_CHAR_X(ret, ksi, "pps_0", *uchar++);
	SAVE_CHAR_X(ret, ksi, "clk_33", *uchar++);
	SAVE_CHAR_X(ret, ksi, "clk_50", *uchar++);
	SAVE_CHAR_X(ret, ksi, "v5_p", *uchar++);
	SAVE_CHAR_X(ret, ksi, "v12_p", *uchar++);
	SAVE_CHAR_X(ret, ksi, "v5_aux", *uchar++);
	SAVE_CHAR_X(ret, ksi, "v5_p_pch", *uchar++);
	SAVE_CHAR_X(ret, ksi, "v12_p_pch", *uchar++);
	SAVE_CHAR_X(ret, ksi, "v3_pch", *uchar++);
	SAVE_CHAR_X(ret, ksi, "v5_pch", *uchar++);
	SAVE_CHAR_X(ret, ksi, "p_fan", *uchar++);
}
#endif

#ifdef __sparc
static void
save_fault_list(ks_returner_t *ret, kstat_t *kp, ks_instance_t *ksi)
{
	struct ft_list *fault;
	char	name[KSTAT_STRLEN + 7];
	int	i;

	for (i = 1, fault = (struct ft_list *)(kp->ks_data);
	    i <= 999999 && i <= kp->ks_data_size / sizeof (struct ft_list);
	    i++, fault++) {
		(void) snprintf(name, sizeof (name), "unit_%d", i);
		SAVE_INT32_X(ret, ksi, name, fault->unit);
		(void) snprintf(name, sizeof (name), "type_%d", i);
		SAVE_INT32_X(ret, ksi, name, fault->type);
		(void) snprintf(name, sizeof (name), "fclass_%d", i);
		SAVE_INT32_X(ret, ksi, name, fault->fclass);
		(void) snprintf(name, sizeof (name), "create_time_%d", i);
		SAVE_HRTIME_X(ret, ksi, name, fault->create_time);
		(void) snprintf(name, sizeof (name), "msg_%d", i);
		SAVE_STRING_X(ret, ksi, name, fault->msg);
	}
}
#endif

static void
save_named(ks_returner_t *ret, kstat_t *kp, ks_instance_t *ksi)
{
	kstat_named_t *knp;
	int	n;

	for (n = kp->ks_ndata, knp = KSTAT_NAMED_PTR(kp); n > 0; n--, knp++) {
		switch (knp->data_type) {
		case KSTAT_DATA_CHAR:
			nvpair_insert(ret, ksi, knp->name,
			    (ks_value_t *)&knp->value, KSTAT_DATA_CHAR);
			break;
		case KSTAT_DATA_INT32:
			nvpair_insert(ret, ksi, knp->name,
			    (ks_value_t *)&knp->value, KSTAT_DATA_INT32);
			break;
		case KSTAT_DATA_UINT32:
			nvpair_insert(ret, ksi, knp->name,
			    (ks_value_t *)&knp->value, KSTAT_DATA_UINT32);
			break;
		case KSTAT_DATA_INT64:
			nvpair_insert(ret, ksi, knp->name,
			    (ks_value_t *)&knp->value, KSTAT_DATA_INT64);
			break;
		case KSTAT_DATA_UINT64:
			nvpair_insert(ret, ksi, knp->name,
			    (ks_value_t *)&knp->value, KSTAT_DATA_UINT64);
			break;
		case KSTAT_DATA_STRING:
			SAVE_STRING_X(ret, ksi, knp->name, KSTAT_NAMED_STR_PTR(knp));
			break;
		default:
			assert(B_FALSE); /* Invalid data type */
			break;
		}
	}
}

static void
save_intr(ks_returner_t *ret, kstat_t *kp, ks_instance_t *ksi)
{
	kstat_intr_t *intr = KSTAT_INTR_PTR(kp);
	char	*intr_names[] = {"hard", "soft", "watchdog", "spurious",
	    "multiple_service"};
	int	n;

	for (n = 0; n < KSTAT_NUM_INTRS; n++)
		SAVE_UINT32_X(ret, ksi, intr_names[n], intr->intrs[n]);
}

static void
save_io(ks_returner_t *ret, kstat_t *kp, ks_instance_t *ksi)
{
	kstat_io_t	*ksio = KSTAT_IO_PTR(kp);

	SAVE_UINT64(ret, ksi, ksio, nread);
	SAVE_UINT64(ret, ksi, ksio, nwritten);
	SAVE_UINT32(ret, ksi, ksio, reads);
	SAVE_UINT32(ret, ksi, ksio, writes);
	SAVE_HRTIME(ret, ksi, ksio, wtime);
	SAVE_HRTIME(ret, ksi, ksio, wlentime);
	SAVE_HRTIME(ret, ksi, ksio, wlastupdate);
	SAVE_HRTIME(ret, ksi, ksio, rtime);
	SAVE_HRTIME(ret, ksi, ksio, rlentime);
	SAVE_HRTIME(ret, ksi, ksio, rlastupdate);
	SAVE_UINT32(ret, ksi, ksio, wcnt);
	SAVE_UINT32(ret, ksi, ksio, rcnt);
}

static void
save_timer(ks_returner_t *ret, kstat_t *kp, ks_instance_t *ksi)
{
	kstat_timer_t	*ktimer = KSTAT_TIMER_PTR(kp);

	SAVE_STRING(ret, ksi, ktimer, name);
	SAVE_UINT64(ret, ksi, ktimer, num_events);
	SAVE_HRTIME(ret, ksi, ktimer, elapsed_time);
	SAVE_HRTIME(ret, ksi, ktimer, min_time);
	SAVE_HRTIME(ret, ksi, ktimer, max_time);
	SAVE_HRTIME(ret, ksi, ktimer, start_time);
	SAVE_HRTIME(ret, ksi, ktimer, stop_time);
}

/*
 * Print the value of a name-value pair.
 */
static ERL_NIF_TERM
ks_nvpair_term(ks_returner_t *ret, ks_nvpair_t *nvpair)
{
	ERL_NIF_TERM	value = 0;

	switch (nvpair->data_type) {
	case KSTAT_DATA_CHAR:
		value = EKSTAT_STRING(nvpair->value.c);
		break;
	case KSTAT_DATA_INT32:
		value = EKSTAT_INT(nvpair->value.i32);
		break;
	case KSTAT_DATA_UINT32:
		value = EKSTAT_UINT(nvpair->value.ui32);
		break;
	case KSTAT_DATA_INT64:
		value = EKSTAT_INT64(nvpair->value.i64);
		break;
	case KSTAT_DATA_UINT64:
		value = EKSTAT_UINT64(nvpair->value.ui64);
		break;
	case KSTAT_DATA_STRING:
		value = EKSTAT_STRING(KSTAT_NAMED_STR_PTR(nvpair));
		break;
	case KSTAT_DATA_HRTIME:
		if (nvpair->value.ui64 == 0)
			value = EKSTAT_INT(0);
		else
			value = EKSTAT_DOUBLE(nvpair->value.ui64 / 1000000000.0);
		break;
	default:
		assert(B_FALSE);
	}

	return EKSTAT_TUPLE2(EKSTAT_STRING(nvpair->name), value);
}

static ERL_NIF_TERM
ks_instance_term(ks_returner_t *ret, ks_instance_t *ksi, ERL_NIF_TERM statistics)
{
	return EKSTAT_TUPLE5(
		EKSTAT_STRING(ksi->ks_class),
		EKSTAT_STRING(ksi->ks_module),
		EKSTAT_INT(ksi->ks_instance),
		EKSTAT_STRING(ksi->ks_name),
		statistics
	);
}

static ks_returner_t *
new_returner(ErlNifEnv *env)
{
	ks_returner_t *ret;
	ret = (ks_returner_t *)(malloc(sizeof (ks_returner_t)));
	ret->env = env;
	ret->ready = B_FALSE;
	return ret;
}

static ERL_NIF_TERM
ks_returner_term(ks_returner_t *ret, ERL_NIF_TERM term)
{
	free(ret);
	return term;
}

static void
ks_selector_arg(ks_returner_t *ret, ks_pattern_t *pattern, ERL_NIF_TERM arg)
{
	unsigned	size;
	char		*string;
	int		result;
	ErlNifSInt64	integer;

	if (ret->ready != B_TRUE) {
		if (enif_is_atom(ret->env, arg)) {
			enif_get_atom_length(ret->env, arg, &size, ERL_NIF_LATIN1);
			string = (char *)(malloc(sizeof (char) * (size + 1)));
			if (string == NULL) {
				ret->term = EKSTAT_ERROR("atom malloc");
				ret->ready = B_TRUE;
				return;
			}
			result = enif_get_atom(ret->env, arg, string, size + 1, ERL_NIF_LATIN1);
			if (result == 0) {
				ret->term = enif_make_badarg(ret->env);
				ret->ready = B_TRUE;
			} else {
				if (strncmp(string, "_", result) == 0) {
					pattern->pstr = "*";
					pattern->free = B_FALSE;
				} else {
					ret->term = enif_make_badarg(ret->env);
					ret->ready = B_TRUE;
				}
				free(string);
			}
		} else if (enif_is_list(ret->env, arg)) {
			enif_get_list_length(ret->env, arg, &size);
			string = (char *)(malloc(sizeof (char) * (size + 1)));
			if (string == NULL) {
				ret->term = EKSTAT_ERROR("list malloc");
				ret->ready = B_TRUE;
				return;
			}
			result = enif_get_string(ret->env, arg, string, size + 1, ERL_NIF_LATIN1);
			if (result == 0) {
				ret->term = enif_make_badarg(ret->env);
				ret->ready = B_TRUE;
			} else {
				pattern->pstr = (char *)(ks_safe_strdup(ret, string));
				pattern->free = B_TRUE;
			}
			free(string);
		} else if (enif_is_number(ret->env, arg)) {
			if (enif_get_int64(ret->env, arg, &integer)) {
				(void) asprintf(&string, "%d", integer);
				pattern->pstr = (char *)(ks_safe_strdup(ret, string));
				pattern->free = B_TRUE;
			} else {
				ret->term = enif_make_badarg(ret->env);
				ret->ready = B_TRUE;
			}
			free(string);
		} else {
			ret->term = enif_make_badarg(ret->env);
			ret->ready = B_TRUE;
		}
	}
}

/*
 * Erlang NIF functions
 */
ErlNifResourceType	*kstat_handle;

static int
load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
	ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
	kstat_handle = enif_open_resource_type(
		env,
		"ekstat",
		"handle",
		&handle_dtor,
		flags,
		0
	);
	return 0;
}

static int
upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
	return 0;
}

static void
handle_dtor(ErlNifEnv *env, void *handle)
{
	ks_instance_t	*ksi, *ktmp;
	ks_nvpair_t	*nvpair, *ntmp;
	(void) kstat_close(((ks_handle_t *)(handle))->ks_ctl);
	/* Free the instances list */
	ksi = list_head(&((ks_handle_t *)(handle))->instances_list);
	while (ksi != NULL) {
		nvpair = list_head(&ksi->ks_nvlist);
		while (nvpair != NULL) {
			ntmp = nvpair;
			nvpair = list_next(&ksi->ks_nvlist, nvpair);
			list_remove(&ksi->ks_nvlist, ntmp);
			if (ntmp->data_type == KSTAT_DATA_STRING)
				free(ntmp->value.str.addr.ptr);
			free(ntmp);
		}

		ktmp = ksi;
		ksi = list_next(&((ks_handle_t *)(handle))->instances_list, ksi);
		list_remove(&((ks_handle_t *)(handle))->instances_list, ktmp);
		list_destroy(&ktmp->ks_nvlist);
		free(ktmp);
	}
}

static ERL_NIF_TERM
open_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	ks_returner_t	*ret;
	ks_handle_t	*handle;
	ERL_NIF_TERM	term;

	ret = new_returner(env);

	handle = (ks_handle_t *)(enif_alloc_resource(kstat_handle, sizeof(ks_handle_t)));
	handle->ks_id = -1;
	list_create(&handle->instances_list, sizeof (ks_instance_t), offsetof(ks_instance_t, ks_next));
	term = enif_make_resource(env, handle);
	enif_release_resource(handle);

	while ((handle->ks_ctl = kstat_open()) == NULL) {
		if (errno == EAGAIN) {
			(void) poll(NULL, 0, 200);
		} else {
			return EKSTAT_RETURN(EKSTAT_ERROR("kstat_open failed"));
		}
	}

	return EKSTAT_RETURN(EKSTAT_OK(term));
}

static ERL_NIF_TERM
update_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	ks_returner_t	*ret;
	kstat_raw_reader_t	save_raw = NULL;
	ks_handle_t	*handle;
	kid_t		id;
	ks_instance_t	*ksi, *ktmp;
	ks_nvpair_t	*nvpair, *ntmp;
	kstat_t		*kp;
	int		count = 0;

	ret = new_returner(env);

	if (argc < 1) {
		return EKSTAT_RETURN(enif_make_badarg(ret->env));
	}

	if (!enif_get_resource(env, argv[0], kstat_handle, (void **)(&handle))) {
		return EKSTAT_RETURN(enif_make_badarg(ret->env));
	}

	if ((id = kstat_chain_update(handle->ks_ctl)) == 0 && handle->ks_id != -1) {
		return EKSTAT_RETURN(EKSTAT_ERROR("kstat_chain_update stopped"));
	}

	if (id == -1) {
		return EKSTAT_RETURN(EKSTAT_ERROR("kid not valid"));
	}

	/* Free the instances list */
	ksi = list_head(&handle->instances_list);
	while (ksi != NULL) {
		nvpair = list_head(&ksi->ks_nvlist);
		while (nvpair != NULL) {
			ntmp = nvpair;
			nvpair = list_next(&ksi->ks_nvlist, nvpair);
			list_remove(&ksi->ks_nvlist, ntmp);
			if (ntmp->data_type == KSTAT_DATA_STRING)
				free(ntmp->value.str.addr.ptr);
			free(ntmp);
		}

		ktmp = ksi;
		ksi = list_next(&handle->instances_list, ksi);
		list_remove(&handle->instances_list, ktmp);
		list_destroy(&ktmp->ks_nvlist);
		free(ktmp);
	}

	for (kp = handle->ks_ctl->kc_chain; kp != NULL; kp = kp->ks_next) {
		/* Don't bother storing the kstat headers */
		if (strncmp(kp->ks_name, "kstat_", 6) == 0) {
			continue;
		}

		/* Don't bother storing raw stats we don't understand */
		if (kp->ks_type == KSTAT_TYPE_RAW) {
			save_raw = lookup_raw_kstat_fn(kp->ks_module, kp->ks_name);
			if (save_raw == NULL) {
				continue;
			}
		}

		/*
		 * Allocate a new instance and fill in the values
		 * we know so far.
		 */
		ksi = (ks_instance_t *)(malloc(sizeof (ks_instance_t)));
		if (ksi == NULL) {
			return EKSTAT_RETURN(EKSTAT_ERROR("ks_instance_t malloc"));
		}

		list_link_init(&ksi->ks_next);

		(void) strlcpy(ksi->ks_module, kp->ks_module, KSTAT_STRLEN);
		(void) strlcpy(ksi->ks_name, kp->ks_name, KSTAT_STRLEN);
		(void) strlcpy(ksi->ks_class, kp->ks_class, KSTAT_STRLEN);

		ksi->ks_instance = kp->ks_instance;
		ksi->ks_snaptime = kp->ks_snaptime;
		ksi->ks_type = kp->ks_type;

		list_create(&ksi->ks_nvlist, sizeof (ks_nvpair_t), offsetof(ks_nvpair_t, nv_next));

		SAVE_HRTIME_X(ret, ksi, "crtime", kp->ks_crtime);
		SAVE_HRTIME_X(ret, ksi, "snaptime", kp->ks_snaptime);

		/* Insert this instance into a sorted list */
		ktmp = list_head(&handle->instances_list);
		while (ktmp != NULL && compare_instances(ksi, ktmp) < 0) {
			ktmp = list_next(&handle->instances_list, ktmp);
		}

		list_insert_before(&handle->instances_list, ktmp, ksi);

		/* Read the actual statistics */
		id = kstat_read(handle->ks_ctl, kp, NULL);
		if (id == -1) {
			continue;
		}

		switch (kp->ks_type) {
		case KSTAT_TYPE_RAW:
			save_raw(ret, kp, ksi);
			break;
		case KSTAT_TYPE_NAMED:
			save_named(ret, kp, ksi);
			break;
		case KSTAT_TYPE_INTR:
			save_intr(ret, kp, ksi);
			break;
		case KSTAT_TYPE_IO:
			save_io(ret, kp, ksi);
			break;
		case KSTAT_TYPE_TIMER:
			save_timer(ret, kp, ksi);
			break;
		default:
			assert(B_FALSE); /* Invalid type */
			break;
		}

		count++;
	}

	return EKSTAT_RETURN(EKSTAT_OK(EKSTAT_INT(count)));
}

static ERL_NIF_TERM
read_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	ks_returner_t	*ret;
	ERL_NIF_TERM	term, statistics;
	ks_handle_t	*handle;
	ks_instance_t	*ksi;
	ks_nvpair_t	*nvpair;
	char		*ks_number;
	ks_selector_t	*selector;
	int		matched;

	ret = new_returner(env);

	if (argc < 1 || argc > 6) {
		return EKSTAT_RETURN(enif_make_badarg(ret->env));
	}

	if (!enif_get_resource(env, argv[0], kstat_handle, (void **)(&handle))) {
		return EKSTAT_RETURN(enif_make_badarg(ret->env));
	}

	selector = new_selector(ret);

	if (ret->ready == B_TRUE) {
		if ((void *)(selector) != NULL) {
			free_selector(selector);
		}
		return EKSTAT_RETURN(ret->term);
	}

	switch (argc) {
	case 2:
		(void) ks_selector_arg(ret, &selector->ks_class, argv[1]);
		break;
	case 3:
		(void) ks_selector_arg(ret, &selector->ks_class, argv[1]);
		(void) ks_selector_arg(ret, &selector->ks_module, argv[2]);
		break;
	case 4:
		(void) ks_selector_arg(ret, &selector->ks_class, argv[1]);
		(void) ks_selector_arg(ret, &selector->ks_module, argv[2]);
		(void) ks_selector_arg(ret, &selector->ks_instance, argv[3]);
		break;
	case 5:
		(void) ks_selector_arg(ret, &selector->ks_class, argv[1]);
		(void) ks_selector_arg(ret, &selector->ks_module, argv[2]);
		(void) ks_selector_arg(ret, &selector->ks_instance, argv[3]);
		(void) ks_selector_arg(ret, &selector->ks_name, argv[4]);
		break;
	case 6:
		(void) ks_selector_arg(ret, &selector->ks_class, argv[1]);
		(void) ks_selector_arg(ret, &selector->ks_module, argv[2]);
		(void) ks_selector_arg(ret, &selector->ks_instance, argv[3]);
		(void) ks_selector_arg(ret, &selector->ks_name, argv[4]);
		(void) ks_selector_arg(ret, &selector->ks_statistic, argv[5]);
		break;
	default:
		break;
	}

	if (ret->ready == B_TRUE) {
		free_selector(selector);
		return EKSTAT_RETURN(ret->term);
	}

	term = enif_make_list(env, 0);

	/* Iterate over each instance */
	for (ksi = list_head(&handle->instances_list); ksi != NULL; ksi = list_next(&handle->instances_list, ksi)) {
		matched = 0;
		statistics = (ERL_NIF_TERM)(NULL);

		(void) asprintf(&ks_number, "%d", ksi->ks_instance);
		if (
			!(ks_match(ret, ksi->ks_module, &selector->ks_module) &&
			ks_match(ret, ksi->ks_name, &selector->ks_name) &&
			ks_match(ret, ks_number, &selector->ks_instance) &&
			ks_match(ret, ksi->ks_class, &selector->ks_class))
		) {
			free(ks_number);
			if (ret->ready == B_TRUE) {
				free_selector(selector);
				return EKSTAT_RETURN(ret->term);
			}
			continue;
		}

		free(ks_number);

		/* Finally iterate over each statistic */
		for (nvpair = list_head(&ksi->ks_nvlist); nvpair != NULL; nvpair = list_next(&ksi->ks_nvlist, nvpair)) {
			if (!ks_match(ret, nvpair->name, &selector->ks_statistic)) {
				if (ret->ready == B_TRUE) {
					free_selector(selector);
					return EKSTAT_RETURN(ret->term);
				}
				continue;
			}

			if ((void *)(statistics) == NULL) {
				statistics = enif_make_list(env, 0);
			}

			matched = 1;
			statistics = enif_make_list_cell(env, ks_nvpair_term(ret, nvpair), statistics);
		}

		if (matched == 1) {
			term = enif_make_list_cell(env, ks_instance_term(ret, ksi, statistics), term);
		}
	}

	free_selector(selector);

	return EKSTAT_RETURN(term);
}

static ERL_NIF_TERM
clear_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	ks_returner_t	*ret;
	ks_handle_t	*handle;
	ks_instance_t	*ksi, *ktmp;
	ks_nvpair_t	*nvpair, *ntmp;
	int		count = 0;

	ret = new_returner(env);

	if (argc < 1) {
		return EKSTAT_RETURN(enif_make_badarg(ret->env));
	}

	if (!enif_get_resource(env, argv[0], kstat_handle, (void **)(&handle))) {
		return EKSTAT_RETURN(enif_make_badarg(ret->env));
	}

	/* Free the instances list */
	ksi = list_head(&handle->instances_list);
	while (ksi != NULL) {
		nvpair = list_head(&ksi->ks_nvlist);
		while (nvpair != NULL) {
			ntmp = nvpair;
			nvpair = list_next(&ksi->ks_nvlist, nvpair);
			list_remove(&ksi->ks_nvlist, ntmp);
			if (ntmp->data_type == KSTAT_DATA_STRING)
				free(ntmp->value.str.addr.ptr);
			free(ntmp);
		}

		ktmp = ksi;
		ksi = list_next(&handle->instances_list, ksi);
		list_remove(&handle->instances_list, ktmp);
		list_destroy(&ktmp->ks_nvlist);
		free(ktmp);

		count++;
	}

	return EKSTAT_RETURN(EKSTAT_OK(EKSTAT_INT(count)));
}

ERL_NIF_INIT(ekstat, nif_funcs, *load, NULL, *upgrade, NULL);
