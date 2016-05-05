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
 * Copyright (c) 2014, Pagoda Box, Inc. All rights reserved.
 * Copyright 2016 Joyent, Inc.
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

#define KSR_INTERNAL
#include "kstat_reader.h"

#define CHECK(expr, label)								\
	if (KSR_OK != (ret = (expr))) {							\
		DPRINTF("CHECK(\"%s\") failed \"%s\" at %s:%d in %s()\n",		\
			#expr, strerror(ret), __FILE__, __LINE__, __func__);		\
		goto label;								\
	}

/*
 * Sort compare function.
 */
static int
ksr_compare_instances(ks_instance_t *l_arg, ks_instance_t *r_arg)
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

/*
 * Inserts an instance in the per selector list.
 */
static int
ksr_nvpair_insert(ks_instance_t *ksi, char *name, ks_value_t *value, uchar_t data_type)
{
	ks_nvpair_t	*instance;
	ks_nvpair_t	*tmp;

	instance = (ks_nvpair_t *)(malloc(sizeof (ks_nvpair_t)));
	if (instance == NULL) {
		if (errno) {
			return (errno);
		}
		return (ENOMEM);
	}
	(void) strlcpy(instance->name, name, KSTAT_STRLEN);
	(void) memcpy(&instance->value, value, sizeof (ks_value_t));
	instance->data_type = data_type;

	tmp = list_head(&ksi->ks_nvlist);
	while (tmp != NULL && strcasecmp(instance->name, tmp->name) < 0) {
		tmp = list_next(&ksi->ks_nvlist, tmp);
	}

	(void) list_insert_before(&ksi->ks_nvlist, tmp, instance);

	return (KSR_OK);
}

/*
 * Match a string against a shell glob or extended regular expression.
 */
static boolean_t
ksr_match(const char *str, ks_pattern_t *pattern)
{
	if (pattern) {
		if (pattern->preg) {
			return ((regexec(pattern->preg, str, 0, NULL, 0) == 0));
		}
		return ((gmatch(str, pattern->pstr) != 0));
	}
	return (B_TRUE);
}

/*
 * Allocates a new all-matching selector.
 */
static ks_selector_t *
ksr_new_selector(char **errbuf, char *class, char *module, char *instance, char *name, char *statistic)
{
	ks_selector_t	*selector;

	selector = (ks_selector_t *)(malloc(sizeof (ks_selector_t)));
	if (selector == NULL) {
		return (NULL);
	}

	selector->class = NULL;
	selector->module = NULL;
	selector->instance = NULL;
	selector->name = NULL;
	selector->statistic = NULL;

	if (class) {
		selector->class = ksr_new_pattern(errbuf, class);
		if (selector->class == NULL) {
			(void) ksr_free_selector(selector);
			return (NULL);
		}
	}

	if (module) {
		selector->module = ksr_new_pattern(errbuf, module);
		if (selector->module == NULL) {
			(void) ksr_free_selector(selector);
			return (NULL);
		}
	}

	if (instance) {
		selector->instance = ksr_new_pattern(errbuf, instance);
		if (selector->instance == NULL) {
			(void) ksr_free_selector(selector);
			return (NULL);
		}
	}

	if (name) {
		selector->name = ksr_new_pattern(errbuf, name);
		if (selector->name == NULL) {
			(void) ksr_free_selector(selector);
			return (NULL);
		}
	}

	if (statistic) {
		selector->statistic = ksr_new_pattern(errbuf, statistic);
		if (selector->statistic == NULL) {
			(void) ksr_free_selector(selector);
			return (NULL);
		}
	}

	return (selector);
}

static ks_pattern_t *
ksr_new_pattern(char **errbuf, char *pstr)
{
	ks_pattern_t	*pattern;
	int		regcode;
	char		*regstr;
	size_t		bufsz;

	if (pstr == NULL) {
		return (NULL);
	}

	pattern = (ks_pattern_t *)(malloc(sizeof (ks_pattern_t)));
	if (pattern == NULL) {
		return (NULL);
	}

	pattern->pstr = NULL;
	pattern->preg = NULL;

	if (gmatch(pstr, "/*/") != 0) {
		pattern->preg = (regex_t *)(malloc(sizeof(regex_t)));
		if (pattern->preg == NULL) {
			(void) ksr_free_pattern(pattern);
			return (NULL);
		}

		/* All regex patterns are strdup'd copies */
		regstr = pstr + 1;
		*(strrchr(regstr, '/')) = '\0';

		regcode = regcomp(pattern->preg, regstr, REG_EXTENDED | REG_NOSUB);
		if (regcode != 0) {
			bufsz = regerror(regcode, NULL, NULL, 0);
			if (bufsz != 0) {
				*errbuf = (char *)(malloc(bufsz));
				if (*errbuf != NULL) {
					(void) regerror(regcode, NULL, *errbuf, bufsz);
				}
				(void) ksr_free_pattern(pattern);
				return (NULL);
			}
		}
	} else {
		pattern->pstr = pstr;
	}

	return (pattern);
}

static void
ksr_free_selector(ks_selector_t *selector)
{
	if (selector == NULL) {
		return;
	}
	(void) ksr_free_pattern(selector->class);
	selector->class = NULL;
	(void) ksr_free_pattern(selector->module);
	selector->module = NULL;
	(void) ksr_free_pattern(selector->instance);
	selector->instance = NULL;
	(void) ksr_free_pattern(selector->name);
	selector->name = NULL;
	(void) ksr_free_pattern(selector->statistic);
	selector->statistic = NULL;
	(void) free(selector);
}

static void
ksr_free_pattern(ks_pattern_t *pattern)
{
	if (pattern == NULL) {
		return;
	}
	if (pattern->pstr != NULL) {
		(void) free(pattern->pstr);
		pattern->pstr = NULL;
	}
	if (pattern->preg != NULL) {
		(void) regfree(pattern->preg);
	}
	(void) free(pattern);
}

static char *
ksr_safe_strdup(char *str)
{
	char	*dup;

	if (str == NULL) {
		return (NULL);
	}

	while ((dup = strdup(str)) == NULL) {
		if (errno == EAGAIN) {
			(void) poll(NULL, 0, 200);
		} else {
			return (NULL);
		}
	}

	return (dup);
}

static int
ksr_save_cpu_stat(kstat_t *kp, ks_instance_t *ksi)
{
	cpu_stat_t	*stat;
	cpu_sysinfo_t	*sysinfo;
	cpu_syswait_t	*syswait;
	cpu_vminfo_t	*vminfo;
	int		ret;

	stat = (cpu_stat_t *)(kp->ks_data);
	sysinfo = &stat->cpu_sysinfo;
	syswait = &stat->cpu_syswait;
	vminfo  = &stat->cpu_vminfo;

	SAVE_UINT32_X(ksi, "idle", sysinfo->cpu[CPU_IDLE]);
	SAVE_UINT32_X(ksi, "user", sysinfo->cpu[CPU_USER]);
	SAVE_UINT32_X(ksi, "kernel", sysinfo->cpu[CPU_KERNEL]);
	SAVE_UINT32_X(ksi, "wait", sysinfo->cpu[CPU_WAIT]);
	SAVE_UINT32_X(ksi, "wait_io", sysinfo->wait[W_IO]);
	SAVE_UINT32_X(ksi, "wait_swap", sysinfo->wait[W_SWAP]);
	SAVE_UINT32_X(ksi, "wait_pio", sysinfo->wait[W_PIO]);
	SAVE_UINT32(ksi, sysinfo, bread);
	SAVE_UINT32(ksi, sysinfo, bwrite);
	SAVE_UINT32(ksi, sysinfo, lread);
	SAVE_UINT32(ksi, sysinfo, lwrite);
	SAVE_UINT32(ksi, sysinfo, phread);
	SAVE_UINT32(ksi, sysinfo, phwrite);
	SAVE_UINT32(ksi, sysinfo, pswitch);
	SAVE_UINT32(ksi, sysinfo, trap);
	SAVE_UINT32(ksi, sysinfo, intr);
	SAVE_UINT32(ksi, sysinfo, syscall);
	SAVE_UINT32(ksi, sysinfo, sysread);
	SAVE_UINT32(ksi, sysinfo, syswrite);
	SAVE_UINT32(ksi, sysinfo, sysfork);
	SAVE_UINT32(ksi, sysinfo, sysvfork);
	SAVE_UINT32(ksi, sysinfo, sysexec);
	SAVE_UINT32(ksi, sysinfo, readch);
	SAVE_UINT32(ksi, sysinfo, writech);
	SAVE_UINT32(ksi, sysinfo, rcvint);
	SAVE_UINT32(ksi, sysinfo, xmtint);
	SAVE_UINT32(ksi, sysinfo, mdmint);
	SAVE_UINT32(ksi, sysinfo, rawch);
	SAVE_UINT32(ksi, sysinfo, canch);
	SAVE_UINT32(ksi, sysinfo, outch);
	SAVE_UINT32(ksi, sysinfo, msg);
	SAVE_UINT32(ksi, sysinfo, sema);
	SAVE_UINT32(ksi, sysinfo, namei);
	SAVE_UINT32(ksi, sysinfo, ufsiget);
	SAVE_UINT32(ksi, sysinfo, ufsdirblk);
	SAVE_UINT32(ksi, sysinfo, ufsipage);
	SAVE_UINT32(ksi, sysinfo, ufsinopage);
	SAVE_UINT32(ksi, sysinfo, inodeovf);
	SAVE_UINT32(ksi, sysinfo, fileovf);
	SAVE_UINT32(ksi, sysinfo, procovf);
	SAVE_UINT32(ksi, sysinfo, intrthread);
	SAVE_UINT32(ksi, sysinfo, intrblk);
	SAVE_UINT32(ksi, sysinfo, idlethread);
	SAVE_UINT32(ksi, sysinfo, inv_swtch);
	SAVE_UINT32(ksi, sysinfo, nthreads);
	SAVE_UINT32(ksi, sysinfo, cpumigrate);
	SAVE_UINT32(ksi, sysinfo, xcalls);
	SAVE_UINT32(ksi, sysinfo, mutex_adenters);
	SAVE_UINT32(ksi, sysinfo, rw_rdfails);
	SAVE_UINT32(ksi, sysinfo, rw_wrfails);
	SAVE_UINT32(ksi, sysinfo, modload);
	SAVE_UINT32(ksi, sysinfo, modunload);
	SAVE_UINT32(ksi, sysinfo, bawrite);
#ifdef	STATISTICS	/* see header file */
	SAVE_UINT32(ksi, sysinfo, rw_enters);
	SAVE_UINT32(ksi, sysinfo, win_uo_cnt);
	SAVE_UINT32(ksi, sysinfo, win_uu_cnt);
	SAVE_UINT32(ksi, sysinfo, win_so_cnt);
	SAVE_UINT32(ksi, sysinfo, win_su_cnt);
	SAVE_UINT32(ksi, sysinfo, win_suo_cnt);
#endif

	SAVE_INT32(ksi, syswait, iowait);
	SAVE_INT32(ksi, syswait, swap);
	SAVE_INT32(ksi, syswait, physio);

	SAVE_UINT32(ksi, vminfo, pgrec);
	SAVE_UINT32(ksi, vminfo, pgfrec);
	SAVE_UINT32(ksi, vminfo, pgin);
	SAVE_UINT32(ksi, vminfo, pgpgin);
	SAVE_UINT32(ksi, vminfo, pgout);
	SAVE_UINT32(ksi, vminfo, pgpgout);
	SAVE_UINT32(ksi, vminfo, swapin);
	SAVE_UINT32(ksi, vminfo, pgswapin);
	SAVE_UINT32(ksi, vminfo, swapout);
	SAVE_UINT32(ksi, vminfo, pgswapout);
	SAVE_UINT32(ksi, vminfo, zfod);
	SAVE_UINT32(ksi, vminfo, dfree);
	SAVE_UINT32(ksi, vminfo, scan);
	SAVE_UINT32(ksi, vminfo, rev);
	SAVE_UINT32(ksi, vminfo, hat_fault);
	SAVE_UINT32(ksi, vminfo, as_fault);
	SAVE_UINT32(ksi, vminfo, maj_fault);
	SAVE_UINT32(ksi, vminfo, cow_fault);
	SAVE_UINT32(ksi, vminfo, prot_fault);
	SAVE_UINT32(ksi, vminfo, softlock);
	SAVE_UINT32(ksi, vminfo, kernel_asflt);
	SAVE_UINT32(ksi, vminfo, pgrrun);
	SAVE_UINT32(ksi, vminfo, execpgin);
	SAVE_UINT32(ksi, vminfo, execpgout);
	SAVE_UINT32(ksi, vminfo, execfree);
	SAVE_UINT32(ksi, vminfo, anonpgin);
	SAVE_UINT32(ksi, vminfo, anonpgout);
	SAVE_UINT32(ksi, vminfo, anonfree);
	SAVE_UINT32(ksi, vminfo, fspgin);
	SAVE_UINT32(ksi, vminfo, fspgout);
	SAVE_UINT32(ksi, vminfo, fsfree);

	return (KSR_OK);
}

static int
ksr_save_var(kstat_t *kp, ks_instance_t *ksi)
{
	struct var	*var = (struct var *)(kp->ks_data);
	int		ret;

	assert(kp->ks_data_size == sizeof (struct var));

	SAVE_INT32(ksi, var, v_buf);
	SAVE_INT32(ksi, var, v_call);
	SAVE_INT32(ksi, var, v_proc);
	SAVE_INT32(ksi, var, v_maxupttl);
	SAVE_INT32(ksi, var, v_nglobpris);
	SAVE_INT32(ksi, var, v_maxsyspri);
	SAVE_INT32(ksi, var, v_clist);
	SAVE_INT32(ksi, var, v_maxup);
	SAVE_INT32(ksi, var, v_hbuf);
	SAVE_INT32(ksi, var, v_hmask);
	SAVE_INT32(ksi, var, v_pbuf);
	SAVE_INT32(ksi, var, v_sptmap);
	SAVE_INT32(ksi, var, v_maxpmem);
	SAVE_INT32(ksi, var, v_autoup);
	SAVE_INT32(ksi, var, v_bufhwm);

	return (KSR_OK);
}

static int
ksr_save_ncstats(kstat_t *kp, ks_instance_t *ksi)
{
	struct ncstats	*ncstats = (struct ncstats *)(kp->ks_data);
	int		ret;

	assert(kp->ks_data_size == sizeof (struct ncstats));

	SAVE_INT32(ksi, ncstats, hits);
	SAVE_INT32(ksi, ncstats, misses);
	SAVE_INT32(ksi, ncstats, enters);
	SAVE_INT32(ksi, ncstats, dbl_enters);
	SAVE_INT32(ksi, ncstats, long_enter);
	SAVE_INT32(ksi, ncstats, long_look);
	SAVE_INT32(ksi, ncstats, move_to_front);
	SAVE_INT32(ksi, ncstats, purges);

	return (KSR_OK);
}

static int
ksr_save_sysinfo(kstat_t *kp, ks_instance_t *ksi)
{
	sysinfo_t	*sysinfo = (sysinfo_t *)(kp->ks_data);
	int		ret;

	assert(kp->ks_data_size == sizeof (sysinfo_t));

	SAVE_UINT32(ksi, sysinfo, updates);
	SAVE_UINT32(ksi, sysinfo, runque);
	SAVE_UINT32(ksi, sysinfo, runocc);
	SAVE_UINT32(ksi, sysinfo, swpque);
	SAVE_UINT32(ksi, sysinfo, swpocc);
	SAVE_UINT32(ksi, sysinfo, waiting);

	return (KSR_OK);
}

static int
ksr_save_vminfo(kstat_t *kp, ks_instance_t *ksi)
{
	vminfo_t	*vminfo = (vminfo_t *)(kp->ks_data);
	int		ret;

	assert(kp->ks_data_size == sizeof (vminfo_t));

	SAVE_UINT64(ksi, vminfo, freemem);
	SAVE_UINT64(ksi, vminfo, swap_resv);
	SAVE_UINT64(ksi, vminfo, swap_alloc);
	SAVE_UINT64(ksi, vminfo, swap_avail);
	SAVE_UINT64(ksi, vminfo, swap_free);
	SAVE_UINT64(ksi, vminfo, updates);

	return (KSR_OK);
}

static int
ksr_save_nfs(kstat_t *kp, ks_instance_t *ksi)
{
	struct mntinfo_kstat *mntinfo = (struct mntinfo_kstat *)(kp->ks_data);
	int	ret;

	assert(kp->ks_data_size == sizeof (struct mntinfo_kstat));

	SAVE_STRING(ksi, mntinfo, mik_proto);
	SAVE_UINT32(ksi, mntinfo, mik_vers);
	SAVE_UINT32(ksi, mntinfo, mik_flags);
	SAVE_UINT32(ksi, mntinfo, mik_secmod);
	SAVE_UINT32(ksi, mntinfo, mik_curread);
	SAVE_UINT32(ksi, mntinfo, mik_curwrite);
	SAVE_INT32(ksi, mntinfo, mik_timeo);
	SAVE_INT32(ksi, mntinfo, mik_retrans);
	SAVE_UINT32(ksi, mntinfo, mik_acregmin);
	SAVE_UINT32(ksi, mntinfo, mik_acregmax);
	SAVE_UINT32(ksi, mntinfo, mik_acdirmin);
	SAVE_UINT32(ksi, mntinfo, mik_acdirmax);
	SAVE_UINT32_X(ksi, "lookup_srtt", mntinfo->mik_timers[0].srtt);
	SAVE_UINT32_X(ksi, "lookup_deviate", mntinfo->mik_timers[0].deviate);
	SAVE_UINT32_X(ksi, "lookup_rtxcur", mntinfo->mik_timers[0].rtxcur);
	SAVE_UINT32_X(ksi, "read_srtt", mntinfo->mik_timers[1].srtt);
	SAVE_UINT32_X(ksi, "read_deviate", mntinfo->mik_timers[1].deviate);
	SAVE_UINT32_X(ksi, "read_rtxcur", mntinfo->mik_timers[1].rtxcur);
	SAVE_UINT32_X(ksi, "write_srtt", mntinfo->mik_timers[2].srtt);
	SAVE_UINT32_X(ksi, "write_deviate", mntinfo->mik_timers[2].deviate);
	SAVE_UINT32_X(ksi, "write_rtxcur", mntinfo->mik_timers[2].rtxcur);
	SAVE_UINT32(ksi, mntinfo, mik_noresponse);
	SAVE_UINT32(ksi, mntinfo, mik_failover);
	SAVE_UINT32(ksi, mntinfo, mik_remap);
	SAVE_STRING(ksi, mntinfo, mik_curserver);

	return (KSR_OK);
}

#ifdef __sparc
static int
ksr_save_sfmmu_global_stat(kstat_t *kp, ks_instance_t *ksi)
{
	struct sfmmu_global_stat *sfmmug =
	    (struct sfmmu_global_stat *)(kp->ks_data);
	int	ret;

	assert(kp->ks_data_size == sizeof (struct sfmmu_global_stat));

	SAVE_INT32(ksi, sfmmug, sf_tsb_exceptions);
	SAVE_INT32(ksi, sfmmug, sf_tsb_raise_exception);
	SAVE_INT32(ksi, sfmmug, sf_pagefaults);
	SAVE_INT32(ksi, sfmmug, sf_uhash_searches);
	SAVE_INT32(ksi, sfmmug, sf_uhash_links);
	SAVE_INT32(ksi, sfmmug, sf_khash_searches);
	SAVE_INT32(ksi, sfmmug, sf_khash_links);
	SAVE_INT32(ksi, sfmmug, sf_swapout);
	SAVE_INT32(ksi, sfmmug, sf_tsb_alloc);
	SAVE_INT32(ksi, sfmmug, sf_tsb_allocfail);
	SAVE_INT32(ksi, sfmmug, sf_tsb_sectsb_create);
	SAVE_INT32(ksi, sfmmug, sf_scd_1sttsb_alloc);
	SAVE_INT32(ksi, sfmmug, sf_scd_2ndtsb_alloc);
	SAVE_INT32(ksi, sfmmug, sf_scd_1sttsb_allocfail);
	SAVE_INT32(ksi, sfmmug, sf_scd_2ndtsb_allocfail);
	SAVE_INT32(ksi, sfmmug, sf_tteload8k);
	SAVE_INT32(ksi, sfmmug, sf_tteload64k);
	SAVE_INT32(ksi, sfmmug, sf_tteload512k);
	SAVE_INT32(ksi, sfmmug, sf_tteload4m);
	SAVE_INT32(ksi, sfmmug, sf_tteload32m);
	SAVE_INT32(ksi, sfmmug, sf_tteload256m);
	SAVE_INT32(ksi, sfmmug, sf_tsb_load8k);
	SAVE_INT32(ksi, sfmmug, sf_tsb_load4m);
	SAVE_INT32(ksi, sfmmug, sf_hblk_hit);
	SAVE_INT32(ksi, sfmmug, sf_hblk8_ncreate);
	SAVE_INT32(ksi, sfmmug, sf_hblk8_nalloc);
	SAVE_INT32(ksi, sfmmug, sf_hblk1_ncreate);
	SAVE_INT32(ksi, sfmmug, sf_hblk1_nalloc);
	SAVE_INT32(ksi, sfmmug, sf_hblk_slab_cnt);
	SAVE_INT32(ksi, sfmmug, sf_hblk_reserve_cnt);
	SAVE_INT32(ksi, sfmmug, sf_hblk_recurse_cnt);
	SAVE_INT32(ksi, sfmmug, sf_hblk_reserve_hit);
	SAVE_INT32(ksi, sfmmug, sf_get_free_success);
	SAVE_INT32(ksi, sfmmug, sf_get_free_throttle);
	SAVE_INT32(ksi, sfmmug, sf_get_free_fail);
	SAVE_INT32(ksi, sfmmug, sf_put_free_success);
	SAVE_INT32(ksi, sfmmug, sf_put_free_fail);
	SAVE_INT32(ksi, sfmmug, sf_pgcolor_conflict);
	SAVE_INT32(ksi, sfmmug, sf_uncache_conflict);
	SAVE_INT32(ksi, sfmmug, sf_unload_conflict);
	SAVE_INT32(ksi, sfmmug, sf_ism_uncache);
	SAVE_INT32(ksi, sfmmug, sf_ism_recache);
	SAVE_INT32(ksi, sfmmug, sf_recache);
	SAVE_INT32(ksi, sfmmug, sf_steal_count);
	SAVE_INT32(ksi, sfmmug, sf_pagesync);
	SAVE_INT32(ksi, sfmmug, sf_clrwrt);
	SAVE_INT32(ksi, sfmmug, sf_pagesync_invalid);
	SAVE_INT32(ksi, sfmmug, sf_kernel_xcalls);
	SAVE_INT32(ksi, sfmmug, sf_user_xcalls);
	SAVE_INT32(ksi, sfmmug, sf_tsb_grow);
	SAVE_INT32(ksi, sfmmug, sf_tsb_shrink);
	SAVE_INT32(ksi, sfmmug, sf_tsb_resize_failures);
	SAVE_INT32(ksi, sfmmug, sf_tsb_reloc);
	SAVE_INT32(ksi, sfmmug, sf_user_vtop);
	SAVE_INT32(ksi, sfmmug, sf_ctx_inv);
	SAVE_INT32(ksi, sfmmug, sf_tlb_reprog_pgsz);
	SAVE_INT32(ksi, sfmmug, sf_region_remap_demap);
	SAVE_INT32(ksi, sfmmug, sf_create_scd);
	SAVE_INT32(ksi, sfmmug, sf_join_scd);
	SAVE_INT32(ksi, sfmmug, sf_leave_scd);
	SAVE_INT32(ksi, sfmmug, sf_destroy_scd);

	return (KSR_OK);
}
#endif

#ifdef __sparc
static int
ksr_save_sfmmu_tsbsize_stat(kstat_t *kp, ks_instance_t *ksi)
{
	struct sfmmu_tsbsize_stat *sfmmut;
	int	ret;

	assert(kp->ks_data_size == sizeof (struct sfmmu_tsbsize_stat));
	sfmmut = (struct sfmmu_tsbsize_stat *)(kp->ks_data);

	SAVE_INT32(ksi, sfmmut, sf_tsbsz_8k);
	SAVE_INT32(ksi, sfmmut, sf_tsbsz_16k);
	SAVE_INT32(ksi, sfmmut, sf_tsbsz_32k);
	SAVE_INT32(ksi, sfmmut, sf_tsbsz_64k);
	SAVE_INT32(ksi, sfmmut, sf_tsbsz_128k);
	SAVE_INT32(ksi, sfmmut, sf_tsbsz_256k);
	SAVE_INT32(ksi, sfmmut, sf_tsbsz_512k);
	SAVE_INT32(ksi, sfmmut, sf_tsbsz_1m);
	SAVE_INT32(ksi, sfmmut, sf_tsbsz_2m);
	SAVE_INT32(ksi, sfmmut, sf_tsbsz_4m);

	return (KSR_OK);
}
#endif

#ifdef __sparc
static int
ksr_save_simmstat(kstat_t *kp, ks_instance_t *ksi)
{
	uchar_t	*simmstat;
	char	*simm_buf;
	char	*list = NULL;
	int	i;
	int	ret;

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
	SAVE_STRING_X(ksi, "status", simm_buf);
	free(list);
	free(simm_buf);

	return (KSR_OK);
}
#endif

#ifdef __sparc
/*
 * Helper function for save_temperature().
 */
static char *
ksr_short_array_to_string(short *shortp, int len)
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

static int
ksr_save_temperature(kstat_t *kp, ks_instance_t *ksi)
{
	struct temp_stats *temps = (struct temp_stats *)(kp->ks_data);
	char	*buf;
	int	ret;

	assert(kp->ks_data_size == sizeof (struct temp_stats));

	SAVE_UINT32(ksi, temps, index);

	buf = short_array_to_string(temps->l1, L1_SZ);
	SAVE_STRING_X(ksi, "l1", buf);
	free(buf);

	buf = short_array_to_string(temps->l2, L2_SZ);
	SAVE_STRING_X(ksi, "l2", buf);
	free(buf);

	buf = short_array_to_string(temps->l3, L3_SZ);
	SAVE_STRING_X(ksi, "l3", buf);
	free(buf);

	buf = short_array_to_string(temps->l4, L4_SZ);
	SAVE_STRING_X(ksi, "l4", buf);
	free(buf);

	buf = short_array_to_string(temps->l5, L5_SZ);
	SAVE_STRING_X(ksi, "l5", buf);
	free(buf);

	SAVE_INT32(ksi, temps, max);
	SAVE_INT32(ksi, temps, min);
	SAVE_INT32(ksi, temps, state);
	SAVE_INT32(ksi, temps, temp_cnt);
	SAVE_INT32(ksi, temps, shutdown_cnt);
	SAVE_INT32(ksi, temps, version);
	SAVE_INT32(ksi, temps, trend);
	SAVE_INT32(ksi, temps, override);

	return (KSR_OK);
}
#endif

#ifdef __sparc
static int
ksr_save_temp_over(kstat_t *kp, ks_instance_t *ksi)
{
	short	*sh = (short *)(kp->ks_data);
	char	*value;
	int	ret;

	assert(kp->ks_data_size == sizeof (short));

	(void) asprintf(&value, "%hu", *sh);
	SAVE_STRING_X(ksi, "override", value);
	free(value);

	return (KSR_OK);
}
#endif

#ifdef __sparc
static int
ksr_save_ps_shadow(kstat_t *kp, ks_instance_t *ksi)
{
	uchar_t	*uchar = (uchar_t *)(kp->ks_data);
	int	ret;

	assert(kp->ks_data_size == SYS_PS_COUNT);

	SAVE_CHAR_X(ksi, "core_0", *uchar++);
	SAVE_CHAR_X(ksi, "core_1", *uchar++);
	SAVE_CHAR_X(ksi, "core_2", *uchar++);
	SAVE_CHAR_X(ksi, "core_3", *uchar++);
	SAVE_CHAR_X(ksi, "core_4", *uchar++);
	SAVE_CHAR_X(ksi, "core_5", *uchar++);
	SAVE_CHAR_X(ksi, "core_6", *uchar++);
	SAVE_CHAR_X(ksi, "core_7", *uchar++);
	SAVE_CHAR_X(ksi, "pps_0", *uchar++);
	SAVE_CHAR_X(ksi, "clk_33", *uchar++);
	SAVE_CHAR_X(ksi, "clk_50", *uchar++);
	SAVE_CHAR_X(ksi, "v5_p", *uchar++);
	SAVE_CHAR_X(ksi, "v12_p", *uchar++);
	SAVE_CHAR_X(ksi, "v5_aux", *uchar++);
	SAVE_CHAR_X(ksi, "v5_p_pch", *uchar++);
	SAVE_CHAR_X(ksi, "v12_p_pch", *uchar++);
	SAVE_CHAR_X(ksi, "v3_pch", *uchar++);
	SAVE_CHAR_X(ksi, "v5_pch", *uchar++);
	SAVE_CHAR_X(ksi, "p_fan", *uchar++);

	return (KSR_OK);
}
#endif

#ifdef __sparc
static int
ksr_save_fault_list(kstat_t *kp, ks_instance_t *ksi)
{
	struct ft_list *fault;
	char	name[KSTAT_STRLEN + 7];
	int	i;
	int	ret;

	for (i = 1, fault = (struct ft_list *)(kp->ks_data);
	    i <= 999999 && i <= kp->ks_data_size / sizeof (struct ft_list);
	    i++, fault++) {
		(void) snprintf(name, sizeof (name), "unit_%d", i);
		SAVE_INT32_X(ksi, name, fault->unit);
		(void) snprintf(name, sizeof (name), "type_%d", i);
		SAVE_INT32_X(ksi, name, fault->type);
		(void) snprintf(name, sizeof (name), "fclass_%d", i);
		SAVE_INT32_X(ksi, name, fault->fclass);
		(void) snprintf(name, sizeof (name), "create_time_%d", i);
		SAVE_HRTIME_X(ksi, name, fault->create_time);
		(void) snprintf(name, sizeof (name), "msg_%d", i);
		SAVE_STRING_X(ksi, name, fault->msg);
	}

	return (KSR_OK);
}
#endif

static int
ksr_save_named(kstat_t *kp, ks_instance_t *ksi)
{
	kstat_named_t *knp;
	int	n;
	int	ret;

	for (n = kp->ks_ndata, knp = KSTAT_NAMED_PTR(kp); n > 0; n--, knp++) {
		switch (knp->data_type) {
		case KSTAT_DATA_CHAR:
			nvpair_insert(ksi, knp->name,
			    (ks_value_t *)&knp->value, KSTAT_DATA_CHAR);
			break;
		case KSTAT_DATA_INT32:
			nvpair_insert(ksi, knp->name,
			    (ks_value_t *)&knp->value, KSTAT_DATA_INT32);
			break;
		case KSTAT_DATA_UINT32:
			nvpair_insert(ksi, knp->name,
			    (ks_value_t *)&knp->value, KSTAT_DATA_UINT32);
			break;
		case KSTAT_DATA_INT64:
			nvpair_insert(ksi, knp->name,
			    (ks_value_t *)&knp->value, KSTAT_DATA_INT64);
			break;
		case KSTAT_DATA_UINT64:
			nvpair_insert(ksi, knp->name,
			    (ks_value_t *)&knp->value, KSTAT_DATA_UINT64);
			break;
		case KSTAT_DATA_STRING:
			SAVE_STRING_X(ksi, knp->name, KSTAT_NAMED_STR_PTR(knp));
			break;
		default:
			assert(B_FALSE); /* Invalid data type */
			break;
		}
	}

	return (KSR_OK);
}

static int
ksr_save_intr(kstat_t *kp, ks_instance_t *ksi)
{
	kstat_intr_t *intr = KSTAT_INTR_PTR(kp);
	char	*intr_names[] = {"hard", "soft", "watchdog", "spurious",
	    "multiple_service"};
	int	n;
	int	ret;

	for (n = 0; n < KSTAT_NUM_INTRS; n++)
		SAVE_UINT32_X(ksi, intr_names[n], intr->intrs[n]);

	return (KSR_OK);
}

static int
ksr_save_io(kstat_t *kp, ks_instance_t *ksi)
{
	kstat_io_t	*ksio = KSTAT_IO_PTR(kp);
	int		ret;

	SAVE_UINT64(ksi, ksio, nread);
	SAVE_UINT64(ksi, ksio, nwritten);
	SAVE_UINT32(ksi, ksio, reads);
	SAVE_UINT32(ksi, ksio, writes);
	SAVE_HRTIME(ksi, ksio, wtime);
	SAVE_HRTIME(ksi, ksio, wlentime);
	SAVE_HRTIME(ksi, ksio, wlastupdate);
	SAVE_HRTIME(ksi, ksio, rtime);
	SAVE_HRTIME(ksi, ksio, rlentime);
	SAVE_HRTIME(ksi, ksio, rlastupdate);
	SAVE_UINT32(ksi, ksio, wcnt);
	SAVE_UINT32(ksi, ksio, rcnt);

	return (KSR_OK);
}

static int
ksr_save_timer(kstat_t *kp, ks_instance_t *ksi)
{
	kstat_timer_t	*ktimer = KSTAT_TIMER_PTR(kp);
	int		ret;

	SAVE_STRING(ksi, ktimer, name);
	SAVE_UINT64(ksi, ktimer, num_events);
	SAVE_HRTIME(ksi, ktimer, elapsed_time);
	SAVE_HRTIME(ksi, ktimer, min_time);
	SAVE_HRTIME(ksi, ktimer, max_time);
	SAVE_HRTIME(ksi, ktimer, start_time);
	SAVE_HRTIME(ksi, ktimer, stop_time);

	return (KSR_OK);
}

static int
ksr_save_hrtimes(kstat_t *kp, ks_instance_t *ksi)
{
	int	ret;

	SAVE_HRTIME_X(ksi, "crtime", kp->ks_crtime);

	return (KSR_OK);
}

static int
ksr_save_error(int errnum, ks_instance_t *ksi)
{
	int	ret;

	SAVE_STRING_X(ksi, "error", strerror(errnum));

	return (KSR_OK);
}

/*
 * This function was taken from the perl kstat module code - please
 * see for further comments there.
 */
static kstat_raw_reader_t
ksr_lookup_raw_kstat_fn(char *module, char *name)
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
 * Public functions
 */

kstat_reader_t *
new_kstat_reader(kstat_ctl_t *control)
{
	kstat_reader_t	*reader;

	reader = (kstat_reader_t *)(malloc(sizeof (kstat_reader_t)));
	if (reader == NULL) {
		return (NULL);
	}

	reader->kid = -1;
	reader->control = control;
	(void) list_create(&reader->cache, sizeof (ks_instance_t), offsetof(ks_instance_t, ks_next));

	return (reader);
}

int
clear_kstat_reader(kstat_reader_t *reader, unsigned int *count)
{
	ks_instance_t	*ksi, *ktmp;
	ks_nvpair_t	*nvpair, *ntmp;

	ksi = list_head(&reader->cache);
	while (ksi != NULL) {
		nvpair = list_head(&ksi->ks_nvlist);
		while (nvpair != NULL) {
			ntmp = nvpair;
			nvpair = list_next(&ksi->ks_nvlist, nvpair);
			(void) list_remove(&ksi->ks_nvlist, ntmp);
			if (ntmp->data_type == KSTAT_DATA_STRING) {
				(void) free(ntmp->value.str.addr.ptr);
			}
			(void) free(ntmp);
		}

		ktmp = ksi;
		ksi = list_next(&reader->cache, ksi);
		(void) list_remove(&reader->cache, ktmp);
		(void) list_destroy(&ktmp->ks_nvlist);
		(void) free(ktmp);

		if (count) {
			(*count)++;
		}
	}

	return (KSR_OK);
}

int
update_kstat_reader(kstat_reader_t *reader)
{
	kid_t	kid;

	kid = kstat_chain_update(reader->control);

	if (kid == 0 && reader->kid != -1) {
		return (KSR_OK);
	}

	if (kid == -1) {
		return (errno);
	}

	reader->kid = kid;

	return (KSR_OK);
}

int
load_kstat_reader(kstat_reader_t *reader, unsigned int *count)
{
	kstat_t			*kp;
	kid_t			kid;
	ks_instance_t		*ksi, *ktmp;
	kstat_raw_reader_t	ksr_save_raw = NULL;
	int			ret;

	for (kp = reader->control->kc_chain; kp != NULL; kp = kp->ks_next) {
		/* Don't bother storing the kstat headers */
		if (strncmp(kp->ks_name, "kstat_", 6) == 0) {
			continue;
		}

		/* Don't bother storing raw stats we don't understand */
		if (kp->ks_type == KSTAT_TYPE_RAW) {
			ksr_save_raw = ksr_lookup_raw_kstat_fn(kp->ks_module, kp->ks_name);
			if (ksr_save_raw == NULL) {
				continue;
			}
		}

		/*
		 * Allocate a new instance and fill in the values
		 * we know so far.
		 */
		ksi = (ks_instance_t *)(malloc(sizeof (ks_instance_t)));
		if (ksi == NULL) {
			(void) clear_kstat_reader(reader, NULL);
			if (errno) {
				return (errno);
			}
			return (ENOMEM);
		}

		(void) list_link_init(&ksi->ks_next);

		(void) strlcpy(ksi->ks_module, kp->ks_module, KSTAT_STRLEN);
		(void) strlcpy(ksi->ks_name, kp->ks_name, KSTAT_STRLEN);
		(void) strlcpy(ksi->ks_class, kp->ks_class, KSTAT_STRLEN);

		ksi->ks_instance = kp->ks_instance;
		ksi->ks_snaptime = kp->ks_snaptime;
		ksi->ks_type = kp->ks_type;

		(void) list_create(&ksi->ks_nvlist, sizeof (ks_nvpair_t), offsetof(ks_nvpair_t, nv_next));

		CHECK(ksr_save_hrtimes(kp, ksi), err1);

		/* Insert this instance into a sorted list */
		ktmp = list_head(&reader->cache);
		while (ktmp != NULL && ksr_compare_instances(ksi, ktmp) < 0) {
			ktmp = list_next(&reader->cache, ktmp);
		}

		(void) list_insert_before(&reader->cache, ktmp, ksi);

		/* Read the actual statistics */
		kid = kstat_read(reader->control, kp, NULL);
		if (kid == -1) {
			/*
			 * It is deeply annoying, but some kstats can return errors
			 * under otherwise routine conditions.  (ACPI is one
			 * offender; there are surely others.)  To prevent these
			 * fouled kstats from completely ruining our day, we assign
			 * an "error" member to the return value that consists of
			 * the strerror().
			 */
			CHECK(ksr_save_error(errno, ksi), err1);
			if (count) {
				(*count)++;
			}
			continue;
		}

		SAVE_HRTIME_X(ksi, "snaptime", kp->ks_snaptime);

		switch (kp->ks_type) {
		case KSTAT_TYPE_RAW:
			CHECK(ksr_save_raw(kp, ksi), err1);
			break;
		case KSTAT_TYPE_NAMED:
			CHECK(ksr_save_named(kp, ksi), err1);
			break;
		case KSTAT_TYPE_INTR:
			CHECK(ksr_save_intr(kp, ksi), err1);
			break;
		case KSTAT_TYPE_IO:
			CHECK(ksr_save_io(kp, ksi), err1);
			break;
		case KSTAT_TYPE_TIMER:
			CHECK(ksr_save_timer(kp, ksi), err1);
			break;
		default:
			assert(B_FALSE); /* Invalid type */
			break;
		}

		if (count) {
			(*count)++;
		}
	}

	return (KSR_OK);

err1:
	(void) clear_kstat_reader(reader, NULL);
	return (ret);
}

int
fold_kstat_reader(kstat_reader_t *reader, kstat_folder_t *folder, void **accumulator)
{
	ks_instance_t	*ksi;
	ks_nvpair_t	*nvpair;
	char		*ks_number;
	int		matched;
	int		result;

	/* Iterate over each instance */
	for (ksi = list_head(&reader->cache); ksi != NULL; ksi = list_next(&reader->cache, ksi)) {
		result = 0;
		matched = 0;

		(void) asprintf(&ks_number, "%d", ksi->ks_instance);
		if (ks_number == NULL) {
			if (errno) {
				return (errno);
			}
			return (ENOMEM);
		}
		if (!(ksr_match(ksi->ks_module, folder->selector->module) &&
				ksr_match(ksi->ks_name, folder->selector->name) &&
				ksr_match(ks_number, folder->selector->instance) &&
				ksr_match(ksi->ks_class, folder->selector->class))) {
			(void) free(ks_number);
			continue;
		}

		(void) free(ks_number);

		/* Finally iterate over each statistic */
		for (nvpair = list_head(&ksi->ks_nvlist); nvpair != NULL; nvpair = list_next(&ksi->ks_nvlist, nvpair)) {
			if (!ksr_match(nvpair->name, folder->selector->statistic)) {
				continue;
			}

			if (matched == 0) {
				result = folder->fold(folder, ksi, NULL, accumulator);
				if (result != KSR_OK) {
					return (result);
				}
			}

			matched = 1;
			result = folder->fold(folder, ksi, nvpair, accumulator);
			if (result != KSR_OK) {
				return (result);
			}
		}

		if (matched == 1) {
			result = folder->fold(folder, NULL, NULL, accumulator);
			if (result != KSR_OK) {
				return (result);
			}
		}
	}

	return (KSR_OK);
}

void
free_kstat_reader(kstat_reader_t *reader)
{
	if (reader == NULL) {
		return;
	}
	(void) clear_kstat_reader(reader, NULL);
	reader->control = NULL;
	(void) free(reader);
}

ks_selector_t *
new_kstat_reader_selector(char **errbuf, char *class, char *module, char *instance, char *name, char *statistic)
{
	return (ksr_new_selector(errbuf, class, module, instance, name, statistic));
}

void
free_kstat_reader_selector(ks_selector_t *selector)
{
	(void) ksr_free_selector(selector);
}
