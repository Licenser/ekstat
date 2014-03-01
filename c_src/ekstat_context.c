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

#include "common.h"

ekstat_context_t *
new_ekstat_context(kstat_ctl_t *control)
{
	ekstat_context_t	*context;

	context = (ekstat_context_t *)(malloc(sizeof(*context)));
	if (context == NULL) {
		return (NULL);
	}

	context->control = control;
	(void) list_create(&context->cache, sizeof (ks_instance_t), offsetof(ks_instance_t, ks_next));
	context->last_error = 0;
	context->last_error_string = NULL;

	return context;
}

int
clear_ekstat_context(ekstat_context_t *context)
{
	ks_instance_t	*ksi, *ktmp;
	ks_nvpair_t	*nvpair, *ntmp;
	int		count = 0;

	ksi = list_head(&context->cache);
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
		ksi = list_next(&context->cache, ksi);
		(void) list_remove(&context->cache, ktmp);
		(void) list_destroy(&ktmp->ks_nvlist);
		(void) free(ktmp);

		count++;
	}

	return count;
}

int
load_ekstat_context(ekstat_context_t *context)
{
	kstat_t			*kp;
	kid_t			id;
	ks_instance_t		*ksi, *ktmp;
	kstat_raw_reader_t	save_raw = NULL;
	int			count = 0;

	for (kp = context->control->kc_chain; kp != NULL; kp = kp->ks_next) {
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
			return (-1);
		}
		ksi->context = context;

		(void) list_link_init(&ksi->ks_next);

		(void) strlcpy(ksi->ks_module, kp->ks_module, KSTAT_STRLEN);
		(void) strlcpy(ksi->ks_name, kp->ks_name, KSTAT_STRLEN);
		(void) strlcpy(ksi->ks_class, kp->ks_class, KSTAT_STRLEN);

		ksi->ks_instance = kp->ks_instance;
		ksi->ks_snaptime = kp->ks_snaptime;
		ksi->ks_type = kp->ks_type;

		(void) list_create(&ksi->ks_nvlist, sizeof (ks_nvpair_t), offsetof(ks_nvpair_t, nv_next));

		SAVE_HRTIME_X(ksi, "crtime", kp->ks_crtime);
		SAVE_HRTIME_X(ksi, "snaptime", kp->ks_snaptime);

		/* Insert this instance into a sorted list */
		ktmp = list_head(&context->cache);
		while (ktmp != NULL && compare_instances(ksi, ktmp) < 0) {
			ktmp = list_next(&context->cache, ktmp);
		}

		(void) list_insert_before(&context->cache, ktmp, ksi);

		/* Read the actual statistics */
		id = kstat_read(context->control, kp, NULL);
		if (id == -1) {
			continue;
		}

		switch (kp->ks_type) {
		case KSTAT_TYPE_RAW:
			save_raw(kp, ksi);
			break;
		case KSTAT_TYPE_NAMED:
			save_named(kp, ksi);
			break;
		case KSTAT_TYPE_INTR:
			save_intr(kp, ksi);
			break;
		case KSTAT_TYPE_IO:
			save_io(kp, ksi);
			break;
		case KSTAT_TYPE_TIMER:
			save_timer(kp, ksi);
			break;
		default:
			assert(B_FALSE); /* Invalid type */
			break;
		}

		count++;
	}

	return count;
}

int
fold_ekstat_context(ekstat_context_t *context, ekstat_folder_t *folder)
{
	ks_instance_t	*ksi;
	ks_nvpair_t	*nvpair;
	char		*ks_number;
	int		matched;
	int		result;

	/* Iterate over each instance */
	for (ksi = list_head(&context->cache); ksi != NULL; ksi = list_next(&context->cache, ksi)) {
		matched = 0;

		(void) asprintf(&ks_number, "%d", ksi->ks_instance);
		if (ks_number == NULL) {
			return (EKSTAT_ENOMEM);
		}
		if (!(ks_match(ksi->ks_module, &folder->selector->ks_module) &&
				ks_match(ksi->ks_name, &folder->selector->ks_name) &&
				ks_match(ks_number, &folder->selector->ks_instance) &&
				ks_match(ksi->ks_class, &folder->selector->ks_class))) {
			(void) free(ks_number);
			continue;
		}

		(void) free(ks_number);

		/* Finally iterate over each statistic */
		for (nvpair = list_head(&ksi->ks_nvlist); nvpair != NULL; nvpair = list_next(&ksi->ks_nvlist, nvpair)) {
			if (!ks_match(nvpair->name, &folder->selector->ks_statistic)) {
				continue;
			}

			if (matched == 0) {
				result = folder->prepare(folder);
				if (result != EKSTAT_OK) {
					return (result);
				}
			}

			matched = 1;
			result = folder->fold(folder, nvpair);
			if (result != EKSTAT_OK) {
				return (result);
			}
		}

		if (matched == 1) {
			result = folder->finalize(folder, ksi);
			if (result != EKSTAT_OK) {
				return (result);
			}
		}
	}

	return (EKSTAT_OK);
}

void
free_ekstat_context(ekstat_context_t *context)
{
	if (context == NULL) {
		return;
	}
	(void) clear_ekstat_context(context);
	context->control = NULL;
	(void) free(context);
}

ks_selector_t *
new_ks_selector(ekstat_context_t *context)
{
	return new_selector(context);
}

void
free_ks_selector(ks_selector_t *selector)
{
	(void) free_selector(selector);
}
