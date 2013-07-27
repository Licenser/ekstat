#include "erl_nif.h"
#include <string.h>
#include <kstat.h>
#include <stdio.h>
#include <errno.h>
#include <sys/sysinfo.h>
#include <sys/var.h>

ErlNifResourceType* kstat_handle;

/*
  {ok, H} = ekstat:open(), ekstat:update(H).
  ekstat:read(H).
  ekstat:read(H, "misc").
  ekstat:read(H, "misc", "unix").
  ekstat:read(H, "misc", "unix", 0).
  ekstat:read(H, "misc", "unix", 0, "rcp").
  ekstat:read(H, "misc", "unix", 0, "rcp", "rpc_client").
*/

typedef struct kstat_record_s {
  kstat_t *data;
  struct kstat_record_s* next;
} kstat_record_s;

typedef struct kstat_handle_s {
  kid_t ksr_kid;
  kstat_ctl_t *ksr_ctl;
  kstat_record_s *records;
} kstat_handle_s;

static void handle_dtor(ErlNifEnv* env, void* handle) {
  kstat_close(((kstat_handle_s*)handle)->ksr_ctl);
  kstat_record_s *r;
  for (r = ((kstat_handle_s*)handle)->records; r != NULL; r = r-> next) {
    free(r);
  };
};

ERL_NIF_TERM data_named(ErlNifEnv* env, kstat_t *ksp) {
  ERL_NIF_TERM data = enif_make_list(env, 0);
  ERL_NIF_TERM val = NULL;
  kstat_named_t *nm = KSTAT_NAMED_PTR(ksp);
  int i;

  for (i = 0; i < ksp->ks_ndata; i++, nm++) {
    switch (nm->data_type) {
    case KSTAT_DATA_CHAR:
      val = enif_make_int(env, nm->value.c[0]);
      break;

    case KSTAT_DATA_INT32:
      val = enif_make_int(env, nm->value.i32);
      break;

    case KSTAT_DATA_UINT32:
      val = enif_make_int(env, nm->value.ui32);
      break;

    case KSTAT_DATA_INT64:
      val = enif_make_int64(env, nm->value.i64);
      break;

    case KSTAT_DATA_UINT64:
      val = enif_make_int64(env, nm->value.ui64);
      break;

    case KSTAT_DATA_STRING:
      val = enif_make_string(env,
                             KSTAT_NAMED_STR_PTR(nm),
                             ERL_NIF_LATIN1);
      break;

    default:
      val = enif_make_tuple3(env,
                             enif_make_atom(env, "error"),
                             enif_make_string(env,
                                              "unknown data type",
                                              ERL_NIF_LATIN1),
                             enif_make_int(env, nm->data_type));
    }
    data = enif_make_list_cell(env,
                               enif_make_tuple2(env,
                                                enif_make_string(env,
                                                                 (char *)(nm->name),
                                                                 ERL_NIF_LATIN1),
                                                val), data);
  }
  return data;
};


ERL_NIF_TERM named_tuple(ErlNifEnv* env, const char* name, long v) {
  return enif_make_tuple2(env,
                          enif_make_string(env,
                                           name,
                                           ERL_NIF_LATIN1),
                          enif_make_int64(env, v));
};

ERL_NIF_TERM data_io(ErlNifEnv* env, kstat_t *ksp) {
  kstat_io_t *io = KSTAT_IO_PTR(ksp);
  ERL_NIF_TERM data[12] = {named_tuple(env, "nread", io->nread),
                           named_tuple(env, "nwritten", io->nwritten),
                           named_tuple(env, "reads", io->reads),
                           named_tuple(env, "writes",io->writes),

                           named_tuple(env, "wtime", io->wtime),
                           named_tuple(env, "wlentime", io->wlentime),
                           named_tuple(env, "wlastupdate", io->wlastupdate),

                           named_tuple(env, "rtime", io->rtime),
                           named_tuple(env, "rlentime", io->rlentime),
                           named_tuple(env, "rlastupdate", io->rlastupdate),

                           named_tuple(env, "wcnt", io->wcnt),
                           named_tuple(env, "rcnt", io->rcnt)};
  return enif_make_list_from_array(env, data, 12);
};

ERL_NIF_TERM data_raw_cpu_stat(ErlNifEnv* env, kstat_t *ksp) {
  cpu_sysinfo_t *cpu_sysinfop;
  cpu_sysinfop = (cpu_sysinfo_t *)(ksp->ks_data);
#ifdef STATISTICS
  ERL_NIF_TERM data[59] =
#else
  ERL_NIF_TERM data[53] =
#endif
  {
    named_tuple(env, "idle", cpu_sysinfop->cpu[CPU_IDLE]),
    named_tuple(env, "user", cpu_sysinfop->cpu[CPU_USER]),
    named_tuple(env, "kernel", cpu_sysinfop->cpu[CPU_KERNEL]),
    named_tuple(env, "wait", cpu_sysinfop->cpu[CPU_WAIT]),
    named_tuple(env, "wait_io", cpu_sysinfop->wait[W_IO]),
    named_tuple(env, "wait_swap", cpu_sysinfop->wait[W_SWAP]),
    named_tuple(env, "wait_pio", cpu_sysinfop->wait[W_PIO]),

    named_tuple(env, "bread", cpu_sysinfop->bread),
    named_tuple(env, "bwrite", cpu_sysinfop->bwrite),
    named_tuple(env, "lread", cpu_sysinfop->lread),
    named_tuple(env, "lwrite", cpu_sysinfop->lwrite),
    named_tuple(env, "phread", cpu_sysinfop->phread),
    named_tuple(env, "phwrite", cpu_sysinfop->phwrite),
    named_tuple(env, "pswitch", cpu_sysinfop->pswitch),
    named_tuple(env, "trap", cpu_sysinfop->trap),
    named_tuple(env, "intr", cpu_sysinfop->intr),
    named_tuple(env, "syscall", cpu_sysinfop->syscall),
    named_tuple(env, "sysread", cpu_sysinfop->sysread),
    named_tuple(env, "syswrite", cpu_sysinfop->syswrite),
    named_tuple(env, "sysfork", cpu_sysinfop->sysfork),
    named_tuple(env, "sysvfork", cpu_sysinfop->sysvfork),
    named_tuple(env, "sysexec", cpu_sysinfop->sysexec),
    named_tuple(env, "readch", cpu_sysinfop->readch),
    named_tuple(env, "writech", cpu_sysinfop->writech),
    named_tuple(env, "rcvint", cpu_sysinfop->rcvint),
    named_tuple(env, "xmtint", cpu_sysinfop->xmtint),
    named_tuple(env, "mdmint", cpu_sysinfop->mdmint),
    named_tuple(env, "rawch", cpu_sysinfop->rawch),
    named_tuple(env, "canch", cpu_sysinfop->canch),
    named_tuple(env, "outch", cpu_sysinfop->outch),
    named_tuple(env, "msg", cpu_sysinfop->msg),
    named_tuple(env, "sema", cpu_sysinfop->sema),
    named_tuple(env, "namei", cpu_sysinfop->namei),
    named_tuple(env, "ufsiget", cpu_sysinfop->ufsiget),
    named_tuple(env, "ufsdirblk", cpu_sysinfop->ufsdirblk),
    named_tuple(env, "ufsipage", cpu_sysinfop->ufsipage),
    named_tuple(env, "ufsinopage", cpu_sysinfop->ufsinopage),
    named_tuple(env, "inodeovf", cpu_sysinfop->inodeovf),
    named_tuple(env, "fileovf", cpu_sysinfop->fileovf),
    named_tuple(env, "procovf", cpu_sysinfop->procovf),
    named_tuple(env, "intrthread", cpu_sysinfop->intrthread),
    named_tuple(env, "intrblk", cpu_sysinfop->intrblk),
    named_tuple(env, "idlethread", cpu_sysinfop->idlethread),
    named_tuple(env, "inv_swtch", cpu_sysinfop->inv_swtch),
    named_tuple(env, "nthreads", cpu_sysinfop->nthreads),
    named_tuple(env, "cpumigrate", cpu_sysinfop->cpumigrate),
    named_tuple(env, "xcalls", cpu_sysinfop->xcalls),
    named_tuple(env, "mutex_adenters", cpu_sysinfop->mutex_adenters),
    named_tuple(env, "rw_rdfails", cpu_sysinfop->rw_rdfails),
    named_tuple(env, "rw_wrfails", cpu_sysinfop->rw_wrfails),
    named_tuple(env, "modload", cpu_sysinfop->modload),
    named_tuple(env, "modunload", cpu_sysinfop->modunload),
    named_tuple(env, "bawrite", cpu_sysinfop->bawrite)
#ifdef STATISTICS
    , named_tuple(env, "rw_enters", cpu_sysinfop->rw_enters),
    named_tuple(env, "win_uo_cnt", cpu_sysinfop->win_uo_cnt),
    named_tuple(env, "win_uu_cnt", cpu_sysinfop->win_uu_cnt),
    named_tuple(env, "win_so_cnt", cpu_sysinfop->win_so_cnt),
    named_tuple(env, "win_su_cnt", cpu_sysinfop->win_su_cnt),
    named_tuple(env, "win_suo_cnt", cpu_sysinfop->win_suo_cnt)};
  return enif_make_list_from_array(env, data, 59);
#else
  };
  return enif_make_list_from_array(env, data, 53);
#endif
};

ERL_NIF_TERM data_raw_sysinfo(ErlNifEnv* env, kstat_t *ksp) {
  sysinfo_t* sysinfop;
  sysinfop = (sysinfo_t *)(ksp->ks_data);
  ERL_NIF_TERM data[6] = {named_tuple(env, "updates", sysinfop->updates),
                          named_tuple(env, "runque", sysinfop->runque),
                          named_tuple(env, "runocc", sysinfop->runocc),
                          named_tuple(env, "swpque",sysinfop->swpque),
                          named_tuple(env, "swpocc", sysinfop->swpocc),
                          named_tuple(env, "waiting", sysinfop->waiting)};
  return enif_make_list_from_array(env, data, 6);
};

ERL_NIF_TERM data_raw_var(ErlNifEnv* env, kstat_t *ksp) {
  struct var *varp;
  varp = (struct var *)(ksp->ks_data);
  ERL_NIF_TERM data[15] = {
    named_tuple(env, "v_buf", varp->v_buf),
    named_tuple(env, "v_call", varp->v_call),
    named_tuple(env, "v_proc", varp->v_proc),
    named_tuple(env, "v_maxupttl", varp->v_maxupttl),
    named_tuple(env, "v_nglobpris", varp->v_nglobpris),
    named_tuple(env, "v_maxsyspri", varp->v_maxsyspri),
    named_tuple(env, "v_clist", varp->v_clist),
    named_tuple(env, "v_maxup", varp->v_maxup),
    named_tuple(env, "v_hbuf", varp->v_hbuf),
    named_tuple(env, "v_hmask", varp->v_hmask),
    named_tuple(env, "v_pbuf", varp->v_pbuf),
    named_tuple(env, "v_sptmap", varp->v_sptmap),
    named_tuple(env, "v_maxpmem", varp->v_maxpmem),
    named_tuple(env, "v_autoup", varp->v_autoup),
    named_tuple(env, "v_bufhwm", varp->v_bufhwm)
  };
  return enif_make_list_from_array(env, data, 15);
};

ERL_NIF_TERM data_raw_vminfo(ErlNifEnv* env, kstat_t *ksp) {
  vminfo_t *vminfop;
  vminfop = (vminfo_t *)(ksp->ks_data);
  ERL_NIF_TERM data[6] = {named_tuple(env, "freemem", vminfop->freemem),
                          named_tuple(env, "swap_resv", vminfop->swap_resv),
                          named_tuple(env, "swap_alloc", vminfop->swap_alloc),
                          named_tuple(env, "swap_avail",vminfop->swap_avail),
                          named_tuple(env, "swap_free", vminfop->swap_free),
                          named_tuple(env, "updates", vminfop->updates)};
  return enif_make_list_from_array(env, data, 6);
};

ERL_NIF_TERM data_raw(ErlNifEnv* env, kstat_t *ksp) {
  ERL_NIF_TERM data;

  if (!strcmp(ksp->ks_module, "cpu_stat")) {
    data = data_raw_cpu_stat(env, ksp);
  } else if (!strcmp(ksp->ks_module, "unix")) {
    if (!strcmp(ksp->ks_name, "sysinfo")) {
      data = data_raw_sysinfo(env, ksp);
    } else if (!strcmp(ksp->ks_name, "var")) {
      data = data_raw_var(env, ksp);
    } else if (!strcmp(ksp->ks_name, "vminfo")) {
      data = data_raw_vminfo(env, ksp);
    } else {
      char error[256];
      snprintf(error, sizeof error, "unknown raw unix ksp->ks_name: %s", ksp->ks_name);
      data = enif_make_tuple2(env,
                              enif_make_atom(env, "error"),
                              enif_make_string(env,
                                               error,
                                               ERL_NIF_LATIN1));
    }
  } else {
    char error[256];
    snprintf(error, sizeof error, "unknown raw ksp->ks_module: %s", ksp->ks_module);
    data = enif_make_tuple2(env,
                            enif_make_atom(env, "error"),
                            enif_make_string(env,
                                             error,
                                             ERL_NIF_LATIN1));
  }

  return data;
};

ERL_NIF_TERM read_entry(ErlNifEnv* env, kstat_ctl_t *ksr_ctl, kstat_t *ksp) {
  ERL_NIF_TERM data;
  if (kstat_read(ksr_ctl, ksp, NULL) == -1) {
    /*
     * It is deeply annoying, but some kstats can return errors
     * under otherwise routine conditions.  (ACPI is one
     * offender; there are surely others.)  To prevent these
     * fouled kstats from completely ruining our day, we assign
     * an "error" member to the return value that consists of
     * the strerror().
     */
    data = enif_make_tuple2(env,
                            enif_make_atom(env, "error"),
                            enif_make_string(env,
                                             strerror(errno),
                                             ERL_NIF_LATIN1));
    return data;
  }
  switch(ksp->ks_type) {
    case KSTAT_TYPE_NAMED:
      data = data_named(env, ksp);
      break;
    case KSTAT_TYPE_IO:
      data = data_io(env, ksp);
      break;
    case KSTAT_TYPE_TIMER:
      data = enif_make_tuple2(env,
                              enif_make_atom(env, "error"),
                              enif_make_string(env,
                                               "unknown timer",
                                               ERL_NIF_LATIN1));
      break;
    case KSTAT_TYPE_INTR:
      data = enif_make_tuple2(env,
                              enif_make_atom(env, "error"),
                              enif_make_string(env,
                                               "unknown intr",
                                               ERL_NIF_LATIN1));
      break;
    case KSTAT_TYPE_RAW:
      data = data_raw(env, ksp);
      break;
    default:
      data = enif_make_tuple2(env,
                              enif_make_atom(env, "error"),
                              enif_make_string(env,
                                               "unknown ksp",
                                               ERL_NIF_LATIN1));
  }
  return data;
}
ERL_NIF_TERM make_row(ErlNifEnv* env, kstat_ctl_t *ksr_ctl, kstat_t *ksp) {
  return enif_make_tuple5(env,
                          enif_make_string(env,
                                           ksp->ks_class,
                                           ERL_NIF_LATIN1),
                          enif_make_string(env,
                                           ksp->ks_module,
                                           ERL_NIF_LATIN1),
                          enif_make_int(env,
                                        ksp->ks_instance),
                          enif_make_string(env,
                                           ksp->ks_name,
                                           ERL_NIF_LATIN1),
                          read_entry(env, ksr_ctl, ksp));
};

ERL_NIF_TERM collect(ErlNifEnv* env, kstat_handle_s *handle, char *class, char *module, int instance, char *name) {
  ERL_NIF_TERM terms = enif_make_list(env, 0);
  kstat_t *ksp;
  kstat_record_s *r;
  for (r = handle->records; r != NULL; r = r-> next) {
    ksp = r->data;
    if ((class && strncmp(class, (char*)ksp->ks_class, KSTAT_STRLEN) != 0) ||
        (module && strncmp(module, (char*)ksp->ks_module, KSTAT_STRLEN) != 0) ||
        (instance != -1 && ksp->ks_instance != instance) ||
        (name && strncmp((char*)name, (char*)ksp->ks_name, KSTAT_STRLEN) != 0))
      continue;
    terms = enif_make_list_cell(env, make_row(env, handle->ksr_ctl, ksp), terms);
  };
  return terms;
};

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
  ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
  kstat_handle = enif_open_resource_type(env,
                                         "ekstat",
                                         "handle",
                                         &handle_dtor,
                                         flags,
                                         0);
  return 0;
};

static ERL_NIF_TERM open_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  kstat_handle_s *handle;

  handle = (kstat_handle_s *)enif_alloc_resource(kstat_handle, sizeof(kstat_handle_s));
  handle->ksr_kid = -1;
  handle->records = NULL;
  ERL_NIF_TERM term = enif_make_resource(env, handle);

  enif_release_resource(handle);
  handle->ksr_ctl = kstat_open();

  if (handle->ksr_ctl == NULL) { // if the handle could not be generated.
    return enif_make_tuple2(env,
                            enif_make_atom(env, "error"),
                            enif_make_string(env,
                                             "could not open create handle",
                                             ERL_NIF_LATIN1));
  };

  return  enif_make_tuple2(env,
                           enif_make_atom(env, "ok"),
                           term);
};

static ERL_NIF_TERM update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  kstat_handle_s *handle;
  kstat_t *ksp;
  kid_t kid;
  kstat_record_s *r;
  int cnt = 0;
  if (argc < 1) {
    return enif_make_badarg(env);
  }

  if (!enif_get_resource(env, argv[0], kstat_handle, (void **)&handle)) {
    return enif_make_badarg(env);
  }

  if ((kid = kstat_chain_update(handle->ksr_ctl)) == 0 && handle->ksr_kid != -1) {
    return enif_make_tuple2(env,
                            enif_make_atom(env, "error"),
                            enif_make_string(env,
                                             "chain updated stopped",
                                             ERL_NIF_LATIN1));
  }

  if (kid == -1)
    return enif_make_tuple2(env,
                            enif_make_atom(env, "error"),
                            enif_make_string(env,
                                             "kid not valid",
                                             ERL_NIF_LATIN1));

  for (r = handle->records; r != NULL; r = r->next) {
    free(r);
  }
  r = NULL;
  for (ksp = handle->ksr_ctl->kc_chain; ksp != NULL; ksp = ksp->ks_next) {
    if (r == NULL) {
      handle->records = malloc(sizeof(kstat_handle_s));
      r = handle->records;
    } else {
      r->next = malloc(sizeof(kstat_handle_s));
      r = r->next;
    };
    r->next = NULL;
    r->data = ksp;
    cnt++;
  };
  return enif_make_tuple2(env,
                          enif_make_atom(env, "ok"),
                          enif_make_int(env,
                                        cnt));

};

static ERL_NIF_TERM read_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  kstat_handle_s *handle;
  int instance = -1;
  char module[KSTAT_STRLEN],
    name[KSTAT_STRLEN],
    class[KSTAT_STRLEN];

  if (!enif_get_resource(env, argv[0], kstat_handle, (void **)&handle)) {
    return enif_make_badarg(env);
  }

  switch (argc) {
  case 5:
    if (!enif_get_string(env, argv[4], name, 64, ERL_NIF_LATIN1)) {
      return enif_make_badarg(env);
    }
  case 4:
    if (!enif_get_int(env, argv[3], &instance)) {
      return enif_make_badarg(env);
    }
  case 3:
    if (!enif_get_string(env, argv[2], module, 64, ERL_NIF_LATIN1)) {
      return enif_make_badarg(env);
    }
  case 2:
    if (!enif_get_string(env, argv[1], class, 64, ERL_NIF_LATIN1)) {
      return enif_make_badarg(env);
    }
  };

  // need to set the unused ones to null
  switch (argc) {
  case 1:
    return collect(env, handle, NULL, NULL, -1, NULL);
  case 2:
    return collect(env, handle, class, NULL, -1, NULL);
  case 3:
    return collect(env, handle, class, module, -1, NULL);
  case 4:
    return collect(env, handle, class, module, instance, NULL);
  default:
    return collect(env, handle, class, module, instance, name);
  };

};

static ErlNifFunc nif_funcs[] = {
  {"open", 0, open_nif},
  {"update", 1, update_nif},
  {"read", 1, read_nif},
  {"read", 2, read_nif},
  {"read", 3, read_nif},
  {"read", 4, read_nif},
  {"read", 5, read_nif},
};

 ERL_NIF_INIT(ekstat, nif_funcs, *load, NULL, NULL, NULL);
