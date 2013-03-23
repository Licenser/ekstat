#include "erl_nif.h"
#include <string.h>
#include <kstat.h>
#include <stdio.h>
#include <errno.h>

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
  if (ksp->ks_type == KSTAT_TYPE_NAMED) {
    data = data_named(env, ksp);
  } else if (ksp->ks_type == KSTAT_TYPE_IO) {
    data = data_io(env, ksp);
  } else {
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
