-module(ekstat).

-export([open/0,
         update/1,
         read/1,
         read/2,
         read/3,
         read/4,
         read/5,
         read/6,
         pagesize/0
        ]).

-on_load(init/0).

-type kstat_handle() :: binary().
-type kstat_error() :: {error, {kstat, string()}}.

-type kstat_statistic() :: {Statistic::string(), Value::integer()} |
                           {Statistic::string(), Value::string()}.

-type kstat_row() :: {Class::string(), Module::string(), Instance::integer(), Name::string(), [kstat_statistic()]}.

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, _} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, "ekstat_drv"), 0).

-spec open() -> {ok, Handle::kstat_handle()} |
                kstat_error().
open() ->
    exit(nif_library_not_loaded).

-spec update(Handle::binary()) -> [kstat_row()].

update(_Handle) ->
    exit(nif_library_not_loaded).

-spec read(Handle::binary()) -> [kstat_row()].

read(_Handle) ->
    exit(nif_library_not_loaded).

-spec read(Handle::binary(), Class::string()) -> [kstat_row()].

read(_Handle, _Class) ->
    exit(nif_library_not_loaded).

-spec read(Handle::binary(), Class::string(), Module::string()) -> [kstat_row()].

read(_Handle, _Class, _Module) ->
    exit(nif_library_not_loaded).

-spec read(Handle::binary(), Class::string(), Module::string(), Instance::integer()) -> [kstat_row()].

read(_Handle, _Class, _Module, _Instance) ->
    exit(nif_library_not_loaded).

-spec read(Handle::binary(), Class::string(), Module::string(), Instance::integer(), Name::string()) -> [kstat_row()].

read(_Handle, _Class, _Module, _Instance, _Name) ->
    exit(nif_library_not_loaded).

-spec read(Handle::binary(), Class::string(), Module::string(), Instance::integer(), Name::string(), Statistic::string()) -> [kstat_row()].

read(_Handle, _Class, _Module, _Instance, _Name, _Statistic) ->
    exit(nif_library_not_loaded).

-spec pagesize() -> {ok, Pagesize::integer()}.
pagesize() ->
    exit(nif_library_not_loaded).

%% Tests.

-ifdef(TEST).

pagesize_test() ->
    Result = os:cmd("/usr/bin/pagesize"),
    {Pagesize, _} = string:to_integer(Result),
    {ok, Pagesize} = ekstat:pagesize().

-endif.
