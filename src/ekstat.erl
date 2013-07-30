-module(ekstat).

%% API
-export([open/0, update/1, clear/1, read/1, read/2, read/3, read/4, read/5, read/6]).

-on_load(init/0).

%% Types
-type kstat_handle() :: binary().
-type kstat_pattern() :: '_' | integer() | string().
-type kstat_error() :: {error, {kstat, string()}}.
-type kstat_statistic() :: {Statistic::string(), Value::integer()} | {Statistic::string(), Value::string()}.
-type kstat_row() :: {Class::string(), Module::string(), Instance::integer(), Name::string(), [kstat_statistic()]}.

-export_type([kstat_handle/0, kstat_pattern/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec open() -> {ok, Handle::kstat_handle()} | kstat_error().
open() ->
    erlang:nif_error(undefined).

-spec update(Handle::binary()) -> {ok, NumInstances::integer()} | kstat_error().
update(_Handle) ->
    erlang:nif_error(undefined).

-spec clear(Handle::binary()) -> {ok, NumInstances::integer()} | kstat_error().
clear(_Handle) ->
    erlang:nif_error(undefined).

-spec read(Handle::binary()) -> [kstat_row()] | kstat_error().
read(_Handle) ->
    erlang:nif_error(undefined).

-spec read(Handle::binary(), Class::kstat_pattern()) -> [kstat_row()] | kstat_error().
read(_Handle, _Class) ->
    erlang:nif_error(undefined).

-spec read(Handle::binary(), Class::kstat_pattern(), Module::kstat_pattern()) -> [kstat_row()] | kstat_error().
read(_Handle, _Class, _Module) ->
    erlang:nif_error(undefined).

-spec read(Handle::binary(), Class::kstat_pattern(), Module::kstat_pattern(), Instance::kstat_pattern()) -> [kstat_row()] | kstat_error().
read(_Handle, _Class, _Module, _Instance) ->
    erlang:nif_error(undefined).

-spec read(Handle::binary(), Class::kstat_pattern(), Module::kstat_pattern(), Instance::kstat_pattern(), Name::kstat_pattern()) -> [kstat_row()] | kstat_error().
read(_Handle, _Class, _Module, _Instance, _Name) ->
    erlang:nif_error(undefined).

-spec read(Handle::binary(), Class::kstat_pattern(), Module::kstat_pattern(), Instance::kstat_pattern(), Name::kstat_pattern(), Statistic::kstat_pattern()) -> [kstat_row()] | kstat_error().
read(_Handle, _Class, _Module, _Instance, _Name, _Statistic) ->
    erlang:nif_error(undefined).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
-spec init() -> ok | {error, {Reason::load_failed | bad_lib | load | reload | upgrade | old_code, Text::string()}}.
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

%% Tests.

-ifdef(TEST).

sanity_test() ->
    {ok, Handle} = ekstat:open(),
    [] = ekstat:read(Handle),
    {ok, Number} = ekstat:update(Handle),
    Number = length(ekstat:read(Handle)),
    {ok, Number} = ekstat:clear(Handle),
    {ok, 0} = ekstat:clear(Handle),
    ok.

-endif.
