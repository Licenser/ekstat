-module(ekstat).

%% API
-export([open/0]).
-export([close/1]).
-export([clear/1]).
-export([update/1]).
-export([read/1]).
-export([read/2]).
-export([read/3]).
-export([read/4]).
-export([read/5]).
-export([read/6]).

-on_load(init/0).

-include("async_nif.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Types
-type kstat_handle() ::
    binary().
-type kstat_pattern() ::
    '_' | integer() | string().
-type kstat_error() ::
    {error, atom()} | {error, {atom(), string()}}.
-type kstat_statistic() ::
    {Statistic::string(), Value::integer()} |
    {Statistic::string(), Value::string()}.
-type kstat_row() ::
    {Class::string(), Module::string(), Instance::integer(),
        Name::string(), [kstat_statistic()]}.

-export_type([kstat_handle/0, kstat_pattern/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec open() ->
    {ok, Handle::kstat_handle()} | kstat_error().
open() ->
    ?ASYNC_NIF_CALL(fun open_nif/1,
        []).

-spec close(Handle) ->
    ok | kstat_error()
    when
        Handle :: binary().
close(Handle) ->
    ?ASYNC_NIF_CALL(fun close_nif/2,
        [Handle]).

-spec clear(Handle) ->
    {ok, NumInstances::integer()} | kstat_error()
    when
        Handle :: binary().
clear(Handle) ->
    ?ASYNC_NIF_CALL(fun clear_nif/2,
        [Handle]).

-spec update(Handle) ->
    {ok, NumInstances::integer()} | kstat_error()
    when
        Handle :: binary().
update(Handle) ->
    ?ASYNC_NIF_CALL(fun update_nif/2,
        [Handle]).

-spec read(Handle) ->
    [kstat_row()] | kstat_error()
    when
        Handle :: binary().
read(Handle) ->
    ?ASYNC_NIF_CALL(fun read_nif/2,
        [Handle]).

-spec read(Handle, Class) ->
    [kstat_row()] | kstat_error()
    when
        Handle :: binary(),
        Class :: kstat_pattern().
read(Handle, Class) ->
    ?ASYNC_NIF_CALL(fun read_nif/3,
        [Handle, Class]).

-spec read(Handle, Class, Module) ->
    [kstat_row()] | kstat_error()
    when
        Handle :: binary(),
        Class :: kstat_pattern(),
        Module :: kstat_pattern().
read(Handle, Class, Module) ->
    ?ASYNC_NIF_CALL(fun read_nif/4,
        [Handle, Class, Module]).

-spec read(Handle, Class, Module, Instance) ->
    [kstat_row()] | kstat_error()
    when
        Handle :: binary(),
        Class :: kstat_pattern(),
        Module :: kstat_pattern(),
        Instance :: kstat_pattern().
read(Handle, Class, Module, Instance) ->
    ?ASYNC_NIF_CALL(fun read_nif/5,
        [Handle, Class, Module, Instance]).

-spec read(Handle, Class, Module, Instance, Name) ->
    [kstat_row()] | kstat_error()
    when
        Handle :: binary(),
        Class :: kstat_pattern(),
        Module :: kstat_pattern(),
        Instance :: kstat_pattern(),
        Name :: kstat_pattern().
read(Handle, Class, Module, Instance, Name) ->
    ?ASYNC_NIF_CALL(fun read_nif/6,
        [Handle, Class, Module, Instance, Name]).

-spec read(Handle, Class, Module, Instance, Name, Statistic) ->
    [kstat_row()] | kstat_error()
    when
        Handle :: binary(),
        Class :: kstat_pattern(),
        Module :: kstat_pattern(),
        Instance :: kstat_pattern(),
        Name :: kstat_pattern(),
        Statistic :: kstat_pattern().
read(Handle, Class, Module, Instance, Name, Statistic) ->
    ?ASYNC_NIF_CALL(fun read_nif/7,
        [Handle, Class, Module, Instance, Name, Statistic]).

%%%-------------------------------------------------------------------
%%% NIF functions
%%%-------------------------------------------------------------------

open_nif(_AsyncRef) ->
    erlang:nif_error(undefined).

close_nif(_AsyncRef, _Handle) ->
    erlang:nif_error(undefined).

clear_nif(_AsyncRef, _Handle) ->
    erlang:nif_error(undefined).

update_nif(_AsyncRef, _Handle) ->
    erlang:nif_error(undefined).

read_nif(_AsyncRef, _Handle) ->
    erlang:nif_error(undefined).

read_nif(_AsyncRef, _Handle, _Class) ->
    erlang:nif_error(undefined).

read_nif(_AsyncRef, _Handle, _Class, _Module) ->
    erlang:nif_error(undefined).

read_nif(_AsyncRef, _Handle, _Class, _Module, _Instance) ->
    erlang:nif_error(undefined).

read_nif(_AsyncRef, _Handle, _Class, _Module, _Instance, _Name) ->
    erlang:nif_error(undefined).

read_nif(_AsyncRef, _Handle, _Class, _Module, _Instance, _Name, _Statistic) ->
    erlang:nif_error(undefined).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
-spec init() ->
    ok | {error, {Reason, Text::string()}}
    when
        Reason :: load_failed | bad_lib | load | reload | upgrade | old_code.
init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, _} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, "ekstat_nif"), 0).

%% Tests.

-ifdef(TEST).

sanity_test() ->
    {ok, Handle} = ekstat:open(),
    N0 = length(ekstat:read(Handle)),
    {ok, N0} = ekstat:clear(Handle),
    {ok, N1} = ekstat:update(Handle),
    N1 = length(ekstat:read(Handle)),
    {ok, N1} = ekstat:clear(Handle),
    {ok, 0} = ekstat:clear(Handle),
    [] = ekstat:read(Handle),
    ok = ekstat:close(Handle),
    ok.

-endif.
