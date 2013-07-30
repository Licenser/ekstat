-module(kstat_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests.
-export([
    read_1/1,
    read_2/1,
    read_3/1,
    read_4/1,
    read_5/1,
    read_6/1
]).

all() ->
    [
        {group, test_kstat}
    ].

groups() ->
    Tests = [
        read_1,
        read_2,
        read_3,
        read_4,
        read_5,
        read_6
    ],
    [
        {test_kstat, [parallel], Tests}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(test_kstat, Config) ->
    ct:log("opening kstat..."),
    {ok, KStat} = ekstat:open(),
    {ok, Stats} = ekstat:update(KStat),
    ct:log("updated ~p statistics", [Stats]),
    [{kstat, KStat}, {stats, Stats} | Config].

end_per_group(test_kstat, Config) ->
    KStat = ?config(kstat, Config),
    Stats = ?config(stats, Config),
    {ok, Stats} = ekstat:clear(KStat),
    ct:log("cleared ~p statistics", [Stats]),
    ok.

%%====================================================================
%% Tests
%%====================================================================

read_1(Config) ->
    KStat = ?config(kstat, Config),
    Stats = ?config(stats, Config),
    Stats = length(ekstat:read(KStat)),
    ok.

read_2(Config) ->
    KStat = ?config(kstat, Config),
    Stats = ?config(stats, Config),
    {error, {kstat, "repetition-operator operand invalid"}} = ekstat:read(KStat, "/*/"),
    Stats = length(ekstat:read(KStat, '_')),
    Stats = length(ekstat:read(KStat, "*")),
    [] = ekstat:read(KStat, "__unknown__"),
    ok.

read_3(Config) ->
    KStat = ?config(kstat, Config),
    Stats = ?config(stats, Config),
    {error, {kstat, "repetition-operator operand invalid"}} = ekstat:read(KStat, '_', "/*/"),
    Stats = length(ekstat:read(KStat, '_', '_')),
    Stats = length(ekstat:read(KStat, "*", "*")),
    [] = ekstat:read(KStat, '_', "__unknown__"),
    ok.

read_4(Config) ->
    KStat = ?config(kstat, Config),
    Stats = ?config(stats, Config),
    {error, {kstat, "repetition-operator operand invalid"}} = ekstat:read(KStat, '_', '_', "/*/"),
    Stats = length(ekstat:read(KStat, '_', '_', '_')),
    Stats = length(ekstat:read(KStat, "*", "*", "*")),
    [] = ekstat:read(KStat, '_', '_', "__unknown__"),
    ok.

read_5(Config) ->
    KStat = ?config(kstat, Config),
    Stats = ?config(stats, Config),
    {error, {kstat, "repetition-operator operand invalid"}} = ekstat:read(KStat, '_', '_', '_', "/*/"),
    Stats = length(ekstat:read(KStat, '_', '_', '_', '_')),
    Stats = length(ekstat:read(KStat, "*", "*", "*", "*")),
    [] = ekstat:read(KStat, '_', '_', '_', "__unknown__"),
    ok.

read_6(Config) ->
    KStat = ?config(kstat, Config),
    Stats = ?config(stats, Config),
    {error, {kstat, "repetition-operator operand invalid"}} = ekstat:read(KStat, '_', '_', '_', '_', "/*/"),
    Stats = length(ekstat:read(KStat, '_', '_', '_', '_', '_')),
    Stats = length(ekstat:read(KStat, "*", "*", "*", "*", "*")),
    [] = ekstat:read(KStat, '_', '_', '_', '_', "__unknown__"),
    true = kstat_compare(ekstat:read(KStat, '_', '_', '_', '_', "freemem"), [
        {"pages","unix",int,"system_pages",[{"freemem",int}]},
        {"vm","unix",int,"vminfo",[{"freemem",int}]}
    ]),
    true = kstat_compare(ekstat:read(KStat, '_', '_', '_', '_', "/freemem|updates/"), [
        {"misc","unix",int,"pset",[{"updates",int}]},
        {"misc","unix",int,"sysinfo",[{"updates",int}]},
        {"pages","unix",int,"system_pages",[{"freemem",int}]},
        {"vm","unix",int,"vminfo",[{"freemem",int},{"updates",int}]}
    ]),
    ok.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%% @private
kstat_compare([], []) ->
    true;
kstat_compare([StatA | StatsA], [StatB | StatsB]) ->
    true = kstat_matches(StatA, StatB),
    kstat_compare(StatsA, StatsB);
kstat_compare(_A, _B) ->
    false.

%% @private
kstat_matches({Class, Module, Instance, Name, StatsA}, {Class, Module, int, Name, StatsB}) ->
    kstat_matches({Class, Module, Instance, Name, StatsA}, {Class, Module, Instance, Name, StatsB});
kstat_matches({Class, Module, Instance, Name, StatsA}, {Class, Module, Instance, Name, StatsB}) ->
    kstat_matches_statistics(StatsA, StatsB);
kstat_matches(_A, _B) ->
    false.

%% @private
kstat_matches_statistics([], []) ->
    true;
kstat_matches_statistics([{Key, Value} | KeyValsA], [{Key, int} | KeyValsB]) ->
    kstat_matches_statistics([{Key, Value} | KeyValsA], [{Key, Value} | KeyValsB]);
kstat_matches_statistics([{Key, Value} | KeyValsA], [{Key, Value} | KeyValsB]) ->
    kstat_matches_statistics(KeyValsA, KeyValsB);
kstat_matches_statistics(_A, _B) ->
    false.
