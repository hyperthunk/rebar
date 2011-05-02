-module(tplugins_rt).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(COMPILE_ERROR, 
    "ERROR: Plugin bad_plugin contains compilation errors:").

files() ->
    [
     {copy, "../../rebar", "rebar"},
     {copy, "rebar.config", "rebar.config"},
     {copy, "bad.config", "bad.config"},
     {copy, "fish.erl", "src/fish.erl"},
     {copy, "test_plugin.erl", "plugins/test_plugin.erl"},
     {copy, "bad_plugin.erl", "plugins/bad_plugin.erl"},
     {create, "fwibble.test", <<"fwibble">>},
     {create, "ebin/fish.app", app(fish, [fish])}
    ].

run(_Dir) ->
    ?assertMatch({ok, _}, retest_sh:run("./rebar -v clean", [])),
    ?assertEqual(false, filelib:is_regular("fwibble.test")),
    {ok, Out} = retest_sh:run("./rebar -C bad.config -v clean", []),
    ?assertEqual(true, 
        lists:any(fun(X) -> lists:prefix(?COMPILE_ERROR, X) end, Out)),
    ?assertEqual(true, lists:member("WARN:  Missing plugins: bad_plugin", Out)),
    ok.

%%
%% Generate the contents of a simple .app file
%%
app(Name, Modules) ->
    App = {application, Name,
           [{description, atom_to_list(Name)},
            {vsn, "1"},
            {modules, Modules},
            {registered, []},
            {applications, [kernel, stdlib]}]},
    io_lib:format("~p.\n", [App]).
