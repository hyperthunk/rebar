-module(thooks_rt).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

files() ->
    [
      %% dependency app
      
      {create, "repo/a/ebin/a.app", app(a, [a])},
      {copy, "a.rebar.config", "repo/a/rebar.config"},
      {copy, "a.erl", "repo/a/src/a.erl"},
      
      
      %% dummy lfe files
      {copy, "dummy_ping.lfe", "src/dummy_ping.lfe"},
      {copy, "rfc3325.abnf", "src/rfc3325.abnf"},
      {copy, "people.asn1", "asn1/people.asn1"},
      {copy, "../../rebar", "rebar"},
      {copy, "rebar.config", "rebar.config"},
      {copy, "fish.erl", "src/fish.erl"},
      {copy, "fish.c", "c_src/fish.c"},
      {create, "priv/lib/README", "nothing to see here!"},
      {create, "ebin/fish.app", app(fish, [fish])}
    ].

run(Dir) ->
    HgCmd = "/bin/sh -c \"hg init && hg add && hg commit -m 'Initial commit'\"",
    {ok, _} = retest_sh:run(HgCmd, [{dir, "repo/a"}]),

    BaseDir = get_test_dir(Dir),
    RunDir = filename:join(filename:join(BaseDir, "rt.work"), "current"),
    Stubs = ["abnfc.erl", "lfe_comp.erl", "asn1ct.erl", "neotoma.erl"],
    lists:foreach(fun(S) -> ?assertMatch({ok,_},
        compile:file(filename:join(BaseDir, S))) end, Stubs),
    ?assertMatch({ok, _},
        retest_sh:run("./rebar -v get-deps clean compile", [])),

    ensure_command_ran_only_once("preclean", "preclean.out"),
    ensure_command_ran_only_once("preclean", filename:join(filename:join("deps", "a"), "a.preclean.out")),

    ensure_precompile_premodule_commands_ran(),

    ensure_port_env_passed_to_command("precompile"),

    ensure_command_ran_only_once("postclean", "postclean.out"),
    ensure_command_ran_only_once("postclean", filename:join(filename:join("deps", "a"), "a.postclean.out")),

    ensure_postcompile_postmodule_commands_ran(),

    ensure_port_env_passed_to_command("postcompile"),

    ok.

ensure_precompile_premodule_commands_ran() ->
    ?assert(filelib:is_regular("precompile.erlc.out")),
    ?assert(filelib:is_regular("precompile.abnfc.out")),
    ?assert(filelib:is_regular("precompile.lfe.out")),
    ?assert(filelib:is_regular("precompile.asn1.out")),
    ?assert(filelib:is_regular("precompile.neotoma.out")),
    ?assert(filelib:is_regular("precompile.port.out")).

ensure_precompile_premodule_commands_ran_in_deps_dir() ->
    ?assert(filelib:is_regular("repo/a/a.precompile.erlc.out")).

ensure_postcompile_postmodule_commands_ran() ->
    ?assert(filelib:is_regular("postcompile.erlc.out")),
    ?assert(filelib:is_regular("postcompile.abnfc.out")),
    ?assert(filelib:is_regular("postcompile.lfe.out")),
    ?assert(filelib:is_regular("postcompile.asn1.out")),
    ?assert(filelib:is_regular("postcompile.neotoma.out")),
    ?assert(filelib:is_regular("postcompile.port.out")).

ensure_postcompile_postmodule_commands_ran_in_deps_dir() ->
    ?assert(filelib:is_regular("repo/a/a.postcompile.erlc.out")).


ensure_port_env_passed_to_command(Command) ->
    {ok, Content} = file:read_file(Command ++ ".port.out"),
    ?assert(string:str(binary_to_list(Content), "-Wl,-rpath priv/lib") =/= 0).

ensure_command_ran_only_once(Command, File) ->
    ?assert(filelib:is_regular(File)),
    %% ensure that this command only ran once (not for each module)
    {ok, Content} = file:read_file(File),
    ?assertEqual(Command ++ "\n", binary_to_list(Content)).

get_test_dir(Dir) ->
    Path = filename:split(Dir),
    Folder = lists:last(Path),
    filename:join(lists:sublist(Path, length(Path) - 3) ++
        [string:substr(Folder, 1, length(Folder) - 3)]).

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
