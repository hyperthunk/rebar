-module(thooks_rt).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

files() ->
    [
      %% dummy lfe files
      {copy, "gps1.lfe", "src/gps1.lfe"},
      {copy, "rfc4234.abnf", "src/rfc4234.abnf"},
      {copy, "People.asn1", "asn1/People.asn1"},
      {copy, "../../rebar", "rebar"},
      {copy, "rebar.config", "rebar.config"},
      {copy, "fish.erl", "src/fish.erl"},
      {copy, "fish.c", "c_src/fish.c"},
      {create, "priv/lib/README", "nothing to see here!"},
      {create, "ebin/fish.app", app(fish, [fish])}
    ].

run(Dir) ->
    BaseDir = get_test_dir(Dir),
    Stubs = ["abnfc.erl", "lfe_comp.erl", "asn1ct.erl", "neotoma.erl"],
    lists:foreach(fun(S) -> ?assertMatch({ok,_}, 
        compile:file(filename:join(BaseDir, S))) end, Stubs),
    ?assertMatch({ok, _}, 
        retest_sh:run("./rebar -v clean compile", [])),
    
    ensure_command_ran_only_once("preclean"),
    
    ensure_precompile_premodule_commands_ran(),
    
    ensure_port_env_passed_to_command("precompile"),
    
    ensure_command_ran_only_once("postclean"),
    
    ensure_postcompile_postmodule_commands_ran(),
    
    ensure_port_env_passed_to_command("postcompile"),
    
    ok.

ensure_precompile_premodule_commands_ran() ->
    ?assertEqual(true, filelib:is_file("precompile.erlc.out")),
    ?assertEqual(true, filelib:is_file("precompile.otp_app.out")),
    ?assertEqual(true, filelib:is_file("precompile.abnfc.out")),
    ?assertEqual(true, filelib:is_file("precompile.lfe.out")),
    ?assertEqual(true, filelib:is_file("precompile.asn1.out")),
    ?assertEqual(true, filelib:is_file("precompile.neotoma.out")),
    ?assertEqual(true, filelib:is_file("precompile.port.out")).

ensure_postcompile_postmodule_commands_ran() ->
    ?assertEqual(true, filelib:is_file("postcompile.erlc.out")),
    ?assertEqual(true, filelib:is_file("postcompile.otp_app.out")),
    ?assertEqual(true, filelib:is_file("postcompile.abnfc.out")),
    ?assertEqual(true, filelib:is_file("postcompile.lfe.out")),
    ?assertEqual(true, filelib:is_file("postcompile.asn1.out")),
    ?assertEqual(true, filelib:is_file("postcompile.neotoma.out")),
    ?assertEqual(true, filelib:is_file("postcompile.port.out")).

ensure_port_env_passed_to_command(Command) ->
    {ok, Content} = file:read_file(Command ++ ".port.out"),
    ?assert(string:str(binary_to_list(Content), "-Wl,-rpath priv/lib") =/= 0).

ensure_command_ran_only_once(Command) ->
    File = Command ++ ".out",
    ?assertEqual(true, filelib:is_file(File)),
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

