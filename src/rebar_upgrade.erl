%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2011 Joe Williams <joe@joetify.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------

-module(rebar_upgrade).

-include("rebar.hrl").

-export(['generate-upgrade'/2]).

%% public api

'generate-upgrade'(_Config, ReltoolFile) ->
    case rebar_config:get_global(previous_release, false) of
        false ->
            ?ABORT("previous_release=PATH is required to "
                   "create upgrade package~n", []);
        OldVerPath ->
            %% Run checks to make sure that building a package is possible
            {NewName, NewVer} = run_checks(OldVerPath, ReltoolFile),
            NameVer = NewName ++ "_" ++ NewVer,

            %% Save the code path prior to doing anything
            OrigPath = code:get_path(),

            %% Prepare the environment for building the package
            ok = setup(OldVerPath, NewName, NewVer, NameVer),

            %% Build the package
            run_systools(NameVer, NewName),

            %% Clean up files that systools created
            ok = cleanup(NameVer),

            %% Restore original path
            true = code:set_path(OrigPath),

            ok
    end.

%% internal api

run_checks(OldVerPath, ReltoolFile) ->
    true = release_path_check(OldVerPath),

    {Name, Ver} = get_release_name(ReltoolFile),
    NamePath = filename:join([".", Name]),
    true = release_path_check(NamePath),

    {NewName, NewVer} = get_release_version(Name, NamePath),
    {OldName, OldVer} = get_release_version(Name, OldVerPath),

    case release_name_check(NewName, OldName) of
        true ->
            ok;
        false ->
            ?ABORT("New and old .rel release names do not match~n", [])
    end,

    case release_name_check(Name, NewName) of
        true ->
            ok;
        false ->
            ?ABORT("Reltool and .rel release names do not match~n", [])
    end,

    case release_version_check(NewVer, OldVer) of
        true ->
            ?ABORT("New and old .rel contain the same version~n", []);
        false ->
            ok
    end,

    case release_version_check(Ver, NewVer) of
        true ->
            true;
        false ->
            ?ABORT("Reltool and .rel versions do not match~n", [])
    end,

    {NewName, NewVer}.

get_release_name(ReltoolFile) ->
    %% expect sys to be the first proplist in reltool.config
    case file:consult(ReltoolFile) of
        {ok, [{sys, Config}| _]} ->
            %% expect the first rel in the proplist to be the one you want
            {rel, Name, Ver, _} = proplists:lookup(rel, Config),
            {Name, Ver};
        _ ->
            ?ABORT("Failed to parse ~s~n", [ReltoolFile])
    end.

get_release_version(Name, Path) ->
    [RelFile] = filelib:wildcard(filename:join([Path, "releases", "*",
                                                Name ++ ".rel"])),
    [BinDir|_] = re:replace(RelFile, Name ++ "\\.rel", ""),
    {ok, [{release, {Name1, Ver}, _, _}]} =
        file:consult(filename:join([binary_to_list(BinDir),
                                    Name ++ ".rel"])),
    {Name1, Ver}.

release_path_check(Path) ->
    case filelib:is_dir(Path) of
        true ->
            true;
        false ->
            ?ABORT("Release directory doesn't exist (~p)~n", [Path])
    end.

release_version_check(Ver1, Ver2) when Ver1 == Ver2 ->
    true;
release_version_check(_, _) ->
    false.

release_name_check(Name1, Name2) when Name1 == Name2 ->
    true;
release_name_check(_, _) ->
    false.

setup(OldVerPath, NewName, NewVer, NameVer) ->
    NewRelPath = filename:join([".", NewName]),
    Src = filename:join([NewRelPath, "releases",
                         NewVer, NewName ++ ".rel"]),
    Dst = filename:join([".", NameVer ++ ".rel"]),
    {ok, _} = file:copy(Src, Dst),
    ok = code:add_pathsa(
           filelib:wildcard(filename:join([NewRelPath, "*"]))),
    ok = code:add_pathsa(
           filelib:wildcard(filename:join([NewRelPath, "lib", "*", "ebin"]))),
    ok = code:add_pathsa(
           filelib:wildcard(filename:join([OldVerPath, "lib", "*", "ebin"]))),
    ok = code:add_pathsa(
           filelib:wildcard(filename:join([OldVerPath, "releases", "*"]))).

run_systools(NewVer, Name) ->
    Opts = [silent],
    NameList = [Name],
    case systools:make_relup(NewVer, NameList, NameList, Opts) of
        {error, _, _Message} ->
            ?ABORT("Systools aborted with: ~p~n", [_Message]);
        _ ->
            ?DEBUG("Relup created~n", []),
            case systools:make_script(NewVer, Opts) of
                {error, _, _Message1} ->
                    ?ABORT("Systools aborted with: ~p~n", [_Message1]);
                _ ->
                    ?DEBUG("Script created~n", []),
                    case systools:make_tar(NewVer, Opts) of
                        {error, _, _Message2} ->
                            ?ABORT("Systools aborted with: ~p~n", [_Message2]);
                        _ ->
                            ?CONSOLE("~p upgrade package created~n", NameList)
                    end
            end
    end.

cleanup(NameVer) ->
    ?DEBUG("Removing files needed for building the upgrade~n", []),
    ok = file:delete(filename:join([".", NameVer ++ ".rel"])),
    ok = file:delete(filename:join([".", NameVer ++ ".boot"])),
    ok = file:delete(filename:join([".", NameVer ++ ".script"])),
    ok = file:delete(filename:join([".", "relup"])).
