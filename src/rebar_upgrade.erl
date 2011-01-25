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

-export([upgrade/2]).

%% public api

upgrade(_Config, ReltoolFile) ->
    case rebar_config:get_global(oldreleasepath, false) of
        false ->
            ?ABORT("oldreleasepath=PATH is required to "
                   "create upgrade package~n", []);
        OldVerPath ->
            %% Run checks to make sure that building a package is possible
            {NewReleaseName, NewReleaseVer} = run_checks(OldVerPath,
                                                         ReltoolFile),
            Release_NewVer = NewReleaseName ++ "_" ++ NewReleaseVer,

            %% Save the code path prior to doing anything
            OrigPath = code:get_path(),

            %% Prepare the environment for building the package
            ok = setup(OldVerPath, NewReleaseName,
                       NewReleaseVer, Release_NewVer),

            %% Build the package
            run_systools(Release_NewVer, NewReleaseName),

            %% Clean up files that systools created
            ok = cleanup(Release_NewVer),

            %% Restore original path
            true = code:set_path(OrigPath),

            ok
    end.

%% internal api

run_checks(OldVerPath, ReltoolFile) ->
    true = release_path_check(OldVerPath),

    {ReleaseName, ReleaseVersion} = get_release_name(ReltoolFile),
    ReleaseNamePath = filename:join([".", ReleaseName]),
    true = release_path_check(ReleaseNamePath),

    {NewReleaseName, NewReleaseVer} =
        get_release_version(ReleaseName, ReleaseNamePath),
    {OldReleaseName, OldReleaseVer} =
        get_release_version(ReleaseName, OldVerPath),

    case release_name_check(NewReleaseName, OldReleaseName) of
        true ->
            ok;
        false ->
            ?ABORT("New and old .rel release names do not match~n", [])
    end,

    case release_name_check(ReleaseName, NewReleaseName) of
        true ->
            ok;
        false ->
            ?ABORT("Reltool and .rel release names do not match~n", [])
    end,

    case release_version_check(NewReleaseVer, OldReleaseVer) of
        true ->
            ?ABORT("New and old .rel contain the same version~n", []);
        false ->
            ok
    end,

    case release_version_check(ReleaseVersion, NewReleaseVer) of
        true ->
            true;
        false ->
            ?ABORT("Reltool and .rel versions do not match~n", [])
    end,

    {NewReleaseName, NewReleaseVer}.

get_release_name(ReltoolFile) ->
    %% expect sys to be the first proplist in reltool.config
    case file:consult(ReltoolFile) of
        {ok, [{sys, Config}| _]} ->
            %% expect the first rel in the proplist to be the one you want
            {rel, Name, Version, _} = proplists:lookup(rel, Config),
            {Name, Version};
        _ ->
            ?ABORT("Failed to parse ~s~n", [ReltoolFile])
    end.

get_release_version(ReleaseName, Path) ->
    [RelFile] = filelib:wildcard(filename:join([Path, "releases", "*",
                                                ReleaseName ++ ".rel"])),
    [BinDir|_] = re:replace(RelFile, ReleaseName ++ "\\.rel", ""),
    {ok, [{release, {ReleaseName1, ReleaseVer}, _, _}]} =
        file:consult(filename:join([binary_to_list(BinDir),
                                    ReleaseName ++ ".rel"])),
    {ReleaseName1, ReleaseVer}.

release_path_check(Path) ->
    case filelib:is_dir(Path) of
        true ->
            true;
        false ->
            ?ABORT("Release directory doesn't exist (~p)~n", [Path])
    end.

release_version_check(Version1, Version2) when Version1 == Version2 ->
    true;
release_version_check(_, _) ->
    false.

release_name_check(Name1, Name2) when Name1 == Name2 ->
    true;
release_name_check(_, _) ->
    false.

setup(OldVerPath, NewReleaseName, NewReleaseVer, Release_NewVer) ->
    NewRelPath = filename:join([".", NewReleaseName]),
    Src = filename:join([NewRelPath, "releases",
                         NewReleaseVer, NewReleaseName ++ ".rel"]),
    Dst = filename:join([".", Release_NewVer ++ ".rel"]),
    {ok, _} = file:copy(Src, Dst),
    ok = code:add_pathsa(
           filelib:wildcard(filename:join([NewRelPath, "*"]))),
    ok = code:add_pathsa(
           filelib:wildcard(filename:join([NewRelPath, "lib", "*", "ebin"]))),
    ok = code:add_pathsa(
           filelib:wildcard(filename:join([OldVerPath, "lib", "*", "ebin"]))),
    ok = code:add_pathsa(
           filelib:wildcard(filename:join([OldVerPath, "releases", "*"]))).

run_systools(NewReleaseVer, ReleaseName) ->
    Opts = [silent],
    NameList = [ReleaseName],
    case systools:make_relup(NewReleaseVer, NameList, NameList, Opts) of
        {error, _, _Message} ->
            ?ABORT("Systools aborted with: ~p~n", [_Message]);
        _ ->
            ?DEBUG("Relup created~n", []),
            case systools:make_script(NewReleaseVer, Opts) of
                {error, _, _Message1} ->
                    ?ABORT("Systools aborted with: ~p~n", [_Message1]);
                _ ->
                    ?DEBUG("Script created~n", []),
                    case systools:make_tar(NewReleaseVer, Opts) of
                        {error, _, _Message2} ->
                            ?ABORT("Systools aborted with: ~p~n", [_Message2]);
                        _ ->
                            ?CONSOLE("~p upgrade package created~n", NameList)
                    end
            end
    end.

cleanup(Release_NewVer) ->
    ?DEBUG("Removing files needed for building the upgrade~n", []),
    ok = file:delete(filename:join([".", Release_NewVer ++ ".rel"])),
    ok = file:delete(filename:join([".", Release_NewVer ++ ".boot"])),
    ok = file:delete(filename:join([".", Release_NewVer ++ ".script"])),
    ok = file:delete(filename:join([".", "relup"])).
