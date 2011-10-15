%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
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
-module(rebar_commands).

%% NB: this is a very *stateful* approach - I've stuck with using rebar_config
%% get_global/2 because that's the current status quo, but perhaps we should
%% consider refactoring so that all Mod:preprocess/2 and/or Mod:Command/2
%% calls can return {ok, AddedCmds} instead. That's a *LOT* more work though.

%% peek accessors
-export([current/0,
         remaining/0,
         contains/1,
         processed/0,
         issued/0]).

%% deque-like interface for managing the pipeline
-export([enqueue_first/1,
         enqueue_last/1]).

%% interface to rebar_core
-export([dequeue/0, initialize/1]).

-include("rebar.hrl").

%%
%% Public API
%%

current() ->
    rebar_config:get_global(current_command, undefined).

remaining() ->
    rebar_config:get_global(remaining_commands, undefined).

contains(Command) ->
    lists:member(Command, remaining()).

processed() ->
    rebar_config:get_global(processed_commands, undefined).

issued() ->
    rebar_config:get_global(issued_commands, undefined).

enqueue_first(Command) ->
    %% we can't change the current command, so we prepend to remaining
    rebar_config:add_global(remaining_commands, Command).

enqueue_last(Command) ->
    Previous = rebar_config:get_global(remaining_commands, []),
    rebar_config:set_global(remaining_commands, Previous ++ [Command]).

dequeue() ->
    case rebar_config:get_global(remaining_commands, []) of
        [] ->
            eoi;
        [Cmd|Remaining] ->
            rebar_config:set_global(remaining_commands, Remaining),
            rebar_config:set_global(current_command, Cmd),
            Cmd
    end.

initialize(Commands) ->
    %% Publish the originally given commands
    rebar_config:set_global(issued_commands, Commands),
    rebar_config:set_global(remaining_commands, Commands).
