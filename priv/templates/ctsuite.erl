%% common_test suite for {{sut}}

-module({{sut}}_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%% common tests macros and functions

-define(TESTDOC(Doc), [{userdata,[{doc,Doc}]}]).
-define(NOT_IMPLEMENTED, {skip,"Not implemented."}).
-define(EXPORT_TESTS(Mod),
	[ {exports, Functions} | _ ] = Mod:module_info(),
    [ FName || {FName, _} <- lists:filter(
            fun ({module_info,_}) -> false ;
                ({all,_}) -> false ;
                ({init_per_suite,1}) -> false ;
                ({end_per_suite,1}) -> false ;
                ({_,1}) -> true ;
                ({_,_}) -> false
            end,
            Functions
        )
    ].

%% automatically registers all exported functions as test cases
all() ->
    ?EXPORT_TESTS(?MODULE).

test_{{sut}}() ->
    ?TESTDOC("Testing the {{sut}} module").

test_{{sut}}(_Config) ->
    ?NOT_IMPLEMENTED.
