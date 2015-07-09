%%%-------------------------------------------------------------------
%%% @author patrick
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Apr 2015 10:02
%%%-------------------------------------------------------------------
-module(ehome_utils).
-author("patrick").

%% API
-export([id2str/1, parse_path/1, maybe/2, to_string/1]).

id2str(Id) ->
    binary:list_to_bin(integer_to_list(Id)).


-spec parse_path(undefined | binary() | string()) ->
    undefined | [any()].
parse_path(undefined) ->
    undefined;
parse_path(Path) when is_binary(Path) ->
    parse_path(binary_to_list(Path));
parse_path(Path) when is_list(Path) ->
    case erl_scan:string(Path ++ ".") of
        {ok, Tokens, _} -> case erl_parse:parse_term(Tokens) of
                               {ok, Val} when is_list(Val) ->
                                   Val;
                               {ok, Val} ->
                                   lager:error("Not a list: ~p", [Val]),
                                   undefined;
                               {error, Reason} ->
                                   lager:error("Cannot parse <~s>: ~p", [Path, Reason]),
                                   undefined
                           end;
        {error, ErrorInfo, _} ->
            lager:error("Cannot tokenize <~s>: ~p", [Path, ErrorInfo]),
            undefined
    end.

maybe(undefined, A) -> A;
maybe(A, _) -> A.

to_string(List) when is_list(List) ->
    List;
to_string(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_string(Number) when is_integer(Number) ->
    integer_to_list(Number).
