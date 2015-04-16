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
-export([id2str/1]).

id2str(Id) ->
    binary:list_to_bin(integer_to_list(Id)).
