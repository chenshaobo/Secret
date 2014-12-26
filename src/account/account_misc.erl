%%%-------------------------------------------------------------------
%%% @author Chenshaobo <chenshaobo65@gmail.com>
%%% @copyright (C) 2014, <MingChao>
%%% @doc
%%%
%%% @end
%%% Created : 25. 十二月 2014 17:52
%%%-------------------------------------------------------------------
-module(account_misc).
-author("Chenshaobo <chenshaobo65@gmail.com>").

%% API
-export([send_g/2]).

send_g(Pid,Requst)->
    erlang:send(Pid,Requst).
