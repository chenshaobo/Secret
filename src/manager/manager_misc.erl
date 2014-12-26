%%%-------------------------------------------------------------------
%%% @author Chenshaobo <chenshaobo65@gmail.com>
%%% @copyright (C) 2014, <MingChao>
%%% @doc
%%%
%%% @end
%%% Created : 22. 十二月 2014 16:52
%%%-------------------------------------------------------------------
-module(manager_misc).
-author("Chenshaobo <chenshaobo65@gmail.com>").

-author("chenshaobo").

%% API
-export([start/0,i/0]).

start()->
    db_misc:init(),
    application:start(gateway),
    application:start(account),
    application:start(content),
    manager_server:start_link(),
    reloader:start_link().



i()->
    gen_server:call(manager_server,i).