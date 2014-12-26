%%%-------------------------------------------------------------------
%%% @author Chenshaobo <chenshaobo65@gmail.com>
%%% @copyright (C) 2014, <MingChao>
%%% @doc
%%%
%%% @end
%%% Created : 24. 十二月 2014 11:29
%%%-------------------------------------------------------------------
-module(gateway).
-author("Chenshaobo <chenshaobo65@gmail.com>").

-behaviour(application).

-include("gateway.hrl").
%% Application callbacks
-export([start/2,
    stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    Port=case application:get_env(tcp_listen)of
             {ok,P}->P;
             _ ->?DEFAULT_PORT
         end,
    {ok,LSock}=gen_tcp:listen(Port,?LISTEN_OPTION),
    case gateway_sup:start_link(LSock) of
        {ok, Pid} ->
            gateway_sup:start_child(),
            {ok, Pid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
