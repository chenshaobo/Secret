%%%-------------------------------------------------------------------
%%% @author Chenshaobo <chenshaobo65@gmail.com>
%%% @copyright (C) 2014, <MingChao>
%%% @doc
%%%
%%% @end
%%% Created : 24. 十二月 2014 11:31
%%%-------------------------------------------------------------------
-module(gateway_client_server).
-author("Chenshaobo <chenshaobo65@gmail.com>").

-behaviour(gen_server).
-include("debug.hrl").
%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(gateway, {account,pid,pname,gpid,gpname,ip,send_sock,listen_scok,state=disconnect}).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------

start_link(LSock) ->
    gen_server:start_link({local,?SERVER}, ?MODULE, [LSock], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------

init([LSock]) ->
    erlang:unregister(gateway_client_server),
    Name=register_name(),
    {ok, #gateway{listen_scok = LSock,gpid = self(),gpname = Name},0}.



handle_call(_Request, _From, State) ->
    {reply, ok, State}.



handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(timeout,State)->
    {ok,SendSock}=gen_tcp:accept(State#gateway.listen_scok),
    gateway_sup:start_child(),
    erlang:send(whereis(manager_server),{connect,self()}),
    ?PRINT("Connect :~w",[SendSock]),
    {noreply,State#gateway{send_sock = SendSock,state = connect}};
handle_info({login,Account,AccountPid},State)->
    true=erlang:unregister(State#gateway.gpname),
    erlang:register(list_to_atom(Account ++ "_GPID"),self()),
    {noreply,State#gateway{account = Account,pid = AccountPid,state = login}};
handle_info({tcp,_Socket,RawData},State)->
    ?PRINT("Receive :~p",[RawData]),
    NewState=do_handle_data(RawData,State),
    {noreply,NewState};
handle_info({tcp_closed,_Socket},State)->
    erlang:send(whereis(manager_server),{disconnect,self()}),
    Pid=State#gateway.pid,
    ?PRINT("TCP close:~w",[_Socket]),
    ?IF(erlang:is_pid(Pid),erlang:send(Pid,disconnect),false),
    erlang:exit(self(),disconnect),
    {noreply,State};
handle_info(_Info, State) ->
    ?PRINT("Receice:~p",[_Info]),
    {noreply, State}.



terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_handle_data(RawData,State=#gateway{state = connect})->
    HeaderList = binary:split(RawData, <<"\r\n">>, [global]),
    HeaderList1 = [list_to_tuple(binary:split(HeaderList, <<": ">>)) || Header <-HeaderList,Header /= nomatch],
    {_, SecWebSocketKey} = lists:keyfind(<<"Sec-WebSocket-Key">>, 1, HeaderList1),
    Sha1 = crypto:sha([SecWebSocketKey, <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>]),
    Base64 = base64:encode(Sha1),
    Handshake = [
        <<"HTTP/1.1 101 Switching Protocols\r\n">>,
        <<"Upgrade: websocket\r\n">>,
        <<"Connection: Upgrade\r\n">>,
        <<"Sec-WebSocket-Accept: ">>, Base64, <<"\r\n">>,
        <<"\r\n">>
    ],
    ?PRINT("Send：~p",[Handshake]),
    ok=gen_tcp:send(State#gateway.send_sock, Handshake),
    State#gateway{state=handshake};
do_handle_data(RawData,State=#gateway{state = handshake})->
    Record=erlang:binary_to_term(RawData),
    %?PRINT("Receice:~w",[Record]),
    router:router(erlang:element(1,Record),Record,State#gateway.gpid);
do_handle_data(RawData,State=#gateway{state = login})->
    Record=erlang:binary_to_term(RawData),
    erlnag:send(State#gateway.pid,{router,Record}),
    ok.


register_name()->
    random:seed(erlang:now()),
    Name=erlang:list_to_atom(integer_to_list(random:uniform(1000000))),
    case whereis(Name) of
        undefined ->
            erlang:register(Name,self()),
            Name;
        _ ->
            register_name()
    end.
