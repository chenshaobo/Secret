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
-include("gateway.hrl").
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

-record(gateway, {account,pid,pname,gpid,gpname,ip,send_sock,listen_sock,state=disconnect}).

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
    {ok, #gateway{listen_sock = LSock,gpid = self(),gpname = Name},0}.



handle_call(_Request, _From, State) ->
    {reply, ok, State}.



handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(timeout,State)->
    ?PRINT("Wait for connect:~w",[?DEFAULT_PORT]),
    {ok,SendSock}=gen_tcp:accept(State#gateway.listen_sock),
    gateway_sup:start_child(),
    erlang:send(whereis(manager_server),{connect,self()}),
    ?PRINT("Connect :~w",[SendSock]),
    NewState=wait(SendSock,State),
    {noreply,NewState};
handle_info({login,Account,AccountPid},State)->
    true=erlang:unregister(State#gateway.gpname),
    erlang:register(list_to_atom(Account ++ "_GPID"),self()),
    {noreply,State#gateway{account = Account,pid = AccountPid,state = login}};
handle_info({tcp,Socket,RawData},State)->
    <<_Fin:1, _Rsv:3, _Opcode:4, _Mask:1, Len:7, Rest/binary>> = RawData,
    <<Masking:4/binary, Payload:Len/binary, _Next/binary>> = Rest,
    Line = unmask(Payload, Masking),
    case unicode:characters_to_list(Line) of
        {incomplete, _, _} ->
            gen_tcp:close(Socket),
            {noreply,State#gateway{state = disconnect}};
        {error,_C,Reason}->
            ?ERROR("~w",[Reason]),
            {noreply,State#gateway{state = disconnect}};
        Str ->
            do_handle_data(Str,State),
            do_send2client(Str,State),
            {noreply,State}
    end;
handle_info({send2client,Data},State)->
    do_send2client(Data,State),
    {noreply,State};
handle_info({tcp_closed,_Socket},State)->
    erlang:send(whereis(manager_server),{disconnect,self()}),
    Pid=State#gateway.pid,
    ?PRINT("TCP close:~w",[_Socket]),
    ?IF(erlang:is_pid(Pid),erlang:send(Pid,disconnect),false),
    erlang:exit(self(),disconnect),
    {noreply,State};
handle_info(_Info, State) ->
    ?PRINT("Receive:~p",[_Info]),
    {noreply, State}.



terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_handle_data(RawData,State=#gateway{state = handshake})->
    Record=erlang:list_to_tuple(RawData),
    ?PRINT("Receice:~w",[Record]),
    router:router(erlang:element(1,Record),Record,State#gateway.gpid);
do_handle_data(RawData,State=#gateway{state = login})->
    Record=erlang:binary_to_term(RawData),
    erlnag:send(State#gateway.pid,{router,Record}),
    ok.


register_name()->
    random:seed(erlang:now()),
    Name=erlang:list_to_atom("gateway_"++integer_to_list(random:uniform(1000000))),
    case whereis(Name) of
        undefined ->
            erlang:register(Name,self()),
            Name;
        _ ->
            register_name()
    end.


wait(Socket,State) ->
    ?PRINT("wait for handshake"),
    receive
        {tcp, Socket, HeaderData} ->
            HeaderList = binary:split(HeaderData, <<"\r\n">>, [global]),
            HeaderList1 = [list_to_tuple(binary:split(Header, <<": ">>)) || Header<-HeaderList,Header /= nomatch],
            {_, SecWebSocketKey} = lists:keyfind(<<"Sec-WebSocket-Key">>, 1, HeaderList1),
            Sha1 = crypto:hash(sha,[SecWebSocketKey, <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>]),
            Base64 = base64:encode(Sha1),
            Handshake = [
                <<"HTTP/1.1 101 Switching Protocols\r\n">>,
                <<"Upgrade: websocket\r\n">>,
                <<"Connection: Upgrade\r\n">>,
                <<"Sec-WebSocket-Accept: ">>, Base64, <<"\r\n">>,
                <<"\r\n">>
            ],
            gen_tcp:send(Socket, Handshake),
            ?PRINT("handshake finish,connect ok!!!"),
            State#gateway{send_sock = Socket,state = handshake};
        Any ->
            io:format("Received: ~p~n", [Any]),
            wait(Socket,State)
    end.


unmask(Payload, Masking) ->
    unmask(Payload, Masking, <<>>).

unmask(Payload, Masking = <<MA:8, MB:8, MC:8, MD:8>>, Acc) ->
    case size(Payload) of
        0 -> Acc;
        1 ->
            <<A>> = Payload,
            <<Acc/binary, (MA bxor A)>>;
        2 ->
            <<A:8, B:8>> = Payload,
            <<Acc/binary, (MA bxor A), (MB bxor B)>>;
        3 ->
            <<A:8, B:8, C:8>> = Payload,
            <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C)>>;
        _Other ->
            <<A:8, B:8, C:8, D:8, Rest/binary>> = Payload,
            Acc1 = <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C), (MD bxor D)>>,
            unmask(Rest, Masking, Acc1)
    end.


do_send2client(Data,State)->
    %Bin=erlang:term_to_binary(Data),
    Bin = unicode:characters_to_binary(Data),
    Frame = <<1:1, 0:3, 2:4, 0:1, (size(Bin)):7, Bin/binary>>,
    ?PRINT("Frame =~w,Size=~w,Bin=~w",[Frame,size(Bin),Bin]),
    gen_tcp:send(State#gateway.send_sock, Frame).