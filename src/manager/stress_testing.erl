%%%-------------------------------------------------------------------
%%% @author chenshaobo
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 二月 2014 下午7:59
%%%-------------------------------------------------------------------
%%coding: utf-8
-module(stress_testing).
-author("chenshaobo").

-define(LOCAL,{127,0,0,1}).
%% API
-export([start/0,stop/0,create_robot/4]).
start() ->
    Max=1000,
    IP={127,0,0,1},
    PORT=8888,
    erlang:spawn_link(?MODULE,create_robot,[IP,PORT,0,Max]).

stop()->
    L= get(pidL),
    lists:foreach(fun(Pid) -> exit(Pid,{close}) end,L).

create_robot(_IP,_PORT,Max,Max)->
    ok;
create_robot(IP,PORT,Num,Max) ->
    case gen_tcp:connect(IP,PORT,[binary,{packet,4},{active,false}]) of
        {ok,Socket}->
            Account="guest" ++ integer_to_list(Num),
            %AccountB=list_to_binary(Account),
            %AccountL=integer_to_list(erlang:bit_size(AccountB)),
            Pro={0002,Account,1},
            case gen_tcp:send(Socket,erlang:term_to_binary(Pro)) of
               ok ->
                   erlang:spawn_link(?MODULE,create_robot,[IP,PORT,Num+1,Max]),
                   handle_recv(Socket,Num,erlang:now());
               {error,Re}->
                   handle_recv(Socket,Num,erlang:now()),
                   io:fwrite("Cant not login ~w~n",[Re])
            end;
        {error,_}->
            erlang:spawn_link(?MODULE,create_robot,[IP,PORT,Num+1,Max])
    end.

handle_recv(Socket,N,Time)->
    {OH,OM,OS}=Time,
    timer:sleep(1000),
    {NH,NM,NS}=time(),
    T=(NS+(NM+NH*60)*60) - (OS+(OH*60+OM)*60),
    around_msg_2s(Socket,N,T),
    around_msg_10s(Socket,N,T),
    case world_or_country_msg(Socket,N,T) of
        ok ->
            handle_recv(Socket,N,time());
        false ->
            handle_recv(Socket,N,Time)
    end.


around_msg_2s(Socket,N,T)->
    %io:fwrite("around time =~w~n",[T]),
    if
        T rem 2 =:= 0 ->
            Data1=msg_to_binary({integer_to_list(141) , "[附近]guest"++integer_to_list(N),"   正在走路..."}),
            gen_tcp:send(Socket, Data1);
        true ->
            ok
    end.
around_msg_10s(Socket,N,T)->
    %io:fwrite("around 10 time =~w~n",[T]),
    if
        T rem 10 =:= 0 ->
            random:seed(now()),
            Nu=random:uniform(2),
            Data1 = msg_to_binary({integer_to_list(140),"[附近]guest"++integer_to_list(N),chat(Nu)}),
            gen_tcp:send(Socket,Data1) ;
        true ->
            ok
    end.
world_or_country_msg(Socket,N,T)->
    %io:fwrite("world time =~w~n",[T]),
    if
        T rem 60 =:= 0 ->
            case random:uniform(2) of
                1 ->
                    random:seed(now()),
                    Num=random:uniform(3),
                    Nu=random:uniform(9),
                    Data1=msg_to_binary({integer_to_list(1),integer_to_list(Num),"[国家]guest"++integer_to_list(N),chats(Nu)}),
                    gen_tcp:send(Socket,Data1);
                2 ->
                    random:seed(now()),
                    Nu=random:uniform(9),
                    Data1=msg_to_binary({integer_to_list(1),integer_to_list(0),"[世界]guest"++integer_to_list(N),chats(Nu)}),
                    gen_tcp:send(Socket,Data1)
            end,
            ok;
        true ->
            false
    end.
msg_to_binary(Msg)->term_to_binary(Msg).
chat(N) ->
    case N of
        1 ->
            "    在吃饭....";
        2->
            "    在睡觉....";
        3->
            "    在走路...."
    end.
chats(N)->
    case N of
        1->
            " 思念是一种很玄的东西 如影随形....";
        2 ->
            " 无声又无息出没在心底...";
        3->
            "我无力抗拒 特别是夜里...";
        4->
            "大声的告诉你...";
        5->
            "我愿意为你 忘记我姓名...";
        6->
            "我愿意为你 被放逐天际 ,只要你真心 拿爱与我回应...";
        7->
            "什么都愿意 什么都愿意 为你...";
        8->
            "就算多一秒 停留在你怀里...";
        9->
            "恨不能立即 朝你狂奔去..."
    end.