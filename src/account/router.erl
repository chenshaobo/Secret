%%%-------------------------------------------------------------------
%%% @author Chenshaobo <chenshaobo65@gmail.com>
%%% @copyright (C) 2014, <MingChao>
%%% @doc
%%%
%%% @end
%%% Created : 25. 十二月 2014 16:14
%%%-------------------------------------------------------------------
-module(router).
-author("Chenshaobo <chenshaobo65@gmail.com>").
-include("mnesia.hrl").
-include("debug.hrl").
%% API
-export([router/3]).


router(1,Data,GPID)->
    {_Pro,Account,Sex}=Data,
    case mnesia:dirty_read(?DB_ACCOUNT_P,Account) of
        [#r_account{}]->
            ?ERROR("Already Reg:~w",[Account]);
        _ ->
            RegTime=?SECOND,
            R=account_sup:start_child({Account,Sex,RegTime,GPID}),
            ?PRINT("Start account:~w",[R])
    end;
router(2,Data,GPID)->
    {2,Account,_Sex}=Data,
    case mnesia:dirty_read(?DB_ACCOUNT_P,Account) of
        [#r_account{account= Account,sex = Sex,reg_time = RegTime}]->
            R=account_sup:start_child({Account,Sex,RegTime,GPID}),
            ?PRINT("Start account:~w",[R]);
        _ ->
            ?ERROR("Account not found:~w",[Account])
    end;
router(_P,Data,_GPName)->
    ?ERROR("Receive Unknow  Pro:~w",[Data]),
    Data.