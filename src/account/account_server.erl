%%%-------------------------------------------------------------------
%%% @author Chenshaobo <chenshaobo65@gmail.com>
%%% @copyright (C) 2014, <MingChao>
%%% @doc
%%%
%%% @end
%%% Created : 25. 十二月 2014 16:39
%%%-------------------------------------------------------------------
-module(account_server).
-author("Chenshaobo <chenshaobo65@gmail.com>").

-behaviour(gen_server).
-include("mnesia.hrl").
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

-record(account, {account,pid,gpid,ip}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link({Account})->
    gen_server:start_link({local,erlang:list_to_atom(Account)}, ?MODULE, [{Account}], []);
start_link({Account,Sex,RegTime,GPName}) ->
    gen_server:start_link({local,erlang:list_to_atom(Account)}, ?MODULE, [{Account,Sex,RegTime,GPName}], []).

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
init([{Account,Sex,RegTime,GPName}]) ->
    Record=#r_account{account = Account,name = Account,sex = Sex,reg_time = RegTime},
    mnesia:dirty_write(?DB_ACCOUNT_P,Record),
    account_misc:send_g(GPName,{login,Account,self()}),
    {ok, #account{account = erlang:list_to_atom(Account),pid = self(),gpid = GPName}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(disconnect,State)->
    erlang:exit(self(),disconnect),
    {noreply,State};

handle_info({router,Record},State)->
    router:router(erlang:element(1,Record),Record,State#account.gpid),
    {noreply,State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------

terminate(Reason, _State) ->
    ?PRINT("Exit:~w",[Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
