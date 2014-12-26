%%%-------------------------------------------------------------------
%%% @author Chenshaobo <chenshaobo65@gmail.com>
%%% @copyright (C) 2014, <MingChao>
%%% @doc
%%%
%%% @end
%%% Created : 26. 十二月 2014 15:26
%%%-------------------------------------------------------------------
-module(publish_server).
-author("Chenshaobo <chenshaobo65@gmail.com>").

-behaviour(gen_server).
-include("mnesia.hrl").
-include("debug.hrl").
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(MAX_LENGTH,50).
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    init_table(),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(i,_From,State)->
    D=get_top_contents(),
    {reply,D,State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info({content,Account,Content,Time},State)->
    RContent=#r_content{account = Account,content=Content,post_time = Time},
    insert_top_content(RContent),
    {noreply, State};
handle_info({comment,ContentIndex,Account,Content,Time,Reply},State)->
    RComment=#r_comment{account=Account,content = Content,post_time = Time,reply_index = Reply},
    update_content(ContentIndex,RComment),
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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    [mnesia:dirty_write(?DB_CONTENT_P,Content)|| Content<-get_top_contents()],
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
init_table()->
    case catch mnesia:dirty_last(?DB_CONTENT_P) of
        Index1 when is_integer(Index1)->
            Contents=
                lists:foldl(fun(ID,Acc)->
                    case mnesia:dirty_read(?DB_CONTENT_P,ID) of
                        [#r_content{index = ID}=C]->
                            [C|Acc];
                        _ ->
                            Acc
                    end end,[],lists:seq((Index1-?MAX_LENGTH),Index1)),
            set_top_contents(Contents);
        _ ->
            ignore
    end.
update_content(ContentIndex,RComment)->
    Contents=get_top_contents(),
    NewIndex=get_new_index(),
    if
        (NewIndex-1 -ContentIndex) < ?MAX_LENGTH->
            case lists:keyfind(ContentIndex,#r_content.index,Contents) of
                #r_content{index = ContentIndex}=Content->
                    #r_content{comments = Comments,comment_num = Num}=Content,
                    NewContent=Content#r_content{comments = [RComment|Comments],comment_num = Num+1},
                    NewContents=lists:keystore(ContentIndex,#r_content.index,Contents,NewContent),
                    set_top_contents(NewContents);
                _ ->
                    ignore
            end;
        true ->
            case mnesia:dirty_read(?DB_CONTENT_P,ContentIndex) of
                [#r_content{index = ContentIndex}=Content]->
                    #r_content{comments = Comments,comment_num = Num}=Content,
                    NewContent=Content#r_content{comments = [RComment|Comments],comment_num = Num+1},
                    mnesia:dirty_write(?DB_CONTENT_P,NewContent);
                _ ->
                    ignore
            end
    end.
get_top_contents()->
    case erlang:get(top_contents) of
        [_|_]=Contents->
            Contents;
        _ ->
            []
    end.
set_top_contents(Contents)->
    erlang:put(top_contents,Contents).
insert_top_content(Content)->
    OldContents=get_top_contents(),
    case  erlang:length(OldContents) of
        0 ->
            set_top_contents([Content#r_content{index = 1}]),
            set_new_index(2);
        Num when Num >= ?MAX_LENGTH ->
            LastContent=lists:last(OldContents),
            dump_content(LastContent),
            Index=get_new_index(),
            set_top_contents([Content#r_content{index = Index}|lists:keydelete(LastContent#r_content.index,#r_content.index,OldContents)]);
        _Num ->
            Index=get_new_index(),
            set_top_contents([Content#r_content{index = Index}|OldContents])
    end.
get_new_index()->
    case erlang:get(new_index) of
        Index when is_integer(Index)->
            set_new_index(Index+1),
            Index;
        _ ->
            [First|_Tail]=get_top_contents(),
            Index=First#r_content.index +1,
            set_new_index(Index+1),
            Index
    end.
set_new_index(Index)->
    erlang:put(new_index,Index).
dump_content(Content)->
    mnesia:dirty_write(?DB_CONTENT_P,Content).