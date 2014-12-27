%%%-------------------------------------------------------------------
%%% @author Chenshaobo <chenshaobo65@gmail.com>
%%% @copyright (C) 2014, <MingChao>
%%% @doc
%%%
%%% @end
%%% Created : 24. 十二月 2014 11:01
%%%-------------------------------------------------------------------
-module(db_misc).
-author("Chenshaobo <chenshaobo65@gmail.com>").
-include("mnesia.hrl").
-include("debug.hrl").
%% API
-export([init/0]).

init()->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),
    [ begin
          case mnesia:create_table(TableName,[{record_name,RecordName},
                                                {disc_copies,[node()]},
                                                {attributes,RecordInfo},{type,ordered_set}]++ExtraOpt) of
              {atomic, ok} ->
                  ok;
              {aborted,R} ->
                  ?PRINT("create abort:~w",[R])
          end
      end||{TableName,RecordName,RecordInfo,ExtraOpt}<-?MNESIA_TABLE].

