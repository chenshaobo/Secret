%%%-------------------------------------------------------------------
%%% @author Chenshaobo <chenshaobo65@gmail.com>
%%% @copyright (C) 2014, <MingChao>
%%% @doc
%%%
%%% @end
%%% Created : 22. 十二月 2014 16:54
%%%-------------------------------------------------------------------
-author("Chenshaobo <chenshaobo65@gmail.com>").

-define(MNESIA_DEFAULT_OPTION,[]).%{disc_copies,[node()]}]).

-define(MNESIA_TABLE,[
    {?DB_ACCOUNT_P,r_account,record_info(fields,r_account),[]},
    {?DB_CONTENT_P,r_content,record_info(fields,r_content),[{type,ordered_set}]}
]).

-define(DB_ACCOUNT_P,db_account_p).
-record(r_account,{account="",name="",sex=0,reg_time=0}).


-define(DB_CONTENT_P,db_content_p).
-record(r_content,{index=0,account="",content="",post_time=0,location_id=0,comments=[],comment_num=0}).
-record(r_comment,{index=0,account="",content="",post_time=0,reply_index=0}).
