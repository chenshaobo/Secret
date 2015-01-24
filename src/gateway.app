%%%-------------------------------------------------------------------
%%% @author chenshaobo
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 八月 2014 11:54
%%%-------------------------------------------------------------------
{application , gateway ,   [
    {description , ""}     ,
    {vsn , "1"}            ,
    {registered , []}      ,
    {applications , [
        kernel,
        stdlib
                    ]}     ,
    {mod,{gateway,[]}},
    {env , [{tcp_listen,8888}]}
                           ]}.