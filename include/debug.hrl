%%%-------------------------------------------------------------------
%%% @author Chenshaobo <chenshaobo65@gmail.com>
%%% @copyright (C) 2014, <MingChao>
%%% @doc
%%%
%%% @end
%%% Created : 25. 十二月 2014 20:25
%%%-------------------------------------------------------------------
-author("Chenshaobo <chenshaobo65@gmail.com>").

-define(PRINT(X),io:format(io_lib:format("Print:~w:~w>",[?MODULE,?LINE])++X++"~n")).
-define(PRINT(X,Y),io:format(io_lib:format("Print:~w:~w>",[?MODULE,?LINE])++ X ++ "~n",Y)).
-define(ERROR(X),io:format( io_lib:format("Error:~w:~w>",[?MODULE,?LINE])++X ++ "~n")).
-define(ERROR(X,Y),io:format( io_lib:format("Error:~w:~w>",[?MODULE,?LINE])++ X ++"~n",Y)).

-define(SECOND,begin {A,B,_C}=erlang:now(),A * 1000000 + B end).

-define(TRY_CATCH(Expression,ErrReason),
    try
        Expression
    catch
        _:ErrReason ->
            ?PRINT("Reason=~w~n,Stacktrace=~p", [ErrReason,erlang:get_stacktrace()])
    end).


-define(IF(X,B ,C ),
    case X of
        true ->
            B;
        false ->
            C
    end).