%%%-------------------------------------------------------------------
%%% @author Jakub
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. sty 2016 09:05
%%%-------------------------------------------------------------------
-module(queue1).
-author("Jakub").

%% API
-export([init/0, loop/0, addQueue/1, stop/0, clear/0, flush/0]).


init()->
  Reciver=spawn(?MODULE,loop,[]),
  register(reciver,Reciver).


addQueue(0)->
  io:format("Koniec dodawania  ");
addQueue(N)->
  reciver ! 5,
  %%timer:sleep(1000),
  addQueue(N-1).


stop()->
  reciver ! stop.


loop()->
  receive
    flush -> flush() ,
      loop();
    stop -> io:format("Koniec  ")
    end.

clear()->
  reciver ! flush.

flush() ->
  receive
    _ -> flush()
  after
    0 -> ok
  end.

