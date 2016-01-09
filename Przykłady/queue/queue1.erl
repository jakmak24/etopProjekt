-module(queue1).
-author("Jakub").

%% API
-export([init/0, loop/0, addQueue/1, stop/0, clear/0, flush/0]).


init()->
  Receiver = spawn(?MODULE,loop,[]),
  register(receiver,Receiver).


addQueue(0)->
  io:format("Koniec dodawania  ");
addQueue(N)->
  receiver ! 5,
  %%timer:sleep(1000),
  addQueue(N-1).

stop()->
  receiver ! stop.

loop()->
  receive
    flush -> flush() ,
      loop();
    stop -> io:format("Koniec  ")
    end.

clear()->
  receiver ! flush.

flush() ->
  receive
    _ -> flush()
  after
    0 -> ok
  end.