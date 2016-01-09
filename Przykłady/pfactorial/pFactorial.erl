-module(pFactorial).
-export([factorial_seq/1, factorial_con/1, factorial_con_wisely/1, factorial_con_wisely/2, times/1]).
-export([factorial_con_manager/3, factorial_con_worker/2, factorial_con_wise_worker/3]).

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).

% Sekwencyjnie – kolejno dla ka¿dej liczby od 0 do N obliczana jest wartoœæ funkcji silnia.
factorial_seq(N) -> lists:map(fun(X) -> factorial(X) end, lists:seq(0, N)).

% Wspó³bie¿nie – dla ka¿dej liczby tworzony jest osobny proces, który obliczy wartoœæ funkcji silnia i odeœle j¹ do procesu gromadz¹cego wyniki.
factorial_con(N) ->
  Manager = spawn(?MODULE, factorial_con_manager, [N, [], self()]),
  lists:foreach(fun(I) -> spawn(?MODULE, factorial_con_worker, [I, Manager]) end, lists:seq(0, N)),
  receive
    {result, List} -> List
  end.

factorial_con_manager(N, List, Master) ->
  receive
    {partial, N, Result} when N == 0 -> Master ! {result, [Result | List]};
    {partial, N, Result} -> pFactorial:factorial_con_manager(N - 1, [Result | List], Master)
  end.

factorial_con_worker(N, Manager) ->
  Manager ! {partial, N, factorial(N)}.

% Wspó³bie¿nie i m¹drze - utworzone zostanie K procesów licz¹cych, gdzie K to liczba rdzeni komputera. Ka¿dy z procesów otrzyma listê zawieraj¹c¹ oko³o N/K wartoœci do policzenia.
factorial_con_wisely(N) ->
  factorial_con_wisely(N, erlang:system_info(logical_processors_available)).
factorial_con_wisely(N, Cores) ->
  Manager = spawn(?MODULE, factorial_con_manager, [N, [], self()]),
  lists:foreach(fun(I) -> spawn(?MODULE, factorial_con_wise_worker, [N-I, Cores, Manager]) end, lists:seq(0, Cores - 1)),
  receive
    {result, List} -> List
  end.

factorial_con_wise_worker(N, _, _) when N < 0 ->
  finished;
factorial_con_wise_worker(N, Cores, Manager) ->
  Manager ! {partial, N, factorial(N)},
  factorial_con_wise_worker(N-Cores, Cores, Manager).

% Funkcja sprawdzaj¹ca czasy wszystkich rozwi¹zañ
times(N) ->
  {STime, _} = timer:tc(?MODULE, factorial_seq, [N]),
  io:format("factorial_seq(~p) - ~p s ~n", [N, STime/1000000]),

  {CTime, _} = timer:tc(?MODULE, factorial_con, [N]),
  io:format("factorial_con(~p) - ~p s ~n", [N, CTime/1000000]),

  {CWTime, _} = timer:tc(?MODULE, factorial_con_wisely, [N]),
  io:format("factorial_con_wisely(~p) using all threads - ~p s ~n", [N, CWTime/1000000]).