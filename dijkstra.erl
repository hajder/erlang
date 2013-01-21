-module(dijkstra).
-export([read_graph/0, perform/4]).

% S -  wierzchołki sprawdzone
% Q - wierzchołki niesprawdzone
% tupla - {V, C, P} - Vertex, Cost, Parent

parse(0, List) ->
  List;

parse(N, List) when N > 0 ->
  {_, Line} = io:fread("input from, to, weight: ", "~d ~d ~d"),
  parse(N-1, List ++ [Line]).

read_graph() ->
  {_, [V]} = io:fread("vortex count: ", "~d"),
  {_, [N]} = io:fread("path count: ", "~d"),
  Paths = parse(N, []),
  {_, [Start]} = io:fread("Start: ", "~d"),
  {_, [Finish]} = io:fread("End: ", "~d"),
  [V, Start, Finish, Paths].
  
perform(N, Start, End, Paths) ->
  S = [],
  Q = lists:map(fun(E) -> {E, 999, 0} end, lists:seq(1, N)),
  calc_dist(S, Q, Start, Paths, End).
  
calc_dist(S, [], _, _, _) ->
  io:fwrite("~w\n", [S]),
  find_path(S);
  
calc_dist([], Q, V, Paths, End) ->
  calc_dist([{V, 0, 0}], Q -- [{V, 999, 0}], {V, 0, 0}, Paths, End);
  
calc_dist(S, _, {End, _, _}, _, End) ->
  calc_dist(S, [], {}, [], End);

calc_dist(S, Q, V, Paths, End) ->
  io:fwrite("~w\t\t~w\t\t~w\n", [S, Q, V]),
  UpdQ = update_neighbours(V, Q, Paths),
  % tutaj jak Neighbours/shortest_path jest puste to sie sypie
  [NewS, NewQ, NewV] = move_next_vertex(S, UpdQ),
  calc_dist(NewS, NewQ, NewV, Paths, End).
  
move_next_vertex(S, Q) ->
  io:fwrite("~w\t\t~w\n", [S, Q]),
  Next = lowest_cost(Q),
  [[Next|S], remove(Next, Q), Next].
  
update_neighbours(V, Q, Paths) -> 
  neighbours(V, Q, Paths, []).

neighbours(_, Q, [], Acc) -> 
  lists:flatten([Acc|Q]);

neighbours(V, Q, [H|Paths], Acc) ->
  [A, B, W] = H,
  {X, Cost, _} = V,
  % check if path starts in parent and target is in Q
  case A == X andalso member(B, Q) of
    % if so add to found neighbours
    true ->
      {_, OldCost, _} = find(B, Q),
      NewQ = remove({B, 0, 0}, Q),
      %io:fwrite("neighbours:\t\t~w\n", [lowest_cost([{X, OldCost, X}, {B, Cost + W, X}])]),
      neighbours(V, NewQ, Paths, [{B, lists:min([Cost + W, OldCost]), X}|Acc]);
    % else continue
    false -> neighbours(V, Q, Paths, Acc)
  end.
  
find_path(S) ->
  find_path(S, []).
  
find_path([], Acc) ->
  Acc;
  
find_path([H|S], []) ->
  {V, _, P} = H,
  find_path(S, [P, V]);
  
find_path([H|S], Acc) ->
  Parent = lists:nth(1, Acc),
  {_, _, P} = H,
  case H of
    {_, _, 0} -> find_path(S, Acc);
    {Parent, _, _} -> find_path(S, [P|Acc]);
    _ -> find_path(S, Acc)
  end.

% helper functions
% check if vertex is in list of tuples
member(_, []) ->
  false;

member(Vertex, [H|List]) ->
  case H of
    {Vertex, _, _} -> true;
    _ -> member(Vertex, List)
  end.

% find vertex in list
find(1, _) ->
  {1,0,0};
  
find(_, []) ->
  {0,0,0};

find(Vertex, [H|List]) ->
  case H of
    {Vertex, _, _} -> H;
    _ -> find(Vertex, List)
  end.

% remove vertex from list of tuples
remove(V, Q) ->
  remove(V, Q, []).

remove(_, [], Acc) ->
  Acc;
  
remove(V, [H|Q], Acc) ->
  {Vertex, _, _} = V,
  case H of
    {Vertex, _, _} ->
      remove(V, Q, Acc);
    _ ->
      remove(V, Q, [H|Acc])
  end.
  
lowest_cost(Q) ->
  lowest_cost(Q, []).
  
lowest_cost([], Result) ->
  Result;
  
lowest_cost([H|Q], []) ->
  lowest_cost(Q, H);
  
lowest_cost([H|Q], Result) ->
  {_, Cost, _} = Result,
  {_, NewCost, _} = H,
  case NewCost < Cost of
    true -> lowest_cost(Q, H);
    false-> lowest_cost(Q, Result)
  end.

