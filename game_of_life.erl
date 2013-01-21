-module(game_of_life).
-export([initialize/0]).

initialize() ->
  B = dict:new(),
  % put 2000 random live cells in
  B1 = dict:store(min_x, 1, B),
  B2 = dict:store(max_x, 50, B1),
  B3 = dict:store(min_y, 1, B2),
  B4 = dict:store(max_y, 50, B3),
  initialize(B4, 1500).
  
initialize(Board, 0) ->
  print(Board),
  tick(Board);
  
initialize(Board, I) ->
  X = random:uniform(dict:fetch(max_x, Board)),
  Y = random:uniform(dict:fetch(max_y, Board)),
  initialize(dict:store({X, Y}, live, Board), I-1).
  
tick(Board) ->
  Diff = tick(Board, dict:new()),
  NewBoard = dict:merge(fun(_, _, V) -> V end, Board, Diff),
  print(NewBoard),
  tick(NewBoard).
    
tick(Board, Diff) ->
  tick(Board, Diff, dict:fetch(max_y, Board)).
  
tick(Board, Diff, Y) ->
  YDiff = tick(Board, Diff, Y, dict:fetch(min_x, Board)),
  MinY = dict:fetch(min_y, Board),
  case Y of
    MinY -> YDiff; 
    _    -> tick(Board, YDiff, Y-1)
  end.
  
tick(Board, Diff, Y, X) ->
  Cell = find_cell(Board, X, Y),
  Nbs = count_neighbours(Board, X, Y),
  case {Nbs, Cell} of
    {0, _} -> XDiff = dict:store({X, Y}, dead, Diff);
    {1, _} -> XDiff = dict:store({X, Y}, dead, Diff);
    {2, live} -> XDiff = dict:store({X, Y}, live, Diff);
    {3, _} -> XDiff = dict:store({X, Y}, live, Diff);
    {_, _} -> XDiff = dict:store({X, Y}, dead, Diff)
  end,
  MaxX = dict:fetch(max_x, Board),
  case X of
    MaxX -> XDiff;
    _    -> tick(Board, XDiff, Y, X+1)
  end.
  
count_neighbours(Board, X, Y) ->
  dict:size(dict:filter(fun({A, B}, V) -> 
    A>=X-1 andalso 
    A=<X+1 andalso 
    B>=Y-1 andalso 
    B=<Y+1 andalso 
    {A,B} =/= {X,Y} andalso
    V == live 
  end, dict:filter(fun(K,_) -> is_tuple(K) end, Board))).
  
print(Board) ->
  print(Board, dict:fetch(max_y, Board)).
  
print(Board, Y) ->
  print(Board, Y, dict:fetch(min_x, Board)),
  io:fwrite("~s", ["\n"]),
  MinY = dict:fetch(min_y, Board),
  case Y of
    MinY -> ''; 
    _    -> print(Board, Y-1)
  end.
  
print(Board, Y, X) ->
  case find_cell(Board, X, Y) of
    live -> io:fwrite("~s", [" *"]);
    _    -> io:fwrite("~s", ["  "])
  end,
  MaxX = dict:fetch(max_x, Board),
  case X of
    MaxX -> '';
    _    -> print(Board, Y, X+1)
  end.
  
find_cell(Board, X, Y) ->
  case dict:find({X, Y}, Board) of
    {ok, live} -> live;
    {ok, _}    -> dead;
    error      -> dead
  end.
  
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
