%% z -> / a trema 0666D -> ٭ Ú   
%% /uniE001 -> \ 
%% Z -> 1 preta -> z
%% Q -> 2 pretas -> q
%% J -> 3 pretas -> j
%% X -> 4 pretas -> x
%% K -> 1 branca -> k
%% V -> 2 brancas -> v
%% B -> 3 brancas -> b
%% P -> 4 brancas -> p

:- consult(display).
:- use_module(library(between)).
:- use_module(library(lists)).

play :- 
  board(5,Board),
  display_rows(Board),nl,
  display_menu,
  read(Choice),
  handle_choice(Choice).

display_menu :-
  write('please choose what mode you want to play:'),
  write('1 - player vs player'),nl,
  write('2 - player vs Computer'),nl,
  write('3 - Computer vs Computer'),nl.


handle_choice(1).

handle_choice(2) :-
  write('Would you like the Computer to be:'),nl,
  write('1 - Random'), nl,
  write('2 - Smart'), nl,
  read(Choice).

handle_choice(3) :-
  write('Would you like the first Computer to be:'),nl,
  write('1 - Random'), nl,
  write('2 - Smart'), nl,
  read(Choice1),
  write('Would you like the second Computer to be:'),nl,
  write('1 - Random'), nl,
  write('2 - Smart'), nl,
  read(Choice2).

access_board(Board,[X,Y], Val) :-
  Y1 is 6 - Y,
  between(1, 5, X),
  between(1, 5, Y1),
  nth1(Y1, Board, Row),
  nth1(X,Row,Val).

valid_moves([Board, Next], Positions) :-
  % Check for the biggest stack
  find_biggest_stacks(Board, Next, StackPositions, Max),
  valid_movesAux(Board, Max, StackPositions, Positions).

valid_movesAux(Board, 1,_, Positions) :-
  % Every Position without a piece is valid 
  findall([[0,0],[Row,Col]],
      (between(1, 5, Col),
      between(1, 5, Row),
      access_board(Board,[Row,Col], Elem),
      Elem = '-'), Positions).

valid_movesAux(Board, 0,_, Positions) :-
  % Every Position without a piece is valid 
  findall([[0,0],[Row,Col]],
      (between(1, 5, Col),
      between(1, 5, Row),
      access_board(Board,[Row,Col], Elem),
      Elem = '-'), Positions).

valid_movesAux(Board, _, StackPositions, Positions) :-
    % Every Position without a piece is valid near stack Positions
    findall([[StackRow, StackCol],[Row,Col]],
        (member([StackRow,StackCol], StackPositions),
         adjacent(StackRow, StackCol, Row, Col),
         between(1, 5, Row),
         between(1, 5, Col),
         access_board(Board,[Row,Col], Elem),
         Elem = '-'),
        PositionsWithDuplicates),
    % Remove duplicates from the list
    sort(PositionsWithDuplicates, Positions).

  

find_biggest_stacks(Board, Next, Positions, Max) :-
    % Find all player (next) positions
    findall((Row,Col,Val),
        (between(1, 5, Col),
         between(1, 5, Row),
        access_board(Board,[Row,Col], Elem),
        Elem \= '-',
        atom_chars(Elem, [Next|NumC]),
        number_chars(Val,NumC)),
        AllPositions),
    findall(Val, member((_, _, Val), AllPositions), Values),
    max_member(MaxNum, Values),
    Max = MaxNum,
    findall([Row,Col],
        (member((Row,Col,Num), AllPositions),
        Num = MaxNum),
        Positions).
    

% Check if its adjacent
adjacent(Row, Col, NewRow, NewCol) :-
    % Orthogonal moves (valid for all positions)
    (NewRow is Row, NewCol is Col + 1);  % Right
    (NewRow is Row, NewCol is Col - 1);  % Left
    (NewRow is Row + 1, NewCol is Col);  % Down
    (NewRow is Row - 1, NewCol is Col);  % Up
    % Diagonal moves (only for intersection points)
    ((Row + Col) mod 2 =:= 0,
      (NewRow is Row + 1, NewCol is Col + 1;
      NewRow is Row + 1, NewCol is Col - 1;
      NewRow is Row - 1, NewCol is Col + 1;
      NewRow is Row - 1, NewCol is Col - 1)).

in_sight(Gamestate, [Row,Col], Locations):-
    in_sight_row(Gamestate, [Row,Col], Rows),
    in_sight_col(Gamestate, [Row, Col], Cols),
    Intersection is ((Row + Col) mod 2),
    in_sight_diagonal(Gamestate, [Row, Col], Diags, Intersection),
    append(Rows,Cols,RowsCols),
    append(RowsCols,Diags,Locations).

% Find the first non empty space for each side
in_sight_col([Board, Next], [Row,SCol], Locations) :-
    RightCol is SCol + 1,
    findall([Row,Col], 
        (between(RightCol, 5, Col),
         access_board(Board, [Row,Col], Elem),
         Elem \= '-',!, % this cut makes it so it stops on first element
         atom_chars(Elem, [Next|_])),
        RightLocs),
    findall([Row,Col], 
        (between(1, 5, SearchCol),
         Col is SCol - SearchCol,
         Col >= 1,
         access_board(Board, [Row,Col], Elem),
         Elem \= '-', !,
         atom_chars(Elem, [Next|_])),
        LeftLocs),
    append(LeftLocs,RightLocs,Locations).

in_sight_row([Board, Next], [SRow,Col], Locations) :-
    RightRow is SRow + 1,
    findall([Row,Col], 
        (between(RightRow, 5, Row),
         access_board(Board, [Row,Col], Elem),
         Elem \= '-',!, % this cut makes it so it stops on first element
         atom_chars(Elem, [Next|_])),
        RightLocs),
    findall([Row,Col], 
        (between(1, 5, SearchRow),
         Row is SRow - SearchRow,
         Row >= 1,
         access_board(Board, [Row,Col], Elem),
         Elem \= '-', !,
         atom_chars(Elem, [Next|_])),
        LeftLocs),
    append(LeftLocs,RightLocs,Locations).


in_sight_diagonal(_,_,[], 1).
in_sight_diagonal([Board, Next], [SRow,SCol], Locations, 0) :-
    % Down-right diagonal
    findall([Row,Col], 
        (between(1, 5, Step),
         Row is SRow + Step,
         Col is SCol + Step,
         Row =< 5, Col =< 5,
         access_board(Board, [Row,Col], Elem),
         Elem \= '-', !,
         atom_chars(Elem, [Next|_])),
        DRLocs),
    % Down-left diagonal
    findall([Row,Col], 
        (between(1, 5, Step),
         Row is SRow + Step,
         Col is SCol - Step,
         Row =< 5, Col >= 1,
         access_board(Board, [Row,Col], Elem),
         Elem \= '-', !,
         atom_chars(Elem, [Next|_])),
        DLLocs),
    % Up-right diagonal
    findall([Row,Col], 
        (between(1, 5, Step),
         Row is SRow - Step,
         Col is SCol + Step,
         Row >= 1, Col =< 5,
         access_board(Board, [Row,Col], Elem),
         Elem \= '-', !,
         atom_chars(Elem, [Next|_])),
        URLocs),
    % Up-left diagonal
    findall([Row,Col], 
        (between(1, 5, Step),
         Row is SRow - Step,
         Col is SCol - Step,
         Row >= 1, Col >= 1,
         access_board(Board, [Row,Col], Elem),
         Elem \= '-', !,
         atom_chars(Elem, [Next|_])),
        ULLocs),
    append(DRLocs, DLLocs, DownLocs),
    append(URLocs, ULLocs, UpLocs),
    append(DownLocs, UpLocs, Locations).

switch_player(a, b).
switch_player(b, a).

firstpiece(a, a1).
firstpiece(b, b1).

move([Board, Next], [Origin,Target],[NewBoard, NewNext]) :-
  valid_moves([Board, Next], ValidPos),
  member([Origin,Target], ValidPos),
  in_sight([Board, Next], Target, SightPos),
  add1topos(Board,SightPos, Board2),
  firstpiece(Next, Piece),
  replace_on_board(Board2, Target, Piece, Board3),
  change_start_piece(Board3, Origin, NewBoard),
  switch_player(Next, NewNext).
  

change_start_piece(Board,[0,0], Board).
change_start_piece(Board, [X,Y], FinalBoard) :-
  access_board(Board, [X,Y], Val),

  atom_chars(Val, [Player|NumC]),
  number_chars(Num, NumC),
  NewNum is Num - 2,
  number_chars(NewNum, NewNumC),
  atom_chars(NewVal, [Player|NewNumC]),

  replace_on_board(Board, [X,Y], NewVal, FinalBoard).

add1topos(Board, [], Board).
add1topos(Board, [[X,Y]|Rest], FinalBoard) :-
    access_board(Board, [X,Y], Val),
  % Solução pouco elegante mas foi a forma que encontrei de converter
    atom_chars(Val, [Player|NumC]),
    number_chars(Num, NumC),
    NewNum is Num + 1,
    number_chars(NewNum, NewNumC),
    atom_chars(NewVal, [Player|NewNumC]),

    replace_on_board(Board, [X,Y], NewVal, TempBoard),
    add1topos(TempBoard, Rest, FinalBoard).
  

  
replace_on_board(Board, [X,Y], Val, FinalBoard) :-
  Y1 is 6-Y,
  nth1(Y1, Board, OldRow),
  replace_nth(OldRow, Val, NewRow, X),
  replace_nth(Board, NewRow, FinalBoard, Y1).

% Baseado no list_slice / list_del p3
replace_nth(Row1, NewVal, NewRow, X) :-
  X1 is X -1,
  length(Pref,X1),
  append(Pref, [_ | Suff],Row1),
  append(Pref, [NewVal | Suff], NewRow).
  
  
