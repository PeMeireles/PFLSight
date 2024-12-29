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
  

positions_board(PosList,PosBoard) :-
    maplist(position_board, PosList, PosBoard).

position_board([X,Y], [X,Y1]) :-
  Y1 is 6 -Y.

valid_moves([Board, Next], Positions) :-
  % Check for the biggest stack
  find_biggest_stacks(Board, Next, StackPositions, Max),
  valid_movesAux(Board, Max, StackPositions, Positions).

valid_movesAux(Board, 1,_, Positions) :-
  % Every Position without a piece is valid 
  findall([Row,Col],
      (between(1, 5, Col),
      between(1, 5, Row),
      access_board(Board,[Row,Col], Elem),
      Elem = '-'), Positions).

valid_movesAux(Board, 0,_, Positions) :-
  % Every Position without a piece is valid 
  findall([Row,Col],
      (between(1, 5, Col),
      between(1, 5, Row),
      access_board(Board,[Row,Col], Elem),
      Elem = '-'), Positions).

valid_movesAux(Board, _, StackPositions, Positions) :-
    % Every Position without a piece is valid near stack Positions
    findall([Row,Col],
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
    LeftCol is SCol - 1,
    findall([Row,Col], 
        (between(1, LeftCol, SearchCol),
         Col is LeftCol - SearchCol,
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
    LeftRow is SRow - 1,
    findall([Row,Col], 
        (between(1, LeftRow, SearchRow),
         Row is LeftRow -SearchRow,
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


  

