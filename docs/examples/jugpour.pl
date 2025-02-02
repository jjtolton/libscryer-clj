/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The code here is liberally borrowed from Markus Triska:
   https://github.com/triska/the-power-of-prolog/blob/a9af9a3f1cfbec0a922e8a6a8740c0174c0a9f89/tist/jugs.pl
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).

moves(Js0) --> {member(jug(c,_,3), Js0)}.

moves(Js0) --> [fill(ID)],
        { select(jug(ID, C, _), Js0, Js) },
        moves([jug(ID,C,C)|Js]).

moves(Js0) --> [empty(ID)],
        { select(jug(ID, C, _), Js0, Js) },
        moves([jug(ID,C,0)|Js]).

moves(Js0) --> [from_to(F,T)],
        { 
          
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
             preconditions
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
          select(jug(F,FC,FF0), Js0, Js1), % remove jug(F,FC,FF0) from Js0 resulting in Js1
          select(jug(T,TC,TF0), Js1, Js) , % remove jug(T,TC,TF0) from Js1 resulting in Js2
          FF0 #> 0,              % source/from jug shouldn't be empty
          TF0 #< TC,             % target/to jug should not be full

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
             postconditions / effects
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
          
          M #= min(FF0, TC-TF0), % pour it all in (FF0) or top it up (TC-TF0)
          FF #= FF0 - M,         % reflect the water poured out from source jug
          TF #= TF0 + M          % reflect the water poured into the source jug
        },
        moves([jug(F, FC, FF), jug(T,TC,TF)|Js]).

solve(N, Moves) :- 
         InitialState=moves([jug(a, 5, 0), jug(b, 2, 0), jug(c,4,0)]),
         length(Moves, N), % constrain length of moves to force iterative deepening
         phrase(InitialState, Moves).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ?- solve(N, Moves).
   %@    N = 3, Moves = [fill(a),from_to(a,b),from_to(a,c)]
   %@ ;  N = 4, Moves = [fill(a),fill(a),from_to(a,b),from_to(a,c)]
   %@ ;  N = 4, Moves = [fill(a),empty(b),from_to(a,b),from_to(a,c)]
   %@ ;  N = 4, Moves = [fill(a),empty(c),from_to(a,b),from_to(a,c)]
   %@ ;  ... .
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

