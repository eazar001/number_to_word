
:- module(groups,
     [ gen//1 ]).

:- initialization compile_predicates([gen//1]).

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).

:- dynamic gen//1.

term_expansion(beyond(N, Word), Rule) :-
  dynamic(gen//1),
  succ(N0, N),
  atom_concat(x_, N, Name),
  atom_concat(x_, N0, Name0),
  Head =.. [Name, [W1,Word|W2]],
  Prev =.. [Name0, W2],
  Rule = ( Head --> x(W1), call(Prev) ),
  Gen =.. [Name, A, B, C],
  GenRule = ( gen(A,B,C) :- Gen ),
  assertz(GenRule).



gen(Ws) --> x(Ws).
gen(Ws) --> x_2(Ws).


x_2([W1,thousand|W2]) -->
  x(W1),
  x(W2).


x([Word]) -->
  dig(zero),
  dig(zero),
  dig(Word).

x([Word]) -->
  dig(zero),
  special(Word).

x(Word) -->
  dig(zero),
  ten(W1),
  dig(W2),
  {  W2 == zero
  -> Word = [W1]
  ;  Word = [W1,W2]
  }.

x(Word) -->
  dig(W1),
  { W1 \== zero },
  % Second digit is either single digit or special number
  (  (  dig(zero),
        dig(W2)
     ;  special(W2)
     ),
     {  W2 == zero
     -> Word = [W1,hundred]
     ;  Word = [W1,hundred,W2]
     }
  ;  ten(W2),
     dig(W3),
     { W2 \== zero,
       (  W3 == zero
       -> Word = [W1,hundred,W2]
       ;  Word = [W1,hundred,W2,W3]
       )
     }
  ).


dig(zero) --> [0].
dig(one) --> [1].
dig(two) --> [2].
dig(three) --> [3].
dig(four) --> [4].
dig(five) --> [5].
dig(six) --> [6].
dig(seven) --> [7].
dig(eight) --> [8].
dig(nine) --> [9].


special(ten) --> [1,0].
special(eleven) --> [1,1].
special(twelve) --> [1,2].
special(thirteen) --> [1,3].
special(fourteen) --> [1,4].
special(fifteen) --> [1,5].
special(sixteen) --> [1,6].
special(seventeen) --> [1,7].
special(eighteen) --> [1,8].
special(nineteen) --> [1,9].


ten(twenty) --> [2].
ten(thirty) --> [3].
ten(forty) --> [4].
ten(fifty) --> [5].
ten(sixty) --> [6].
ten(seventy) --> [7].
ten(eighty) --> [8].
ten(ninety) --> [9].


beyond(3, million).
beyond(4, billion).
beyond(5, trillion).
beyond(6, quadrillion).
beyond(7, quintillion).
beyond(8, sextillion).
beyond(9, septillion).
beyond(10, octillion).
beyond(11, nonillion).
beyond(12, decillion).
beyond(13, undecillion).
beyond(14, duodecillion).
beyond(15, tredecillion).
beyond(16, quattuordecillion).
beyond(17, quindecillion).
beyond(18, sexdecillion).
beyond(19, septendecillion).
beyond(20, octodecillion).
beyond(21, novemdecillion).
beyond(22, vigintillion).


