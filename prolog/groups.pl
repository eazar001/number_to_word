
:- module(groups,
     [ group//1
      ,unit/2
      ,ten/2
      ,beyond/2 ]).


:- use_module(library(clpfd)).


term_expansion(beyond(Goal, Word), Goal0) :-
  Num is Goal,
  Goal0 = beyond(Num, Word).


group([Word]) -->
  { Sequence = [A]
  ; Sequence = [0,A]
  ; Sequence = [0,0,A]
  },
  Sequence,
  { unit(A, Word) }.

% Double digit handling
group([ten]) --> [1,0].
group([ten]) --> [0,1,0].
group([W0,W1]) -->
  { Sequence = [A,B]
  ; Sequence = [0,A,B]
  },
  Sequence,
  { B #> 0,
    A0 #= A*10,
    ten(A0, W0),
    unit(B, W1)
  }.

group([Word]) -->
  { Sequence = [A,B]
  ; Sequence = [0,A,B]
  },
  Sequence,
  { A0 #= A*10,
    N #= A0+B,
    ten(N, Word)
  }.

% Triple digit handling
group(Word) -->
  [A,B,C],
  { A #> 0,
    unit(A, W0),
    phrase(group(W2), [B,C]),
    format(atom(W1), "~a hundred", [W0]),
    append([W1], W2, Word)
  }.


% Single digit numbers
unit(0, zero).
unit(1, one).
unit(2, two).
unit(3, three).
unit(4, four).
unit(5, five).
unit(6, six).
unit(7, seven).
unit(8, eight).
unit(9, nine).

% Larger numbers
ten(11, eleven).
ten(12, twelve).
ten(13, thirteen).
ten(14, fourteen).
ten(15, fifteen).
ten(16, sixteen).
ten(17, seventeen).
ten(18, eighteen).
ten(19, nineteen).
ten(20, twenty).
ten(30, thirty).
ten(40, forty).
ten(50, fifty).
ten(60, sixty).
ten(70, seventy).
ten(80, eighty).
ten(90, ninety).

% Everything after
beyond(10^3, thousand).
beyond(10^6, million).
beyond(10^9, billion).
beyond(10^12, trillion).
beyond(10^15, quadrillion).
beyond(10^18, quintillion).
beyond(10^21, sextillion).
beyond(10^24, septillion).
beyond(10^27, octillion).
beyond(10^30, nonillion).
beyond(10^33, decillion).
beyond(10^36, undecillion).
beyond(10^39, duodecillion).
beyond(10^42, tredecillion).
beyond(10^45, quattuordecillion).
beyond(10^48, quindecillion).
beyond(10^51, sexdecillion).
beyond(10^54, septendecillion).
beyond(10^57, octodecillion).
beyond(10^60, novemdecillion).
beyond(10^63, vigintillion).

