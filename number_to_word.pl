
:- module(number_to_word,
     [ format_word/2 ]).

:- use_module(groups).


format_word(Number, Word) :-
  apply_suffix(Number, Digits), !,
  (  Digits = [[]]
  -> Word = "zero"
  ;  format_word_(Digits, Words0),
     reverse(Words0, Words1),
     foldl(atomic_concat, Words1, '', Atom),
     atom_string(Atom, Word)
  ).

format_word_([], []).
format_word_([List|Rest], [Word|Words]) :-
  (  List = L-S
  -> atomic_list_concat(L, ' ', L0),
     format(atom(Word), "~a ~a, ", [L0, S])
  ;  atomic_list_concat(List, ' ', Word)
  ),
  format_word_(Rest, Words).


apply_suffix(Number, Word) :-
  number_digits(Number, Digits-Len),
  apply_suffix_(Digits, Word0, Len),
  exclude(=([]-_), Word0, Word).

apply_suffix_([Last], [Word], _) :-
  apply_phrase(Last, Word0),
  exclude(=(zero), Word0, Word).

apply_suffix_([G|Gs], [W-Suffix|Ws], N) :-
  apply_phrase(G, W0),
  exclude(=(zero), W0, W),
  Index is 10^N,
  beyond(Index, Suffix),
  N0 is N-3,
  apply_suffix_(Gs, Ws, N0).

apply_phrase(List, Word) :-
  phrase(group(Word), List).


%% number_digits(+Num, -Digits)
%
%  Digits is a list of digits that represents an integer Num. Digits is divided
%  into groups of 3. Digits will unify with a list of digits paired to its own
%  length.

number_digits(Num, Digits-Length) :-
  number_chars(Num, Chars),
  reverse(Chars, Rchars),
  digits_triplets(Rchars, RDigits, Length),
  reverse(RDigits, Digits).


digits_triplets(Chars, Digits, Length) :-
  digits_triplets_(Chars, Digits, -3, Length).

digits_triplets_([], [], L, L) :- !.
digits_triplets_([A], [[AN]], L0, L) :- !,
  L is L0+3,
  atom_number(A, AN).

digits_triplets_([A,B], [[BN,AN]], L0, L) :- !,
  L is L0+3,
  maplist(atom_number, [A,B], [AN,BN]).

digits_triplets_([A,B,C|Rest], [[CN,BN,AN]|Triplets], L0, L) :-
  L1 is L0+3,
  maplist(atom_number, [A,B,C], [AN,BN,CN]),
  digits_triplets_(Rest, Triplets, L1, L).

