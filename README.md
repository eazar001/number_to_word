# What this does

This is a utility pack for converting integers to their English word
representations. This was one cool feature that came built in to the
Common Lisp implementation. This pack brings this functionality and
other related ones to SWI Prolog.

# Example usage

```prolog
?- number_word([5,5,2,0,1,2,0,0,6], Word).
Word = [[five, hundred, fifty, two], million, [twelve], thousand, six]

?- number_word([0,0,6], Word).
Word = [six]

?- number_word(Number, [[one], thousand, three, hundred, seventy, two]).
Number = [0, 0, 1, 3, 7, 2]

?- number_word(Number, [[one], thousand, X, hundred, seventy, two]).
Number = [0, 0, 1, 1, 7, 2],
X = one ;
Number = [0, 0, 1, 2, 7, 2],
X = two ;
Number = [0, 0, 1, 3, 7, 2],
X = three ;
... etc.
```