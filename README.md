# What this does

This is a utility pack for converting integers to their English word
representations. This was one cool feature that came built in to the
Common Lisp implementation. This pack is to bring that functionality
to SWI Prolog.


# Example usage

```prolog
?- number_word(1024, Word).
Word = "one thousand, twenty four".

?- number_word(2048, "two thousand, forty eight").
true.
```