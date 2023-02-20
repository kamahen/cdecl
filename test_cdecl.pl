:- module(test_cdecl, []).

:- consult(cdecl).  % TOOD: use_module(cdecl).
:- use_module(library(plunit)).

:- begin_tests(base).

test(identifier, [nondet, Id == foo]) :-
    test_phrase("foo", identifier(Id)).
test(identifier, [nondet, Id == 'Foo_123']) :-
    test_phrase("   Foo_123", identifier(Id)).
test(identifier, [nondet, Ids == []]) :-
    test_phrase("", seq(identifier, Ids)).
test(identifier, [nondet, Ids == ['fXX_123']]) :-
    test_phrase("   fXX_123", seq(identifier, Ids)).
test(identifier, [nondet, Ids == ['fXX_123','bar']]) :-
    test_phrase("   fXX_123 bar", seq(identifier, Ids)).  % TODO: seq(identifier, token(','), ...)

test(identifier, [nondet, String == "foo"]) :-
    phrase_test(identifier(foo), String).
test(identifier, [nondet, String == ""]) :-
    phrase_test(seq(identifier, []), String).
test(identifier, [nondet, String == "foo"]) :-
    phrase_test(seq(identifier, [foo]), String).
test(identifier, [nondet, String == "abcd"]) :- % TODO: seq(identifier, token(','), ...)
    phrase_test(seq(identifier, [a,b,cd]), String).

:- end_tests(base).

test_phrase(String, Goal) :-
    string_codes(String, Codes),
    phrase(Goal, Codes).

phrase_test(Goal, String) :-
    phrase(Goal, Codes),
    string_codes(String, Codes).
