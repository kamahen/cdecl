:- module(test_cdecl, []).

:- consult(cdecl).  % TOOD: use_module(cdecl).
:- use_module(library(plunit)).
:- use_module(library(dcg/high_order)).

:- begin_tests(base).

test(identifier, [nondet, Id == foo]) :-
    test_phrase("foo", identifier(Id)).
test(identifier, [nondet, Id == 'Foo_123']) :-
    test_phrase("   Foo_123", identifier(Id)).
test(identifier, [nondet, Ids == []]) :-
    test_phrase("", sequence(identifier, Ids)).
test(identifier, [nondet, Ids == ['fXX_123']]) :-
    test_phrase("   fXX_123", sequence(identifier, Ids)).
test(identifier, [nondet, Ids == ['fXX_123']]) :-
    test_phrase("   fXX_123", sequence(identifier, token(','), Ids)).
test(identifier, [nondet, Ids == ['fXX_123','bar']]) :-
    test_phrase("   fXX_123,  bar", sequence(identifier, token(','), Ids)).

test(identifier, [nondet, String == "foo"]) :-
    phrase_test(identifier(foo), String).
test(identifier, [nondet, String == ""]) :-
    phrase_test(sequence(identifier, []), String).
test(identifier, [nondet, String == "foo"]) :-
    phrase_test(sequence(identifier, [foo]), String).
test(identifier, [nondet, String == "a,b,cd"]) :-
    phrase_test(sequence(identifier, token(','), [a,b,cd]), String).

test(plus_seq, fail) :-
    test_phrase("  ", plus_seq(identifier, _Ids)).
test(plus_seq, [nondet, Ids = [foo]]) :-
    test_phrase("  foo", plus_seq(identifier, Ids)).
test(plus_seq, [nondet, Ids = [foo,bar]]) :-
    test_phrase("  foo  bar", plus_seq(identifier, Ids)).
test(plus_seq, fail) :-
    phrase_test(plus_seq(identifier, []), _String).
test(plus_seq, [nondet, String == "foo"]) :-
    phrase_test(plus_seq(identifier, [foo]), String).
test(plus_seq, [nondet, String == "foobar"]) :- % TODO: test with something other than identifier
    phrase_test(plus_seq(identifier, [foo,bar]), String).

:- end_tests(base).

test_phrase(String, Goal) :-
    string_codes(String, Codes),
    phrase(Goal, Codes).

phrase_test(Goal, String) :-
    phrase(Goal, Codes),
    string_codes(String, Codes).
