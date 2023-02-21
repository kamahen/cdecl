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
    test_phrase("", seq(identifier, Ids)).
test(identifier, [nondet, Ids == ['fXX_123']]) :-
    test_phrase("   fXX_123", seq(identifier, Ids)).
test(identifier, [nondet, Ids == ['fXX_123']]) :-
    test_phrase("   fXX_123", seq(identifier, token(','), Ids)).
test(identifier, [nondet, Ids == ['fXX_123','bar']]) :-
    test_phrase("   fXX_123,  bar", seq(identifier, token(','), Ids)).

test(identifier, [nondet, String == "foo"]) :-
    phrase_test(identifier(foo), String).
test(identifier, [nondet, String == ""]) :-
    phrase_test(seq(identifier, []), String).
test(identifier, [nondet, String == "foo"]) :-
    phrase_test(seq(identifier, [foo]), String).
test(identifier, [nondet, String == "a,b,cd"]) :-
    phrase_test(seq(identifier, token(','), [a,b,cd]), String).

test(seq_plus, fail) :-
    test_phrase("  ", seq_plus(identifier, _Ids)).
test(seq_plus, [nondet, Ids = [foo]]) :-
    test_phrase("  foo", seq_plus(identifier, Ids)).
test(seq_plus, [nondet, Ids = [foo,bar]]) :-
    test_phrase("  foo  bar", seq_plus(identifier, Ids)).
test(seq_plus, fail) :-
    phrase_test(seq_plus(identifier, []), _String).
test(seq_plus, [nondet, String == "foo"]) :-
    phrase_test(seq_plus(identifier, [foo]), String).
test(seq_plus, [nondet, String == "foo"]) :-
    phrase_test(seq_plus(single_char, [f,o,o]), String).
test(seq_plus, [nondet, String == "a,b,cd"]) :-
    phrase_test(seq_plus(identifier, token(','), [a,b,cd]), String).

test(opt, fail) :-
    test_phrase("   abc", opt(identifier(Id), {Id='<no-id>'}, Id)).
test(opt, fail) :-
    test_phrase("abc", opt(identifier(Id), {Id='<no-id>'}, Id)).
test(opt, [nondet, Id == '<no-id>']) :-
    test_phrase("", opt(identifier(Id), {Id='<no-id>'}, Id)).
% TODO: is the following test correct?
test(opt, [nondet, String == "abc"]) :-
    Id = 'abc', phrase_test(opt(identifier(Id), {Id='<no-id>'}, Id), String).
test(opt, [nondet, String == ""]) :-
    phrase_test(opt(identifier(Id), {Id='<no-id>'}, Id), String).

:- end_tests(base).

test_phrase(String, Goal) :-
    string_codes(String, Codes),
    phrase(Goal, Codes).

single_char(C) --> [C].


phrase_test(Goal, String) :-
    phrase(Goal, Codes),
    string_codes(String, Codes).
