% -*- mode: Prolog; coding: utf-8 -*-

% Simple bi-directional implementation of cdecl, which explains C
% declarations or makes them from an English-like syntax.

% See also https://github.com/paul-j-lucas/cdecl

% This code isn't a full implementation, but is intended to show how
% DCG notation can be used bidirectionally. It also doesn't handle
% C casts, although that wouldn't be difficult to add.
% And quite a few things are left out, such as _Atomic, _Thread_local, etc.

:- meta_predicate seq(2, ?, ?). % TODO: add param
:- meta_predicate seq(2, ?, ?, ?).
:- meta_predicate seq_(?, 2, ?, ?).
:- meta_predicate optional(2, ?, ?). % TODO: add param

cdecl_explain(Cdecl, _Explanation) :-
    string_codes(Cdecl, Codes),
    phrase(declaration, Codes).

% See grammar at https://en.cppreference.com/w/c/language/declarations
% The grammar is limited to only handle a single "declarator".
% Also: https://cs.wmich.edu/~gupta/teaching/cs4850/sumII06/The%20syntax%20of%20C%20in%20Backus-Naur%20form.htm

% This grammar was adapted from Section A13 of The C programming
% language, 2nd edition, by Brian W. Kernighan and Dennis
% M. Ritchie,Prentice Hall, 1988.
%
% from
% https://cs.wmich.edu/~gupta/teaching/cs4850/sumII06/The%20syntax%20of%20C%20in%20Backus-Naur%20form.htm
% similar to: https://gist.github.com/arslancharyev31/c48d18d8f917ffe217a0e23eb3535957

% <declaration> ::=  {<declaration-specifier>}+ {<init-declarator>}* ;

declaration --> plus_seq(declaration_specifier), seq(init_declarator), optional(token(';')), optional_space.

% <declaration-specifier> ::= <storage-class-specifier>
%                           | <type-specifier>
%                           | <type-qualifier>

declaration_specifier --> storage_class_specifier.
declaration_specifier --> type_specifier.
declaration_specifier --> type_qualifier.

% <storage-class-specifier> ::= 'auto'
%                             | 'register'
%                             | 'static'
%                             | 'extern'
%                             | 'typedef'

storage_class_specifier --> token(auto).
storage_class_specifier --> token(register).
storage_class_specifier --> token(static).
storage_class_specifier --> token(extern).
storage_class_specifier --> token(typedef).

% <type-specifier> ::= 'void'
%                    | 'char'
%                    | 'short'
%                    | 'int'
%                    | 'long'
%                    | 'float'
%                    | 'double'
%                    | 'signed'
%                    | 'unsigned'
%                    | <struct-or-union-specifier>
%                    | <enum-specifier>
%                    | <typedef-name>

type_specifier --> token(void).
type_specifier --> token(char).
type_specifier --> token(short).
type_specifier --> token(int).
type_specifier --> token(long).
type_specifier --> token(float).
type_specifier --> token(double).
type_specifier --> token(signed).
type_specifier --> token(unsigned).
type_specifier --> struct_or_union_specifier.
type_specifier --> enum_specifier.
type_specifier --> typedef_name.

% <struct-or-union-specifier> ::= <struct-or-union> <identifier> '{' {<struct-declaration>}+ '}'
%                               | <struct-or-union> '{' {<struct-declaration>}+ '}'
%                               | <struct-or-union> <identifier>

% struct_or_union_specifier --> struct_or_union, identifier, token('{'), plus_seq(struct_declaration, _S), token('}').
% struct_or_union_specifier --> struct_or_union, token('{'), plus_seq(struct_declaration, _S), token('}').
struct_or_union_specifier --> struct_or_union, identifier(_Id).

% <struct-or-union> ::= 'struct'
%                     | 'union'

struct_or_union --> token(struct).
struct_or_union --> token(union).

% % <struct-declaration> ::= {<specifier-qualifier>}* <struct-declarator-list>

% struct_declaration -->  seq(specifier_qualifier), struct_declarator_list.

% <specifier-qualifier> ::= <type-specifier>
%                         | <type-qualifier>

specifier_qualifier --> type_specifier.
specifier_qualifier --> type_qualifier.

% % <struct-declarator-list> ::= <struct-declarator>
% %                            | <struct-declarator-list> ',' <struct-declarator>


% struct_declarator_list --> struct_declarator.
% struct_declarator_list --> struct_declarator_list, token(','), struct_declarator.

% % <struct-declarator> ::= <declarator>
% %                       | <declarator> ':' <constant-expression>
% %                       | ':' <constant-expression>

% struct_declarator --> declarator.
% struct_declarator --> declarator, token(':'), constant_expression.
% struct_declarator --> token(':'), constant_expression.

% <declarator> ::= {<pointer>}? <direct-declarator>

declarator --> optional(pointer), direct_declarator.

% <pointer> ::= * {<type-qualifier>}* {<pointer>}?

pointer --> token('*'), seq(type_qualifier), optional(pointer).

% <type-qualifier> ::= const
%                    | volatile

type_qualifier --> token(const).
type_qualifier --> token(volatile).

% <direct-declarator> ::= <identifier>
%                       | '(' <declarator> ')'
%                       | <direct-declarator> '[' {<constant-expression>}? ']'
%                       | <direct-declarator> '(' <parameter-type-list> ')'
%                       | <direct-declarator> '(' {<identifier>}* ')'

direct_declarator --> identifier(_Id).
direct_declarator --> token('('), declarator,  token(')').
direct_declarator --> direct_declarator, token('['), optional(constant_expression), token(']').
direct_declarator --> direct_declarator, token('('), parameter_type_list, token(')').
direct_declarator --> direct_declarator, token('('), seq(identifier(_)), token(')').

% <type-name> ::= {<specifier-qualifier>}+ {<abstract-declarator>}?

type_name --> plus_seq(specifier_qualifier), optional(abstract_declarator).

% <parameter-type-list> ::= <parameter-list>
%                         | <parameter-list> ',' '...'

parameter_type_list --> parameter_list.
parameter_type_list --> parameter_list, token(','), token('...').

% <parameter-list> ::= <parameter-declaration>
%                    | <parameter-list> ',' <parameter-declaration>

parameter_list --> parameter_declaration.
parameter_list --> parameter_list, token(','), parameter_declaration.

% <parameter-declaration> ::= {<declaration-specifier>}+ <declarator>
%                           | {<declaration-specifier>}+ <abstract-declarator>
%                           | {<declaration-specifier>}+

parameter_declaration --> plus_seq(declaration_specifier), declarator.
parameter_declaration --> plus_seq(declaration_specifier), abstract_declarator.
parameter_declaration --> plus_seq(declaration_specifier).

% <abstract-declarator> ::= <pointer>
%                         | <pointer> <direct-abstract-declarator>
%                         | <direct-abstract-declarator>

abstract_declarator --> pointer.
abstract_declarator --> pointer, direct_abstract_declarator.
abstract_declarator --> direct_abstract_declarator.

% <direct-abstract-declarator> ::=  '(' <abstract-declarator> ')'
%                                | {<direct-abstract-declarator>}? '[' {<constant-expression>}? ']'
%                                | {<direct-abstract-declarator>}? '(' {<parameter-type-list>}? ')'

direct_abstract_declarator --> token('('), abstract_declarator, token(')').
direct_abstract_declarator --> optional(direct_abstract_declarator), token('['), optional(constant_expression), token(']').
direct_abstract_declarator --> optional(direct_abstract_declarator), token('('), optional(parameter_type_list), token(')').

% <enum-specifier> ::= 'enum' <identifier> '{' <enumerator-list> '}'
%                    | 'enum' '{' <enumerator-list> }
%                    | 'enum' <identifier>

enum_specifier --> token(enum), identifier(_Id), token('{'), enumerator_list, token('}').
enum_specifier --> token(enum), token('{'), enumerator_list, token('}').
enum_specifier --> token(enum), identifier(_Id).

% <enumerator-list> ::= <enumerator>
%                     | <enumerator-list> ',' <enumerator>

enumerator_list --> enumerator.
enumerator_list --> enumerator_list, token(','), enumerator.

% <enumerator> ::= <identifier>
%                | <identifier> '=' <constant-expression>

enumerator --> identifier(_Id).
% enumerator --> identifier, token('='), constant_expression.

% <typedef-name> ::= <identifier>

typedef_name --> identifier(_Id).

% <init-declarator> ::= <declarator>
%                     | <declarator> '=' <initializer>

init_declarator --> declarator.
% init_declarator --> declarator, token('='), initializer.


constant_expression --> token(_). % TODO: improve this

% TODO: improve this (starts with "_" or alpha, all "_").
identifier(Id) -->
    (   { var(Id) }
    ->  optional_space,
        [C],
        { is_csymf(C) },
        identifier_(IdCodes),
        { atom_codes(Id, [C|IdCodes]) }
    ;   { atom_codes(Id, IdCodes) },
        IdCodes
    ).

identifier_([C|Cs]) -->
    [C],
    { is_csym(C) },
    identifier_(Cs).
identifier_([]) --> [].

token(T) -->
    optional_space,
    (   { var(T) }
    ->  nonblanks(T_codes),
        { atom_codes(T, T_codes) }
    ;   { atom_codes(T, T_codes) },
        T_codes
    ).

optional_space --> [].
optional_space -->
    [C],
    { nonvar(C) },
    { is_space(C) },
    optional_space.

space -->
    [C],
    (   { var(C) }
    ->  { C = " " }
    ;   { is_space(C) },
        optional_space
    ).

%! nonblanks(-Codes)// is det.
%  Take all =graph= characters
%  (greedy)

nonblanks([H|T]) -->
    [H],
    { is_graph(H) },
    !,
    nonblanks(T).
nonblanks([]) -->
    [].

seq(Match) --> seq(Match, _List).  % TODO: remove

seq(Match, List) --> seq_(List, Match).

seq_([], _Match) --> [].
seq_([E|Es], Match) -->
    call(Match, E),
    seq_(Es, Match).

plus_seq(_Match) --> plus_seq(_).

plus_seq(Match, [A|List]) -->
    call(Match, A),
    seq_(List, Match).

optional(Match) --> % TODO: add Default
    Match, !.
optional(_Match) --> [].

%%%%%%%%%%%%%%%%%

tree_nodes(Tree, Nodes) :-
    phrase(tree_nodes(Tree, Nodes), Nodes).

tree_nodes(Tree, Nodes) --> tree_nodes(Tree, Nodes, _).

tree_nodes(nil, Ls, Ls) --> [].
tree_nodes(node(Name, Left, Right), [_|Ls0], Ls) -->
        tree_nodes(Left, Ls0, Ls1),
        [Name],
        tree_nodes(Right, Ls1, Ls).
