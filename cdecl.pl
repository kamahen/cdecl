% -*- mode: Prolog; coding: utf-8 -*-

% Simple bi-directional implementation of cdecl, which explains C
% declarations or makes them from an English-like syntax.

% See also https://github.com/paul-j-lucas/cdecl

% This code isn't a full implementation, but is intended to show how
% DCG notation can be used bidirectionally. It also doesn't handle
% C casts, although that wouldn't be difficult to add.
% And quite a few things are left out, such as _Atomic, _Thread_local, etc.

:- use_module(library(dcg/high_order)).

:- meta_predicate
    seq(3,?,?,?),
    seq(3,//,?,?,?),
    seq_plus(3,?,?,?),
    seq_plus(3,//,?,?,?),
    opt(//, //, ?, ?),
    opt(//, //, ?, ?, ?).

% TODO: import sequence//2 as seq//2
seq(Element, List) --> sequence(Element, List).
seq(Element, Sep, List) --> sequence(Element, Sep, List).

cdecl_explain(Cdecl, _Explanation) :-
    string_codes(Cdecl, Codes),
    % TODO: use eos//0
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

declaration -->
    seq_plus(declaration_specifier, _XXX),
    seq(init_declarator, _XXX2),
    opt(token(';'), []),
    optional_space. % TODO: eos?

% <declaration-specifier> ::= <storage-class-specifier>
%                           | <type-specifier>
%                           | <type-qualifier>

declaration_specifier(Specifier) --> storage_class_specifier(Specifier).
declaration_specifier(Specifier) --> type_specifier(Specifier).
declaration_specifier(Specifier) --> type_qualifier(Specifier).

% <storage-class-specifier> ::= 'auto'
%                             | 'register'
%                             | 'static'
%                             | 'extern'
%                             | 'typedef'

storage_class_specifier(auto)     --> token(auto).
storage_class_specifier(register) --> token(register).
storage_class_specifier(static)   --> token(static).
storage_class_specifier(extern)   --> token(extern).
storage_class_specifier(typedef)  --> token(typedef).

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

type_specifier(void)      --> token(void).
type_specifier(char)      --> token(char).
type_specifier(short)     --> token(short).
type_specifier(int)       --> token(int).
type_specifier(long)      --> token(long).
type_specifier(float)     --> token(float).
type_specifier(double)    --> token(double).
type_specifier(signed)    --> token(signed).
type_specifier(unsigned)  --> token(unsigned).
type_specifier(Specifier) --> struct_or_union_specifier(Specifier).
type_specifier(Specifier) --> enum_specifier(Specifier).
type_specifier(Specifier) --> typedef_name(Specifier).

% <struct-or-union-specifier> ::= <struct-or-union> <identifier> '{' {<struct-declaration>}+ '}'
%                               | <struct-or-union> '{' {<struct-declaration>}+ '}'
%                               | <struct-or-union> <identifier>

% struct_or_union_specifier --> struct_or_union, identifier, token('{'), seq_plus(struct_declaration, _XXX), token('}').
% struct_or_union_specifier --> struct_or_union, token('{'), seq_plus(struct_declaration, _XXX), token('}').
struct_or_union_specifier(Specifier) --> struct_or_union(Specifier), identifier(_XXX).

% <struct-or-union> ::= 'struct'
%                     | 'union'

struct_or_union(struct) --> token(struct).
struct_or_union(union)  --> token(union).

% % <struct-declaration> ::= {<specifier-qualifier>}* <struct-declarator-list>

% struct_declaration -->  seq(specifier_qualifier, _), struct_declarator_list.

% <specifier-qualifier> ::= <type-specifier>
%                         | <type-qualifier>

specifier_qualifier(Specifier) --> type_specifier(Specifier).
specifier_qualifier(Specifier) --> type_qualifier(Specifier).

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

declarator --> opt(pointer, []), direct_declarator.

% <pointer> ::= * {<type-qualifier>}* {<pointer>}?

pointer --> token('*'), seq(type_qualifier, _XXX), opt(pointer, []).

% <type-qualifier> ::= const
%                    | volatile

type_qualifier(const)    --> token(const).
type_qualifier(volatile) --> token(volatile).

% <direct-declarator> ::= <identifier>
%                       | '(' <declarator> ')'
%                       | <direct-declarator> '[' {<constant-expression>}? ']'
%                       | <direct-declarator> '(' <parameter-type-list> ')'
%                       | <direct-declarator> '(' {<identifier>}* ')'

direct_declarator --> identifier(_XXX).
direct_declarator --> token('('), declarator,  token(')').
direct_declarator --> direct_declarator, token('['), opt(constant_expression, []), token(']').
direct_declarator --> direct_declarator, token('('), parameter_type_list, token(')').
direct_declarator --> direct_declarator, token('('), seq(identifier, _), token(')').

% <type-name> ::= {<specifier-qualifier>}+ {<abstract-declarator>}?

type_name --> seq_plus(specifier_qualifier, _XXX), opt(abstract_declarator, []).

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

parameter_declaration --> seq_plus(declaration_specifier, _XXX), declarator.
parameter_declaration --> seq_plus(declaration_specifier, _XXX), abstract_declarator.
parameter_declaration --> seq_plus(declaration_specifier, _XXX).

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
direct_abstract_declarator --> opt(direct_abstract_declarator, []), token('['), opt(constant_expression, []), token(']').
direct_abstract_declarator --> opt(direct_abstract_declarator, []), token('('), opt(parameter_type_list, []), token(')').

% <enum-specifier> ::= 'enum' <identifier> '{' <enumerator-list> '}'
%                    | 'enum' '{' <enumerator-list> }
%                    | 'enum' <identifier>

enum_specifier(enum(Id,Enumerators)) --> token(enum), identifier(Id), token('{'), enumerator_list(Enumerators), token('}').
enum_specifier(enum('',Enumerators)) --> token(enum), token('{'), enumerator_list(Enumerators), token('}').
enum_specifier(enum(Id,[])) --> token(enum), identifier(Id).

% <enumerator-list> ::= <enumerator>
%                     | <enumerator-list> ',' <enumerator>

enumerator_list([Specifier]) --> enumerator(Specifier).
enumerator_list([Specifier|Specifiers]) --> enumerator(Specifier), token(','), enumerator_list(Specifiers).

% <enumerator> ::= <identifier>
%                | <identifier> '=' <constant-expression>

enumerator(Id) --> identifier(Id).
% enumerator --> identifier, token('='), constant_expression.

% <typedef-name> ::= <identifier>

typedef_name(Id) --> identifier(Id).

% <init-declarator> ::= <declarator>
%                     | <declarator> '=' <initializer>

init_declarator(_XXX) --> declarator.
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

% TODO: add seq_plus//2 and seq_plus//3 to library(dcg/high_order)

seq_plus(Match, [A|List]) -->
    call(Match, A),
    seq(Match, List).

seq_plus(Match, Sep, [A|List]) -->
    call(Match, A),
    (   Sep
    ->  seq(Match, Sep, List)
    ;   { List = [] }
    ).

opt(Match, Default) -->
    opt(Match, Default, .).

% Reversible optional -- Var is what Match instantiates.
% For example: opt(identifer(Id), '<no-id>', Id).
% An alternative way of defining this would be to make the Var
% implicit (similar to the way seq//3 is defined):
%    opt(identifier, '<no-id>', Id).
opt(Match, Default, Var) -->
    (   { var(Var) },
        Default
    *-> []
    ;   Match
    *-> []
    ;   Default
    ).


%%%%%%%%%%%%%%%%%

tree_nodes(Tree, Nodes) :-
    phrase(tree_nodes(Tree, Nodes), Nodes).

tree_nodes(Tree, Nodes) --> tree_nodes(Tree, Nodes, _).

tree_nodes(nil, Ls, Ls) --> [].
tree_nodes(node(Name, Left, Right), [_|Ls0], Ls) -->
        tree_nodes(Left, Ls0, Ls1),
        [Name],
        tree_nodes(Right, Ls1, Ls).
