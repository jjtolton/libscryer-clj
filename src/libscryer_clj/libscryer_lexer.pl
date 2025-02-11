:- module(libscryer_lexer, [lex/2]).
:- use_module(library(dif)).
:- use_module(library(charsio)).
:- use_module(library(dcgs)).


lex(ClojureString, Symbols) :-
        phrase(lex(Symbols), ClojureString).


lex([open_paren|Symbols]) -->
        open_paren,
        lex(Symbols).

lex([close_paren|Symbols]) -->
        close_paren,
        lex(Symbols).

lex([atom(Symbol)|Symbols]) -->
        atom(Symbol),
        lex(Symbols).

lex([eof]) --> [], !.

open_paren --> "(".
close_paren --> ")".

atom([C|Cs]) -->
        [C],
        { char_type(C, ascii_graphic),
          char_type(C, upper([CUpper|_])),
          dif(C,CUpper)
        },
        atom_(Cs).

atom_([C|Cs]) -->
        [C],
        { char_type(C, ascii_graphic) },
        atom_(Cs).
atom_([]) --> [].

