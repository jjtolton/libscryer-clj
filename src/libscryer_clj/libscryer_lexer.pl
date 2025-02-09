:- module(libscryer_lexer, []).
:- use_module(library(dif)).
:- use_module(library(dcgs)).
:- use_module(library(terms)).
:- use_module(library(time)).
:- use_module(library(lists)).
:- use_module(library(charsio)).
:- use_module(library(iso_ext)).
:- use_module(library(format)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   lexer
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

open_paren --> ['('].
close_paren --> [')'].
question_mark --> ['?'].
dash --> ['-'].
underscore --> ['_'].
quotation_mark --> ['"'].

white_space -->
        [' '],
        white_space.
white_space --> [].

% scryer_clojure([Token|Tokens]) -->
%         [Token],
%         open_paren.

variable(Varname) -->
        question_mark,
        variable_name(Varname).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
?- length(Term, _), phrase(variable(Varname), Term).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

variable_name([Vup|Varname]) -->
        [V],
        { char_type(V, lower), char_type(V, upper([Vup|_])) },
        variable_name_(Varname).

variable_name_([V|Varname]) -->
        [V],
        { memberchk(V, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789") },
        variable_name_(Varname).

variable_name_(['-'|Varname]) -->
        dash,
        variable_name_(Varname).

variable_name_(['_'|Varname]) -->
        underscore,
        variable_name_(Varname).

variable_name_([]) --> [].

atom([C|Cs]) -->
        [C],
        { memberchk(C, "abcdefghijklmnopqrstuvwxyz")},
        atom_(Cs).

atom_([C|Cs]) -->
        [C],
        { memberchk(C, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")},
        atom_(Cs).

atom_([]) --> [].

string(atom("[]")) -->
        quotation_mark,
        quotation_mark.

string(compound(".", [atom([A]), atom("[]")])) -->
       quotation_mark,
       [A],
       { char_type(A, ascii) },
       quotation_mark.
       

string(compound(".", [StringContents])) -->
        quotation_mark,
        string_(StringContents),
        quotation_mark.

string_(compound(".", [atom([A])|[StringContents]])) -->
        [A],
        { char_type(A, ascii) },
        string_(StringContents).

string_(atom("[]")) --> [].
        
compound(CompoundName, CompoundTerms) -->
        open_paren,
        white_space,
        atom(CompoundName),
        white_space,
        terms(CompoundTerms),
        close_paren.

boolean("true") --> "true".

boolean("false") --> "false".

integer([Int|Ints]) -->
        [Int],
        { memberchk(Int, "123456789") },
        integer_(Ints).

integer_([Int|Ints]) -->
        [Int],
        { memberchk(Int, "0123456789") },
        integer_(Ints).

integer_([]) --> [].

decimal([Int,'.'|Ints]) -->
        [Int],
        ['.'],
        { memberchk(Int, "0123456789") },
        integer_(Ints).

decimal(['0', '.'|Ints]) -->
        ['.'],
        integer_(Ints).

decimal([Int, '.', '0']) -->
        [Int],
        { memberchk(Int, "0123456789") },
        ['.'].


terms([Term|Terms]) -->
        term(Term),
        white_space,
        terms(Terms).

terms([]) -->
        [].

term(atom(Term)) -->
        atom(Term),
        { dif(Term, "true"), dif(Term, "false") }.

term(compound(Name, Terms)) -->
        % is there a way to use libscryer_lexer:compound here?
        open_paren,
        atom(Name),
        white_space,
        terms(Terms),
        close_paren.

term(T) -->
        string(T).
                
term(variable(Term)) -->
        variable(Term).

term(boolean(Term)) -->
        boolean(Term).

term(decimal(Term)) -->
        decimal(Term).

term(integer(Term)) -->
        integer(Term).





