:- module(lexer_test, []).
:- use_module(libscryer_lexer).
:- use_module(test_framework).

:- initialization(main).


test("lex open paren", (libscryer_lexer:lex("(", Result),
                   Result=[open_paren, eof])
    ).

test("lex close paren", (libscryer_lexer:lex(")", Result),
                         Result=[close_paren, eof])
    ).

test("lex atom", (String="aAbBcC_X13",
                  lex(String, Result),
                  Result=[atom(String), eof])).

main :-
        test_framework:main(lexer_test).







