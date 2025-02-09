:- use_module(libscryer_lexer).
:- use_module(library(debug)).
:- use_module(library(dcgs)).

:- initialization(run_tests).


expecting(Name, X) :-
        (   call(X), !
        ;   error(test_error(Name), X)
        ).


run_tests :-
        (   expecting("Open paren works", phrase(libscryer_lexer:open_paren, ['(']))
        , expecting("Close paren works", phrase(libscryer_lexer:close_paren, [')']))
        , expecting("Varname parsable", phrase(libscryer_lexer:variable("Zz"), "?zz"))
        , expecting("Varname parsable", phrase(libscryer_lexer:variable("X"), "?x"))
        , expecting("Varname parsable mixed-case", phrase(libscryer_lexer:variable("XXaBaB"), "?xXaBaB"))
        , expecting("Varname can contain numbers", phrase(libscryer_lexer:variable("X1"), "?x1"))
        % todo -- should we allow dashes?
        , expecting("Varname can contain dashes",       phrase(libscryer_lexer:variable("X-1"), "?x-1"))
        , expecting("Varnames can contain underscores", phrase(libscryer_lexer:variable("X_1"), "?x_1"))
        , expecting("atom name works pretty well", phrase(libscryer_lexer:atom("xa1_b2zAz"), "xa1_b2zAz"))
        , expecting("basic compound term", phrase(libscryer_lexer:compound("foo", [atom("x")]), "(foo x)"))
        , expecting("basic compound term//2", phrase(libscryer_lexer:compound("foo", [atom("x"), atom("y")]), "(foo x y)"))
        , expecting("basic compound with variables",
                    phrase(libscryer_lexer:compound("foo", [variable("X")]), "(foo ?x)"))
        , expecting("boolean", phrase(libscryer_lexer:boolean("true"), "true"))
        , expecting("boolean term", (phrase(libscryer_lexer:term(T), "true"), T=boolean("true")))
        , expecting("integer term", (phrase(libscryer_lexer:term(T1), "123"), T1=integer("123")))
        , expecting("decimal term: 1.2", (phrase(libscryer_lexer:term(T2), "1.2"), T2=decimal("1.2")))
        , expecting("decimal term: .1 -> 0.1", (phrase(libscryer_lexer:term(T3), ".1"), T3=decimal("0.1")))
        , expecting("decimal term: 1. -> 1.0", (phrase(libscryer_lexer:term(T4), "1."), T4=decimal("1.0")))
        , expecting("complex compound term: p(X, 1, a, 1.2)",
                    (   phrase(libscryer_lexer:compound("p", [variable("X"), integer("1"), atom("a"), decimal("1.2")]),
                               "(p ?x 1 a 1.2)")
                    ))
        , expecting("compound term with nested compound term",
                    (   phrase(libscryer_lexer:term(compound("p", [compound("f", [integer("1")])])),
                               "(p (f 1))")
                    ))
        , expecting("simple string to compound term",
                    (   phrase(libscryer_lexer:string(TSimpleString), "\"a\""),
                        TSimpleString=compound(".", [atom("a"), atom("[]")])
                    )
                   )
        , expecting("string to compound term: \"abc\"",
                    (   
                        phrase(libscryer_lexer:string(compound(".",[compound(".",[atom("a"),compound(".",[atom("b"),compound(".",[atom("c"),atom("[]")])])])])), "\"abc\"")
                    )
                   )
        , expecting("compound term with complex nested terms: p(f(X,a),Y,\"abc\")",
                    (
                     phrase(libscryer_lexer:term(compound("p",[compound("f",[variable("X"),atom("a")]),variable("Y"),compound(".",[compound(".",[atom("a"),compound(".",[atom("b"),compound(".",[atom("c"),atom("[]")])])])])])),
                            "(p (f ?x a) ?y \"abc\")")
                    )
                   )
        )
        .

