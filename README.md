![epic handshake meme of muscular lisp arm and muscular prolog arm clasping hands](img/handshake.jpg)

Because a modern lisp deserves a modern prolog.

### Working Commits
Known to work with Scryer Prolog `jjtolton` fork [8b307a1c515b9f3489e9a581aff8fb77e8f63e76](https://github.com/jjtolton/scryer-prolog/commit/8b307a1c515b9f3489e9a581aff8fb77e8f63e76).

# Libscryer-clj

[Scryer Prolog](https://www.scryer.pl/) is an [ISO/IEC 13211-1](https://www.iso.org/standard/21413.html) compliant modern prolog system written in rust.

[Clojure](https://clojure.org/index) is an opinionated and practical modern lisp that runs on the JVM, but I'm assuming that if you're reading this, you already know that.

Following in the proud tradition of:

* [libpython-clj](https://github.com/clj-python/libpython-clj),
* [libjulia-clj](https://github.com/cnuernber/libjulia-clj), and
* [libapl-clj](https://github.com/jjtolton/libapl-clj),

you now have the ability to play with one of the most powerful and simple-to-use programming languages ever made, all from the comfort of your own familiar Clojure REPL.

If you are unfamiliar with [Scryer Prolog](https://www.scryer.pl/) or Prolog in general, I would _strongly_ recommend checking out [Markus Triksa](https://www.metalevel.at/)'s [exceptional materials on Prolog](https://www.metalevel.at/prolog) (**Note:** this site and this book are _running on Scryer Prolog... yes, **even the web server**_) and his fantastic YouTube series, [The Power of Prolog](https://www.youtube.com/results?search_query=the+power+of+prolog).

## Status
Pre-alpha, hackers only.  Clojure and Scryer are both individually solid and mature, but the FFI bindings for Scryer Prolog are still being developed. It's worth checking out [shared library documentation](https://github.com/jjtolton/scryer-prolog/tree/ISSUE-2464/scryer-prolog-shared-lib-eval-code-c/docs/shared_library).

## Installation

<details>
<summary>
This guide assumes you already have Clojure setup for you machine.
</summary>

First, until https://github.com/mthom/scryer-prolog/pull/2465 is merged into the upstream branch, you need to clone [my fork](https://github.com/jjtolton/scryer-prolog/tree/ISSUE-2464/scryer-prolog-shared-lib-eval-code-c) of Scryer Prolog, which has the required shared library bindings. 

Then you need to follow the very simple build instructions for [native compilation](https://github.com/mthom/scryer-prolog/pull/2465).

Then it's time to AI party like it's 1972.
</details>

## Usage

Untils this goes on clojars, you'll need to paste this into you `deps.edn` with

<details>
<summary>
Setup:
</summary>

```clojure
jjtolton/libscryer-clj  {:git/url "https://github.com/user/my-library" 
                         :git/sha "insert-sha-here"}
```

It's easiest if you place a `scryer.edn` in the same directory as your `deps.edn`. 

Mine looks like this:

```clojure
{:libscryer-prolog-path "/home/jay/programs/scryer-prolog/target/release/libscryer_prolog.so"
 :prelude
 ":- use_module(library(clpz)).
  :- use_module(library(dif)).
  :- use_module(library(lists))."
 :auto-intialize        true}
```

Then, fire up your REPL and get to work!

```clojure

(require '[libscryer-clj.scryer :as scryer])

;; if you chose not to :auto-intialize, you could do something like this:
(scryer/initialize!
   {:libscryer-prolog-path "/home/jay/programs/scryer-prolog/target/release/libscryer_prolog.so"
    :prelude
    ":- use_module(library(clpz)).
     :- use_module(library(dif)).
     :- use_module(library(lists))."})
```
</details>

There are 2 primary concepts to understand:

<details>
<summary>
1. Consulting, via `consult!`. This is how you load Scryer Prolog source code (facts, clauses, and rules) into your Scryer runtime (the Warren Abstract Machine).
</summary>

Example:

```clojure
(consult! "fact(1).
           fact(2).
           fact(3).
           fact(4).") ;;=> :ok


(query! "fact(X).") ;;=> ({?x 1} {?x 2} {?x 3} {?x 4})

```
</details>

<details>
<summary>
2. Querying, via `query!`, `get-lazy-query-iterator!`, and `lazy-query!` This is how you make Libscryer-clj do work for you!
</summary>
`query!`, as you saw above, is the easiest to use. However, note that it is greedy and exhaustive, and therefore not suitable for the amazing infinitely generative queries that Prolog is capable of.

Consider the following example taken from [Markus's DCG tutorial](https://www.metalevel.at/prolog/dcg):

```prolog
:- use_module(library(dcgs)).

as --> [].
as --> [a], as.

?- phrase(as, As).
```

![](img/as.gif)

Those `as` will keep going forever, and if you try that with `scryer/query!`, you will cause a threadlock.  

However, you can instantiate a query iterator with `scryer/start-lazy-query!` and then use `.next` in conjunction with `scryer/process-prolog-result` to get results lazily!
```clojure
(scryer/consult! "
:- use_module(library(dcgs)).
as --> [].
as --> [a], as.
")

(def query-iter (scryer/get-lazy-query-iterator! "phrase(as, As)."))

#_ (.next query-iter) ;; raw pointer -- resource leak!

;; do this instead (marshalls and deallocates string properly):
(scryer/process-prolog-result (.next query-iter)) ;;=> ({?as []})
(scryer/process-prolog-result (.next query-iter)) ;;=> ({?as "a"})
(scryer/process-prolog-result (.next query-iter)) ;;=> ({?as "aa"})
(scryer/process-prolog-result (.next query-iter)) ;;=> ({?as "aaa"})
;; and so on

;; don't forget to close
(.close query-iter)

;; double close == crash
(.close query-iter) ;;=> IllegalStateException
```

Alternatively, to use a more idiomatic/transducible pipeline, you can use `scryer/lazy-query!`:

```clojure
(with-open [query-iter (get-lazy-query-iterator! "phrase(as, As).")]
    (into []
          (take 10)
          (lazy-query! query-iter))) ;;=> [{?as [""]} {?as "a"} {?as "aa"} {?as "aaa"} ... ]
```

**Note:** the `with-open` automatically closes the query iterator when the block is exited.
However, if you hang on to a lazy reference outside of this block and then try to realize it, an exception will be thrown.

For example:

```clojure
(with-open [query-iter (start-lazy-query! "phrase(as, As).")]
    (take 10 (lazy-query! query-iter))) ;;=> java.lang.IllegalStateException 
```

This is because the resources have been deallocated, and an exception is thrown to prevent a panic or crash.

</details>


## Caveats, Nuances, Limitations, Foot Guns, and Garbage Collection

The bindings are still [under development](https://github.com/mthom/scryer-prolog/pull/2465).

Please check the section in the [documentation](https://github.com/jjtolton/scryer-prolog/tree/ISSUE-2464/scryer-prolog-shared-lib-eval-code-c/docs/shared_library) about Important Caveats and Known Limitations.

    
    
