# things that should be done:

### language features
- comments
- variaduc functions -> currying and composition, pipes
- class or struct
- do
- list-comprehension or for/yield ala scala
- symbols (:sym) ala ruby

### nice-to-haves
- documentation baked into the interpreter, ala clojure
- make vector functions operate on lists *
- make let expand to nested lanbdas, so *we* dont need to nest let-blocks

* this can (hopefully) be done at interpreter-level as
```
f (LieStr s) = LieStr $ toList (f $ LieVec $ fromList s),
```
in other words converting converting it to and from a vector.
this is probably O(n), but the only other option is to change
the implementation of strings to vectors of chars, and this is
hard (maybe impossible), since a LieChar would be indistinguishable
from any other type, and ghc can not enforce LieVec LieChar, only
LieVec LieVal, which could be anything.

### functions
- zip
- unzip
- uncons
- intersperse
- transpose
- permutations
- pairs

### interpreter-level
- more, and better, error handling

### misc
- documentation
  - write a latex-document
  - make a website (lie.haug.rs)

-------------------------------------------------------------------------
4. Jan

### Whenever
- functions on collections (all these are done, but we need more)
    - fold (perhaps foldl and foldr)
    - fold1
    - map
    - filter
    - cons
    - len
    - take-n
    - head
    - last
    - tail
    - min/max

- functions on strings
    - join
    - slice (maybe this is dumb, because it is O(n))
    - words / split-at (words would be dimplemented as split-at ' ')
    - eval (??? o fuk)
    - contains (this cay be dumb, O(n))

- functions on functions / features of functions
    - (partially working *) composition
    - threading a-la clojure
    - (partially working *) currying
    - partial application

* curry and compose are implemented for arity-1 functions, but without
*proper* variadic functtions better implementations are hard.

### Data structures
- maps (radix trees or hash maps)
- sets (binary tree)
- queue / stack
    - other types of queue structures (min/max binary heaps)

Implementing mutable data structures is probably (maybe) not desirable.

### Short term:
- [22. Dec] git
- [22. Dec] Numeric functions *N E E D* to throw errors when the args are of wrong type
- [21. Dec] error handling
- [on hold] refactoring. in particular:
    - left-factoring number parsing
    - left-factoring cond/case
- [21. Dec] split the module, at least into parser/primitives/main
- [21. Dec] (Maybe) split the primitives module in types/exception/primitives
- [] special forms
    - [23. Dec] if/unless
    - [23. Dec] case-on/cond
    - [done] let / do
    - [waiting for compose] threading (clojure ->>) or equivalent

### Nice to haves:
- [23. dec] port REPL to readline

### Long term:
- [24. Dec] lexical scopes, environments
- [24. Dec] symbol definitions / mutation
- [25. Dec] let construct for imperative programming
- *CURRYING*

- assosciative datastructure (hashmap/search tree/trie)
- sets or some other unordered elem/not elem data structure

- [24. Dec] functions and lambdas (dep. lexical scoping)
    - partial application and automatic currying
- [25. Dec] include-functionality

#### Libraries
- Linear algebra
- Statistics
- Graph theory
- Number theory
- networking
- databases
- foreign function interface *
  - interface to rust would be fun
  - interface to haskell would be useful, and almost as fun
  - interface to C/C++ would be really useful for libs
  - Fortran if we really want to get dirty with linear algebra

* extensions to other languages will let us implement libraries as thin
wrappers of other libraries. some useful ones would include:
- Diesel (Rust), for databases
- Rocket (Rust), webservers et. al.
- SQLite (C)
- LAPACK (Fortran), linear algebra
- ALTAS (C), linear algebra

### Maybe never:
- Implement arrays as vector tries a-la clojure
- Monads or something monad-ish (*at least* Maybe)
    - then we need >>= in some form to fight verbosity
- easy basic regex syntax (perl compatible, with /expr/-notation)

# Notes and ideas

### Lambda, function notation options
```
(lambda x y z. (do ...))

(λ x y z. (do ...))

(λ x y. f x y)

(fn func x y z. do ...)

(fn square x. * x x)

(λ x. * x x)

(fn func (x y z) do ...)

-- multiple-arity (ala clojure)
(function foobar
  (λ x. "arity one")
  (λ x y. "arity two")
  (λ x y z. "arity three"))

```

### Switch, if/unmless notation options
```
(if (predicate) then
    (do ...)
 else
    (do ...))

(case expr of
    k1 then v1.
    k2 then v2.
    k3 then v3.
       else v4)

(case x of
    4 then "foobar".
    5 then "loodar".
    6 then "zoogar".)

(def x 5)
(switch x on 4 -> "four". 5 -> "five". _ -> "unknown") ;; -> "five"
(switch x on 4 then "four". 5 then "five". _ then "unknown") ;; -> "five"

(cond (predicate) then (do ...).
      (predicate) then (do ...).
                  else (do ...))
```

### Let notation, do
```
(let a <- 5.
     b <- 7.
 in (+ a b))

-- convert to ((lambda a b. + x y) 5 7)

(fn foldl fun acc vec.
    let x  <- (head vec).
        xs <- (tail vec).
    in foldl fun (fun acc x) xs)

-- do
(do (println "foo")
    (println "bar")
    x)

-- threading
(->> x. square -> halve -> (λ x. + 3 x) -> fac)
```

### Currying and parial application
```
(f x y z) -> (((f x) y) z)
```

### Example functions
```
(fn decr n. - n 1)

(fn fib n.
    cond (zero? n) then 1.
         (one? n)  then 1.
                   else (+ n (fib . decr n)))

(fn fib n.
    case n of
        0 then 1.
        1 then 1.
          else (+ n (fib . decr n)))

(fn collatz n.
    case (mod n 2) of
        0 then (div n 2).
          else (+ 1 (* 3 n)))
```

### For-like construct
```
-- Should build cartesian product of vectors
(for i <- vector1.
     j <- vector2.
 yield (f i j))

[f i j | i <- vector1. j <- vector2]
```

### uncons
```
(let [v vs] <- (uncons vec).)

```
