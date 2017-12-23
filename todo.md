# things that should be done:

### Whenever
- functions on collections
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
    - composition
    - threading a-la clojure
    - automatic currying
    - partial application

### Short term:
- [22. Dec] *GIT GIT GIT GIT* i shalt break the interpreter irreversibly ... nevermore
- [22. Dec] Numeric functions *N E E D* to throw errors when the args are of wrong type
- [21. Dec] error handling
- [on hold] refactoring. in particular:
    - left-factoring number parsing
    - left-factoring cond/case
- [21. Dec] split the module, at least into parser/primitives/main
- [21. Dec] (Maybe) split the primitives module in types/exception/primitives

### Nice to haves:
- parser for delimited sequences ('[' a b c ... ']' etc.)
- left-factor parsers for common subsequences

When this is done right the interpreter should be fairly usable, and
splitting/refactoring the parser should make new patterns easier to
implement (special forms, expressive syntax, list comprehensions)

### Long term:
- lexical scopes, environments
- symbol definitions
- let / do constructs for imperative programming

- assosciative datastructure (hashmap/search tree/trie)
- sets or some other unordered elem/not elem data structure

- functions and lambdas (dep. lexical scoping)
    - partial application and automatic currying
- include-functionality

#### Libraries
- Linear algebra
- Statistics
- Graph theory
- Number theory

### Maybe never:
- Implement arrays as vector tries a-la clojure
- Monads (*at least* Maybe)
    - then we need >>= in some form to fight verbosity
- ez regex syntax

### Lambda, function notation options
```
(lambda x y z. (do ...))

(λ x y z. (do ...))

(λ x y. f x y)

(fn func x y z. do ...)

(fn square x. * x x)

(λ x. * x x)

(fn func (x y z) do ...)
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

(def x 5)
(switch x on 4 -> "four". 5 -> "five". _ -> "unknown") ;; -> "five"
(switch x on 4 then "four". 5 then "five". _ then "unknown") ;; -> "five"

(cond (predicate) then (do ...).
      (predicate) then (do ...).
                  else (do ...))
```