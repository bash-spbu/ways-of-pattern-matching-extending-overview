# Rethinking pattern matching as embedded DSL

- [Rethinking pattern matching as embedded DSL](#rethinking-pattern-matching-as-embedded-dsl)
  - [About this proposal](#about-this-proposal)
  - [Theoretical motivation](#theoretical-motivation)
  - [Conclusion](#conclusion)
  - [Gasche design overview](#gasche-design-overview)
  - [Alternative solution based on nested matches](#alternative-solution-based-on-nested-matches)

## About this document

Many thoughts and musings of extending pattern matching have been proposed already to be able to reduce some unavoidable boilerplate and solve some tasks more expressively. There were many ideas as [language extensions](https://discuss.ocaml.org/t/musings-on-extended-pattern-matching-syntaxes/3600), ppx modules, external [preprocessors](https://code.google.com/archive/p/ocaml-patterns/) etc.

Language design is a pretty hard task because it is very subjective in its nature, so the problem is to somehow select the best of proposed solutions and be able to prove it. So the goals of this document are as follows:

- to summarize and systemize all information about pattern matching and known approaches to it;
- to give objective theoretical rationales of necessity or otherwise futility of the extension;
- to point out exactly which language elements should be modified and why;
- to give some criterions so that known ideas could be compared with each other.

Finally we will discuss and analyze [@gasche design](https://discuss.ocaml.org/t/musings-on-extended-pattern-matching-syntaxes/3600) and offer [an alternative lightweight solution](#alternative-solution-based-on-nested-matches) based on the nested matches.

## Theoretical motivation

One of the most comprehensive overviews of pattern matching the theoretical background was given in the work [First Class Patterns, M. Tullsen, 2000](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.37.7006). A model described there is pretty universal, is not bound to any particular language or technology and to this day it is still one of the most flexible and powerful approaches to the pattern matching task. In this document we will mainly concentrate on the opportunities provided by this model: we are interested which properties are already supported by OCaml and which are not. So to begin with, we will will give a short overview of the main conceptions stated in the M. Tullsen work.

It is claimed that any single pattern is nothing more than an arbitrary function of the type `'a -> option b`. For instance, pattern `x :: xs` is represented by the function

```ocaml
let head_option: 'a list -> ('a * 'a list) option = (* ... *)
```

And pattern `v1 = v2` can be represented as

```ocaml
let is_equal: 'a * 'a -> () option = (* ... *)
```

Herewith patterns can take any additional parameters along with scrutinee and also can be given itself as parameters (to other patterns as well), that is patterns are first class values. Thereby the problem of performing pattern matching on the abstract types is solved automatically – now the module just needs to export the set of patterns represented by plain functions.  

Farther the work introduces pattern combinators which allow building of arbitrary complex patterns (their definitions are quite intuitive and can be seen in the original work):

```ocaml
type ('a, 'b) pat = 'a -> 'b option

val ($|): ('a, 'b) pat -> ('a, 'b) pat -> ('a, 'b) pat (* or *)
val ($&): ('a, 'b) pat -> ('a, 'b) pat -> ('a, ('b, 'b)) pat (* and *)
val ($:): ('a, 'b) pat -> ('b, 'c) pat -> ('a, 'c) pat (* then *)
val ($>): ('a, 'b) pat -> ('b -> 'c)   -> ('a, 'c) pat (* compose *)
val ($*): ('a, 'c) pat -> ('b, 'd) pat -> (('a, 'c), ('b, 'd)) pat
(* parallel *)
```

Thereby now instead of the using of special pattern matching construction (`match`-clause for instance) one could just apply the pattern to the inspecting value as a plain function, what **fundamentally protected from the dreaded `MatchError`**!

And finally the author noticed that the last thing which has not been abstracted yet is the type `option` in the pattern types. It was replaced by the type class `MonadPlus`, so that now the semantics of the pattern matching could been parameterized by the any instance type of the `MonadPlus` type class. Some use cases of such property were presented in the original work and in [F# Active patterns paper](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.542.4243&rep=rep1&type=pdf).

To sum up, we show again which **opportunities** are provided by the described model:

1) Patterns are represented by plain functions – there is no need for the special support from the compiler and patterns are automatically first class values, fully typed and can be parameterized.
2) If the pattern is build by pattern combinators, its application can not throw `MatchError` and does not **demand match construction at all**.
3) Pattern matching now can be applied to abstract types due to possibility of module patterns export.
4) Match semantics can be parameterized by types.

Note, that currently none of these opportunities is supported by the OCaml directly.

However this model has two significant drawbacks:

1) Though it is pretty subjective, but (as also noted [here](https://www.researchgate.net/publication/2646388_Pattern_Guards_and_Transformational_Patterns)) pattern combinators usage is very clumsy and hard to understand compared with well-known `match`-expressions, mostly because of its point-free form. The following example contrasts two notations:

    ```haskell
    -- Haskell code
    zipList1, zipList2 :: ([a], [b]) -> Maybe [(a, b)]

    zipList1 x : xs y : ys = Just $ ((x, y) :) <*> zipList1 xs ys
    zipList1 []     []     = Just []
    zipList1 _      _      = None

    -- Cons# is a compiler generated function defined as
    -- Cons# (Cons x) = Just x
    -- Cons# _        = Nothing
    zipList2 = []# .* []# .-> (\_ -> [])
            .| :#  .* :#  .:  (pid .* zipList2 .-> uncurry (:)) . zipTuple

    zipTuple ((x1, x2), (y1, y2)) = ((x1, y1), (x2, y2))
    ```

2) What is more important is the fact that patterns construction **happens in the runtime** (what actually increases its theoretical flexibility) and its evaluations is processed strictly according to how it was built, what does not allow logical optimizations, and uses plain function invocations, what does not allow physical optimizations. Thereby the overall **performance** of this model is completely **unacceptable**. There are actually two possible solutions of this problem:

   1) JIT-compilation (demands huge efforts from the compiler) which lets completely implement all theoretical opportunities with acceptable performance.
   2) Extra introduction of the pattern matching construction to the language as an *embedded DSL* which would preserve all needed properties and allow to express the biggest part of the theoretically possible matches in compile time, what would let the compiler to perform its optimizations and generate the most effective machine codes. Additionally it would also improve a pattern construction readability making it more declarative compared to the pattern combinators.

   We suggest to concentrate on the second item, because it is much easier for the implementation, more habitual in usage and finally it allows to get the maximum performance of the generated code. After all OCaml already has the pattern matching construction and to stop use it would be strange (slightly reminds a situation with an object system).

Here we should note, what actually we accustomed to call as a pattern matching construction. Semantically, pattern *declaration* and pattern *application* are two different and independent tasks, however our well-known `match` performs both actions at once. For example, in the function

```ocaml
let rec last lst =
  match lst with
  | []     -> None
  | [x]    -> Some x
  | _ :: t -> last t
```

`match` declares a pattern `([] -> None) | ([x] -> Some x) | (_ :: t -> last t)` and at the same time applies it to the scrutinee `lst`. Also note, that **result expression is a part of the pattern** and not the separate object of the match construction! (N.B. look how natural in these terms seems the [@gasche idea about *generalized handlers*](https://discuss.ocaml.org/t/musings-on-extended-pattern-matching-syntaxes/3600).)

Finally, we declare two *objective* criterions for the searching of the best design:

1) We call the pattern matching construction **maximal** if any arbitrary pattern built by pattern combinators can be declared by it as well.
2) We call the pattern matching construction **expressive** if it does not demand a duplication of the syntactically equal code snippets.

Now it good to find out whether current abilities of the `match`-construction satisfy maximum and expressiveness criterions or not, for what we will discuss patterns and each combinator separately.

Currently only data constructors can be used as base patterns (and any predicate in section of the guard expressions, which we will discuss later). From the one hand this limitation increases opportunities of the match optimizations, but from the other it makes **impossible the abstraction over patterns** (including pattern matching with abstract types instances). Moreover patterns are not first class values and can not be parameterized. It is first and very serious lack among the current OCaml pattern matching abilities, which deserves some attention. One of simplest and elegant solutions is the conception of [F# active patterns](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.542.4243&rep=rep1&type=pdf) or [Haskell view patterns](https://gitlab.haskell.org/ghc/ghc/wikis/view-patterns#further-syntactic-extensions). Such feature will allow implementation of many ideas by language means without special compiler support like [pattern ranges](https://github.com/ocaml/ocaml/issues/8504) and [additional bindings](https://github.com/ocaml/ocaml/issues/6901)(which we will also discuss later).

Now we will review the current support of the pattern combinators:

1) `($|) (* or match *)` combinator is *partially supported* by or-patterns. [As @gasche notes in the section about *generalized handlers*](https://discuss.ocaml.org/t/musings-on-extended-pattern-matching-syntaxes/3600), the problem is that it is impossible to specify different result expressions for each disjunct, what demands the duplication of the whole match branches (also note [the @shonfeder comment](https://discuss.ocaml.org/t/musings-on-extended-pattern-matching-syntaxes/3600/20)):

    ```ocaml
    match e with
    | (p, q1) -> e1
    | (p, q2) -> e2

    (* instead of more natural *)

    match e with
    | (p, (q1 -> e1 | q2 -> e2))
    ```

    Another problem is an [impossibility of the introducing of additional binds for one of the disjuncts](https://github.com/ocaml/ocaml/issues/6901), that also demands the code duplication. An example from the link:

    ```ocaml
    let rec unif x y =
    match x,y with
      | Var r           , _ when !r != None -> unif (deref f   []) y
      | App(Var r, args), _ when !r != None -> unif (deref f args) y
      | ...
    ```

    There is no mean to add `[] as args` binding for the first disjunct and it requires to duplicate the whole branch logic including guard section.

2) `($&) (* and match *)` combinator **is currently unsupported**. Nonetheless its natural necessity has been already noted in some works, for instance in [F# active patterns](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.542.4243&rep=rep1&type=pdf), which supports them, and also in [Successor-ML project](https://people.mpi-sws.org/~rossberg/hamlet/README-succ.txt). Now one does not feel any necessity in it because only matches with data constructors are allowed which are always mutually exclusive. However with the permission of arbitrary patterns they can overlap and there may be the task of successful application of several patterns to one object.

    Let us also mention [@gasche's note](https://discuss.ocaml.org/t/musings-on-extended-pattern-matching-syntaxes/3600) that OCaml already has keyword `as` which can be used for the pattern intersection support such as `p as q`.

    The example from the F# active patterns work in the proposed syntax:

    ```ocaml
    (* Proposed syntax with active patterns support *)
    match xml_data with
    | (Attr "x" (Float x) as
       Attr "y" (Float y) as
       Attr "z" (Float z)) -> Some(x,y,z)
    | _ -> None
    ```

    Note also that the `$&` combinator can be emulated by active patterns or [view patterns](https://gitlab.haskell.org/ghc/ghc/wikis/view-patterns#both-patterns), for example:

    ```fsharp
    // F# code
    let (|All3|) x = (x, x, x)

    match xml_data with
    | All3(Attr "x" (Float x), Attr "y" (Float y), Attr "z" (Float z))
        -> Some(x,y,z)
    | _ -> None
    ```

3) `($:) (* then match *)` is *partially supported* by structural nesting of patterns (there is also a solution by nested matches which we discuss in its [own section](#alternative-solution-based-on-nested-matches)). For example, in the pattern `1 :: tl` the outer pattern `::` first performs match with list constructor, and then inner patterns `1` и `tl` performs match with number and binding respectively. The problem is in the impossibility of application patterns with several arguments. Because now only data constructor patterns are allowed, which requires exactly one argument, there is no such problem. However let us consider the function of the removal of sequential duplicates in list:

    ```ocaml
    let rec destutter = function
      | [] | [_] as l -> l
      | hd :: (hd' :: _ as tl) when hd = hd' -> destutter tl
      | hd :: tl -> hd :: destutter tl
    ```

    We need to check on equality `hd` and `hd'` for what we must use the function `=` with two parameters. First of all lets note that to allow a sequential matching and to allow usage of arbitrary functions in the pattern matching construction are absolutely **different and independent** tasks. But in the OCaml language there were implemented **guards expressions** which somehow solve both tasks at the same time but are **very limited** in its opportunities. At first, the `when` section is allowed only at the outermost level of patterns as a part of the branch though in general it should be allowed on the arbitrary depth of pattern. At second, only boolean functions can be used. And finally, in order to use several predicates we can unite them by conjunction, whereas to use several arbitrary patterns (represented by functions) we can not do the such thing and need the possibility to order several sequential matches.

    In the Haskell there was suggested ind implemented a [generalization of guard expressions to pattern guards](https://www.researchgate.net/publication/2646388_Pattern_Guards_and_Transformational_Patterns), which allow usage of arbitrary functions and unbounded on its length line of sequential matches. For example:

    ```haskell
    filtSeq :: (a -> Bool) -> Seq a -> Seq a
    filtSeq p xs
      | Just (y, ys) <- lview xs, p y = lcons y (filtSeq p ys)
      | Just (y, ys) <- lview xs      = filtSeq p ys
      | otherwise                     = nil
    ```

    An arbitrary pattern `pat :: a -> Maybe b` can be used as

    ```haskell
    f x
      | Just (...) ← pat x = ...
    ```

    However, such syntax demands the duplication of branches prefixes (as with `Just (y, ys) <- lview xs` in the example above), so that is why it can not be named *expressive* according to our definition.

    To sum up, as we can see currently the `$:` combinator has very weak support in the OCaml though it is needed even in some simplest pattern matching tasks.
4) `($*) (* parallel match *)` combinator is fully supported by the structural nesting of patterns. For example, in the expression

    ```ocaml
    match something with
    | (x :: xs, y :: ys) -> (* ... *)
    ```

    `x :: xs` and `y :: ys` performs parallel match.
5) `($>) (* compose match *)` combinator is *partially supported* but in general it requires the support of arbitrary patterns.

    Probably this is not really interesting case, but for the fulness of the overview we will also discuss it. This combinator allows map the binding value by some function and bind its result instead of this value. In most cases one does it by performing needed function invocation inside the result expression, for example:

    ```ocaml
    match something with
    | A x -> (*...*) f x (*...*)
    ```

    However we might want something like

    ```ocaml
    (* Imaginary syntax *)
    match something with
    | A (y by f) -> (*...*) y (*...*) (* here y = f x from the snippet above *)
    ```

    It looks unessential but it can have some effects with more complicated matches, in particular with or-patterns, for instance:

    ```ocaml
    match something with
    | A (x by f) | B x -> (*...*)
    ```

    Here the feature is that if the binding will happen in the first pattern, its value will be additionally mapped by f, whereas if the binding will happen in the second pattern, its value will not. Currently in the OCaml it will be written as

    ```ocaml
    match something with
    | A x -> ... f x ...
    | B x -> ... x   ...
    ```

    And here we must duplicate the result expressions with replacement `x` on `f x` or vice versa.

## Conclusion

In this document we wanted to show that, theoretically, the well-known pattern matching is nothing more than an embedded DSL which implements model described in [First Class Patterns, M. Tullsen, 2000](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.37.7006). And also as we have seen currently only a part of available functionality is supported by the OCaml `match`-expressions.

3 (almost) independent task can be formulated in order to achieve the full support of the pattern matching:

1) The support of arbitrary patterns. Possible solutions: [active patterns](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.542.4243&rep=rep1&type=pdf) or the ability of any function invocation inside the `match`-expression as [Haskell's view patterns](https://gitlab.haskell.org/ghc/ghc/wikis/view-patterns#further-syntactic-extensions).
2) The full support of all combinators. As we could see, the current support of combinators is pretty irregular and lopsided.
3) The support of pattern matching semantics parameterization by types implementing `MonadPlus`. By now it is probably the most independent and not the most important task, thereby it probably should be implemented latest.

## Gasche design overview

In this section we want to discuss one of the suggested extensions designs: [@gasche with-section and generalized handlers](https://discuss.ocaml.org/t/musings-on-extended-pattern-matching-syntaxes/3600). We will judge it by earlier defined criterions of maximum and expressiveness to show the example of the design objective estimation.

@gasche suggests to allow usage of arbitrary patterns in `and with` sections, and besides it is possible to write unbounded by length sequential matches as `x and f x with y and g y with z and ...`.

Also *generalized handlers* are introduced which allow to specify result expressions on the arbitrary depth. It is necessary for the full support of `$|` and `$&` combinators and also fully corresponds to the theoretical model according to which result expressions are parts of patterns.

Thereby, @gasche design:

1) Allows to support arbitrary patterns by invocation of any function inside the `and with` section, that solves the problem of pattern matching over abstract types, though in less expressive syntax compared to the active patterns design.
2) **Fully** supports `$|` combinators, *fulfills needed requirements* for the `$&` combinator support and **fully** supports `$>` combinator (due to ability of arbitrary patterns usage).
3) Does not influence on the pattern matching semantics parameterization task.

One important question is where the `and with` section can be used. If its usage is allowed only at the outermost level (as guard expressions) then such design is not maximal (because `$:` combinator support will still be very limited) not expressive (as discussed further).

However, if make it possible to use section `and with` on arbitrary depth and add the support of the `$&` combinator (via `as` clause) then such design will be both **maximal** and **expressive**.

As an example lets consider the following [cascade of nested matches](https://discuss.ocaml.org/t/musings-on-extended-pattern-matching-syntaxes/3600/21):

```ocaml
match f () with
| Some y ->
    begin match g y with
    | A x ->
        begin match h x with
        | Ok result -> result
        | Error _ -> raise Not_found
        end
    | _ -> raise Not_found
    end
| None -> raise Not_found
```

Without backtracking opportunity we are to duplicate the fallback logic.

In terms of @gasche design such match can be rewritten much shorter as:

```ocaml
match f () with
| Some y and g y with A x and h x with Ok result -> result
| _ -> raise Not_found
```

N.B. For the sake of fairness we note the *subjective opinion*, that such case of full fallback logic coincidence between different branches happens not that frequently, because usually every place is characterized with its own index or textual message of the error. E.g. in the production code it would be more probably written as something like:

```ocaml
match f () with
| Some y ->
    begin match g y with
    | A x ->
        begin match h x with
        | Ok result -> result
        | Error _ -> raise (Not_found "because of 1...")
        end
    | _ -> raise (Not_found "because of 2...")
    end
| None -> raise (Not_found "because of 3...")
```

And in such case this code does not contain any logic duplication and fully corresponds the needed behavior. There is also an [opinion](https://discuss.ocaml.org/t/musings-on-extended-pattern-matching-syntaxes/3600/22) that such logic probably should be implemented with more high-level means such as monads. One more realistic example with different error messages can be found [here](https://code.google.com/archive/p/ocaml-patterns/wikis/PatternGuards.wiki).

Now lets consider an example with the [additional introduction of bindings in disjuncts](https://github.com/ocaml/ocaml/issues/6901).
Due to the opportunity of usage `and with` section on an arbitrary depth, additional bindings can be introduced for any pattern:

```ocaml
let rec unif x y =
  match x, y with
  | Var r and args with [] | App(Var r, args), _ when !r != None ->
      unif (deref f args) y
  | ...
```

Also note, that if `and with` section was allowed only at the outermost level as guard expression then it would be impossible to rewrite this example without logic duplication, what shows that such design is not expressive.

## Alternative solution based on nested matches

[As @shonfeder noted](https://discuss.ocaml.org/t/musings-on-extended-pattern-matching-syntaxes/3600/20) many examples can be equivalently rewritten via nested `match`-es. For example, following definitions are semantically the same:

```ocaml
let rec filter_map_1 f li = match li with
  | [] -> []
  | x :: xs and f x with (
    | Some y -> y :: filter_map f xs
    | None   -> filter_map f xs
  )

let rec filter_map_2 f li = match li with
  | [] -> []
  | x :: xs -> match f x with
    | Some y -> y :: filter_map f xs
    | None   -> filter_map f xs
```

And indeed, in terms of pattern combinators nested `match` expressions fully implement the `$:` combinator with just one single exception – they do not support the backtracking opportunity, i.e. when inner `match` fails then instead of a return to the outer match continuation the `MatchError` is thrown. But when the inner `match` is exhaustive than there is no such problem and we have the **full support** of the `$:` combinator by the current means of the language!

Backtracking opportunity is indeed quite easy to implement by the addition of the single keyword with trivial semantics. Or we can even use an extension point, what frees us from any modification of the language grammar. In this terms [example with inner matches cascade](https://discuss.ocaml.org/t/musings-on-extended-pattern-matching-syntaxes/3600/21) can be rewritten as:

```ocaml
match f () with
| Some y ->
    begin match%back g y with
    | A x ->
      begin match%back h x with
      | Ok result -> result
      end
    end
| None -> raise Not_found
```

The logic duplication is fully removed! And the support of the `%back` extension point is almost trivial – compiler just needs to generate an additional fallback-branch with the jump to the needed pattern of the outer branch (which one exactly can be said by the [logical optimizer](https://www.researchgate.net/publication/2840783_Optimizing_Pattern_Matching)), what is also actually a very effective implementation.

However, as we can see, such code is quite hard to read because of the syntactic noise from keywords `begin`, `end`, `%back` etc. But it is easy to simplify by the introduction of some syntactic sugar. Note that it is impossible to judge objectively between different variations of syntactic sugar but anyway its support is also trivial in the compiler. For example, it can be a good reason to return to [the discussion of the `if-let` syntax](https://github.com/ocaml/ocaml/issues/6685), in which terms the example could be rewritten as:

```ocaml
match f () with
| Some y ->
    if%back A  x      = g y then
    if%back Ok result = h x then
    result
| None   -> raise Not_found
```

It looks very simple and even in the monadic spirit of the recently added `let*` and `and+`, moreover this version would be probably compiled more efficiently than monadic one.

Now let us objectively judge this design:

1) It is possible to use arbitrary patterns as function invocations via backtracking nested matches. It covers all theoretical functionality but it can not be expressed via patterns structural nesting. As example we can compare this notation to [F# active patterns](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.542.4243&rep=rep1&type=pdf):

    ```fsharp
    // F# code
    type ('a, 'b) choice2 =
      | Choice2_1 of 'a
      | Choice2_2 of 'b

    // Active patterns are implemented as plain functions.
    let (|Cons|Nil|): LazyList<'a> -> (('a, LazyList<'a>), ()) choice2 = (* ... *)

    let rec pairSum xs =
      match xs with
      | Cons(x, Cons(y,ys)) -> consl (x + y) (lazy (pairSum ys))
      | Cons(x, Nil())      -> consl x (lazy nil)
      | Nil()               -> nil

    (* OCaml with nested matches version *)
    let rec pairSum xs =
      match (|Cons|Nil|) xs with
      | Choice2_1(x, tl) ->
          (* Because nested match is exhaustive we can omit %back specifier *)
          begin match (|Cons|Nil|) tl with
          | Choice2_1(y, ys) -> consl (x + y) (lazy (pairSum ys)
          | Choice2_2()      -> consl x (lazy nil)
          end
      | Choice2_2() -> nil
    ```

    This is much less attractive, but this design does not exclude active patterns: moreover it can be very practical to support both these extensions.
2) `$|`, `$&` and `$>` combinators support has not changed, `$:` is **fully supported**.
3) Does not influence on the pattern matching semantics parameterization task.

This design does not improve the `$|` combinator support (for instance [example with additional bindings](https://github.com/ocaml/ocaml/issues/6901) still can not be rewritten without logic duplication), that is why this extension is not maximal nor expressive. But with additional support of [active patterns](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.542.4243&rep=rep1&type=pdf) this problem can be solved. For instance, the example above can be rewritten as:

```fsharp
// F# code
let (|bind|) binding_value scrutinee = (binding_value, scrutinee)

let rec unif x y =
  match x, y with
  | bind [] (args, Var r) | App(Var r, args), _ when !r != None ->
      unif (deref f args) y
  | ...
```

Thereby, with additional support of the `$&` combinator and active patterns this design will be maximal and expressive for the most of the tasks. Its benefits is that it does not influence the OCaml language, requires the trivial support from the compiler and can be quite easily integrated with current pattern matching compilation scheme. But it demands an additional introduction of the syntactic sugar (for example, [if-let](https://github.com/ocaml/ocaml/issues/6685)), which support is also quite easy and which will be useful in other tasks as well.

To sum up, the combination of support of

1) `$&` combinator,
2) [active patterns](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.542.4243&rep=rep1&type=pdf) or [view patterns](https://gitlab.haskell.org/ghc/ghc/wikis/view-patterns#further-syntactic-extensions),
3) this extension,
4) syntactic sugar [if-let](https://github.com/ocaml/ocaml/issues/6685)

represents 4 weakly-connected and well formalized tasks with almost disjoint functionalities and minimal syntactic novelties. They can be developed in parallel and their summary result provides almost maximal theoretical opportunities of the pattern matching, at the same time preserving the current level of the performance.
