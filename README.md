# Writing parsers combinators with F#6 `[<InlineIfLambda>]`

For [F# Advent 2021](https://sergeytihon.com/2021/10/18/f-advent-calendar-2021/) I wrote a [blog post](https://gist.github.com/mrange/fbefd946dba6725a0b727b7d3fd81d6f) exploring how F#6 `[<InlineIfLambda>]` can improve data pipeline performance.

I was thinking of other places where `[<InlineIfLambda>]` and decided to try to build a parser combinator library with `[<InlineIfLambda>]`.

## Parser combinators

One of the best overall F# libraries out there is [FParsec](http://www.quanttec.com/fparsec/), this happens to build a parser combinator library that allows you to create complex parsers from combining simple parsers.

Graham Hutton and Erik Meijer has written an excellent [introduction](https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf) to parser combinators. Reading this article many years ago it was the first time that I felt; this is what Functional Programming is all about.

We build a parser combinator library by defining what is a parser and then various functions to combine parser functions.

One simple Parser could be this:

```fsharp
type 'T Parser = string -> ('T*string) option
```

This parser takes a string, produces a value and the unconsumed string. If the parser fails it returns `None`.

It's possible to build a parser combinator library but it won't be efficient as we need to constantly produce substrings.

## FParser

My first attempt for the parser combinator was this

```fsharp
type 'T Parser = string -> int -> ('T*int) option
```

The parser takes a string and the current position and returns a value and the position of the first unconsumed char if successful.

While experimenting with performance I found that the overhead from create tuples and options were undesirable, switching to value tuples and options reduced performance but avoided GC pressure.

In the end I ended up with this:

```fsharp
type FParserContext(input : string) =
  class
    [<DefaultValue>] val mutable Pos : int

    member x.Input = input
  end

type 'T FParser = FParserContext -> 'T
```

The `ParserContext` stores the string to be parsed and the current position. A parser accepts the `ParserContext` and returns a value if successful and has updates the position in the context. Upon failure, the parser returns the default value of `'T` and has set the position to a negative value.

Not elegant but it was the most performant option I found. There might be better ones.

I will not go through all combinators but I show how `<|>` works

```fsharp
  // <|> runs the first parser and if successful returns its value
  //  , otherwise runs the second
  // Tag as inline + InlineIfLambda to inline both the function and the nested parsers
  let inline (<|>) ([<InlineIfLambda>] pf : _ FParser) ([<InlineIfLambda>] ps : _ FParser) : _ FParser =
    fun c ->
      // Save the current pos
      let spos = c.Pos
      // Runs the first parser
      let fv = pf c
      // Did we succeed?
      if c.IsGood () then
        // Yes
        success fv
      else
        // Otherwise restore the position and run the second parser
        c.Pos <- spos
        ps c
```

With the parser combinator library one can implement a simple expression parser

```fsharp
// Expression tree for expressions like: x*(2+123*y)
type Expr =
  | Value     of int
  | Variable  of string
  | Binary    of char*(int->int->int)*Expr*Expr

module FParserCalculator =
  open FParser.Core
  open FParser

  let inline ptoken label v =
    pskipChar label v
    >>. pskipWhitespace ()

  let inline pop exp ([<InlineIfLambda>] t) ([<InlineIfLambda>] m) =
    pcharSat exp t |>> m .>> pskipWhitespace ()

  // Normally should have been a module but that interacts poorly with
  //  for some reason Benchmark.NET
  type ParserState() =
    class
      // Since the grammar is recursive use pfwd to do a forward declaration of
      //  pexpr
      let struct (pexpr, sexpr) = pfwd<Expr> ()

      // A term is either
      let pterm : Expr FParser =
            // An integer
            (pint () |>> Value .>> pskipWhitespace ())
            // A variable
        <|> (pstringSat1 "variable" (fun ch p -> (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z')) |>> Variable .>> pskipWhitespace ())
            // Or a sub expression wrapped in parantheses
        <|> (ptoken "(" '(' >>. pexpr .>> ptoken ")" ')')

      let op0 ch =
        match ch with
        | '*' -> fun l r -> Binary ('*', ( * ), l, r)
        | '/' -> fun l r -> Binary ('/', ( / ), l, r)
        | _   -> failwith "Expected * or /"
      let op1 ch =
        match ch with
        | '+' -> fun l r -> Binary ('+', ( + ), l, r)
        | '-' -> fun l r -> Binary ('-', ( - ), l, r)
        | _   -> failwith "Expected + or -"
      // pop0 parses expression separated by */
      let pop0 : Expr FParser =
        pterm >+> pchainLeft1 (pop "*/" (fun ch -> ch = '*' || ch ='/') (fun ch -> op0 ch))
      // pop1 parses expression separated by +-
      //  by splitting it in this way we get different precendece for */ and +-
      let pop1 : Expr FParser =
        pop0 >+> pchainLeft1 (pop "+-" (fun ch -> ch = '+' || ch ='-') (fun ch -> op1 ch))
      do
        // We now have a full pexpr parser (pop1), set it using sexpr
        sexpr pop1
      // The full grammar
      let pfull = pskipWhitespace () >>. pop1 .>> peof ()

      member x.Parse s = prun pfull s
    end
```

I also implemented a similar grammar using `FParsec` and painstakingly implemented a `Baseline` parser in an imperative style.

## Benchmark results

Using `Benchmark.net` I compared the performance of `FParser`, `FParsec` and `Baseline`.

```
BenchmarkDotNet=v0.13.1, OS=Windows 10.0.19044.1415 (21H2)
Intel Core i5-3570K CPU 3.40GHz (Ivy Bridge), 1 CPU, 4 logical and 4 physical cores
.NET SDK=6.0.101
  [Host] : .NET 6.0.1 (6.0.121.56705), X64 RyuJIT DEBUG
  PGO    : .NET 6.0.1 (6.0.121.56705), X64 RyuJIT
  STD    : .NET 6.0.1 (6.0.121.56705), X64 RyuJIT


|                     Method | Job |       Mean |   Error |  StdDev |  Gen 0 | Allocated |
|--------------------------- |---- |-----------:|--------:|--------:|-------:|----------:|
|   Baseline_BasicExpression | STD |   193.0 ns | 0.34 ns | 0.32 ns | 0.0713 |     224 B |
|    FParser_BasicExpression | STD |   229.6 ns | 0.41 ns | 0.34 ns | 0.0713 |     224 B |
|    FParsec_BasicExpression | STD |   675.0 ns | 2.45 ns | 2.05 ns | 0.1631 |     512 B |
| Baseline_ComplexExpression | STD |   548.7 ns | 1.36 ns | 1.21 ns | 0.2804 |     880 B |
|  FParser_ComplexExpression | STD |   678.2 ns | 1.08 ns | 0.96 ns | 0.2804 |     880 B |
|  FParsec_ComplexExpression | STD | 1,668.4 ns | 2.90 ns | 2.57 ns | 0.3986 |   1,256 B |
|   Baseline_BasicExpression | PGO |   168.4 ns | 0.54 ns | 0.45 ns | 0.0713 |     224 B |
|    FParser_BasicExpression | PGO |   200.8 ns | 0.41 ns | 0.34 ns | 0.0713 |     224 B |
|    FParsec_BasicExpression | PGO |   595.1 ns | 0.86 ns | 0.81 ns | 0.1631 |     512 B |
| Baseline_ComplexExpression | PGO |   429.6 ns | 1.18 ns | 1.11 ns | 0.2804 |     880 B |
|  FParser_ComplexExpression | PGO |   566.2 ns | 0.78 ns | 0.70 ns | 0.2804 |     880 B |
|  FParsec_ComplexExpression | PGO | 1,432.3 ns | 3.05 ns | 2.85 ns | 0.3986 |   1,256 B |
```

This does look very promising. As expected the `Baseline` parser does the best but `FParser` is not far behind. `FParsec` has very respectable performance but `FParser` manages to do a bit better. Now `FParsec` supports great error messages which is  currently not implemented in `FParser` but there are ways to support error messages without too much overhead.

Another nice aspect of `FParser` is that it doesn't add any extra memory overhead compared to the `Baseline` parser.

`PGO` uses a new feature in the runtime called [Dynamic Profile-Guided Optimization](https://gist.github.com/EgorBo/dc181796683da3d905a5295bfd3dd95b) which allows the jitter to improve code over time and does seem to give benefit to all parser variants over the standard approach.

## Conclusion

My F# Advent post was quite amibitious and took a long time to write. This time I have lower ambitions and skip over alot of details to give you the benchmark results.

There is much to investigate and improve but once again `[<InlineIfLambda>]` seems like its a powerful tool that allow us to write idiomatic functional programs with imperative performance.

Regards,

Mårten

