# Parsers combinators with F#6 `[<InlineIfLambda>]`

_Thanks to [manofstick](https://gist.github.com/manofstick) peer reviewing the blog post._


For [F# Advent 2021](https://sergeytihon.com/2021/10/18/f-advent-calendar-2021/) I wrote a [blog post](https://gist.github.com/mrange/fbefd946dba6725a0b727b7d3fd81d6f) exploring how F#6 `[<InlineIfLambda>]` can improve data pipeline performance.

I was thinking of other places where `[<InlineIfLambda>]` can help and decided to try to build a parser combinator library with `[<InlineIfLambda>]`.

## Parser combinators

[FParsec](http://www.quanttec.com/fparsec/) is a parser combinator library that allows you to create complex parsers by combining simple parsers. FParsec is a great library and I was amazed by it the first time I tried it.

Graham Hutton and Erik Meijer have written an excellent [introduction](https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf) to parser combinators. When I read this article many years ago it was the first time that I felt that I understood what Functional Programming is all about.

We create a parser combinator library by defining what is a parser and then define various functions that combine parsers.

A Parser could be this:

```fsharp
type 'T Parser = string -> ('T*string) option
```

This function takes a string, produces a value and the unconsumed string. If the parser fails it returns `None`.

It's possible to build a parser combinator library around this definition but it won't be efficient as we will constantly create substrings.

## FParser

My first attempt was this:

```fsharp
type 'T Parser = string -> int -> ('T*int) option
```

The parser takes a string and the current position and returns a value and the position of the first unconsumed char if successful.

While experimenting with performance I found that the overhead from creating tuples and options was significant, switching to value tuples and options reduced performance but avoided GC pressure.

In the end I ended up with this:

```fsharp
type FParserContext(input : string) =
  class
    [<DefaultValue>] val mutable Pos : int

    member x.Input = input
  end

type 'T FParser = FParserContext -> 'T
```

The `ParserContext` stores the string to be parsed and the current position. A parser takes a `ParserContext` and returns a value if successful and updates the position in the context. Upon failure, the parser returns the default value of `'T` and has set the position to a negative value.

Not elegant but it was the most performant option I found. There might be better ones.

In addition; there are a number of parser combinators defined and I won't go through all of them but I will show how `<|>` works which is the choice combinator.

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

The other combinators are implemented in similar style.

With the parser combinator library one can implement a simple math expression parser

```fsharp
// Expression tree for math expressions like: x*(2+123*y)
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
  type ParserConfig () =
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
        // pop1 is the complete the forward declared pexpr
        //  set pexpr to pop1
        sexpr pop1
      // The full grammar
      let pfull = pskipWhitespace () >>. pop1 .>> peof ()

      member x.Parse s = prun pfull s
    end
```

I also implemented a math expression parser using `FParsec` and painstakingly implemented a `Baseline` math expression parser in imperative style.

## Benchmark results

Using `Benchmark.net` I compared the performance of `FParser`, `FParsec` and `Baseline`. As a late bonus I aslo implemented a calculator expression parser in `C#` using a delegate based parser combinator library.

```
BenchmarkDotNet=v0.13.1, OS=Windows 10.0.19044.1415 (21H2)
Intel Core i5-3570K CPU 3.40GHz (Ivy Bridge), 1 CPU, 4 logical and 4 physical cores
.NET SDK=6.0.101
  [Host] : .NET 6.0.1 (6.0.121.56705), X64 RyuJIT DEBUG
  PGO    : .NET 6.0.1 (6.0.121.56705), X64 RyuJIT
  STD    : .NET 6.0.1 (6.0.121.56705), X64 RyuJIT


|                     Method | Job |       Mean |    Error |   StdDev |  Gen 0 | Allocated |
|--------------------------- |---- |-----------:|---------:|---------:|-------:|----------:|
|   Baseline_BasicExpression | STD |   197.9 ns |  1.63 ns |  1.44 ns | 0.0610 |     192 B |
|    FParser_BasicExpression | STD |   245.2 ns |  1.40 ns |  1.31 ns | 0.0710 |     224 B |
|    FParsec_BasicExpression | STD |   734.3 ns |  4.70 ns |  4.17 ns | 0.1631 |     512 B |
|   CsParser_BasicExpression | STD |   514.5 ns |  3.54 ns |  3.31 ns | 0.0610 |     192 B |
| Baseline_ComplexExpression | STD |   579.4 ns |  2.69 ns |  2.38 ns | 0.2699 |     848 B |
|  FParser_ComplexExpression | STD |   721.9 ns | 10.04 ns |  9.39 ns | 0.2804 |     880 B |
|  FParsec_ComplexExpression | STD | 1,731.1 ns |  7.95 ns |  6.64 ns | 0.3986 |   1,256 B |
| CsParser_ComplexExpression | STD | 1,375.4 ns |  6.31 ns |  5.91 ns | 0.2689 |     848 B |
|   Baseline_BasicExpression | PGO |   171.5 ns |  1.68 ns |  1.57 ns | 0.0610 |     192 B |
|    FParser_BasicExpression | PGO |   222.5 ns |  2.39 ns |  2.23 ns | 0.0713 |     224 B |
|    FParsec_BasicExpression | PGO |   617.4 ns |  5.82 ns |  5.45 ns | 0.1631 |     512 B |
|   CsParser_BasicExpression | PGO |   425.6 ns |  6.12 ns |  5.72 ns | 0.0610 |     192 B |
| Baseline_ComplexExpression | PGO |   449.1 ns |  2.06 ns |  1.93 ns | 0.2699 |     848 B |
|  FParser_ComplexExpression | PGO |   615.8 ns |  8.99 ns |  8.41 ns | 0.2804 |     880 B |
|  FParsec_ComplexExpression | PGO | 1,564.1 ns | 14.88 ns | 13.92 ns | 0.3986 |   1,256 B |
| CsParser_ComplexExpression | PGO | 1,228.4 ns |  6.07 ns |  5.68 ns | 0.2689 |     848 B |
```

This does look very promising. As expected the `Baseline` parser does the best but `FParser` is not far behind. `FParsec` has very respectable performance but `FParser` manages to do a bit better. Now `FParsec` supports great error messages which is currently not implemented in `FParser` but there are ways to support error messages without too much overhead.

Another nice aspect of `FParser` is that it adds less memory overhead than `FParsec`, the overhead comes from creating `FParserContext`

`CsParser` falls between `FParser` and `FParsec` which is pretty good considering it's delegate based. `CsParser` also don't collect error information so comparing to `FParsec` is a bit unfair here as well.

`PGO` uses a new feature in the runtime called [Dynamic Profile-Guided Optimization](https://gist.github.com/EgorBo/dc181796683da3d905a5295bfd3dd95b) which allows the jitter to improve code over time and does seem to give benefit to all parser variants over the standard approach.

## Conclusion

There is much more to investigate and improve but once again `[<InlineIfLambda>]` seems like its a powerful tool that allows us to write functional style programs with close to imperative performance.

Regards,

Mårten
