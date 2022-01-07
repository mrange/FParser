open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Running

open System
open System.Text

open CsBenchmark

type Expr =
  | Value     of int
  | Variable  of string
  | Binary    of char*(int->int->int)*Expr*Expr

  override x.ToString () =
    let sb = StringBuilder 16
    let rec traverse (sb : StringBuilder) pop e =
      match e with
      | Value     v             -> sb.Append v |> ignore
      | Variable  v             -> sb.Append v |> ignore
      | Binary (op, _, l, r) ->
        sb.Append '(' |> ignore
        traverse sb op l
        sb.Append op  |> ignore
        traverse sb op r
        sb.Append ')' |> ignore
    traverse sb '+' x
    sb.ToString ()

module Common =
  let resultToString r =
    match r with
    | Ok    ok  -> sprintf "OK : %s" <| string ok
    | Error err -> sprintf "ERR: %s" <| string err
open Common

module BaselineCalculator =
  open System.Globalization
  type ParserState(input : string) =
    class
      let mutable pos = 0

      let consumeWhiteSpace () =
        let inline isWhiteSpace ch = (ch >= '\u0009' && ch <= '\u000D') || (ch = '\u0020')
        while pos < input.Length && isWhiteSpace (input.[pos]) do
          pos <- pos + 1

      let skipChar c =
        if pos < input.Length && input.[pos] = c then
          pos <- pos + 1
          consumeWhiteSpace ()
          true
        else
          false

      let oops () = failwith "Programming error"
      member x.TryParseIntExpr (e : Expr outref) =
        let ppos = pos
        let inline isDigit ch = ch >= '0' && ch <= '9'

        while pos < input.Length && isDigit (input.[pos]) do
          pos <- pos + 1

        if ppos < pos then
          let v = Int32.Parse (input.AsSpan (ppos, pos - ppos), NumberStyles.Integer, CultureInfo.InvariantCulture)
          e <- Value v
          consumeWhiteSpace ()
          true
        else
          pos <- ppos
          false

      member x.TryParseVariableExpr (e : Expr outref) =
        let ppos = pos
        let inline isLetter ch = (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z')

        while pos < input.Length && isLetter (input.[pos]) do
          pos <- pos + 1

        if ppos < pos then
          let v = input.Substring(ppos, pos - ppos)
          e <- Variable v
          consumeWhiteSpace ()
          true
        else
          pos <- ppos
          false

      member x.TryParseGroupExpr (e : Expr outref) =
        let ppos = pos

        if skipChar '(' then
          if x.TryParseOp1Expr &e then
            if skipChar ')' then
              true
            else
              pos <- ppos
              false
          else
            pos <- ppos
            false

        else
          pos <- ppos
          false

      member x.TryParseTermExpr (e : Expr outref) =
        let ppos = pos

        if x.TryParseIntExpr &e then
          true
        else
          pos <- ppos
          if x.TryParseVariableExpr &e then
            true
          else
            pos <- ppos
            if x.TryParseGroupExpr &e then
              true
            else
              pos <- ppos
              false

      member x.TryParseOp0Expr (e : Expr outref) =
        let ppos          = pos

        let mutable agg   = Unchecked.defaultof<_>
        let zero          = char 0

        if x.TryParseTermExpr &agg then
          let mutable state = 0
          while state = 0 do
            let op =
              if   skipChar '*' then '*'
              elif skipChar '/' then '/'
              else zero
            if op > zero then
              let mutable next = Unchecked.defaultof<_>
              if x.TryParseTermExpr &next then
                agg <- 
                  match op with
                  | '*' -> Binary ('*', ( * ), agg, next)
                  | '/' -> Binary ('/', ( * ), agg, next)
                  | _   -> oops ()
              else
                // Bad
                state <- 2 
            else
              // Good
              state <- 1
          match state with
          | 1 -> e <- agg   ; true
          | 2 -> pos <- ppos; false
          | _ -> oops ()
        else
          pos <- ppos
          false

      member x.TryParseOp1Expr (e : Expr outref) =
        let ppos          = pos

        let mutable agg   = Unchecked.defaultof<_>
        let zero          = char 0

        if x.TryParseOp0Expr &agg then
          let mutable state = 0
          while state = 0 do
            let op =
              if   skipChar '+' then '+'
              elif skipChar '-' then '-'
              else zero
            if op > zero then
              let mutable next = Unchecked.defaultof<_>
              if x.TryParseOp0Expr &next then
                agg <- 
                  match op with
                  | '+' -> Binary ('+', ( + ), agg, next)
                  | '-' -> Binary ('-', ( - ), agg, next)
                  | _   -> oops ()
              else
                // Bad
                state <- 2 
            else
              // Good
              state <- 1
          match state with
          | 1 -> e <- agg   ; true
          | 2 -> pos <- ppos; false
          | _ -> oops ()
        else
          pos <- ppos
          false

      member x.TryParseFullExpr (e : Expr outref) =
        let ppos = pos

        consumeWhiteSpace ()
        if x.TryParseOp1Expr &e then
          // Eof?
          pos >= input.Length
        else
          pos <- ppos
          false

      member x.Parse () =
        pos <- 0
        let mutable e = Unchecked.defaultof<_>
        if x.TryParseFullExpr &e then
          Ok e
        else
          Error pos
    end

  let parse input =
    let p = ParserState input
    p.Parse ()

  let test () =
    let testCases =
      [|
        "1"
        "123"
        "(123+2)*x"
        "(123+2)*5"
        "x"
        "force*mass/(r*r)-G*MASS*mass/(r*r)"
      |]

    printfn "Baseline calculator"
    for testCase in testCases do
      let result = resultToString <| parse testCase
      printfn "  %s -> %A" testCase result

module FParserCalculator =
  open FParser.Core
  open FParser

  let inline ptoken label v = 
    pskipChar label v 
    >>. pskipWhitespace ()

  let inline pop exp ([<InlineIfLambda>] t) ([<InlineIfLambda>] m) = 
    pcharSat exp t |>> m .>> pskipWhitespace ()

  let inline op0 ch =
    match ch with
    | '*' -> fun l r -> Binary ('*', ( * ), l, r)
    | '/' -> fun l r -> Binary ('/', ( / ), l, r)
    | _   -> failwith "Expected * or /"
  let inline op1 ch =
    match ch with
    | '+' -> fun l r -> Binary ('+', ( + ), l, r)
    | '-' -> fun l r -> Binary ('-', ( - ), l, r)
    | _   -> failwith "Expected + or -"

  type ParserState() =
    class
      let struct (pexpr, sexpr) = pfwd<Expr> ()

      let pterm : Expr FParser = 
            (pint () |>> Value .>> pskipWhitespace ())
        <|> (pstringSat1 "variable" (fun ch p -> (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z')) |>> Variable .>> pskipWhitespace ())
        <|> (ptoken "(" '(' >>. pexpr .>> ptoken ")" ')')

      let pop0 : Expr FParser =
        pterm >+> pchainLeft1 (pop "*/" (fun ch -> ch = '*' || ch ='/') (fun ch -> op0 ch))
      let pop1 : Expr FParser =
        pop0 >+> pchainLeft1 (pop "+-" (fun ch -> ch = '+' || ch ='-') (fun ch -> op1 ch))
      do
        sexpr pop1
      let pfull = pskipWhitespace () >>. pop1 .>> peof ()

      member x.Parse s = prun pfull s
    end

  let test () =
    let p = ParserState()
    let testCases =
      [|
        "1"
        "123"
        "(123+2)*x"
        "(123+2)*5"
        "x"
        "force*mass/(r*r)-G*MASS*mass/(r*r)"
      |]

    printfn "FParser calculator"
    for testCase in testCases do
      let result = resultToString <| p.Parse testCase
      printfn "  %s -> %A" testCase result

module FParsecCalculator =
  open FParsec

  let inline ptoken v = 
    skipChar v
    >>. spaces

  type ParserState() =
    class
      let pexpr, rpexpr = createParserForwardedToRef ()

      let pterm = 
            (pint32 |>> Value .>> spaces)
        <|> (many1SatisfyL (fun ch -> (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z')) "variable" |>> Variable)
        <|> (between (ptoken '(') (ptoken ')') pexpr)

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
      let pop0 =
        chainl1 pterm (satisfyL (fun ch -> ch = '*' || ch ='/') "*/" |>> op0)
      let pop1 =
        chainl1 pop0 (satisfyL (fun ch -> ch = '+' || ch ='-') "+-" |>> op1)
      do
        rpexpr := pop1
      let pfull : Parser<Expr, unit> = spaces >>. pop1 .>> eof

      member x.Parse s =  run pfull s
    end

  let test () =
    let p = ParserState ()
    let testCases =
      [|
        "1"
        "123"
        "(123+2)*x"
        "(123+2)*5"
        "x"
        "force*mass/(r*r)-G*MASS*mass/(r*r)"
      |]

    printfn "FParsec calculator"
    for testCase in testCases do
      let result = 
        match p.Parse testCase with
        | Success (v, _, _) -> sprintf "OK : %s" <| string v
        | Failure (s, _, _) -> sprintf "ERR: %s" <| string s
      printfn "  %s -> %A" testCase result

[<Config(typeof<BenchmarkConfig>)>]
[<MemoryDiagnoser>]
//[<RyuJitX64Job>]
//[<RyuJitX86Job>]
type ParserBenchmark() =
  class
    let basicExpression   = "(123+2)*x"
    let complexExpression = "force*mass/(r*r)-G*MASS*mass/(r*r)"

    let fparsec   = FParsecCalculator.ParserState  ()
    let fparser   = FParserCalculator.ParserState ()

    [<Benchmark>]
    member x.Baseline_BasicExpression() =
      BaselineCalculator.parse basicExpression

    [<Benchmark>]
    member x.FParser_BasicExpression() =
      fparser.Parse basicExpression

    [<Benchmark>]
    member x.FParsec_BasicExpression() =
      fparsec.Parse basicExpression

    [<Benchmark>]
    member x.Baseline_ComplexExpression() =
      BaselineCalculator.parse complexExpression

    [<Benchmark>]
    member x.FParser_ComplexExpression() =
      fparser.Parse complexExpression

    [<Benchmark>]
    member x.FParsec_ComplexExpression() =
      fparsec.Parse complexExpression

  end

#if DEBUG
BaselineCalculator.test ()
FParserCalculator.test ()
FParsecCalculator.test ()
#else
BenchmarkRunner.Run<ParserBenchmark>() |> ignore
#endif
