open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Running

open System
open System.Text

open CsBenchmark
open CsParser

type Expr =
  | Value     of int
  | Variable  of ReadOnlyMemory<char>
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

  static member Cons (s:string) = Variable (s.AsMemory())

module Common =
  let resultToString r =
    match r with
    | Ok    ok  -> sprintf "OK : %s" <| string ok
    | Error err -> sprintf "ERR: %s" <| string err
open Common

module BaselineCalculator =
  open System.Globalization

  let oops () = failwith "Programming error"

  let consumeWhiteSpace (input : string) (current : int byref) =
    let inline isWhiteSpace ch = (ch >= '\u0009' && ch <= '\u000D') || (ch = '\u0020')
    let mutable pos = current
    while pos < input.Length && isWhiteSpace (input.[pos]) do
      pos <- pos + 1
    current <- pos

  let trySkipChar (input : string) (current : int byref) c =
    let mutable pos = current
    if pos < input.Length && input.[pos] = c then
      pos     <- pos + 1
      current <- pos
      consumeWhiteSpace input &current
      true
    else
      false

  let tryParseIntExpr (input : string) (current : int byref) (e : Expr outref) =
    let inline isDigit ch = ch >= '0' && ch <= '9'
    let mutable pos = current

    while pos < input.Length && isDigit (input.[pos]) do
      pos <- pos + 1

    if current < pos then
      let v = Int32.Parse (input.AsSpan (current, pos - current), NumberStyles.Integer, CultureInfo.InvariantCulture)
      e       <- Value v
      current <- pos
      consumeWhiteSpace input &current
      true
    else
      false

  let tryParseVariableExpr (input : string) (current : int byref) (e : Expr outref) =
    let inline isLetter ch = (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z')
    let mutable pos = current

    while pos < input.Length && isLetter (input.[pos]) do
      pos <- pos + 1

    if current < pos then
      let v = input.AsMemory(current, pos - current)
      e       <- Variable v
      current <- pos
      consumeWhiteSpace input &current
      true
    else
      false

  let rec tryParseGroupExpr (input : string) (current : int byref) (e : Expr outref) =
    if trySkipChar input &current '(' then
      if tryParseOp1Expr input &current &e then
        if trySkipChar input &current ')' then
          true
        else
          false
      else
        false

    else
      false

  and tryParseTermExpr (input : string) (current : int byref) (e : Expr outref) =
    let spos = current
    if tryParseIntExpr input &current &e then
      true
    else
      current <- spos
      if tryParseVariableExpr input &current &e then
        true
      else
        current <- spos
        if tryParseGroupExpr input &current &e then
          true
        else
          false

  and tryParseOp0Expr (input : string) (current : int byref) (e : Expr outref) =
    let mutable agg   = Unchecked.defaultof<_>
    let zero          = char 0

    if tryParseTermExpr input &current &agg then
      let mutable state = 0
      while state = 0 do
        let op =
          if   trySkipChar input &current '*' then '*'
          elif trySkipChar input &current '/' then '/'
          else zero
        if op > zero then
          let mutable next = Unchecked.defaultof<_>
          if tryParseTermExpr input &current &next then
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
      | 1 -> e <- agg; true
      | 2 -> false
      | _ -> oops ()
    else
      false

  and tryParseOp1Expr (input : string) (current : int byref) (e : Expr outref) =
    let mutable agg   = Unchecked.defaultof<_>
    let zero          = char 0

    if tryParseOp0Expr input &current &agg then
      let mutable state = 0
      while state = 0 do
        let op =
          if   trySkipChar input &current '+' then '+'
          elif trySkipChar input &current '-' then '-'
          else zero
        if op > zero then
          let mutable next = Unchecked.defaultof<_>
          if tryParseOp0Expr input &current &next then
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
      | 2 -> false
      | _ -> oops ()
    else
      false

  let tryParseFullExpr (input : string) (current : int byref) (e : Expr outref) =

    consumeWhiteSpace input &current
    if tryParseOp1Expr input &current &e then
      // Eof?
      current >= input.Length
    else
      false

  let parse input =
    let mutable current = 0
    let mutable e       = Unchecked.defaultof<_>
    if tryParseFullExpr input &current &e then
      Ok e
    else
      Error current

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
      printfn "  %s -> %s" testCase result

module FParserCalculator =
  open FParser6
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

  type ParserConfig () =
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
    let p = ParserConfig ()
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
      printfn "  %s -> %s" testCase result

module FParsecCalculator =
  open FParsec

  let inline ptoken v = 
    skipChar v
    >>. spaces

  type ParserConfig () =
    class
      let pexpr, rpexpr = createParserForwardedToRef ()

      let pterm = 
            (pint32 |>> Value .>> spaces)
        <|> (many1SatisfyL (fun ch -> (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z')) "variable" |>> Expr.Cons)
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
    let p = ParserConfig ()
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
      printfn "  %s -> %s" testCase result

[<Config(typeof<BenchmarkConfig>)>]
[<MemoryDiagnoser>]
//[<RyuJitX64Job>]
//[<RyuJitX86Job>]
type ParserBenchmark() =
  class
    let basicExpression   = "(123+2)*x"
    let complexExpression = "force*mass/(r*r)-G*MASS*mass/(r*r)"

    let fparsec   = FParsecCalculator.ParserConfig ()
    let fparser   = FParserCalculator.ParserConfig ()
    let csparser  = new CsParserCalculator ()

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
    member x.CsParser_BasicExpression() =
      csparser.Parse basicExpression

    [<Benchmark>]
    member x.Baseline_ComplexExpression() =
      BaselineCalculator.parse complexExpression

    [<Benchmark>]
    member x.FParser_ComplexExpression() =
      fparser.Parse complexExpression

    [<Benchmark>]
    member x.FParsec_ComplexExpression() =
      fparsec.Parse complexExpression

    [<Benchmark>]
    member x.CsParser_ComplexExpression() =
      csparser.Parse complexExpression

  end

#if DEBUG
BaselineCalculator.test ()
FParserCalculator.test ()
FParsecCalculator.test ()
CsParserCalculator.Test()
#else
BenchmarkRunner.Run<ParserBenchmark>() |> ignore
#endif
