namespace FParser.Core

type FParserContext(input : string) =
  class
    [<DefaultValue>] val mutable Pos : int

    member x.Input = input
    member x.End   = input.Length

    member inline x.Scan ([<InlineIfLambda>] sat : char -> int -> bool) i : int =
      let input     = x.Input
      let mutable c = i
      while (c < input.Length && if sat input.[c] c then c <- c + 1; true else false) do ()
      c

    member inline x.FlipDecision () = x.Pos <- ~~~x.Pos

    member inline x.IsGood () = x.Pos >= 0

    member inline x.IsBad () = x.Pos < 0

    member inline x.SuccessAt v i =
      x.Pos <- i
      v

    member inline x.Expected (label : string) =
      Unchecked.defaultof<_>

    member inline x.ExpectedAt (label : string) i =
      x.Pos <- ~~~i
      Unchecked.defaultof<_>
  end

type 'T FParser = FParserContext -> 'T

module FParser =
  open System
  open System.Globalization

  module Details =
    let inline success v  = v
    let inline failure () = Unchecked.defaultof<_>

    module Deduplicator =
      let pint (c : FParserContext) i =
        let e = c.Scan (fun ch p -> ch >= '0' && ch <= '9') i
        if i < e then
          let v = Int32.Parse (c.Input.AsSpan (i, e-i), NumberStyles.Integer, CultureInfo.InvariantCulture)
          c.SuccessAt v e
        else
          c.ExpectedAt "int" i

      let pskipWhitespace (c : FParserContext) i =
        let e = c.Scan (fun ch p -> (ch >= '\u0009' && ch <= '\u000D') || (ch = '\u0020')) i
        c.SuccessAt () e

  open Details

  let inline (>+>) ([<InlineIfLambda>] v : _ -> _) ([<InlineIfLambda>] f : _ -> _) =
    f v

  let inline prun ([<InlineIfLambda>] pf : _ FParser) input =
    let c = FParserContext input
    let v = pf c
    if c.IsGood () then
      Ok v
    else
      Error (~~~c.Pos)

  let inline preturn (v : 'T) : 'T FParser =
    fun c ->
      success v

  let inline pexpected label : _ FParser =
    fun c ->
      c.Expected label

  let inline peof () : unit FParser =
    fun c ->
      if c.Pos >= c.End then
        c.SuccessAt () c.End
      else
        c.ExpectedAt "eof" c.End

  let inline pchainLeft1 ([<InlineIfLambda>] psep : _ FParser) ([<InlineIfLambda>] pp : _ FParser) : _ FParser = 
    fun c ->
      let fv = pp c
      if c.IsGood () then
        let mutable res = fv
        while 
          (
            let sv = psep c
            if c.IsGood () then
              let rv = pp c
              if c.IsGood () then
                res <- sv res rv
                true
              else
                // Failed to parse, expression incomplete
                res <- failure ()
                false
            else
              // Failed to parse separator, all is good
              c.FlipDecision ()
              false
          ) do ()
        res
      else
        failure ()

  let inline (<*>) ([<InlineIfLambda>] pf : _ FParser) ([<InlineIfLambda>] pv : _ FParser) : _ FParser = 
    fun c ->
      let fv = pf c
      if c.IsGood () then
        let vv = pv c
        if c.IsGood () then
          success (fv vv)
        else
          failure ()
      else
        failure ()

  let inline (.>>) ([<InlineIfLambda>] pf : _ FParser) ([<InlineIfLambda>] ps : _ FParser) : _ FParser = 
    fun c ->
      let fv = pf c
      if c.IsGood () then
        let _ = ps c
        if c.IsGood () then
          success fv
        else
          failure ()
      else
        failure ()

  let inline (>>.) ([<InlineIfLambda>] pf : _ FParser) ([<InlineIfLambda>] ps : _ FParser) : _ FParser = 
    fun c ->
      let _ = pf c
      if c.IsGood () then
        let sv = ps c
        if c.IsGood () then
          success sv
        else
          failure ()
      else
        failure ()

  let inline (.>>.) ([<InlineIfLambda>] pf : _ FParser) ([<InlineIfLambda>] ps : _ FParser) : _ FParser = 
    fun c ->
      let fv = pf c
      if c.IsGood () then
        let sv = ps c
        if c.IsGood () then
          success (fv, sv)
        else
          failure ()
      else
        failure ()


  let inline (<|>) ([<InlineIfLambda>] pf : _ FParser) ([<InlineIfLambda>] ps : _ FParser) : _ FParser = 
    fun c ->
      let spos = c.Pos
      let fv = pf c
      if c.IsGood () then
        success fv
      else
        c.Pos <- spos
        ps c

  let inline (|>>) ([<InlineIfLambda>] pp : _ FParser) ([<InlineIfLambda>] m : _ -> _) : _ FParser = 
    fun c ->
      let pv = pp c
      if c.IsGood () then
        success (m pv)
      else
        failure ()

  let inline pfwd<'T> () : struct ('T FParser*('T FParser -> unit)) = 
    let mutable shared = pexpected "pfwd"
    let p c   = shared c
    let s pp  = shared <- pp
    p, s

  let inline pdebug nm ([<InlineIfLambda>] pf : _ FParser) : _ FParser = 
    fun c ->
      let fv = pf c
      if c.IsGood () then
        printfn "GOOD - %s - %i - %A" nm c.Pos fv
        success fv
      else
        printfn "FAIL - %s - %i" nm c.Pos
        failure ()

  let inline pint () : int FParser = 
    fun c ->
      Deduplicator.pint c c.Pos

  let inline pchar () : char FParser = 
    fun c ->
      let i = c.Pos
      if i < c.End then
        c.SuccessAt c.Input.[i] (i + 1)
      else
        c.ExpectedAt "char" i

  let inline pcharSat label ([<InlineIfLambda>] sat) : char FParser = 
    fun c ->
      let i = c.Pos
      if i < c.End && sat (c.Input.[i]) then
        c.SuccessAt c.Input.[i] (i + 1)
      else
        c.ExpectedAt label i

  let inline pskipChar label v : unit FParser = 
    fun c ->
      let i = c.Pos
      if i < c.End && v = c.Input.[i] then
        c.SuccessAt () (i + 1)
      else
        c.ExpectedAt label i

  let inline pstringSat1 label ([<InlineIfLambda>] sat) : string FParser = 
    fun c ->
      let i = c.Pos
      let e = c.Scan sat i
      if i < e then
        c.SuccessAt (c.Input.Substring(i, e - i)) e
      else
        c.ExpectedAt label i

  let inline pskipWhitespace () : unit FParser = 
    fun c ->
      Deduplicator.pskipWhitespace c c.Pos

