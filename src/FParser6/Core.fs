namespace FParser6

type FParserContext(input : string) =
  class
    [<DefaultValue>] val mutable Pos : int

    member x.Input = input
  end

type 'T FParser = FParserContext -> 'T

module FParser =
  open System
  open System.Globalization

  module Details =
    let inline success v  = v
    let inline failure () = Unchecked.defaultof<_>

    let inline scan (c : FParserContext) ([<InlineIfLambda>] sat : char -> int -> bool) i : int =
      let input     = c.Input
      let mutable p = i
      while (p < input.Length && if sat input.[p] p then p <- p + 1; true else false) do ()
      p

    let inline flipDecision  (c : FParserContext)     = c.Pos <- ~~~c.Pos
    let inline isGood        (c : FParserContext)     = c.Pos >= 0
    let inline isBad         (c : FParserContext)     = c.Pos < 0
    let inline successAt     (c : FParserContext) v i = c.Pos <- i; v

    let inline expected (c : FParserContext) (label : string) =
      Unchecked.defaultof<_>

    let inline expectedAt (c : FParserContext) (label : string) i =
      c.Pos <- ~~~i
      Unchecked.defaultof<_>

    module Deduplicator =
      let pint (c : FParserContext) i =
        let e = scan c (fun ch p -> ch >= '0' && ch <= '9') i
        if i < e then
          let v = Int32.Parse (c.Input.AsSpan (i, e-i), NumberStyles.Integer, CultureInfo.InvariantCulture)
          successAt c v e
        else
          expectedAt c "int" i

      let pskipWhitespace (c : FParserContext) i =
        let e = scan c (fun ch p -> (ch >= '\u0009' && ch <= '\u000D') || (ch = '\u0020')) i
        successAt c () e
  open Details

  let inline (>+>) ([<InlineIfLambda>] v : _ -> _) ([<InlineIfLambda>] f : _ -> _) =
    f v

  let inline prun ([<InlineIfLambda>] pf : _ FParser) input =
    let c = FParserContext input
    let v = pf c
    if isGood c then
      Ok v
    else
      Error (~~~c.Pos)

  let inline preturn (v : 'T) : 'T FParser =
    fun c ->
      success v

  let inline pexpected label : _ FParser =
    fun c ->
      expected c label

  let inline peof () : unit FParser =
    fun c ->
      let l = c.Input.Length
      if c.Pos >= l then
        successAt c () l
      else
        expectedAt c "eof" l

  let inline pchainLeft1 ([<InlineIfLambda>] psep : _ FParser) ([<InlineIfLambda>] pp : _ FParser) : _ FParser = 
    fun c ->
      let fv = pp c
      if isGood c then
        let mutable res = fv
        while 
          (
            let sv = psep c
            if isGood c then
              let rv = pp c
              if isGood c then
                res <- sv res rv
                true
              else
                // Failed to parse, expression incomplete
                res <- failure ()
                false
            else
              // Failed to parse separator, all is good
              flipDecision c
              false
          ) do ()
        res
      else
        failure ()

  let inline (<*>) ([<InlineIfLambda>] pf : _ FParser) ([<InlineIfLambda>] pv : _ FParser) : _ FParser = 
    fun c ->
      let fv = pf c
      if isGood c then
        let vv = pv c
        if isGood c then
          success (fv vv)
        else
          failure ()
      else
        failure ()

  let inline (.>>) ([<InlineIfLambda>] pf : _ FParser) ([<InlineIfLambda>] ps : _ FParser) : _ FParser = 
    fun c ->
      let fv = pf c
      if isGood c then
        let _ = ps c
        if isGood c then
          success fv
        else
          failure ()
      else
        failure ()

  let inline (>>.) ([<InlineIfLambda>] pf : _ FParser) ([<InlineIfLambda>] ps : _ FParser) : _ FParser = 
    fun c ->
      let _ = pf c
      if isGood c then
        let sv = ps c
        if isGood c then
          success sv
        else
          failure ()
      else
        failure ()

  let inline (.>>.) ([<InlineIfLambda>] pf : _ FParser) ([<InlineIfLambda>] ps : _ FParser) : _ FParser = 
    fun c ->
      let fv = pf c
      if isGood c then
        let sv = ps c
        if isGood c then
          success (fv, sv)
        else
          failure ()
      else
        failure ()


  let inline (<|>) ([<InlineIfLambda>] pf : _ FParser) ([<InlineIfLambda>] ps : _ FParser) : _ FParser = 
    fun c ->
      let spos = c.Pos
      let fv = pf c
      if isGood c then
        success fv
      else
        c.Pos <- spos
        ps c

  let inline (|>>) ([<InlineIfLambda>] pp : _ FParser) ([<InlineIfLambda>] m : _ -> _) : _ FParser = 
    fun c ->
      let pv = pp c
      if isGood c then
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
      if isGood c then
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
      let input = c.Input
      let pos   = c.Pos
      if pos < input.Length then
        successAt c input.[pos] (pos + 1)
      else
        expectedAt c "char" pos

  let inline pcharSat label ([<InlineIfLambda>] sat) : char FParser = 
    fun c ->
      let input = c.Input
      let pos   = c.Pos
      if pos < input.Length && sat (input.[pos]) then
        successAt c input.[pos] (pos + 1)
      else
        expectedAt c label pos

  let inline pskipChar label v : unit FParser = 
    fun c ->
      let input = c.Input
      let pos   = c.Pos
      if pos < input.Length && v = input.[pos] then
        successAt c () (pos + 1)
      else
        expectedAt c label pos

  let inline pstringSat1 label ([<InlineIfLambda>] sat) : string FParser = 
    fun c ->
      let i = c.Pos
      let e = scan c sat i
      if i < e then
        successAt c (c.Input.Substring(i, e - i)) e
      else
        expectedAt c label i

  let inline pskipWhitespace () : unit FParser = 
    fun c ->
      Deduplicator.pskipWhitespace c c.Pos

