// ------------------------------------------------------------------
// Monoids
// ------------------------------------------------------------------

type LazyList<'T> = LL of (unit -> LazyListRes<'T>)
and LazyListRes<'T> = Nil | Cons of 'T * LazyList<'T>

type LazyListBuileder() = 
  member x.Delay(f) = LL (fun () -> let (LL r) = f() in r())
  member x.Run(LL f) = LL f
  member x.Yield(v) = LL (fun () -> Cons(v, LL (fun () -> Nil)))
  member x.Combine(LL a, LL b) = LL (fun () ->
    match a() with
    | Nil -> b()
    | Cons(v, rest) -> Cons(v, x.Combine(rest, LL b)))

  member x.Zero() = LL (fun () -> Nil)
  member x.For(inp, body) = 
    match inp with 
    | v::vs -> x.Combine(body v, x.For(vs, body))
    | [] -> x.Zero()
  member x.While(cond, body) =
    if cond() then x.Combine(body, x.Delay(fun () -> x.While(cond, body)))
    else x.Zero()

let llist = LazyListBuileder()

let rec print n (LL v) =
  if n > 0 then 
    match v() with
    | Nil -> ()
    | Cons(v, rest) -> printfn "%A" v; print (n-1) rest

llist { yield 1
        printfn "calculating"
        yield 2 }
|> print 2

// ------------------------------------------------------------------

type IntMonoid = IM of int

type AdditionBuilder() = 
  member x.Delay(f) = f()
  member x.Combine(IM a, IM b) = IM (a + b)
  member x.Zero() = IM 0
  member x.Yield(v) = IM v
  member x.YieldFrom(IM v) = IM v
  member x.For(inp, body) = 
    match inp with 
    | v::vs -> x.Combine(body v, x.For(vs, body))
    | [] -> x.Zero()

let add = AdditionBuilder()

add { yield 1
      yield 2 }

let rec addTo f t = 
  add { if f <= t then
          yield f
          yield! addTo (f + 1) t }

addTo 1 10

add { for i in [ 1 .. 10 ] do
        yield i }

// ------------------------------------------------------------------

type DisjMonoid = DM of bool

type DisjunctionBuilder() = 
  member x.Delay(f) = f
  member x.Run(f) = f()
  member x.Combine(DM a, f) = if a then DM true else f()
  member x.Zero() = DM false 
  member x.Yield(v) = DM v
  member x.YieldFrom(DM v) = DM v
  member x.For(inp, body) = 
    match inp with 
    | v::vs -> x.Combine(body v, fun () -> x.For(vs, body))
    | [] -> x.Zero()

let disj = DisjunctionBuilder()

disj { yield true
       printfn "calculating"
       yield false }

// ------------------------------------------------------------------
// Monads
// ------------------------------------------------------------------

type Maybe<'T> = Just of 'T | Nothing

type MaybeBuilder() = 
  member x.Return(v) = Just v
  member x.ReturnFrom(c) = c
  member x.Bind(m, f) = match m with Just v -> f v | _ -> Nothing
  member x.Zero() = Just ()
  member x.Combine(a:Maybe<unit>, f) = x.Bind(a, f)
  member x.Delay(f) = f
  member x.Run(f) = f()
  member x.TryWith(body, handler) = try body() with e -> handler e

let maybe = MaybeBuilder()

//let divide a b = if b = 0 then Nothing else Just (a / b)

let divideE a b =
  maybe { try 
            return a / b
          with e ->
            return! Nothing }

let divide a b =
  maybe { if b = 0 then return! Nothing
          return a / b }

divide 4 2


maybe { let! a = divide 10 0
        let! b = divide 0 10
        if a > 0 then return 10
        else return a + b }

async { return ()
        return false }

//type MaybeBuilder with
//  member x.


type SeqBuilder() =   
  member x.Combine(a, b) = seq { yield! a; yield! b }
  member x.Zero() = Seq.empty
  member x.For(inp, body) = 
    match inp with 
    | v::vs -> x.Combine(body v, x.For(vs, body))
    | [] -> x.Zero()
  member x.Yield(v) = seq { yield v }

let seq = SeqBuilder()

seq { for i in [1;2;3] do
        for j in [10;100] do
          yield i * j }
