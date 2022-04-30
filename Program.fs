// https://github.com/moocfi/haskell-mooc/blob/master/exercises/Set1.hs

open System

(*
  Ex 1: define variables one and two. They should have type Int and
  values 1 and 2, respectively.
*)

let one = 1
let two = 2

(*
  Ex 2: define the function double of type Integer->Integer. Double
  should take one argument and return it multiplied by two.
*)

let double = (*) 2

(*
  Ex 3: define the function quadruple that uses the function double
  from the previous exercise to return its argument multiplied by
  four.
*)

let quadruple = double >> double

(*
  Ex 4: define the function distance. It should take four arguments of
  type Double: x1, y1, x2, and y2 and return the (euclidean) distance
  between points (x1,y1) and (x2,y2).

  Give distance a type signature, i.e. distance :: something.

  PS. if you can't remember how the distance is computed, the formula is:
    square root of ((x distance) squared + (y distance) squared)

  Examples:
    distance 0 0 1 1  ==>  1.4142135...
    distance 1 1 4 5  ==>  5.0
*)

let distance (x1: float) (y1: float) (x2: float) (y2: float) =
    let square n = Math.Pow(n, 2.0)
    let squareRoot n = Math.Sqrt(n)

    (x2 - x1 |> square)
    |> (+) (y2 - y1 |> square)
    |> squareRoot

(*
  Ex 5: define the function eeny that returns "eeny" for even inputs
  and "meeny" for odd inputs.

  Ps. have a look at the built in function "even"
*)

let (|Even|Odd|) n = if n % 2 = 0 then Even else Odd

let eeny =
    function
    | Even -> "eeny"
    | Odd -> "meeny"

(*
  Ex 6: here's the function checkPassword from the course material.
  Modify it so that it accepts two passwords, "swordfish" and
  "mellon".
*)

let checkPassword =
    function
    | "swordfish"
    | "mellon" -> "You're in."
    | _ -> "ACCESS DENIED!"

(*
  Ex 7: A postal service prices packages the following way.
  Packages that weigh up to 500 grams cost 250 credits.
  Packages over 500 grams cost 300 credit + 1 credit per gram.
  Packages over 5000 grams cost a constant 6000 credits.

  Write a function postagePrice that takes the weight of a package
  in grams, and returns the cost in credits.
*)

let postagePrice w =
    if w <= 500 then 250
    else if w <= 5000 then 300 + (w - 500)
    else 6000

(*
  Ex 8: define a function isZero that returns True if it is given an
  Integer that is 0, and False otherwise. Give isZero a type signature.

  Use pattern matching! Don't use comparisons!

  Ps. remember, the type of booleans in haskell is Bool
*)

let isZero =
    function
    | 0 -> true
    | _ -> false
