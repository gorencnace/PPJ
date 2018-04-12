(* definicija tokova = neskoncni seznam *)
(* val s = Cons(1, Cons(2, Cons(...))) *)
datatype 'a stream = Cons of 'a * (unit -> 'a stream)

(* Prvih n elementov toka pretvori v seznam *)
(* pomozni tok → s() *)
fun to_list 0 _ = []
  | to_list n (Cons(x, s)) = x :: (to_list (n-1) (s()))

(* n-ti element toka *)
fun elementAt 0 (Cons (x, _)) = x
  | elementAt n (Cons (_, s)) = elementAt (n-1) (s ())

(* prvi element (=glava) toka *)
(* α stream → α *)
fun head (Cons (x, xs)) = x

(* rep toka (= vse, razen glave) *)
(* α stream → α stream *)
fun tail (Cons(x,xs)) = xs()

(* seznam → tok *)
(* najprej bo v toku seznam, potem pa ponavljajoci element r *)
fun from_list [] r = Cons(r, fn() => from_list [] r)
	| from_list (x::xs) r = Cons(x, fn() => from_list xs r)

(* preslikava elementov toka *)
(* glavni tip: α stream → α stream *)
fun map f (Cons (x, xs)) = Cons(f x, fn() => map f (xs()))


(* tok naravnih stevil (z niclo) *)
val nat =
  let fun from k = Cons(k, fn () => from (k+1))
  in from 0
  end

(* tok Fibonaccijevih stevil *)
val fib =
  let fun gen a b = Cons(a, fn () => gen b (a+b))
  in gen 0 1
  end

(* preslikava parov dveh tokov *)
(* zip : (α → β → γ) → α stream → β stream → γ stream *)
fun zip f (Cons(x, xs)) (Cons(y, ys)) =
  Cons(f x y, fn () => zip f (xs()) (ys()))

(* vrne tok veckratnikov k-ja *)
fun veckratniki_stevila k =
  let fun mn a b = Cons(b * a, fn () => mn (a+1) b)
  in mn 0 k
  end

(* tok tokov veckratniki : int stream stream *)
(* (0, 0, 0, 0, ⋯), (0, 1, 2, 3, ⋯), (0, 2, 4, 6, ⋯), (0, 3, 6, 9, ⋯), ⋯  *)
val veckratniki = 
  let fun f a =
    Cons(veckratniki_stevila a, fn () => tok (a+1))
  in f 0
  end
