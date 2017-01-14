(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Andree Kaba, Id Number: 260493293 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code MUST compile and must NOT go into infinite
loops.  An assignment like that means you have not tested it.  You will get
ZERO FOR THE ENTIRE ASSIGMENT even if the problem is only with one
question.  If you are not able to get the code to compile and run do not
submit it.  *)

(* module hw1_sol.  Use this if you want to load the file into an interactive session.*)

(* Question 1 *) (* Do not edit this line. *)

let rec sumlist l =
	match l with
	| [] -> 0.0
	| x::xs -> x + (sumlist xs)

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::(pairlists(xs,ys))

let w_mean weights data = failwith "TODO"
  
(* Question 2. *)  (* Do not edit this line. *)

let rec memberof pair =
	match pair with
	| (e,[]) -> false
	| (e,x::xs)-> 
		if e = x then true
		else memberof (e,xs)


let rec remove(item, lst) =
	match lst with
	| [] -> []
	| x::xs -> 
		if item = x then remove(item,xs)
		else x::remove(item,xs)


(* Question 3. *)  (* Do not edit this line. *)

let findMax l = 
  let rec helper(l,m) = failwith "Not implemented"
	match l with
	| [] -> m
	| x::xs -> 
		if m > x then helper(xs,m)
		else helper(xs,x)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  
let rec selsort l = failwith "Not implemented"
  match l with
  | [] -> []
  | _ -> let m = (findMax l)
         m::(selsort(remove(m,l)))

(* Question 5. *)  (* Do not edit this line. *)

let rec common twolists =
	match twolists with
	| ([],l) -> []
	| (x::xs,l) ->
		if memberof (x,l) then x::common(xs,l)
		else common(xs,l)

(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

let rec split l =
	match l with
	| [x] -> [x]
	| _ -> 
		let n=l.Length
		(arr.[0..(n/2)-1],arr.[(n/2)..n-1])
	

let rec merge twolists =
	match twolists with
	| ([],[]) -> []
	| ([],y::ys) -> ys
	| (x::xs,[]) -> xs
	| (x::xs,y::ys) -> 
		if x < y then x::merge(xs,y::ys)
		else y::merge(x::xs,ys)

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
	  let p=split n::ns
	  merge(mergesort fst p,mergesort snd p) 

