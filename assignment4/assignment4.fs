(* Assignment 4 *) (* Do not edit this line. *)
(* Student name: Andree Kaba, Id Number: 260493293 *) (* Edit this line. *)

module Hw4
type id = string

type term =
  | Var of id
  | Const of int 
  | Term of id * term list
  
(* invariant for substitutions: *)
(* no id on a lhs occurs in any term earlier in the list *)
type substitution = (id * term) list

(* check if a variable occurs in a term *)
let rec occurs (x : id) (t : term) : bool =
    match t with
    | Var v -> (v=x)
    | Const c -> false
    | Term (_,args) -> (List.fold (fun acc b -> acc || b) false [ for t in args do yield (occurs x t) ])

(* substitute term s for all occurrences of variable x in term t *)
let rec subst (s : term) (x : id) (t : term) : term =
    match t with
    | Var v-> 
        if v=x then s 
        else (Var v)
    | Const c -> t
    | Term (f,args) -> Term(f,(List.map (fun arg -> subst s x arg) args))
            
              
(* apply a substitution right to left; use foldBack *)
let apply (s : substitution) (t : term) : term =
    List.foldBack (fun (s_id :id , s_t:term) acc -> (subst s_t  s_id acc )) s t


(* unify one pair *)
let rec unify (s : term) (t : term) : substitution =

    //use the same code that we created in assingment 1 to merge the lists :)
    let rec merge twolists = 
        match twolists with
        | ([],[]) -> []
        | ([],x::xs) -> failwith "Error -- lists are not of the same length"
        | (x::xs, []) -> failwith "Error -- lists are not of the same length"
        | (x::xs, y::ys) -> (x,y) :: (merge (xs,ys))
        
    match (s,t) with
    //two variables we add them to the substition directly 
    | (Var v1,Var v2) -> []
    //variable and a constant easy TODO: f(x,x) f(3,4)
    | (Var v,Const c) | (Const c,Var v)-> [(v,Const c)]
    //const and another const much match
    | (Const c1,Const c2) -> if c1=c2 then [] else failwith "not unifiable: clashing constants"
    //we cannot match a function with a constant
    | (Term _,Const _) | (Const _,Term _) -> failwith "not unifiable: term constant clash"
    //match two functions
    | (Term (f1,args1),Term (f2,args2)) ->
        if f1=f2 && List.length args1 = List.length args2 then
            unify_list (merge (args1,args2))
        else failwith "not unifiable: head symbol conflict"
    //make sure that cases such as f(x)=x don't happen
    | (Var v,Term (f,args)) | (Term (f,args),Var v) ->
        if occurs v (Term (f,args)) then failwith "not unifiable: circularity"
        else [(v,Term (f,args))]


(* unify a list of pairs *)
and unify_list (s : (term * term) list) : substitution =
    match s with
    | [] -> []
    | (t1,t2)::rest -> 
        //work with the rest
        let unified_rest=(unify_list rest)
        //apply the substitution from right to left :)
        (unify (apply unified_rest t1) (apply unified_rest t2))@unified_rest
