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
    match s with
    | Var v1->
        match t with
        | Var v2 -> [(v1,Var v2)]
        | Const c2 -> [(v1,Const c2)]
        | Term (f2,args2) -> if not (occurs v1 (Term (f2,args2))) then
                                [(v1,Term (f2,args2))]
                             else
                                failwith "not unifiable: circularity"
    | Const c1 ->
        match t with
        | Var v2 -> [(v2,Const c1)]
        | Const c2 -> if c1=c2 then [] 
                      else failwith "not unifiable: clashing constants" 
        | Term (f2,args2) -> failwith "not unifiable: term constant clash"
    | Term (f1,args1) ->
        match t with
        | Var v2 -> if not (occurs v2 (Term (f1,args1))) then
                        [(v2,Term (f1,args1))]
                    else
                        failwith "not unifiable: circularity"
        | Const c2 -> failwith "not unifiable: term constant clash"
        | Term (f2,args2) -> if f2 <> f2 then failwith "not unifiable: head symbol conflict"
                             else unify_list (List.fold2 (fun acc t1 t2 -> (t1,t2)::acc) [] args1 args2)

(* unify a list of pairs *)
and unify_list (s : (term * term) list) : substitution =
    match s with
    | [] -> []
    | (t1,t2)::rest -> let temps=(unify t1 t2)
                       let tempr=(unify_list rest)
                       if List.tryFind 
    
                       
(*
Examples
> let t1 = Term("f",[Var "x";Var "y"; Term("h",[Var "x"])]);;
val t1 : term = Term ("f",[Var "x"; Var "y"; Term ("h",[Var "x"])])
> let t2 = Term("f", [Term("g",[Var "z"]); Term("h",[Var "x"]); Var "y"]);;
val t2 : term =
  Term ("f",[Term ("g",[Var "z"]); Term ("h",[Var "x"]); Var "y"])
> let t3 = Term("f", [Var "x"; Var "y"; Term("g", [Var "u"])]);;
val t3 : term = Term ("f",[Var "x"; Var "y"; Term ("g",[Var "u"])])
> unify t1 t2;;
val it : substitution =
  [("x", Term ("g",[Var "z"])); ("y", Term ("h",[Var "x"]))]
> let t4 = Term("f", [Var "x"; Term("h", [Var "z"]); Var "x"]);;
val t4 : term = Term ("f",[Var "x"; Term ("h",[Var "z"]); Var "x"])
>  let t5 = Term("f", [Term("k", [Var "y"]); Var "y"; Var "x"]);;
val t5 : term = Term ("f",[Term ("k",[Var "y"]); Var "y"; Var "x"])
> unify t4 t5;;
val it : substitution =
  [("x", Term ("k",[Term ("h",[Var "z"])])); ("y", Term ("h",[Var "z"]))]
> unify t5 t4;;
val it : substitution =
  [("x", Term ("k",[Term ("h",[Var "z"])])); ("y", Term ("h",[Var "z"]))]
> apply it t4;;
val it : term =
  Term
    ("f",
     [Term ("k",[Term ("h",[Var "z"])]); Term ("h",[Var "z"]);
      Term ("k",[Term ("h",[Var "z"])])])
> let t6 = Term("f", [Const 2; Var "x"; Const 3]);;
val t6 : term = Term ("f",[Const 2; Var "x"; Const 3])
> let t7 = Term("f", [Const 2; Const 3; Var "y"]);;
val t7 : term = Term ("f",[Const 2; Const 3; Var "y"])
> unify t6 t7;;
val it : substitution = [("x", Const 3); ("y", Const 3)]
> apply it t7;;
val it : term = Term ("f",[Const 2; Const 3; Const 3])
> unify t1 t7;;
System.Exception: not unifiable: term constant clash
....... junk removed .............
Stopped due to error
*)*
