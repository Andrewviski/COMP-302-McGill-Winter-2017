module Hw2

(* Assignment 2 *) (* Do not edit this line. *)
(* Student name: Andree Kaba, Id Number: 260493293 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  It is OK to change a "rec" declaration and put the
recursive function inside a helper if you want to.  Your code MUST compile
and must NOT go into infinite loops.  An assignment like that means you
have not tested it.  You will get ZERO FOR THE ENTIRE ASSIGMENT even if the
problem is only with one question.  If you are not able to get the code to
compile and run do not submit it.  *)

(* Question 1 *)

let deriv (f, dx: float) = fun x -> ((f(x + dx) - f(x))/dx)

let rec newton(f,guess:float,tol:float,dx:float) =
    if (f guess)<tol then
        guess
    else
        newton(f,(guess - ((f guess)/(deriv(f,dx) guess))),tol,dx)

(* For testing
let make_cubic(a:float,b,c) = fun x -> (x*x*x + a * x*x + b*x + c)
newton(make_cubic(2.0,-3.0,1.0),0.0,0.0001,0.0001)
let root = newton(sin,5.0,0.0001,0.0001)
*)

(* Question 2 *)

type term = Term of float * int
type poly = Poly of (float * int) list

exception EmptyList

(* An auxiliary function to help maintain the poly in the required form *)

let fix(Poly p:poly):poly =
    let l=(List.filter (fun (a:float,b:int) -> a <> 0.0 ) p |> List.sortWith (fun (c1,e1) (c2,e2)-> if e1 < e2 then 1 else -1))
    if l=[] then
        Poly [(0.0,0)]
    else
        Poly l 

let multiplyPolyByTerm(Term (c,e):term, Poly p:poly):poly =
    match p with
    | [] -> raise EmptyList
    | _ -> Poly (List.map (fun (tc,te) -> (tc*c,te+e)) p) |> fix
                    
let addTermToPoly(Term (c,e):term, Poly p:poly):poly =
    match p with
    | [] -> raise EmptyList
    | _ ->
        match List.tryFind (fun (_,x) -> x=e) p with
        | None -> (Poly ((c,e)::p))
        | Some (a,b) -> (Poly ((c+a,e)::(p |> List.filter (fun (tc,te) -> te <> e)))) |> fix

let addPolys(Poly p1:poly, Poly p2:poly):poly = 
    match (p1,p2) with
    | (_, []) -> raise EmptyList
    | ([], _) -> raise EmptyList
    | (_, _) -> (List.fold (fun (Poly acc) (a,b) -> addTermToPoly(Term (a,b),Poly acc)) (Poly p1) p2) |> fix


let multPolys(Poly p1:poly, Poly p2:poly) =
    match (p1,p2) with
    | (_, []) -> raise EmptyList
    | ([], _) -> raise EmptyList
    | (_, _) ->  (List.fold (fun acc p -> addPolys(acc,p)) (Poly [(0.0,0)])
                    [for t in p1 do yield multiplyPolyByTerm(Term t,Poly p2)]) |> fix
                

let exp(b:float, e:int) =
  let rec helper(b:float, e:int, a: float) =
    if (b = 0.0) then 0.0
    elif (e = 0) then a
    elif (e % 2 = 1) then helper(b,e-1, b*a)
    else helper(b*b,e/2,a)
  helper(b,e,1.0)

let evalTerm (v:float) (Term (c,e):term) = if (e=0) then c else c * exp(v,e)

let evalPoly(Poly p:poly,v:float):float =
    match p with
    | [] -> raise EmptyList
    | _ -> List.fold (fun acc (c,e)  -> acc+ evalTerm v (Term (c,e))) 0.0 p

let diffPoly (Poly p) =
    match p with
    | [] -> raise EmptyList
    | _ -> Poly ((List.map (fun (a:float,b) -> (a*float(b),b-1)) p)) |> fix

(* Question 3 *)
type Exptree =
    | Const of int
    | Var of string
    | Add of Exptree * Exptree
    | Mul of Exptree * Exptree

type Bindings = (string * int) list

(* exception notFound *)
exception NotFound

let rec lookup(name:string, env: Bindings) =
    match env with
    | [] -> None
    | (s,v:int)::xs -> 
        if s = name then
            Some v
        else
            (lookup(name,xs))

let rec insert(name:string, value: int, b: Bindings) =
    match b with
    | [] -> [(name,value)]
    | (s,v)::xs -> 
        if name <= s then
            (name,value)::(s,v)::xs
        else
            (s,v)::(insert(name,value,xs))

let rec eval(exp : Exptree, env:Bindings) =
    match exp with
    | Const x -> Some x
    | Var x -> lookup(x,env)
    | Add (l,r) -> 
        match (eval(l,env),eval(r,env)) with
        | (None,_) -> None
        | (_,None) -> None
        | (Some a,Some b) -> Some (a+b)            
    | Mul (l,r) -> 
        match (eval(l,env),eval(r,env)) with
        | (None,_) -> None
        | (_,None) -> None
        | (Some a,Some b) -> Some (a*b)

(* For testing
let env:Bindings = [("a",3);("b",4);("c",5)]
let exp1 = Add(Const 3, Const 4)
let exp2 = Add(Const 3, Var "b")
let exp3 = Add(Var "c", Var "b")
let exp4 = Mul(exp3,exp2)
let exp5 = Add(Var "d",exp3)
let env2 = insert("b",10,env)
*)


(* Question 4 *)

type Team = string
type Goals = Goals of int
type Points = Points of int
type Fixture = Team * Team
type Result = ((Team * Goals) * (Team * Goals))
type Table = Map<Team,Points>

let league =
  ["Chelsea"; "Spurs"; "Liverpool"; "ManCity"; "ManUnited"; "Arsenal"; "Everton"; "Leicester"]

let pointsMade (r: Result) = 
    let ((t1,Goals g1),(t2,Goals g2))=r
    if g1>g2 then
        ((t1,Points 3),(t2,Points 0))
    else if g1=g2 then
         ((t1,Points 1),(t2,Points 1))
    else
         ((t1,Points 0),(t2,Points 3))

let initEntry (name:Team) = (name, Points 0)

let initializeTable l = Map.ofList (List.map initEntry l)

let weekend1:Result list = [(("Chelsea", Goals 2),("Spurs", Goals 1)); (("Liverpool", Goals 3),("ManCity", Goals 2));(("ManUnited", Goals 1),("Arsenal", Goals 4));(("Everton", Goals 1),("Leicester", Goals 5))]

let weekend2:Result list = [(("Chelsea", Goals 5),("Arsenal", Goals 0)); (("Spurs", Goals 3),("ManCity",Goals 2)); (("ManUnited", Goals 1),("Liverpool", Goals 0));(("Everton", Goals 3),("Leicester", Goals 5))]

let s = [weekend2;weekend1]

let updateTable(t:Table,r:Result):Table =
    let ((t1,Points p1),(t2,Points p2))= pointsMade(r)
    (t |> Map.map (fun name (Points p) -> if name=t1 then Points (p+p1)
                                          else if name=t2 then Points (p+p2)
                                          else Points p))

let rec weekendUpdate(t:Table,rl: Result list): Table =
    match rl with
    | [] -> t
    | r::xs -> weekendUpdate( updateTable(t,r),xs)

let rec seasonUpdate(t:Table, sll:Result list list) : Table =
    match sll with
    | [] -> t
    | r::xs -> seasonUpdate( weekendUpdate(t,r),xs)

let less((s1,n1):Team * Points, (s2,n2):Team * Points) =
    if n1<n2 then true
    else if (n1=n2 && s1 > s2) then true
    else false

let rec myinsert item lst =
  match lst with
  | [] -> [item]
  | x::xs -> if less(item,x) then x::(myinsert item xs) else item::lst

let rec isort lst =
  match lst with
  | [] -> []
  | x::xs -> myinsert x (isort xs)

let showStandings (t:Table) = isort (Map.toList t)

(* Question 5 *)

type Destination = City of string
type RoadMap = Roads of Map<Destination, Set<Destination>>

let roadData = [
  "Andulo", ["Bibala"; "Cacolo"; "Dondo"]
  "Bibala", ["Andulo"; "Dondo"; "Galo"]
  "Cacolo", ["Andulo"; "Dondo"]
  "Dondo",  ["Andulo"; "Bibala"; "Cacolo"; "Ekunha"; "Funda"]
  "Ekunha", ["Dondo"; "Funda"]
  "Funda",  ["Dondo"; "Ekunha"; "Galo"; "Kuito"]
  "Galo",   ["Bibala"; "Funda"; "Huambo"; "Jamba"]
  "Huambo", ["Galo"]
  "Jamba",  ["Galo"]
  "Kuito",  ["Ekunha"; "Funda"]
]

let makeRoadMap data =
    Roads (Map.ofList (List.map (fun (c:string,l:string list) -> (City c,Set<Destination> [for c in l do yield City c])) data))

let rec upToManySteps (Roads r) n startCity =
    match n with
    | 0 ->  Set.ofList [startCity]
    | _ ->  let dests=Map.tryFind startCity r
            match dests with
            | None -> raise NotFound 
            | Some neighbors -> (Set.fold (fun acc s -> (Set.union acc s)) (Set.ofList [startCity]) 
                                    (Set.ofList [for c in neighbors do yield (upToManySteps (Roads r) (n-1) c)]))
                                
                   