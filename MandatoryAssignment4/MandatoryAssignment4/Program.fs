//Functional Programming 02157
// Oliver Rutving s164209

type Name = string 
type Event = string 
type Point = int 
type Score = Name * Event * Point

type Scoreboard = Score list

let sb = [
    ("Joe", "June Fishing", 35); 
    ("Peter", "May Fishing", 30); 
    ("Joe", "May Fishing", 28); 
    ("Paul", "June Fishing", 28)];;

let sb1 = [
    ("Joe", "June Fishing", 35); 
    ("Peter", "May Fishing", 30); 
    ("Joe", "May Fishing", 31); 
    ("Paul", "June Fishing", 28)];;




////SCOREBOARDS////


//Exercise 1
let inv sl =
    let (_,r) = List.foldBack(fun (_,_,p) (m,b) -> (p, b && p>=m) ) sl (0,true); 
    r;;

//Exercise 1 test
let invTest = inv sb;; //Returns true as the scores are decreasing
let invTest1 = inv sb1;; //Returns false as the scores are not consistently decreasing


//Exercise 2
let insert s sl = 
    let (n',e',p')=s
    let (sln,b') = List.foldBack(fun (n,e,p) (sl',b) -> if (p'<=p && b) then ([(n,e,p)]@[(n',e',p')]@sl',false) else ([(n,e,p)]@sl',b) ) sl ([],true)
    if p'>=0 then (if b' then [(n',e',p')]@sln else sln) else sl;;

//Exercise 2 test
let insertTest1 = insert ("Paul", "May Fishing", 31) sb;; //Score is inserted between score 35 and 30 which is correct
let insertTest2 = insert ("Joe", "June Fishing", -1) sb;; //Score is not inserted as it is negative
let insertTest3 = insert ("Peter", "June Fishing", 0) sb;; //Score is inserted as the last element
let insertTest4 = insert ("Peter", "June Fishing", 100) sb;; //Score is inserted as the first element


//Exercise 3
let get(n,sl) =
    List.fold(fun l (n', e', p') -> if n=n' then l@[(e',p')] else l ) [] sl;;

//Exercise 3 test
let getTest1 = get("Joe", sb);; //Returns [("June Fishing", 35); ("May Fishing", 28)] which are all Joe's scores
let getTest2 = get("Peter", sb);; //Returns [("May Fishing", 30)] which is Peter's only score
let getTest3 = get("Other Guy", sb);; //Returns [] as Other Guy is not in the scoreboard and has no scores


 //Exercise 4
 let top k sl =
    let (sln, kn) = List.fold(fun (sl', k') acc -> if k'<k then (sl'@[acc], k'+1) else (sl',k')  ) ([],0) sl
    if kn=k then Some sln else None;;

//Exercise 4 test
let topTest1 = top -1 sb;; //Negative values returns None
let topTest2 = top 0 sb;; //Returns Some [] as there are 0 scores in top 0 scores
let topTest3 = top 2 sb;; //Returns Some [("Joe", "June Fishing", 35); ("Peter", "May Fishing", 30)] as these are the 2 highest scores
let topTest4 = top 4 sb;; //Returns all scores in the scoreboard as there are exactly 4 scores
let topTest5 = top 6 sb;; //Returns None since there are only 4 scores in the scoreboard




////PATHS IN TREES////


type T<'a> = N of 'a * T<'a> list;;
type Path = int list;;

let td = N("g", []);; 
let tc = N("c", [N("d",[]); N("e",[td])]);; 
let tb = N("b", [N("c",[])]);; 
let ta = N("a", [tb; tc; N("f",[])]);;


//Exercise 1
let rec toList t =
    match t with
    |N(v, t') -> [v]@List.fold(fun l l' -> l@(toList l')) [] t';;

//Exercise 1 test
let toListTest1 = toList td;; //Returns ["g"]
let toListTest2 = toList tc;; //Returns ["c"; "d"; "e"; "g"]
let toListTest3 = toList tb;; //Returns ["b"; "c"]
let toListTest4 = toList ta;; //Returns ["a"; "b"; "c"; "c"; "d"; "e"; "g"; "f"]


//Exercise 2
let rec map f t = //('a->'b)->T<'a>->T<'b>
    match t with
    |N(v, t') -> N(f v, List.fold(fun l l' -> l@[(map f l')]) [] t');;

//Exercise 2 test
let f x = x+"h" ;;
let mapTest1 = map f td;;
let mapTest2 = map f tc;;
let mapTest3 = map f tb;;
let mapTest4 = map f ta;;
//They all return the complete tree where all values have been extended with "h"


//Exercise 3
let rec isPath is t =
    match is with
    |n::tail -> match t with
                |N(_,t') -> if (t'.Length<n+1)||n<0 then false else (isPath tail (t'.Item (n)))
    |[] -> true;;

//Exercise 3 test
let isPathTest1 = isPath [1;1;0] ta;; //This is true, there is a subtree
let isPathTest2 = isPath [1;1;0;0] ta;; //Last element tries to check an empty list (no subtrees) and fails
let isPathTest3 = isPath [0;1] ta;; //This should be false, there is no subtree here
let isPathTest4 = isPath [0;0] ta;; //this should be true
let isPathTest5 = isPath [-1] tc;; //isPath does not accept negative indexes


//Exercise 4
let rec gett is t =
    match is with
    |n::tail -> match t with
                |N(_,t') -> if (t'.Length<n+1)||n<0 then failwith "Could not find subtree" else (gett tail (t'.Item (n)))
    |[] -> t;;

//Exercise 4 test
let gettTest1 = gett [1;1;0] ta;; //Returns td
let gettTest2 = gett [] ta;; //Returns ta
let gettTest3 = gett [0;1] ta;; //No subtree, throw exception: "Could not find subtree"
let gettTest4 = gett [0;0] ta;; //Returns N("c",[])
let gettTest5 = gett [-1] tc;; //gett does not accept negative indexes


//Exercise 5 Aux
let rec exists v t =
    match t with
    |N(v', t') when v=v' -> true
    |N(v', t') -> List.fold(fun b l' -> if (exists v l') then true else b ) false t';;


//Exercise 5 (unfinished)
//let rec tryFindPathto v t = 
//    match t with
//    |N(v',t') when not (exists v t) -> None
//    |N(v',t') -> Some (List.fold(fun l e -> match e with |N(vv,tt) -> if (exists v e) then l@[List.tryFindIndex (fun x -> exists v x) tt]@(tryFindPathto v e) else l ) [] t');;
