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




////SCOREBOARDS////


//Exercise 1
let inv sl =
    let (_,r) = List.foldBack(fun (_,_,p) (m,b) -> (p, b && p>=m) ) sl (0,true); 
    r;;

//Exercise 1 test
let invTest = inv sb;;


//Exercise 2
let insert s sl = 
    let (n',e',p')=s
    let (sln,b') = List.foldBack(fun (n,e,p) (sl',b) -> if (p'<=p && b) then ([(n,e,p)]@[(n',e',p')]@sl',false) else ([(n,e,p)]@sl',b) ) sl ([],true)
    if p'>=0 then (if b' then [(n',e',p')]@sln else sln) else sl;;

//Exercise 2 test
let insertTest1 = insert ("Paul", "May Fishing", 31) sb;;
let insertTest2 = insert ("Joe", "June Fishing", -1) sb;;
let insertTest3 = insert ("Peter", "June Fishing", 0) sb;;
let insertTest4 = insert ("Peter", "June Fishing", 100) sb;;


//Exercise 3
let get n sl =
    List.fold(fun l (n', e', p') -> if n=n' then l@[(e',p')] else l ) [] sl;;

 //Exercise 3 test
 let getTest1 = get "Joe" sb;


 //Exercise 4
 let top k sl =
    let (sln, kn) = List.fold(fun (sl', k') acc -> if k'<k then ([acc]@sl', k'+1) else (sl',k')  ) ([],0) sl
    if kn=k then Some sln else None;;

//Exercise 4 test
let topTest1 = top -1 sb;;
let topTest2 = top 0 sb;;
let topTest3 = top 2 sb;;
let topTest4 = top 4 sb;;
let topTest5 = top 6 sb;;




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
let toListTest1 = toList td;;
let toListTest2 = toList tc;;
let toListTest3 = toList tb;;
let toListTest4 = toList ta;;


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




