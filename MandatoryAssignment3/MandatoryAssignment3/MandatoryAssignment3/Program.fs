//Functional Programming 02157
// Oliver Rutving s164209

type ExprTree = 
    | Const of int 
    | Ident of string 
    | Sum of ExprTree * ExprTree 
    | Let of string * ExprTree * ExprTree
    
    | App of string * ExprTree list

//Exercise 1
let valTree1 = Let("x", Const 5, Sum(Ident "x", Ident "x") )
let valTree2 = Let("x", Const 5, Sum(Ident "x", Sum(Ident "z", Ident "x") ) )
let valTree3 = Let("x", Sum(Const 9, Ident "y"), Sum(Const 90, Ident "x") )
let valTree4 = Let("x", Sum(Ident "x", Sum(Const 2, Sum(Const 2, Ident "y") ) ), Ident "x")


//Exercise 2
let rec findConst vTree =
    match vTree with
    | Const n -> [n]
    | Ident _ -> []
    | Sum (n1, n2) -> findConst(n1) @ findConst(n2)
    | Let (_, n1, n2) -> findConst(n1) @ findConst(n2)
    
    | App (_, l) -> List.fold (fun acc n -> acc@findConst(n)) [] l;;

//Exercise 2 test
let findConstTest1 = findConst valTree1;; //Expected to return [5] as 5 is the only integer constant in valTree1
let findConstTest2 = findConst valTree2;; //Expected to return [5] as 5 is the only integer constant in valTree2
let findConstTest3 = findConst valTree3;; //Expected to return [9, 90] as 9 and 90 are the only integer constants in valTree3
let findConstTest4 = findConst valTree4;; //Expected to return [2, 2] as 2 and 2 are the only integer constants in valTree4



//Exercise 3: ExprTree -> Set<string>
let rec findFree vTree =
    let findFreeSet = Set.empty
    match vTree with
    | Const _ -> findFreeSet
    | Ident x -> findFreeSet.Add x
    | Sum (n1, n2) ->
        if Set.isEmpty (findFree n1) && Set.isEmpty (findFree n2) then Set.empty
        else if Set.isEmpty (findFree n1) then findFree n2
        else if Set.isEmpty (findFree n2) then findFree n1
        else Set.union (findFree n1) (findFree n2)
    | Let (x, n1, n2) ->
        if Set.isEmpty (findFree n1) && Set.isEmpty (findFree n2) then Set.empty
        else if Set.isEmpty (findFree n1) then Set.remove x (findFree n2)
        else if Set.isEmpty (findFree n2) then findFree n1
        else Set.union (findFree n1) (Set.remove x (findFree n2))

    | App (x, l) ->  List.fold (fun acc n -> if Set.isEmpty (findFree n) then acc else if Set.isEmpty acc then (findFree n) else Set.union acc (findFree n) ) Set.empty l;;

//Exercise 3 test
let findFreeTest1 = findFree valTree1;; //Returns an empty set as there are no free identifiers in valTree1
let findFreeTest2 = findFree valTree2;; //Returns set ["z"] as "z" is the only free identifier in valTree2
let findFreeTest3 = findFree valTree3;; //Returns set ["y"] as "y" is the only free identifier in valTree3
let findFreeTest4 = findFree valTree4;; //Returns set ["x"; "y"] as "x" and "y" are free identifier in valTree4


//Exercise 4: string -> int -> ExprTree -> ExprTree
let rec subst x n t =
    match t with
    | Const n' -> Const n'
    | Ident x' -> if x=x' then Const n else Ident x'
    | Sum (n1, n2) -> Sum(subst x n n1, subst x n n2)
    | Let (x', n1, n2) -> Let(x', subst x n n1, n2)

    | App (x', l) -> App(x', List.fold(fun acc n' -> acc@[subst x n n']) [] l);;

//Exercise 4 test
let substTest1 = subst "x" 0 valTree1;; //There are no free identifiers so it returns valTree1
let substTest2 = subst "x" 0 valTree2;; //"z" is the only free identifier in valTree2, so it returns valTree2
let substTest3 = subst "x" 0 valTree3;; //"y" is the only free identifier in valTree3, so it returns valTree3
let substTest4 = subst "x" 0 valTree4;; //"x" is a free identifier and is replaced with Const 0 returning: 
                                            //Let ("x",Sum (Const 0,Sum (Const 2,Sum (Const 2,Ident "y"))),Ident "x")


//Exercise 5
//App(x, e) is expected to read: "let x be defined by e". 
//x is therefor not expected to be a free identifier.
//The free identifiers of App(x, e) consists of the free identifiers of e.
//ExprTree has been extended with the constructer App.
//Solutions to Exercise 2 through 4 have been modified to cope with the constructer App.

//Exercise 5 test:
//Defining two new tree values:
let valTree5 = Let("x", App("y", [Const 2; Ident "x"; Const 3]) , Sum(Const 10, Ident "x") )
let valTree6 = Let("x", App("z", [Const 2; Ident "y"; Ident "z"]) , Sum(Const 10, App("y", [Const 2; Sum(Ident "x", Ident "w")]) ) )
//Testing exercise 2:
let findConstTest5 = findConst valTree5;; //Should return [2;3;10] as these are the constants in valTree5
let findConstTest6 = findConst valTree6;;  //Should return [2;10;2] as these are the constants in valTree6
//Testing exercise 3:
let findFreeTest5 = findFree valTree5;; //Should return ["x"] as "x" is the only free identifier in valTree5
let findFreeTest6 = findFree valTree6;; //Should return ["w"; "y"; "z"] as these are the free identifiers in valTree6
//Testing exercise 4:
let substTest5 = subst "x" 0 valTree5;; //Replaces the one appearance of the free "x" identifier with 0 (constant) and returns:  
                                            //Let ("x",App ("y",[Const 2; Const 0; Const 3]),Sum (Const 10,Ident "x"))
let substTest6 = subst "y" 0 valTree6;; //Replaces the one appearance of the free "y" identifier with 0 (constant) and returns: 
                                            //Let("x",App ("z",[Const 2; Const 0; Ident "z"]),Sum (Const 10,App ("y",[Const 2; Sum (Ident "x",Ident "w")])))

//End of the assignment