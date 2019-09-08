module Exercise1

//Exercise 1
let rec fact(n) =
    match n with
        | 0 -> 0
        | n -> n+fact(n-1);;
        //fact(5);;

//Exercise 2
let rec sum(n,m) =
    match (n, m) with
    | (0,m) -> m
    | (_,m) -> m+n+sum(n-1,m);;
    //sum(3,3);;


//Exercise 3
let rec bin(n,k) =
    match (n,k) with
    | (n,0) -> 1
    | (n,k) -> 
        if (n=k) then 1
        else bin(n-1,k-1)+bin(n-1,k);;
        //bin(4,2);;

//Exercise 4
let rec multiplicity(n,l) = 
    match l with 
    | [] -> 0 
    | x::tail when n=x -> 1+ multiplicity(n,tail)
    | x::tail -> multiplicity(n,tail);;
    //multiplicity(3,[3;3;1;3;3;3;3;4;2;2;2;3]);;

//Exercise 5
let rec mulC(n,l) = 
    match l with 
    | [] -> [] 
    | x::tail -> (x*n)::mulC(n,tail);;
    //mulC(2,[3;7;1;2;3]);;

//Exercise 6
let rec sumE(l1,l2) = 
    match (l1,l2) with 
    | ([],[]) -> []
    | (x::tail,[]) -> x::sumE(tail,[])
    | ([],x::tail) -> x::sumE([],tail)
    | (x::tail1,y::tail2) -> (x+y)::sumE(tail1,tail2);;
    //sumE([1;2;3],[4;5;6;7;8]);;

//Exercise 7
//a
let rec mulX(l) = 
    match l with
    |[]->[]
    |l -> 0::l;;
    //mulX([1;2;3]);;

//b
let rec mul(l1,l2) = 
    match (l1,l2) with
    | ([],[]) -> []
    | (_,[]) -> []
    | ([],_) -> []
    | (x::tail1,y::tail2) -> sumE(mulC(x,y::tail2), mulX(mul(tail1,y::tail2)));;
    //mul([2;3;0;1],[1;2;3]);;

//c
//wut?
