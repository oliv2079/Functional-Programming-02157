module Week2

//Exercise 2.1
let f a =
    match a with
    | a when a%2=0 && a%3=0 && a%5<>0-> true
    | a -> false;;


//Exercise 2.2
let rec pow (s,n) =
    match (s,n) with
    | (s,1) -> s
    | (s,n) -> string s + pow(string s,n-1);;
    //pow("hej",10);;

//Exercise 4.3
let rec evenN a = [0..a-1]
    //evenN 5;;

//Exercise 4.8
let rec split items = 
    match items with
    | [] -> ([],[])
    | [x0] -> ([x0],[])
    | [x0;x1] -> ([x0],[x1])
    | x0::x1::xs -> let (r1, r2) = split xs
                    (x0::r1, x1::r2)


//Exercise 4.9
let rec zip (x,y) =
    match (x,y) with
    | ([],[]) -> []
    | (x::[],y::[]) -> [(x,y)]
    | (x::xs,y::ys) when xs.Length=ys.Length -> (x,y)::zip (xs,ys)
    | _ -> failwith "Not the same list lengths";;
    //zip([1..5],[6..10]);; 

//Exercise 4.12
let sum (predicate, list)=
    let rec sum' (predicate: int -> bool) (list: int list) (currentSum:int) =
        match list with
        | [] -> currentSum
        | x::xs -> sum' predicate xs currentSum + (if predicate x then x else 0)
    sum' predicate list 0
 

//ExtraExercise 1
let isEqual x = x && true;;
    //isEqual false;;

//ExtraExercise 2
let plus x y = x+y;;
    //plus 4 6;;

//ExtraExercise 3
let onlya x y = x;;
    //onlya 1 2;;
