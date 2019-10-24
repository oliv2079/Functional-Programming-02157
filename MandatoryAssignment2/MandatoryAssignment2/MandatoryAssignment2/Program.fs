//Course 02157
//Oliver Rutving (s164209)

type Part = string 
type Task = string 
type Cost = int (* can be assumed to be positive *) 
type Duration = int (* can be assumed to be positive *) 
type PartReg = Map<Part, Cost> 
type TaskReg = Map<Task, Duration*Cost>
type Stock = Map<Part, int>

type WorkStation = Task * (Part*int) list 
type AssemblyLine = WorkStation list


//Aux wellDefWS

//Exercise 1
let wellDefWS preg treg workstation = 
    let (tsk,parts)=workstation
    Map.tryFind tsk treg <> None && 
    List.forall(fun (p,k) -> Map.tryFind p preg <> None && k>0) parts;;

//Exercise 2
let wellDefAL preg treg al = List.forall(fun a -> wellDefWS preg treg a) al;;

//Exercise 3
(*longestDuration: AssemblyLine*TaskReg->Duration*)      // ('a * 'b) list * Map<'a,(int * 'c)> -> int
let longestDuration(al,treg) = 
    List.fold (fun a (tsk,_) -> 
        let (d,_)= Map.find tsk treg
        max a d
    ) 0 al;;

//Exercise 4
let partCostAL preg al = 
    List.fold (fun a (_,parts) -> 
        a+List.fold (fun b (pt,qrt) ->
            let p = Map.find pt preg
            b+qrt*p
        ) 0 parts
    ) 0 al;;

//Exercise 5
let prodDurCost treg al =
    List.fold (fun (a,b) (tsk,_) -> 
        let (d,c)= Map.find tsk treg
        (a + d,b+c)
    ) (0,0) al;;



//Aux1 - Exercise 6 (Gave up)
//let rec aux1 al h =
//    match al with
//    |x::xs -> 
//        List.fold(fun l ->
//            if (List.contains l h) then 
//            
//        )x
//    |_->[]

//Exercise 6
//let toStock al =
//    Map.ofList (aux1 al []);;

//TESTS

(* Part and task registers for balance bikes *) 
let preg1 = Map.ofList [
    ("wheel",50);  
    ("saddle",10); 
    ("handlebars",75); 
    ("frame",100); 
    ("screw bolt",5); 
    ("nut",3)];; 

let preg2 = Map.ofList [
    ("wheel",50);  
    ("saddle",10); 
    ("handlebars",75); 
    ("frame",100); 
    ("screw bolt",5);
    ("random",100)];

let treg1 = Map.ofList [
    ("addWheels",(10,2)); 
    ("addSaddle",(5,2)); 
    ("addHandlebars",(6,1))];;

let treg2 = Map.ofList [
    ("addWheels",(10,2)); 
    ("addSaddle",(5,2));
    ("addSomething",(100,100));
    ("addNothing",(1,1))];;

let ws1 = ("addWheels",[("wheel",2);("frame",1);("screw bolt",2);("nut",2)]) 
let ws2 = ("addSaddle",[("saddle",1);("screw bolt",1);("nut",1)]) 
let ws3 = ("addHandlebars",[("handlebars",1);("screw bolt",1);("nut",1)]) 

let ws4 = ("addSomething",[("handlebars",1);("screw bolt",1);("random",10)]) 
let ws5 = ("addNothing",[]) 

let al1 = [ws1; ws2; ws3];;

let al2 = [ws1; ws2; ws3; ws4];;
let al3 = [ws1; ws2; ws3; ws5];;
let al4 = [ws5];;
let al5 = [ws4];;


//1 - wellDefWS
let TestWellDefWS1 = wellDefWS preg1 treg1 ws1;; //True since there is an entry for every task and part in the registries 
let TestWellDefWS2 = wellDefWS preg1 treg2 ws3;; //False because there isn't a addhandlebars task in task registry treg2
let TestWellDefWS3 = wellDefWS preg2 treg2 ws4;; //True since there is an entry for every task and part in the registries 
let TestWellDefWS4 = wellDefWS preg2 treg2 ws5;; //True since addNothing is a task entry in treg2 and no parts are needed.

//2 - wellDefAL
let TestWellDefAL1 = wellDefAL preg1 treg1 al1;; //True since ws1, ws2 and ws3 are all well defined
let TestWellDefAL2 = wellDefAL preg1 treg1 al2;; //False since there is no "random" part in preg1 and no addSomething in treg1
let TestWellDefAL3 = wellDefAL preg2 treg2 al3;; //False since there is no task with addHandlebars in treg2 and no nut in preg2

//3 - longestDuration
let TestLongestDuration1 = longestDuration(al1,treg1) //int=10 since addWheels has the longest duration of all tasks
let TestLongestDuration2 = longestDuration(al3,treg2) //Keynotfoundexception since the assembly line is not well defined
let TestLongestDuration3 = longestDuration(al4,treg2) //int=1 since "addNothing" is the only task and takes 1 time unit

//4 - partCostAL
let TestPartCostAL1 = partCostAL preg1 al1 //int=317 is the cost of all parts in assembly line al1
let TestPartCostAL2 = partCostAL preg1 al3 //int=317 is the cost of all parts in al3 since no parts are added in workstation "addNothing"
let TestPartCostAL3 = partCostAL preg2 al5 //int=1080 since we need 10 of the "random" par which costs 100, handlebars costs 75 and screw bolt 5

//5 - prodDurCost
let TestProdDurCost1 = prodDurCost treg1 al1 //returns (21,5) since all tasks in treg1 takes 10+5+6 time units to complete and costs 2+2+1 to assemble
let TestProdDurCost2 = prodDurCost treg2 al4 //returns (1,1) because "addNothing" takes 1 time unit and costs 1
let TestProdDurCost3 = prodDurCost treg2 al5 //returns (100,100) since there is only one task "addSomething" which takes 100 time units and costs 100 to do

 