////------TASK 2------////

// Flight travellers may check-in the pieces of luggage, that should follow them on their journey, 
// also when it contains multiple stops. A piece of luggage is marked with an identiﬁcation (type Lid) 
// by the start of the journey and that identiﬁcation is associated with the route (type Route) of the 
// journey. A route is a list of pairs identifying the ﬂights (type Flight) and airports (type Airport) 
// the luggage is passing on the journey.
// Furthermore, a luggage catalogue (type LuggageCatalogue) is maintained, that uniquely identiﬁes the 
// routes of all pieces of luggage leaving some airport.

// This is captured by the type declarations:

type Lid = string;;
type Flight = string;; 
type Airport = string;;

type Route = (Flight * Airport) list;;
type LuggageCatalogue = (Lid * Route) list;;
type ArrivalCatalogue = (Airport * Lid list) list;; //ArrivalCatalogue type from question 4



// An example of a Luggage Catalogue is:
let lc1 = [
        ("DL 016-914", [("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")]); 
        ("SK 222-142", [("SK 208","ATL"); ("DL 124","BRU"); ("SK 122","JFK")]);
    ];;    

// where ﬁrst element in the list describes that the piece of luggage with identiﬁcation "DL 016-914” 
// is following a route, where it is ﬁrst ﬂown to Atlanta ("ATL”) with ﬂight "DL 189”, then ﬂown to 
// Bruxelles "BRU” with ﬂight "DL 124”, and so on.



//---1---//
// Declare a function findRoute: Lid*LuggageCatalogue -> Route, that ﬁnds the route for a given 
// luggage identiﬁcation in a luggage catalogue. A suitable exception should be raise if a route is 
// not found.


let rec findRoute l lc =
    match lc with
    | (l',r)::_ when l=l'   -> r
    | _::rest               -> findRoute  l rest
    | _                     -> failwith(l + " is an unknown luggage ID");;

//// findRoute tests:

// Should return Route [("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")] since this is the 
// route that luggage "DL 016-914" is flying with (in the luggage catalogue lc1):
let findRouteTest1 = findRoute "DL 016-914" lc1;;

// Should return Route [("SK 208","ATL"); ("DL 124","BRU"); ("SK 122","JFK")] since this is the
// route that luggage "SK 222-142" is flying with:
let findRouteTest2 = findRoute "SK 222-142" lc1;;

// Should fail and print "System.Exception: SK 222-143 is an unknown luggage ID" since "SK 222-143"
// does not exist in the catalogue lc1:
let findRouteTest3 = findRoute "SK 222-143" lc1;;



//---2---//
// Declare a function inRoute: Flight -> Route -> bool, 
// that decides whether a given ﬂight occurs in a route.

let rec inRoute f = function
    | r'::rest -> 
        let (f', _) = r'
        if f'=f then true else (inRoute f (rest))
    //| _::rest              -> inRoute f rest
    | _                    -> false;;

//// inRoute tests:
//routes from the luggage catalogue lc1:
let r1 = [("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")];;
let r2 = [("SK 208","ATL"); ("DL 124","BRU"); ("SK 122","JFK")];;

// Should return true since "DL 189" does incur in route r1:
let inRouteTest1 = inRoute "DL 189" r1;; 

// Should return false since "DL 189" does not incur in route r2:
let inRouteTest2 = inRoute "DL 189" r2;; 

// Should return true since "DL 124" does incur in route r1
let inRouteTest3 = inRoute "DL 124" r1;; 

// Should return true since "DL 124" does incur in route r2
let inRouteTest4 = inRoute "DL 124" r2;;

// Should return false since "DL 125" does not incur in route r2
let inRouteTest5 = inRoute "DL 125" r2;;



//---3---//
// Declare a function withFlight f lc, where f is a ﬂight and lc is a luggage catalogue. 
// The value of the expression withFlight f lc is a list of luggage identiﬁers for the pieces of 
// luggage that should travel with f according to lc. The sequence in which the identiﬁers occur 
// in the list is of no concern. For the above example, both "DL 016-914” and "SK 222-142” should 
// travel with the ﬂight "DL 124”.

let rec withFlight f lc =
    match lc with
    | (l', r'::tail)::rest ->
        let (f',_)=r'
        if f'=f then ([l'] @ withFlight f ((l',tail)::rest)) else (withFlight f ((l',tail)::rest))
    | _::rest              -> withFlight f rest
    | _                    -> [];;

//// withFlight tests:

// Should return with ["DL 016-914"; "SK 222-142"] as these are the luggages traveling with flight "DL 124":
let withFlightTest1 = withFlight "DL 124" lc1;; 

// Should return with ["SK 222-142"] as this is the only luggage traveling with flight "SK 208":
let withFlightTest2 = withFlight "SK 208" lc1;; 

// Should return with [] as no luggages from luggage catalogue lc1 are traveling with flight "SK 209":
let withFlightTest3 = withFlight "SK 209" lc1;;

// Should return with ["DL 016-914"] as this is the only luggage traveling with flight "SN 733":
let withFlightTest4 = withFlight "SN 733" lc1;;



//---4---//
// An arrival catalogue associates with every airport, identiﬁcations of all pieces of 
// luggage that should arrive at the airport. This is captured by the type declaration:

// type ArrivalCatalogue = (Airport * Lid list) list

//The following arrival catalogue is derived from the luggage catalogue appearing on the previous page:

let ac1 = [
    ("ATL", ["DL 016-914"; "SK 222-142"]); 
    ("BRU", ["DL 016-914"; "SK 222-142"]); 
    ("JFK", ["SK 222-142"]); 
    ("CPH", ["DL 016-914"])
    ];;


// Declare a function extend: Lid*Route*ArrivalCatalogue -> ArrivalCalalogue so that 
// extend(lid,r,ac) is the arrival catalogue obtained by extending ac with the information 
// that lid will arrive at each airport contained in route r. 

let rec inRouteA a = function
    | r'::rest -> 
        let (_, a') = r'
        if a'=a then true else (inRoute a (rest))
    | _                    -> false;;

let rec withLug lid lug =
    match lug with
    | lid'::tail ->
        if (lid'=lid) then true else (withLug lid tail)
    | _                    -> false;;

let rec extend (lid, r, ac) =
    match r with
    | [] -> ac
    | (_,airport)::rtail ->
        match ac with
        | (a,lid'::tail)::rest -> 
            if (inRouteA a r) && not(withLug lid (lid'::tail)) 
            then (a,lid::(lid'::tail))::extend (lid, rtail, rest)
            else (a,lid'::tail):: extend (lid, rtail, rest)
        | _ -> (airport,[lid])::extend (lid, rtail,[])
    
//// extend tests:

// Should return:
// [("ATL", ["DL 016-915"; "DL 016-914"; "SK 222-142"]);
// ("BRU", ["DL 016-915"; "DL 016-914"; "SK 222-142"]);
// ("JFK", ["DL 016-915"; "SK 222-142"]);
// ("CPH", ["DL 016-914"])]
// Since luggage "DL 016-915" is not in the arrival catalogue it should be added to airport ATL, BRU and JFK
let extendTest1 = extend("DL 016-915",r2,ac1);; 

// Should return:
// [("ATL", ["DL 016-914"; "SK 222-142"]);
// ("BRU", ["DL 016-914"; "SK 222-142"]); 
// ("JFK", ["SK 222-142"]);
// ("CPH", ["DL 016-914"])]
// Since luggage "DL 016-914" is already on route r1 and there will be no changes to ac1
let extendTest2 = extend("DL 016-914",r1,ac1);; a


// Should return:
// [("ATL", ["DL 016-914"; "SK 222-142"]);
// ("BRU", ["DL 016-914"; "SK 222-142"]);
// ("JFK", ["DL 016-914"; "SK 222-142"]); 
// ("CPH", ["DL 016-914"])]
// Since luggage "DL 016-914" is now on both r1 and r2 it is naturally included in all lists!
let extendTest3 = extend("DL 016-914",r2,ac1);; 

// Should return:
// [("ATL", ["DL 016-914"]); 
// ("BRU", ["DL 016-914"]); 
// ("CPH", ["DL 016-914"])] 
// Since arrival catalogue is an empty list, the resulting catalogue is just the route of r1
let extendTest4 = extend("DL 016-914",r1,[]);; 



//---5---//
// Declare a function toArrivalCatalogue: LuggageCatalogue -> ArrivalCatalogue, that creates an arrival 
// catalogue from the information of a given luggage catalogue. You should solve this exercise using extend 
// from the previous question in combination with either List.fold or List.foldBack.

let rec toArrivalCatalogue lc = 
    List.fold (fun acc (lid, r) -> extend(lid, r, acc) ) [("",[])] lc;;


//// toArrivalCatalogue tests:
// Should return:
// [("ATL", ["SK 222-142"; "DL 016-914"]);
// ("BRU", ["SK 222-142"; "DL 016-914"]); 
// ("JFK", ["SK 222-142"]);
// ("CPH", ["DL 016-914"])]
// with luggage catalogue lc1
let toArrivalCatalogueTest1 = toArrivalCatalogue lc1;;



////------TASK 3------////

//---2---//
// In this case, list.fold would be a good choice as it is possible to set the initial value to 0
// which can be seen as false. If any element of a route has an identical flight ID then 1 is added
// and as soon as that happens the result will be true. Hence, it is not needed to e.g. exit the function
// when a constraint is met
let ninRoute f = function
    | r -> 0 < List.fold (fun acc (f', _) -> if f=f' then acc+1 else acc ) 0 r;;
    
// Should return true since "DL 189" does incur in route r1:
let ninRouteTest1 = ninRoute "DL 189" r1;; 

// Should return false since "DL 189" does not incur in route r2:
let ninRouteTest2 = ninRoute "DL 189" r2;; 

// Should return true since "DL 124" does incur in route r1
let ninRouteTest3 = ninRoute "DL 124" r1;; 

// Should return true since "DL 124" does incur in route r2
let ninRouteTest4 = ninRoute "DL 124" r2;;

// Should return false since "DL 125" does not incur in route r2
let ninRouteTest5 = ninRoute "DL 125" r2;;



//---3---//
// List.exists is a fitting function for this problem. It is checked whether the corresponding list of a luggage ID 
// has a flight equal to the argument. It is a boolean question, we just need to know if it exists which makes this 
// function fitting. Instead of making nwithFlight recursive, one could perhaps also wrap the line.exists into a
// List.fold function.
let rec nwithFlight f lc =
    match lc with
    | (lid,r)::rest -> if List.exists (fun (f',_) -> f=f') r then lid::(nwithFlight f rest) else (nwithFlight f rest)
    | _ -> [];;
    

//// withFlight tests:
// Should return with ["DL 016-914"; "SK 222-142"] as these are the luggages traveling with flight "DL 124":
let nwithFlightTest1 = nwithFlight "DL 124" lc1;; 

// Should return with ["SK 222-142"] as this is the only luggage traveling with flight "SK 208":
let nwithFlightTest2 = nwithFlight "SK 208" lc1;; 

// Should return with [] as no luggages from luggage catalogue lc1 are traveling with flight "SK 209":
let nwithFlightTest3 = nwithFlight "SK 209" lc1;;

// Should return with ["DL 016-914"] as this is the only luggage traveling with flight "SN 733":
let nwithFlightTest4 = nwithFlight "SN 733" lc1;;

