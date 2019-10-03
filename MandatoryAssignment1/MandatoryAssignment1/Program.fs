open System

module MandatoryAssignment1 =

    // Flight travellers may check-in the pieces of luggage, that should follow them on their journey, 
    // also when it contains multiple stops. A piece of luggage is marked with an identiﬁcation (type Lid) 
    // by the start of the journey and that identiﬁcation is associated with the route (type Route) of the 
    // journey. A route is a list of pairs identifying the ﬂights (type Flight) and airports (type Airport) 
    // the luggage is passing on the journey.
    // Furthermore, a luggage catalogue (type LuggageCatalogue) is maintained, that uniquely identiﬁes the 
    // routes of all pieces of luggage leaving some airport.

    // This is captured by the type declarations:

    type Lid = string
    type Flight = string 
    type Airport = string

    type Route = (Flight * Airport) list
    type LuggageCatalogue = (Lid * Route) list
    type ArrivalCatalogue = (Airport * Lid list) list;; //ArrivalCatalogue type from question 4

    

    // An example of a Luggage Catalogue is:
    let lc1 = [
            ("DL 016-914", [("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")]); 
            ("SK 222-142", [("SK 208","ATL"); ("DL 124","BRU"); ("SK 122","JFK")]);
        ];;    

    // where ﬁrst element in the list describes that the piece of luggage with identiﬁcation "DL 016-914” 
    // is following a route, where it is ﬁrst ﬂown to Atlanta ("ATL”) with ﬂight "DL 189”, then ﬂown to 
    // Bruxelles "BRU” with ﬂight "DL 124”, and so on.

    // 1.
    // Declare a function findRoute: Lid*LuggageCatalogue -> Route, that ﬁnds the route for a given 
    // luggage identiﬁcation in a luggage catalogue. A suitable exception should be raise if a route is 
    // not found.
    

    let rec findRoute l = function
        | (l',r)::_ when l=l'   -> r
        | _::rest               -> findRoute l rest
        | _                     -> failwith(l + " is an unknown luggage ID");;
    
        // test: (ignore)
        // findRoute "DL 016-914" lc1;; // ~> [("DL 189", "ATL"); ("DL 124", "BRU"); ("SN 733", "CPH")]
        // findRoute "SK 222-142" lc1;; ~> [("SK 208", "ATL"); ("DL 124", "BRU"); ("SK 122", "JFK")]
        // findRoute "SK 222-143" lc1;; ~> System.Exception: SK 222-143 is an unknown luggage ID

    // 2
    // Declare a function inRoute: Flight -> Route -> bool, 
    // that decides whether a given ﬂight occurs in a route.
    
    //MISTAKE!!!! SHOULD ONLY TAKE A FLIGHT AS AN INPUT
    let rec inRoute f = function
        | (l', r'::tail)::rest -> 
            let (f', _) = r'
            if f'=f then true else (inRoute f ((l',tail)::rest))
        | _::rest              -> inRoute f rest
        | _                    -> false;;


        // test: (ignore)
        // inRoute "DL 189" lc1;; ~> val it : bool = true
        // inRoute "DL 124" lc1;; ~> val it : bool = true
        // inRoute "DL 125" lc1;; ~> val it : bool = false
        // inRoute "SK 122" lc1;; ~> val it : bool = true

    // 3
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

        // test: (ignore)
        // withFlight "DL 124" lc1;; ~> val it : string list = ["DL 016-914"; "SK 222-142"]
        // withFlight "SK 208" lc1;; ~> val it : string list = ["SK 222-142"]
        // withFlight "SK 209" lc1;; ~> val it : string list = []
        // withFlight "SN 733" lc1;; ~> val it : string list = ["DL 016-914"]

        //4
        // An arrival catalogue associates with every airport, identiﬁcations of all pieces of 
        // luggage that should arrive at the airport. This is captured by the type declaration:

        // type ArrivalCatalogue = (Airport * Lid list) list
        
        //The following arrival catalogue is derived from the luggage catalogue appearing on the previous page:
        
        let ac = [
            ("ATL", ["DL 016-914"; "SK 222-142"]); 
            ("BRU", ["DL 016-914"; "SK 222-142"]); 
            ("JFK", ["SK 222-142"]); 
            ("CPH", ["DL 016-914"])
            ];;

        // Declare a function extend: Lid*Route*ArrivalCatalogue -> ArrivalCalalogue so that 
        // extend(lid,r,ac) is the arrival catalogue obtained by extending ac with the information 
        // that lid will arrive at each airport contained in route r. 
