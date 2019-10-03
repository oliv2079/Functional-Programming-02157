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
    type findRoute = Lid*LuggageCatalogue -> Route;; 

    // An example of a Luggage Catalogue is:
    let LuggageCatalogue = [
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

    let rec findRoute Lid = function
        | (Lid',Route)::_ when Lid=Lid' -> Route
        | _::rest           -> findRoute Lid rest
        | _                             -> failwith(Lid + " is an unknown luggage ID");;
    
        // test:
        // findRoute "DL 016-914" LuggageCatalogue;; // ~> [("DL 189", "ATL"); ("DL 124", "BRU"); ("SN 733", "CPH")]
        // findRoute "SK 222-142" LuggageCatalogue;; ~> [("SK 208", "ATL"); ("DL 124", "BRU"); ("SK 122", "JFK")]
        // findRoute "SK 222-143" LuggageCatalogue;; ~> System.Exception: SK 222-143 is an unknown luggage ID

    // 2
    // Declare a function inRoute: Flight -> Route -> bool, 
    // that decides whether a given ﬂight occurs in a route.

    let rec inRoute Flight = function
        | (Lid', Route'::tail)::rest -> 
            let (Flight', _) = Route'
            if Flight'=Flight then true else (inRoute Flight ((Lid',tail)::rest))
        | _::rest                     -> inRoute Flight rest
        | _                           -> false;;


        // test:
        // inRoute "DL 189" LuggageCatalogue;; ~> val it : bool = true
        // inRoute "DL 124" LuggageCatalogue;; ~> val it : bool = true
        // inRoute "DL 125" LuggageCatalogue;; ~> val it : bool = false
        // inRoute "SK 122" LuggageCatalogue;; ~> val it : bool = true

    // 3
    // Declare a function withFlight f lc, where f is a ﬂight and lc is a luggage catalogue. 
    // The value of the expression withFlight f lc is a list of luggage identiﬁers for the pieces of 
    // luggage that should travel with f according to lc. The sequence in which the identiﬁers occur 
    // in the list is of no concern. For the above example, both "DL 016-914” and "SK 222-142” should 
    // travel with the ﬂight "DL 124”.

    

