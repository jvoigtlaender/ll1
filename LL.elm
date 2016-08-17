module LL (Symbol(..), RHS, NonTerminal, CFG(..), firstStep, followStep, predictionStep, get) where


type alias NonTerminal =
    String


type alias Terminal =
    String


type Symbol
    = NT NonTerminal
    | T Terminal


type alias RHS =
    List Symbol


type alias Rules =
    List ( NonTerminal, RHS )


type CFG
    = CFG NonTerminal (List NonTerminal) (List Terminal) Rules


type alias FirstSets =
    List ( NonTerminal, List (Maybe Terminal) )


type alias FollowSets =
    List ( NonTerminal, List Terminal )


type alias Predictions =
    List ( NonTerminal, List ( Terminal, List RHS ) )


firstStep : NonTerminal -> RHS -> FirstSets -> FirstSets
firstStep nt rhs firsts =
    add nt (firstExtended rhs firsts) firsts


firstExtended : RHS -> FirstSets -> List (Maybe Terminal)
firstExtended rhs firsts =
    case rhs of
        [] ->
            [ Nothing ]

        (T t) :: _ ->
            [ Just t ]

        (NT a) :: rest ->
            let
                bs =
                    get a firsts
            in
                if List.member Nothing bs then
                    List.filter ((/=) Nothing) bs ++ firstExtended rest firsts
                else
                    bs


followStep : FirstSets -> Maybe NonTerminal -> RHS -> FollowSets -> FollowSets
followStep firsts nt rhs follows =
    let
        ntFollows =
            Maybe.withDefault [] (Maybe.map (\a -> get a follows) nt)

        toAdd : RHS -> List ( NonTerminal, List Terminal )
        toAdd rhs =
            case rhs of
                [] ->
                    []

                (T _) :: rest ->
                    toAdd rest

                (NT b) :: rest ->
                    let
                        bs =
                            firstExtended rest firsts
                    in
                        ( b
                        , List.filterMap identity bs
                            ++ if List.member Nothing bs then
                                ntFollows
                               else
                                []
                        )
                            :: toAdd rest
    in
        List.foldl (uncurry add) follows (toAdd rhs)


predictionStep : FirstSets -> FollowSets -> NonTerminal -> RHS -> Predictions -> Predictions
predictionStep firsts follows nt rhs =
    let
        fromJust m =
            case m of
                Nothing ->
                    Debug.crash "IMPOSSIBLE!"

                Just t ->
                    t
    in
        add' nt (dropDuplicates (List.concatMap (\b -> List.map fromJust (firstExtended (rhs ++ [ T b ]) firsts)) (get nt follows))) rhs


get : a -> List ( a, List b ) -> List b
get nt list =
    case list of
        [] ->
            []

        ( a, bs ) :: rest ->
            if a == nt then
                bs
            else
                get nt rest


add : a -> List b -> List ( a, List b ) -> List ( a, List b )
add a bs list =
    case ( bs, list ) of
        ( [], _ ) ->
            list

        ( _, [] ) ->
            [ ( a, bs ) ]

        ( _, ( a', bs' ) :: rest ) ->
            if a == a' then
                ( a', dropDuplicates (bs' ++ bs) ) :: rest
            else
                ( a', bs' ) :: add a bs rest


add' : a -> List b -> c -> List ( a, List ( b, List c ) ) -> List ( a, List ( b, List c ) )
add' a bs c list =
    case list of
        [] ->
            [ ( a, List.map (\b -> ( b, [ c ] )) bs ) ]

        ( a', bs' ) :: rest ->
            if a == a' then
                ( a', List.foldl (\b -> add b [ c ]) bs' bs ) :: rest
            else
                ( a', bs' ) :: add' a bs c rest


dropDuplicates : List a -> List a
dropDuplicates list =
    case list of
        a :: ((_ :: _) as rest) ->
            a :: dropDuplicates (List.filter ((/=) a) rest)

        _ ->
            list
