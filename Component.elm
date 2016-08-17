module Component (init, view, update, Action(..)) where

import Html
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, attribute, align, autofocus, id)
import String
import LL exposing (..)


init ((CFG _ _ _ rules) as cfg) =
    { cfg = cfg
    , selected = Maybe.map (\( nt, rhs ) -> ( 0, ( Just nt, rhs ) )) (List.head rules)
    , stage = ComputingFirst
    , firstTable = []
    , followTable = []
    , predictions = []
    }


type Stage
    = ComputingFirst
    | FirstFinished
    | ComputingFollow
    | FollowFinished
    | ComputingPredictions


view address { cfg, selected, stage, firstTable, followTable, predictions } =
    let
        (CFG s os ts rules) =
            cfg

        pseudoRule =
            ( Nothing, [ NT s, T "#" ] )

        ts' =
            ts ++ [ "#" ]

        toStr x =
            case x of
                NT nt ->
                    nt

                T t ->
                    t

        renderRHS rhs =
            case rhs of
                [] ->
                    "ε"

                _ ->
                    String.concat <| List.intersperse " " <| List.map toStr rhs

        inFirst =
            List.member stage [ ComputingFirst, FirstFinished ]

        inFollow =
            List.member stage [ ComputingFollow, FollowFinished ]

        cellStyle =
            [ ( "border", "1px solid black" ), ( "padding", "5px" ) ]
    in
        Html.div [] <|
            [ Html.table
                [ style [ ( "width", "100%" ) ] ]
                [ Html.tr
                    []
                    [ Html.th [ style [ ( "width", "33%" ) ], align "left" ] [ Html.text "Grammatik" ]
                    , Html.th [ style [ ( "width", "22%" ) ], align "left" ] [ Html.text "FIRST-Mengen" ]
                    , Html.th
                        [ align "left" ]
                      <|
                        if inFirst then
                            []
                        else
                            [ Html.text "FOLLOW-Mengen" ]
                    ]
                , Html.tr
                    []
                  <|
                    [ Html.td
                        [ attribute "valign" "top" ]
                        [ Html.ol
                            []
                          <|
                            List.indexedMap
                                (\i (( nt, rhs ) as rule) ->
                                    Html.li
                                        ([ onClick address (Select ( i, rule )) ]
                                            ++ if Maybe.map fst selected == Just i then
                                                [ style [ ( "backgroundColor", "yellow" ), ( "width", "90%" ) ] ]
                                               else
                                                []
                                        )
                                        [ Html.text (Maybe.withDefault "" nt ++ " ⇾ " ++ renderRHS rhs) ]
                                )
                            <|
                                List.map (\( nt, rhs ) -> ( Just nt, rhs )) rules
                                    ++ if inFollow then
                                        [ pseudoRule ]
                                       else
                                        []
                        ]
                    , Html.td
                        [ attribute "valign" "top" ]
                      <|
                        [ Html.dl
                            []
                            (List.concatMap
                                (\nt ->
                                    let
                                        entries =
                                            get nt firstTable
                                    in
                                        [ Html.dt [] [ Html.text (nt ++ ":") ]
                                        , Html.dd
                                            []
                                            [ Html.text <|
                                                if List.isEmpty entries then
                                                    "Ø"
                                                else
                                                    "{ " ++ String.concat (List.intersperse " , " (List.map (Maybe.withDefault "ε") entries)) ++ " }"
                                            ]
                                        ]
                                )
                                (s :: os)
                            )
                        ]
                            ++ (if inFirst then
                                    [ Html.button [ id "step", onClick address Step, autofocus True ] [ Html.text "Schritt" ] ]
                                else
                                    []
                               )
                            ++ (if stage == FirstFinished then
                                    [ Html.button [ onClick address Follow ] [ Html.text "Follow" ] ]
                                else
                                    []
                               )
                    ]
                        ++ if inFirst then
                            []
                           else
                            [ Html.td
                                [ attribute "valign" "top" ]
                              <|
                                [ Html.dl
                                    []
                                    (List.concatMap
                                        (\nt ->
                                            let
                                                entries =
                                                    get nt followTable
                                            in
                                                [ Html.dt [] [ Html.text (nt ++ ":") ]
                                                , Html.dd
                                                    []
                                                    [ Html.text <|
                                                        if List.isEmpty entries then
                                                            "Ø"
                                                        else
                                                            "{ " ++ String.concat (List.intersperse " , " entries) ++ " }"
                                                    ]
                                                ]
                                        )
                                        (s :: os)
                                    )
                                ]
                                    ++ (if inFollow then
                                            [ Html.button [ id "step", onClick address Step ] [ Html.text "Schritt" ] ]
                                        else
                                            []
                                       )
                                    ++ (if stage == FollowFinished then
                                            [ Html.button [ onClick address Predictions ] [ Html.text "Tabelle" ] ]
                                        else
                                            []
                                       )
                            ]
                ]
            ]
                ++ if stage == ComputingPredictions then
                    [ Html.h4 [] [ Html.text "Vorhersagetabelle" ]
                    , Html.table
                        [ style [ ( "border-collapse", "collapse" ) ] ]
                      <|
                        Html.tr [ align "center" ] (Html.td [] [ Html.button [ id "step", onClick address Step ] [ Html.text "Schritt" ] ] :: List.map (\t -> Html.td [ style cellStyle ] [ Html.text t ]) ts')
                            :: List.map
                                (\nt ->
                                    Html.tr [ align "center" ] <|
                                        Html.td [ style cellStyle ] [ Html.text nt ]
                                            :: List.map
                                                (\t ->
                                                    let
                                                        rhss =
                                                            get t (get nt predictions)
                                                    in
                                                        Html.td
                                                            [ style <|
                                                                if List.length rhss < 2 then
                                                                    cellStyle
                                                                else
                                                                    ( "color", "red" ) :: cellStyle
                                                            ]
                                                            [ Html.text <| String.concat <| List.intersperse " , " <| List.map renderRHS rhss ]
                                                )
                                                ts'
                                )
                                (s :: os)
                    , Html.p [] []
                    ]
                   else
                    []


type Action
    = Select ( Int, ( Maybe NonTerminal, RHS ) )
    | Up
    | Down
    | Escape
    | Step
    | Follow
    | Predictions


update action ({ cfg, selected, stage, firstTable, followTable, predictions } as model) =
    let
        (CFG s _ _ rules) =
            cfg

        pseudoRule =
            ( Nothing, [ NT s, T "#" ] )

        rules' =
            List.map (\( nt, rhs ) -> ( Just nt, rhs )) rules
                ++ if List.member stage [ ComputingFollow, FollowFinished ] then
                    [ pseudoRule ]
                   else
                    []

        head' xs =
            case xs of
                x :: _ ->
                    x

                [] ->
                    Debug.crash "IMPOSSIBLE!"
    in
        case ( stage, action ) of
            ( _, Select (( i, _ ) as select) ) ->
                { model
                    | selected =
                        if Maybe.map fst selected == Just i then
                            Nothing
                        else
                            Just select
                }

            ( _, Up ) ->
                { model
                    | selected =
                        Maybe.map
                            (\( i, rule ) ->
                                let
                                    i' =
                                        i - 1
                                in
                                    if i' < 0 then
                                        ( i, rule )
                                    else
                                        ( i', head' (List.drop i' rules') )
                            )
                            selected
                }

            ( _, Down ) ->
                { model
                    | selected =
                        Maybe.map
                            (\( i, rule ) ->
                                let
                                    i' =
                                        i + 1
                                in
                                    if i' >= List.length rules' then
                                        ( i, rule )
                                    else
                                        ( i', head' (List.drop i' rules') )
                            )
                            selected
                }

            ( _, Escape ) ->
                { model | selected = Nothing }

            ( ComputingFirst, Step ) ->
                let
                    process ( nt, rhs ) =
                        case nt of
                            Nothing ->
                                Debug.crash "IMPOSSIBLE!"

                            Just nt ->
                                { model | firstTable = firstStep nt rhs firstTable }
                in
                    Maybe.map (process << snd) selected
                        |> Maybe.withDefault { model | firstTable = Maybe.withDefault firstTable (tryAnyStep firstStep firstTable rules) }
                        |> \({ firstTable } as model) ->
                            { model
                                | stage =
                                    if tryAnyStep firstStep firstTable rules == Nothing then
                                        FirstFinished
                                    else
                                        stage
                            }

            ( FirstFinished, Step ) ->
                model

            ( _, Follow ) ->
                { model | stage = ComputingFollow, selected = Just ( List.length rules, pseudoRule ) }

            ( ComputingFollow, Step ) ->
                let
                    process ( nt, rhs ) =
                        { model | followTable = followStep firstTable nt rhs followTable }
                in
                    Maybe.map (process << snd) selected
                        |> Maybe.withDefault { model | followTable = Maybe.withDefault followTable (tryAnyStep (followStep firstTable) followTable (List.reverse rules')) }
                        |> \({ followTable } as model) ->
                            { model
                                | stage =
                                    if tryAnyStep (followStep firstTable) followTable rules' == Nothing then
                                        FollowFinished
                                    else
                                        stage
                            }

            ( FollowFinished, Step ) ->
                model

            ( _, Predictions ) ->
                { model | stage = ComputingPredictions, selected = Maybe.map (\( nt, rhs ) -> ( 0, ( Just nt, rhs ) )) (List.head rules) }

            ( ComputingPredictions, Step ) ->
                let
                    process ( nt, rhs ) =
                        case nt of
                            Nothing ->
                                Debug.crash "IMPOSSIBLE!"

                            Just nt ->
                                { model | predictions = predictionStep firstTable followTable nt rhs predictions }
                in
                    Maybe.map (process << snd) selected
                        |> Maybe.withDefault { model | predictions = Maybe.withDefault predictions (tryAnyStep (predictionStep firstTable followTable) predictions rules) }


tryAnyStep step table =
    List.foldl
        (\( nt, rhs ) m ->
            case m of
                Nothing ->
                    let
                        table' =
                            step nt rhs table
                    in
                        if table == table' then
                            Nothing
                        else
                            Just table'

                Just _ ->
                    m
        )
        Nothing
