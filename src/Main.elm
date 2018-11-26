module Main (main) where

import Examples
import Component
import StartApp
import Html
import Html.Events exposing (onClick, on)
import Html.Attributes exposing (property, id)
import Json.Encode exposing (string)
import Json.Decode exposing (at, int)
import Keyboard
import Effects
import Task exposing (Task)


noEffects =
    flip (,) Effects.none


choices =
    [ ( "zur Übungsaufgabe", Examples.cfg_exercise )
    , ( "aus Textbuch", Examples.cfg_8_7 )
    , ( "aus Textbuch, Variante", Examples.cfg_8_9 )
    , ( "Arithmetik", Examples.cfg_left_associative_arith )
    , ( "Arithmetik, Linksrekursion entfernt", Examples.cfg_left_associative_arith_norec )
    , ( "Session-Beispiel", Examples.cfg_8_9_no_eps )
    , ( "Session-Beispiel, linksfaktorisiert", Examples.cfg_8_9_no_eps_mod )
    , ( "Einführungsbeispiel", Examples.cfg_arith )
    , ( "Einführungsbeispiel, linksfaktorisiert", Examples.cfg_arith_mod )
    , ( "vereinfachtes Beispiel", Examples.cfg_arith_simpl )
    , ( "vereinfachtes Beispiel, linksfaktorisiert", Examples.cfg_arith_simpl_mod )
    , ( "vereinfachtes Beispiel, linksfaktorisiert, keine Linksrekursion", Examples.cfg_arith_simpl_mod_norec )
    , ( "fast triviales Beispiel", Examples.cfg_8_4 )
    , ( "zur Klausuraufgabe", Examples.cfg_exam )
    , ( "LR-Beispiel aus Vorlesung", Examples.cfg_lr_lecture )
    , ( "LR-Beispiel aus Übung", Examples.cfg_lr_exercise )
    ]


init =
    case List.head choices of
        Nothing ->
            Debug.crash "IMPOSSIBLE!"

        Just ( _, cfg ) ->
            noEffects <|
                { component = Component.init cfg, previous = [], next = [] }


view address { previous, component, next } =
    Html.div [] <|
        [ Html.h3 [ property "innerHTML" <| string "LL(1)-Analyse" ] []
        , Html.select
            [ id "choices", on "change" (at [ "target", "selectedIndex" ] int) (Signal.message address << Switch) ]
            (List.map (\( name, _ ) -> Html.option [] [ Html.text name ]) choices)
        , Html.p [] []
        , Component.view (Signal.forwardTo address Component) component
        ]
            ++ (if List.isEmpty previous then
                    []
                else
                    [ Html.button [ onClick address Back ] [ Html.text "Zurück" ] ]
               )
            ++ (if List.isEmpty next then
                    []
                else
                    [ Html.button [ onClick address Forth ] [ Html.text "Vor" ] ]
               )


type Action
    = Component Component.Action
    | Back
    | Forth
    | Arrows { x : Int, y : Int }
    | Escape
    | Switch Int
    | NoOp


update action ({ previous, component, next } as model) =
    case ( previous, action, next ) of
        ( _, Component action, _ ) ->
            let
                component' =
                    Component.update action component
            in
                ( if component == component' then
                    model
                  else
                    { previous = component :: previous, component = component', next = [] }
                , if List.member action [ Component.Follow, Component.Predictions ] then
                    Effects.task (Task.map (always NoOp) (Signal.send focusStepMailbox.address ()))
                  else
                    Effects.none
                )

        ( back :: rest, Back, _ ) ->
            noEffects <|
                { previous = rest, component = back, next = component :: next }

        ( _, Forth, forth :: rest ) ->
            noEffects <|
                { previous = component :: previous, component = forth, next = rest }

        ( _, Arrows { y }, _ ) ->
            if y == 1 then
                update (Component Component.Up) model
            else if y == -1 then
                update (Component Component.Down) model
            else
                noEffects model

        ( _, Escape, _ ) ->
            update (Component Component.Escape) model

        ( _, Switch i, _ ) ->
            ( case List.head (List.drop i choices) of
                Nothing ->
                    model

                Just ( _, cfg ) ->
                    { component = Component.init cfg, previous = [], next = [] }
            , Effects.task (Task.map (always NoOp) (Signal.send switchedMailbox.address ()))
            )

        ( _, NoOp, _ ) ->
            noEffects model

        _ ->
            Debug.crash "IMPOSSIBLE!"


switchedMailbox =
    Signal.mailbox ()


port switched : Signal ()
port switched =
    switchedMailbox.signal


focusStepMailbox =
    Signal.mailbox ()


port focusStep : Signal ()
port focusStep =
    focusStepMailbox.signal


app =
    StartApp.start
        { init = init
        , view = view
        , update = update
        , inputs =
            [ Signal.map Arrows Keyboard.arrows
            , Signal.filterMap
                (\b ->
                    if b then
                        Just Escape
                    else
                        Nothing
                )
                NoOp
                (Keyboard.isDown 27)
            ]
        }


main =
    app.html


port run : Signal (Task Effects.Never ())
port run =
    app.tasks
