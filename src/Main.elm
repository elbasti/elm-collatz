module Main exposing (..)

import Html exposing (Html, Attribute, button, div, input, text)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Html.App
import String

-- Model

type alias CollatzList =
    List Int


type alias Model = 
    { valueToChange : Int
    , list : CollatzList
    }


init : (Model, Cmd Msg)
init = 
    ({valueToChange = 1, list = [1]}, Cmd.none)



-- Functions

isEven : Int -> Bool
isEven int =
    if (%) int 2 == 0 then
       True
       
    else
       False


nextItem : CollatzList -> CollatzList
nextItem list = 
    -- If list is empty, default to `1`
    let
        tail = case List.head (List.reverse list) of
                Nothing -> 1 
                Just a -> a

    in
       if tail == 1 then 
          list -- Stop if 1 
       else if isEven tail then
           List.append list (nextItem [tail // 2])
       else 
           List.append list (nextItem [1 + (3 * tail)])


-- Messages

type Msg = Change String | Calculate


-- View

view : Model -> Html Msg
view model =
    div []
        [ input [placeholder "Number to calculate", onInput Change ] [] 
        , button [ onClick Calculate ] [ text "Calculate" ]
        , text ( toString model)
        ]


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg) 
update msg model =
    case msg of
        Calculate -> 
            let 
                newList = List.append [model.valueToChange] [] |> nextItem 
            in
                ( { model | list = newList}, Cmd.none )

        Change newValue ->
            let 
                newValToInt = String.toInt newValue |> Result.toMaybe |> Maybe.withDefault 0
            in 
                ( { model | valueToChange = newValToInt }, Cmd.none )
        
-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- MAIN

main : Program Never
main = 
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

