module Main exposing (..)

import Html exposing (Html, Attribute, button, div, input, text, ul, li)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Html.App
import String
import List.Extra

-- Model

type alias Model = 
    { valueToChange : Int
    , list : List Int 
    }


init : (Model, Cmd Msg)
init = 
    ({valueToChange = 1, list = [1]}, Cmd.none)



-- Functions

isEven : Int -> Bool
isEven int =
    (%) int 2 == 0


collatz : Int -> Maybe (Int, Int)
collatz n = 
    -- Read comment to collatzSequence to understand the type signature
    if n == 1 then 
        Nothing

    else if isEven n then
        let res = (n // 2) in
            Just (res, res)

    else
        let res = ((n * 3) + 1) in 
            Just (res, res)


collatzSequence : Int -> List Int
collatzSequence int = 
        -- unfoldr will apply collatz to int.
        -- It expects Maybe (a, b). If a is not nothing, it will apply 
        -- collatz again to b
        (List.Extra.unfoldr collatz int)

-- Messages

type Msg = Change String | Calculate


-- VIEW

listItem : a -> Html Msg
listItem item =
  li [] [text (toString item)]

view : Model -> Html Msg
view model =
    div [id "container"]
        [
        div [id "inputBox"]
            [ input [placeholder "Number to calculate", onInput Change ] [] 
            , button [ onClick Calculate ] [ text "Calculate" ]
            ]
        , div [id "outputBox"] 
            [ text "Output:" 
            , ul [] (List.map listItem model.list) ]
        ]


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg) 
update msg model =
    case msg of
        Calculate -> 
            let 
                newList = collatzSequence model.valueToChange 
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

