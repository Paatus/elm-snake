module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


main : Program () Int Msg
main =
    Browser.sandbox { init = 0, update = update, view = view }


type Msg
    = Increment
    | Decrement


update : Msg -> number -> number
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view : Int -> Html Msg
view model =
    div [ class "max-w-5xl m-auto text-center" ]
        [ button [ onClick Decrement, class "px-4 py-2 border rounded-sm cursor-pointer" ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick Increment, class "px-4 py-2 border rounded-sm cursor-pointer" ] [ text "+" ]
        ]
