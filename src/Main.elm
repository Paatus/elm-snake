port module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Events exposing (onKeyDown)
import Canvas exposing (..)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Text exposing (TextAlign(..), align, font)
import Color
import Html exposing (Html, div, h1, p, section)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Random
import String exposing (fromInt)
import Time



-- TYPES --


type alias Position =
    ( Int, Int )


type Direction
    = Up
    | Right
    | Down
    | Left


type alias Flags =
    { highScore : Int }


type alias Snake =
    { direction : Direction, head : Position, body : List Position }


type alias Model =
    { canvasProps : { width : Int, height : Int, cellSize : Int, size : Int }
    , snake : Snake
    , food : Position
    , gameOver : Bool
    , acceptingInput : Bool
    , highScore : Int
    }


type Msg
    = NoOp
    | Tick Time.Posix
    | KeyPress Direction
    | RestartGame
    | NewFood Position


randomPosition : Int -> (Position -> Msg) -> Cmd Msg
randomPosition size msg =
    Random.pair (randomWidth size) (randomHeight size)
        |> Random.generate msg


randomWidth : Int -> Random.Generator Int
randomWidth size =
    Random.int 0 (size - 1)


randomHeight : Int -> Random.Generator Int
randomHeight size =
    Random.int 0 (size - 1)


disallowedDirectionTransitions : List ( Direction, Direction )
disallowedDirectionTransitions =
    [ ( Left, Right ), ( Right, Left ), ( Up, Down ), ( Down, Up ) ]


port setHighscore : Int -> Cmd msg


main : Program Decode.Value Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.gameOver then
        Sub.batch [ onKeyDown keyDecoder ]

    else
        Sub.batch [ Time.every 100 Tick, onKeyDown keyDecoder ]


initialModel : Int -> Model
initialModel highScore =
    let
        size =
            25
    in
    { canvasProps = { width = 400, height = 400, cellSize = 400 // size, size = size }
    , snake = { direction = Down, head = ( 10, 10 ), body = [ ( 10, 9 ), ( 10, 8 ), ( 10, 7 ) ] }
    , food = ( -15, -15 ) -- place a food outside the board and generate a position at random
    , gameOver = False
    , acceptingInput = True
    , highScore = highScore
    }


flagsDecoder : Decode.Decoder Flags
flagsDecoder =
    Decode.map Flags (Decode.field "highScore" Decode.int)


init : Decode.Value -> ( Model, Cmd Msg )
init val =
    case Decode.decodeValue flagsDecoder val of
        Ok flags ->
            initModel flags

        Err _ ->
            initModel { highScore = 0 }


initModel : Flags -> ( Model, Cmd Msg )
initModel flags =
    ( initialModel flags.highScore
    , randomPosition (initialModel 0).canvasProps.size NewFood
    )


maybeSetHighscore : Int -> Int -> Cmd msg
maybeSetHighscore previousHS score =
    if score > previousHS then
        setHighscore score

    else
        Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RestartGame ->
            initModel { highScore = model.highScore }

        NewFood pos ->
            ( { model | food = pos }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        Tick _ ->
            tick model

        KeyPress direction ->
            if List.member ( model.snake.direction, direction ) disallowedDirectionTransitions || not model.acceptingInput then
                ( model, Cmd.none )

            else
                ( { model | snake = { head = model.snake.head, direction = direction, body = model.snake.body }, acceptingInput = False }, Cmd.none )


tick : Model -> ( Model, Cmd Msg )
tick model =
    if model.gameOver then
        runGameOverTick model

    else
        runGameTick model


runGameOverTick : Model -> ( Model, Cmd Msg )
runGameOverTick model =
    ( model, Cmd.none )


runGameTick : Model -> ( Model, Cmd Msg )
runGameTick model =
    let
        boardSize =
            model.canvasProps.size

        wrap : Position -> Position
        wrap ( x, y ) =
            if x < 0 then
                ( boardSize - 1, y )

            else if y < 0 then
                ( x, boardSize - 1 )

            else if x >= boardSize then
                ( 0, y )

            else if y >= boardSize then
                ( x, 0 )

            else
                ( x, y )

        snake =
            model.snake

        newHeadPos : Position
        newHeadPos =
            case model.snake.direction of
                Up ->
                    wrap ( Tuple.first snake.head, Tuple.second snake.head - 1 )

                Down ->
                    wrap ( Tuple.first snake.head, Tuple.second snake.head + 1 )

                Left ->
                    wrap ( Tuple.first snake.head - 1, Tuple.second snake.head )

                Right ->
                    wrap ( Tuple.first snake.head + 1, Tuple.second snake.head )

        snakeAte =
            newHeadPos == model.food

        gameOver =
            checkSelfCollision model.snake

        score =
            List.length newSnake.body - List.length (initialModel 0).snake.body

        highScore =
            if gameOver && score > model.highScore then
                score

            else
                model.highScore

        newSnake : Snake
        newSnake =
            if gameOver then
                model.snake

            else
                { direction = model.snake.direction, head = newHeadPos, body = updateBody snakeAte model.snake }

        cmd =
            if snakeAte then
                randomPosition model.canvasProps.size NewFood

            else if gameOver then
                maybeSetHighscore model.highScore (List.length newSnake.body - List.length (initialModel 0).snake.body)

            else
                Cmd.none
    in
    ( { model | snake = newSnake, gameOver = gameOver, acceptingInput = True, highScore = highScore }, cmd )


checkSelfCollision : Snake -> Bool
checkSelfCollision snake =
    List.member snake.head snake.body


updateBody : Bool -> Snake -> List Position
updateBody snakeAteFood { head, body } =
    if snakeAteFood then
        head :: body

    else
        head :: removeLast body


removeLast : List a -> List a
removeLast list =
    List.take (List.length list - 1) list


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey string =
    case string of
        "ArrowUp" ->
            KeyPress Up

        "ArrowLeft" ->
            KeyPress Left

        "ArrowDown" ->
            KeyPress Down

        "ArrowRight" ->
            KeyPress Right

        " " ->
            RestartGame

        _ ->
            NoOp



-- VIEW --


view : Model -> Html Msg
view model =
    let
        width =
            model.canvasProps.width

        height =
            model.canvasProps.height

        score : Int
        score =
            List.length model.snake.body - List.length (initialModel 0).snake.body
    in
    div [ class "h-full grid place-items-center bg-neutral-300 dark:bg-neutral-800" ]
        [ div []
            [ section [ class "flex justify-between w-full max-w-[400px] mb-2" ]
                [ h1 [ class "text-[green] text-5xl font-bold" ] [ Html.text "SnakElm" ]
                , div [ class "dark:text-gray-200" ]
                    [ p [] [ Html.text ("Score: " ++ fromInt score) ]
                    , p [] [ Html.text ("Highscore: " ++ fromInt model.highScore) ]
                    ]
                ]
            , Canvas.toHtml ( width, height )
                []
                (shapes [ fill Color.darkGreen ] [ rect ( 0, 0 ) (toFloat width) (toFloat height) ]
                    :: renderWorld model
                )
            ]
        ]


renderWorld : Model -> List Renderable
renderWorld { canvasProps, snake, food, gameOver } =
    let
        transformPosition : Position -> Point
        transformPosition pos =
            Tuple.mapBoth (\p -> p * canvasProps.cellSize |> toFloat) (\p -> p * canvasProps.cellSize |> toFloat) pos

        -- shifts position by a half tile, because circle takes center position
        centerPosition : Point -> Point
        centerPosition p =
            Tuple.mapBoth (\x -> x + (toFloat <| canvasProps.cellSize // 2)) (\x -> x + (toFloat <| canvasProps.cellSize // 2)) p

        foodPosition : Point
        foodPosition =
            transformPosition food |> centerPosition

        gameOverRenderables : List Renderable
        gameOverRenderables =
            if gameOver then
                [ shapes [ fill (Color.rgba 0 0 0 0.5) ] [ rect ( 0, 0 ) (toFloat <| canvasProps.width) (toFloat <| canvasProps.height) ]
                , text [ font { size = 40, family = "Sans" }, align Center, fill Color.red ] ( toFloat <| canvasProps.width // 2, toFloat <| canvasProps.height // 2 ) "Game Over"
                , text [ font { size = 18, family = "Sans" }, align Center, fill Color.white ] ( toFloat <| canvasProps.width // 2, toFloat <| (canvasProps.height // 2) + 30 ) "Press space te restart"
                ]

            else
                []
    in
    [ shapes [ fill Color.red ] [ circle foodPosition (toFloat <| canvasProps.cellSize // 3) ]
    , shapes [ fill (Color.rgba 0.8 0.8 0.8 1), stroke Color.black ]
        (List.map
            (\pos -> rect (transformPosition pos) (toFloat canvasProps.cellSize) (toFloat canvasProps.cellSize))
            snake.body
        )
    , shapes [ fill (Color.rgba 0.9 0.9 0.9 1), stroke Color.black ]
        [ rect (transformPosition snake.head) (toFloat canvasProps.cellSize) (toFloat canvasProps.cellSize) ]
    ]
        ++ gameOverRenderables
