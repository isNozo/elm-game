module Main exposing (..)

import Browser
import Browser.Events as Events
import Html exposing (Html, div)
import Html.Events exposing (..)
import Json.Decode as D
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Model =
    { balls : List Ball
    }


type Msg
    = Tick Float
    | RandomBalls (List Ball)
    | MouseMove Float Float


type alias Ball =
    { x : Float
    , y : Float
    , dx : Float
    , dy : Float
    , r : Float
    , color : Color
    , hitcolor : Color
    , hit : Bool
    , id : Int
    }


type alias Color =
    { r : Int, g : Int, b : Int }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { balls = []
      }
    , Random.generate RandomBalls (randomBalls 500)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMove mousex mousey ->
            let
                _ =
                    Debug.log "move" ( mousex, mousey )

                newballs =
                    case model.balls of
                        [] ->
                            []

                        b :: bs ->
                            { b
                                | x = mousex
                                , y = mousey
                                , dx = 0
                                , dy = 0
                                , r = 10
                            }
                                :: bs
            in
            ( { model
                | balls = newballs
              }
            , Cmd.none
            )

        Tick _ ->
            ( { model
                | balls =
                    List.map (updateball model.balls) model.balls
              }
            , Cmd.none
            )

        RandomBalls rndballs ->
            ( { model
                | balls =
                    List.indexedMap (\i ball -> { ball | id = i }) rndballs
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        fw =
            String.fromFloat field.w

        fh =
            String.fromFloat field.h
    in
    div
        [ on "mousemove"
            (D.map2
                MouseMove
                (D.field "clientX" D.float)
                (D.field "clientY" D.float)
            )
        ]
        [ svg
            [ width fw
            , height fh
            , viewBox ("0 0 " ++ fw ++ " " ++ fh)
            ]
            (List.map drawBall model.balls)
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onAnimationFrameDelta Tick ]


field : { w : Float, h : Float }
field =
    { w = 600, h = 400 }


randomBall : Random.Generator Ball
randomBall =
    let
        color =
            { r = 0, g = 155, b = 155 }
    in
    Random.map5
        (\x y dx dy r ->
            Ball
                x
                y
                dx
                dy
                r
                color
                (invColor color)
                False
                0
        )
        (Random.float 0 field.w)
        (Random.float 0 field.h)
        (Random.float -0.5 0.5)
        (Random.float -0.5 0.5)
        (Random.float 1 3)


randomBalls : Int -> Random.Generator (List Ball)
randomBalls num =
    Random.list num randomBall


isHit : Ball -> Ball -> Bool
isHit b1 b2 =
    (b1.id /= b2.id)
        && ((b2.x - b1.x) ^ 2 + (b2.y - b1.y) ^ 2 < (b1.r + b2.r) ^ 2)


updateball : List Ball -> Ball -> Ball
updateball balls ball =
    let
        ( newx, newdx ) =
            if ball.x > field.w || ball.x < 0 then
                ( ball.x - ball.dx, -ball.dx )

            else
                ( ball.x + ball.dx, ball.dx )

        ( newy, newdy ) =
            if ball.y > field.h || ball.y < 0 then
                ( ball.y - ball.dy, -ball.dy )

            else
                ( ball.y + ball.dy, ball.dy )

        newhit =
            List.any (isHit ball) balls
    in
    { ball
        | x = newx
        , y = newy
        , dx = newdx
        , dy = newdy
        , hit = newhit
    }


invColor : Color -> Color
invColor color =
    { r = 255 - color.r
    , g = 255 - color.g
    , b = 255 - color.b
    }


drawBall : Ball -> Svg msg
drawBall ball =
    let
        color =
            if ball.hit then
                ball.hitcolor

            else
                ball.color
    in
    circle
        [ cx (String.fromFloat ball.x)
        , cy (String.fromFloat ball.y)
        , r (String.fromFloat ball.r)
        , fill
            ("rgb("
                ++ String.fromInt color.r
                ++ ","
                ++ String.fromInt color.g
                ++ ","
                ++ String.fromInt color.b
                ++ ")"
            )
        ]
        []
