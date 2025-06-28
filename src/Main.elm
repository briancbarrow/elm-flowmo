port module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (..)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Time



-- PORTS


port playSound : () -> Cmd msg



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { time : Float
    , direction : Direction
    , running : Bool
    , lastPosix : Time.Posix
    }


type Direction
    = CountingUp
    | CountingDown


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = 0
      , direction = CountingUp
      , running = False
      , lastPosix = Time.millisToPosix 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Start
    | Stop
    | ToggleDirection
    | Tick Time.Posix



-- = LinkClicked Browser.UrlRequest
-- | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | running = True, lastPosix = Time.millisToPosix 0 }, Cmd.none )

        Stop ->
            let
                modelWithNewTime =
                    { model | running = False, time = model.time / 5 }
            in
            update ToggleDirection modelWithNewTime

        ToggleDirection ->
            ( { model
                | direction =
                    case model.direction of
                        CountingDown ->
                            CountingUp

                        CountingUp ->
                            CountingDown
              }
            , Cmd.none
            )

        Tick now ->
            if not model.running then
                ( model, Cmd.none )

            else
                let
                    -- If lastPosix is 0, this is the first tick after starting
                    isFirstTick : Bool
                    isFirstTick =
                        Time.posixToMillis model.lastPosix == 0

                    deltaMilli : Float
                    deltaMilli =
                        if isFirstTick then
                            0

                        else
                            toFloat (Time.posixToMillis now - Time.posixToMillis model.lastPosix)

                    deltaSec : Float
                    deltaSec =
                        deltaMilli / 1000

                    newTime : Float
                    newTime =
                        case model.direction of
                            CountingUp ->
                                model.time + deltaSec

                            CountingDown ->
                                model.time - deltaSec

                    stopCounter : Bool
                    stopCounter =
                        newTime <= 0 && model.direction == CountingDown

                    -- Check if we just crossed zero while counting down
                    justHitZero : Bool
                    justHitZero =
                        model.direction == CountingDown && model.time > 0 && newTime <= 0

                    soundCommand : Cmd Msg
                    soundCommand =
                        if justHitZero then
                            playSound ()

                        else
                            Cmd.none
                in
                ( { model
                    | time = newTime
                    , lastPosix = now
                    , running = not stopCounter
                  }
                , soundCommand
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 100 Tick



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        totalSeconds =
            model.time |> round

        hours =
            totalSeconds // 3600

        minutes =
            (totalSeconds - (hours * 3600)) // 60

        seconds =
            totalSeconds - (hours * 3600) - (minutes * 60)

        padZero num =
            if num < 10 then
                "0" ++ String.fromInt num

            else
                String.fromInt num

        timeDisplay =
            if hours > 0 then
                padZero hours ++ ":" ++ padZero minutes ++ ":" ++ padZero seconds

            else
                padZero minutes ++ ":" ++ padZero seconds
    in
    { title = "Flowmodoro Technique"
    , body =
        [ h1 [] [ text timeDisplay ]
        , button [ onClick Start, disabled model.running ] [ text "Start" ]
        , button [ onClick Stop, disabled (not model.running) ] [ text "Stop" ]
        ]
    }
