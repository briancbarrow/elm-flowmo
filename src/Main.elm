port module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, disabled)
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
                in
                if justHitZero then
                    let
                        ( newModel, _ ) =
                            update ToggleDirection
                                { model
                                    | time = newTime
                                    , lastPosix = now
                                    , running = False
                                }
                    in
                    ( newModel, playSound () )

                else
                    ( { model
                        | time = newTime
                        , lastPosix = now
                        , running = not stopCounter
                      }
                    , Cmd.none
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
    { title =
        if model.running then
            timeDisplay

        else
            "Flowmodoro Technique"
    , body =
        [ div [ class "text-center font-sans p-5" ]
            [ h1
                [ class "text-6xl text-gray-800 mb-8 font-light" ]
                [ text timeDisplay ]
            , div [ class "mb-5" ]
                [ button
                    [ onClick Start
                    , disabled model.running
                    , class
                        "bg-green-500 text-white border-none py-4 px-8 text-lg mx-2 rounded-md transition-all duration-200 disabled:opacity-50 hover:bg-green-600 cursor-pointer disabled:opacity-60 disabled:cursor-not-allowed"
                    ]
                    [ text "Start" ]
                , button
                    [ onClick Stop
                    , disabled (not model.running)
                    , class
                        "bg-red-500 text-white border-none py-4 px-8 text-lg mx-2 rounded-md transition-all duration-200 cursor-pointer disabled:bg-gray-500 disabled:opacity-50 disabled:cursor-not-allowed hover:bg-red-600 "
                    ]
                    [ text "Stop" ]
                ]
            ]
        ]
    }
