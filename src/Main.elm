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
                        stopCounter && model.time > 0
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
        [ div [ class "min-h-screen bg-gradient-to-br from-blue-50 to-indigo-100 flex items-center justify-center font-sans" ]
            [ div [ class "bg-gradient-to-br from-blue-100 to-indigo-200 rounded-3xl shadow-xl border border-blue-100 p-12 max-w-md w-full mx-4" ]
                [ -- Timer section
                  div [ class "text-center mb-8" ]
                    [ div [ class "relative inline-flex items-center justify-center" ]
                        [ div [ class "w-32 h-32 flex items-center justify-center" ]
                            [ div [ class "text-center" ]
                                [ div
                                    [ class
                                        (case model.direction of
                                            CountingUp ->
                                                "text-blue-600 text-lg font-semibold mb-2 transition-all duration-500 ease-in-out"

                                            CountingDown ->
                                                "text-green-600 text-lg font-semibold mb-2 transition-all duration-500 ease-in-out"
                                        )
                                    ]
                                    [ text
                                        (case model.direction of
                                            CountingUp ->
                                                "Focus"

                                            CountingDown ->
                                                "Break"
                                        )
                                    ]
                                , div [ class "text-6xl font-light text-gray-900 drop-shadow-lg" ]
                                    [ text timeDisplay ]
                                ]
                            ]
                        ]
                    ]
                , -- Buttons section
                  div [ class "flex justify-center space-x-4" ]
                    [ button
                        [ onClick Start
                        , disabled model.running
                        , class
                            "bg-gradient-to-r from-blue-500 to-indigo-500 text-white border-none py-3 px-6 text-base rounded-lg transition-all duration-200 disabled:opacity-50 hover:from-blue-600 hover:to-indigo-600 cursor-pointer disabled:cursor-not-allowed shadow-lg hover:shadow-xl transform hover:-translate-y-0.5"
                        ]
                        [ text "Start" ]
                    , button
                        [ onClick Stop
                        , disabled (not model.running)
                        , class
                            "bg-gradient-to-r from-slate-500 to-gray-500 text-white border-none py-3 px-6 text-base rounded-lg transition-all duration-200 disabled:opacity-50 cursor-pointer disabled:cursor-not-allowed shadow-lg hover:from-slate-600 hover:to-gray-600 hover:shadow-xl transform hover:-translate-y-0.5"
                        ]
                        [ text "Stop" ]
                    ]
                ]
            ]
        ]
    }
