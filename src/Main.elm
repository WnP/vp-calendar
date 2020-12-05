module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, a, div, hr, img, input, option, select, span, text)
import Html.Attributes exposing (alt, class, href, selected, src, style, target, type_, value)
import Html.Events exposing (onInput)
import I18n
import Iso8601
import Task
import Time
import Time.Format.I18n.I_fr_fr as Fr


main : Program String Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { firstDay : Time.Posix
    , monthFormater : Time.Month -> String
    , dayFormater : Time.Weekday -> String
    , language : I18n.Language
    }


init : String -> ( Model, Cmd Msg )
init lang =
    ( { firstDay = getFirstDayOfTheYear 1070
      , monthFormater = Fr.monthName
      , dayFormater = Fr.dayShort
      , language = I18n.parseLang lang
      }
    , Task.perform GotNow Time.now
    )


getFirstDayOfTheYear : Int -> Time.Posix
getFirstDayOfTheYear year =
    String.fromInt year
        ++ "-01-01"
        |> Iso8601.toTime
        |> Result.withDefault (Time.millisToPosix 0)


dayLength : Int
dayLength =
    24 * 60 * 60 * 1000


calFirstDay : Time.Posix -> Time.Posix
calFirstDay firstDay =
    case Time.toWeekday Time.utc firstDay of
        Time.Mon ->
            firstDay

        Time.Tue ->
            computeDays -1 firstDay

        Time.Wed ->
            computeDays -2 firstDay

        Time.Thu ->
            computeDays -3 firstDay

        Time.Fri ->
            computeDays -4 firstDay

        Time.Sat ->
            computeDays -5 firstDay

        Time.Sun ->
            computeDays -6 firstDay


calLastDay : Time.Posix -> Time.Posix
calLastDay firstDay =
    let
        lastDay =
            firstDay
                |> Time.toYear Time.utc
                |> String.fromInt
                |> (\year -> year ++ "-12-31")
                |> Iso8601.toTime
                |> Result.withDefault (Time.millisToPosix 0)
    in
    case Time.toWeekday Time.utc lastDay of
        Time.Mon ->
            computeDays 6 lastDay

        Time.Tue ->
            computeDays 5 lastDay

        Time.Wed ->
            computeDays 4 lastDay

        Time.Thu ->
            computeDays 3 lastDay

        Time.Fri ->
            computeDays 2 lastDay

        Time.Sat ->
            computeDays 1 lastDay

        Time.Sun ->
            lastDay


computeDays : Int -> Time.Posix -> Time.Posix
computeDays days time =
    (Time.posixToMillis time + days * dayLength)
        |> Time.millisToPosix


calDays : Time.Posix -> List Time.Posix
calDays firstDay =
    let
        cfd =
            calFirstDay firstDay

        cld =
            calLastDay firstDay
    in
    (toFloat (Time.posixToMillis cld) - toFloat (Time.posixToMillis cfd))
        / toFloat dayLength
        |> floor
        |> List.range 0
        |> List.map (\i -> computeDays i cfd)


splitByWeek : List Time.Posix -> List (List Time.Posix)
splitByWeek days =
    let
        rest =
            List.drop 7 days
    in
    List.take 7 days
        :: (if List.isEmpty rest then
                []

            else
                splitByWeek rest
           )


type Msg
    = GotNow Time.Posix
    | GotInput String
    | SetLang I18n.Language


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotNow time ->
            let
                currentYear =
                    Time.toYear Time.utc time

                month =
                    Time.toMonth Time.utc time

                year =
                    if
                        List.member month
                            [ Time.Jan
                            , Time.Feb
                            , Time.Mar
                            , Time.Apr
                            , Time.May
                            , Time.Jun
                            ]
                    then
                        currentYear

                    else
                        currentYear + 1
            in
            update
                (SetLang model.language)
                { model | firstDay = getFirstDayOfTheYear year }

        GotInput value ->
            ( { model
                | firstDay =
                    value
                        |> String.toFloat
                        |> Maybe.withDefault 1970
                        |> floor
                        |> getFirstDayOfTheYear
              }
            , Cmd.none
            )

        SetLang lang ->
            ( { model
                | language = lang
                , monthFormater = I18n.getMonthFormater lang
                , dayFormater = I18n.getDayFormater lang
              }
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view model =
    { title = "Calendar"
    , body =
        [ div [ class "no-print container nav" ]
            [ input
                [ value
                    (model.firstDay
                        |> Time.toYear Time.utc
                        |> String.fromInt
                    )
                , type_ "number"
                , onInput GotInput
                ]
                []
            , select [ onInput (\lang -> SetLang (I18n.parseLang lang)) ]
                (I18n.availableLanguages
                    |> List.map
                        (\lang ->
                            option
                                [ value (I18n.shortCode lang)
                                , selected (model.language == lang)
                                ]
                                [ text (I18n.toString lang) ]
                        )
                )
            , a
                [ href "https://www.buymeacoffee.com/steevec"
                , target "_blank"
                ]
                [ img
                    [ src "https://cdn.buymeacoffee.com/buttons/v2/default-yellow.png"
                    , alt "Buy Me A Coffee"
                    , style "height" "60px"
                    , style "width" "217px"
                    ]
                    []
                ]
            ]
        , div [ class "container" ]
            [ div [ class "page front-page" ]
                [ div []
                    [ model.firstDay
                        |> Time.toYear Time.utc
                        |> String.fromInt
                        |> text
                    ]
                ]
            ]
        , div [ class "pagebreak" ] []
        , model.firstDay
            |> calDays
            |> splitByWeek
            |> List.map
                (\week ->
                    let
                        leftFirstDay =
                            week
                                |> List.head
                                |> Maybe.withDefault (Time.millisToPosix 0)

                        rightFirstDay =
                            week
                                |> List.drop 3
                                |> List.head
                                |> Maybe.withDefault (Time.millisToPosix 0)
                    in
                    div []
                        [ div [ class "page" ]
                            ([ viewHeader model leftFirstDay
                             , hr [] []
                             , div [ class "cell" ] [ div [ class "notes" ] [ text "Notes" ] ]
                             , hr [] []
                             ]
                                ++ viewDays model (List.take 3 week)
                            )
                        , div [ class "pagebreak" ] []
                        , div [ class "page" ]
                            ([ viewHeader model rightFirstDay
                             , hr [] []
                             ]
                                ++ viewDays model (List.drop 3 week)
                            )
                        , div [ class "pagebreak" ] []
                        ]
                )
            |> div [ class "container" ]
        ]
    }


viewHeader : Model -> Time.Posix -> Html Msg
viewHeader model day =
    div [ class "page-header" ]
        [ div [ class "month" ]
            [ day
                |> Time.toMonth Time.utc
                |> model.monthFormater
                |> text
            ]
        , div [ class "month" ]
            [ day
                |> Time.toYear Time.utc
                |> String.fromInt
                |> text
            ]
        ]


viewDays : Model -> List Time.Posix -> List (Html Msg)
viewDays model days =
    days
        |> List.map
            (\d ->
                div [ class "cell" ]
                    [ div [ class "day" ]
                        [ div [ class "name" ]
                            [ d
                                |> Time.toWeekday Time.utc
                                |> model.dayFormater
                                |> text
                            ]
                        , div [ class "number" ]
                            [ d
                                |> Time.toDay Time.utc
                                |> String.fromInt
                                |> text
                            ]
                        ]
                    , div [ class "comments" ] []
                    ]
            )
        |> List.intersperse (hr [] [])
