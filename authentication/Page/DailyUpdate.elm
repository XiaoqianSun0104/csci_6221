module Page.DailyUpdate exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import User exposing (User, userDecoder, userEncoder)
import Weather exposing (Weather, weatherDecoder)
import RemoteData exposing (WebData)
import Route
import Html.Attributes exposing (type_, checked, value)
import Date exposing (..)
import Time
import Task exposing (Task)
import Json.Encode

type alias Model =
    { navKey : Nav.Key
    , weather : WebData Weather
    , userId: String
    , user: WebData User
    , date: Time.Posix
    , saveError : Maybe String
    }


init : String -> String -> Nav.Key -> ( Model, Cmd Msg )
init userId zip navKey =
    ( initialModel userId navKey, fetchWeather userId zip)


initialModel : String -> Nav.Key -> Model
initialModel userId navKey =
    { navKey = navKey
    , weather = RemoteData.Loading
    , userId = userId
    , user = RemoteData.Loading
    , date = (Time.millisToPosix 0)
    , saveError = Nothing
    }


fetchWeather : String -> String -> Cmd Msg
fetchWeather userId zip =
    Http.get
        { url = "http://api.weatherapi.com/v1/current.json?key=818ba1f50d2a4010a8c163025212606&aqi=yes&q="++(zip)
        , expect =
            weatherDecoder
                |> Http.expectJson (RemoteData.fromResult >> WeatherDataReceived)
        }

fetchUser : String -> Cmd Msg
fetchUser userId =
    Http.get
        { url = "http://34.195.151.200:5019/users/" ++ userId
        , expect =
            userDecoder
                |> Http.expectJson (RemoteData.fromResult >> UserDataReceived)
        }


type Msg
    = WeatherDataReceived (WebData Weather)
    | UserDataReceived (WebData User)
    | GoResult
    | GoDiary

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WeatherDataReceived weather ->
            ( { model | weather = weather }, Cmd.none )
        UserDataReceived user ->
            ( { model | user = user }, Cmd.none )
        GoResult ->
            ( model, Nav.pushUrl model.navKey <| "/users/result/"++model.userId )
        GoDiary ->
            ( model, Nav.pushUrl model.navKey <| "/users/diary/"++model.userId )

view : Model -> Html Msg
view model =
    div [class "container"]
        [
        viewPost model.weather
        ]


viewPost : WebData Weather -> Html Msg
viewPost weather =
    case weather of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading User Data..." ]

        RemoteData.Success weatherData ->
            dailyDiv weatherData

        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


dailyDiv : Weather -> Html Msg
dailyDiv weather =
    div [class "weatherContent"][

        h3 [class "weatherTitle"][
            text ("Today is "++(weather.current.condition.text))
            , img [ class "vertical-mid", src weather.current.condition.icon ] []
            ], 
        div []
            [ 
                div [class "leftColumn"] [
                    div [class "weatherBlock"][h5 [] [text "Location"], span [][text (weather.location.name++", "++weather.location.region)]]
                    , div [class "weatherBlock"][h5 [] [text "UV Index"], span [][text (String.fromFloat weather.current.uv++" of 10")]
                        , p [][text "UV Index is high today. We suggest you wear sunscreen with at least SPF 30 and avoid UV rays outside which are at their peak between 10 a.m. and 4 p.m."]]
                    , div [class "weatherBlock"][h5 [] [text "Air Quality Index"], span [][text (String.fromInt weather.current.air_quality.usepaindex)]
                        ,p [] [text "Air quality is satisfactory, and air pollution poses little or no risk. Enjoy the weather"]]
                ],
                div [class "rightColumn"] [
                    div [class "weatherBlock"][h5 [] [text "Temperature"], span [][text (String.fromFloat weather.current.temp_f ++" F")]]
                    , div [class "weatherBlock"][h5 [] [text "Humidity"], span [][text (String.fromInt weather.current.humidity++"%")] 
                        ,p [][text "Humidity today is high. We suggest wear extra layer, stay warm, turn on dehumidifier and work out a little bit. Check below for today’s exercise."]]
                ]
                , div [class "weatherBlock recipe"][h5 [] [text "Workout Choices"], span [][]
                        ,p [] [text "Regarding today’s  workout session, we suggest  Low-impact yoga or Ti chi. Check below videos for inspiration."]
                        , iframe [ class "workoutVideo", src "https://www.youtube.com/embed/g13nVd7OLYs", property "frameborder" (Json.Encode.string "0")][]]
                , div [class "weatherBlock recipe"][
                    h5 [][text "Yummy&Healthy Recipe"]
                    , p [] [text"Eat well is important to keep energetic and healthy, especially for Lupus groups. Let check out today’s Recipe."]
                    , h6 [] [text "BERRY-BANANA SMOOTHIE"]
                    , p [][text "This delicious smoothie is great for lupus patients experiencing fatigue but will also help prevent bone loss and cardiovascular disease. "]
                    , p [] [text "PREP: 10 minutes"]
                    , p [] [text "MAKES: 2 servings (about a cup each)"]
                    , p [] [text "Ingredients:"]
                    , p [class "listItem"] [text "- 1 cup of low-fat yogurt (any flavor)"]
                    , p [class "listItem"] [text "- 1/2 cup of round oat cereal (i.e. Cheerios)"]
                    , p [class "listItem"] [text "- 2 tablespoons ground flax-seed or flax-seed meal"]
                    , p [class "listItem"] [text "- 1/2 cup fresh strawberry halves or raspberries, or frozen whole strawberries"]
                    , p [class "listItem"] [text "- 1/2 cup skim milk or vanilla Rice Drink (non-dairy beverage)"]
                    , p [class ""] [text "- 1/2 banana"]
                    , p [] [text "Place all ingredients in blender.  Cover and blend on high speed 10 seconds; stop blender to scrape sides.  Cover and blend about 20 seconds longer or until smooth. Pour mixture into glasses.  Serve immediately."]
                    , p [class "listItem"] [ text "Nutrition Facts Per Serving:"]
                    ,p [class "listItem"] [text "Calories 245; Fat 4g; Saturated Fat 1g; Trans Fat 0g; Cholesterol 0mg; Sodium 125mg"]
                    ]
                , button [ type_ "button", onClick GoResult, class "backBtn" ]
                    [ text "Back to Report" ]
                , button [ type_ "button", onClick GoDiary, class "goDiaryBtn" ]
                    [ text "Go to Diary" ]
                , p[][text"df"]
            ]
    ]
    
        


viewFetchError : String -> Html Msg
viewFetchError errorMessage =
    let
        errorHeading =
            "Couldn't fetch weather info at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]
