module Page.LifeLog exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import User exposing (User, userDecoder, userEncoder)
import RemoteData exposing (WebData)
import Route
import Html.Attributes exposing (type_, checked, value)

type alias Model =
    { navKey : Nav.Key
    , user : WebData User
    , saveError : Maybe String
    }


init : String -> Nav.Key -> ( Model, Cmd Msg )
init userId navKey =
    ( initialModel navKey, fetchPost userId )


initialModel : Nav.Key -> Model
initialModel navKey =
    { navKey = navKey
    , user = RemoteData.Loading
    , saveError = Nothing
    }


fetchPost : String -> Cmd Msg
fetchPost userId =
    Http.get
        { url = "http://34.195.151.200:5019/users/" ++ userId
        , expect =
            userDecoder
                |> Http.expectJson (RemoteData.fromResult >> UserDataReceived)
        }


type Msg
    = UserDataReceived (WebData User)
    | UpdateLevel String Int
    | SaveLevels
    | LevelsSaved (Result Http.Error User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserDataReceived user ->
            ( { model | user = user }, Cmd.none )

        UpdateLevel symptom level->
            let
                updateSymptom = 
                    RemoteData.map
                        (\userData ->
                            case symptom of
                                "jointPain" ->
                                    {userData | jointPainNum = level }
                                "jointSwelling" ->
                                    {userData | jointSwellingNum = level }
                                "fatigue" ->
                                    {userData | fatigueNum = level }
                                "musclePain" ->
                                    {userData | musclePainNum = level }
                                "fever" ->
                                    {userData | feverNum = level }
                                "rash" ->
                                    {userData | rashNum = level }
                                "chestPain" ->
                                    {userData | chestPainNum = level }
                                "hairLoss" ->
                                    {userData | hairLossNum = level }
                                "sunlightSensitivity" ->
                                    {userData | sunlightSensitivityNum = level}
                                "kidney" ->
                                    {userData | kidneyNum = level }
                                "mouthSores" ->
                                    {userData | mouthSoresNum = level }
                                "anemia" ->
                                    {userData | anemiaNum = level }
                                "dryEyes" ->
                                    {userData | dryEyesNum = level }
                                "eyeInflammation" ->
                                    {userData | eyeInflammationNum = level }
                                "eyelidRashes" ->
                                    {userData | eyelidRashesNum = level }
                                "otherSymptom" ->
                                    {userData | otherSymptomNum = level }
                                _ ->
                                    {userData | jointPainNum = level }
                        )
                        model.user
            in
            ( { model | user = updateSymptom }, Cmd.none )
        SaveLevels ->
            ( model, saveLevels model.user )

        LevelsSaved (Ok userData) ->
            let
                user =
                    RemoteData.succeed userData
            in
            ( { model | user = user, saveError = Nothing }
            , Nav.pushUrl model.navKey <| "/users/dailyupdate/"++userData.id++"/"++(String.fromInt userData.zip)
            )

        LevelsSaved (Err error) ->
            ( { model | saveError = Just (buildErrorMessage error) }
            , Cmd.none
            )


saveLevels : WebData User -> Cmd Msg
saveLevels user =
    case user of
        RemoteData.Success userData ->
            let
                postUrl =
                    "http://34.195.151.200:5019/users/"
                        ++ userData.id
            in
            Http.request
                { method = "PATCH"
                , headers = []
                , url = postUrl
                , body = Http.jsonBody (userEncoder userData)
                , expect = Http.expectJson LevelsSaved userDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

        _ ->
            Cmd.none

view : Model -> Html Msg
view model =
    div [class "container"]
        [ h3 [class "lifelogTitle"] [ text "My Lupus Lifelog" ]
        , viewPost model.user
        , viewSaveError model.saveError
        ]


viewPost : WebData User -> Html Msg
viewPost user =
    case user of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading User Data..." ]

        RemoteData.Success userData ->
            editForm userData

        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


editForm : User -> Html Msg
editForm user =
    Html.form []
        [ jointPainView user
        , jointSwellingView user
        , fatigueView user
        , musclePainView user
        , feverView user
        , rashView user
        , chestPainView user
        , hairLossView user
        , sunlightSensitivityView user
        , kidneyView user
        , mouthSoresView user
        , anemiaView user
        , dryEyesView user
        , eyeInflammationView user
        , eyelidRashesView user
        , otherView user
        , div []
            [ button [ class "lifelogBtn", type_ "button", onClick SaveLevels ]
                [ text "Submit" ]
            ]
        ]


viewFetchError : String -> Html Msg
viewFetchError errorMessage =
    let
        errorHeading =
            "Couldn't fetch user at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewSaveError : Maybe String -> Html msg
viewSaveError maybeError =
    case maybeError of
        Just error ->
            div []
                [ h3 [] [ text "Couldn't save user at this time." ]
                , text ("Error: " ++ error)
                ]

        Nothing ->
            text ""

jointPainView: User -> Html Msg
jointPainView user = 
    if user.jointPain then
        div [ class "logRow"]
            [ 
                h5 [] [text "Joint Pain:"]
                , span [] [
                    text "1"
                    , input [ type_ "radio", name "jointPainNum", onClick <| UpdateLevel "jointPain" 1 ] []
                ]
                ,span [] [text "2"
                    , input [ type_ "radio", name "jointPainNum", onClick <| UpdateLevel "jointPain" 2 ] []
                ]
                ,span [] [text "3"
                    , input [ type_ "radio", name "jointPainNum", onClick <| UpdateLevel "jointPain" 3 ] []
                ]
                ,span [] [text "4"
                    , input [ type_ "radio", name "jointPainNum", onClick <| UpdateLevel "jointPain" 4 ] []
                ]
                ,span [] [text "5"
                    , input [ type_ "radio", name "jointPainNum", onClick <| UpdateLevel "jointPain" 5 ] []
                ]
                ,span [] [text "6"
                    , input [ type_ "radio", name "jointPainNum", onClick <| UpdateLevel "jointPain" 6 ] []
                ]
                ,span [] [text "7"
                    , input [ type_ "radio", name "jointPainNum", onClick <| UpdateLevel "jointPain" 7 ] []
                ]
                ,span [] [text "8"
                    , input [ type_ "radio", name "jointPainNum", onClick <| UpdateLevel "jointPain" 8 ] []
                ]
                ,span [] [text "9"
                    , input [ type_ "radio", name "jointPainNum", onClick <| UpdateLevel "jointPain" 9 ] []
                ]
                ,span [] [text "10"
                    , input [ type_ "radio", name "jointPainNum", onClick <| UpdateLevel "jointPain" 10 ] []
                ]
            ]
    else 
        text ""

jointSwellingView: User -> Html Msg
jointSwellingView user = 
    if user.jointSwelling then
        div [ class "logRow"]
            [ 
                h5 [] [text "Joints Swelling:"]
                , span [] [
                    text "1"
                    , input [ type_ "radio", name "jointSwellingNum", onClick <| UpdateLevel "jointSwelling" 1 ] []
                ]
                ,span [] [text "2"
                    , input [ type_ "radio", name "jointSwellingNum", onClick <| UpdateLevel "jointSwelling" 2 ] []
                ]
                ,span [] [text "3"
                    , input [ type_ "radio", name "jointSwellingNum", onClick <| UpdateLevel "jointSwelling" 3 ] []
                ]
                ,span [] [text "4"
                    , input [ type_ "radio", name "jointSwellingNum", onClick <| UpdateLevel "jointSwelling" 4 ] []
                ]
                ,span [] [text "5"
                    , input [ type_ "radio", name "jointSwellingNum", onClick <| UpdateLevel "jointSwelling" 5 ] []
                ]
                ,span [] [text "6"
                    , input [ type_ "radio", name "jointSwellingNum", onClick <| UpdateLevel "jointSwelling" 6 ] []
                ]
                ,span [] [text "7"
                    , input [ type_ "radio", name "jointSwellingNum", onClick <| UpdateLevel "jointSwelling" 7 ] []
                ]
                ,span [] [text "8"
                    , input [ type_ "radio", name "jointSwellingNum", onClick <| UpdateLevel "jointSwelling" 8 ] []
                ]
                ,span [] [text "9"
                    , input [ type_ "radio", name "jointSwellingNum", onClick <| UpdateLevel "jointSwelling" 9 ] []
                ]
                ,span [] [text "10"
                    , input [ type_ "radio", name "jointSwellingNum", onClick <| UpdateLevel "jointSwelling" 10 ] []
                ]
            ]
    else 
        text ""

fatigueView: User -> Html Msg
fatigueView user = 
    if user.fatigue then
        div [ class "logRow"]
            [ 
                h5 [] [text "Fatigue:"]
                , span [] [
                    text "1"
                    , input [ type_ "radio", name "fatigueNum", onClick <| UpdateLevel "fatigue" 1 ] []
                ]
                ,span [] [text "2"
                    , input [ type_ "radio", name "fatigueNum", onClick <| UpdateLevel "fatigue" 2 ] []
                ]
                ,span [] [text "3"
                    , input [ type_ "radio", name "fatigueNum", onClick <| UpdateLevel "fatigue" 3 ] []
                ]
                ,span [] [text "4"
                    , input [ type_ "radio", name "fatigueNum", onClick <| UpdateLevel "fatigue" 4 ] []
                ]
                ,span [] [text "5"
                    , input [ type_ "radio", name "fatigueNum", onClick <| UpdateLevel "fatigue" 5 ] []
                ]
                ,span [] [text "6"
                    , input [ type_ "radio", name "fatigueNum", onClick <| UpdateLevel "fatigue" 6 ] []
                ]
                ,span [] [text "7"
                    , input [ type_ "radio", name "fatigueNum", onClick <| UpdateLevel "fatigue" 7 ] []
                ]
                ,span [] [text "8"
                    , input [ type_ "radio", name "fatigueNum", onClick <| UpdateLevel "fatigue" 8 ] []
                ]
                ,span [] [text "9"
                    , input [ type_ "radio", name "fatigueNum", onClick <| UpdateLevel "fatigue" 9 ] []
                ]
                ,span [] [text "10"
                    , input [ type_ "radio", name "fatigueNum", onClick <| UpdateLevel "fatigue" 10 ] []
                ]
            ]
    else 
        text ""

musclePainView: User -> Html Msg
musclePainView user = 
    if user.musclePain then
        div [ class "logRow"]
            [ 
                h5 [] [text "Muscle Pain:"]
                , span [] [
                    text "1"
                    , input [ type_ "radio", name "musclePainNum", onClick <| UpdateLevel "musclePain" 1 ] []
                ]
                ,span [] [text "2"
                    , input [ type_ "radio", name "musclePainNum", onClick <| UpdateLevel "musclePain" 2 ] []
                ]
                ,span [] [text "3"
                    , input [ type_ "radio", name "musclePainNum", onClick <| UpdateLevel "musclePain" 3 ] []
                ]
                ,span [] [text "4"
                    , input [ type_ "radio", name "musclePainNum", onClick <| UpdateLevel "musclePain" 4 ] []
                ]
                ,span [] [text "5"
                    , input [ type_ "radio", name "musclePainNum", onClick <| UpdateLevel "musclePain" 5 ] []
                ]
                ,span [] [text "6"
                    , input [ type_ "radio", name "musclePainNum", onClick <| UpdateLevel "musclePain" 6 ] []
                ]
                ,span [] [text "7"
                    , input [ type_ "radio", name "musclePainNum", onClick <| UpdateLevel "musclePain" 7 ] []
                ]
                ,span [] [text "8"
                    , input [ type_ "radio", name "musclePainNum", onClick <| UpdateLevel "musclePain" 8 ] []
                ]
                ,span [] [text "9"
                    , input [ type_ "radio", name "musclePainNum", onClick <| UpdateLevel "musclePain" 9 ] []
                ]
                ,span [] [text "10"
                    , input [ type_ "radio", name "musclePainNum", onClick <| UpdateLevel "musclePain" 10 ] []
                ]
            ]
    else 
        text ""

feverView: User -> Html Msg
feverView user = 
    if user.fever then
        div [ class "logRow"]
            [ 
                h5 [] [text "Fever:"]
                , span [] [
                    text "1"
                    , input [ type_ "radio", name "feverNum", onClick <| UpdateLevel "fever" 1 ] []
                ]
                ,span [] [text "2"
                    , input [ type_ "radio", name "feverNum", onClick <| UpdateLevel "fever" 2 ] []
                ]
                ,span [] [text "3"
                    , input [ type_ "radio", name "feverNum", onClick <| UpdateLevel "fever" 3 ] []
                ]
                ,span [] [text "4"
                    , input [ type_ "radio", name "feverNum", onClick <| UpdateLevel "fever" 4 ] []
                ]
                ,span [] [text "5"
                    , input [ type_ "radio", name "feverNum", onClick <| UpdateLevel "fever" 5 ] []
                ]
                ,span [] [text "6"
                    , input [ type_ "radio", name "feverNum", onClick <| UpdateLevel "fever" 6 ] []
                ]
                ,span [] [text "7"
                    , input [ type_ "radio", name "feverNum", onClick <| UpdateLevel "fever" 7 ] []
                ]
                ,span [] [text "8"
                    , input [ type_ "radio", name "feverNum", onClick <| UpdateLevel "fever" 8 ] []
                ]
                ,span [] [text "9"
                    , input [ type_ "radio", name "feverNum", onClick <| UpdateLevel "fever" 9 ] []
                ]
                ,span [] [text "10"
                    , input [ type_ "radio", name "feverNum", onClick <| UpdateLevel "fever" 10 ] []
                ]
            ]
    else 
        text ""

rashView: User -> Html Msg
rashView user = 
    if user.rash then
        div [ class "logRow"]
            [ 
                h5 [] [text "Rash:"]
                , span [] [
                    text "1"
                    , input [ type_ "radio", name "rashNum", onClick <| UpdateLevel "rash" 1 ] []
                ]
                ,span [] [text "2"
                    , input [ type_ "radio", name "rashNum", onClick <| UpdateLevel "rash" 2 ] []
                ]
                ,span [] [text "3"
                    , input [ type_ "radio", name "rashNum", onClick <| UpdateLevel "rash" 3 ] []
                ]
                ,span [] [text "4"
                    , input [ type_ "radio", name "rashNum", onClick <| UpdateLevel "rash" 4 ] []
                ]
                ,span [] [text "5"
                    , input [ type_ "radio", name "rashNum", onClick <| UpdateLevel "rash" 5 ] []
                ]
                ,span [] [text "6"
                    , input [ type_ "radio", name "rashNum", onClick <| UpdateLevel "rash" 6 ] []
                ]
                ,span [] [text "7"
                    , input [ type_ "radio", name "rashNum", onClick <| UpdateLevel "rash" 7 ] []
                ]
                ,span [] [text "8"
                    , input [ type_ "radio", name "rashNum", onClick <| UpdateLevel "rash" 8 ] []
                ]
                ,span [] [text "9"
                    , input [ type_ "radio", name "rashNum", onClick <| UpdateLevel "rash" 9 ] []
                ]
                ,span [] [text "10"
                    , input [ type_ "radio", name "rashNum", onClick <| UpdateLevel "rash" 10 ] []
                ]
            ]
    else 
        text ""

chestPainView: User -> Html Msg
chestPainView user = 
    if user.chestPain then
        div [ class "logRow"]
            [ 
                h5 [] [text "Chest Pain:"]
                , span [] [
                    text "1"
                    , input [ type_ "radio", name "chestPainNum", onClick <| UpdateLevel "chestPain" 1 ] []
                ]
                ,span [] [text "2"
                    , input [ type_ "radio", name "chestPainNum", onClick <| UpdateLevel "chestPain" 2 ] []
                ]
                ,span [] [text "3"
                    , input [ type_ "radio", name "chestPainNum", onClick <| UpdateLevel "chestPain" 3 ] []
                ]
                ,span [] [text "4"
                    , input [ type_ "radio", name "chestPainNum", onClick <| UpdateLevel "chestPain" 4 ] []
                ]
                ,span [] [text "5"
                    , input [ type_ "radio", name "chestPainNum", onClick <| UpdateLevel "chestPain" 5 ] []
                ]
                ,span [] [text "6"
                    , input [ type_ "radio", name "chestPainNum", onClick <| UpdateLevel "chestPain" 6 ] []
                ]
                ,span [] [text "7"
                    , input [ type_ "radio", name "chestPainNum", onClick <| UpdateLevel "chestPain" 7 ] []
                ]
                ,span [] [text "8"
                    , input [ type_ "radio", name "chestPainNum", onClick <| UpdateLevel "chestPain" 8 ] []
                ]
                ,span [] [text "9"
                    , input [ type_ "radio", name "chestPainNum", onClick <| UpdateLevel "chestPain" 9 ] []
                ]
                ,span [] [text "10"
                    , input [ type_ "radio", name "chestPainNum", onClick <| UpdateLevel "chestPain" 10 ] []
                ]
            ]
    else 
        text ""

hairLossView: User -> Html Msg
hairLossView user = 
    if user.hairLoss then
        div [ class "logRow"]
            [ 
                h5 [] [text "Hair Loss:"]
                , span [] [
                    text "1"
                    , input [ type_ "radio", name "hairLossNum", onClick <| UpdateLevel "hairLoss" 1 ] []
                ]
                ,span [] [text "2"
                    , input [ type_ "radio", name "hairLossNum", onClick <| UpdateLevel "hairLoss" 2 ] []
                ]
                ,span [] [text "3"
                    , input [ type_ "radio", name "hairLossNum", onClick <| UpdateLevel "hairLoss" 3 ] []
                ]
                ,span [] [text "4"
                    , input [ type_ "radio", name "hairLossNum", onClick <| UpdateLevel "hairLoss" 4 ] []
                ]
                ,span [] [text "5"
                    , input [ type_ "radio", name "hairLossNum", onClick <| UpdateLevel "hairLoss" 5 ] []
                ]
                ,span [] [text "6"
                    , input [ type_ "radio", name "hairLossNum", onClick <| UpdateLevel "hairLoss" 6 ] []
                ]
                ,span [] [text "7"
                    , input [ type_ "radio", name "hairLossNum", onClick <| UpdateLevel "hairLoss" 7 ] []
                ]
                ,span [] [text "8"
                    , input [ type_ "radio", name "hairLossNum", onClick <| UpdateLevel "hairLoss" 8 ] []
                ]
                ,span [] [text "9"
                    , input [ type_ "radio", name "hairLossNum", onClick <| UpdateLevel "hairLoss" 9 ] []
                ]
                ,span [] [text "10"
                    , input [ type_ "radio", name "hairLossNum", onClick <| UpdateLevel "hairLoss" 10 ] []
                ]
            ]
    else 
        text ""

sunlightSensitivityView: User -> Html Msg
sunlightSensitivityView user = 
    if user.sunlightSensitivity then
        div [ class "logRow"]
            [ 
                h5 [] [text "Sunlight Sensitivity:"]
                , span [] [
                    text "1"
                    , input [ type_ "radio", name "sunlightSensitivityNum", onClick <| UpdateLevel "sunlightSensitivity" 1 ] []
                ]
                ,span [] [text "2"
                    , input [ type_ "radio", name "sunlightSensitivityNum", onClick <| UpdateLevel "sunlightSensitivity" 2 ] []
                ]
                ,span [] [text "3"
                    , input [ type_ "radio", name "sunlightSensitivityNum", onClick <| UpdateLevel "sunlightSensitivity" 3 ] []
                ]
                ,span [] [text "4"
                    , input [ type_ "radio", name "sunlightSensitivityNum", onClick <| UpdateLevel "sunlightSensitivity" 4 ] []
                ]
                ,span [] [text "5"
                    , input [ type_ "radio", name "sunlightSensitivityNum", onClick <| UpdateLevel "sunlightSensitivity" 5 ] []
                ]
                ,span [] [text "6"
                    , input [ type_ "radio", name "sunlightSensitivityNum", onClick <| UpdateLevel "sunlightSensitivity" 6 ] []
                ]
                ,span [] [text "7"
                    , input [ type_ "radio", name "sunlightSensitivityNum", onClick <| UpdateLevel "sunlightSensitivity" 7 ] []
                ]
                ,span [] [text "8"
                    , input [ type_ "radio", name "sunlightSensitivityNum", onClick <| UpdateLevel "sunlightSensitivity" 8 ] []
                ]
                ,span [] [text "9"
                    , input [ type_ "radio", name "sunlightSensitivityNum", onClick <| UpdateLevel "sunlightSensitivity" 9 ] []
                ]
                ,span [] [text "10"
                    , input [ type_ "radio", name "sunlightSensitivityNum", onClick <| UpdateLevel "sunlightSensitivity" 10 ] []
                ]
            ]
    else 
        text ""

kidneyView: User -> Html Msg
kidneyView user = 
    if user.kidney then
        div [ class "logRow"]
            [ 
                h5 [] [text "Kidney:"]
                , span [] [
                    text "1"
                    , input [ type_ "radio", name "kidneyNum", onClick <| UpdateLevel "kidney" 1 ] []
                ]
                ,span [] [text "2"
                    , input [ type_ "radio", name "kidneyNum", onClick <| UpdateLevel "kidney" 2 ] []
                ]
                ,span [] [text "3"
                    , input [ type_ "radio", name "kidneyNum", onClick <| UpdateLevel "kidney" 3 ] []
                ]
                ,span [] [text "4"
                    , input [ type_ "radio", name "kidneyNum", onClick <| UpdateLevel "kidney" 4 ] []
                ]
                ,span [] [text "5"
                    , input [ type_ "radio", name "kidneyNum", onClick <| UpdateLevel "kidney" 5 ] []
                ]
                ,span [] [text "6"
                    , input [ type_ "radio", name "kidneyNum", onClick <| UpdateLevel "kidney" 6 ] []
                ]
                ,span [] [text "7"
                    , input [ type_ "radio", name "kidneyNum", onClick <| UpdateLevel "kidney" 7 ] []
                ]
                ,span [] [text "8"
                    , input [ type_ "radio", name "kidneyNum", onClick <| UpdateLevel "kidney" 8 ] []
                ]
                ,span [] [text "9"
                    , input [ type_ "radio", name "kidneyNum", onClick <| UpdateLevel "kidney" 9 ] []
                ]
                ,span [] [text "10"
                    , input [ type_ "radio", name "kidneyNum", onClick <| UpdateLevel "kidney" 10 ] []
                ]
            ]
    else 
        text ""

mouthSoresView: User -> Html Msg
mouthSoresView user = 
    if user.mouthSores then
        div [ class "logRow"]
            [ 
                h5 [] [text "Mouth Sores:"]
                , span [] [
                    text "1"
                    , input [ type_ "radio", name "mouthSoresNum", onClick <| UpdateLevel "mouthSores" 1 ] []
                ]
                ,span [] [text "2"
                    , input [ type_ "radio", name "mouthSoresNum", onClick <| UpdateLevel "mouthSores" 2 ] []
                ]
                ,span [] [text "3"
                    , input [ type_ "radio", name "mouthSoresNum", onClick <| UpdateLevel "mouthSores" 3 ] []
                ]
                ,span [] [text "4"
                    , input [ type_ "radio", name "mouthSoresNum", onClick <| UpdateLevel "mouthSores" 4 ] []
                ]
                ,span [] [text "5"
                    , input [ type_ "radio", name "mouthSoresNum", onClick <| UpdateLevel "mouthSores" 5 ] []
                ]
                ,span [] [text "6"
                    , input [ type_ "radio", name "mouthSoresNum", onClick <| UpdateLevel "mouthSores" 6 ] []
                ]
                ,span [] [text "7"
                    , input [ type_ "radio", name "mouthSoresNum", onClick <| UpdateLevel "mouthSores" 7 ] []
                ]
                ,span [] [text "8"
                    , input [ type_ "radio", name "mouthSoresNum", onClick <| UpdateLevel "mouthSores" 8 ] []
                ]
                ,span [] [text "9"
                    , input [ type_ "radio", name "mouthSoresNum", onClick <| UpdateLevel "mouthSores" 9 ] []
                ]
                ,span [] [text "10"
                    , input [ type_ "radio", name "mouthSoresNum", onClick <| UpdateLevel "mouthSores" 10 ] []
                ]
            ]
    else 
        text ""

anemiaView: User -> Html Msg
anemiaView user = 
    if user.anemia then
        div [ class "logRow"]
            [ 
                h5 [] [text "Anemia:"]
                , span [] [
                    text "1"
                    , input [ type_ "radio", name "anemiaNum", onClick <| UpdateLevel "anemia" 1 ] []
                ]
                ,span [] [text "2"
                    , input [ type_ "radio", name "anemiaNum", onClick <| UpdateLevel "anemia" 2 ] []
                ]
                ,span [] [text "3"
                    , input [ type_ "radio", name "anemiaNum", onClick <| UpdateLevel "anemia" 3 ] []
                ]
                ,span [] [text "4"
                    , input [ type_ "radio", name "anemiaNum", onClick <| UpdateLevel "anemia" 4 ] []
                ]
                ,span [] [text "5"
                    , input [ type_ "radio", name "anemiaNum", onClick <| UpdateLevel "anemia" 5 ] []
                ]
                ,span [] [text "6"
                    , input [ type_ "radio", name "anemiaNum", onClick <| UpdateLevel "anemia" 6 ] []
                ]
                ,span [] [text "7"
                    , input [ type_ "radio", name "anemiaNum", onClick <| UpdateLevel "anemia" 7 ] []
                ]
                ,span [] [text "8"
                    , input [ type_ "radio", name "anemiaNum", onClick <| UpdateLevel "anemia" 8 ] []
                ]
                ,span [] [text "9"
                    , input [ type_ "radio", name "anemiaNum", onClick <| UpdateLevel "anemia" 9 ] []
                ]
                ,span [] [text "10"
                    , input [ type_ "radio", name "anemiaNum", onClick <| UpdateLevel "anemia" 10 ] []
                ]
            ]
    else 
        text ""

dryEyesView: User -> Html Msg
dryEyesView user = 
    if user.dryEyes then
        div [ class "logRow"]
            [ 
                h5 [] [text "Dry Eyes:"]
                , span [] [
                    text "1"
                    , input [ type_ "radio", name "dryEyesNum", onClick <| UpdateLevel "dryEyes" 1 ] []
                ]
                ,span [] [text "2"
                    , input [ type_ "radio", name "dryEyesNum", onClick <| UpdateLevel "dryEyes" 2 ] []
                ]
                ,span [] [text "3"
                    , input [ type_ "radio", name "dryEyesNum", onClick <| UpdateLevel "dryEyes" 3 ] []
                ]
                ,span [] [text "4"
                    , input [ type_ "radio", name "dryEyesNum", onClick <| UpdateLevel "dryEyes" 4 ] []
                ]
                ,span [] [text "5"
                    , input [ type_ "radio", name "dryEyesNum", onClick <| UpdateLevel "dryEyes" 5 ] []
                ]
                ,span [] [text "6"
                    , input [ type_ "radio", name "dryEyesNum", onClick <| UpdateLevel "dryEyes" 6 ] []
                ]
                ,span [] [text "7"
                    , input [ type_ "radio", name "dryEyesNum", onClick <| UpdateLevel "dryEyes" 7 ] []
                ]
                ,span [] [text "8"
                    , input [ type_ "radio", name "dryEyesNum", onClick <| UpdateLevel "dryEyes" 8 ] []
                ]
                ,span [] [text "9"
                    , input [ type_ "radio", name "dryEyesNum", onClick <| UpdateLevel "dryEyes" 9 ] []
                ]
                ,span [] [text "10"
                    , input [ type_ "radio", name "dryEyesNum", onClick <| UpdateLevel "dryEyes" 10 ] []
                ]
            ]
    else 
        text ""

eyeInflammationView: User -> Html Msg
eyeInflammationView user = 
    if user.eyeInflammation then
        div [ class "logRow"]
            [ 
                h5 [] [text "Eye Inflammation:"]
                , span [] [
                    text "1"
                    , input [ type_ "radio", name "eyeInflammationNum", onClick <| UpdateLevel "eyeInflammation" 1 ] []
                ]
                ,span [] [text "2"
                    , input [ type_ "radio", name "eyeInflammationNum", onClick <| UpdateLevel "eyeInflammation" 2 ] []
                ]
                ,span [] [text "3"
                    , input [ type_ "radio", name "eyeInflammationNum", onClick <| UpdateLevel "eyeInflammation" 3 ] []
                ]
                ,span [] [text "4"
                    , input [ type_ "radio", name "eyeInflammationNum", onClick <| UpdateLevel "eyeInflammation" 4 ] []
                ]
                ,span [] [text "5"
                    , input [ type_ "radio", name "eyeInflammationNum", onClick <| UpdateLevel "eyeInflammation" 5 ] []
                ]
                ,span [] [text "6"
                    , input [ type_ "radio", name "eyeInflammationNum", onClick <| UpdateLevel "eyeInflammation" 6 ] []
                ]
                ,span [] [text "7"
                    , input [ type_ "radio", name "eyeInflammationNum", onClick <| UpdateLevel "eyeInflammation" 7 ] []
                ]
                ,span [] [text "8"
                    , input [ type_ "radio", name "eyeInflammationNum", onClick <| UpdateLevel "eyeInflammation" 8 ] []
                ]
                ,span [] [text "9"
                    , input [ type_ "radio", name "eyeInflammationNum", onClick <| UpdateLevel "eyeInflammation" 9 ] []
                ]
                ,span [] [text "10"
                    , input [ type_ "radio", name "eyeInflammationNum", onClick <| UpdateLevel "eyeInflammation" 10 ] []
                ]
            ]
    else 
        text ""

eyelidRashesView: User -> Html Msg
eyelidRashesView user = 
    if user.eyelidRashes then
        div [ class "logRow"]
            [ 
                h5 [] [text "Eyelid Rashes:"]
                , span [] [
                    text "1"
                    , input [ type_ "radio", name "eyelidRashesNum", onClick <| UpdateLevel "eyelidRashes" 1 ] []
                ]
                ,span [] [text "2"
                    , input [ type_ "radio", name "eyelidRashesNum", onClick <| UpdateLevel "eyelidRashes" 2 ] []
                ]
                ,span [] [text "3"
                    , input [ type_ "radio", name "eyelidRashesNum", onClick <| UpdateLevel "eyelidRashes" 3 ] []
                ]
                ,span [] [text "4"
                    , input [ type_ "radio", name "eyelidRashesNum", onClick <| UpdateLevel "eyelidRashes" 4 ] []
                ]
                ,span [] [text "5"
                    , input [ type_ "radio", name "eyelidRashesNum", onClick <| UpdateLevel "eyelidRashes" 5 ] []
                ]
                ,span [] [text "6"
                    , input [ type_ "radio", name "eyelidRashesNum", onClick <| UpdateLevel "eyelidRashes" 6 ] []
                ]
                ,span [] [text "7"
                    , input [ type_ "radio", name "eyelidRashesNum", onClick <| UpdateLevel "eyelidRashes" 7 ] []
                ]
                ,span [] [text "8"
                    , input [ type_ "radio", name "eyelidRashesNum", onClick <| UpdateLevel "eyelidRashes" 8 ] []
                ]
                ,span [] [text "9"
                    , input [ type_ "radio", name "eyelidRashesNum", onClick <| UpdateLevel "eyelidRashes" 9 ] []
                ]
                ,span [] [text "10"
                    , input [ type_ "radio", name "eyelidRashesNum", onClick <| UpdateLevel "eyelidRashes" 10 ] []
                ]
            ]
    else 
        text ""

otherView: User -> Html Msg
otherView user = 
    if user.otherSymptom=="" then
        text ""
    else 
        div [ class "logRow"]
            [ 
                h5 [] [text user.otherSymptom]
                , span [] [
                    text "1"
                    , input [ type_ "radio", name "otherSymptomNum", onClick <| UpdateLevel "otherSymptom" 1 ] []
                ]
                ,span [] [text "2"
                    , input [ type_ "radio", name "otherSymptomNum", onClick <| UpdateLevel "otherSymptom" 2 ] []
                ]
                ,span [] [text "3"
                    , input [ type_ "radio", name "otherSymptomNum", onClick <| UpdateLevel "otherSymptom" 3 ] []
                ]
                ,span [] [text "4"
                    , input [ type_ "radio", name "otherSymptomNum", onClick <| UpdateLevel "otherSymptom" 4 ] []
                ]
                ,span [] [text "5"
                    , input [ type_ "radio", name "otherSymptomNum", onClick <| UpdateLevel "otherSymptom" 5 ] []
                ]
                ,span [] [text "6"
                    , input [ type_ "radio", name "otherSymptomNum", onClick <| UpdateLevel "otherSymptom" 6 ] []
                ]
                ,span [] [text "7"
                    , input [ type_ "radio", name "otherSymptomNum", onClick <| UpdateLevel "otherSymptom" 7 ] []
                ]
                ,span [] [text "8"
                    , input [ type_ "radio", name "otherSymptomNum", onClick <| UpdateLevel "otherSymptom" 8 ] []
                ]
                ,span [] [text "9"
                    , input [ type_ "radio", name "otherSymptomNum", onClick <| UpdateLevel "otherSymptom" 9 ] []
                ]
                ,span [] [text "10"
                    , input [ type_ "radio", name "otherSymptomNum", onClick <| UpdateLevel "otherSymptom" 10 ] []
                ]
            ]

passwordError: Bool -> Html Msg
passwordError incorrect = 
    if incorrect then
        div []
            [h3 [] [text"Password not the same, please confirm"]]
    else
        text ""