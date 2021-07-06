module Page.Diary exposing (Model, Msg, init, update, view)

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
    , userId: String
    , user : WebData User
    , saveError : Maybe String
    }


init : String -> Nav.Key -> ( Model, Cmd Msg )
init userId navKey =
    ( initialModel userId navKey, fetchUser userId )


initialModel : String -> Nav.Key -> Model
initialModel userId navKey =
    { navKey = navKey
    , userId = userId
    , user = RemoteData.Loading
    , saveError = Nothing
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
    = UserDataReceived (WebData User)
    | GoBack


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserDataReceived user ->
            ( { model | user = user }, Cmd.none )
        GoBack ->
            ( model, Nav.pushUrl model.navKey <| "/users/result/"++model.userId )


view : Model -> Html Msg
view model =
    div [class "container"]
        [ h3 [class "lifelogTitle"][text "My Diary"]
        ,viewPost model.user
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
    Html.form [class "timeline"]
        [   h5 [][text "Tue, Jun 29"]
           ,ul [][
                jointPainView user
                , jointSwellingView user
                , fatigueView user
                ,musclePainView user
                ,feverView user
                ,rashView user
                ,chestPainView user
                ,hairLossView user
                ,sunlightSensitivityView user
                ,kidneyView user
                ,mouthSoresView user
                ,anemiaView user
                ,dryEyesView user
                ,eyeInflammationView user
                ,eyelidRashesView user
            ]
            , button [ type_ "button", onClick GoBack, class "goDiaryBtn" ]
                    [ text "Back to Report" ]
        
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
        li [][text ("Joints Pain - Level "++String.fromInt user.jointPainNum)]
    else
        text ""

jointSwellingView: User -> Html Msg
jointSwellingView user = 
    if user.jointSwelling then
        li [][text ("Joint Swelling - Level "++String.fromInt user.jointSwellingNum)]
    else
        text ""


fatigueView: User -> Html Msg
fatigueView user = 
    if user.fatigue then
        li [][text ("Fatigue - Level "++String.fromInt user.fatigueNum)]
    else
        text ""

musclePainView: User -> Html Msg
musclePainView user = 
    if user.musclePain then
        li [][text ("Muscle Pain - Level "++String.fromInt user.musclePainNum)]
    else
        text ""

feverView: User -> Html Msg
feverView user = 
    if user.fever then
        li [][text ("Fever- Level "++String.fromInt user.feverNum)]
    else
        text ""

rashView: User -> Html Msg
rashView user = 
    if user.rash then
        li [][text ("Rash- Level "++String.fromInt user.rashNum)]
    else
        text ""

chestPainView: User -> Html Msg
chestPainView user = 
    if user.chestPain then
        li [][text ("Chest Pain - Level "++String.fromInt user.chestPainNum)]
    else
        text ""

hairLossView: User -> Html Msg
hairLossView user = 
    if user.hairLoss then
        li [][text ("Hair Loss - Level "++String.fromInt user.hairLossNum)]
    else
        text ""

sunlightSensitivityView: User -> Html Msg
sunlightSensitivityView user = 
    if user.sunlightSensitivity then
        li [][text ("Sunlight Sensitivity - Level "++String.fromInt user.sunlightSensitivityNum)]
    else
        text ""

kidneyView: User -> Html Msg
kidneyView user = 
    if user.kidney then
        li [][text ("Kidney - Level "++String.fromInt user.kidneyNum)]
    else
        text ""

mouthSoresView: User -> Html Msg
mouthSoresView user = 
    if user.mouthSores then
        li [][text ("Mouth Sores - Level "++String.fromInt user.mouthSoresNum)]
    else
        text ""

anemiaView: User -> Html Msg
anemiaView user = 
    if user.anemia then
        li [][text ("Anemia - Level "++String.fromInt user.anemiaNum)]
    else
        text ""

dryEyesView: User -> Html Msg
dryEyesView user = 
    if user.dryEyes then
        li [][text ("Dry Eyes - Level "++String.fromInt user.dryEyesNum)]
    else
        text ""

eyeInflammationView: User -> Html Msg
eyeInflammationView user = 
    if user.eyeInflammation then
        li [][text ("Eye Inflammation - Level "++String.fromInt user.eyeInflammationNum)]
    else
        text ""

eyelidRashesView: User -> Html Msg
eyelidRashesView user = 
    if user.eyelidRashes then
        li [][text ("Eyelid Rashes - Level "++String.fromInt user.eyelidRashesNum)]
    else
        text ""