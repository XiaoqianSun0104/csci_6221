module Page.Symptoms exposing (Model, Msg, init, update, view)

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
    | UpdateSymptom String
    | UpdateOtherSymptom String
    | SaveSymptoms
    | SaveZip String
    | SymptomsSaved (Result Http.Error User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserDataReceived user ->
            ( { model | user = user }, Cmd.none )

        UpdateSymptom symptom ->
            let
                updateSymptom = 
                    RemoteData.map
                        (\userData ->
                            case symptom of
                                "jointPain" ->
                                    {userData | jointPain = not userData.jointPain }
                                "jointSwelling" ->
                                    {userData | jointSwelling = not userData.jointSwelling }
                                "fatigue" ->
                                    {userData | fatigue = not userData.fatigue }
                                "musclePain" ->
                                    {userData | musclePain = not userData.musclePain }
                                "fever" ->
                                    {userData | fever = not userData.fever }
                                "rash" ->
                                    {userData | rash = not userData.rash }
                                "chestPain" ->
                                    {userData | chestPain = not userData.chestPain }
                                "hairLoss" ->
                                    {userData | hairLoss = not userData.hairLoss }
                                "sunlightSensitivity" ->
                                    {userData | sunlightSensitivity = not userData.sunlightSensitivity }
                                "kidney" ->
                                    {userData | kidney = not userData.kidney }
                                "mouthSores" ->
                                    {userData | mouthSores = not userData.mouthSores }
                                "anemia" ->
                                    {userData | anemia = not userData.anemia }
                                "dryEyes" ->
                                    {userData | dryEyes = not userData.dryEyes }
                                "eyeInflammation" ->
                                    {userData | eyeInflammation = not userData.eyeInflammation }
                                "eyelidRashes" ->
                                    {userData | eyelidRashes = not userData.eyelidRashes }
                                _ ->
                                    {userData | jointPain = not userData.jointPain}
                        )
                        model.user
            in
            ( { model | user = updateSymptom }, Cmd.none )
        UpdateOtherSymptom newOther ->
            let
                updateOtherSymptom =
                    RemoteData.map
                        (\userData ->
                            { userData | otherSymptom = newOther }
                        )
                        model.user
            in
            ( { model | user = updateOtherSymptom }, Cmd.none )

        SaveSymptoms ->
            ( model, saveSymptoms model.user )
        SaveZip newZip ->
            let
                updateZip =
                    RemoteData.map
                        (\userData ->
                            { userData | zip = (Maybe.withDefault 20052 (String.toInt newZip)) }
                        )
                        model.user
            in
            ( { model | user = updateZip }, Cmd.none )

        SymptomsSaved (Ok userData) ->
            let
                user =
                    RemoteData.succeed userData
            in
            ( { model | user = user, saveError = Nothing }
            , Nav.pushUrl model.navKey <| "/users/lifelog/"++userData.id
            )
        SymptomsSaved (Err error) ->
            ( { model | saveError = Just (buildErrorMessage error) }
            , Cmd.none
            )


saveSymptoms : WebData User -> Cmd Msg
saveSymptoms user =
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
                , expect = Http.expectJson SymptomsSaved userDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

        _ ->
            Cmd.none

view : Model -> Html Msg
view model =
    div [class "container"]
        [ viewPost model.user
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
    Html.form [class "sypmtoms-list"]
        [ h3 [class "locTitle"] [ text "Current Location:" ]
        , span [class "locInput"][
            input [ type_ "text", placeholder "Zip Code"
                , value (String.fromInt user.zip)
                , onInput SaveZip] []
        ]
        , h3 [] [ text "Edit Symptoms" ]
        , div []
            [ label [] [
                input [ type_ "checkbox", checked <| user.jointPain, onClick <| UpdateSymptom "jointPain" ] []
                , span [] [text "Joints Pain"]
            ]]
        , div []
            [ label [] [
                input [ type_ "checkbox", checked <| user.jointSwelling, onClick <| UpdateSymptom "jointSwelling" ] []
                , span [] [text "Joints Swellings"]
            ]]
        , div []
            [ label [] [
                input [ type_ "checkbox", checked <| user.fatigue, onClick <| UpdateSymptom "fatigue" ] []
                , span [] [text "Fatigue"]
            ]]
        , div []
            [ label [] [
                input [ type_ "checkbox", checked <| user.musclePain, onClick <| UpdateSymptom "musclePain" ] []
                , span [] [text "Muscle Pain"]
            ]]
        , div []
            [ label [] [
                input [ type_ "checkbox", checked <| user.fever, onClick <| UpdateSymptom "fever" ] []
                ,span [] [text "Fever"]
            ]]
        , div []
            [ label [] [
                input [ type_ "checkbox", checked <| user.rash, onClick <| UpdateSymptom "rash" ] []
                , span [] [text "Rash"]
            ]]
        , div []
            [ label [] [
                input [ type_ "checkbox", checked <| user.chestPain, onClick <| UpdateSymptom "chestPain" ] []
                , span [] [text "Chest Pain"]
            ]]
        , div []
            [ label [] [
                input [ type_ "checkbox", checked <| user.hairLoss, onClick <| UpdateSymptom "hairLoss" ] []
                , span [] [text "Hair Loss"]
            ]]
        , div []
            [ label [] [
                input [ type_ "checkbox", checked <| user.sunlightSensitivity, onClick <| UpdateSymptom "sunlightSensitivity" ] []
                , span [] [text "Sun or light sensitivity"]
            ]]
        , div []
            [ label [] [
                input [ type_ "checkbox", checked <| user.kidney, onClick <| UpdateSymptom "kidney" ] []
                , span [] [text "Kidney"]
            ]]
        , div []
            [ label [] [
                input [ type_ "checkbox", checked <| user.mouthSores, onClick <| UpdateSymptom "mouthSores" ] []
                , span [] [text "Mouth sores"]
            ]]
        , div []
            [ label [] [
                input [ type_ "checkbox", checked <| user.anemia, onClick <| UpdateSymptom "anemia" ] []
                ,span [] [text "Anemia"]
            ]]
        , div []
            [ label [] [
                input [ type_ "checkbox", checked <| user.dryEyes, onClick <| UpdateSymptom "dryEyes" ] []
                , span [] [text "Dry Eyes"]
            ]]
        , div []
            [ label [] [
                input [ type_ "checkbox", checked <| user.eyeInflammation, onClick <| UpdateSymptom "eyeInflammation" ] []
                , span [] [text "Eye Inflammation"]
            ]]
        , div []
            [ label [] [
                input [ type_ "checkbox", checked <| user.eyelidRashes, onClick <| UpdateSymptom "eyelidRashes" ] []
                , span [] [text "Eyelid Rashes"]
            ]]
        , div [class "otherSymptom"]
            [ label [] [
                input [ type_ "text", placeholder "Other", value user.otherSymptom
                , onInput UpdateOtherSymptom ] []
                
            ]]
        , 
             button [ class "symptomBtn", type_ "button", onClick SaveSymptoms ]
                [ text "Submit" ]
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
