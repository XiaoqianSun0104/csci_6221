module Page.CreateUser exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import User exposing (User, emptyUser, newUserEncoder, userDecoder)
import Route
import Debug exposing (log)
import Process
import Task

type alias Model =
    { navKey : Nav.Key
    , user : User
    , createError : Maybe String
    , loading: Bool
    , passwordIncorrect: Bool
    }


init : Nav.Key -> ( Model, Cmd Msg )
init navKey =
    ( initialModel navKey, Cmd.none )


initialModel : Nav.Key -> Model
initialModel navKey =
    { navKey = navKey
    , user = emptyUser
    , createError = Nothing
    , loading = False
    , passwordIncorrect = False
    }


view : Model -> Html Msg
view model =
    div [class "container right-panel-active"]
        [
            div [class "form-container sign-up-container"]
                [ 
                    Html.form [class "sign-up-login-form"] [
                        newUserForm
                        , passwordError model.passwordIncorrect
                        , viewError model.createError
                    ]
                ]
            , div [class "overlay-container"]
            [
                div [class "overlay"] [
                    div [class "overlay-panel overlay-left"] [
                        h1 [] [text"Welcome Back!"]
                        , p [] [text"To keep connected, please register an account"]
                        , a [ href "/users/login", class "ghost button"] [ text "Log In" ]
                    ]
                ]
            ]
            , loadingView model.loading
        ]


newUserForm : Html Msg
newUserForm =
    div []
        [ 
            h1 [] [ text "Create Account" ]
            , input [ type_ "text", placeholder "Username", onInput StoreUsername ] []
            
            , input [ type_ "password", placeholder "Password", onInput StorePassword ] []
            
            , input [ type_ "password", placeholder "Confirm Password", onInput ValidatePassword ] []
            
            , input [ type_ "file", placeholder "Gene Data" ] []

            , div []
                [ span [] [text "Gene Data not Gathered - Please head to xxx to extract your gene data"]
                , br [] []
                ]
            , br [] []
            , div []
                [ button [ type_ "button", onClick CreateUser ]
                    [ text "Submit" ]
                ]
        ]


type Msg
    = StoreUsername String
    | StorePassword String
    | ValidatePassword String
    | CreateUser
    | CreateuserCall
    | UserCreated (Result Http.Error User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoreUsername username ->
            let
                oldPost =
                    model.user

                updateUsername =
                    { oldPost | username = username, id=username }
            in
            ( { model | user = updateUsername }, Cmd.none )

        StorePassword password ->
            let
                oldPost =
                    model.user

                updatePassword =
                    { oldPost | password = password }
            in
            ( { model | user = updatePassword }, Cmd.none )

        ValidatePassword cPassword ->
            let
                oldPost =
                    model.user
                updateIncorrect = 
                    if oldPost.password == cPassword then
                        False
                    else
                        True
            in
            ( { model | passwordIncorrect = updateIncorrect}, Cmd.none )
            

        CreateUser ->
            ( {model|loading=True}, createUser model )

        CreateuserCall ->
            ({model | loading = False}, createUserCall model)
        UserCreated (Ok user) ->
            ( { model | user = user, createError = Nothing }
            , Nav.pushUrl model.navKey <| "/users/result/"++user.id
            )

        UserCreated (Err error) ->
            ( { model | createError = Just (buildErrorMessage error) }
            , Cmd.none
            )

createUserCall : Model -> Cmd Msg
createUserCall model =
    Http.post
                { url = "http://34.195.151.200:5019/users"
                , body = Http.jsonBody (newUserEncoder model.user)
                , expect = Http.expectJson UserCreated userDecoder
                }

createUser : Model -> Cmd Msg
createUser model =
    if model.passwordIncorrect then 
        Cmd.none
    else
        Process.sleep 4000
            |> Task.perform (\_ -> CreateuserCall)
passwordError: Bool -> Html Msg
passwordError incorrect = 
    if incorrect then
        div []
            [p [class "error-msg"] [text"Password not the same, please confirm"]]
    else
        text ""

viewError : Maybe String -> Html msg
viewError maybeError =
    case maybeError of
        Just error ->
            div []
                [ h3 [] [ text "Couldn't create a user at this time." ]
                , text ("Error: " ++ error)
                ]

        Nothing ->
            text ""
loadingView: Bool -> Html Msg
loadingView loading = 
    if loading then
        div [class "loadingDiv"][text "Please wait. Analyzing..."]
    else
    text ""