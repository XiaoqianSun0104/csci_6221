module Page.LoginUser exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import User exposing (User, emptyUser, userDecoder)
import Route


type alias Model =
    { navKey : Nav.Key
    , user : User
    , loginError : Maybe String
    }


init : Nav.Key -> ( Model, Cmd Msg )
init navKey =
    ( initialModel navKey, Cmd.none )


initialModel : Nav.Key -> Model
initialModel navKey =
    { navKey = navKey
    , user = emptyUser
    , loginError = Nothing
    }


view : Model -> Html Msg
view model =
    div [class "container left-panel-active"] [
        div [class "form-container sign-in-container"]
            [   Html.form [class "sign-up-login-form"] [
                    loginUserForm
                    , viewError model.loginError
                ]
            ]
        , div [class "overlay-container"]
            [
                div [class "overlay"] [
                    div [class "overlay-panel overlay-right"] [
                        h1 [] [text"Hello Friend!"]
                        , p [] [text"To keep connected, please register an account"]
                        , a [ href "/users/new", class "ghost button"] [ text "Sign Up" ]
                    ]
                ]
            ]
    ]
    


loginUserForm : Html Msg
loginUserForm =
    div []
        [ 
            h3 [] [ text "User Login" ]
          
            , input [ type_ "text", placeholder "Username", onInput StoreUsername ] []
            
        
            , input [ type_ "password", placeholder "Password", onInput StorePassword ] []
            
            , div []
                [ button [ type_ "button", onClick AuthenticateUser ]
                    [ text "Login" ]
                ]
        ]


type Msg
    = StoreUsername String
    | StorePassword String
    | AuthenticateUser
    | UserChecked (Result Http.Error User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoreUsername username ->
            let
                oldUser =
                    model.user

                updateUsername =
                    { oldUser | username = username, id= username }
            in
            ( { model | user = updateUsername }, Cmd.none )

        StorePassword password ->
            let
                oldUser =
                    model.user

                updatePassword =
                    { oldUser | password = password }
            in
            ( { model | user = updatePassword }, Cmd.none )

        AuthenticateUser ->
            ( model, authenticateUser model.user )

        UserChecked (Ok user) ->
            ( { model | user = user, loginError = Nothing }
            , Nav.pushUrl model.navKey <| "/users/result/"++user.id
            )

        UserChecked (Err error) ->
            ( { model | loginError = Just (buildErrorMessage error) }
            , Cmd.none
            )


authenticateUser : User -> Cmd Msg
authenticateUser user =
    Http.get
        { url = "http://34.195.151.200:5019/users/"++user.id
        , expect = Http.expectJson UserChecked userDecoder
        }

viewError : Maybe String -> Html msg
viewError maybeError =
    case maybeError of
        Just error ->
            div []
                [ p [class "error-msg"] [ text "Username or password incorrect, Pleast try again." ]
                ]

        Nothing ->
            text ""
