module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Page.EditPost as EditPost
import Page.ListPosts as ListPosts
import Page.CreateUser as NewUser
import Page.LoginUser as LoginUser
import Page.LupusResult as LupusResult
import Page.Symptoms as Symptoms
import Page.LifeLog as LifeLog
import Page.Diary as Diary
import Page.DailyUpdate as DailyUpdate
import Route exposing (Route)
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type alias Model =
    { route : Route
    , page : Page
    , navKey : Nav.Key
    }


type Page
    = NotFoundPage
    | ListPage ListPosts.Model
    | EditPage EditPost.Model
    | NewPage NewUser.Model
    | LoginPage LoginUser.Model
    | LupusResultPage LupusResult.Model
    | SymptomsPage Symptoms.Model
    | LifeLogPage LifeLog.Model
    | DiaryPage Diary.Model
    | DailyUpdatePage DailyUpdate.Model


type Msg
    = ListPageMsg ListPosts.Msg
    | LinkClicked UrlRequest
    | UrlChanged Url
    | EditPageMsg EditPost.Msg
    | NewPageMsg NewUser.Msg
    | LoginPageMsg LoginUser.Msg
    | LupusResultPageMsg LupusResult.Msg
    | SymptomsPageMsg Symptoms.Msg
    | LifeLogPageMsg LifeLog.Msg
    | DiaryPageMsg Diary.Msg
    | DailyUpdatePageMsg DailyUpdate.Msg


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            { route = Route.parseUrl url
            , page = NotFoundPage
            , navKey = navKey
            }
    in
    initCurrentPage ( model, Cmd.none )


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.route of
                Route.NotFound ->
                    ( NotFoundPage, Cmd.none )

                Route.Posts ->
                    let
                        ( pageModel, pageCmds ) =
                            ListPosts.init
                    in
                    ( ListPage pageModel, Cmd.map ListPageMsg pageCmds )

                Route.Post postId ->
                    let
                        ( pageModel, pageCmd ) =
                            EditPost.init postId model.navKey
                    in
                    ( EditPage pageModel, Cmd.map EditPageMsg pageCmd )

                Route.NewUser ->
                    let
                        ( pageModel, pageCmd ) =
                            NewUser.init model.navKey
                    in
                    ( NewPage pageModel, Cmd.map NewPageMsg pageCmd )

                Route.LoginUser ->
                    let
                        ( pageModel, pageCmd ) =
                            LoginUser.init model.navKey
                    in
                    ( LoginPage pageModel, Cmd.map LoginPageMsg pageCmd )

                Route.LupusResult userId ->
                    let
                        ( pageModel, pageCmd ) =
                            LupusResult.init userId model.navKey
                    in
                    ( LupusResultPage pageModel, Cmd.map LupusResultPageMsg pageCmd )

                Route.Symptoms userId ->
                    let
                        ( pageModel, pageCmd ) =
                            Symptoms.init userId model.navKey
                    in
                    ( SymptomsPage pageModel, Cmd.map SymptomsPageMsg pageCmd )

                Route.LifeLog userId ->
                    let
                        ( pageModel, pageCmd ) =
                            LifeLog.init userId model.navKey
                    in
                    ( LifeLogPage pageModel, Cmd.map LifeLogPageMsg pageCmd )

                Route.Diary userId ->
                    let
                        ( pageModel, pageCmd ) =
                            Diary.init userId model.navKey
                    in
                    ( DiaryPage pageModel, Cmd.map DiaryPageMsg pageCmd )

                Route.DailyUpdate userId zip->
                    let
                        ( pageModel, pageCmd ) =
                            DailyUpdate.init userId zip model.navKey
                    in
                    ( DailyUpdatePage pageModel, Cmd.map DailyUpdatePageMsg pageCmd )
    in
    ( { model | page = currentPage }
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )


view : Model -> Document Msg
view model =
    { title = "Post App"
    , body = [ currentView model ]
    }


currentView : Model -> Html Msg
currentView model =
    case model.page of
        NotFoundPage ->
            notFoundView

        ListPage pageModel ->
            ListPosts.view pageModel
                |> Html.map ListPageMsg

        EditPage pageModel ->
            EditPost.view pageModel
                |> Html.map EditPageMsg

        NewPage pageModel ->
            NewUser.view pageModel
                |> Html.map NewPageMsg
        
        LoginPage pageModel ->
            LoginUser.view pageModel
                |> Html.map LoginPageMsg

        LupusResultPage pageModel ->
            LupusResult.view pageModel
                |> Html.map LupusResultPageMsg
        
        SymptomsPage pageModel ->
            Symptoms.view pageModel
                |> Html.map SymptomsPageMsg

        LifeLogPage pageModel ->
            LifeLog.view pageModel
                |> Html.map LifeLogPageMsg

        DiaryPage pageModel ->
            Diary.view pageModel
                |> Html.map DiaryPageMsg
        
        DailyUpdatePage pageModel ->
            DailyUpdate.view pageModel
                |> Html.map DailyUpdatePageMsg


notFoundView : Html msg
notFoundView =
    h3 [] [ text "Oops! The page you requested was not found!" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( ListPageMsg subMsg, ListPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    ListPosts.update subMsg pageModel
            in
            ( { model | page = ListPage updatedPageModel }
            , Cmd.map ListPageMsg updatedCmd
            )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        ( UrlChanged url, _ ) ->
            let
                newRoute =
                    Route.parseUrl url
            in
            ( { model | route = newRoute }, Cmd.none )
                |> initCurrentPage

        ( EditPageMsg subMsg, EditPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    EditPost.update subMsg pageModel
            in
            ( { model | page = EditPage updatedPageModel }
            , Cmd.map EditPageMsg updatedCmd
            )

        ( NewPageMsg subMsg, NewPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    NewUser.update subMsg pageModel
            in
            ( { model | page = NewPage updatedPageModel }
            , Cmd.map NewPageMsg updatedCmd
            )

        ( LoginPageMsg subMsg, LoginPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    LoginUser.update subMsg pageModel
            in
            ( { model | page = LoginPage updatedPageModel }
            , Cmd.map LoginPageMsg updatedCmd
            )

        ( LupusResultPageMsg subMsg, LupusResultPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    LupusResult.update subMsg pageModel
            in
            ( { model | page = LupusResultPage updatedPageModel }
            , Cmd.map LupusResultPageMsg updatedCmd
            )

        ( SymptomsPageMsg subMsg, SymptomsPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    Symptoms.update subMsg pageModel
            in
            ( { model | page = SymptomsPage updatedPageModel }
            , Cmd.map SymptomsPageMsg updatedCmd
            )

        ( LifeLogPageMsg subMsg, LifeLogPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    LifeLog.update subMsg pageModel
            in
            ( { model | page = LifeLogPage updatedPageModel }
            , Cmd.map LifeLogPageMsg updatedCmd
            )

        ( DiaryPageMsg subMsg, DiaryPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    Diary.update subMsg pageModel
            in
            ( { model | page = DiaryPage updatedPageModel }
            , Cmd.map DiaryPageMsg updatedCmd
            )

        ( DailyUpdatePageMsg subMsg, DailyUpdatePage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    DailyUpdate.update subMsg pageModel
            in
            ( { model | page = DailyUpdatePage updatedPageModel }
            , Cmd.map DailyUpdatePageMsg updatedCmd
            )

        ( _, _ ) ->
            ( model, Cmd.none )
