module Route exposing (Route(..), parseUrl, pushUrl)

import Browser.Navigation as Nav
import Post exposing (PostId)
import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = NotFound
    | Posts
    | Post PostId
    | NewUser
    | LoginUser
    | LupusResult String
    | Symptoms String
    | LifeLog String
    | Diary String
    | DailyUpdate String String


parseUrl : Url -> Route
parseUrl url =
    case parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFound


matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map LoginUser top
        , map Posts (s "posts")
        , map Post (s "posts" </> Post.idParser)
        , map NewUser (s "users" </> s "new")
        , map LoginUser (s "users" </> s"login")
        , map LupusResult (s "users" </> s"result" </> string)
        , map Symptoms (s "users" </> s"symptoms" </> string)
        , map LifeLog (s "users" </> s"lifelog" </> string)
        , map Diary (s "users" </> s"diary" </> string)
        , map DailyUpdate (s "users" </> s"dailyupdate" </> string </> string)
        ]


pushUrl : Route -> Nav.Key -> Cmd msg
pushUrl route navKey =
    routeToString route
        |> Nav.pushUrl navKey


routeToString : Route -> String
routeToString route =
    case route of
        NotFound ->
            "/not-found"

        Posts ->
            "/posts"

        Post postId ->
            "/posts/" ++ Post.idToString postId

        NewUser ->
            "/users/new"
        
        LoginUser ->
            "/users/login"

        LupusResult userId->
            "/users/result" ++ userId

        Symptoms userId->
            "/users/symptoms/" ++ userId

        LifeLog userId->
            "/users/lifelog/" ++ userId

        Diary userId->
            "/users/diary/" ++ userId

        DailyUpdate userId zip->
            "/users/dailyupdate/" ++ userId ++ "/" ++zip
