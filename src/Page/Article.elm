module Page.Article exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| Viewing an individual article.
-}

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Article exposing (Article, Full, Preview)
import Article.Body exposing (Body)
import Article.Slug as Slug exposing (Slug)
import Author exposing (Author(..), FollowedAuthor, UnfollowedAuthor)
import Avatar
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (attribute, class, disabled, href, id, placeholder, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Loading
import Log
import Page
import Profile exposing (Profile)
import Route
import Session exposing (Session)
import Task exposing (Task)
import Time
import Timestamp
import Username exposing (Username)
import Viewer exposing (Viewer)


-- MODEL


type alias Model =
    { session : Session
    , timeZone : Time.Zone
    , errors : List String

    -- Loaded independently from server
    , article : Status (Article Full)
    }


type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed


init : Session -> Slug -> ( Model, Cmd Msg )
init session slug =
    let
        maybeCred =
            Session.cred session
    in
        ( { session = session
          , timeZone = Time.utc
          , errors = []
          , article = Loading
          }
        , Cmd.batch
            [ Article.fetch maybeCred slug
                |> Http.send CompletedLoadArticle
            , Task.perform GotTimeZone Time.here
            , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
            ]
        )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    case model.article of
        Loaded article ->
            let
                { title } =
                    Article.metadata article

                author =
                    Article.author article

                avatar =
                    Profile.avatar (Author.profile author)

                slug =
                    Article.slug article

                profile =
                    Author.profile author

                buttons =
                    case Session.cred model.session of
                        Just cred ->
                            viewButtons cred article author

                        Nothing ->
                            []
            in
                { title = title
                , content =
                    div [ class "article-page" ]
                        [ div [ class "banner" ]
                            [ div [ class "container" ]
                                [ h1 [] [ text title ]
                                , div [ class "article-meta" ] <|
                                    List.append
                                        [ a [ Route.href (Route.Profile (Author.username author)) ]
                                            [ img [ Avatar.src (Profile.avatar profile) ] [] ]
                                        , div [ class "info" ]
                                            [ Author.view (Author.username author)
                                            , Timestamp.view model.timeZone (Article.metadata article).createdAt
                                            ]
                                        ]
                                        buttons
                                , Page.viewErrors ClickedDismissErrors model.errors
                                ]
                            ]
                        , div [ class "container page" ]
                            [ div [ class "row article-content" ]
                                [ div [ class "col-md-12" ]
                                    [ Article.Body.toHtml (Article.body article) [] ]
                                ]
                            , hr [] []
                            , div [ class "article-actions" ]
                                [ div [ class "article-meta" ] <|
                                    List.append
                                        [ a [ Route.href (Route.Profile (Author.username author)) ]
                                            [ img [ Avatar.src avatar ] [] ]
                                        , div [ class "info" ]
                                            [ Author.view (Author.username author)
                                            , Timestamp.view model.timeZone (Article.metadata article).createdAt
                                            ]
                                        ]
                                        buttons
                                ]
                            ]
                        ]
                }

        Loading ->
            { title = "Article", content = text "" }

        LoadingSlowly ->
            { title = "Article", content = Loading.centeredIcon }

        Failed ->
            { title = "Article", content = Loading.error "article" }


viewButtons : Cred -> Article Full -> Author -> List (Html Msg)
viewButtons cred article author =
    case author of
        IsFollowing followedAuthor ->
            [ Author.unfollowButton ClickedUnfollow cred followedAuthor
            , text " "
            , favoriteButton cred article
            ]

        IsNotFollowing unfollowedAuthor ->
            [ Author.followButton ClickedFollow cred unfollowedAuthor
            , text " "
            , favoriteButton cred article
            ]

        IsViewer _ _ ->
            [ editButton article
            , text " "
            , deleteButton cred article
            ]



-- UPDATE


type Msg
    = ClickedDeleteArticle Cred Slug
    | ClickedDismissErrors
    | ClickedFavorite Cred Slug Body
    | ClickedUnfavorite Cred Slug Body
    | ClickedFollow Cred UnfollowedAuthor
    | ClickedUnfollow Cred FollowedAuthor
    | CompletedLoadArticle (Result Http.Error (Article Full))
    | CompletedDeleteArticle (Result Http.Error ())
    | CompletedFavoriteChange (Result Http.Error (Article Full))
    | CompletedFollowChange (Result Http.Error Author)
    | GotTimeZone Time.Zone
    | GotSession Session
    | PassedSlowLoadThreshold


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedDismissErrors ->
            ( { model | errors = [] }, Cmd.none )

        ClickedFavorite cred slug body ->
            ( model, fave Article.favorite cred slug body )

        ClickedUnfavorite cred slug body ->
            ( model, fave Article.unfavorite cred slug body )

        CompletedLoadArticle (Ok article) ->
            ( { model | article = Loaded article }, Cmd.none )

        CompletedLoadArticle (Err error) ->
            ( { model | article = Failed }
            , Log.error
            )

        CompletedFavoriteChange (Ok newArticle) ->
            ( { model | article = Loaded newArticle }, Cmd.none )

        CompletedFavoriteChange (Err error) ->
            ( { model | errors = Api.addServerError model.errors }
            , Log.error
            )

        ClickedUnfollow cred followedAuthor ->
            ( model
            , Author.requestUnfollow followedAuthor cred
                |> Http.send CompletedFollowChange
            )

        ClickedFollow cred unfollowedAuthor ->
            ( model
            , Author.requestFollow unfollowedAuthor cred
                |> Http.send CompletedFollowChange
            )

        CompletedFollowChange (Ok newAuthor) ->
            case model.article of
                Loaded article ->
                    ( { model | article = Loaded (Article.mapAuthor (\_ -> newAuthor) article) }, Cmd.none )

                _ ->
                    ( model, Log.error )

        CompletedFollowChange (Err error) ->
            ( { model | errors = Api.addServerError model.errors }
            , Log.error
            )

        ClickedDeleteArticle cred slug ->
            ( model
            , delete slug cred
                |> Http.send CompletedDeleteArticle
            )

        CompletedDeleteArticle (Ok ()) ->
            ( model, Route.replaceUrl (Session.navKey model.session) Route.Home )

        CompletedDeleteArticle (Err error) ->
            ( { model | errors = Api.addServerError model.errors }
            , Log.error
            )

        GotTimeZone tz ->
            ( { model | timeZone = tz }, Cmd.none )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        PassedSlowLoadThreshold ->
            let
                -- If any data is still Loading, change it to LoadingSlowly
                -- so `view` knows to render a spinner.
                article =
                    case model.article of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other
            in
                ( { model | article = article }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- HTTP


delete : Slug -> Cred -> Http.Request ()
delete slug cred =
    Api.delete (Endpoint.article slug) cred Http.emptyBody (Decode.succeed ())



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session



-- INTERNAL


fave : (Slug -> Cred -> Http.Request (Article Preview)) -> Cred -> Slug -> Body -> Cmd Msg
fave toRequest cred slug body =
    toRequest slug cred
        |> Http.toTask
        |> Task.map (Article.fromPreview body)
        |> Task.attempt CompletedFavoriteChange


favoriteButton : Cred -> Article Full -> Html Msg
favoriteButton cred article =
    let
        { favoritesCount, favorited } =
            Article.metadata article

        slug =
            Article.slug article

        body =
            Article.body article

        kids =
            [ text (" Favorite Article (" ++ String.fromInt favoritesCount ++ ")") ]
    in
        if favorited then
            Article.unfavoriteButton cred (ClickedUnfavorite cred slug body) [] kids
        else
            Article.favoriteButton cred (ClickedFavorite cred slug body) [] kids


deleteButton : Cred -> Article a -> Html Msg
deleteButton cred article =
    let
        msg =
            ClickedDeleteArticle cred (Article.slug article)
    in
        button [ class "btn btn-outline-danger btn-sm", onClick msg ]
            [ i [ class "ion-trash-a" ] [], text " Delete Meal" ]


editButton : Article a -> Html Msg
editButton article =
    a [ class "btn btn-outline-secondary btn-sm", Route.href (Route.EditArticle (Article.slug article)) ]
        [ i [ class "ion-edit" ] [], text " Edit Meal" ]
