module Page.Article exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toSession
    , update
    , view
    )

{-| Viewing an individual article.
-}

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Article exposing (Article, Full, Preview)
import Article.Body exposing (Body)
import Article.Comment as Comment exposing (Comment)
import Article.Slug as Slug exposing (Slug)
import Author exposing (Author(..), FollowedAuthor, UnfollowedAuthor)
import Avatar
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Text as Text
import Bootstrap.Utilities.Display as Display
import Bootstrap.Utilities.Spacing as Spacing
import Browser.Navigation as Nav
import CommentId exposing (CommentId)
import Frame
import Html exposing (..)
import Html.Attributes
    exposing
        ( attribute
        , class
        , disabled
        , href
        , id
        , placeholder
        , value
        )
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Loading
import Log
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
    , comments : Status ( CommentText, List Comment )
    , article : Status (Article Full)
    }


type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed


type CommentText
    = Editing String
    | Sending String


init : Session -> Slug -> ( Model, Cmd Msg )
init session slug =
    let
        maybeCred =
            Session.cred session
    in
    ( { session = session
      , timeZone = Time.utc
      , errors = []
      , comments = Loading
      , article = Loading
      }
    , Cmd.batch
        [ Article.fetch maybeCred slug
            |> Http.send CompletedLoadArticle
        , Comment.list maybeCred slug
            |> Http.send CompletedLoadComments
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
                { title, createdAt } =
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
                            div [] []

                viewCommentsSection =
                    Grid.col
                        [ Col.xs12
                        , Col.md8
                        , Col.offsetMd2
                        ]
                    <|
                        -- Don't render the comments until the article has
                        -- loaded!
                        case model.comments of
                            Loading ->
                                []

                            LoadingSlowly ->
                                [ Loading.icon ]

                            Loaded ( commentText, comments ) ->
                                -- Don't let users add comments until they can
                                -- see the existing comments!  Otherwise you
                                -- may be about to repeat something that's
                                -- already been said.
                                [ Grid.container
                                    []
                                    [ Grid.row
                                        []
                                        [ model.session
                                            |> Session.viewer
                                            |> viewAddComment slug commentText
                                        ]
                                    , comments
                                        |> viewComments
                                        |> Grid.row []
                                    ]
                                ]

                            Failed ->
                                [ Loading.error "comments" ]

                viewComments comments =
                    List.map (viewComment model.timeZone slug) comments

                viewBanner =
                    div
                        [ class "banner"
                        , class "bg-dark"
                        , class "text-light"
                        , Spacing.p5
                        , Spacing.my5
                        ]
                        [ Grid.container []
                            [ Grid.row
                                []
                                [ Grid.col
                                    [ Col.xl12
                                    ]
                                    [ h1 [] [ text title ] ]
                                ]
                            , Grid.row
                                []
                                [ Grid.col
                                    [ Col.xl12
                                    , Col.attrs
                                        [ class "d-flex"
                                        , class "align-items-center"
                                        , class "flex-wrap"
                                        ]
                                    ]
                                    [ a
                                        [ author
                                            |> Author.username
                                            |> Route.Profile
                                            |> Route.href
                                        ]
                                        [ img
                                            [ profile
                                                |> Profile.avatar
                                                |> Avatar.src
                                            , class "article-author-img"
                                            , class "rounded-circle"
                                            ]
                                            []
                                        ]
                                    , div
                                        [ class "mx-3"
                                        ]
                                        [ a
                                            [ author
                                                |> Author.username
                                                |> Route.Profile
                                                |> Route.href
                                            ]
                                            [ author
                                                |> Author.username
                                                |> Author.view
                                            ]
                                        , Html.br [] []
                                        , span
                                            [ class "text-muted small" ]
                                            [ Timestamp.view
                                                model.timeZone
                                                createdAt
                                            ]
                                        ]
                                    , buttons
                                    ]
                                ]
                            ]
                        ]

                viewContent =
                    Grid.container [ class "article" ]
                        [ Grid.row
                            []
                            [ Grid.col [ Col.xl12 ]
                                [ model.errors
                                    |> Frame.viewErrors ClickedDismissErrors
                                ]
                            ]
                        , Grid.row
                            []
                            [ Grid.col [ Col.xl12 ]
                                [ Article.Body.toHtml
                                    (Article.body article)
                                    []
                                ]
                            ]
                        , Grid.row
                            []
                            [ Grid.col
                                [ Col.xl12
                                , Col.attrs [ class "text-center" ]
                                ]
                                [ buttons
                                ]
                            ]
                        , Grid.row
                            []
                            [ viewCommentsSection ]
                        ]
            in
            { title = title
            , content =
                div
                    [ class "page-article" ]
                    [ viewBanner
                    , viewContent
                    ]
            }

        Loading ->
            { title = "Article", content = text "" }

        LoadingSlowly ->
            { title = "Article", content = Loading.icon }

        Failed ->
            { title = "Article", content = Loading.error "article" }


viewAddComment : Slug -> CommentText -> Maybe Viewer -> Grid.Column Msg
viewAddComment slug commentText maybeViewer =
    case maybeViewer of
        Just viewer ->
            let
                avatar =
                    Viewer.avatar viewer

                cred =
                    Viewer.cred viewer

                ( commentStr, buttonAttrs ) =
                    case commentText of
                        Editing str ->
                            ( str, [] )

                        Sending str ->
                            ( str, [ disabled True ] )
            in
            Grid.col []
                [ Html.form
                    [ class "card comment-form"
                    , slug
                        |> ClickedPostComment cred
                        |> onSubmit
                    ]
                    [ div [ class "card-block" ]
                        [ textarea
                            [ class "form-control"
                            , placeholder "Write a comment..."
                            , attribute "rows" "3"
                            , onInput EnteredCommentText
                            , value commentStr
                            ]
                            []
                        ]
                    , div [ class "card-footer" ]
                        [ img
                            [ class "comment-author-img"
                            , class "img-fluid"
                            , class "rounded-circle"
                            , Avatar.src avatar
                            ]
                            []
                        , text " "
                        , button
                            (class "btn btn-sm btn-primary"
                                :: buttonAttrs
                            )
                            [ text "Post Comment" ]
                        ]
                    ]
                ]

        Nothing ->
            Grid.col []
                [ p []
                    [ a [ Route.href Route.Login ] [ text "Sign in" ]
                    , text " or "
                    , a [ Route.href Route.Register ] [ text "sign up" ]
                    , text " to comment."
                    ]
                ]


viewButtons : Cred -> Article Full -> Author -> Html Msg
viewButtons cred article author =
    div [ class "my-2" ] <|
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


viewComment : Time.Zone -> Slug -> Comment -> Grid.Column Msg
viewComment timeZone slug comment =
    let
        author =
            Comment.author comment

        profile =
            Author.profile author

        authorUsername =
            Author.username author

        deleteCommentButton =
            case author of
                IsViewer cred _ ->
                    let
                        msg =
                            ClickedDeleteComment cred slug (Comment.id comment)
                    in
                    span
                        [ class "mod-options"
                        , onClick msg
                        ]
                        [ i [ class "ion-trash-a" ] [] ]

                _ ->
                    -- You can't delete other peoples' comments!
                    text ""

        timestamp =
            Timestamp.format timeZone (Comment.createdAt comment)
    in
    Grid.col [ Col.xl12 ]
        [ div
            [ class "card"
            , class "my-3"
            ]
            [ div
                [ class "card-block"
                , class "p-3"
                ]
                [ p [ class "card-text" ] [ text (Comment.body comment) ] ]
            , div
                [ class "card-footer" ]
                [ a
                    [ class "comment-author", href "" ]
                    [ img
                        [ class "comment-author-img"
                        , class "img-fluid"
                        , class "rounded-circle"
                        , profile
                            |> Profile.avatar
                            |> Avatar.src
                        ]
                        []
                    ]
                , text " "
                , a
                    [ class "comment-author"
                    , class "img-fluid"
                    , class "rounded-circle"
                    , authorUsername
                        |> Route.Profile
                        |> Route.href
                    ]
                    [ text (Username.toString authorUsername) ]
                , text " "
                , small
                    [ class "date-posted"
                    , class "text-muted"
                    ]
                    [ text timestamp ]
                , text " "
                , deleteCommentButton
                ]
            ]
        ]



-- UPDATE


type Msg
    = ClickedDeleteArticle Cred Slug
    | ClickedDeleteComment Cred Slug CommentId
    | ClickedDismissErrors
    | ClickedFavorite Cred Slug Body
    | ClickedUnfavorite Cred Slug Body
    | ClickedFollow Cred UnfollowedAuthor
    | ClickedUnfollow Cred FollowedAuthor
    | ClickedPostComment Cred Slug
    | EnteredCommentText String
    | CompletedLoadArticle (Result Http.Error (Article Full))
    | CompletedLoadComments (Result Http.Error (List Comment))
    | CompletedDeleteArticle (Result Http.Error ())
    | CompletedDeleteComment CommentId (Result Http.Error ())
    | CompletedFavoriteChange (Result Http.Error (Article Full))
    | CompletedFollowChange (Result Http.Error Author)
    | CompletedPostComment (Result Http.Error Comment)
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

        CompletedLoadComments (Ok comments) ->
            ( { model | comments = Loaded ( Editing "", comments ) }
            , Cmd.none
            )

        CompletedLoadComments (Err error) ->
            ( { model | article = Failed }, Log.error )

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
                    ( { model
                        | article =
                            article
                                |> Article.mapAuthor (\_ -> newAuthor)
                                |> Loaded
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Log.error )

        CompletedFollowChange (Err error) ->
            ( { model | errors = Api.addServerError model.errors }
            , Log.error
            )

        EnteredCommentText str ->
            case model.comments of
                Loaded ( Editing _, comments ) ->
                    -- You can only edit comment text once comments have loaded
                    -- successfully, and when the comment is not currently
                    -- being submitted.
                    ( { model | comments = Loaded ( Editing str, comments ) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Log.error )

        ClickedPostComment cred slug ->
            case model.comments of
                Loaded ( Editing "", comments ) ->
                    -- No posting empty comments!
                    -- We don't use Log.error here because this isn't an error,
                    -- it just doesn't do anything.
                    ( model, Cmd.none )

                Loaded ( Editing str, comments ) ->
                    ( { model | comments = Loaded ( Sending str, comments ) }
                    , cred
                        |> Comment.post slug str
                        |> Http.send CompletedPostComment
                    )

                _ ->
                    -- Either we have no comment to post, or there's already
                    -- one in the process of being posted, or we don't have
                    -- a valid article, in which case how did we post this?
                    ( model, Log.error )

        CompletedPostComment (Ok comment) ->
            case model.comments of
                Loaded ( _, comments ) ->
                    ( { model
                        | comments =
                            Loaded ( Editing "", comment :: comments )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Log.error )

        CompletedPostComment (Err error) ->
            ( { model | errors = Api.addServerError model.errors }
            , Log.error
            )

        ClickedDeleteComment cred slug id ->
            ( model
            , cred
                |> Comment.delete slug id
                |> Http.send (CompletedDeleteComment id)
            )

        CompletedDeleteComment id (Ok ()) ->
            case model.comments of
                Loaded ( commentText, comments ) ->
                    ( { model
                        | comments =
                            Loaded ( commentText, withoutComment id comments )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Log.error )

        CompletedDeleteComment id (Err error) ->
            ( { model | errors = Api.addServerError model.errors }
            , Log.error
            )

        ClickedDeleteArticle cred slug ->
            ( model
            , delete slug cred
                |> Http.send CompletedDeleteArticle
            )

        CompletedDeleteArticle (Ok ()) ->
            ( model
            , Route.replaceUrl (Session.navKey model.session) Route.Home
            )

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

                comments =
                    case model.comments of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other
            in
            ( { model | article = article, comments = comments }, Cmd.none )



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


fave :
    (Slug -> Cred -> Http.Request (Article Preview))
    -> Cred
    -> Slug
    -> Body
    -> Cmd Msg
fave toRequest cred slug body =
    toRequest slug cred
        |> Http.toTask
        |> Task.map (Article.fromPreview body)
        |> Task.attempt CompletedFavoriteChange


withoutComment : CommentId -> List Comment -> List Comment
withoutComment id list =
    List.filter (\comment -> Comment.id comment /= id) list


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
            [ text
                (" Favorite Article ("
                    ++ String.fromInt favoritesCount
                    ++ ")"
                )
            ]
    in
    if favorited then
        Article.unfavoriteButton cred
            (ClickedUnfavorite cred slug body)
            []
            kids

    else
        Article.favoriteButton cred
            (ClickedFavorite cred slug body)
            []
            kids


deleteButton : Cred -> Article a -> Html Msg
deleteButton cred article =
    let
        msg =
            ClickedDeleteArticle cred (Article.slug article)
    in
    button [ class "btn btn-outline-danger btn-sm", onClick msg ]
        [ i [ class "ion-trash-a" ] [], text " Delete Article" ]


editButton : Article a -> Html Msg
editButton article =
    a
        [ class "btn btn-outline-secondary btn-sm"
        , article
            |> Article.slug
            |> Route.EditArticle
            |> Route.href
        ]
        [ i [ class "ion-edit" ] [], text " Edit Article" ]
