module Frame exposing (NavbarIndicator(..), view, viewErrors)

import Api exposing (Cred)
import Avatar
import Bootstrap.Navbar as Navbar
import Browser exposing (Document)
import Html exposing (Html, a, button, div, footer, i, img, li, nav, p, span, text, ul)
import Html.Attributes exposing (class, classList, href, style)
import Html.Events exposing (onClick)
import Profile
import Route exposing (Route)
import Session exposing (Session)
import Username exposing (Username)
import Viewer exposing (Viewer)


{-| Determines which navbar link (if any) will be rendered as active.

Note that we don't enumerate every page here, because the navbar doesn't
have links for every page. Anything that's not part of the navbar falls
under None.

-}
type NavbarIndicator
    = None
    | Home
    | Login
    | Register
    | Settings
    | Profile Username
    | NewArticle


{-| Take a page's Html and frames it with a header and footer.

The caller provides the current user, so we can display in either
"signed in" (rendering username) or "signed out" mode.

isLoading is for determining whether we should show a loading spinner
in the header. (This comes up during slow page transitions.)

-}
view :
    Maybe Viewer
    -> NavbarIndicator
    -> Navbar.Config msg
    -> Navbar.State
    -> { title : String, content : Html msg }
    -> Document msg
view maybeViewer navbarIndicator navbarConfig navbarState { title, content } =
    let
        header : Html msg
        header =
            viewHeader
                navbarIndicator
                navbarConfig
                navbarState
                maybeViewer

        footer : Html msg
        footer =
            viewFooter
    in
    { title = title ++ " - Conduit"
    , body =
        [ header
        , content
        , footer
        ]
    }


viewHeader :
    NavbarIndicator
    -> Navbar.Config msg
    -> Navbar.State
    -> Maybe Viewer
    -> Html msg
viewHeader navbarIndicator navbarConfig navbarState maybeViewer =
    navbarConfig
        |> Navbar.light
        |> Navbar.collapseSmall
        |> Navbar.withAnimation
        |> Navbar.brand [ Route.href Route.Home ] [ Html.text "conduit" ]
        |> Navbar.items
            (navbarLink navbarIndicator Route.Home [ text "Home" ]
                :: viewMenu navbarIndicator maybeViewer
            )
        |> Navbar.view navbarState


viewMenu : NavbarIndicator -> Maybe Viewer -> List (Navbar.Item msg)
viewMenu navbarIndicator maybeViewer =
    let
        linkTo =
            navbarLink navbarIndicator
    in
    case maybeViewer of
        Just viewer ->
            let
                username =
                    Viewer.username viewer

                avatar =
                    Viewer.avatar viewer
            in
            [ linkTo Route.NewArticle [ i [ class "ion-compose" ] [], text "\u{00A0}New Post" ]
            , linkTo Route.Settings [ i [ class "ion-gear-a" ] [], text "\u{00A0}Settings" ]
            , linkTo
                (Route.Profile username)
                [ img [ class "user-pic", Avatar.src avatar ] []
                , Username.toHtml username
                ]
            , linkTo Route.Logout [ text "Sign out" ]
            ]

        Nothing ->
            [ linkTo Route.Login [ text "Sign in" ]
            , linkTo Route.Register [ text "Sign up" ]
            ]


viewFooter : Html msg
viewFooter =
    footer []
        [ div [ class "container" ]
            [ a [ class "logo-font", href "/" ] [ text "conduit" ]
            , span [ class "attribution" ]
                [ text "An interactive learning project from "
                , a [ href "https://thinkster.io" ] [ text "Thinkster" ]
                , text ". Code & design licensed under MIT."
                ]
            ]
        ]


navbarLink : NavbarIndicator -> Route -> List (Html msg) -> Navbar.Item msg
navbarLink navbarIndicator route linkContent =
    if isActive navbarIndicator route then
        Navbar.itemLinkActive [ Route.href route ] linkContent

    else
        Navbar.itemLink [ Route.href route ] linkContent


isActive : NavbarIndicator -> Route -> Bool
isActive navbarIndicator route =
    case ( navbarIndicator, route ) of
        ( Home, Route.Home ) ->
            True

        ( Login, Route.Login ) ->
            True

        ( Register, Route.Register ) ->
            True

        ( Settings, Route.Settings ) ->
            True

        ( Profile pageUsername, Route.Profile routeUsername ) ->
            pageUsername == routeUsername

        ( NewArticle, Route.NewArticle ) ->
            True

        _ ->
            False


{-| Render dismissable errors. We use this all over the place!
-}
viewErrors : msg -> List String -> Html msg
viewErrors dismissErrors errors =
    if List.isEmpty errors then
        Html.text ""

    else
        div
            [ class "error-messages"
            , style "position" "fixed"
            , style "top" "0"
            , style "background" "rgb(250, 250, 250)"
            , style "padding" "20px"
            , style "border" "1px solid"
            ]
        <|
            List.map (\error -> p [] [ text error ]) errors
                ++ [ button [ onClick dismissErrors ] [ text "Ok" ] ]
