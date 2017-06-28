module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (..)
import List exposing (..)


---- MODEL ----


type FilterType
    = All
    | Odd
    | Even


type alias Model =
    { users : WebData (List User)
    , currentFilterType : FilterType
    }


type alias User =
    { id : Int
    , avatar : String
    , firstName : String
    , lastName : String
    }


init : ( Model, Cmd Msg )
init =
    ( { users = RemoteData.NotAsked, currentFilterType = All }, getUser )



---- UPDATE ----


type Msg
    = UsersResponse (WebData (List User))
    | Filter FilterType


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsersResponse response ->
            ( { model | users = response }, Cmd.none )

        Filter filter ->
            ( { model | currentFilterType = filter }, Cmd.none )



---- VIEW ----


filter : FilterType -> List User -> List User
filter filterType users =
    case filterType of
        All ->
            users

        Odd ->
            List.filter (\user -> user.id % 2 /= 0) users

        Even ->
            List.filter (\user -> user.id % 2 == 0) users


view : Model -> Html Msg
view model =
    case model.users of
        NotAsked ->
            text "Initialising."

        Loading ->
            text "Loading."

        Failure err ->
            text ("Error: " ++ toString err)

        Success users ->
            div []
                [ viewFilters users
                , viewUsers (filter model.currentFilterType users)
                ]


viewFilters : List User -> Html Msg
viewFilters users =
    div []
        [ button [ onClick (Filter Odd) ] [ text "Odd" ]
        , button [ onClick (Filter Even) ] [ text "Even" ]
        , button [ onClick (Filter All) ] [ text "Clear" ]
        ]


viewUsers : List User -> Html Msg
viewUsers users =
    users
        |> List.map
            (\user ->
                div []
                    [ img [ src user.avatar ] []
                    , h2 [] [ text (user.firstName ++ " " ++ user.lastName) ]
                    ]
            )
        |> div []



---- HTTP ----


apiUrl : String -> String
apiUrl str =
    "https://reqres.in/api" ++ str


decodeUsers : Decoder (List User)
decodeUsers =
    Decode.at [ "data" ] (Decode.list decodeUser)


decodeUser : Decode.Decoder User
decodeUser =
    Decode.map4 User
        (Decode.field "id" Decode.int)
        (Decode.field "avatar" Decode.string)
        (Decode.field "first_name" Decode.string)
        (Decode.field "last_name" Decode.string)


getUser : Cmd Msg
getUser =
    Http.get (apiUrl "/users") decodeUsers
        |> RemoteData.sendRequest
        |> Cmd.map UsersResponse



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
