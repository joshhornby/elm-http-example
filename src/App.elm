module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (..)
import List exposing (..)


---- MODEL ----


type alias Model =
    { users : WebData (List User)
    }


type alias User =
    { id : Int
    , avatar : String
    , firstName : String
    , lastName : String
    }


init : ( Model, Cmd Msg )
init =
    ( { users = RemoteData.NotAsked }, getUser )



---- UPDATE ----


type Msg
    = UsersResponse (WebData (List User))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsersResponse response ->
            ( { model | users = response }, Cmd.none )



---- VIEW ----


view : Model -> Html msg
view model =
    case model.users of
        NotAsked ->
            text "Initialising."

        Loading ->
            text "Loading."

        Failure err ->
            text ("Error: " ++ toString err)

        Success users ->
            viewUsers users


viewUsers : List User -> Html msg
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
