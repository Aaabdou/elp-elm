
module Test exposing (..)

import Browser
import Html exposing (Html, code, div, p, pre, span, text)
import Http
import Json.Decode as JD


type alias Model =
    { data : RemoteData Http.Error (List String) }


type RemoteData err a
    = NotAsked
    | Loading
    | Loaded a
    | Failed err


type Msg
    = GotUrls (Result Http.Error (List String))


initialModel =
    { data = NotAsked }


getUrls : Cmd Msg
getUrls =
    Http.get
        { url = "https://api.dictionaryapi.dev/api/v2/entries/en/hello"

        , expect = Http.expectJson GotUrls urlDecoder
        }


urlDecoder : JD.Decoder (List String)
urlDecoder =
    JD.list JD.string


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotUrls result ->
            case result of
                Ok strings ->
                    ( { model | data = Loaded strings }, Cmd.none )

                Err error ->
                    ( { model | data = Failed error }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        (case model.data of
            NotAsked ->
                [ text "no data yet" ]

            Loading ->
                [ text "loading" ]

            Failed err ->
                [ text "Failed... ", show err ]

            Loaded strings ->
                strings |> List.map (\s -> p [] [ text s ])
        )


show : Http.Error -> Html Msg
show error =
    case error of
        Http.BadUrl string ->
            span [] [ text "Bad Url: ", text string ]

        Http.Timeout ->
            text "Timeout"

        Http.NetworkError ->
            text "Network error"

        Http.BadStatus int ->
            span [] [ text "Bad Status: ", int |> String.fromInt |> text ]

        Http.BadBody string ->
            span [] [ text "Bad Body: ", pre [] [ code [] [ text string ] ] ]


main : Program () Model Msg
main =
    Browser.element
        { init = always ( initialModel, getUrls )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
