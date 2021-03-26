module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, string)
import Json.Encode exposing (..)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type State
    = Failure
    | Loading
    | Success (List MyRecord)


type alias Model =
    { newTitle : String, newBoards : List String, answer : String, state : State }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { newTitle = "", newBoards = [], answer = "", state = Loading }, getBoards )



-- UPDATE


type Msg
    = LoadBoards
    | GotBoards (Result Http.Error (List MyRecord))
    | ChangedTitle String
    | SubmitForm
    | GotAnswer (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadBoards ->
            ( { model | state = Loading }, getBoards )

        GotBoards result ->
            case result of
                Ok records ->
                    ( { model | state = Success records }, Cmd.none )

                Err _ ->
                    ( { model | state = Failure }, Cmd.none )

        ChangedTitle value ->
            ( { model | newTitle = value }, Cmd.none )

        SubmitForm ->
            ( { model | newBoards = model.newTitle :: model.newBoards, newTitle = "" }, postBoard model.newTitle )

        GotAnswer result ->
            case result of
                Ok answer ->
                    ( { model | answer = answer }, getBoards )

                Err _ ->
                    ( { model | answer = "Failure" }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Boards: " ]
        , viewBoards model
        , div []
            [ input [ placeholder "New Board Title", value model.newTitle, onInput ChangedTitle ] []
            , button [ onClick SubmitForm ] [ text "Submit" ]
            ]
        , div [] (List.map renderBoard model.newBoards)
        ]


renderBoard : String -> Html Msg
renderBoard title =
    p [] [ text title ]


renderItem : MyRecord -> Html Msg
renderItem rec =
    p [] [ text (String.fromInt rec.id ++ " " ++ rec.title) ]


viewBoards : Model -> Html Msg
viewBoards model =
    case model.state of
        Failure ->
            div []
                [ text "I could not load boards for some reason. "
                , button [ onClick LoadBoards ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading..."

        Success records ->
            div []
                [ button [ onClick LoadBoards, style "display" "block" ] [ text "Reload Boards" ]
                , div [] (List.map renderItem records)
                ]



-- HTTP


type alias MyRecord =
    { id : Int
    , title : String
    }


recordDecoder : Decoder MyRecord
recordDecoder =
    Json.Decode.map2 MyRecord
        (field "id" Json.Decode.int)
        (field "title" Json.Decode.string)


recordListDecoder : Decoder (List MyRecord)
recordListDecoder =
    Json.Decode.list recordDecoder


recordsDecoder : Decoder (List MyRecord)
recordsDecoder =
    field "boards" recordListDecoder



-- Authorization: 'Bearer 123'


getBoards : Cmd Msg
getBoards =
    Http.request
        { method = "GET"
        , url = "https://trello-back.shpp.me/asadov/api/v1/board/"
        , expect = Http.expectJson GotBoards recordsDecoder
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.emptyBody
        , headers = [ Http.header "Authorization" "Bearer 123" ]
        }


answerDecoder : Decoder String
answerDecoder =
    field "result" Json.Decode.string


postBoard : String -> Cmd Msg
postBoard title =
    Http.request
        { method = "POST"
        , url = "https://trello-back.shpp.me/asadov/api/v1/board/"
        , expect = Http.expectJson GotAnswer answerDecoder
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.jsonBody (bodyVal title)
        , headers = [ Http.header "Authorization" "Bearer 123" ]
        }


bodyVal : String -> Json.Encode.Value
bodyVal title =
    Json.Encode.object
        [ ( "title", Json.Encode.string title )
        ]
