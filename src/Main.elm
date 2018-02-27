module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (..)
import Random exposing (generate)
import Random.List exposing (shuffle)


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Card =
    { id : String
    , img : String
    , matchFound : Bool
    }


type alias Model =
    { cards : List Card
    , backImage : String
    }


cards : List Card
cards =
    [ { id = "finn", img = "https://avatars3.githubusercontent.com/u/14337958?v=4", matchFound = False }
    , { id = "aisha", img = "https://avatars3.githubusercontent.com/u/22300773?v=4", matchFound = False }
    , { id = "suha", img = "https://avatars1.githubusercontent.com/u/24496866?v=4", matchFound = False }
    ]


init : ( Model, Cmd Msg )
init =
    Model
        (interweave cards cards)
        ""
        ! []


type Msg
    = NoOp
    | ShuffleCards
    | ShuffledDeck (List Card)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShuffleCards ->
            ( model, generate ShuffledDeck (shuffle model.cards) )

        ShuffledDeck shuffledDeck ->
            { model | cards = shuffledDeck } ! []

        NoOp ->
            model ! []


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick ShuffleCards ] [ text "New Game" ]
        , div [ class "" ] (model.cards |> List.map renderCard)
        ]


renderCard : Card -> Html Msg
renderCard card =
    div [ class "h4 w4 center" ]
        [ img [ src card.img ] []
        ]
