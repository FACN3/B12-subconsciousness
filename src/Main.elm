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
    , flipped : Bool
    }


type alias Model =
    { cards : List Card
    , backImage : String
    , hideBtn : Bool
    }


cards : List Card
cards =
    [ { id = "finn", img = "https://avatars3.githubusercontent.com/u/14337958?v=4", matchFound = False, flipped = False }
    , { id = "aisha", img = "https://avatars3.githubusercontent.com/u/22300773?v=4", matchFound = False, flipped = False }
    , { id = "suha", img = "https://avatars1.githubusercontent.com/u/24496866?v=4", matchFound = False, flipped = False }
    , { id = "james-blonde", img = "https://avatars0.githubusercontent.com/u/25667270?v=4", matchFound = False, flipped = False }
    , { id = "shireen", img = "https://avatars3.githubusercontent.com/u/22002193?v=4", matchFound = False, flipped = False }
    , { id = "king", img = "https://avatars3.githubusercontent.com/u/25408167?v=4", matchFound = False, flipped = False }
    , { id = "des-des", img = "https://avatars1.githubusercontent.com/u/12845233?v=4", matchFound = False, flipped = False }
    , { id = "claire", img = "https://avatars2.githubusercontent.com/u/10425219?v=4", matchFound = False, flipped = False }
    ]


init : ( Model, Cmd Msg )
init =
    Model
        (interweave cards cards)
        "https://theanalyticalcouchpotato.files.wordpress.com/2012/04/mtg-card-back1.jpg"
        False
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
            { model | cards = List.map (\x -> { x | flipped = True }) shuffledDeck, hideBtn = True } ! []

        NoOp ->
            model ! []


view : Model -> Html Msg
view model =
    div []
        [ div [ class "tc", hidden model.hideBtn ]
            [ button [ onClick ShuffleCards, class "mv4" ] [ text "New Game" ]
            ]
        , div [ class "flex w-100 justify-center items-center flex-wrap ph7 pv2" ] (model.cards |> List.map renderCard)
        ]


renderCard : Card -> Html Msg
renderCard card =
    let
        cardUrl =
            if card.flipped then
                card.img
            else
                "https://d1u5p3l4wpay3k.cloudfront.net/mtgsalvation_gamepedia/7/7a/Magic_card_back_2.jpg?version=6d697ae1a0e6361ac10505af3c75387a"
    in
        div [ class "w-25 pv3 tc" ]
            [ img [ src cardUrl, class "h4 w4 br3" ] []
            ]
