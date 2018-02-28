module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (..)
import Random exposing (generate)
import Random.List exposing (shuffle)
import Time exposing (..)
import Task exposing (..)
import Process exposing (..)


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Card =
    { id : String
    , img : String
    , matchFound : Bool
    , flipped : Bool
    , isClickable : UiLocked
    }


type alias Model =
    { cards : List Card
    , hideBtn : Bool
    , firstCard : Maybe Card
    , uiLocked : UiLocked
    }


type UiLocked
    = Locked
    | Unlocked


cards : List Card
cards =
    [ { id = "finn", img = "https://avatars3.githubusercontent.com/u/14337958?v=4", matchFound = False, flipped = False, isClickable = Locked }
    , { id = "aisha", img = "https://avatars3.githubusercontent.com/u/22300773?v=4", matchFound = False, flipped = False, isClickable = Locked }
    , { id = "suha", img = "https://avatars1.githubusercontent.com/u/24496866?v=4", matchFound = False, flipped = False, isClickable = Locked }
    , { id = "james-blonde", img = "https://avatars0.githubusercontent.com/u/25667270?v=4", matchFound = False, flipped = False, isClickable = Locked }
    , { id = "shireen", img = "https://avatars3.githubusercontent.com/u/22002193?v=4", matchFound = False, flipped = False, isClickable = Locked }
    , { id = "king", img = "https://avatars3.githubusercontent.com/u/25408167?v=4", matchFound = False, flipped = False, isClickable = Locked }
    , { id = "des-des", img = "https://avatars1.githubusercontent.com/u/12845233?v=4", matchFound = False, flipped = False, isClickable = Locked }
    , { id = "claire", img = "https://avatars2.githubusercontent.com/u/10425219?v=4", matchFound = False, flipped = False, isClickable = Locked }
    ]


init : ( Model, Cmd Msg )
init =
    Model
        (List.indexedMap
            (\i x -> { x | id = x.id ++ (toString i) })
         <|
            (interweave cards cards)
        )
        False
        Nothing
        Locked
        ! []


type Msg
    = NoOp
    | ShuffleCards
    | ShuffledDeck (List Card)
    | FlipBack
    | ShowCard Card
    | MatchFound String



-- | Reset


timeout : Msg -> Int -> Cmd Msg
timeout msg time =
    Process.sleep (Time.second * (toFloat time)) |> Task.perform (\_ -> msg)


gameOver : Model -> Bool
gameOver currentState =
    let
        cardsMatched =
            List.filter (\x -> not (x.matchFound || x.flipped)) currentState.cards
    in
        List.isEmpty cardsMatched


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MatchFound string ->
            case (gameOver model) of
                True ->
                    { model
                        | cards =
                            List.map
                                (\x -> { x | matchFound = False, isClickable = Locked, flipped = False })
                                model.cards
                        , hideBtn = False
                        , uiLocked = Locked
                    }
                        ! []

                False ->
                    { model
                        | firstCard = Nothing
                        , uiLocked = Unlocked
                        , cards =
                            List.map
                                (\x ->
                                    if x.img == string then
                                        { x | matchFound = True, isClickable = Locked }
                                    else
                                        x
                                )
                                model.cards
                    }
                        ! []

        ShowCard card ->
            let
                cmd =
                    case model.firstCard of
                        Just oldCard ->
                            if oldCard.img == card.img && oldCard.id /= card.id then
                                timeout (MatchFound oldCard.img) 1
                            else
                                timeout FlipBack 1

                        Nothing ->
                            Cmd.none
            in
                { model
                    | cards =
                        List.map
                            (\x ->
                                if x.id == card.id then
                                    { x | flipped = True }
                                else
                                    x
                            )
                            model.cards
                    , uiLocked =
                        case model.firstCard of
                            Just _ ->
                                Locked

                            Nothing ->
                                Unlocked
                    , firstCard =
                        case model.firstCard of
                            Just string ->
                                Nothing

                            Nothing ->
                                Just card
                }
                    ! [ cmd ]

        FlipBack ->
            { model
                | cards =
                    List.map
                        (\x -> { x | flipped = False, isClickable = Unlocked })
                        model.cards
                , firstCard = Nothing
                , uiLocked = Unlocked
            }
                ! []

        ShuffleCards ->
            ( model, generate ShuffledDeck (shuffle model.cards) )

        ShuffledDeck shuffledDeck ->
            { model
                | cards = List.map (\x -> { x | flipped = True }) shuffledDeck
                , hideBtn = True
                , uiLocked = Locked
            }
                ! [ (timeout FlipBack 2) ]

        NoOp ->
            model ! []


view : Model -> Html Msg
view model =
    div []
        [ div [ class "tc", hidden model.hideBtn ]
            [ button [ onClick ShuffleCards, class "btn-reset mv4 pv2 ph3 bg-green white br3 " ] [ text "New Game" ]
            ]
        , div [ class "flex w-100 justify-center items-center flex-wrap ph7 pv2" ] (model.cards |> List.map (renderCard model))
        ]


renderCard : Model -> Card -> Html Msg
renderCard model card =
    let
        clickMsg =
            case model.uiLocked of
                Locked ->
                    NoOp

                Unlocked ->
                    cardClick

        cardClick =
            case card.isClickable of
                Locked ->
                    NoOp

                Unlocked ->
                    ShowCard card

        cardUrl =
            if card.matchFound then
                "https://3.imimg.com/data3/TE/RL/MY-2020667/plain-grey-250x250.jpg"
            else if card.flipped then
                card.img
            else
                "/assets/Magic_card_back_EDITED.jpg"
    in
        div [ class "w-25 pv3 tc" ]
            [ img
                [ onClick clickMsg
                , src cardUrl
                , class "h4 w4 br2"
                ]
                []
            ]
