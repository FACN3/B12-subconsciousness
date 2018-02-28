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
    , currentPlayer : Player
    , playerOneScore : Int
    , playerTwoScore : Int
    }


type UiLocked
    = Locked
    | Unlocked


type Player
    = PlayerOne
    | PlayerTwo
    | NoPlayer


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
        NoPlayer
        0
        0
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
                        , currentPlayer = NoPlayer
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
                        , currentPlayer =
                            case model.currentPlayer of
                                PlayerOne ->
                                    PlayerTwo

                                PlayerTwo ->
                                    PlayerOne

                                NoPlayer ->
                                    NoPlayer
                        , playerOneScore =
                            case model.currentPlayer of
                                PlayerOne ->
                                    (model.playerOneScore + 1)

                                PlayerTwo ->
                                    model.playerOneScore

                                NoPlayer ->
                                    0
                        , playerTwoScore =
                            case model.currentPlayer of
                                PlayerTwo ->
                                    (model.playerTwoScore + 1)

                                PlayerOne ->
                                    model.playerTwoScore

                                NoPlayer ->
                                    0
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
                , currentPlayer =
                    case model.currentPlayer of
                        PlayerOne ->
                            PlayerTwo

                        PlayerTwo ->
                            PlayerOne

                        NoPlayer ->
                            PlayerOne
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


header : Model -> Html Msg
header model =
    div [ class "tc" ]
        [ div [ hidden model.hideBtn ]
            [ h1 [ class "mt0 pt3 title-color title--text" ] [ text "Magic Match \x1F9D9" ]
            , h3 [ class "title-color custom--text tracked" ] [ text "You're going to have 1 sec to look at the cards, Good luck!" ]
            , button [ onClick ShuffleCards, class "btn-reset mt2 mb4 pv3 ph4 custom--text tracked f4 button-background bn b--none button-reset custom-white fw7 br3 " ] [ text "New Game" ]
            ]
        , div [ hidden (not model.hideBtn), class "ph7" ]
            [ div [ class "mt0 pt3 flex justify-between row items-center" ]
                [ div []
                    [ h1 [ class "player-1 mt0" ] [ text "Player 1" ]
                    , Html.span [ class "title-color f3" ] [ text ("Score : " ++ (toString model.playerOneScore)) ]
                    ]
                , div []
                    [ h1 [ class "player-2 mt0" ] [ text "Player 2" ]
                    , Html.span [ class "title-color f3" ] [ text ("Score : " ++ (toString model.playerTwoScore)) ]
                    ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div [ class "bg--custom-white" ]
        [ header model
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
                "https://theanalyticalcouchpotato.files.wordpress.com/2012/04/mtg-card-back1.jpg"
    in
        div [ class "w-25 pv2 tc" ]
            [ img
                [ onClick clickMsg
                , src cardUrl
                , class "h4 w4 br3"
                ]
                []
            ]
