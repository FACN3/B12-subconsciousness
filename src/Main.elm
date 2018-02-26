module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


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
    }


init : ( Model, Cmd Msg )
init =
    Model
        [ { id = "finn", img = "https://avatars3.githubusercontent.com/u/14337958?v=4", matchFound = False }
        , { id = "aisha", img = "https://avatars3.githubusercontent.com/u/22300773?v=4", matchFound = False }
        , { id = "suha", img = "https://avatars1.githubusercontent.com/u/24496866?v=4", matchFound = False }
        ]
        ! []


type Msg
    = NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []


view : Model -> Html Msg
view model =
    div []
        [ div [] (List.map renderCard model.cards)
        ]


renderCard : Card -> Html Msg
renderCard card =
    div [ class "h4 w4 center" ]
        [ img [ src card.img ] []
        ]
