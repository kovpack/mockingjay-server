module App where

import Html exposing (..)
import Html.Events exposing (onClick)
import String exposing (..)
import StartApp.Simple exposing (start)
import List

type alias Endpoint = {
  name: String,
  uri: String
}

type alias Model = List Endpoint

initialModel = [
    { name = "Test endpoint", uri = "/hello"},
    { name = "Another endpoint", uri = "/hello/pants"}
  ]

type Action = Toggle

update : Action -> Model -> Model
update action model = model

renderEndpoint : Endpoint -> Html
renderEndpoint endpoint = li [] [ div [] [
      h2 [] [(text endpoint.name)],
      code [] [text endpoint.uri]
    ]
  ]

renderEndpoints : List Endpoint -> Html
renderEndpoints endpoints = ul [] (List.map renderEndpoint endpoints)

view : Signal.Address Action -> Model -> Html
view address model = div [] [
    h1 [] [text "Mockingjay"],
    renderEndpoints model
  ]


main =
  start
    { model = initialModel
    , update = update
    , view = view
    }
