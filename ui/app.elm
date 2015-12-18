module App where

import Html exposing (..)
import Html.Events exposing (onClick)
import String exposing (..)
import StartApp
import List
import Http
import Json.Decode as Decode exposing (Decoder, (:=))
import Task
import Effects exposing (Effects, Never)
import Debug

-- model
type alias Request = {
  uri: String
}

type alias Endpoint = {
  name: String,
  cdcDisabled: Bool,
  request: Request
}

type alias Model = List Endpoint

testModel = []
init : (Model, Effects Action)
init = (testModel, getEndpoints)

type Action = GetEndpoints (Maybe (List Endpoint))

getEndpoints : Effects Action
getEndpoints =
  Http.get decodeEndpoint "http://localhost:9090/mj-endpoints"
    |> Task.toMaybe
    |> Task.map GetEndpoints
    |> Effects.task

decodeEndpoint : Decode.Decoder (List Endpoint)
decodeEndpoint =
  let
    request = Decode.object1 Request

    endpoint =
        Decode.object3 Endpoint
        ("Name" := Decode.string)
        ("CDCDisabled" := Decode.bool)
        (
          Decode.object1 Request
          (Decode.at ["Request", "URI"] Decode.string)
        )
  in
      Decode.list endpoint

-- update

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    GetEndpoints endpoints ->
      (
        Maybe.withDefault testModel endpoints,
        Effects.none
      )


renderEndpoint : Endpoint -> Html
renderEndpoint endpoint = li [] [ div [] [
      h2 [] [(text endpoint.name)],
      code [] [(text "CDC Disabled: "), endpoint.cdcDisabled |> toString |> text],
      p [] [(text endpoint.request.uri)]
    ]
  ]

renderEndpoints : List Endpoint -> Html
renderEndpoints endpoints = ul [] (List.map renderEndpoint endpoints)

-- view

view : Signal.Address Action -> Model -> Html
view address model = div [] [
    h1 [] [text "Mockingjay"],
    renderEndpoints model
  ]


app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = []
    }

main =
  app.html

-- wtf does this do
port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
