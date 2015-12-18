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
import Dict

-- model
type alias Request = {
  uri: String,
  method: String,
  headers: Maybe (Dict.Dict String String),
  body: String
}

type alias Response = {
  status: Int,
  headers: Maybe (Dict.Dict String String),
  body: String
}

type alias Endpoint = {
  name: String,
  cdcDisabled: Bool,
  request: Request,
  response: Response
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
        Decode.object4 Endpoint
        ("Name" := Decode.string)
        ("CDCDisabled" := Decode.bool)
        (
          Decode.object4 Request
          (Decode.at ["Request", "URI"] Decode.string)
          (Decode.at ["Request", "Method"] Decode.string)
          (Decode.maybe (Decode.at ["Request", "Headers"] (Decode.dict Decode.string)))
          (Decode.at ["Request", "Body"] Decode.string)
        )
        (
          Decode.object3 Response
          (Decode.at ["Response", "Code"] Decode.int)
          (Decode.maybe (Decode.at ["Response", "Headers"] (Decode.dict Decode.string)))
          (Decode.at ["Response", "Body"] Decode.string)
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
      h3 [] [(text "Request")],
      p [] [(text endpoint.request.method), (text " "), (text endpoint.request.uri)],
      Maybe.withDefault (text "lol") (Maybe.map renderHeaders endpoint.request.headers), -- must be a better way!
      blockquote [] [(text endpoint.request.body)],
      h3 [] [(text "Response")],
      p [] [(endpoint.response.status |> toString |> text)],
      Maybe.withDefault (text "lol") (Maybe.map renderHeaders endpoint.response.headers), -- must be a better way!
      blockquote [] [(text endpoint.response.body)]
    ]
  ]

renderEndpoints : List Endpoint -> Html
renderEndpoints endpoints = ul [] (List.map renderEndpoint endpoints)

renderHeaders : Dict.Dict String String -> Html
renderHeaders headers =
  let
    itemRenderer : (String, String) -> Html
    itemRenderer (key, value) = li [] [(text key), (text " -> "), (text value)]
  in
    ul [] (List.map itemRenderer (Dict.toList headers))


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
