module App where

import Html exposing (..)
import Html.Events exposing (onClick)
import String exposing (..)
import StartApp
import List
import Http
import Json.Decode as Json exposing((:=))
import Task
import Effects exposing (Effects, Never)
import Debug

-- model
type alias Endpoint = {
  name: String
}

type alias Model = List Endpoint

testModel = [
      { name = "Test endpoint"},
      { name = "Another endpoint"}
  ]

init : (Model, Effects Action)
init = (testModel, getEndpoints)

type Action = GetEndpoints (Maybe (List Endpoint))

getEndpoints : Effects Action
getEndpoints =
  Http.get decodeEndpoint "http://localhost:9090/mj-endpoints"
    |> Task.toMaybe
    |> Task.map GetEndpoints
    |> Effects.task

decodeEndpoint : Json.Decoder (List Endpoint)
decodeEndpoint =
  let endpoint =
        Json.object1 (\title-> {name = title})
          ("Name" := Json.string)
  in
      Json.list endpoint

-- update

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    GetEndpoints endpoints ->
      let
        foo = 1 + Debug.log "number" 1
        in
      (
        Maybe.withDefault testModel endpoints,
        Effects.none
      )


renderEndpoint : Endpoint -> Html
renderEndpoint endpoint = li [] [ div [] [
      h2 [] [(text endpoint.name)]
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
