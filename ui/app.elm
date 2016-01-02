module App where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import String exposing (..)
import StartApp
import List
import Http
import Json.Decode as Decode exposing (Decoder, (:=))
import Json.Encode as Encode
import Task
import Effects exposing (Effects, Never)
import Debug
import Dict
import Debug

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

type alias Model = {
  endpoints: List Endpoint,
  newEndpoint: Endpoint
}

emptyEndpoint = {
    name = "",
    cdcDisabled = False,
    request = {
      uri = "",
      method = "GET",
      headers = Maybe.Nothing,
      body = ""
    },
    response = {
      status = 200,
      headers = Maybe.Nothing,
      body = ""
    }
  }

testModel = { endpoints = [], newEndpoint = emptyEndpoint }
init : (Model, Effects Action)
init = (testModel, getEndpoints)

type Action
  = GetEndpoints (Maybe (List Endpoint))
  | InputName String
  | InputRequestURI String
  | CreateEndpoint
  | EndpointCreated

endpointFromInputs: Model -> Endpoint
endpointFromInputs model =
  {
    name = model.newEndpoint.name,
    cdcDisabled = True,
    request = {
      uri = model.newEndpoint.request.uri,
      method = "GET",
      headers = Maybe.Nothing,
      body = ""
    },
    response = {
      status = 200,
      headers = Maybe.Nothing,
      body = "Hello from Elm"
    }
  }

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


encodeEndpoint: Endpoint -> String
encodeEndpoint endpoint =
  let
    endpointJSON = Encode.object
      [ ("Name", Encode.string endpoint.name),
        ("CDCDisabled", Encode.bool endpoint.cdcDisabled),
        ("Request", Encode.object [
          ("URI", Encode.string endpoint.request.uri),
          ("Method", Encode.string endpoint.request.method),
          ("Body", Encode.string endpoint.request.body)
        ]),
        ("Response", Encode.object [
          ("Code", Encode.int endpoint.response.status),
          ("Body", Encode.string endpoint.response.body)
        ])
      ]
  in
    Encode.encode 0 endpointJSON

createEndpointRequest: Endpoint -> Http.Request
createEndpointRequest endpoint =
  { verb = "POST"
  , headers = []
  , url = "http://localhost:9090/mj-new-endpoint"
  , body = Http.string (encodeEndpoint endpoint)
  }

-- update

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    GetEndpoints endpoints ->
      (
        Maybe.withDefault model (Maybe.map (\endpoint -> {model | endpoints = endpoint}) endpoints),
        Effects.none
      )
    InputName name ->
      let
        thingToUpdate = model.newEndpoint
        newEndpoint = {thingToUpdate | name = name}
      in
        ({model | newEndpoint = newEndpoint}, Effects.none)
    InputRequestURI uri ->
      let
        endpointToUpdate = model.newEndpoint
        requestToUpdate = endpointToUpdate.request
        updatedRequest = {requestToUpdate | uri = uri}
        updatedEndpoint = {endpointToUpdate | request = updatedRequest}
      in
        ({model | newEndpoint = updatedEndpoint}, Effects.none)
    EndpointCreated -> (model, Effects.none)
    CreateEndpoint ->
      let
        newEndpoint = (endpointFromInputs model)
        request =
          Http.send Http.defaultSettings (createEndpointRequest newEndpoint)
          |> Task.toMaybe
          |> Task.map (\result -> EndpointCreated)
          |> Effects.task

      in
        ({model | endpoints = newEndpoint :: model.endpoints}, request)

-- view

renderRequest : Request -> Html
renderRequest request = div [class "request"] [
    h2 [] [(text "Request")],
    p [] [(text request.method), (text " "), (text request.uri)],
    Maybe.withDefault (text "lol") (Maybe.map renderHeaders request.headers),
    blockquote [] [(text request.body)]
  ]

renderResponse : Response -> Html
renderResponse response = div [class "response"] [
    h3 [] [(text "Response")],
    p [] [(response.status |> toString |> text)],
    Maybe.withDefault (text "lol") (Maybe.map renderHeaders response.headers),
    blockquote [] [(text response.body)]
  ]

renderEndpoint : Endpoint -> Html
renderEndpoint endpoint = li [] [ div [] [
      h2 [] [(text endpoint.name)],
      code [] [(text "CDC Disabled: "), endpoint.cdcDisabled |> toString |> text],
      (renderRequest endpoint.request),
      (renderResponse endpoint.response)
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

renderAddForm : Signal.Address Action -> Model -> Html
renderAddForm address model = div [class "add-form"] [
    h2 [] [text "Create new endpoint"],
    field "text" address InputName "Endpoint name" model.newEndpoint.name,
    field "text" address InputRequestURI "Request URI" model.newEndpoint.request.uri,
    button [onClick address CreateEndpoint] [text "Create endpoint"]
  ]

field : String -> Signal.Address Action -> (String -> Action) -> String -> String -> Html
field fieldType address toAction name content =
  div []
    [ div [] [text name]
    , input
        [ type' fieldType
        , placeholder name
        , value content
        , on "input" targetValue (\string -> Signal.message address (toAction string))
        ]
        []
    ]

view : Signal.Address Action -> Model -> Html
view address model = div [class "mockingjay-wrap"] [
    h1 [] [text "Mockingjay"],
    renderAddForm address model,
    renderEndpoints model.endpoints
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
