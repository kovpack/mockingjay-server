module Model where

import Json.Decode as Decode exposing (Decoder, (:=))
import Json.Encode as Encode
import Effects exposing (Effects, Never)
import Dict
import Task
import Http

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
  | InputResponseBody String

endpointFromInputs: Model -> Endpoint
endpointFromInputs model = model.newEndpoint

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
