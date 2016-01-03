module App where

import Model exposing (..)
import View exposing (..)

import StartApp
import Http
import Task
import Effects exposing (Effects, Never)

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
    InputResponseBody body ->
      let
        endpointToUpdate = model.newEndpoint
        responseToUpdate = endpointToUpdate.response
        updatedResponse = {responseToUpdate | body = body}
        updatedEndpoint = {endpointToUpdate | response = updatedResponse}
      in
        ({model | newEndpoint = updatedEndpoint}, Effects.none)
    EndpointCreated -> (model, Effects.none)
    CreateEndpoint ->
      let
        request =
          Http.send Http.defaultSettings (createEndpointRequest model.newEndpoint)
          |> Task.toMaybe
          |> Task.map (\result -> EndpointCreated)
          |> Effects.task

      in
        ({model | endpoints = model.newEndpoint :: model.endpoints}, request)

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
