module View where

import Html exposing (..)
import Model exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Dict

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
    field "textarea" address InputResponseBody "Response Body" model.newEndpoint.response.body,
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
