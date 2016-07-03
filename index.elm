import Html exposing (..)
import Html.App exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode exposing (..)
import Task

import Debug

import ParseAPI

main = Html.App.program
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }


type alias Model =
  { response : Maybe Http.Response
  , data : String
  }

initModel =
  { response = Nothing
  , data = ""
  }

init = (initModel, Cmd.none)

type Msg
  = GotResponse Http.Response
  | DataInput String
  | MakeRequest
  | ResponseError
  | ResponseSuccess

update msg model =
  case msg of
    GotResponse response ->
      { model | response = Just response } ! []
    DataInput data ->
      { model | data = data } ! []
    MakeRequest ->
      ( model, requestCmd model.data )
    ResponseError ->
      model ! []
    ResponseSuccess ->
      model ! []

creds =
  { appId = "123"
  , apiKey = "123"
  , url = "http://10.0.0.22:1337/"
  }

requestCmd : String -> Cmd Msg
requestCmd data =
  ParseAPI.create creds "scrape" (string data)
  |> Task.perform
    (\error -> Debug.log "error" ResponseError)
    (\response -> Debug.log "success" ResponseSuccess)

subscriptions model =
  case model of
    _ -> Sub.none

view model =
  div []
  [ input
    [ type' "text"
    , onInput (\ textInput -> DataInput textInput)
    ] []
  , button
    [ onClick MakeRequest
    ] [text "Make request"]
  ]
  