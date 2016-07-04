module ParseAPI exposing (create, find)

import Http exposing (..)
import Json.Encode as Json
import Json.Decode as JD exposing ((:=))
import Task exposing (..)
import String


-- TYPES

type alias Credentials =
  { appId : String
  , apiKey : String
  , url : String
  }


-- HELPERS

headers : Credentials -> List (String, String)
headers creds =
  [ ("X-Parse-Application-Id", creds.appId)
  , ("X-Parse-REST-API-Key", creds.apiKey)
  ]

trailingSlash : String -> String
trailingSlash url =
  url ++ (if String.endsWith "/" url then "" else "/")

appendToURL : String -> String -> String
appendToURL url str =
  trailingSlash <| (trailingSlash url) ++ str

pathURL : String -> List String -> String
pathURL url pathList =
  List.foldl (flip appendToURL) url pathList


-- OBJECTS

create : Credentials -> String -> Json.Value -> Task Http.RawError Http.Response
create credentials class value =
  send defaultSettings
    { verb = "POST"
    , headers = ("Content-Type", "application/json") :: (headers credentials)
    , url = pathURL credentials.url ["classes", class]
    , body = string <| Json.encode 0 value
    }


-- QUERIES

type alias Options =
  { order : Maybe (List String)
  , limit : Maybe Int
  , skip : Maybe Int
  , keys : Maybe (List String)
  , include : Maybe (List String)
  , count : Bool
  }

find : Credentials -> String -> List (String, Json.Value) -> JD.Decoder doc -> Task Http.Error (List doc)
find credentials class query decoder =
  let
    resultsDecoder = ("results" := JD.list decoder)
    urlQuery =
      Http.url
        (pathURL credentials.url ["classes", class])
        [("where", Json.encode 0 (Json.object query))]
  in
    get resultsDecoder urlQuery
