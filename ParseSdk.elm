module ParseSdk exposing (init, Credentials)

import Http
import Json.Encode as JsonE
import Json.Decode as JsonD exposing ((:=))
import Task
import String


-- TYPES


type alias Create msg =
    String -> (Http.RawError -> msg) -> (Http.Response -> msg) -> JsonE.Value -> Cmd msg


type alias Query doc msg =
    String -> List ( String, JsonE.Value ) -> JsonD.Decoder doc -> (Http.Error -> msg) -> (List doc -> msg) -> Cmd msg


type alias ParseSdk doc msg =
    { create : Create msg
    , query : Query doc msg
    }


type alias Credentials =
    { appId : String
    , apiKey : String
    , url : String
    }



-- HELPERS


headers : Credentials -> List ( String, String )
headers creds =
    [ ( "X-Parse-Application-Id", creds.appId )
    , ( "X-Parse-REST-API-Key", creds.apiKey )
    ]


trailingSlash : String -> String
trailingSlash url =
    if String.endsWith "/" url then
        url
    else
        url ++ "/"


appendToURL : String -> String -> String
appendToURL url str =
    trailingSlash <| (trailingSlash url) ++ str


pathURL : String -> List String -> String
pathURL url pathList =
    List.foldl (flip appendToURL) url pathList



-- OBJECTS


create : Credentials -> Create msg
create credentials class onError onSucceed value =
    Http.send Http.defaultSettings
        { verb = "POST"
        , headers = ( "Content-Type", "application/json" ) :: (headers credentials)
        , url = pathURL credentials.url [ "classes", class ]
        , body = Http.string <| JsonE.encode 0 value
        }
        |> Task.perform onError onSucceed


{-| http://parse.com/docs/rest/guide#queries-query-constraints
  NOTE: Not used yet
-}
type alias Options =
    { order : Maybe (List String)
    , limit : Maybe Int
    , skip : Maybe Int
    , keys : Maybe (List String)
    , include : Maybe (List String)
    , count : Bool
    }


query : Credentials -> Query doc msg
query credentials class query decoder onError onSucceed =
    Http.send Http.defaultSettings
        { verb = "GET"
        , headers = ( "Content-Type", "application/json" ) :: (headers credentials)
        , url = pathURL credentials.url [ "classes", class ]
        , body = Http.string "hello"
        }
        |> Http.fromJson ("results" := JsonD.list decoder)
        |> Task.perform onError onSucceed


init : Credentials -> ParseSdk doc msg
init credentials =
    { create = create credentials
    , query = query credentials
    }
