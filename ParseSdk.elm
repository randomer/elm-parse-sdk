module ParseSdk exposing (create, find)

import Http
import Json.Encode as JsonE
import Json.Decode as JsonD exposing ((:=))
import Task
import String


-- TYPES


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


create : Credentials -> String -> JsonE.Value -> Task.Task Http.RawError Http.Response
create credentials class value =
    Http.send Http.defaultSettings
        { verb = "POST"
        , headers = ( "Content-Type", "application/json" ) :: (headers credentials)
        , url = pathURL credentials.url [ "classes", class ]
        , body = Http.string <| JsonE.encode 0 value
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


find : Credentials -> String -> List ( String, JsonE.Value ) -> JsonD.Decoder doc -> Task.Task Http.Error (List doc)
find credentials class query decoder =
    let
        resultsDecoder =
            ("results" := JsonD.list decoder)

        urlQuery =
            Http.url (pathURL credentials.url [ "classes", class ])
                [ ( "where", JsonE.encode 0 (JsonE.object query) ) ]
    in
        Http.get resultsDecoder urlQuery
