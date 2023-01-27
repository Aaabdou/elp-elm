module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (required)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)
import Random

type alias Model
  = {liste : List String
    ,worderr : Status
    ,dicerr:Status
    ,nb : Int
    ,dic :Dictionary
    ,win : Bool
  } 

type Status =Done 
            |Loading
            |Error String
            |Waiting

type Msg = SendHttpRequest 
        | DataReceived (Result Http.Error (List Dictionary) ) 
        | WordsReceived (Result Http.Error String)
        | Change String
        | NewRandomNumber Int
     
type alias Dictionary ={word :String,meanings : List Meaning}
type alias Meaning = { partOfSpeech : String, definitions : List Definition }
type alias Definition ={definition : String}


init : () -> (Model,Cmd Msg)
init _=({liste=[],worderr=Loading,dicerr=Waiting,nb=3,dic={word="",meanings=[]},win=False},getNicknames)

getNicknames : Cmd Msg
getNicknames =Http.get 
            {url ="http://localhost:8000/Documents/GitHub/elp-elm/static/1000.txt",
            expect = Http.expectString WordsReceived 
            }

url : String
url = "https://api.dictionaryapi.dev/api/v2/entries/en/"  

getWord :  List String->Int->String
getWord liste nb= case  List.head  (List.drop nb liste) of
        Just y -> y
        Nothing -> ""
getDef : String->Cmd Msg
getDef s=Http.get 
                    {url = "https://api.dictionaryapi.dev/api/v2/entries/en/" ++ s,
                    expect = Http.expectJson DataReceived gDecoder
                    }
        
        
        {-case  List.head  (List.drop m.nb m.liste) of
        Just y ->Http.get 
            {url =(url ++ y),
            expect = Http.expectJson DataReceived dicDecoder
            }
        Nothing -> Cmd.none
                (url ++ m.dic.word)  "https://api.dictionaryapi.dev/api/v2/entries/en/burn"  -}

gDecoder : Decoder (List Dictionary)
gDecoder=list dicDecoder
dicDecoder: Decoder Dictionary
dicDecoder= Json.Decode.succeed Dictionary
        |> required "word" string
        |> required "meanings" (list meaningDecoder)

meaningDecoder :Decoder Meaning
meaningDecoder = Json.Decode.succeed Meaning
        |> required "partOfSpeech" string
        |> required "definitions" (list defDecoder)
defDecoder : Decoder Definition
defDecoder =
    Json.Decode.succeed Definition
        |> required "definition" string



update : Msg -> Model -> (Model,Cmd Msg)
update msg model =
    case msg of
        SendHttpRequest->
            (model,getNicknames)
        WordsReceived result->
            case result of 
                Ok nicknamesStr ->
                    let
                        nicknames =
                            String.split " " nicknamesStr
                        
                    in 
                       ({liste =nicknames,worderr=Done,dicerr=Loading,nb=model.nb,dic={word="",meanings=[]},win=False},Random.generate NewRandomNumber (Random.int 0 999))
                        
                        
                Err httpError ->
                    ({liste =model.liste,worderr=Error (buildErrorMessage httpError),dicerr=Waiting ,nb=model.nb,dic={word="",meanings=[]},win=False},Cmd.none)
        DataReceived result->
            case result of 
                Ok defs ->
                    ({liste=model.liste,worderr=Done,dicerr=Done,nb=model.nb,dic=(test defs),win=False},Cmd.none)
                Err httpError ->
                    ({liste=model.liste,worderr=model.worderr,dicerr=Error (buildErrorMessage httpError),nb=model.nb,dic={word=model.dic.word,meanings=[]},win=False},Cmd.none)
        Change word->
            ({liste =model.liste,worderr=model.worderr,dicerr=model.dicerr,nb=model.nb,dic={word=model.dic.word,meanings=model.dic.meanings},win=word==model.dic.word},Cmd.none)
        NewRandomNumber y ->let z=getWord model.liste y
                        in case z of 
                            ""->({liste =model.liste,worderr=Error "Couldn't find word",dicerr=Waiting ,nb=y,dic={word="",meanings=[]},win=False},Cmd.none)
                            _->({liste =model.liste,worderr=Done,dicerr=Loading ,nb=y,dic={word=z,meanings=[]},win=False},getDef z)

            
                

{--}
test: List Dictionary -> Dictionary
test l= case  List.head  l of
        Just y ->y
        Nothing ->{word="",meanings=[]}


view : Model -> Html Msg
view model =
    div []
        [h1 [] [text "Find the wordaaaa"]
        ,viewDefsOrError model
        
        --,viewNicknames model
        ]

viewDefsOrError : Model -> Html Msg
viewDefsOrError model =
    case model.worderr of
        Error message ->
            viewError message "word"
        Done->case model.dicerr of
            Error message->div[][viewError message "def",viewNickname model.dic.word]
                        
            Done->if model.win then 
                        li[] [viewNickname model.dic.word,
                        ol [](viewDic model.dic.meanings)
                        ,viewInput model
                        ,viewWin model.dic.word]
                    else li[] [viewNickname model.dic.word,
                        ol [](viewDic model.dic.meanings)
                        ,viewInput model]
            Loading-> li[] [text "definition Loading"]
            Waiting-> li[] [text "defintition Waiting"]
        Loading-> li[] [text "word Loading"]
        Waiting-> li[] [text "word Waiting"]
            

viewWin:String->Html Msg
viewWin s=text ("You found the word,it was "++s)
viewError: String ->String-> Html Msg
viewError err a=div[]
        [ h3 [] [text "Error encountered"  ]
        , text ("Error " ++a++" : "++ err)
        ]
--[h3 [][text "Defintion"]]
viewDic: List Meaning -> List(Html Msg)
viewDic meanings = case meanings of
        []->[]
        (meaning :: rest)->li [] [text meaning.partOfSpeech, ol [](viewMeaning meaning.definitions)]  :: viewDic rest

viewMeaning: List Definition -> List (Html Msg)
viewMeaning defs= case defs of
        []->[]
        (definition :: rest)->li [] [viewDef definition] :: viewMeaning(rest)


viewDef : Definition -> Html Msg
viewDef def= case def.definition of
            ""->text "pas de def chacal"
            _->text def.definition
            
viewNicknames:Model ->Html Msg
viewNicknames m=ul[](List.map viewNickname m.liste)
viewNickname : String -> Html Msg
viewNickname s= li[][text s ] 
--(url++m.dic.word)

viewInput :  Model -> Html Msg
viewInput model= input [ placeholder "Guess the word", onInput Change ] []







{-[li [] [text meaning.partOfSpeech]] ++ [ol [] (List.map viewDefs meaning.definitions)]
viewDefs : List Definition -> List (Html Msg)
viewDefs defs= case defs of
    []->[]
    (def :: rest)->[ol [] (List.map viewDef defs)]++viewDefs rest-}


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
