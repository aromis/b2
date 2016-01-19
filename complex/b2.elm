module B2 where

import ArtistEntry exposing(Artist)

import Html exposing (..)
import Html.Attributes exposing (style, placeholder, value)
import Html.Events exposing (on, targetValue, onClick)
import StartApp.Simple exposing (start)
import Signal exposing (Address)
import String exposing (toUpper, contains)
import List exposing (map, filter)

-- MODEL

type alias Playlist = List Artist

music : Playlist
music =
  [ { artist = "Askew Sisters", albums = ["One", "Two"] }
  , { artist = "AC/DC", albums = ["Back in Black", "Highway to Hell"] }
  , { artist = "Zymodel", albums = ["Bastard!"] }
  ]

artists: List String
artists = List.map (.artist >> toUpper) music

-- UPDATE

type Action = Reset | Find String 

update : Action -> List String -> List String
update action model =
  case action of
    Reset -> artists
    Find str -> List.filter ((toUpper str) |> contains) artists
      -- i.e. filter (String.contains (String.toUpper str)) artists
      
-- VIEW

type Visibility = Short | Full

showartist: Visibility -> String -> Html
showartist visibility a = 
  case visibility of
    Short -> li [] [Html.a [] [text a]]
    Full -> text "Yeah" 
     
view : Signal.Address Action -> List String -> Html
view address currentList =
  div []
    [ input
        [ placeholder "Find"
        , on "input" targetValue (\str -> Signal.message address (Find str))
        ]
        []    
    , div [] 
      (List.map (showartist Short) currentList)
    ]

main =
  start { model = artists, update = update, view = view }
  
