module B2 where

import StartApp.Simple exposing (start)
import Regex

-- MODEL & VIEW

import B2Model exposing(..)
import B2View exposing (..)

-- UPDATE HELPERS

-- if the current selection is clicked again, toggle it
-- or if new selection, set it
toggle : Maybe a -> a -> Maybe a
toggle a b = if a == (Just b) then Nothing else Just b

-- find all artists that match a certain (sub)string
-- it's easier to rewrite this for each model and search
-- than to create a polymorphous function
search : Model -> String -> Model
search model string =
  let
    r = Regex.caseInsensitive ( Regex.regex (Regex.escape string) )
    helper artist = Regex.contains r artist.name
    artists' = List.filter helper model.artists
    searchResult = if List.isEmpty artists' then (NoSuch string) else (Found string)
  in
    { model | artists = artists', search = searchResult }

-- implement actions requested
update : Action -> Model -> Model
update action model =
  case action of
    Expand artist ->
      { model | expanded = toggle model.expanded artist }
    Play album ->
      { model | selected = toggle model.selected album }
    Find str ->
      if "" == str then
        initialState
      else
        search model str
    Stop ->
      { model | selected = Nothing }
    Reset ->
      initialState

main =
  start { model = initialState, view = view, update = update }

hazel : Artist
hazel = initArtist "Askew Sisters" ["Red", "Rice", "Green"]
emily : Artist
emily = initArtist "Peking Bros" ["Foo", "Bar", "Baz"]

initialState : Model
initialState = { artists = [ hazel, emily ], selected = Nothing, expanded = Nothing, search = Empty }