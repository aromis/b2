module B2View where

import B2Model exposing (..)

import Html exposing (Html, div, text, button, ul, li, a, input)
import Html.Attributes exposing (style, placeholder, value)
import Html.Events exposing (onClick, on, targetValue)

-- VIEW

newButton : Signal.Address Action -> Action -> String -> List Html
newButton address action label =
  [ button
    [ style [ ("margin-left", "2px") ] , onClick address action ]
    [ text label ]
  ]

resetButton : Signal.Address Action -> List Html
resetButton address = newButton address Reset "Reset"

stopButton : Signal.Address Action -> List Html
stopButton address = newButton address Stop "Stop"

searchString  : Search -> String
searchString search =
  case search of
    Found s   -> s
    NoSuch s  -> s
    Empty     -> ""

searchBox : Signal.Address Action -> Search -> List Html
searchBox address search =
  let bg = case search of
              Found _   -> "#90EE90"
              NoSuch _  -> "#CD5C5C"
              Empty     ->  "#E0FFFF"
  in
    [ input
      [ placeholder "Find"
      , value (searchString search)
      , style [ ( "background", bg ) ]
      , on "input" targetValue (\str -> Signal.message address (Find str))
      ]
      []
    ]

-- main page is a list of artists (un-expanded) below a reset button and search box
view : Signal.Address Action -> Model -> Html
view address model =
  let artists = List.map (viewArtist address model) model.artists
  in div [ style [ ("margin-left", "2em") ] ] <|
    searchBox address model.search
    ++ resetButton address
    ++ stopButton address
    ++ artists

-- each artist name is click sensitive and expands to reveal
-- that artist's albums
viewArtist : Signal.Address Action -> Model -> Artist -> Html
viewArtist address model artist =
  let albumList =  viewAlbums address model artist
  in
    ul [] <|
      li [ ]
         [a [ onClick address (Expand artist) ] [ text artist.name ] ]
      :: albumList

-- albums are clickable list items, emboldened if selected
viewAlbums : Signal.Address Action -> Model -> Artist -> List Html
viewAlbums address model artist =
  let
    highlight album =
      case model.selected of
        Nothing -> []
        Just a -> if a == album then [ style [("font-weight", "bold")] ] else [ ]

    viewAlbum address album =
      li ( highlight album ) [ a [ onClick address (Play album) ] [ text album ] ]
  in
    case model.expanded of
      Nothing -> []
      Just a -> if a.name == artist.name then
        [ ul [] ( List.map (viewAlbum address) artist.albums ) ]
      else []
