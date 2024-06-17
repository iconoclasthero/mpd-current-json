module Network.MPD.Parse
  ( getStatusItem
  , getTag
  , processSong
  , headMay
  , valueToStringMay
  , (.=?)
  ) where

import qualified Network.MPD as MPD
import Network.MPD
       ( Metadata(..), Song, PlaybackState(Stopped, Playing, Paused) )
import Data.Aeson ( Key, KeyValue(..), ToJSON )

{- | Extract a field from the returned MPD.Status data record.

This takes an @Either@ 'Network.MPD.MPDError' 'Network.MPD.Status'
value and a field label function @f@ as arguments. It returns @Just
(f st)@ if the input status is @Right st@, where @st@ is the
'Network.MPD.Status' value. This function helps to extract a
specific field from the @MPD.Status@ data record by providing the
corresponding field label function.  If the input status "@st@" is
not @Right st@, indicating an error, or the field label function is
not applicable, it returns @Nothing@.
-}
getStatusItem :: Either MPD.MPDError MPD.Status -> (MPD.Status -> a) -> Maybe a
getStatusItem (Right st) f = Just (f st)
getStatusItem _ _ = Nothing

{- | @Either@ check for the returned value of 'Network.MPD.currentSong',
then call 'processSong' or return @Nothing@.
-}
getTag :: Metadata -> Either a (Maybe Song) -> Maybe String
getTag t c =
  case c of
    Left _ -> Nothing
    Right song -> processSong t song

{- | Use 'Network.MPD.sgGetTag' to extract a @tag@ from a @song@, safely
get only the head item of the returned @Maybe@ list, then safely
convert it to a string.
-}
processSong :: Metadata -> Maybe Song -> Maybe String
processSong _ Nothing = Nothing
processSong tag (Just song) = do
  let tagVal = MPD.sgGetTag tag song
  valueToStringMay =<< (headMay =<< tagVal)

{- | Safely get the head of a list. Same as [Safe.headMay](Safe#headMay).
-}
headMay :: [a] -> Maybe a
headMay []    = Nothing
headMay (x:_) = Just x

{- | Convert 'Network.MPD.Value' to @String@ within a @Maybe@ context.

This @Value@ is from 'Network.MPD' and is basically the same as a
@String@ but used internally to store metadata values.

==== __Example__:

@
processSong :: Metadata -> Maybe Song -> Maybe String
processSong _ Nothing = Nothing
processSong tag (Just song) = do
  let tagVal = MPD.sgGetTag tag song
  valueToStringMay =<< (headMay =<< tagVal)
@

'MPD.sgGetTag' returns a @Maybe [Value]@. [libmpd](Network.MPD) also provides
'Network.MPD.toString' that can convert, along other types, a
'Network.MPD.Value' to a @String@.
-}
valueToStringMay :: MPD.Value -> Maybe String
valueToStringMay x = Just (MPD.toString x)

{- | Check if @Maybe v@ exists and is of type expected by
'Data.Aeson.object' as defined in 'Data.Aeson.Value', if it is return
both the @key@ and @value@ within the @Maybe@ context tied with
'Data.Aeson..='. This gives support to \'optional\' fields using
'Data.Maybe.catMaybes' that discard @Nothing@ values and is meant to
prevent creating JSON key/value pairs with @null@ values, e.g.:

@
jsonTags = object . catMaybes $
    [ "artist"  .=? artist
    , "album"   .=? album
    , "title"   .=? title
    ]
@

Where if a value on the right is @Nothing@ that key/value pair will
not be included in 'Data.Aeson.object' because of
'Data.Maybe.catMaybes'.
-}
(.=?) :: (KeyValue e a, ToJSON v) => Key -> Maybe v -> Maybe a
key .=? Just value = Just (key .= value)
_   .=? Nothing    = Nothing
