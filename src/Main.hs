{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import qualified Network.MPD as MPD
{-import Network.MPD
       ( Metadata(..), PlaybackState(Stopped, Playing, Paused), Status(..), Song(..), Path )-}
import Network.MPD
    ( Metadata(..), PlaybackState(Stopped, Playing, Paused)
    , currentSong, status, withMPDEx
    , sgFilePath, sgLastModified
    , stState, stRepeat, stTime, stRandom, stSingle, stConsume
    , stSongPos, stPlaylistLength, stBitrate, stAudio, stError
    , stNextSongPos, stNextSongID
    , Path, toString, Song, Response, MPDError
    )
import Data.Maybe ( catMaybes, fromMaybe )
import Data.String (fromString)
import Data.Aeson (ToJSON, toJSON, object, Value(String), (.=), encode)
import Data.Aeson ( object, KeyValue(..) )
import Data.Aeson.Encode.Pretty ( encodePretty )
import qualified Data.ByteString.Lazy.Char8 as C
import Text.Printf ( printf )
import Options
    ( optsParserInfo, execParser, Opts(optPass, optHost, optPort) )

import Network.MPD.Parse ( getStatusItem
                             , getTag
                             , (.=?) )

instance ToJSON Path where
    toJSON path = String . fromString $ toString path

{- | Where the program connects to MPD and uses the helper functions to
extract values, organize them into a list of key/value pairs, make
them a 'Data.Aeson.Value' using 'Data.Aeson.object', then encode it to
a conventional JSON @ByteString@ with
'Data.Aeson.Encode.Pretty.encodePretty' for the pretty-print version.
-}
main :: IO ()
main = do
  opts <- execParser optsParserInfo

  csResponse <- MPD.withMPDEx (optHost opts) (optPort opts) (optPass opts) MPD.currentSong
  st <- MPD.withMPDEx (optHost opts) (optPort opts) (optPass opts) MPD.status

  let cs = case csResponse of
              Left _ -> Nothing
              Right song -> song

  let filename                   = fmap MPD.sgFilePath                cs
  let modified                   = cs >>= MPD.sgLastModified
  let songid                     = cs >>= MPD.sgId

  cs <- MPD.withMPDEx (optHost opts) (optPort opts) (optPass opts) MPD.currentSong

  let artist                     = getTag Artist                      cs
      artistSort                 = getTag ArtistSort                  cs
      album                      = getTag Album                       cs
      albumSort                  = getTag AlbumSort                   cs
      albumArtist                = getTag AlbumArtist                 cs
      albumArtistSort            = getTag AlbumArtistSort             cs
      title                      = getTag Title                       cs
      track                      = getTag Track                       cs
      name                       = getTag Name                        cs
      genre                      = getTag Genre                       cs
      date                       = getTag Date                        cs
      originalDate               = getTag OriginalDate                cs
      composer                   = getTag Composer                    cs
      performer                  = getTag Performer                   cs
      conductor                  = getTag Conductor                   cs
      work                       = getTag Work                        cs
      grouping                   = getTag Grouping                    cs
      comment                    = getTag Comment                     cs
      disc                       = getTag Disc                        cs
      label                      = getTag Label                       cs
      musicbrainz_Artistid       = getTag MUSICBRAINZ_ARTISTID        cs
      musicbrainz_Albumid        = getTag MUSICBRAINZ_ALBUMID         cs
      musicbrainz_Albumartistid  = getTag MUSICBRAINZ_ALBUMARTISTID   cs
      musicbrainz_Trackid        = getTag MUSICBRAINZ_TRACKID         cs
      musicbrainz_Releasetrackid = getTag MUSICBRAINZ_RELEASETRACKID  cs
      musicbrainz_Workid         = getTag MUSICBRAINZ_WORKID          cs

  let state :: Maybe String
      state = case getStatusItem st MPD.stState of
                Just ps -> case ps of
                             Playing -> Just "playing"
                             Paused  -> Just "paused"
                             Stopped -> Just "stopped"
                Nothing -> Nothing

      time = getStatusItem st MPD.stTime

      elapsed = case time of
        Just t -> case t of
                    Just (e, _) -> Just e
                    _           -> Nothing
        Nothing -> Nothing

      duration = case time of
        Just t -> case t of
                    Just (_, d) -> Just d
                    _           -> Nothing
        Nothing -> Nothing

      elapsedPercent :: Maybe Double
      elapsedPercent = case time of
        Just t -> case t of
                    Just t1 -> Just (read $ printf "%.2f" (uncurry (/) t1 * 100))
                    Nothing -> Just 0
        Nothing -> Nothing

      repeatSt       = getStatusItem st MPD.stRepeat
      randomSt       = getStatusItem st MPD.stRandom
      singleSt       = getStatusItem st MPD.stSingle
      consumeSt      = getStatusItem st MPD.stConsume
      pos            = getStatusItem st MPD.stSongPos
      id             = getStatusItem st MPD.stSongID
      playlistLength = getStatusItem st MPD.stPlaylistLength
      playlistNum    = getStatusItem st MPD.stPlaylistVersion
      bitrate        = getStatusItem st MPD.stBitrate
      audioFormat    = getStatusItem st MPD.stAudio
      errorSt        = getStatusItem st MPD.stError
      nextSong       = getStatusItem st MPD.stNextSongPos
      nextSongId     = getStatusItem st MPD.stNextSongID
      volume         = getStatusItem st MPD.stVolume
      xfadewidth     = getStatusItem st MPD.stXFadeWidth
      mixrampdb      = getStatusItem st MPD.stMixRampdB
      mixrampdelay   = getStatusItem st MPD.stMixRampDelay
      updatingdb     = getStatusItem st MPD.stUpdatingDb

  -- sgTags
  let jTags = object . catMaybes $
        [ "artist"                     .=? artist
        , "artist_sort"                .=? artistSort
        , "album"                      .=? album
        , "album_sort"                 .=? albumSort
        , "album_artist"               .=? albumArtist
        , "album_artist_sort"          .=? albumArtistSort
        , "modified"                   .=? modified
        , "time"                       .=? time
        , "duration"                   .=? duration
{-        , "songid"                     .=? songid -}
        , "title"                      .=? title
        , "track"                      .=? track
        , "name"                       .=? name
        , "genre"                      .=? genre
        , "date"                       .=? date
        , "original_date"              .=? originalDate
        , "composer"                   .=? composer
        , "performer"                  .=? performer
        , "conductor"                  .=? conductor
        , "work"                       .=? work
        , "grouping"                   .=? grouping
        , "comment"                    .=? comment
        , "disc"                       .=? disc
        , "label"                      .=? label
        , "musicbrainz_artistid"       .=? musicbrainz_Artistid
        , "musicbrainz_albumid"        .=? musicbrainz_Albumid
        , "musicbrainz_albumartistid"  .=? musicbrainz_Albumartistid
        , "musicbrainz_trackid"        .=? musicbrainz_Trackid
        , "musicbrainz_releasetrackid" .=? musicbrainz_Releasetrackid
        , "musicbrainz_workid"         .=? musicbrainz_Workid
        , "filename"                   .=? filename
        ]

  -- status
  let jStatus = object . catMaybes $
        [ "state"           .=? state
        , "repeat"          .=? repeatSt
        , "elapsed"         .=? elapsed
        , "duration"        .=? duration
        , "elapsed_percent" .=? elapsedPercent
        , "random"          .=? randomSt
        , "single"          .=? singleSt
        , "consume"         .=? consumeSt
        , "song_position"   .=? pos
        , "playlist_length" .=? playlistLength
        , "bitrate"         .=? bitrate
        , "audio_format"    .=? audioFormat
        , "error"           .=? errorSt
        , "nextSong"        .=? nextSong
        , "volume" .=? fmap (show . fromMaybe 0) volume
{-        , "nextSongId"      .=? nextSongId -}
        ]

  let jObject = object [ "tags"   .= jTags
                       , "status" .= jStatus ]

--  C.putStrLn $ encodePretty jObject

  C.putStrLn $ encode jObject
