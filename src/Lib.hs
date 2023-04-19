{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib where

import           Control.Arrow ((&&&))
import           Control.Monad.State
import           Data.Aeson
import           Data.Foldable (toList)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Time
import           Data.Time.Calendar.Julian (fromJulian)
import           GHC.Generics (Generic)
import           Network.Mail.Mime
import           Network.Mail.SMTP

config_root :: FilePath
config_root = "/home/sandy/.socialrc"

config_people :: FilePath
config_people = "/home/sandy/.tino/social.cal"

data Config = Config
  { cfg_smtp_server :: String
  , cfg_email :: String
  , cfg_password :: String
  }
  deriving (Show, Read)

data Person = Person
  { p_name        :: Text
  , p_last_seen   :: Day
  , p_last_pinged :: Day
  , p_cadence     :: Integer
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Read)

people :: [Person]
people =
  [ Person "Rory"           (fromJulian 2022 1 1) (fromJulian 2023 1 23) 5
  ]

shouldPing :: Day -> Person -> Bool
shouldPing today p =
  diffDays today (max (p_last_pinged p) (p_last_seen p)) >= p_cadence p

mkMap :: [Person] -> Map Text Person
mkMap = M.fromList . fmap (p_name &&& id)

toContact :: Day -> State (Map Text Person) [Person]
toContact today = do
  people <- get
  let lapsed = filter (shouldPing today) $ toList people
      updated = fmap (\p -> p { p_last_pinged = today }) lapsed
  modify $ mappend $ mkMap updated
  pure updated


withData :: ([Person] -> IO [Person]) -> IO ()
withData f = do
  let fp = config_people
  !input <- fmap read $ readFile fp
  output <- f input
  -- output <- f people
  writeFile fp $ show output










main :: IO ()
main = do
  Config { .. } <- fmap read $ readFile config_root
  now <- getCurrentTime
  let today = utctDay now
  withData $ \peeps -> do
    let (a, out) = flip runState (mkMap peeps) $ toContact today
    when (not $ null a) $ do
      let me = Address Nothing $ T.pack cfg_email
      sendMailWithLoginTLS
          cfg_smtp_server
          cfg_email
          cfg_password
        $ simpleMail' me me ("People Digest for " <> T.pack (show today))
        $ LT.pack $ T.unpack
        $ T.unlines
        $ fmap p_name a
    pure $ toList out

