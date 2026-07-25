{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -Wno-missing-export-lists #-}

module TypesJSON where

import Control.Applicative
import Data.Char (toLower)
import Data.List (stripPrefix)
import GHC.Generics

import Data.Aeson

{- API 用の JSON 定義なので、変更するときは API 互換性に注意 -}

-- ------------------------------------------------------------------------

customOptions :: String -> Options
customOptions prefix =
  defaultOptions
  { fieldLabelModifier = \name ->
      case stripPrefix prefix name of
        Nothing -> name
        Just name' -> f name'
  }
  where
    f [] = []
    f (c:cs) = toLower c : cs

-- ------------------------------------------------------------------------

newtype ProblemId = ProblemId String
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

data ProbStat =
  ProbStat
  { pstatId                :: ProblemId
  , pstatSlug              :: String
  , pstatName              :: String
  , pstatProblemSetName    :: String
  , pstatProblemSetVisible :: Bool
  , pstatOrderInSet        :: Int
  , pstatStatus            :: String
  }
  deriving (Show, Generic)

instance ToJSON ProbStat where
  toEncoding = genericToEncoding (customOptions "pstat")

instance FromJSON ProbStat where
  parseJSON = genericParseJSON (customOptions "pstat")

-- ------------------------------------------------------------------------

data ProbIO =
  ProbIO
  { pioInput       :: Value
  , pioOutput      :: Maybe Value
  , pioDisplay     :: Maybe Value
  , pioConstraints :: Maybe [String]
  }
  deriving (Show, Generic)

instance ToJSON ProbIO where
  toEncoding = genericToEncoding ((customOptions "pio"){omitNothingFields = True})

instance FromJSON ProbIO where
  parseJSON = genericParseJSON (customOptions "pio")

-- ------------------------------------------------------------------------

data TDSingle =
  TDSingle
  { tdsingleName :: String
  , tdsingleIn   :: [String]
  , tdsingleOut  :: [String]
  }
  deriving (Show, Generic)

instance ToJSON TDSingle where
  toEncoding = genericToEncoding (customOptions "tdsingle")

instance FromJSON TDSingle where
  parseJSON = genericParseJSON (customOptions "tdsingle")

-- -----

data Round1 =
  Round1
  { round1In  :: [String]
  , round1Out :: [String]
  }
  deriving (Show, Generic)


instance ToJSON Round1 where
  toEncoding = genericToEncoding (customOptions "round1")

instance FromJSON Round1 where
  parseJSON = genericParseJSON (customOptions "round1")

-- -----

data TDRounds =
  TDRounds
  { tdroundsName :: String
  , tdroundsRounds :: [Round1]
  }
  deriving (Show, Generic)

instance ToJSON TDRounds where
  toEncoding = genericToEncoding (customOptions "tdrounds")

instance FromJSON TDRounds where
  parseJSON = genericParseJSON (customOptions "tdrounds")

-- -----

data ProbTestData
  = PTDSingle TDSingle
  | PTDRounds TDRounds
  deriving (Show, Generic)

instance ToJSON ProbTestData where
  toEncoding (PTDSingle single) = toEncoding single
  toEncoding (PTDRounds rounds) = toEncoding rounds

instance FromJSON ProbTestData where
  parseJSON v =
    PTDSingle <$> parseJSON v
    <|>
    PTDRounds <$> parseJSON v

-- ------------------------------------------------------------------------

data Problem =
  Problem
  { probId             :: ProblemId
  , probSlug           :: String
  , probName           :: String
  , probDescription    :: String
  , probExtraNotes     :: String
  , probIo             :: ProbIO
  , probProblemSetName :: String
  , probPublicTestData :: [ProbTestData]
  , probStatus         :: String
  , probScoring        :: String
  -- , probTickCap :: Maybe _
  , probPrivateTestCount :: Int
  }
  deriving (Show, Generic)

instance ToJSON Problem where
  toEncoding = genericToEncoding (customOptions "prob")

instance FromJSON Problem where
  parseJSON = genericParseJSON (customOptions "prob")

-- ------------------------------------------------------------------------

data Submit =
  Submit
  { submitProblemId :: ProblemId
  , submitProgram :: String
  }
  deriving (Show, Generic)

instance ToJSON Submit where
  toEncoding = genericToEncoding (customOptions "submit")

instance FromJSON Submit where
  parseJSON = genericParseJSON (customOptions "submit")

-- ------------------------------------------------------------------------
