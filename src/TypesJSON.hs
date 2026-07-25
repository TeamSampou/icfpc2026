{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -Wno-missing-export-lists #-}

module TypesJSON where

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

data ProbStat =
  ProbStat
  { pstatId                :: String
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
  { pioInput :: Object
  , pioOutput :: Object
  , pioConstraints :: [String]
  }
  deriving (Show, Generic)

instance ToJSON ProbIO where
  toEncoding = genericToEncoding (customOptions "pio")

instance FromJSON ProbIO where
  parseJSON = genericParseJSON (customOptions "pio")

-- ------------------------------------------------------------------------

data ProbTestData =
   ProbTestData
  { ptdataName :: String
  , ptdataIn   :: [String]
  , ptdataOut  :: [String]
  }
  deriving (Show, Generic)

instance ToJSON ProbTestData where
  toEncoding = genericToEncoding (customOptions "ptdata")

instance FromJSON ProbTestData where
  parseJSON = genericParseJSON (customOptions "ptdata")

-- ------------------------------------------------------------------------

data Problem =
  Problem
  { probId             :: String
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
