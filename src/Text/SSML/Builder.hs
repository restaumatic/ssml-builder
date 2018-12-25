-- | A simple wrapper for a subset of SSML.
-- <https://www.w3.org/TR/2010/REC-speech-synthesis11-20100907/>
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.SSML.Builder
  ( Document
  , render

  , def

  , prosody
  , prosody_rate

  , text
  ) where

import           Data.Default   (Default, def)
import           Data.Semigroup (Semigroup)
import           GHC.Generics   (Generic)
import qualified Text.XML.Light as X

newtype Document = Document { unDocument :: [X.Content] } deriving (Semigroup, Monoid)

render :: Document -> String
render =
  X.showTopElement .
  X.add_attr (X.Attr (X.unqual "xmlns") "http://www.w3.org/2001/10/synthesis") .
  X.unode "speak" .
  unDocument

-- | Use 'def' to obtain a default value, then tweak it by settings record fields.
data Prosody = Prosody
  { prosody_rate :: Maybe Int
  } deriving (Generic)

instance Default Prosody

-- | A @<prosody>@ element.
prosody :: Prosody -> Document -> Document
prosody attrs (Document children) =
  Document
    [ X.Elem $
      maybe id
        (\rate -> X.add_attr (X.Attr (X.unqual "rate") (show rate ++ "%")))
        (prosody_rate attrs) $
      X.unode "prosody" children
    ]

text :: String -> Document
text str = Document [ X.Text (X.blank_cdata { X.cdData = str }) ]
