module Main where

import           Test.Hspec
import           Text.SSML.Builder (def, prosody, prosody_rate, render, text)

spec :: Spec
spec =
  it "test document renders correctly" $
    render (prosody def { prosody_rate = Just 120 } (text "Hello world < & ąść")) `shouldBe`
      "<?xml version='1.0' ?><speak xmlns=\"http://www.w3.org/2001/10/synthesis\"><prosody rate=\"120%\">Hello world &lt; &amp; ąść</prosody></speak>"

main :: IO ()
main = hspec spec
