{-# LANGUAGE OverloadedStrings #-}
module Network.Mail.MimeSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Network.Mail.Mime
import qualified Data.ByteString.Lazy.Char8 as L8
import Blaze.ByteString.Builder (toLazyByteString)
import Control.Monad (forM_)

spec :: Spec
spec = describe "Network.Mail.Mime" $ do
    describe "quotedPrintable" $ do
        it "doesn't generate lines longer than 76 characters" $ do
            let lbs = toLazyByteString
                    $ quotedPrintable True (L8.replicate 1000 'x')
            forM_ (lines' lbs) $ (\l -> L8.length l `shouldSatisfy` (<= 76))

lines' :: L8.ByteString -> [L8.ByteString]
lines' =
    map stripCR . L8.lines
  where
    stripCR bs
        | L8.null bs = bs
        | L8.last bs == '\r' = L8.init bs
        | otherwise = bs
