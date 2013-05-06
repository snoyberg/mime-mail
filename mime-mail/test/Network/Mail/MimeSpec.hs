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
        it "under 76 in presence of terminating space" $ do
            let lbs = toLazyByteString
                    $ quotedPrintable True
                    $ L8.pack
                    $ foldr
                        (\a b -> b ++ replicate 74 'x' ++ [a])
                        ""
                        (" ")
            forM_ (lines' lbs) $ (\l -> L8.length l `shouldSatisfy` (<= 76))
        prop "always under 76 characters, text" $ \s ->
            let orig = L8.pack s
                gen = toLazyByteString $ quotedPrintable True orig
             in all (\l -> L8.length l <= 76) $ lines' gen
        prop "always under 76 characters, binary" $ \s ->
            let orig = L8.pack s
                gen = toLazyByteString $ quotedPrintable True orig
             in all (\l -> L8.length l <= 76) $ lines' gen

        it "example from Wikipedia" $
            let enc = "If you believe that truth=3Dbeauty, then surely mathematics is the most bea=\r\nutiful branch of philosophy."
                dec = "If you believe that truth=beauty, then surely mathematics is the most beautiful branch of philosophy."
             in toLazyByteString (quotedPrintable True dec) `shouldBe` enc

lines' :: L8.ByteString -> [L8.ByteString]
lines' =
    map stripCR . L8.lines
  where
    stripCR bs
        | L8.null bs = bs
        | L8.last bs == '\r' = L8.init bs
        | otherwise = bs
