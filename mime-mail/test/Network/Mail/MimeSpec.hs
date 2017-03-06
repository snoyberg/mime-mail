{-# LANGUAGE OverloadedStrings #-}
module Network.Mail.MimeSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Network.Mail.Mime
import qualified Data.ByteString.Lazy.Char8 as L8
import Blaze.ByteString.Builder (toLazyByteString)
import Control.Monad (forM_)
import Data.Text.Lazy.Encoding (encodeUtf8)

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
                        [' ']
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
            let enc = "If you believe that truth=3Dbeauty, then surely mathematics is the most bea=\r\nutiful branch of philosophy=2E"
                dec = "If you believe that truth=beauty, then surely mathematics is the most beautiful branch of philosophy."
             in toLazyByteString (quotedPrintable True dec) `shouldBe` enc

        it "issue #17- as text" $
            let enc = "</a>=E3=81=AB=E3=81=A4=E3=81=84=E3=81=A6=E3=81=AE=E3=83=86=E3=82=B9=E3=83=\r\n=88"
                dec = encodeUtf8 "</a>についてのテスト"
             in toLazyByteString (quotedPrintable True dec) `shouldBe` enc

        it "issue #17- as binary" $
            let enc = "</a>=E3=81=AB=E3=81=A4=E3=81=84=E3=81=A6=E3=81=AE=E3=83=86=E3=82=B9=E3=83=\r\n=88"
                dec = encodeUtf8 "</a>についてのテスト"
             in toLazyByteString (quotedPrintable False dec) `shouldBe` enc

        it "concrete example: over 76 characters" $
            let orig = "\240\238\191aa\149aa\226a\235\255a=aa\SI\159a\187a\147aa\ACKa\184aaaaaa\191a\237aaaa\EM a"
                gen = toLazyByteString $ quotedPrintable True orig
             in if all (\l -> L8.length l <= 76) $ lines' gen
                    then True
                    else error $ show $ lines' gen

        it "issue #50" $
            let addr = Address (Just "Name_ (is): @hi?") "user@domain.tld"
                enc = "=?utf-8?Q?Name=5F_=28is=29=3A_=40hi=3F?= <user@domain.tld>"
             in renderAddress addr `shouldBe` enc

lines' :: L8.ByteString -> [L8.ByteString]
lines' =
    map stripCR . L8.lines
  where
    stripCR bs
        | L8.null bs = bs
        | L8.last bs == '\r' = L8.init bs
        | otherwise = bs
