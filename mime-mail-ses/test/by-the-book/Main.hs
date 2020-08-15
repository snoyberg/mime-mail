{-# language TypeApplications #-}
{-# language OverloadedStrings #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.CaseInsensitive (CI)
import Data.Time

import Test.Tasty
import Test.Tasty.HUnit
import Network.Mail.Mime.SES.Internal

-- Examples taken from <https://docs.aws.amazon.com/general/latest/gr/sigv4_signing.html>.

time :: UTCTime
time = read @UTCTime "2015-08-30 12:36:00 +0000"

service, secret, keyId, region, queryString, canonicalRequest, stringToSign, sig, authorizationString :: ByteString
service = "iam"
keyId = "AKIDEXAMPLE"
secret = "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
region = "us-east-1"
queryString = "Action=ListUsers&Version=2010-05-08"
canonicalRequest = ByteString.intercalate "\n"
  [ "GET"
  , "/"
  , "Action=ListUsers&Version=2010-05-08"
  , "content-type:application/x-www-form-urlencoded; charset=utf-8"
  , "host:iam.amazonaws.com"
  , "x-amz-date:20150830T123600Z"
  , ""
  , "content-type;host;x-amz-date"
  , "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
  ]

stringToSign = ByteString.intercalate "\n"
  [ "AWS4-HMAC-SHA256"
  , "20150830T123600Z"
  , "20150830/us-east-1/iam/aws4_request"
  , "f536975d06c0309214f805bb90ccff089219ecd68b2577efef23edd43b7e1a59"
  ]

sig = "5d672d79c15b13162d9279b0855cfba6789a8edb4c82c400e06b5924a6f2b5d7"

authorizationString = "AWS4-HMAC-SHA256 \
                      \Credential=AKIDEXAMPLE/20150830/us-east-1/iam/aws4_request\
                      \, SignedHeaders=content-type;host;x-amz-date\
                      \, Signature=5d672d79c15b13162d9279b0855cfba6789a8edb4c82c400e06b5924a6f2b5d7"

headers :: [(CI ByteString, ByteString)]
headers =
  [ ("Host", "iam.amazonaws.com")
  , ("Content-Type", "application/x-www-form-urlencoded; charset=utf-8")
  , ("X-Amz-Date", "20150830T123600Z")
  ]

main :: IO ( )
main = defaultMain $ testGroup "Check Sig Version 4 internals."
  [ testCase "Correctly generates a canonical request"
    $ assertEqual "" canonicalRequest (makeCanonicalRequest "GET" "/" queryString headers "")
  , testCase "Correctly generates a string to sign"
    $ assertEqual "" stringToSign (makeStringToSign service time region canonicalRequest)
  , testCase "Correctly generates a cryptographic signature"
    $ assertEqual "" sig (makeSig service time  region  secret stringToSign)
  , testCase "Correctly generates an authorization string"
    $ assertEqual "" authorizationString (makeAuthorizationString service time region headers keyId sig)
  ]
