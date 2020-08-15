{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP #-}

module Network.Mail.Mime.SES.Internal where

import           Crypto.Hash                 (SHA256, hmac, hmacGetDigest, hash)
import           Data.Bifunctor              (bimap)
import           Data.Byteable               (toBytes)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as B
import           Data.ByteString.Base16      as Base16
import qualified Data.ByteString.Char8       as S8
import qualified Data.ByteString.Lazy        as L
import           Data.Char                   (toLower)
import           Data.CaseInsensitive        (CI)
import qualified Data.CaseInsensitive        as CI
import           Data.List                   (sort)
#if MIN_VERSION_base(4, 11, 0)
#else
import           Data.Monoid ((<>))
#endif

import           Data.Time                   (UTCTime)
import           Data.Time.Format            (formatTime)
import           Network.HTTP.Client         (Request, RequestBody(RequestBodyLBS, RequestBodyBS),
#if MIN_VERSION_http_client(0, 5, 0)
                                             parseRequest,
#else
                                             checkStatus,
                                             parseUrl,
#endif
                                             method, host, path, requestHeaders, queryString, requestBody
                                             )
#if MIN_VERSION_time(1,5,0)
import           Data.Time                   (defaultTimeLocale)
#else
import           System.Locale               (defaultTimeLocale)
#endif

-- | Create a canonical request according to <https://docs.aws.amazon.com/general/latest/gr/sigv4-create-canonical-request.html>.
makeCanonicalRequest :: ByteString -> ByteString -> ByteString -> [(CI ByteString, ByteString)] -> ByteString -> ByteString
makeCanonicalRequest requesMethod requestPath requestQueryString headers payload = S8.intercalate "\n"
  [ requesMethod
  , requestPath
  , requestQueryString
  , S8.concat . fmap (\ (name, value) -> name <> ":" <> value <> "\n")
    . sort . fmap (bimap (bytesToLowerCase . CI.original) id)
    $ headers
  , makeListOfHeaders $ headers
  , unaryHashBase16 $ payload
  ]

canonicalizeRequest :: Request -> ByteString
canonicalizeRequest request
  = makeCanonicalRequest
  (method request)
  (path request)
  (queryString request)
  (patchedRequestHeaders request)
  (requestBodyAsByteString request)

-- | Create a string to sign according to <https://docs.aws.amazon.com/general/latest/gr/sigv4-create-string-to-sign.html>.
makeStringToSign :: ByteString -> UTCTime -> ByteString -> ByteString -> ByteString
makeStringToSign service time region canonicalRequest = S8.intercalate "\n"
  [ "AWS4-HMAC-SHA256"
  , formatAmazonTime time
  , makeCredentialScope service time region
  , unaryHashBase16 canonicalRequest
  ]

-- | Create a signature according to <https://docs.aws.amazon.com/general/latest/gr/sigv4-calculate-signature.html>.
makeSig :: ByteString -> UTCTime -> ByteString -> ByteString -> ByteString -> ByteString
makeSig service time region secret stringToSign =
  let f = flip keyedHash
  in Base16.encode
     . f stringToSign
     . f "aws4_request"
     . f service
     . f region
     . f (formatAmazonDate time)
     $ ("AWS4" <> secret)

-- | Create an authorization string according to <https://docs.aws.amazon.com/general/latest/gr/sigv4-add-signature-to-request.html>.
makeAuthorizationString :: ByteString -> UTCTime -> ByteString -> [(CI ByteString, ByteString)] -> ByteString -> ByteString -> ByteString
makeAuthorizationString service time region headers keyId sig = S8.concat
            [ "AWS4-HMAC-SHA256 Credential="
                <> keyId
                <> "/"
                <> makeCredentialScope service time region
            , ", SignedHeaders=" <> makeListOfHeaders headers
            , ", Signature=" <> sig
            ]

formatAmazonTime :: UTCTime -> ByteString
formatAmazonTime = S8.pack . formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ"

formatAmazonDate :: UTCTime -> ByteString
formatAmazonDate = S8.pack . formatTime defaultTimeLocale "%Y%m%d"

buildRequest :: String -> IO Request
buildRequest url = do
#if MIN_VERSION_http_client(0, 5, 0)
  requestBase <- (parseRequest url)
#else
  requestBase <- parseUrl url {checkStatus = \_ _ _ -> Nothing}
#endif
  return requestBase

requestBodyAsByteString :: Request -> ByteString
requestBodyAsByteString request = case requestBody request of
                                    RequestBodyBS x -> x
                                    RequestBodyLBS x -> L.toStrict x
                                    _ -> error "Not implemented."

requestBodyLength :: Request -> Int
requestBodyLength = B.length . requestBodyAsByteString

makeListOfHeaders :: [(CI ByteString, ByteString)] -> ByteString
makeListOfHeaders = S8.intercalate ";" . sort . fmap (bytesToLowerCase . CI.original . fst)

patchedRequestHeaders :: Request -> [(CI ByteString, ByteString)]
patchedRequestHeaders request = requestHeaders request ++
      [ (CI.mk "Host", host request)
      , (CI.mk "Content-Length", S8.pack . show $ requestBodyLength request)
      -- @http-client@ [adds the @Content-Length@ header automatically when sending the request](https://hackage.haskell.org/package/http-client-0.7.1/docs/Network-HTTP-Client.html#v:requestHeaders),
      -- so we have to reconstruct it by hand.
      ]

makeCredentialScope :: ByteString -> UTCTime -> ByteString -> ByteString
makeCredentialScope service time region = S8.intercalate "/" [formatAmazonDate time, region, service, "aws4_request"]

bytesToLowerCase :: ByteString -> ByteString
bytesToLowerCase = S8.pack . fmap toLower . S8.unpack

unaryHashBase16 :: ByteString -> ByteString
unaryHashBase16 = Base16.encode . toBytes . hash @SHA256

keyedHash :: ByteString -> ByteString -> ByteString
keyedHash key payload = toBytes . hmacGetDigest $ hmac @SHA256 key payload
