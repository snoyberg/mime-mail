{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Network.Mail.Mime.SES
    ( sendMailSES
    , renderSendMailSES
    , SES (..)
    , SESException (..)
    ) where

import Data.ByteString (ByteString)
import Network.Mail.Mime (Mail, renderMail')
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Enumerator
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Data.Time (getCurrentTime)
import qualified Data.ByteString.Char8 as S8
import Crypto.HMAC
import Crypto.Hash.SHA256 (SHA256)
import Data.ByteString.Base64 (encode)
import qualified Data.Serialize as S
import Control.Exception (Exception, throwIO)
import Data.Typeable (Typeable)
import Control.Monad (unless)

data SES = SES
    { sesFrom :: ByteString
    , sesTo :: [ByteString]
    , sesAccessKey :: ByteString
    , sesSecretKey :: ByteString
    }

renderSendMailSES :: SES -> Mail -> IO ()
renderSendMailSES ses mail = renderMail' mail >>= sendMailSES ses

sendMailSES :: SES -> L.ByteString -> IO ()
sendMailSES ses msg = do
    now <- getCurrentTime
    let date = S8.pack $ format now
        sig = makeSig date $ sesSecretKey ses
    req' <- parseUrl "https://email.us-east-1.amazonaws.com"
    let auth = S8.concat
            [ "AWS3-HTTPS AWSAccessKeyId="
            , sesAccessKey ses
            , ", Algorithm=HmacSHA256, Signature="
            , sig
            ]
    let req = req'
            { queryString = qs
            , requestHeaders =
                [ ("Date", date)
                , ("X-Amzn-Authorization", auth)
                ]
            }
    res <- withManager $ httpLbs req
    unless (statusCode res == 200) $ throwIO $ SESException res
  where
    qs =
          ("Action", Just "SendRawEmail")
        : ("Source", Just $ sesFrom ses)
        : ("RawMessage.Data", Just $ encode $ S8.concat $ L.toChunks msg)
        : zipWith mkDest [1 :: Int ..] (sesTo ses)
    mkDest num addr = (S8.pack $ "Destinations.member." ++ show num, Just addr)
    format = formatTime defaultTimeLocale "%a, %e %b %Y %H:%M:%S %z"

makeSig :: ByteString -> ByteString -> ByteString
makeSig payload key =
      encode
    $ S.encode
    $ hmac' (MacKey key) payload
      `asTypeOf` x
  where
    x :: SHA256
    x = undefined

data SESException = SESException Response
    deriving (Show, Typeable)
instance Exception SESException
