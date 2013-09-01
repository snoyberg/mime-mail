{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.Mail.Mime.SES
    ( sendMailSES
    , renderSendMailSES
    , SES (..)
    ) where

import Data.ByteString (ByteString)
import Network.Mail.Mime (Mail, renderMail')
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Conduit (httpLbs, Manager, parseUrl, requestHeaders, urlEncodedBody)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Data.Time (getCurrentTime)
import qualified Data.ByteString.Char8 as S8
import Crypto.HMAC
#if MIN_VERSION_cryptohash(0, 10, 0)
import Crypto.Hash.CryptoAPI (SHA256)
#else
import Crypto.Hash.SHA256 (SHA256)
#endif
import Data.ByteString.Base64 (encode)
import qualified Data.Serialize as S
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class (liftIO)

data SES = SES
    { sesFrom :: ByteString
    , sesTo :: [ByteString]
    , sesAccessKey :: ByteString
    , sesSecretKey :: ByteString
    }

renderSendMailSES :: (MonadBaseControl IO m, MonadResource m) => Manager -> SES -> Mail -> m ()
renderSendMailSES m ses mail = liftIO (renderMail' mail) >>= sendMailSES m ses

sendMailSES :: (MonadBaseControl IO m, MonadResource m) => Manager -> SES -> L.ByteString -> m ()
sendMailSES manager ses msg = do
    now <- liftIO getCurrentTime
    let date = S8.pack $ format now
        sig = makeSig date $ sesSecretKey ses
    req' <- liftIO $ parseUrl "https://email.us-east-1.amazonaws.com"
    let auth = S8.concat
            [ "AWS3-HTTPS AWSAccessKeyId="
            , sesAccessKey ses
            , ", Algorithm=HmacSHA256, Signature="
            , sig
            ]
    let req = req'
            { requestHeaders =
                [ ("Date", date)
                , ("X-Amzn-Authorization", auth)
                ]
            }
    _ <- flip httpLbs manager $ urlEncodedBody qs req
    return ()
  where
    qs =
          ("Action", "SendRawEmail")
        : ("Source", sesFrom ses)
        : ("RawMessage.Data", encode $ S8.concat $ L.toChunks msg)
        : zipWith mkDest [1 :: Int ..] (sesTo ses)
    mkDest num addr = (S8.pack $ "Destinations.member." ++ show num, addr)
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
