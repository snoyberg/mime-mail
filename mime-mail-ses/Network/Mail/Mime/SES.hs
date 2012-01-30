{-# LANGUAGE OverloadedStrings #-}
module Network.Mail.Mime.SES
    ( sendMailSES
    , renderSendMailSES
    , SES (..)
    ) where

import Data.ByteString (ByteString)
import Network.Mail.Mime (Mail, renderMail')
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Conduit (httpLbs, Manager, parseUrl, queryString, requestHeaders)
import Network.HTTP.Types (renderQuery)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Data.Time (getCurrentTime)
import qualified Data.ByteString.Char8 as S8
import Crypto.HMAC
import Crypto.Hash.SHA256 (SHA256)
import Data.ByteString.Base64 (encode)
import qualified Data.Serialize as S
import Control.Monad.Trans.Resource (ResourceIO, ResourceT)
import Control.Monad.IO.Class (liftIO)

data SES = SES
    { sesFrom :: ByteString
    , sesTo :: [ByteString]
    , sesAccessKey :: ByteString
    , sesSecretKey :: ByteString
    }

renderSendMailSES :: ResourceIO m => Manager -> SES -> Mail -> ResourceT m ()
renderSendMailSES m ses mail = liftIO (renderMail' mail) >>= sendMailSES m ses

sendMailSES :: ResourceIO m => Manager -> SES -> L.ByteString -> ResourceT m ()
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
            { queryString = renderQuery False qs
            , requestHeaders =
                [ ("Date", date)
                , ("X-Amzn-Authorization", auth)
                ]
            }
    _ <- httpLbs req manager
    return ()
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
