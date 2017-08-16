{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP #-}
module Network.Mail.Mime.SES
    ( sendMailSES
    , renderSendMailSES
    , SES (..)
    , usEast1
    , usWest2
    , euWest1
    , SESException (..)
    ) where

import           Control.Exception           (Exception, throwIO)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Crypto.Hash                 (Digest, SHA256, hmac,
                                              hmacGetDigest)
import           Data.Byteable               (toBytes)
import           Data.ByteString             (ByteString)
import           Data.ByteString.Base64      (encode)
import qualified Data.ByteString.Char8       as S8
import qualified Data.ByteString.Lazy        as L
import           Data.Conduit                (Sink, await, ($$), (=$))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time                   (getCurrentTime)
import           Data.Time.Format            (formatTime)
import           Data.Typeable               (Typeable)
import           Data.XML.Types              (Content (ContentText), Event (EventBeginElement, EventContent))
import           Network.HTTP.Client         (Manager,
#if MIN_VERSION_http_client(0, 5, 0)
                                              parseRequest,
#else
                                              checkStatus,
                                              parseUrl,
#endif
                                              requestHeaders, responseBody,
                                              responseStatus, urlEncodedBody,
                                              withResponse)
import           Network.HTTP.Client.Conduit (bodyReaderSource)
import           Network.HTTP.Types          (Status)
import           Network.Mail.Mime           (Mail, renderMail')
import           Text.XML.Stream.Parse       (def, parseBytes)

#if MIN_VERSION_time(1,5,0)
import           Data.Time                   (defaultTimeLocale)
#else
import           System.Locale               (defaultTimeLocale)
#endif

data SES = SES
    { sesFrom         :: ByteString
    , sesTo           :: [ByteString]
    , sesAccessKey    :: ByteString
    , sesSecretKey    :: ByteString
    , sesSessionToken :: Maybe ByteString
    , sesRegion       :: Text
    }
  deriving Show

renderSendMailSES :: MonadIO m => Manager -> SES -> Mail -> m ()
renderSendMailSES m ses mail = liftIO (renderMail' mail) >>= sendMailSES m ses

sendMailSES :: MonadIO m => Manager -> SES -> L.ByteString -> m ()
sendMailSES manager ses msg = liftIO $ do
    now <- getCurrentTime
    let date = S8.pack $ format now
        sig = makeSig date $ sesSecretKey ses
        region = T.unpack $ sesRegion ses
#if MIN_VERSION_http_client(0, 5, 0)
    req' <- parseRequest $ concat ["https://email.", region , ".amazonaws.com"]
#else
    req' <- parseUrl $ concat ["https://email.", region , ".amazonaws.com"]
#endif
    let auth = S8.concat
            [ "AWS3-HTTPS AWSAccessKeyId="
            , sesAccessKey ses
            , ", Algorithm=HmacSHA256, Signature="
            , sig
            ]
    let req = urlEncodedBody qs $ req'
            { requestHeaders =
                [ ("Date", date)
                , ("X-Amzn-Authorization", auth)
                ] ++ case sesSessionToken ses of
                    Just token -> [("X-Amz-Security-Token", token)]
                    Nothing    -> []

#if !MIN_VERSION_http_client(0, 5, 0)
            , checkStatus = \_ _ _ -> Nothing
#endif
            }
    withResponse req manager $ \res ->
           bodyReaderSource (responseBody res)
        $$ parseBytes def
        =$ checkForError (responseStatus res)
  where
    qs =
          ("Action", "SendRawEmail")
        : ("Source", sesFrom ses)
        : ("RawMessage.Data", encode $ S8.concat $ L.toChunks msg)
        : zipWith mkDest [1 :: Int ..] (sesTo ses)
    mkDest num addr = (S8.pack $ "Destinations.member." ++ show num, addr)
    format = formatTime defaultTimeLocale "%a, %e %b %Y %H:%M:%S %z"

checkForError :: Status -> Sink Event IO ()
checkForError status = do
    name <- getFirstStart
    if name == errorResponse
        then loop "" "" ""
        else return ()
  where
    errorResponse = "{http://ses.amazonaws.com/doc/2010-12-01/}ErrorResponse"
    getFirstStart = do
        mx <- await
        case mx of
            Nothing -> return errorResponse
            Just (EventBeginElement name _) -> return name
            _ -> getFirstStart
    loop code msg reqid =
        await >>= maybe finish go
      where
        getContent front = do
            mx <- await
            case mx of
                Just (EventContent (ContentText t)) -> getContent (front . (t:))
                _ -> return $ T.concat $ front []
        go (EventBeginElement "{http://ses.amazonaws.com/doc/2010-12-01/}Code" _) = do
            t <- getContent id
            loop t msg reqid
        go (EventBeginElement "{http://ses.amazonaws.com/doc/2010-12-01/}Message" _) = do
            t <- getContent id
            loop code t reqid
        go (EventBeginElement "{http://ses.amazonaws.com/doc/2010-12-01/}RequestId" _) = do
            t <- getContent id
            loop code msg t
        go _ = loop code msg reqid
        finish = liftIO $ throwIO SESException
            { seStatus = status
            , seCode = code
            , seMessage = msg
            , seRequestId = reqid
            }

-- |
--
-- Exposed since: 0.3.2
data SESException = SESException
    { seStatus    :: !Status
    , seCode      :: !Text
    , seMessage   :: !Text
    , seRequestId :: !Text
    }
    deriving (Show, Typeable)
instance Exception SESException

makeSig :: ByteString -> ByteString -> ByteString
makeSig payload key =
    encode $ toBytes (hmacGetDigest $ hmac key payload :: Digest SHA256)

usEast1 :: Text
usEast1 = "us-east-1"

usWest2 :: Text
usWest2 = "us-west-2"

euWest1 :: Text
euWest1 = "eu-west-1"
