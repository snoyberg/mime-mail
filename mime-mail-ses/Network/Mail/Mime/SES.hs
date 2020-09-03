{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP #-}
module Network.Mail.Mime.SES
    ( sendMailSES
    , sendMailSESWithResponse
    , sendMailSESGlobal
    , renderSendMailSES
    , renderSendMailSESGlobal
    , SES (..)
    , usEast1
    , usWest2
    , euWest1
    , SESException (..)
    ) where

import           Control.Exception           (Exception, throwIO)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Data.ByteString             (ByteString)
import           Data.ByteString.Base64      (encode)
import qualified Data.ByteString.Char8       as S8
import qualified Data.ByteString.Lazy        as L
import           Data.Conduit                (Sink, await, ($$), (=$))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as E
import           Data.Time                   (getCurrentTime)
import           Data.Typeable               (Typeable)
import           Data.XML.Types              (Content (ContentText), Event (EventBeginElement, EventContent))
import           Network.HTTP.Client         (Manager,
                                              requestHeaders, responseBody,
                                              responseStatus, urlEncodedBody,
                                              withResponse)
import           Network.HTTP.Client.Conduit (bodyReaderSource)
import           Network.HTTP.Types          (Status)
import           Network.HTTP.Client.TLS     (getGlobalManager)
import           Network.Mail.Mime           (Mail, renderMail')
import           Text.XML.Stream.Parse       (def, parseBytes)

import Network.Mail.Mime.SES.Internal

data SES = SES
    { sesFrom         :: !ByteString
    , sesTo           :: ![ByteString]
    , sesAccessKey    :: !ByteString
    , sesSecretKey    :: !ByteString
    , sesSessionToken :: !(Maybe ByteString)
    , sesRegion       :: !Text
    }
  deriving Show

renderSendMailSES :: MonadIO m => Manager -> SES -> Mail -> m ()
renderSendMailSES m ses mail = liftIO (renderMail' mail) >>= sendMailSES m ses

-- | @since 0.4.1
-- Same as 'renderSendMailSES' but uses the global 'Manager'.
renderSendMailSESGlobal :: MonadIO m => SES -> Mail -> m ()
renderSendMailSESGlobal ses mail = do
  mgr <- liftIO getGlobalManager
  renderSendMailSES mgr ses mail

sendMailSES :: MonadIO m => Manager -> SES 
            -> L.ByteString -- ^ Raw message data. You must ensure that
                            -- the message format complies with
                            -- Internet email standards regarding
                            -- email header fields, MIME types, and
                            -- MIME encoding.
            -> m ()
sendMailSES manager ses msg =
  sendMailSESWithResponse manager ses msg checkForError

-- | @since 0.4.3
-- Generalised version of 'sendMailSES' which allows customising the final return type.
sendMailSESWithResponse :: MonadIO m => Manager -> SES
                        -> L.ByteString
                        -> (Status -> Sink Event IO a)
                        -- ^ What to do with the HTTP 'Status' returned in the 'Response'.
                        -> m a
sendMailSESWithResponse manager ses msg onResponseStatus = liftIO $ do
    now <- getCurrentTime
    requestBase <- buildRequest (concat ["https://email.", T.unpack (sesRegion ses) , ".amazonaws.com"])
    let headers =
          [ ("Date", formatAmazonTime now)
          ]
          ++ case sesSessionToken ses of
               Just token -> [("X-Amz-Security-Token", token)]
               Nothing    -> []
    let tentativeRequest = urlEncodedBody qs $ requestBase {requestHeaders = headers}
        canonicalRequest = canonicalizeRequest tentativeRequest
        stringToSign = makeStringToSign "ses" now (E.encodeUtf8 (sesRegion ses)) canonicalRequest
        sig = makeSig "ses" now (E.encodeUtf8 (sesRegion ses)) (sesSecretKey ses) stringToSign
        authorizationString = makeAuthorizationString "ses" now (E.encodeUtf8 (sesRegion ses))
                              (patchedRequestHeaders tentativeRequest) (sesAccessKey ses) sig
        finalRequest = tentativeRequest {requestHeaders = ("Authorization", authorizationString): requestHeaders tentativeRequest}
    withResponse finalRequest manager $ \res ->
           bodyReaderSource (responseBody res)
        $$ parseBytes def
        =$ onResponseStatus (responseStatus res)
  where
    qs =
          ("Action", "SendRawEmail")
        : ("Source", sesFrom ses)
        : ("RawMessage.Data", encode $ S8.concat $ L.toChunks msg)
        : zipWith mkDest [1 :: Int ..] (sesTo ses)
    mkDest num addr = (S8.pack $ "Destinations.member." ++ show num, addr)

-- | @since 0.4.1
-- Same as 'sendMailSES' but uses the global 'Manager'.
sendMailSESGlobal :: MonadIO m => SES 
                  -> L.ByteString -- ^ Raw message data. You must ensure that
                                  -- the message format complies with
                                  -- Internet email standards regarding
                                  -- email header fields, MIME types, and
                                  -- MIME encoding.
                  -> m ()
sendMailSESGlobal ses msg = do
  mgr <- liftIO getGlobalManager
  sendMailSES mgr ses msg

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

usEast1 :: Text
usEast1 = "us-east-1"

usWest2 :: Text
usWest2 = "us-west-2"

euWest1 :: Text
euWest1 = "eu-west-1"
