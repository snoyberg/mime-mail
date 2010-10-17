{-# LANGUAGE OverloadedStrings #-}
module Yesod.Mail
    ( -- * Datatypes
      Boundary (..)
    , Mail (..)
    , Part (..)
    , Encoding (..)
    , Disposition (..)
      -- * Render a message
    , renderMail
    , renderMail'
      -- * Sending messages
    , sendmail
    , renderSendMail
      -- * Utilities
    , randomString
    ) where

import qualified Data.ByteString.Lazy as L
import Text.Blaze.Builder.Utf8
import Text.Blaze.Builder.Core
import Data.Monoid
import System.Random
import Control.Arrow
import System.Process
import System.IO
import System.Exit
import Codec.Binary.Base64 (encode)
import Control.Monad ((<=<))

-- | Generates a random sequence of alphanumerics of the given length.
randomString :: RandomGen d => Int -> d -> (String, d)
randomString len =
    first (map toChar) . sequence' (replicate len (randomR (0, 61)))
  where
    sequence' [] g = ([], g)
    sequence' (f:fs) g =
        let (f', g') = f g
            (fs', g'') = sequence' fs g'
         in (f' : fs', g'')
    toChar i
        | i < 26 = toEnum $ i + fromEnum 'A'
        | i < 52 = toEnum $ i + fromEnum 'a' - 26
        | otherwise = toEnum $ i + fromEnum '0' - 52

-- | MIME boundary between parts of a message.
newtype Boundary = Boundary { unBoundary :: String }
instance Random Boundary where
    randomR = const random
    random = first Boundary . randomString 10

-- | An entire mail message.
data Mail = Mail
    { mailHeaders :: [(String, String)] -- ^ All headers, including to, from subject.
    , mailPlain :: String -- ^ Content before the MIME message begins, only seen by very old clients.
    , mailParts :: [Part]
    }

-- | How to encode a single part. You should use 'Base64' for binary data.
data Encoding = None | Base64

-- | Basically, 'Inline' is for things to be shown in the mail client, 'Attachment' for files to be downloaded.
data Disposition = Inline
                 | Attachment String -- ^ the name of the file

-- | A single part of a multipart message.
data Part = Part
    { partType :: String -- ^ content type
    , partEncoding :: Encoding
    , partDisposition :: Disposition
    , partContent :: L.ByteString
    }

-- | Render a 'Mail' with a given 'Boundary'.
--
-- You can also get a randomly-generated 'Boundary' by using renderMail'.
renderMail :: Boundary -> Mail -> L.ByteString
renderMail (Boundary b) (Mail headers plain parts) = toLazyByteString $ mconcat
    [ mconcat $ map showHeader headers
    , mconcat $ map showHeader
        [ ("MIME-Version", "1.0")
        , ("Content-Type", "multipart/mixed; boundary=\""
            ++ b ++ "\"")
        ]
    , fromByteString "\n"
    , fromString plain
    , mconcat $ map showPart parts
    , fromByteString "\n--"
    , fromString b
    , fromByteString "--"
    ]
  where
    showHeader (k, v) = mconcat
        [ fromString k
        , fromByteString ": "
        , fromString v
        , fromByteString "\n"
        ]
    showPart (Part contentType encoding disposition content) = mconcat
        [ fromByteString "\n--"
        , fromString b
        , fromByteString "\n"
        , showHeader ("Content-Type", contentType)
        , case encoding of
            None -> mempty
            Base64 -> showHeader ("Content-Transfer-Encoding", "base64")
        , case disposition of
            Inline -> mempty
            Attachment filename ->
                showHeader ("Content-Disposition", "attachment; filename=" ++ filename)
        , fromByteString "\n"
        , case encoding of
            None -> writeList writeByteString $ L.toChunks content
            Base64 -> writeList writeByte $ map (toEnum . fromEnum)
                    $ encode $ L.unpack content
        ]

-- | Like 'renderMail', but generates a random boundary.
renderMail' :: Mail -> IO L.ByteString
renderMail' m = do
    b <- randomIO
    return $ renderMail b m

-- | Send a fully-formed email message via the sendmail executable.
sendmail :: L.ByteString -> IO ()
sendmail lbs = do
    (Just hin, _, _, phandle) <- createProcess $ (proc
        "/usr/sbin/sendmail" ["-t"]) { std_in = CreatePipe }
    L.hPut hin lbs
    hClose hin
    exitCode <- waitForProcess phandle
    case exitCode of
        ExitSuccess -> return ()
        _ -> error $ "sendmail exited with error code " ++ show exitCode

-- | Render an email message and send via 'sendmail'.
renderSendMail :: Mail -> IO ()
renderSendMail = sendmail <=< renderMail'
