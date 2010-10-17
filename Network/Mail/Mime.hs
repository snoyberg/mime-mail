{-# LANGUAGE OverloadedStrings #-}
module Network.Mail.Mime
    ( -- * Datatypes
      Boundary (..)
    , Mail (..)
    , Alternatives
    , Part (..)
    , Encoding (..)
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
import Data.List (intersperse)

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
    -- | A list of different sets of alternatives. As a concrete example:
    --
    -- > mailParts = [ [textVersion, htmlVersion], [attachment1], [attachment1]]
    --
    -- Make sure when specifying alternatives to place the most preferred
    -- version last.
    , mailParts :: [Alternatives]
    }

-- | How to encode a single part. You should use 'Base64' for binary data.
data Encoding = None | Base64

-- | Multiple alternative representations of the same data. For example, you
-- could provide a plain-text and HTML version of a message.
type Alternatives = [Part]

-- | A single part of a multipart message.
data Part = Part
    { partType :: String -- ^ content type
    , partEncoding :: Encoding
    -- | The filename for this part, if it is to be sent with an attachemnt
    -- disposition.
    , partFilename :: Maybe String
    , partContent :: L.ByteString
    }

type Headers = [(String, String)]
type Pair = (Headers, Builder)

partToPair :: Part -> Pair
partToPair (Part contentType encoding disposition content) =
    (headers, builder)
  where
    headers =
        ((:) ("Content-Type", contentType))
      $ (case encoding of
            None -> id
            Base64 -> (:) ("Content-Transfer-Encoding", "base64"))
      $ (case disposition of
            Nothing -> id
            Just fn ->
                (:) ("Content-Disposition", "attachment; filename=" ++ fn))
      $ []
    builder =
        case encoding of
            None -> writeList writeByteString $ L.toChunks content
            Base64 -> writeList writeByte $ map (toEnum . fromEnum)
                    $ encode $ L.unpack content

showPairs :: RandomGen g
          => String -- ^ multipart type, eg mixed, alternative
          -> [Pair]
          -> g
          -> (Pair, g)
showPairs _ [] _ = error "renderParts called with null parts"
showPairs _ [pair] gen = (pair, gen)
showPairs mtype parts gen =
    ((headers, builder), gen')
  where
    (Boundary b, gen') = random gen
    headers =
        [ ("Content-Type", concat
            [ "multipart/"
            , mtype
            , "; boundary=\""
            , b
            , "\""
            ])
        ]
    builder = mconcat
        [ mconcat $ intersperse (fromByteString "\n")
                  $ map (showBoundPart $ Boundary b) parts
        , showBoundEnd $ Boundary b
        ]

-- | Render a 'Mail' with a given 'RandomGen' for producing boundaries.
renderMail :: RandomGen g => g -> Mail -> (L.ByteString, g)
renderMail g0 (Mail headers parts) =
    (toLazyByteString builder, g'')
  where
    pairs = map (map partToPair) parts
    (pairs', g') = helper g0 $ map (showPairs "alternative") pairs
    helper :: g -> [g -> (x, g)] -> ([x], g)
    helper g [] = ([], g)
    helper g (x:xs) =
        let (b, g') = x g
            (bs, g'') = helper g' xs
         in (b : bs, g'')
    ((finalHeaders, finalBuilder), g'') = showPairs "mixed" pairs' g'
    builder = mconcat
        [ mconcat $ map showHeader headers
        , showHeader ("MIME-Version", "1.0")
        , mconcat $ map showHeader finalHeaders
        , fromByteString "\n"
        , finalBuilder
        ]

showHeader (k, v) = mconcat
    [ fromString k
    , fromByteString ": "
    , fromString v
    , fromByteString "\n"
    ]

showBoundPart (Boundary b) (headers, content) = mconcat
    [ fromByteString "--"
    , fromString b
    , fromByteString "\n"
    , mconcat $ map showHeader headers
    , fromByteString "\n"
    , content
    ]

showBoundEnd (Boundary b) = mconcat
    [ fromByteString "\n--"
    , fromString b
    , fromByteString "--"
    ]

-- | Like 'renderMail', but generates a random boundary.
renderMail' :: Mail -> IO L.ByteString
renderMail' m = do
    g <- getStdGen
    let (lbs, g') = renderMail g m
    setStdGen g'
    return lbs

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
