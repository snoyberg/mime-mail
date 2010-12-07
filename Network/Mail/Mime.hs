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
      -- * High-level 'Mail' creation
    , simpleMail
      -- * Utilities
    , randomString
    , quotedPrintable
    ) where

import qualified Data.ByteString.Lazy as L
import Blaze.ByteString.Builder.Char.Utf8
import Blaze.ByteString.Builder
import Data.Monoid
import System.Random
import Control.Arrow
import System.Process
import System.IO
import System.Exit
import Codec.Binary.Base64 (encode)
import Control.Monad ((<=<), forM)
import Data.List (intersperse)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.ByteString.Char8 ()
import Data.Bits ((.&.), shiftR)
import Data.Char (isAscii)
import Data.Word (Word8)

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
    { -- | All headers, including to, from subject.
      mailHeaders :: [(String, String)]
    -- | A list of different sets of alternatives. As a concrete example:
    --
    -- > mailParts = [ [textVersion, htmlVersion], [attachment1], [attachment1]]
    --
    -- Make sure when specifying alternatives to place the most preferred
    -- version last.
    , mailParts :: [Alternatives]
    }

-- | How to encode a single part. You should use 'Base64' for binary data.
data Encoding = None | Base64 | QuotedPrintableText | QuotedPrintableBinary

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
    , partHeaders :: [(String, String)]
    , partContent :: L.ByteString
    }

type Headers = [(String, String)]
type Pair = (Headers, Builder)

partToPair :: Part -> Pair
partToPair (Part contentType encoding disposition headers content) =
    (headers', builder)
  where
    headers' =
        ((:) ("Content-Type", contentType))
      $ (case encoding of
            None -> id
            Base64 -> (:) ("Content-Transfer-Encoding", "base64")
            QuotedPrintableText ->
                (:) ("Content-Transfer-Encoding", "quoted-printable")
            QuotedPrintableBinary ->
                (:) ("Content-Transfer-Encoding", "quoted-printable"))
      $ (case disposition of
            Nothing -> id
            Just fn ->
                (:) ("Content-Disposition", "attachment; filename=" ++ fn))
      $ headers
    builder =
        case encoding of
            None -> fromWrite16List writeByteString $ L.toChunks content
            Base64 -> fromWrite16List writeWord8 $ map (toEnum . fromEnum)
                    $ encode $ L.unpack content
            QuotedPrintableText -> quotedPrintable True content
            QuotedPrintableBinary -> quotedPrintable False content

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
        let (b, g_) = x g
            (bs, g__) = helper g_ xs
         in (b : bs, g__)
    ((finalHeaders, finalBuilder), g'') = showPairs "mixed" pairs' g'
    builder = mconcat
        [ mconcat $ map showHeader headers
        , showHeader ("MIME-Version", "1.0")
        , mconcat $ map showHeader finalHeaders
        , fromByteString "\n"
        , finalBuilder
        ]

showHeader :: (String, String) -> Builder
showHeader (k, v) = mconcat
    [ fromString k
    , fromByteString ": "
    , v''
    , fromByteString "\n"
    ]
  where
   v' = LT.pack v
   v'' = if needsEncodedWord v'
            then encodedWord v'
            else fromLazyText v'

showBoundPart :: Boundary -> ([(String, String)], Builder) -> Builder
showBoundPart (Boundary b) (headers, content) = mconcat
    [ fromByteString "--"
    , fromString b
    , fromByteString "\n"
    , mconcat $ map showHeader headers
    , fromByteString "\n"
    , content
    ]

showBoundEnd :: Boundary -> Builder
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

-- | A simple interface for generating an email with HTML and plain-text
-- alternatives and some file attachments.
--
-- Note that we use lazy IO for reading in the attachment contents.
simpleMail :: String -- ^ to
           -> String -- ^ from
           -> String -- ^ subject
           -> LT.Text -- ^ plain body
           -> LT.Text -- ^ HTML body
           -> [(String, FilePath)] -- ^ content type and path of attachments
           -> IO Mail
simpleMail to from subject plainBody htmlBody attachments = do
    as <- forM attachments $ \(ct, fn) -> do
        content <- L.readFile fn
        return (ct, fn, content)
    return Mail {
        mailHeaders =
            [ ("To", to)
            , ("From", from)
            , ("Subject", subject)
            ]
        , mailParts =
            [ Part "text/plain; charset=utf-8" QuotedPrintableText Nothing []
            $ LT.encodeUtf8 plainBody
            , Part "text/html; charset=utf-8" QuotedPrintableText Nothing []
            $ LT.encodeUtf8 htmlBody
            ] :
            (map (\(ct, fn, content) ->
                    [Part ct Base64 (Just fn) [] content]) as)
        }

-- | The first parameter denotes whether the input should be treated as text.
-- If treated as text, then CRs will be stripped and LFs output as CRLFs. If
-- binary, then CRs and LFs will be escaped.
quotedPrintable :: Bool -> L.ByteString -> Builder
quotedPrintable isText lbs =
    fst $ L.foldl' go (mempty, 0 :: Int) lbs
  where
    go (front, lineLen) w =
        (front `mappend` b, lineLen')
      where
        (lineLen', b)
            | w == 13 && isText = (lineLen, mempty) -- CR
            | w == 10 && isText = (0, fromByteString "\r\n")
            | w == 61 = helper 3 $ fromByteString "=3D"
            | 33 <= w && w <= 126 = helper 1 $ fromWord8 w
            | (w == 9 || w == 0x20) && lineLen < 75 = helper 1 $ fromWord8 w
            | w == 9 = (0, fromByteString "=09=\r\n")
            | w == 0x20 = (0, fromByteString "=20=\r\n")
            | otherwise = helper 3 escaped
        helper newLen bs
            | newLen + lineLen > 78 =
                (0, bs `mappend` fromByteString "=\r\n")
            | otherwise = (newLen + lineLen, bs)
        escaped = fromWord8 61 `mappend` hex (w `shiftR` 4)
                               `mappend` hex (w .&. 15)

hex :: Word8 -> Builder
hex x
    | x < 10 = fromWord8 $ x + 48
    | otherwise = fromWord8 $ x + 55

needsEncodedWord :: LT.Text -> Bool
needsEncodedWord = not . LT.all isAscii

encodedWord :: LT.Text -> Builder
encodedWord t = mconcat
    [ fromByteString "=?utf-8?Q?"
    , L.foldl' go mempty $ LT.encodeUtf8 t
    , fromByteString "?="
    ]
  where
    go front w = front `mappend` go' w
    go' 32 = fromWord8 95
    go' 95 = go'' 95
    go' 63 = go'' 63
    go' 61 = go'' 61
    go' w
        | 33 <= w && w <= 126 = fromWord8 w
        | otherwise = go'' w
    go'' w = fromWord8 61 `mappend` hex (w `shiftR` 4)
                          `mappend` hex (w .&. 15)
