{-# LANGUAGE CPP, OverloadedStrings, RecordWildCards #-}
module Network.Mail.Mime
    ( -- * Datatypes
      Boundary (..)
    , Mail (..)
    , emptyMail
    , Address (..)
    , Alternatives
    , Part (..)
    , PartContent (..)
    , Disposition (..)
    , Encoding (..)
    , InlineImage(..)
    , ImageContent(..)
    , Headers
      -- * Render a message
    , renderMail
    , renderMail'
      -- * Sending messages
    , sendmail
    , sendmailCustom
    , sendmailCustomCaptureOutput
    , renderSendMail
    , renderSendMailCustom
      -- * High-level 'Mail' creation
    , simpleMail
    , simpleMail'
    , simpleMailInMemory
    , simpleMailWithImages
      -- * Utilities
    , addPart
    , addAttachment
    , addAttachments
    , addAttachmentBS
    , addAttachmentsBS
    , renderAddress
    , htmlPart
    , plainPart
    , filePart
    , filePartBS
    , randomString
    , quotedPrintable
    , relatedPart
    , addImage
    , mkImageParts
    ) where

import qualified Data.ByteString.Lazy as L
import Blaze.ByteString.Builder.Char.Utf8
import Blaze.ByteString.Builder
import Control.Concurrent (forkIO, putMVar, takeMVar, newEmptyMVar)
import Data.Monoid
import System.Random
import Control.Arrow
import System.Process
import System.IO
import System.Exit
import System.FilePath (takeFileName)
import qualified Data.ByteString.Base64 as Base64
import Control.Monad ((<=<), (>=>), foldM, void)
import Control.Exception (throwIO, ErrorCall (ErrorCall))
import Data.List (intersperse)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.ByteString.Char8 ()
import Data.Bits ((.&.), shiftR)
import Data.Char (isAscii, isControl)
import Data.Word (Word8)
import Data.String (IsString(..))
import qualified Data.ByteString as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

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
newtype Boundary = Boundary { unBoundary :: Text }
  deriving (Eq, Show)
instance Random Boundary where
    randomR = const random
    random = first (Boundary . T.pack) . randomString 10

-- | An entire mail message.
data Mail = Mail
    { mailFrom :: Address
    , mailTo   :: [Address]
    , mailCc   :: [Address]
    , mailBcc  :: [Address]
    -- | Other headers, excluding from, to, cc and bcc.
    , mailHeaders :: Headers
    -- | A list of different sets of alternatives. As a concrete example:
    --
    -- > mailParts = [ [textVersion, htmlVersion], [attachment1], [attachment1]]
    --
    -- Make sure when specifying alternatives to place the most preferred
    -- version last.
    , mailParts :: [Alternatives]
    }
  deriving Show

-- | A mail message with the provided 'from' address and no other
-- fields filled in.
emptyMail :: Address -> Mail
emptyMail from = Mail
    { mailFrom    = from
    , mailTo      = []
    , mailCc      = []
    , mailBcc     = []
    , mailHeaders = []
    , mailParts   = []
    }

data Address = Address
    { addressName  :: Maybe Text
    , addressEmail :: Text
    }
  deriving (Eq, Show)

instance IsString Address where
    fromString = Address Nothing . Data.String.fromString

-- | How to encode a single part. You should use 'Base64' for binary data.
data Encoding = None | Base64 | QuotedPrintableText | QuotedPrintableBinary
  deriving (Eq, Show)

-- | Multiple alternative representations of the same data. For example, you
-- could provide a plain-text and HTML version of a message.
type Alternatives = [Part]

-- | A single part of a multipart message.
data Part = Part
    { partType :: Text -- ^ content type
    , partEncoding :: Encoding
    -- | The filename for this part, if it is to be sent with an attachemnt
    -- disposition.
    , partDisposition :: Disposition
    , partHeaders :: Headers
    , partContent :: PartContent
    }
  deriving (Eq, Show)

-- | NestedParts are for multipart-related: One HTML part and some inline images
data PartContent = PartContent L.ByteString | NestedParts [Part]
  deriving (Eq, Show)

data Disposition = AttachmentDisposition Text
                 | InlineDisposition Text
                 | DefaultDisposition
                 deriving (Show, Eq)

type Headers = [(S.ByteString, Text)]

data Pair = Pair (Headers, Builder)
          | CompoundPair (Headers, [Pair])

partToPair :: Part -> Pair
partToPair (Part contentType encoding disposition headers (PartContent content)) =
    Pair (headers', builder)
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
            AttachmentDisposition fn ->
                (:) ("Content-Disposition", "attachment; filename=" `T.append` fn)
            InlineDisposition cid ->
                (:) ("Content-Disposition", "inline; filename=" `T.append` cid) . (:) ("Content-ID", "<" <> cid <> ">") . (:) ("Content-Location", cid)
            DefaultDisposition -> id
        )
      $ headers
    builder =
        case encoding of
            None -> fromWriteList writeByteString $ L.toChunks content
            Base64 -> base64 content
            QuotedPrintableText -> quotedPrintable True content
            QuotedPrintableBinary -> quotedPrintable False content
partToPair (Part contentType encoding disposition headers (NestedParts parts)) =
    CompoundPair (headers', pairs)
  where
    headers' = ("Content-Type", contentType):headers
    pairs = map partToPair parts


-- This function merges sibling pairs into a multipart pair
showPairs :: RandomGen g
          => Text -- ^ multipart type, eg mixed, alternative
          -> [Pair]
          -> g
          -> (Pair, g)
showPairs _ [] _ = error "renderParts called with null parts"
showPairs _ [pair] gen = (pair, gen)
showPairs mtype parts gen =
    (Pair (headers, builder), gen')
  where
    (Boundary b, gen') = random gen
    headers =
        [ ("Content-Type", T.concat
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

-- This function flattens any compound pairs into a multipart
-- related, but leaves other pairs in tact
-- NOTE that this is not recursive, and assumes only one level of nesting.
flattenCompoundPair :: RandomGen g => Pair -> g -> (Pair, g)
flattenCompoundPair pair@(Pair _) gen = (pair, gen)
flattenCompoundPair (CompoundPair (hs, pairs)) gen =
       (Pair (headers, builder), gen')
  where
    (Boundary b, gen') = random gen
    headers =
        [ ("Content-Type", T.concat
            [ "multipart/related" , "; boundary=\"" , b , "\"" ])
        ]
    builder = mconcat
        [ mconcat $ intersperse (fromByteString "\n")
                  $ map (showBoundPart $ Boundary b) pairs
        , showBoundEnd $ Boundary b
        ]


-- | Render a 'Mail' with a given 'RandomGen' for producing boundaries.
renderMail :: RandomGen g => g -> Mail -> (L.ByteString, g)
renderMail g0 (Mail from to cc bcc headers parts) =
    (toLazyByteString builder, g'')
  where
    addressHeaders = map showAddressHeader [("From", [from]), ("To", to), ("Cc", cc), ("Bcc", bcc)]
    -- parts is [Alternative], or [[Part]]
    -- reverse parts so attachments come at the end
    pairs :: [[Pair]]
    pairs = map (map partToPair) (reverse parts)

    (pairs1, g1) = helper2 g0 $ map (map flattenCompoundPair) pairs
    (pairs', g') = helper g1 $ map (showPairs "alternative") pairs1

    helper :: g -> [g -> (x, g)] -> ([x], g)
    helper g [] = ([], g)
    helper g (x:xs) =
        let (b, g_) = x g
            (bs, g__) = helper g_ xs
         in (b : bs, g__)

    -- new 2nd order helper
    helper2 :: g -> [[g -> (x, g)]] -> ([[x]], g)
    helper2 g [] = ([], g)
    helper2 g (x:xs) =
        let (b, g_) = helper g x  -- original helper
            (bs, g__) = helper2 g_ xs
         in (b : bs, g__)

    (Pair (finalHeaders, finalBuilder), g'') = showPairs "mixed" pairs' g'
    builder = mconcat
        [ mconcat addressHeaders
        , mconcat $ map showHeader headers
        , showHeader ("MIME-Version", "1.0")
        , mconcat $ map showHeader finalHeaders
        , fromByteString "\n"
        , finalBuilder
        ]

-- | Format an E-Mail address according to the name-addr form (see: RFC5322
-- § 3.4 "Address specification", i.e: [display-name] '<'addr-spec'>')
-- This can be handy for adding custom headers that require such format.
--
-- @since 0.4.11
renderAddress :: Address -> Text
renderAddress address =
    TE.decodeUtf8 $ toByteString $ showAddress address

-- Only accept characters between 33 and 126, excluding colons. [RFC2822](https://tools.ietf.org/html/rfc2822#section-2.2)
sanitizeFieldName :: S.ByteString -> S.ByteString
sanitizeFieldName = S.filter (\w -> w >= 33 && w <= 126 && w /= 58)

showHeader :: (S.ByteString, Text) -> Builder
showHeader (k, v) = mconcat
    [ fromByteString (sanitizeFieldName k)
    , fromByteString ": "
    , encodeIfNeeded (sanitizeHeader v)
    , fromByteString "\n"
    ]

showAddressHeader :: (S.ByteString, [Address]) -> Builder
showAddressHeader (k, as) =
  if null as
  then mempty
  else mconcat
    [ fromByteString k
    , fromByteString ": "
    , mconcat (intersperse (fromByteString ", ") . map showAddress $ as)
    , fromByteString "\n"
    ]

-- |
--
-- Since 0.4.3
showAddress :: Address -> Builder
showAddress a = mconcat
    [ maybe mempty ((<> fromByteString " ") . encodedWord) (addressName a)
    , fromByteString "<"
    , fromText (sanitizeHeader $ addressEmail a)
    , fromByteString ">"
    ]

-- Filter out control characters to prevent CRLF injection.
sanitizeHeader :: Text -> Text
sanitizeHeader = T.filter (not . isControl)

showBoundPart :: Boundary -> Pair -> Builder
showBoundPart (Boundary b) (Pair (headers, content)) = mconcat
    [ fromByteString "--"
    , fromText b
    , fromByteString "\n"
    , mconcat $ map showHeader headers
    , fromByteString "\n"
    , content
    ]

showBoundEnd :: Boundary -> Builder
showBoundEnd (Boundary b) = mconcat
    [ fromByteString "\n--"
    , fromText b
    , fromByteString "--"
    ]

-- | Like 'renderMail', but generates a random boundary.
renderMail' :: Mail -> IO L.ByteString
renderMail' m = do
    g <- getStdGen
    let (lbs, g') = renderMail g m
    setStdGen g'
    return lbs

-- | Send a fully-formed email message via the default sendmail
-- executable with default options.
sendmail :: L.ByteString -> IO ()
sendmail = sendmailCustom sendmailPath ["-t"]

sendmailPath :: String
#ifdef MIME_MAIL_SENDMAIL_PATH
sendmailPath = MIME_MAIL_SENDMAIL_PATH
#else
sendmailPath = "/usr/sbin/sendmail"
#endif

-- | Render an email message and send via the default sendmail
-- executable with default options.
renderSendMail :: Mail -> IO ()
renderSendMail = sendmail <=< renderMail'

-- | Send a fully-formed email message via the specified sendmail
-- executable with specified options.
sendmailCustom :: FilePath        -- ^ sendmail executable path
                  -> [String]     -- ^ sendmail command-line options
                  -> L.ByteString -- ^ mail message as lazy bytestring
                  -> IO ()
sendmailCustom sm opts lbs = void $ sendmailCustomAux False sm opts lbs

-- | Like 'sendmailCustom', but also returns sendmail's output to stderr and
-- stdout as strict ByteStrings.
--
-- Since 0.4.9
sendmailCustomCaptureOutput :: FilePath
                               -> [String]
                               -> L.ByteString
                               -> IO (S.ByteString, S.ByteString)
sendmailCustomCaptureOutput sm opts lbs = sendmailCustomAux True sm opts lbs

sendmailCustomAux :: Bool
                     -> FilePath
                     -> [String]
                     -> L.ByteString
                     -> IO (S.ByteString, S.ByteString)
sendmailCustomAux captureOut sm opts lbs = do
    let baseOpts = (proc sm opts) { std_in = CreatePipe }
        pOpts = if captureOut
                    then baseOpts { std_out = CreatePipe
                                  , std_err = CreatePipe
                                  }
                    else baseOpts
    (Just hin, mHOut, mHErr, phandle) <- createProcess pOpts
    L.hPut hin lbs
    hClose hin
    errMVar <- newEmptyMVar
    outMVar <- newEmptyMVar
    case (mHOut, mHErr) of
        (Nothing, Nothing) -> return ()
        (Just hOut, Just hErr) -> do
            void . forkIO $ S.hGetContents hOut >>= putMVar outMVar
            void . forkIO $ S.hGetContents hErr >>= putMVar errMVar
        _ -> error "error in sendmailCustomAux: missing a handle"
    exitCode <- waitForProcess phandle
    case exitCode of
        ExitSuccess -> if captureOut
            then do
                errOutput <- takeMVar errMVar
                outOutput <- takeMVar outMVar
                return (outOutput, errOutput)
            else return (S.empty, S.empty)
        _ -> throwIO $ ErrorCall ("sendmail exited with error code " ++ show exitCode)

-- | Render an email message and send via the specified sendmail
-- executable with specified options.
renderSendMailCustom :: FilePath    -- ^ sendmail executable path
                        -> [String] -- ^ sendmail command-line options
                        -> Mail     -- ^ mail to render and send
                        -> IO ()
renderSendMailCustom sm opts = sendmailCustom sm opts <=< renderMail'

-- FIXME usage of FilePath below can lead to issues with filename encoding

-- | A simple interface for generating an email with HTML and plain-text
-- alternatives and some file attachments.
--
-- Note that we use lazy IO for reading in the attachment contents.
simpleMail :: Address -- ^ to
           -> Address -- ^ from
           -> Text -- ^ subject
           -> LT.Text -- ^ plain body
           -> LT.Text -- ^ HTML body
           -> [(Text, FilePath)] -- ^ content type and path of attachments
           -> IO Mail
simpleMail to from subject plainBody htmlBody attachments =
      addAttachments attachments
    . addPart [plainPart plainBody, htmlPart htmlBody]
    $ mailFromToSubject from to subject

-- | A simple interface for generating an email with only plain-text body.
simpleMail' :: Address -- ^ to
            -> Address -- ^ from
            -> Text -- ^ subject
            -> LT.Text -- ^ body
            -> Mail
simpleMail' to from subject body = addPart [plainPart body]
                                 $ mailFromToSubject from to subject

-- | A simple interface for generating an email with HTML and plain-text
-- alternatives and some 'ByteString' attachments.
--
-- Since 0.4.7
simpleMailInMemory :: Address -- ^ to
           -> Address -- ^ from
           -> Text -- ^ subject
           -> LT.Text -- ^ plain body
           -> LT.Text -- ^ HTML body
           -> [(Text, Text, L.ByteString)] -- ^ content type, file name and contents of attachments
           -> Mail
simpleMailInMemory to from subject plainBody htmlBody attachments =
      addAttachmentsBS attachments
    . addPart [plainPart plainBody, htmlPart htmlBody]
    $ mailFromToSubject from to subject


data InlineImage = InlineImage {
      imageContentType :: Text
    , imageContent :: ImageContent
    , imageCID :: Text
    } deriving Show

data ImageContent = ImageFilePath FilePath | ImageByteString L.ByteString
  deriving Show

-- | An interface for generating an email with HTML and plain-text
-- alternatives, some file attachments, and inline images.
-- Note that we use lazy IO for reading in the attachment and inlined images.
-- Inline images can be referred to from the HTML content using
-- the @src="cid:{{CONTENT-ID}}"@ syntax, where CONTENT-ID is
-- the filename of the image.
--
-- Since 0.5.0
simpleMailWithImages :: [Address] -- ^ to (multiple)
           -> Address -- ^ from
           -> Text -- ^ subject
           -> LT.Text -- ^ plain body
           -> LT.Text -- ^ HTML body
           -> [InlineImage]
           -> [(Text, FilePath)] -- ^ content type and path of attachments
           -> IO Mail
simpleMailWithImages to from subject plainBody htmlBody images attachments = do
    inlineImageParts <- mkImageParts images
    addAttachments attachments
      . addPart [ plainPart plainBody
                , relatedPart ((htmlPart htmlBody):inlineImageParts) ]
      $ (emptyMail from) { mailTo = to, mailHeaders = [("Subject", subject)] }

mailFromToSubject :: Address -- ^ from
                  -> Address -- ^ to
                  -> Text -- ^ subject
                  -> Mail
mailFromToSubject from to subject =
    (emptyMail from) { mailTo = [to]
                     , mailHeaders = [("Subject", subject)]
                     }

-- | Add an 'Alternative' to the 'Mail's parts.
--
-- To e.g. add a plain text body use
-- > addPart [plainPart body] (emptyMail from)
addPart :: Alternatives -> Mail -> Mail
addPart alt mail = mail { mailParts = alt : mailParts mail }

-- | Add a 'Related' Part
relatedPart :: [Part] -> Part
relatedPart parts =
   Part "multipart/related" None DefaultDisposition [] (NestedParts parts)

-- | Construct a UTF-8-encoded plain-text 'Part'.
plainPart :: LT.Text -> Part
plainPart body = Part cType QuotedPrintableText DefaultDisposition []
    $ PartContent (LT.encodeUtf8 body)
  where cType = "text/plain; charset=utf-8"

-- | Construct a UTF-8-encoded html 'Part'.
htmlPart :: LT.Text -> Part
htmlPart body = Part cType QuotedPrintableText DefaultDisposition []
    $ PartContent (LT.encodeUtf8 body)
  where cType = "text/html; charset=utf-8"

-- | Construct a BASE64-encoded file attachment 'Part'
--
-- Since 0.5.0
filePart :: Text -> FilePath -> IO Part
filePart ct fn = do
    content <- L.readFile fn
    return $ filePartBS ct (T.pack (takeFileName fn)) content

-- | Construct a BASE64-encoded file attachment 'Part'
--
-- Since 0.5.0
filePartBS :: Text -> Text -> L.ByteString -> Part
filePartBS ct filename content = Part ct Base64 (AttachmentDisposition filename) [] (PartContent content)

-- | Add an attachment from a file and construct a 'Part'.
addAttachment :: Text -> FilePath -> Mail -> IO Mail
addAttachment ct fn mail = do
    part <- filePart ct fn
    return $ addPart [part] mail

addAttachments :: [(Text, FilePath)] -> Mail -> IO Mail
addAttachments xs mail = foldM fun mail xs
  where fun m (c, f) = addAttachment c f m

-- | Add an inline image from a file and construct a 'Part'.
--
-- Since 0.5.0
addImage :: InlineImage -> IO Part
addImage InlineImage{..} = do
    content <- case imageContent of
                ImageFilePath fn -> L.readFile fn
                ImageByteString bs -> return bs
    return
      $ Part imageContentType Base64 (InlineDisposition imageCID) [] (PartContent content)

mkImageParts :: [InlineImage] -> IO [Part]
mkImageParts xs =
    mapM addImage xs

-- | Add an attachment from a 'ByteString' and construct a 'Part'.
--
-- Since 0.4.7
addAttachmentBS :: Text -- ^ content type
                -> Text -- ^ file name
                -> L.ByteString -- ^ content
                -> Mail -> Mail
addAttachmentBS ct fn content mail = addPart [filePartBS ct fn content] mail

-- |
-- Since 0.4.7
addAttachmentsBS :: [(Text, Text, L.ByteString)] -> Mail -> Mail
addAttachmentsBS xs mail = foldl fun mail xs
  where fun m (ct, fn, content) = addAttachmentBS ct fn content m


data QP = QPPlain S.ByteString
        | QPNewline
        | QPTab
        | QPSpace
        | QPEscape S.ByteString

data QPC = QPCCR
         | QPCLF
         | QPCSpace
         | QPCTab
         | QPCPlain
         | QPCEscape
    deriving Eq

toQP :: Bool -- ^ text?
     -> L.ByteString
     -> [QP]
toQP isText =
    go
  where
    go lbs =
        case L.uncons lbs of
            Nothing -> []
            Just (c, rest) ->
                case toQPC c of
                    QPCCR -> go rest
                    QPCLF -> QPNewline : go rest
                    QPCSpace -> QPSpace : go rest
                    QPCTab -> QPTab : go rest
                    QPCPlain ->
                        let (x, y) = L.span ((== QPCPlain) . toQPC) lbs
                         in QPPlain (toStrict x) : go y
                    QPCEscape ->
                        let (x, y) = L.span ((== QPCEscape) . toQPC) lbs
                         in QPEscape (toStrict x) : go y

    toStrict = S.concat . L.toChunks

    toQPC :: Word8 -> QPC
    toQPC 13 | isText = QPCCR
    toQPC 10 | isText = QPCLF
    toQPC 9 = QPCTab
    toQPC 0x20 = QPCSpace
    toQPC 46 = QPCEscape
    toQPC 61 = QPCEscape
    toQPC w
        | 33 <= w && w <= 126 = QPCPlain
        | otherwise = QPCEscape

buildQPs :: [QP] -> Builder
buildQPs =
    go (0 :: Int)
  where
    go _ [] = mempty
    go currLine (qp:qps) =
        case qp of
            QPNewline -> copyByteString "\r\n" `mappend` go 0 qps
            QPTab -> wsHelper (copyByteString "=09") (fromWord8 9)
            QPSpace -> wsHelper (copyByteString "=20") (fromWord8 0x20)
            QPPlain bs ->
                let toTake = 75 - currLine
                    (x, y) = S.splitAt toTake bs
                    rest
                        | S.null y = qps
                        | otherwise = QPPlain y : qps
                 in helper (S.length x) (copyByteString x) (S.null y) rest
            QPEscape bs ->
                let toTake = (75 - currLine) `div` 3
                    (x, y) = S.splitAt toTake bs
                    rest
                        | S.null y = qps
                        | otherwise = QPEscape y : qps
                 in if toTake == 0
                        then copyByteString "=\r\n" `mappend` go 0 (qp:qps)
                        else helper (S.length x * 3) (escape x) (S.null y) rest
      where
        escape =
            S.foldl' add mempty
          where
            add builder w =
                builder `mappend` escaped
              where
                escaped = fromWord8 61 `mappend` hex (w `shiftR` 4)
                                       `mappend` hex (w .&. 15)

        helper added builder noMore rest =
            builder' `mappend` go newLine rest
           where
             (newLine, builder')
                | not noMore || (added + currLine) >= 75 =
                    (0, builder `mappend` copyByteString "=\r\n")
                | otherwise = (added + currLine, builder)

        wsHelper enc raw
            | null qps =
                if currLine <= 73
                    then enc
                    else copyByteString "\r\n=" `mappend` enc
            | otherwise = helper 1 raw (currLine < 76) qps

-- | The first parameter denotes whether the input should be treated as text.
-- If treated as text, then CRs will be stripped and LFs output as CRLFs. If
-- binary, then CRs and LFs will be escaped.
quotedPrintable :: Bool -> L.ByteString -> Builder
quotedPrintable isText = buildQPs . toQP isText

hex :: Word8 -> Builder
hex x
    | x < 10 = fromWord8 $ x + 48
    | otherwise = fromWord8 $ x + 55

encodeIfNeeded :: Text -> Builder
encodeIfNeeded t =
  if needsEncodedWord t
  then encodedWord t
  else fromText t

needsEncodedWord :: Text -> Bool
needsEncodedWord = not . T.all isAscii

encodedWord :: Text -> Builder
encodedWord t = mconcat
    [ fromByteString "=?utf-8?Q?"
    , S.foldl' go mempty $ TE.encodeUtf8 t
    , fromByteString "?="
    ]
  where
    go front w = front `mappend` go' w
    go' 32 = fromWord8 95 -- space
    go' 95 = go'' 95 -- _
    go' 63 = go'' 63 -- ?
    go' 61 = go'' 61 -- =

    -- The special characters from RFC 2822. Not all of these always give
    -- problems, but at least @[];"<>, gave problems with some mail servers
    -- when used in the 'name' part of an address.
    go' 34 = go'' 34 -- "
    go' 40 = go'' 40 -- (
    go' 41 = go'' 41 -- )
    go' 44 = go'' 44 -- ,
    go' 46 = go'' 46 -- .
    go' 58 = go'' 58 -- ;
    go' 59 = go'' 59 -- ;
    go' 60 = go'' 60 -- <
    go' 62 = go'' 62 -- >
    go' 64 = go'' 64 -- @
    go' 91 = go'' 91 -- [
    go' 92 = go'' 92 -- \
    go' 93 = go'' 93 -- ]
    go' w
        | 33 <= w && w <= 126 = fromWord8 w
        | otherwise = go'' w
    go'' w = fromWord8 61 `mappend` hex (w `shiftR` 4)
                          `mappend` hex (w .&. 15)

-- 57 bytes, when base64-encoded, becomes 76 characters.
-- Perform the encoding 57-bytes at a time, and then append a newline.
base64 :: L.ByteString -> Builder
base64 lbs
    | L.null lbs = mempty
    | otherwise = fromByteString x64 `mappend`
                  fromByteString "\r\n" `mappend`
                  base64 y
  where
    (x', y) = L.splitAt 57 lbs
    x = S.concat $ L.toChunks x'
    x64 = Base64.encode x
