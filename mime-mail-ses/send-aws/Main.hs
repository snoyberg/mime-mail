{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language CPP #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.IO as LazyText
import Data.Text.Encoding
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.Mail.Mime
import Network.Mail.Mime.SES
import Options.Applicative
import Options.Applicative.Types
import System.IO

#if MIN_VERSION_base(4, 11, 0)
#else
import           Data.Monoid ((<>))
#endif

main = do
  (Options {..}, manager) <- pure (,)
    <*> execParser obtainOptions
    <*> newManager tlsManagerSettings

  putStr "Enter AWS secret: "
  hFlush stdout
  secret <- Text.getLine
  putStrLn "Enter message below."
  message <- LazyText.getContents

  let letter = (emptyMail from)
        { mailTo = to
        , mailHeaders = [("Subject", subject)]
        , mailParts = [[plainPart message]]
        }
  renderSendMailSES manager (makeSES letter key secret region) letter

data Options = Options
  { subject :: Text
  , from :: Address
  , to :: [Address]
  , key :: Text
  , region :: Text
  }

obtainOptions :: ParserInfo Options
obtainOptions = info (parseOptions <**> helper) (fullDesc <> progDesc "Send a message via Amazon Simple Email Service.")
  where
    parseOptions :: Parser Options
    parseOptions = pure Options
      <*> option readText (metavar "..." <> long "subject" <> help "The `subject` header of the letter.")
      <*> option readAddress (metavar "..." <> long "from" <> help "Source address.")
      <*> some (option readAddress (metavar "..." <> long "to" <> help "A destination."))
      <*> option readText (metavar "..." <> long "key" <> help "AWS access key identifier.")
      <*> option readText (metavar "..." <> long "region" <> help "AWS region to connect to.")

    -- Monomorphic readers are required in place of simple `strOption` ≡
    -- `option str` because `str` has been monomorphic prior to
    -- `optparse-applicative` version 0.14 and we want to support that.

    readText :: ReadM Text
    readText = Text.pack <$> readerAsk

    readAddress :: ReadM Address
    readAddress = Address Nothing <$> readText

makeSES :: Mail -> Text -> Text -> Text -> SES
makeSES Mail {..} key secret region = SES
  { sesFrom = (encodeUtf8 . addressEmail) mailFrom
  , sesTo = fmap (encodeUtf8 . addressEmail) mailTo
  , sesAccessKey = encodeUtf8 key
  , sesSecretKey = encodeUtf8 secret
  , sesSessionToken = Nothing
  , sesRegion = region
  }
