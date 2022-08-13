module Dex.Arguments (
  InputArgs(..),
  UserAccess(..),
  userAccess,
  mainOpts,
) where

-- General stuff for working with input values.

import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative

data UserAccess = UserAccess
  { uaName :: Text
  , uaPass :: Text
  } deriving (Eq)

-- | To handle all the possible inputs.
data InputArgs = InputArgs
  { iaUserAccess  :: Maybe UserAccess
  , iaKnownTokens :: Maybe FilePath
  , iaTimeCheck   :: Maybe Int
  , iaRunCandles  :: Bool
  } deriving stock (Eq)

userAccess :: Parser UserAccess
userAccess = UserAccess
  <$> strOption
    (  long "username"
    <> long "user"
    <> long "usr"
    <> short 'u'
    <> metavar "USER_NAME"
    <> help "Username for DB access."
    )
  <*> strOption
    (  long "password"
    <> long "pwd"
    <> short 'p'
    <> metavar "PWD"
    <> help "Password for DB access."
    )

knownTokens :: Parser FilePath
knownTokens = strOption
  (  long "known_tokens"
  <> long "tokens"
  <> long "token_database"
  <> long "token_store"
  <> long "metadata"
  <> short 'm'
  <> metavar "TOKEN_STORE.json"
  <> help "JSON-encoded list of known token metadata."
  )

timeCheck :: Parser Int
timeCheck = option auto
  (  long "time_check"
  <> long "time_interval"
  <> long "time"
  <> long "interval"
  <> short 't'
  <> metavar "MINUTES"
  <> help "How often (in minutes) to report the differenece between actual time and Tx time."
  )

candleCheck :: Parser Bool
candleCheck = flag' False
  (  long "no_candle"
  <> long "nocandle"
  <> long "nocandles"
  <> long "candles_off"
  <> short 'n'
  <> help "Do not try to create candlesticks; leave them to the DB (default)."
  )

candleCheck2 :: Parser Bool
candleCheck2 = flag' True
  (  long "candle"
  <> long "candles"
  <> long "candles_on"
  <> short 'c'
  <> help "Try to create candlesticks. Not recommended anymore."
  )

candleCheck3 :: Parser Bool
candleCheck3 = candleCheck <|> candleCheck2 <|> (pure False)

mainArgs :: Parser InputArgs
mainArgs = InputArgs
  <$> optional userAccess
  <*> optional knownTokens
  <*> optional timeCheck
  <*> candleCheck3

mainOpts :: ParserInfo InputArgs
mainOpts = info (mainArgs <**> helper) mempty
