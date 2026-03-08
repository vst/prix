{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Prix.Commons where

import qualified Autodocodec as ADC
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Exit (ExitCode (..), die)
import System.IO (hPutStrLn, stderr)
import qualified System.Process.Typed as TP


-- * Owner


data Owner = MkOwner
  { ownerType :: !OwnerType
  , ownerLogin :: !T.Text
  , ownerUrl :: !T.Text
  , ownerAvatarUrl :: !(Maybe T.Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Owner)


instance ADC.HasCodec Owner where
  codec =
    ADC.object "Owner" $
      MkOwner
        <$> ADC.requiredField "type" "Owner Type" ADC..= ownerType
        <*> ADC.requiredField "login" "Owner Login" ADC..= ownerLogin
        <*> ADC.requiredField "url" "Owner URL" ADC..= ownerUrl
        <*> ADC.requiredField "avatarUrl" "Owner Avatar URL" ADC..= ownerAvatarUrl


data OwnerType
  = OwnerTypeUser
  | OwnerTypeOrganization
  deriving (Show, Eq, Generic, Enum, Bounded)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via ADC.Autodocodec OwnerType


instance ADC.HasCodec OwnerType where
  codec = ADC.boundedEnumCodec ownerTypeLabel


ownerTypeLabel :: OwnerType -> T.Text
ownerTypeLabel OwnerTypeUser = "USER"
ownerTypeLabel OwnerTypeOrganization = "ORGANIZATION"


-- * Colors


-- | Colors for different options.
--
-- These colors are defined in the GitHub GraphQL API. We are just sticking to
-- them.
data OptionColor
  = OptionColorGray
  | OptionColorBlue
  | OptionColorGreen
  | OptionColorYellow
  | OptionColorOrange
  | OptionColorRed
  | OptionColorPink
  | OptionColorPurple
  deriving (Eq, Show, Ord, Bounded, Enum, Generic)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via ADC.Autodocodec OptionColor


instance ADC.HasCodec OptionColor where
  codec = ADC.boundedEnumCodec optionColorLabel


optionColorLabel :: OptionColor -> T.Text
optionColorLabel OptionColorGray = "GRAY"
optionColorLabel OptionColorBlue = "BLUE"
optionColorLabel OptionColorGreen = "GREEN"
optionColorLabel OptionColorYellow = "YELLOW"
optionColorLabel OptionColorOrange = "ORANGE"
optionColorLabel OptionColorRed = "RED"
optionColorLabel OptionColorPink = "PINK"
optionColorLabel OptionColorPurple = "PURPLE"


-- | Emoji symbol for the given option color.
--
-- >>> fmap optionColorEmoji [OptionColorGray .. OptionColorPurple]
-- ["\11036","\128998","\129001","\129000","\128999","\128997","\129003","\129002"]
optionColorEmoji :: OptionColor -> T.Text
optionColorEmoji OptionColorGray = "⬜" -- White large square as 'gray'
optionColorEmoji OptionColorBlue = "🟦"
optionColorEmoji OptionColorGreen = "🟩"
optionColorEmoji OptionColorYellow = "🟨"
optionColorEmoji OptionColorOrange = "🟧"
optionColorEmoji OptionColorRed = "🟥"
optionColorEmoji OptionColorPink = "🟫" -- No pink square, hence the brown square
optionColorEmoji OptionColorPurple = "🟪"


-- * GH helpers


ghGetRateLimitRemaining :: IO Integer
ghGetRateLimitRemaining = do
  res <- runProcessRead "gh" ["api", "rate_limit", "--jq", ".rate.remaining"]
  case res of
    Left err -> printProcessResultError "gh-api-rate-limit" err >> die "Exiting..."
    Right sv -> pure sv


-- * Helpers


type ProcessResult a = Either ProcessResultError a


type ProcessResultError = (Int, BL.ByteString, BL.ByteString)


printProcessResultError :: T.Text -> ProcessResultError -> IO ()
printProcessResultError label (code, out, err) = do
  hPutStrLn stderr (T.unpack label <> " failed with exit code " <> show code)
  unless (BL.null out) $ do
    hPutStrLn stderr "Standard Output:"
    BLC.hPutStrLn stderr out
  unless (BL.null err) $ do
    hPutStrLn stderr "Standard Error:"
    BLC.hPutStrLn stderr err


runProcessBLC :: T.Text -> [T.Text] -> IO (ProcessResult BL.ByteString)
runProcessBLC cmd args = do
  let process = TP.setStdout TP.byteStringOutput . TP.setStderr TP.byteStringOutput $ TP.proc (T.unpack cmd) (fmap T.unpack args)
  (exitCode, out, err) <- TP.readProcess process
  pure $ case exitCode of
    ExitSuccess -> Right out
    ExitFailure c -> Left (c, out, err)


runProcessJSON :: Aeson.FromJSON a => T.Text -> [T.Text] -> IO (ProcessResult a)
runProcessJSON cmd args = do
  res <- runProcessBLC cmd args
  case res of
    Left err -> pure (Left err)
    Right out -> case Aeson.eitherDecode out of
      Left err2 -> pure (Left (-1, out, BLC.pack err2))
      Right val -> pure (Right val)


runProcessRead :: Read a => T.Text -> [T.Text] -> IO (ProcessResult a)
runProcessRead cmd args =
  runProcessBLC cmd args >>= \case
    Left err -> pure (Left err)
    Right out -> case reads (BLC.unpack out) of
      [(val, _)] -> pure (Right val)
      _ -> pure (Left (-1, out, "Failed to parse output"))
