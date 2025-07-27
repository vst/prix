{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Prix.Config where

import qualified Autodocodec as ADC
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Aeson as Aeson
import qualified Data.Time as Time
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import qualified Path as P
import qualified Path.IO as PIO


newtype Config = MkConfig
  { configProjects :: ConfigProjects
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Config)


instance ADC.HasCodec Config where
  codec =
    ADC.object "Config" $
      MkConfig
        <$> ADC.requiredField "projects" "Projects Configuration" ADC..= configProjects


newtype ConfigProjects = MkConfigProjects
  { configProjectsInception :: Time.Day
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ConfigProjects)


instance ADC.HasCodec ConfigProjects where
  codec =
    ADC.object "ConfigProjects" $
      MkConfigProjects
        <$> ADC.requiredField "inception" "Inception Date" ADC..= configProjectsInception


readConfigFromFile :: MonadIO m => P.Path P.Abs P.File -> m (Either String Config)
readConfigFromFile path = do
  result <- liftIO $ Yaml.decodeFileEither (P.toFilePath path)
  pure $ case result of
    Left err -> Left (show err)
    Right config -> Right config


readConfig :: MonadIO m => m (Either String Config)
readConfig = do
  pfs <- defConfigFiles
  mfp <- findConfigFile pfs
  case mfp of
    Nothing -> pure $ Left "No configuration file found"
    Just fp -> readConfigFromFile fp


findConfigFile :: MonadIO m => [P.Path P.Abs P.File] -> m (Maybe (P.Path P.Abs P.File))
findConfigFile [] = pure Nothing
findConfigFile (p : ps) = do
  exists <- PIO.doesFileExist p
  if exists
    then pure (Just p)
    else findConfigFile ps


defConfigFiles :: MonadIO m => m [P.Path P.Abs P.File]
defConfigFiles = do
  pwdFile <- pwdConfigFile
  xdgFile <- xdgConfigFile
  pure [pwdFile, xdgFile]


pwdConfigFile :: MonadIO m => m (P.Path P.Abs P.File)
pwdConfigFile =
  (P.</> $(P.mkRelFile "config.yaml")) <$> PIO.getCurrentDir


xdgConfigFile :: MonadIO m => m (P.Path P.Abs P.File)
xdgConfigFile =
  (P.</> $(P.mkRelFile "config.yaml")) <$> PIO.getAppUserDataDir "prix"
