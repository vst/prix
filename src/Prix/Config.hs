{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Prix.Config where

import qualified Autodocodec as ADC
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import qualified Path as P
import qualified Path.IO as PIO


data Config = MkConfig
  { configInception :: !Time.Day
  , configProjects :: ![ConfigProject]
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Config)


instance ADC.HasCodec Config where
  codec =
    ADC.object "Config" $
      MkConfig
        <$> ADC.requiredField "inception" "Inception Date" ADC..= configInception
        <*> ADC.requiredField "projects" "Projects Configuration" ADC..= configProjects


data ConfigProject = MkConfigProject
  { configProjectOwner :: !Owner
  , configProjectNumber :: !Int
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ConfigProject)


instance ADC.HasCodec ConfigProject where
  codec =
    ADC.object "ConfigProject" $
      MkConfigProject
        <$> ADC.requiredField "owner" "Project Owner" ADC..= configProjectOwner
        <*> ADC.requiredField "number" "Project Number" ADC..= configProjectNumber


data Owner
  = OwnerUser !T.Text
  | OwnerOrganization !T.Text
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Owner)


instance ADC.HasCodec Owner where
  codec = ADC.object "Owner" ADC.objectCodec


instance ADC.HasObjectCodec Owner where
  objectCodec = ADC.discriminatedUnionCodec "type" enc dec
    where
      loginFieldCodec = ADC.requiredField' "login"
      enc = \case
        OwnerUser s -> ("USER", ADC.mapToEncoder s loginFieldCodec)
        OwnerOrganization n -> ("ORGANIZATION", ADC.mapToEncoder n loginFieldCodec)
      dec =
        HashMap.fromList
          [ ("USER", ("USER", ADC.mapToDecoder OwnerUser loginFieldCodec))
          , ("ORGANIZATION", ("ORGANIZATION", ADC.mapToDecoder OwnerOrganization loginFieldCodec))
          ]


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


-- * Application Configuration Files


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
  (P.</> $(P.mkRelFile "config.yaml")) <$> getAppConfDir


-- * Application Data Files


getAppDataFileProjectItems :: MonadIO m => m (P.Path P.Abs P.File)
getAppDataFileProjectItems =
  (P.</> $(P.mkRelFile "project-items.json")) <$> getAppDataDir


-- * Application Directories


getAppConfDir :: MonadIO m => m (P.Path P.Abs P.Dir)
getAppConfDir =
  PIO.getXdgDir PIO.XdgConfig (Just _appRelPath)


getAppDataDir :: MonadIO m => m (P.Path P.Abs P.Dir)
getAppDataDir =
  PIO.getXdgDir PIO.XdgData (Just _appRelPath)


_appRelPath :: P.Path P.Rel P.Dir
_appRelPath = $(P.mkRelDir "prix")
