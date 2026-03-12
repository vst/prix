{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Prix.ProjectConfig where

import qualified Autodocodec as ADC
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Time as Time
import GHC.Generics (Generic)
import qualified Prix.Commons as Commons
import Prix.Config (ConfigProject (..), Owner (..))
import qualified Zamazingo.Text as Z.Text


-- * Project Config


data ProjectConfig = MkProjectConfig
  { projectConfigId :: !T.Text
  , projectConfigNumber :: !Int
  , projectConfigTitle :: !T.Text
  , projectConfigUrl :: !T.Text
  , projectConfigShortDescription :: !(Maybe T.Text)
  , projectConfigReadme :: !(Maybe T.Text)
  , projectConfigOwner :: !Commons.Owner
  , projectConfigFields :: ![ProjectConfigField]
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectConfig)


instance ADC.HasCodec ProjectConfig where
  codec =
    ADC.object "ProjectConfig" $
      MkProjectConfig
        <$> ADC.requiredField "id" "Project ID" ADC..= projectConfigId
        <*> ADC.requiredField "number" "Project Number" ADC..= projectConfigNumber
        <*> ADC.requiredField "title" "Project Title" ADC..= projectConfigTitle
        <*> ADC.requiredField "url" "Project URL" ADC..= projectConfigUrl
        <*> ADC.requiredField "shortDescription" "Project Short Description" ADC..= projectConfigShortDescription
        <*> ADC.requiredField "readme" "Project Readme" ADC..= projectConfigReadme
        <*> ADC.requiredField "owner" "Project Owner" ADC..= projectConfigOwner
        <*> ADC.requiredField "fields" "Project Fields" ADC..= projectConfigFields


projectConfigIdent :: ProjectConfig -> (T.Text, Int)
projectConfigIdent =
  (,) <$> projectConfigId <*> projectConfigNumber


-- * Project Fields


data ProjectConfigField
  = ProjectConfigFieldCommon !ProjectConfigFieldCommon
  | ProjectConfigFieldIteration !ProjectConfigFieldIteration
  | ProjectConfigFieldSingleSelect !ProjectConfigFieldSingleSelect
  deriving stock (Show, Eq, Generic)


instance Aeson.FromJSON ProjectConfigField where
  parseJSON = Aeson.withObject "ProjectConfigField" $ \obj -> do
    dataType <- obj Aeson..: "dataType"
    case (dataType :: T.Text) of
      "ITERATION" -> ProjectConfigFieldIteration <$> Aeson.parseJSON (Aeson.Object obj)
      "SINGLE_SELECT" -> ProjectConfigFieldSingleSelect <$> Aeson.parseJSON (Aeson.Object obj)
      _ -> ProjectConfigFieldCommon <$> Aeson.parseJSON (Aeson.Object obj)


instance Aeson.ToJSON ProjectConfigField where
  toJSON = \case
    ProjectConfigFieldCommon field -> Aeson.toJSON field
    ProjectConfigFieldIteration field -> Aeson.toJSON field
    ProjectConfigFieldSingleSelect field -> Aeson.toJSON field


instance ADC.HasCodec ProjectConfigField where
  codec = ADC.codecViaAeson "ProjectConfigField"


data ProjectConfigFieldCommon = MkProjectConfigFieldCommon
  { projectConfigFieldCommonId :: !T.Text
  , projectConfigFieldCommonName :: !T.Text
  , projectConfigFieldCommonDataType :: !T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectConfigFieldCommon)


instance ADC.HasCodec ProjectConfigFieldCommon where
  codec =
    ADC.object "ProjectConfigFieldCommon" $
      MkProjectConfigFieldCommon
        <$> ADC.requiredField "id" "Field ID" ADC..= projectConfigFieldCommonId
        <*> ADC.requiredField "name" "Field Name" ADC..= projectConfigFieldCommonName
        <*> ADC.requiredField "dataType" "Field Data Type" ADC..= projectConfigFieldCommonDataType


data ProjectConfigFieldIteration = MkProjectConfigFieldIteration
  { projectConfigFieldIterationId :: !T.Text
  , projectConfigFieldIterationName :: !T.Text
  , projectConfigFieldIterationDataType :: !T.Text
  , projectConfigFieldIterationConfiguration :: !ProjectConfigIterationConfiguration
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectConfigFieldIteration)


instance ADC.HasCodec ProjectConfigFieldIteration where
  codec =
    ADC.object "ProjectConfigFieldIteration" $
      MkProjectConfigFieldIteration
        <$> ADC.requiredField "id" "Field ID" ADC..= projectConfigFieldIterationId
        <*> ADC.requiredField "name" "Field Name" ADC..= projectConfigFieldIterationName
        <*> ADC.requiredField "dataType" "Field Data Type" ADC..= projectConfigFieldIterationDataType
        <*> ADC.requiredField "configuration" "Iteration Configuration" ADC..= projectConfigFieldIterationConfiguration


data ProjectConfigIterationConfiguration = MkProjectConfigIterationConfiguration
  { projectConfigIterationConfigurationDuration :: !Int
  , projectConfigIterationConfigurationStartDay :: !Int
  , projectConfigIterationConfigurationIterations :: ![ProjectConfigIteration]
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectConfigIterationConfiguration)


instance ADC.HasCodec ProjectConfigIterationConfiguration where
  codec =
    ADC.object "ProjectConfigIterationConfiguration" $
      MkProjectConfigIterationConfiguration
        <$> ADC.requiredField "duration" "Iteration Duration" ADC..= projectConfigIterationConfigurationDuration
        <*> ADC.requiredField "startDay" "Iteration Start Day" ADC..= projectConfigIterationConfigurationStartDay
        <*> ADC.requiredField "iterations" "Iterations" ADC..= projectConfigIterationConfigurationIterations


data ProjectConfigIteration = MkProjectConfigIteration
  { projectConfigIterationId :: !T.Text
  , projectConfigIterationDuration :: !Int
  , projectConfigIterationStartDate :: !Time.Day
  , projectConfigIterationTitle :: !T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectConfigIteration)


instance ADC.HasCodec ProjectConfigIteration where
  codec =
    ADC.object "ProjectConfigIteration" $
      MkProjectConfigIteration
        <$> ADC.requiredField "id" "Iteration ID" ADC..= projectConfigIterationId
        <*> ADC.requiredField "duration" "Iteration Duration" ADC..= projectConfigIterationDuration
        <*> ADC.requiredField "startDate" "Iteration Start Date" ADC..= projectConfigIterationStartDate
        <*> ADC.requiredField "title" "Iteration Title" ADC..= projectConfigIterationTitle


data ProjectConfigFieldSingleSelect = MkProjectConfigFieldSingleSelect
  { projectConfigFieldSingleSelectId :: !T.Text
  , projectConfigFieldSingleSelectName :: !T.Text
  , projectConfigFieldSingleSelectDataType :: !T.Text
  , projectConfigFieldSingleSelectOptions :: ![ProjectConfigSingleSelectOption]
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectConfigFieldSingleSelect)


instance ADC.HasCodec ProjectConfigFieldSingleSelect where
  codec =
    ADC.object "ProjectConfigFieldSingleSelect" $
      MkProjectConfigFieldSingleSelect
        <$> ADC.requiredField "id" "Field ID" ADC..= projectConfigFieldSingleSelectId
        <*> ADC.requiredField "name" "Field Name" ADC..= projectConfigFieldSingleSelectName
        <*> ADC.requiredField "dataType" "Field Data Type" ADC..= projectConfigFieldSingleSelectDataType
        <*> ADC.requiredField "options" "Options" ADC..= projectConfigFieldSingleSelectOptions


data ProjectConfigSingleSelectOption = MkProjectConfigSingleSelectOption
  { projectConfigSingleSelectOptionId :: !T.Text
  , projectConfigSingleSelectOptionName :: !T.Text
  , projectConfigSingleSelectOptionColor :: !Commons.OptionColor
  , projectConfigSingleSelectOptionDescription :: !(Maybe T.Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectConfigSingleSelectOption)


instance ADC.HasCodec ProjectConfigSingleSelectOption where
  codec =
    ADC.object "ProjectConfigSingleSelectOption" $
      MkProjectConfigSingleSelectOption
        <$> ADC.requiredField "id" "Option ID" ADC..= projectConfigSingleSelectOptionId
        <*> ADC.requiredField "name" "Option Name" ADC..= projectConfigSingleSelectOptionName
        <*> ADC.requiredField "color" "Option Color" ADC..= projectConfigSingleSelectOptionColor
        <*> ADC.requiredField "description" "Option Description" ADC..= projectConfigSingleSelectOptionDescription


-- * IO


getProjectConfigData :: ConfigProject -> IO (Either Commons.ProcessResultError ProjectConfig)
getProjectConfigData (MkConfigProject owner number) = do
  let (entity, handle_) = case owner of
        OwnerUser l -> ("--user", l)
        OwnerOrganization l -> ("--org", l)
  Commons.runProcessJSON "gh-prix-project-config" [entity, handle_, "--project", Z.Text.tshow number]
