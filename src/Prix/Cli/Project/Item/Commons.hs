{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Prix.Cli.Project.Item.Commons where

import Control.Applicative ((<|>))
import Control.Exception (IOException, try)
import Control.Monad (forM)
import qualified Data.Aeson as Aeson
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Data.String.Interpolate (i)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Options.Applicative as OA
import qualified Path as P
import qualified Prix.Commons as Commons
import qualified Prix.Config as Config
import qualified Prix.Project as Project
import qualified Prix.ProjectConfig as ProjectConfig
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import qualified Zamazingo.Terminal.Prompts as Z.Term.Prompts
import qualified Zamazingo.Text as Z.Text


-- * Types


data BodySource
  = BodySourceText !T.Text
  | BodySourceFile !FilePath
  deriving (Show, Eq)


data FieldUpdate = FieldUpdate
  { fieldUpdateId :: !T.Text
  , fieldUpdateValue :: !Aeson.Value
  }


-- * Prompts


promptAssignees :: [T.Text] -> IO [T.Text]
promptAssignees def = do
  let prompt =
        if null def
          then "Assignees (comma-separated, optional)"
          else "Assignees (comma-separated)"
  mLine <- Z.Term.Prompts.text prompt (Just (T.intercalate "," def))
  case mLine of
    Nothing -> pure def
    Just "" -> pure def
    Just s -> pure (splitComma s)


promptSelectOptional :: Eq a => T.Text -> (a -> T.Text) -> Maybe a -> [a] -> IO (Maybe a)
promptSelectOptional _ _ mDef [] = pure mDef
promptSelectOptional label asText mDef items = do
  mChoice <- Z.Term.Prompts.choose label asText mDef items
  pure (mChoice <|> mDef)


-- * Issue Types


-- | Fetch issue types available for the org that owns the given @owner/repo@ string.
-- Returns an empty list for user-owned repos or unrecognised formats.
getRepoOrgIssueTypes :: T.Text -> IO [Commons.OrgIssueType]
getRepoOrgIssueTypes repo =
  case T.splitOn "/" repo of
    [owner, _] -> Commons.ghGetOrgIssueTypes owner
    _ -> pure []


-- | Filter the 'Project.IssueType' enum to only those whose labels match a name in
-- the provided org issue type list.
matchingIssueTypes :: [Commons.OrgIssueType] -> [Project.IssueType]
matchingIssueTypes orgTypes =
  filter hasMatch [minBound .. maxBound]
  where
    hasMatch t = any (\ot -> T.toLower (Commons.orgIssueTypeName ot) == T.toLower (Project.issueTypeLabel t)) orgTypes


-- | Resolve a 'Project.IssueType' to the org-specific GitHub ID.
-- Returns 'Nothing' and emits a warning when the type is not found in the org.
resolveIssueTypeId :: [Commons.OrgIssueType] -> Maybe Project.IssueType -> IO (Maybe T.Text)
resolveIssueTypeId _ Nothing = pure Nothing
resolveIssueTypeId orgTypes (Just issueType) =
  case L.find matches orgTypes of
    Nothing -> do
      hPutStrLn stderr ("Warning: issue type \"" <> T.unpack (Project.issueTypeLabel issueType) <> "\" not found in org. Skipping.")
      pure Nothing
    Just ot -> pure (Just (Commons.orgIssueTypeId ot))
  where
    matches ot = T.toLower (Commons.orgIssueTypeName ot) == T.toLower (Project.issueTypeLabel issueType)


-- * Assignees


resolveAssigneeIds :: [T.Text] -> IO [T.Text]
resolveAssigneeIds logins =
  forM logins $ \login -> do
    res <- Commons.ghLookupUserId login
    case res of
      Left err -> die err
      Right userId -> pure userId


-- * Item Body


resolveBodySource :: BodySource -> IO T.Text
resolveBodySource = \case
  BodySourceText body -> pure body
  BodySourceFile path -> do
    res <- try (TIO.readFile path)
    case res of
      Left err -> die ("Failed to read body file: " <> show (err :: IOException))
      Right body -> pure body


bodySourceParser :: OA.Parser BodySource
bodySourceParser =
  bodyText <|> bodyFile
  where
    bodyText = BodySourceText . T.pack <$> OA.strOption (OA.long "body" <> OA.metavar "BODY" <> OA.help "Item body.")
    bodyFile = BodySourceFile <$> OA.strOption (OA.long "body-file" <> OA.metavar "PATH" <> OA.help "Read item body from a file.")


maybeBodySourceParser :: OA.Parser (Maybe BodySource)
maybeBodySourceParser =
  OA.optional bodyText <|> OA.optional bodyFile
  where
    bodyText = BodySourceText . T.pack <$> OA.strOption (OA.long "body" <> OA.metavar "BODY" <> OA.help "Item body.")
    bodyFile = BodySourceFile <$> OA.strOption (OA.long "body-file" <> OA.metavar "PATH" <> OA.help "Read item body from a file.")


-- * CLI Parsers


parseEnumOption :: (Bounded a, Enum a) => (a -> T.Text) -> OA.ReadM a
parseEnumOption labelFn =
  OA.eitherReader $ \s ->
    let val = T.toLower (T.pack s)
        options = [minBound .. maxBound]
        matches a = T.toLower (labelFn a) == val
     in case L.find matches options of
          Nothing -> Left "Invalid option."
          Just a -> Right a


-- * Project Config


loadProjectConfigs :: IO [ProjectConfig.ProjectConfig]
loadProjectConfigs = do
  filePath <- Config.getAppDataFileProjectConfig
  eConfigs <- (Aeson.eitherDecodeFileStrict' (P.toFilePath filePath) :: IO (Either String [ProjectConfig.ProjectConfig]))
  case eConfigs of
    Left err ->
      die $
        "Failed to read project config from "
          <> P.toFilePath filePath
          <> ": "
          <> err
          <> "\nRun `prix project sync` to fetch the config."
    Right configs -> pure configs


loadProjectConfig :: Project.Project -> IO ProjectConfig.ProjectConfig
loadProjectConfig project = do
  configs <- loadProjectConfigs
  case L.find matches configs of
    Nothing -> die "Matching project config not found. Run `prix project sync` to refresh config."
    Just cfg -> pure cfg
  where
    matches cfg =
      ProjectConfig.projectConfigNumber cfg == fromIntegral (Project.projectMetaNumber (Project.projectMeta project))
        && Commons.ownerLogin (ProjectConfig.projectConfigOwner cfg) == Commons.ownerLogin (Project.projectOwner project)


findProjectByIdent :: (T.Text, Int) -> [ProjectConfig.ProjectConfig] -> Maybe ProjectConfig.ProjectConfig
findProjectByIdent ident =
  L.find (\cfg -> ProjectConfig.projectConfigIdent cfg == ident)


selectProjectConfig :: (T.Text, Int) -> [ProjectConfig.ProjectConfig] -> IO ProjectConfig.ProjectConfig
selectProjectConfig ident@(o, n) configs =
  case filterProjectConfigs ident configs of
    [] -> die [i|No matching project config found for #{o}/#{n}. Hint: Run `prix project sync`.|]
    [cfg] -> pure cfg
    _ -> die "Multiple project configs found. Use --project or --owner, or run in a TTY for selection."


filterProjectConfigs :: (T.Text, Int) -> [ProjectConfig.ProjectConfig] -> [ProjectConfig.ProjectConfig]
filterProjectConfigs (owner, number) =
  filter ((&&) <$> matchesNumber <*> matchesOwner)
  where
    matchesNumber = (==) number . ProjectConfig.projectConfigNumber
    matchesOwner = (==) owner . Commons.ownerLogin . ProjectConfig.projectConfigOwner


-- | Build a browser URL for a project item given its numeric database ID.
projectItemUrl :: ProjectConfig.ProjectConfig -> Integer -> T.Text
projectItemUrl cfg dbId =
  let ownerSegment = case Commons.ownerType (ProjectConfig.projectConfigOwner cfg) of
        Commons.OwnerTypeUser -> "users"
        Commons.OwnerTypeOrganization -> "orgs"
      login = Commons.ownerLogin (ProjectConfig.projectConfigOwner cfg)
      number = ProjectConfig.projectConfigNumber cfg
   in "https://github.com/" <> ownerSegment <> "/" <> login <> "/projects/" <> Z.Text.tshow number <> "/?pane=issue&itemId=" <> Z.Text.tshow dbId


projectConfigToText :: ProjectConfig.ProjectConfig -> T.Text
projectConfigToText ProjectConfig.MkProjectConfig {..} =
  [i|#{projectConfigTitle} (#{Commons.ownerLogin projectConfigOwner} / #{projectConfigNumber})|]


projectConfigIterations :: ProjectConfig.ProjectConfig -> [(Integer, T.Text)]
projectConfigIterations cfg =
  case findFieldByName "Iteration" (ProjectConfig.projectConfigFields cfg) of
    Just (ProjectConfig.ProjectConfigFieldIteration field) ->
      mapMaybe
        ( \iter -> do
            num <- iterationNumberFromTitle (ProjectConfig.projectConfigIterationTitle iter)
            pure (num, ProjectConfig.projectConfigIterationTitle iter)
        )
        (ProjectConfig.projectConfigIterationConfigurationIterations (ProjectConfig.projectConfigFieldIterationConfiguration field))
    _ -> []


-- * Project Items


loadProjectItem :: T.Text -> IO (Project.Project, Project.ProjectItem)
loadProjectItem itemId = do
  filePath <- Config.getAppDataFileProjectItems
  eProjects <- Aeson.eitherDecodeFileStrict' @[Project.Project] (P.toFilePath filePath)
  case eProjects of
    Left err ->
      die $
        "Failed to read project items from "
          <> P.toFilePath filePath
          <> ": "
          <> err
          <> "\nRun `prix project sync` to fetch the items."
    Right projects ->
      case findProjectItem itemId projects of
        Nothing -> die "Project item not found. Run `prix project sync` to refresh items."
        Just res -> pure res


findProjectItem :: T.Text -> [Project.Project] -> Maybe (Project.Project, Project.ProjectItem)
findProjectItem itemId =
  findMap $ \project ->
    findMap (\item -> if Project.projectItemId item == itemId then Just (project, item) else Nothing) (Project.projectItems project)
  where
    findMap f = foldr (\a acc -> f a <|> acc) Nothing


-- * Project Fields


findFieldByName :: T.Text -> [ProjectConfig.ProjectConfigField] -> Maybe ProjectConfig.ProjectConfigField
findFieldByName name =
  L.find $ \field -> T.toLower (fieldName field) == T.toLower name
  where
    fieldName :: ProjectConfig.ProjectConfigField -> T.Text
    fieldName = \case
      ProjectConfig.ProjectConfigFieldCommon field -> ProjectConfig.projectConfigFieldCommonName field
      ProjectConfig.ProjectConfigFieldIteration field -> ProjectConfig.projectConfigFieldIterationName field
      ProjectConfig.ProjectConfigFieldSingleSelect field -> ProjectConfig.projectConfigFieldSingleSelectName field


requireSingleSelect :: ProjectConfig.ProjectConfig -> T.Text -> Either String ProjectConfig.ProjectConfigFieldSingleSelect
requireSingleSelect cfg name =
  case findFieldByName name (ProjectConfig.projectConfigFields cfg) of
    Just (ProjectConfig.ProjectConfigFieldSingleSelect field) -> Right field
    Just _ -> Left ("Field " <> T.unpack name <> " has wrong type.")
    Nothing -> Left ("Missing field " <> T.unpack name <> " in project config.")


requireIteration :: ProjectConfig.ProjectConfig -> T.Text -> Either String ProjectConfig.ProjectConfigFieldIteration
requireIteration cfg name =
  case findFieldByName name (ProjectConfig.projectConfigFields cfg) of
    Just (ProjectConfig.ProjectConfigFieldIteration field) -> Right field
    Just _ -> Left ("Field " <> T.unpack name <> " has wrong type.")
    Nothing -> Left ("Missing field " <> T.unpack name <> " in project config.")


requireCommon :: ProjectConfig.ProjectConfig -> T.Text -> Either String ProjectConfig.ProjectConfigFieldCommon
requireCommon cfg name =
  case findFieldByName name (ProjectConfig.projectConfigFields cfg) of
    Just (ProjectConfig.ProjectConfigFieldCommon field) -> Right field
    Just _ -> Left ("Field " <> T.unpack name <> " has wrong type.")
    Nothing -> Left ("Missing field " <> T.unpack name <> " in project config.")


ensureDataType :: T.Text -> T.Text -> T.Text -> Either String ()
ensureDataType fieldLabel expected actual
  | T.toUpper actual == T.toUpper expected = Right ()
  | otherwise =
      Left
        ( "Field "
            <> T.unpack fieldLabel
            <> " expects data type "
            <> T.unpack expected
            <> " but found "
            <> T.unpack actual
            <> "."
        )


selectOptionId :: T.Text -> T.Text -> [ProjectConfig.ProjectConfigSingleSelectOption] -> Either String T.Text
selectOptionId fieldLabel optionName options =
  case L.find matches options of
    Nothing -> Left ("Unknown option " <> T.unpack optionName <> " for " <> T.unpack fieldLabel <> ".")
    Just opt -> Right (ProjectConfig.projectConfigSingleSelectOptionId opt)
  where
    matches opt = T.toLower (ProjectConfig.projectConfigSingleSelectOptionName opt) == T.toLower optionName


selectIterationId :: T.Text -> Integer -> ProjectConfig.ProjectConfigIterationConfiguration -> Either String T.Text
selectIterationId fieldLabel number config =
  case L.find matches (ProjectConfig.projectConfigIterationConfigurationIterations config) of
    Nothing -> Left ("Unknown iteration " <> show number <> " for " <> T.unpack fieldLabel <> ".")
    Just iter -> Right (ProjectConfig.projectConfigIterationId iter)
  where
    matches iter = iterationNumberFromTitle (ProjectConfig.projectConfigIterationTitle iter) == Just number


updateProjectField :: ProjectConfig.ProjectConfig -> T.Text -> T.Text -> Aeson.Value -> IO ()
updateProjectField cfg itemId fieldId value = do
  let projectId = ProjectConfig.projectConfigId cfg
  res <- Commons.ghUpdateProjectItemField projectId itemId fieldId value
  case res of
    Left err -> die err
    Right _ -> pure ()


-- * Utilities


iterationNumberFromTitle :: T.Text -> Maybe Integer
iterationNumberFromTitle title = do
  rest <- T.stripPrefix "Iteration " title
  readMaybeInteger (T.unpack rest)


readMaybeInteger :: String -> Maybe Integer
readMaybeInteger s =
  case reads s of
    [(n, _)] -> Just n
    _ -> Nothing


splitComma :: T.Text -> [T.Text]
splitComma =
  fmap T.strip . T.splitOn "," . T.filter (/= '\n')
