{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Prix.ProjectItemCreate (
  CreateOptions (..),
  createOptionsParser,
  runCreate,
) where

import Control.Applicative ((<|>))
import Control.Exception (IOException, try)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as AesonTypes
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (find)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.String.Interpolate (i)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import qualified Options.Applicative as OA
import qualified Path as P
import qualified Prix.Commons as Commons
import Prix.Config (Config (..))
import qualified Prix.Config as Config
import qualified Prix.Project as Project
import qualified Prix.ProjectConfig as ProjectConfig
import qualified System.Console.Haskeline as HL
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), die)
import System.IO (hClose, hFlush, hIsTerminalDevice, stdin)
import qualified System.IO.Temp as Temp
import qualified System.Process.Typed as TP


data CreateOptions = MkCreateOptions
  { createOptInteractive :: !Bool
  , createOptOwner :: !(Maybe T.Text)
  , createOptProjectNumber :: !(Maybe Int)
  , createOptRepo :: !(Maybe T.Text)
  , createOptTitle :: !(Maybe T.Text)
  , createOptBodySource :: !(Maybe BodySource)
  , createOptAssignees :: ![T.Text]
  , createOptStatus :: !(Maybe Project.ProjectItemStatus)
  , createOptIteration :: !(Maybe Integer)
  , createOptUrgency :: !(Maybe Project.ProjectItemUrgency)
  , createOptImpact :: !(Maybe Project.ProjectItemImpact)
  , createOptReach :: !(Maybe Project.ProjectItemReach)
  , createOptSize :: !(Maybe Project.ProjectItemSize)
  , createOptDifficulty :: !(Maybe Project.ProjectItemDifficulty)
  , createOptConfidence :: !(Maybe Project.ProjectItemConfidence)
  , createOptTheme :: !(Maybe Project.ProjectItemTheme)
  }
  deriving (Show, Eq, Generic)


createOptionsParser :: OA.Parser CreateOptions
createOptionsParser =
  MkCreateOptions
    <$> interactiveP
    <*> ownerP
    <*> projectNumberP
    <*> repoP
    <*> titleP
    <*> bodySourceParser
    <*> assigneesP
    <*> statusP
    <*> iterationP
    <*> urgencyP
    <*> impactP
    <*> reachP
    <*> sizeP
    <*> difficultyP
    <*> confidenceP
    <*> themeP
  where
    interactiveP = OA.switch (OA.long "interactive" <> OA.short 'i' <> OA.help "Run in interactive mode.")
    ownerP = OA.optional (T.pack <$> OA.strOption (OA.long "owner" <> OA.metavar "LOGIN" <> OA.help "Project owner login to disambiguate."))
    projectNumberP = OA.optional (OA.option OA.auto (OA.long "project" <> OA.metavar "NUMBER" <> OA.help "Project number to use."))
    repoP = OA.optional (T.pack <$> OA.strOption (OA.long "repo" <> OA.metavar "OWNER/REPO" <> OA.help "Repository for the new issue."))
    titleP = OA.optional (T.pack <$> OA.strOption (OA.long "title" <> OA.metavar "TITLE" <> OA.help "Item title."))
    assigneesP = OA.many (T.pack <$> OA.strOption (OA.long "assignee" <> OA.metavar "LOGIN" <> OA.help "Assignee login (repeatable)."))
    statusP = OA.optional (OA.option (parseEnumOption Project.projectItemStatusLabel) (OA.long "status" <> OA.metavar "STATUS" <> OA.help "Item status."))
    iterationP = OA.optional (OA.option OA.auto (OA.long "iteration" <> OA.metavar "NUMBER" <> OA.help "Iteration number."))
    urgencyP = OA.optional (OA.option (parseEnumOption Project.projectItemUrgencyLabel) (OA.long "urgency" <> OA.metavar "URGENCY" <> OA.help "Urgency level."))
    impactP = OA.optional (OA.option (parseEnumOption Project.projectItemImpactLabel) (OA.long "impact" <> OA.metavar "IMPACT" <> OA.help "Impact level."))
    reachP = OA.optional (OA.option (parseEnumOption Project.projectItemReachLabel) (OA.long "reach" <> OA.metavar "REACH" <> OA.help "Reach level."))
    sizeP = OA.optional (OA.option (parseEnumOption Project.projectItemSizeLabel) (OA.long "size" <> OA.metavar "SIZE" <> OA.help "Size level."))
    difficultyP = OA.optional (OA.option (parseEnumOption Project.projectItemDifficultyLabel) (OA.long "difficulty" <> OA.metavar "DIFFICULTY" <> OA.help "Difficulty level."))
    confidenceP = OA.optional (OA.option (parseEnumOption Project.projectItemConfidenceLabel) (OA.long "confidence" <> OA.metavar "CONFIDENCE" <> OA.help "Confidence level."))
    themeP = OA.optional (OA.option (parseEnumOption Project.projectItemThemeLabel) (OA.long "theme" <> OA.metavar "THEME" <> OA.help "Strategic theme."))


runCreate :: Config -> CreateOptions -> IO ExitCode
runCreate _cfg opts = do
  isTty <- hIsTerminalDevice stdin
  when (createOptInteractive opts && not isTty) $
    die "Interactive mode requires a TTY."
  resolvedBody <- traverse resolveBodySource (createOptBodySource opts)
  let defaults = (createOptRepo opts, createOptTitle opts, resolvedBody)
  projectConfigs <- loadProjectConfigs
  let requiredReady = requiredOptionsProvided defaults
      needsProjectSelection =
        case filterProjectConfigs (createOptProjectNumber opts) (createOptOwner opts) projectConfigs of
          [] -> False
          [_] -> False
          _ -> True
      interactive =
        createOptInteractive opts || (isTty && (not requiredReady || needsProjectSelection))
  (projectConfig, inputs) <-
    if interactive
      then HL.runInputT HL.defaultSettings $ do
        selected <- promptProjectConfig (createOptProjectNumber opts) (createOptOwner opts) projectConfigs
        filled <- promptInputs selected defaults opts
        pure (selected, filled)
      else do
        selected <- selectProjectConfig (createOptProjectNumber opts) (createOptOwner opts) projectConfigs
        pure (selected, toInputs defaults opts)
  validateInputs inputs
  assigneeIds <- resolveAssigneeIds (createAssignees inputs)
  itemId <- createFromNewIssue projectConfig inputs assigneeIds
  applyFieldUpdates projectConfig itemId inputs
  pure ExitSuccess


-- * Inputs


data BodySource
  = BodySourceText !T.Text
  | BodySourceFile !FilePath
  deriving (Show, Eq)


data CreateInputs = MkCreateInputs
  { createRepo :: !(Maybe T.Text)
  , createTitle :: !(Maybe T.Text)
  , createBody :: !(Maybe T.Text)
  , createAssignees :: ![T.Text]
  , createStatus :: !(Maybe Project.ProjectItemStatus)
  , createIteration :: !(Maybe Integer)
  , createUrgency :: !(Maybe Project.ProjectItemUrgency)
  , createImpact :: !(Maybe Project.ProjectItemImpact)
  , createReach :: !(Maybe Project.ProjectItemReach)
  , createSize :: !(Maybe Project.ProjectItemSize)
  , createDifficulty :: !(Maybe Project.ProjectItemDifficulty)
  , createConfidence :: !(Maybe Project.ProjectItemConfidence)
  , createTheme :: !(Maybe Project.ProjectItemTheme)
  }
  deriving (Show, Eq)


toInputs :: (Maybe T.Text, Maybe T.Text, Maybe T.Text) -> CreateOptions -> CreateInputs
toInputs (repo, title, body) MkCreateOptions {..} =
  MkCreateInputs
    { createRepo = repo
    , createTitle = title
    , createBody = body
    , createAssignees = createOptAssignees
    , createStatus = createOptStatus
    , createIteration = createOptIteration
    , createUrgency = createOptUrgency
    , createImpact = createOptImpact
    , createReach = createOptReach
    , createSize = createOptSize
    , createDifficulty = createOptDifficulty
    , createConfidence = createOptConfidence
    , createTheme = createOptTheme
    }


requiredOptionsProvided :: (Maybe T.Text, Maybe T.Text, Maybe T.Text) -> Bool
requiredOptionsProvided (repo, title, _) =
  maybe False (not . T.null) repo && isJustText title


validateInputs :: CreateInputs -> IO ()
validateInputs MkCreateInputs {..} = do
  require "repo" createRepo
  require "title" createTitle
  where
    require label = \case
      Nothing -> die $ "Missing required " <> label <> "."
      Just _ -> pure ()


-- * Prompting


promptProjectConfig :: Maybe Int -> Maybe T.Text -> [ProjectConfig.ProjectConfig] -> HL.InputT IO ProjectConfig.ProjectConfig
promptProjectConfig mNumber mOwner configs = do
  let filtered = filterProjectConfigs mNumber mOwner configs
  case filtered of
    [] -> liftDie "No matching project config found. Run `prix project sync`."
    [cfg] -> pure cfg
    xs -> do
      HL.outputStrLn "Select a project:"
      forM_ (zip [1 :: Int ..] xs) $ \(idx, cfg) ->
        HL.outputStrLn $
          show idx
            <> ") "
            <> T.unpack (ProjectConfig.projectConfigTitle cfg)
            <> " ("
            <> T.unpack (Commons.ownerLogin (ProjectConfig.projectConfigOwner cfg))
            <> " #"
            <> show (ProjectConfig.projectConfigNumber cfg)
            <> ")"
      pickIndex xs
  where
    pickIndex xs = do
      mLine <- HL.getInputLine "Project number: "
      case mLine >>= readMaybeInt of
        Nothing -> HL.outputStrLn "Please enter a number." >> pickIndex xs
        Just idx
          | idx <= 0 || idx > length xs -> HL.outputStrLn "Out of range." >> pickIndex xs
          | otherwise -> pure (xs !! (idx - 1))


promptInputs :: ProjectConfig.ProjectConfig -> (Maybe T.Text, Maybe T.Text, Maybe T.Text) -> CreateOptions -> HL.InputT IO CreateInputs
promptInputs cfg (repoDefault, titleDefault, bodyDefault) opts = do
  createRepo <- promptText "Repository (owner/repo)" repoDefault
  createTitle <- promptText "Title" titleDefault
  createBody <- promptOptionalText "Body" bodyDefault
  createAssignees <- promptAssignees (createOptAssignees opts)
  createStatus <- promptSelectOptional "Status" (statusOptions cfg) (createOptStatus opts)
  createIteration <- promptIteration cfg (createOptIteration opts)
  createUrgency <- promptSelectOptional "Urgency" (urgencyOptions cfg) (createOptUrgency opts)
  createImpact <- promptSelectOptional "Impact" (impactOptions cfg) (createOptImpact opts)
  createReach <- promptSelectOptional "Reach" (reachOptions cfg) (createOptReach opts)
  createSize <- promptSelectOptional "Size" (sizeOptions cfg) (createOptSize opts)
  createDifficulty <- promptSelectOptional "Difficulty" (difficultyOptions cfg) (createOptDifficulty opts)
  createConfidence <- promptSelectOptional "Confidence" (confidenceOptions cfg) (createOptConfidence opts)
  createTheme <- promptSelectOptional "Theme" (themeOptions cfg) (createOptTheme opts)
  pure MkCreateInputs {..}


bodySourceParser :: OA.Parser (Maybe BodySource)
bodySourceParser =
  OA.optional bodyText <|> OA.optional bodyFile
  where
    bodyText = BodySourceText . T.pack <$> OA.strOption (OA.long "body" <> OA.metavar "BODY" <> OA.help "Item body.")
    bodyFile = BodySourceFile <$> OA.strOption (OA.long "body-file" <> OA.metavar "PATH" <> OA.help "Read item body from a file.")


resolveBodySource :: BodySource -> IO T.Text
resolveBodySource = \case
  BodySourceText body -> pure body
  BodySourceFile path -> do
    res <- try (TIO.readFile path)
    case res of
      Left err -> die ("Failed to read body file: " <> show (err :: IOException))
      Right body -> pure body


promptText :: T.Text -> Maybe T.Text -> HL.InputT IO (Maybe T.Text)
promptText label def = do
  let prompt = maybe label (\d -> label <> " [" <> d <> "]") def <> ": "
  mLine <- HL.getInputLine (T.unpack prompt)
  pure $
    case mLine of
      Nothing -> def
      Just "" -> def
      Just s -> Just (T.pack s)


promptOptionalText :: T.Text -> Maybe T.Text -> HL.InputT IO (Maybe T.Text)
promptOptionalText label def = do
  let prompt = label <> " (optional) [enter to skip, 'e' to edit]: "
  mLine <- HL.getInputLine (T.unpack prompt)
  case mLine of
    Nothing -> pure def
    Just "" -> pure def
    Just s
      | T.toLower (T.pack s) == "e" -> liftIO (editBody def)
      | otherwise -> pure (Just (T.pack s))


editBody :: Maybe T.Text -> IO (Maybe T.Text)
editBody def = do
  editor <- lookupEnv "EDITOR"
  case editor of
    Nothing -> die "EDITOR is not set."
    Just cmd -> do
      let initial = fromMaybe "" def
      Temp.withSystemTempFile "prix-body.md" $ \path handle -> do
        TIO.hPutStr handle initial
        hFlush handle
        hClose handle
        _ <- TP.runProcess (TP.shell (cmd <> " " <> path))
        content <- TIO.readFile path
        pure $
          if T.null content
            then Nothing
            else Just content


promptAssignees :: [T.Text] -> HL.InputT IO [T.Text]
promptAssignees def = do
  let prompt =
        if null def
          then "Assignees (comma-separated, optional): "
          else "Assignees (comma-separated) [" <> T.unpack (T.intercalate "," def) <> "]: "
  mLine <- HL.getInputLine prompt
  case mLine of
    Nothing -> pure def
    Just "" -> pure def
    Just s -> pure (splitComma (T.pack s))


promptSelectOptional :: T.Text -> [(T.Text, a)] -> Maybe a -> HL.InputT IO (Maybe a)
promptSelectOptional label options def = do
  unless (null options) $ do
    HL.outputStrLn (T.unpack label <> " options:")
    forM_ (zip [1 :: Int ..] options) $ \(idx, (optLabel, _)) ->
      HL.outputStrLn (show idx <> ") " <> T.unpack optLabel)
  let prompt =
        case def of
          Nothing -> T.unpack label <> " (optional): "
          Just _ -> T.unpack label <> " (optional) [default]: "
  mLine <- HL.getInputLine prompt
  case mLine of
    Nothing -> pure def
    Just "" -> pure def
    Just s ->
      case selectByInput (T.pack s) options of
        Nothing -> HL.outputStrLn "Invalid selection." >> promptSelectOptional label options def
        Just v -> pure (Just v)


promptIteration :: ProjectConfig.ProjectConfig -> Maybe Integer -> HL.InputT IO (Maybe Integer)
promptIteration cfg def = do
  let iterations = projectConfigIterations cfg
  unless (null iterations) $ do
    HL.outputStrLn "Iteration options:"
    forM_ iterations $ \(num, title) ->
      HL.outputStrLn ("- " <> show num <> ": " <> T.unpack title)
  let prompt = maybe "Iteration" (\d -> "Iteration [" <> show d <> "]") def <> " (optional): "
  mLine <- HL.getInputLine prompt
  case mLine of
    Nothing -> pure def
    Just "" -> pure def
    Just s ->
      case readMaybeInt s of
        Nothing -> HL.outputStrLn "Please enter a number." >> promptIteration cfg def
        Just val -> pure (Just (fromIntegral val))


-- * Project Config Selection


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


selectProjectConfig :: Maybe Int -> Maybe T.Text -> [ProjectConfig.ProjectConfig] -> IO ProjectConfig.ProjectConfig
selectProjectConfig mNumber mOwner configs =
  case filterProjectConfigs mNumber mOwner configs of
    [] -> die "No matching project config found. Run `prix project sync`."
    [cfg] -> pure cfg
    _ -> die "Multiple project configs found. Use --project or --owner, or run in a TTY for selection."


filterProjectConfigs :: Maybe Int -> Maybe T.Text -> [ProjectConfig.ProjectConfig] -> [ProjectConfig.ProjectConfig]
filterProjectConfigs mNumber mOwner =
  filter $ \cfg ->
    matchesNumber mNumber cfg && matchesOwner mOwner cfg
  where
    matchesNumber = \case
      Nothing -> const True
      Just number -> \cfg -> ProjectConfig.projectConfigNumber cfg == number
    matchesOwner = \case
      Nothing -> const True
      Just owner -> \cfg -> Commons.ownerLogin (ProjectConfig.projectConfigOwner cfg) == owner


-- * GraphQL


data FieldUpdate = FieldUpdate
  { fieldUpdateId :: !T.Text
  , fieldUpdateValue :: !Aeson.Value
  }


createFromNewIssue :: ProjectConfig.ProjectConfig -> CreateInputs -> [T.Text] -> IO T.Text
createFromNewIssue cfg inputs@MkCreateInputs {..} assigneeIds = do
  repo <- maybe (die "Missing required repo.") pure createRepo
  (repoOwner, repoName) <-
    case T.splitOn "/" repo of
      [o, n] -> pure (o, n)
      _ -> die "Repository must be in OWNER/REPO format."
  repoId <- lookupRepoId repoOwner repoName
  issueId <- createIssue repoId inputs assigneeIds
  addItemToProject cfg issueId


addItemToProject :: ProjectConfig.ProjectConfig -> T.Text -> IO T.Text
addItemToProject cfg contentId = do
  let projectId = ProjectConfig.projectConfigId cfg
      query =
        [i|mutation($projectId:ID!, $contentId:ID!) {
  addProjectV2ItemById(input: {projectId:$projectId, contentId:$contentId}) {
    item { id }
  }
}|]
      vars =
        [ ("projectId", Aeson.String projectId)
        , ("contentId", Aeson.String contentId)
        ]
  res <- runGhGraphqlValue query vars
  case res >>= extractPayloadItemId "addProjectV2ItemById" "item" of
    Left err -> die err
    Right itemId -> pure itemId


createIssue :: T.Text -> CreateInputs -> [T.Text] -> IO T.Text
createIssue repoId MkCreateInputs {..} assigneeIds = do
  let title = fromMaybe "" createTitle
      body = createBody
      assigneeValue =
        if null assigneeIds
          then Aeson.Null
          else Aeson.toJSON assigneeIds
      query =
        [i|mutation($repoId:ID!, $title:String!, $body:String, $assigneeIds:[ID!]) {
  createIssue(input: {repositoryId:$repoId, title:$title, body:$body, assigneeIds:$assigneeIds}) {
    issue { id }
  }
}|]
      vars =
        [ ("repoId", Aeson.String repoId)
        , ("title", Aeson.String title)
        , ("body", maybe Aeson.Null Aeson.String body)
        , ("assigneeIds", assigneeValue)
        ]
  res <- runGhGraphqlValue query vars
  case res >>= extractIssueId of
    Left err -> die err
    Right issueId -> pure issueId


applyFieldUpdates :: ProjectConfig.ProjectConfig -> T.Text -> CreateInputs -> IO ()
applyFieldUpdates cfg itemId inputs = do
  updates <- case buildFieldUpdates cfg inputs of
    Left err -> die err
    Right xs -> pure xs
  forM_ updates $ \FieldUpdate {..} -> updateProjectField cfg itemId fieldUpdateId fieldUpdateValue


updateProjectField :: ProjectConfig.ProjectConfig -> T.Text -> T.Text -> Aeson.Value -> IO ()
updateProjectField cfg itemId fieldId value = do
  let projectId = ProjectConfig.projectConfigId cfg
      query =
        [i|mutation($projectId:ID!, $itemId:ID!, $fieldId:ID!, $value:ProjectV2FieldValue!) {
  updateProjectV2ItemFieldValue(input: {projectId:$projectId, itemId:$itemId, fieldId:$fieldId, value:$value}) {
    projectV2Item { id }
  }
}|]
      vars =
        [ ("projectId", Aeson.String projectId)
        , ("itemId", Aeson.String itemId)
        , ("fieldId", Aeson.String fieldId)
        , ("value", value)
        ]
  res <- runGhGraphqlValue query vars
  case res of
    Left err -> die err
    Right _ -> pure ()


lookupRepoId :: T.Text -> T.Text -> IO T.Text
lookupRepoId owner name = do
  let query =
        [i|query($owner:String!, $name:String!) {
  repository(owner:$owner, name:$name) { id }
}|]
      vars =
        [ ("owner", Aeson.String owner)
        , ("name", Aeson.String name)
        ]
  res <- runGhGraphqlValue query vars
  case res >>= extractRepoId of
    Left err -> die err
    Right repoId -> pure repoId


lookupUserId :: T.Text -> IO (Either String T.Text)
lookupUserId login = do
  let query =
        [i|query($login:String!) {
  user(login:$login) { id }
}|]
      vars = [("login", Aeson.String login)]
  res <- runGhGraphqlValue query vars
  pure $ first (\err -> "Failed to resolve user " <> T.unpack login <> ": " <> err) (res >>= extractUserId)


resolveAssigneeIds :: [T.Text] -> IO [T.Text]
resolveAssigneeIds logins =
  forM logins $ \login -> do
    res <- lookupUserId login
    case res of
      Left err -> die err
      Right userId -> pure userId


-- * Field Updates


buildFieldUpdates :: ProjectConfig.ProjectConfig -> CreateInputs -> Either String [FieldUpdate]
buildFieldUpdates cfg MkCreateInputs {..} = do
  status <- traverse (buildSingleSelect "Status" Project.projectItemStatusLabel) createStatus
  urgency <- traverse (buildSingleSelect "Urgency" Project.projectItemUrgencyLabel) createUrgency
  impact <- traverse (buildSingleSelect "Impact" Project.projectItemImpactLabel) createImpact
  reach <- traverse (buildSingleSelect "Reach" Project.projectItemReachLabel) createReach
  size <- traverse (buildSingleSelect "Size" Project.projectItemSizeLabel) createSize
  difficulty <- traverse (buildSingleSelect "Difficulty" Project.projectItemDifficultyLabel) createDifficulty
  confidence <- traverse (buildSingleSelect "Confidence" Project.projectItemConfidenceLabel) createConfidence
  theme <- traverse (buildSingleSelect "Theme" Project.projectItemThemeLabel) createTheme
  iteration <- traverse (buildIteration "Iteration") createIteration
  score <- traverse (buildNumber "Score") computedScore
  pure $
    catMaybes
      [ status
      , iteration
      , urgency
      , impact
      , reach
      , size
      , difficulty
      , confidence
      , theme
      , score
      ]
  where
    buildSingleSelect fieldLabel labelFn value = do
      field <- requireSingleSelect cfg fieldLabel
      optionId <- selectOptionId fieldLabel (labelFn value) (ProjectConfig.projectConfigFieldSingleSelectOptions field)
      pure $ FieldUpdate (ProjectConfig.projectConfigFieldSingleSelectId field) (Aeson.object ["singleSelectOptionId" Aeson..= optionId])
    buildIteration fieldLabel value = do
      field <- requireIteration cfg fieldLabel
      iterId <- selectIterationId fieldLabel value (ProjectConfig.projectConfigFieldIterationConfiguration field)
      pure $ FieldUpdate (ProjectConfig.projectConfigFieldIterationId field) (Aeson.object ["iterationId" Aeson..= iterId])
    buildNumber fieldLabel value = do
      field <- requireCommon cfg fieldLabel
      ensureDataType fieldLabel "NUMBER" (ProjectConfig.projectConfigFieldCommonDataType field)
      pure $ FieldUpdate (ProjectConfig.projectConfigFieldCommonId field) (Aeson.object ["number" Aeson..= value])
    computedScore =
      Project.projectItemPriority
        <$> createUrgency
        <*> createReach
        <*> createImpact
        <*> createConfidence
        <*> createSize
        <*> createDifficulty


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


findFieldByName :: T.Text -> [ProjectConfig.ProjectConfigField] -> Maybe ProjectConfig.ProjectConfigField
findFieldByName name =
  find $ \field -> T.toLower (fieldName field) == T.toLower name


fieldName :: ProjectConfig.ProjectConfigField -> T.Text
fieldName = \case
  ProjectConfig.ProjectConfigFieldCommon field -> ProjectConfig.projectConfigFieldCommonName field
  ProjectConfig.ProjectConfigFieldIteration field -> ProjectConfig.projectConfigFieldIterationName field
  ProjectConfig.ProjectConfigFieldSingleSelect field -> ProjectConfig.projectConfigFieldSingleSelectName field


selectOptionId :: T.Text -> T.Text -> [ProjectConfig.ProjectConfigSingleSelectOption] -> Either String T.Text
selectOptionId fieldLabel optionName options =
  case find matches options of
    Nothing -> Left ("Unknown option " <> T.unpack optionName <> " for " <> T.unpack fieldLabel <> ".")
    Just opt -> Right (ProjectConfig.projectConfigSingleSelectOptionId opt)
  where
    matches opt = T.toLower (ProjectConfig.projectConfigSingleSelectOptionName opt) == T.toLower optionName


selectIterationId :: T.Text -> Integer -> ProjectConfig.ProjectConfigIterationConfiguration -> Either String T.Text
selectIterationId fieldLabel number config =
  case find matches (ProjectConfig.projectConfigIterationConfigurationIterations config) of
    Nothing -> Left ("Unknown iteration " <> show number <> " for " <> T.unpack fieldLabel <> ".")
    Just iter -> Right (ProjectConfig.projectConfigIterationId iter)
  where
    matches iter = iterationNumberFromTitle (ProjectConfig.projectConfigIterationTitle iter) == Just number


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


iterationNumberFromTitle :: T.Text -> Maybe Integer
iterationNumberFromTitle title = do
  rest <- T.stripPrefix "Iteration " title
  readMaybeInteger (T.unpack rest)


-- * Option Lists


statusOptions :: ProjectConfig.ProjectConfig -> [(T.Text, Project.ProjectItemStatus)]
statusOptions _ =
  enumOptions Project.projectItemStatusLabel


urgencyOptions :: ProjectConfig.ProjectConfig -> [(T.Text, Project.ProjectItemUrgency)]
urgencyOptions _ =
  enumOptions Project.projectItemUrgencyLabel


impactOptions :: ProjectConfig.ProjectConfig -> [(T.Text, Project.ProjectItemImpact)]
impactOptions _ =
  enumOptions Project.projectItemImpactLabel


reachOptions :: ProjectConfig.ProjectConfig -> [(T.Text, Project.ProjectItemReach)]
reachOptions _ =
  enumOptions Project.projectItemReachLabel


sizeOptions :: ProjectConfig.ProjectConfig -> [(T.Text, Project.ProjectItemSize)]
sizeOptions _ =
  enumOptions Project.projectItemSizeLabel


difficultyOptions :: ProjectConfig.ProjectConfig -> [(T.Text, Project.ProjectItemDifficulty)]
difficultyOptions _ =
  enumOptions Project.projectItemDifficultyLabel


confidenceOptions :: ProjectConfig.ProjectConfig -> [(T.Text, Project.ProjectItemConfidence)]
confidenceOptions _ =
  enumOptions Project.projectItemConfidenceLabel


themeOptions :: ProjectConfig.ProjectConfig -> [(T.Text, Project.ProjectItemTheme)]
themeOptions _ =
  enumOptions Project.projectItemThemeLabel


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


enumOptions :: (Bounded a, Enum a) => (a -> T.Text) -> [(T.Text, a)]
enumOptions labelFn =
  fmap (\a -> (labelFn a, a)) [minBound .. maxBound]


-- * Parse Helpers


parseEnumOption :: (Bounded a, Enum a) => (a -> T.Text) -> OA.ReadM a
parseEnumOption labelFn =
  OA.eitherReader $ \s ->
    let val = T.toLower (T.pack s)
        options = [minBound .. maxBound]
        matches a = T.toLower (labelFn a) == val
     in case find matches options of
          Nothing -> Left "Invalid option."
          Just a -> Right a


-- * GraphQL Helpers


runGhGraphqlValue :: T.Text -> [(T.Text, Aeson.Value)] -> IO (Either String Aeson.Value)
runGhGraphqlValue query vars = do
  let variables =
        Aeson.Object $
          KM.fromList (fmap (first AesonKey.fromText) vars)
      payload =
        Aeson.Object $
          KM.fromList
            [ (AesonKey.fromText "query", Aeson.String query)
            , (AesonKey.fromText "variables", variables)
            ]
      process =
        TP.setStdin (TP.byteStringInput (Aeson.encode payload))
          . TP.setStdout TP.byteStringOutput
          . TP.setStderr TP.byteStringOutput
          $ TP.proc "gh" ["api", "graphql", "--input", "-"]
  (exitCode, out, err) <- TP.readProcess process
  case exitCode of
    ExitSuccess ->
      case Aeson.eitherDecode out of
        Left err2 -> pure . Left $ "Failed to parse gh output: " <> err2
        Right val -> pure (Right val)
    ExitFailure code ->
      pure . Left $
        "gh api graphql failed: "
          <> show code
          <> "\nstdout:\n"
          <> BLC.unpack out
          <> "\nstderr:\n"
          <> BLC.unpack err


extractPayloadItemId :: T.Text -> T.Text -> Aeson.Value -> Either String T.Text
extractPayloadItemId payloadField itemField =
  AesonTypes.parseEither . Aeson.withObject "response" $ \obj -> do
    dat <- obj Aeson..: "data"
    Aeson.withObject
      "data"
      ( \datObj -> do
          case KM.lookup (AesonKey.fromText payloadField) datObj of
            Nothing -> fail ("Missing response field " <> T.unpack payloadField <> ".")
            Just payload ->
              Aeson.withObject
                "payload"
                ( \payloadObj -> do
                    case KM.lookup (AesonKey.fromText itemField) payloadObj of
                      Nothing -> fail ("Missing response field " <> T.unpack itemField <> ".")
                      Just itemVal ->
                        Aeson.withObject "item" (Aeson..: "id") itemVal
                )
                payload
      )
      dat


extractRepoId :: Aeson.Value -> Either String T.Text
extractRepoId =
  AesonTypes.parseEither . Aeson.withObject "response" $ \obj -> do
    dat <- obj Aeson..: "data"
    repo <- dat Aeson..: "repository"
    repo Aeson..: "id"


extractIssueId :: Aeson.Value -> Either String T.Text
extractIssueId =
  AesonTypes.parseEither . Aeson.withObject "response" $ \obj -> do
    dat <- obj Aeson..: "data"
    payload <- dat Aeson..: "createIssue"
    issue <- payload Aeson..: "issue"
    issue Aeson..: "id"


extractUserId :: Aeson.Value -> Either String T.Text
extractUserId =
  AesonTypes.parseEither . Aeson.withObject "response" $ \obj -> do
    dat <- obj Aeson..: "data"
    usr <- dat Aeson..: "user"
    usr Aeson..: "id"


-- * Misc Helpers


selectByInput :: T.Text -> [(T.Text, a)] -> Maybe a
selectByInput input options =
  case readMaybeInt (T.unpack input) of
    Just idx
      | idx > 0 && idx <= length options -> Just (snd (options !! (idx - 1)))
    _ ->
      case find matches options of
        Nothing -> Nothing
        Just (_, val) -> Just val
  where
    matches (label, _) = T.toLower label == T.toLower input


splitComma :: T.Text -> [T.Text]
splitComma =
  fmap T.strip . filter (not . T.null) . T.splitOn ","


isJustText :: Maybe T.Text -> Bool
isJustText =
  maybe False (not . T.null)


readMaybeInt :: String -> Maybe Int
readMaybeInt s =
  case reads s of
    [(n, _)] -> Just n
    _ -> Nothing


readMaybeInteger :: String -> Maybe Integer
readMaybeInteger s =
  case reads s of
    [(n, _)] -> Just n
    _ -> Nothing


liftDie :: String -> HL.InputT IO a
liftDie msg =
  HL.outputStrLn msg >> liftIO (die msg)
