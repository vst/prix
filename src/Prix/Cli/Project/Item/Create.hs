{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Prix.Cli.Project.Item.Create (
  CreateOptions,
  createOptionsParser,
  runCreate,
) where

import Control.Monad (forM_, unless, when)
import qualified Data.Aeson as Aeson
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Options.Applicative as OA
import qualified Prix.Cli.Project.Item.Commons as Item.Commons
import qualified Prix.Commons as Commons
import Prix.Config (Config (..))
import qualified Prix.Project as Project
import qualified Prix.ProjectConfig as ProjectConfig
import System.Exit (ExitCode (..), die)
import System.IO (hIsTerminalDevice, stdin)
import qualified Zamazingo.Base as Z.Base
import qualified Zamazingo.Terminal.Prompts as Z.Term.Prompts
import qualified Zamazingo.Text as Z.Text


-- * CLI Options


data CreateOptions = MkCreateOptions
  { createOptInteractive :: !Bool
  , createOptDraft :: !Bool
  , createOptOwner :: !(Maybe T.Text)
  , createOptProjectNumber :: !(Maybe Int)
  , createOptRepo :: !(Maybe T.Text)
  , createOptTitle :: !(Maybe T.Text)
  , createOptBodySource :: !(Maybe Item.Commons.BodySource)
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
  , createOptIssueType :: !(Maybe Project.IssueType)
  }
  deriving (Show, Eq, Generic)


createOptionsParser :: OA.Parser CreateOptions
createOptionsParser =
  MkCreateOptions
    <$> interactiveP
    <*> draftP
    <*> ownerP
    <*> projectNumberP
    <*> repoP
    <*> titleP
    <*> Item.Commons.maybeBodySourceParser
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
    <*> issueTypeP
  where
    interactiveP = OA.switch (OA.long "interactive" <> OA.short 'i' <> OA.help "Run in interactive mode.")
    draftP = OA.switch (OA.long "draft" <> OA.short 'd' <> OA.help "Create a draft item (no repository required).")
    ownerP = OA.optional (T.pack <$> OA.strOption (OA.long "owner" <> OA.metavar "LOGIN" <> OA.help "Project owner login to disambiguate."))
    projectNumberP = OA.optional (OA.option OA.auto (OA.long "project" <> OA.metavar "NUMBER" <> OA.help "Project number to use."))
    repoP = OA.optional (T.pack <$> OA.strOption (OA.long "repo" <> OA.metavar "OWNER/REPO" <> OA.help "Repository for the new issue."))
    titleP = OA.optional (T.pack <$> OA.strOption (OA.long "title" <> OA.metavar "TITLE" <> OA.help "Item title."))
    assigneesP = OA.many (T.pack <$> OA.strOption (OA.long "assignee" <> OA.metavar "LOGIN" <> OA.help "Assignee login (repeatable)."))
    statusP = OA.optional (OA.option (Item.Commons.parseEnumOption Project.projectItemStatusLabel) (OA.long "status" <> OA.metavar "STATUS" <> OA.help "Item status."))
    iterationP = OA.optional (OA.option OA.auto (OA.long "iteration" <> OA.metavar "NUMBER" <> OA.help "Iteration number."))
    urgencyP = OA.optional (OA.option (Item.Commons.parseEnumOption Project.projectItemUrgencyLabel) (OA.long "urgency" <> OA.metavar "URGENCY" <> OA.help "Urgency level."))
    impactP = OA.optional (OA.option (Item.Commons.parseEnumOption Project.projectItemImpactLabel) (OA.long "impact" <> OA.metavar "IMPACT" <> OA.help "Impact level."))
    reachP = OA.optional (OA.option (Item.Commons.parseEnumOption Project.projectItemReachLabel) (OA.long "reach" <> OA.metavar "REACH" <> OA.help "Reach level."))
    sizeP = OA.optional (OA.option (Item.Commons.parseEnumOption Project.projectItemSizeLabel) (OA.long "size" <> OA.metavar "SIZE" <> OA.help "Size level."))
    difficultyP = OA.optional (OA.option (Item.Commons.parseEnumOption Project.projectItemDifficultyLabel) (OA.long "difficulty" <> OA.metavar "DIFFICULTY" <> OA.help "Difficulty level."))
    confidenceP = OA.optional (OA.option (Item.Commons.parseEnumOption Project.projectItemConfidenceLabel) (OA.long "confidence" <> OA.metavar "CONFIDENCE" <> OA.help "Confidence level."))
    themeP = OA.optional (OA.option (Item.Commons.parseEnumOption Project.projectItemThemeLabel) (OA.long "theme" <> OA.metavar "THEME" <> OA.help "Strategic theme."))
    issueTypeP = OA.optional (OA.option (Item.Commons.parseEnumOption Project.issueTypeLabel) (OA.long "issue-type" <> OA.metavar "TYPE" <> OA.help "Issue type (org repos only)."))


-- * Runner


runCreate :: Config -> CreateOptions -> IO ExitCode
runCreate _cfg opts = do
  isTty <- hIsTerminalDevice stdin
  when (createOptInteractive opts && not isTty) $ die "Interactive mode requires a TTY."
  resolvedBody <- traverse Item.Commons.resolveBodySource (createOptBodySource opts)
  let isDraft = createOptDraft opts
      defaults = (createOptRepo opts, createOptTitle opts, resolvedBody)
  projectConfigs <- Item.Commons.loadProjectConfigs
  let requiredReady = requiredOptionsProvided isDraft defaults
      projectIdent = (,) <$> createOptOwner opts <*> createOptProjectNumber opts
      needsProjectSelection = case projectIdent of
        Nothing -> True
        Just si -> case Item.Commons.filterProjectConfigs si projectConfigs of
          [] -> True
          [_] -> False
          _ -> True
      interactive = createOptInteractive opts || (isTty && (not requiredReady || needsProjectSelection))
  (projectConfig, inputs) <-
    if interactive
      then do
        selected <- promptProjectConfig projectIdent projectConfigs
        case selected of
          Nothing -> die "No matching project config found. Run `prix project sync`."
          Just cfg -> do
            filled <- promptInputs cfg defaults opts
            pure (cfg, filled)
      else do
        selected <- case projectIdent of
          Nothing -> die "Project owner and number are required when not running interactively."
          Just si -> Item.Commons.selectProjectConfig si projectConfigs
        pure (selected, toInputs defaults opts)
  validateInputs inputs
  assigneeIds <- Item.Commons.resolveAssigneeIds (createAssignees inputs)
  itemId <-
    if createIsDraft inputs
      then createDraftItem projectConfig inputs assigneeIds
      else createFromNewIssue projectConfig inputs assigneeIds
  applyFieldUpdates projectConfig itemId inputs
  pure ExitSuccess


-- * Inputs


data CreateInputs = MkCreateInputs
  { createIsDraft :: !Bool
  , createRepo :: !(Maybe T.Text)
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
  , createIssueType :: !(Maybe Project.IssueType)
  }
  deriving (Show, Eq)


toInputs :: (Maybe T.Text, Maybe T.Text, Maybe T.Text) -> CreateOptions -> CreateInputs
toInputs (repo, title, body) MkCreateOptions {..} =
  MkCreateInputs
    { createIsDraft = createOptDraft
    , createRepo = repo
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
    , createIssueType = createOptIssueType
    }


requiredOptionsProvided :: Bool -> (Maybe T.Text, Maybe T.Text, Maybe T.Text) -> Bool
requiredOptionsProvided isDraft (mRepo, mTitle, _) =
  (isDraft || maybe False (not . T.null) mRepo) && maybe False (not . T.null) mTitle


validateInputs :: CreateInputs -> IO ()
validateInputs MkCreateInputs {..} = do
  unless createIsDraft . when (isNothing createRepo) $ die "Repository is required."
  when (isNothing createTitle) $ die "Title is required."


-- * GitHub IO


createDraftItem :: ProjectConfig.ProjectConfig -> CreateInputs -> [T.Text] -> IO T.Text
createDraftItem cfg MkCreateInputs {..} assigneeIds = do
  let projectId = ProjectConfig.projectConfigId cfg
      title = fromMaybe "[Undefined Title]" createTitle
  Commons.ghCreateDraftItem projectId title createBody assigneeIds >>= either die pure


createFromNewIssue :: ProjectConfig.ProjectConfig -> CreateInputs -> [T.Text] -> IO T.Text
createFromNewIssue cfg inputs@MkCreateInputs {..} assigneeIds = do
  repo <- maybe (die "Missing required repo.") pure createRepo
  (repoOwner, repoName) <- case T.splitOn "/" repo of
    [o, n] -> pure (o, n)
    _ -> die "Repository must be in OWNER/REPO format."
  repoId <- Commons.ghLookupRepoId repoOwner repoName >>= either die pure
  orgIssueTypes <- Commons.ghGetOrgIssueTypes repoOwner
  mIssueTypeId <- Item.Commons.resolveIssueTypeId orgIssueTypes createIssueType
  issueId <- createIssue repoId inputs mIssueTypeId assigneeIds
  addItemToProject cfg issueId


createIssue :: T.Text -> CreateInputs -> Maybe T.Text -> [T.Text] -> IO T.Text
createIssue repoId MkCreateInputs {..} mIssueTypeId assigneeIds = do
  let title = fromMaybe "[Undefined Title]" createTitle
      body = fromMaybe "[Undefined Body]" createBody
  Commons.ghCreateIssue repoId title body assigneeIds mIssueTypeId >>= either die pure


addItemToProject :: ProjectConfig.ProjectConfig -> T.Text -> IO T.Text
addItemToProject cfg contentId = do
  let projectId = ProjectConfig.projectConfigId cfg
  Commons.ghAddContentToProject projectId contentId >>= either die pure


applyFieldUpdates :: ProjectConfig.ProjectConfig -> T.Text -> CreateInputs -> IO ()
applyFieldUpdates cfg itemId inputs = do
  updates <- either die pure $ buildFieldUpdates cfg inputs
  forM_ updates $ \Item.Commons.FieldUpdate {..} -> Item.Commons.updateProjectField cfg itemId fieldUpdateId fieldUpdateValue


-- * Field Updates


buildFieldUpdates :: ProjectConfig.ProjectConfig -> CreateInputs -> Either String [Item.Commons.FieldUpdate]
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
  pure $ catMaybes [status, iteration, urgency, impact, reach, size, difficulty, confidence, theme, score]
  where
    buildSingleSelect fieldLabel labelFn value = do
      field <- Item.Commons.requireSingleSelect cfg fieldLabel
      optionId <- Item.Commons.selectOptionId fieldLabel (labelFn value) (ProjectConfig.projectConfigFieldSingleSelectOptions field)
      pure $ Item.Commons.FieldUpdate (ProjectConfig.projectConfigFieldSingleSelectId field) (Aeson.object ["singleSelectOptionId" Aeson..= optionId])
    buildIteration fieldLabel value = do
      field <- Item.Commons.requireIteration cfg fieldLabel
      iterId <- Item.Commons.selectIterationId fieldLabel value (ProjectConfig.projectConfigFieldIterationConfiguration field)
      pure $ Item.Commons.FieldUpdate (ProjectConfig.projectConfigFieldIterationId field) (Aeson.object ["iterationId" Aeson..= iterId])
    buildNumber fieldLabel value = do
      field <- Item.Commons.requireCommon cfg fieldLabel
      Item.Commons.ensureDataType fieldLabel "NUMBER" (ProjectConfig.projectConfigFieldCommonDataType field)
      pure $ Item.Commons.FieldUpdate (ProjectConfig.projectConfigFieldCommonId field) (Aeson.object ["number" Aeson..= value])
    computedScore =
      Project.projectItemPriority
        <$> createUrgency
        <*> createReach
        <*> createImpact
        <*> createConfidence
        <*> createSize
        <*> createDifficulty


-- * Prompting


promptProjectConfig :: Maybe (T.Text, Int) -> [ProjectConfig.ProjectConfig] -> IO (Maybe ProjectConfig.ProjectConfig)
promptProjectConfig mPId configs =
  promptProjectConfig' ((`Item.Commons.findProjectByIdent` configs) =<< mPId) configs


promptProjectConfig' :: Maybe ProjectConfig.ProjectConfig -> [ProjectConfig.ProjectConfig] -> IO (Maybe ProjectConfig.ProjectConfig)
promptProjectConfig' mDef config =
  Z.Term.Prompts.choose "Select a project:" Item.Commons.projectConfigToText mDef config >>= maybe (pure mDef) (pure . Just)


promptInputs :: ProjectConfig.ProjectConfig -> (Maybe T.Text, Maybe T.Text, Maybe T.Text) -> CreateOptions -> IO CreateInputs
promptInputs cfg (repoDefault, titleDefault, bodyDefault) MkCreateOptions {..} = do
  let createIsDraft = createOptDraft
  createRepo <-
    if createOptDraft
      then pure Nothing
      else Z.Term.Prompts.text "Repository (owner/repo)" repoDefault
  orgIssueTypes <-
    if createOptDraft
      then pure []
      else maybe (pure []) Item.Commons.getRepoOrgIssueTypes createRepo
  createTitle <- Z.Term.Prompts.text "Title" titleDefault
  createBody <- Z.Term.Prompts.multilineText "Body" bodyDefault
  createAssignees <- Item.Commons.promptAssignees createOptAssignees
  createIssueType <-
    if createOptDraft
      then pure Nothing
      else Item.Commons.promptSelectOptional "Issue Type" Project.issueTypeLabel createOptIssueType (Item.Commons.matchingIssueTypes orgIssueTypes)
  createStatus <- Z.Term.Prompts.choose "Status" Project.projectItemStatusLabel createOptStatus Z.Base.enumerate
  createIteration <- Z.Term.Prompts.choose "Iteration" Z.Text.tshow createOptIteration (fmap fst (Item.Commons.projectConfigIterations cfg))
  createUrgency <- Z.Term.Prompts.choose "Urgency" Project.projectItemUrgencyLabel createOptUrgency Z.Base.enumerate
  createImpact <- Z.Term.Prompts.choose "Impact" Project.projectItemImpactLabel createOptImpact Z.Base.enumerate
  createReach <- Z.Term.Prompts.choose "Reach" Project.projectItemReachLabel createOptReach Z.Base.enumerate
  createSize <- Z.Term.Prompts.choose "Size" Project.projectItemSizeLabel createOptSize Z.Base.enumerate
  createDifficulty <- Z.Term.Prompts.choose "Difficulty" Project.projectItemDifficultyLabel createOptDifficulty Z.Base.enumerate
  createConfidence <- Z.Term.Prompts.choose "Confidence" Project.projectItemConfidenceLabel createOptConfidence Z.Base.enumerate
  createTheme <- Z.Term.Prompts.choose "Theme" Project.projectItemThemeLabel createOptTheme Z.Base.enumerate
  pure MkCreateInputs {..}
