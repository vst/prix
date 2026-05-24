{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Prix.Cli.Project.Item.Create (
  CreateOptions,
  createOptionsParser,
  runCreate,
) where

import Control.Monad (forM_, unless, when)
import qualified Data.Aeson as Aeson
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.String.Interpolate (i)
import qualified Data.Text as T
import qualified Data.Time as Time
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
  , createOptDeadline :: !(Maybe Time.Day)
  , createOptImpact :: !(Maybe Project.ProjectItemImpact)
  , createOptScope :: !(Maybe Project.ProjectItemScope)
  , createOptSeverity :: !(Maybe Project.ProjectItemSeverity)
  , createOptRisk :: !(Maybe Project.ProjectItemRisk)
  , createOptFootprint :: !(Maybe Project.ProjectItemFootprint)
  , createOptComplexity :: !(Maybe Project.ProjectItemComplexity)
  , createOptConfidence :: !(Maybe Project.ProjectItemConfidence)
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
    <*> deadlineP
    <*> impactP
    <*> scopeP
    <*> severityP
    <*> riskP
    <*> footprintP
    <*> complexityP
    <*> confidenceP
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
    deadlineP = OA.optional (OA.option OA.auto (OA.long "deadline" <> OA.metavar "YYYY-MM-DD" <> OA.help "Deadline date."))
    impactP = OA.optional (OA.option (Item.Commons.parseEnumOption Project.projectItemImpactLabel) (OA.long "impact" <> OA.metavar "IMPACT" <> OA.help "Impact level."))
    scopeP = OA.optional (OA.option (Item.Commons.parseEnumOption Project.projectItemScopeLabel) (OA.long "scope" <> OA.metavar "SCOPE" <> OA.help "Scope level."))
    severityP = OA.optional (OA.option (Item.Commons.parseEnumOption Project.projectItemSeverityLabel) (OA.long "severity" <> OA.metavar "SEVERITY" <> OA.help "Severity level."))
    riskP = OA.optional (OA.option (Item.Commons.parseEnumOption Project.projectItemRiskLabel) (OA.long "risk" <> OA.metavar "RISK" <> OA.help "Risk level."))
    footprintP = OA.optional (OA.option (Item.Commons.parseEnumOption Project.projectItemFootprintLabel) (OA.long "footprint" <> OA.metavar "FOOTPRINT" <> OA.help "Footprint level."))
    complexityP = OA.optional (OA.option (Item.Commons.parseEnumOption Project.projectItemComplexityLabel) (OA.long "complexity" <> OA.metavar "COMPLEXITY" <> OA.help "Complexity level."))
    confidenceP = OA.optional (OA.option (Item.Commons.parseEnumOption Project.projectItemConfidenceLabel) (OA.long "confidence" <> OA.metavar "CONFIDENCE" <> OA.help "Confidence level."))
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
  mDbId <- Commons.ghGetItemFullDatabaseId itemId
  case mDbId of
    Right dbId -> putStrLn [i|Created: #{itemId}\nURL: #{Item.Commons.projectItemUrl projectConfig dbId}|]
    Left _ -> putStrLn [i|Created: #{itemId}|]
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
  , createDeadline :: !(Maybe Time.Day)
  , createImpact :: !(Maybe Project.ProjectItemImpact)
  , createScope :: !(Maybe Project.ProjectItemScope)
  , createSeverity :: !(Maybe Project.ProjectItemSeverity)
  , createRisk :: !(Maybe Project.ProjectItemRisk)
  , createFootprint :: !(Maybe Project.ProjectItemFootprint)
  , createComplexity :: !(Maybe Project.ProjectItemComplexity)
  , createConfidence :: !(Maybe Project.ProjectItemConfidence)
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
    , createDeadline = createOptDeadline
    , createImpact = createOptImpact
    , createScope = createOptScope
    , createSeverity = createOptSeverity
    , createRisk = createOptRisk
    , createFootprint = createOptFootprint
    , createComplexity = createOptComplexity
    , createConfidence = createOptConfidence
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
  deadline <- traverse (buildDate "Deadline") createDeadline
  impact <- traverse (buildSingleSelect "Impact" Project.projectItemImpactLabel) createImpact
  scope <- traverse (buildSingleSelect "Scope" Project.projectItemScopeLabel) createScope
  severity <- traverse (buildSingleSelect "Severity" Project.projectItemSeverityLabel) createSeverity
  risk <- traverse (buildSingleSelect "Risk" Project.projectItemRiskLabel) createRisk
  footprint <- traverse (buildSingleSelect "Footprint" Project.projectItemFootprintLabel) createFootprint
  complexity <- traverse (buildSingleSelect "Complexity" Project.projectItemComplexityLabel) createComplexity
  confidence <- traverse (buildSingleSelect "Confidence" Project.projectItemConfidenceLabel) createConfidence
  iteration <- traverse (buildIteration "Iteration") createIteration
  score <- traverse (buildNumber "Score") computedScore
  pure $ catMaybes [status, iteration, deadline, impact, scope, severity, risk, footprint, complexity, confidence, score]
  where
    buildSingleSelect fieldLabel labelFn value = do
      field <- Item.Commons.requireSingleSelect cfg fieldLabel
      optionId <- Item.Commons.selectOptionId fieldLabel (labelFn value) (ProjectConfig.projectConfigFieldSingleSelectOptions field)
      pure $ Item.Commons.FieldUpdate (ProjectConfig.projectConfigFieldSingleSelectId field) (Aeson.object ["singleSelectOptionId" Aeson..= optionId])
    buildIteration fieldLabel value = do
      field <- Item.Commons.requireIteration cfg fieldLabel
      iterId <- Item.Commons.selectIterationId fieldLabel value (ProjectConfig.projectConfigFieldIterationConfiguration field)
      pure $ Item.Commons.FieldUpdate (ProjectConfig.projectConfigFieldIterationId field) (Aeson.object ["iterationId" Aeson..= iterId])
    buildDate fieldLabel value = do
      field <- Item.Commons.requireCommon cfg fieldLabel
      Item.Commons.ensureDataType fieldLabel "DATE" (ProjectConfig.projectConfigFieldCommonDataType field)
      pure $ Item.Commons.FieldUpdate (ProjectConfig.projectConfigFieldCommonId field) (Aeson.object ["date" Aeson..= value])
    buildNumber fieldLabel value = do
      field <- Item.Commons.requireCommon cfg fieldLabel
      Item.Commons.ensureDataType fieldLabel "NUMBER" (ProjectConfig.projectConfigFieldCommonDataType field)
      pure $ Item.Commons.FieldUpdate (ProjectConfig.projectConfigFieldCommonId field) (Aeson.object ["number" Aeson..= value])
    computedScore =
      Project.projectItemScoreEstimate
        <$> createImpact
        <*> createScope
        <*> createSeverity
        <*> createRisk
        <*> createConfidence
        <*> createFootprint
        <*> createComplexity


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
  createDeadline <- Item.Commons.promptDayOptional "Deadline" createOptDeadline
  createImpact <- Z.Term.Prompts.choose "Impact" Project.projectItemImpactLabel createOptImpact Z.Base.enumerate
  createScope <- Z.Term.Prompts.choose "Scope" Project.projectItemScopeLabel createOptScope Z.Base.enumerate
  createSeverity <- Z.Term.Prompts.choose "Severity" Project.projectItemSeverityLabel createOptSeverity Z.Base.enumerate
  createRisk <- Z.Term.Prompts.choose "Risk" Project.projectItemRiskLabel createOptRisk Z.Base.enumerate
  createFootprint <- Z.Term.Prompts.choose "Footprint" Project.projectItemFootprintLabel createOptFootprint Z.Base.enumerate
  createComplexity <- Z.Term.Prompts.choose "Complexity" Project.projectItemComplexityLabel createOptComplexity Z.Base.enumerate
  createConfidence <- Z.Term.Prompts.choose "Confidence" Project.projectItemConfidenceLabel createOptConfidence Z.Base.enumerate
  pure MkCreateInputs {..}
