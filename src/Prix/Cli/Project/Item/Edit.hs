{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Prix.Cli.Project.Item.Edit (
  EditOptions (..),
  editOptionsParser,
  runEdit,
) where

import Control.Applicative (asum, some, (<|>))
import Control.Monad (forM_, unless, when)
import qualified Data.Aeson as Aeson
import Data.Char (toUpper)
import Data.Fixed (Milli)
import Data.List ((\\))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, fromMaybe)
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
import qualified Zamazingo.Control as Z.Control
import qualified Zamazingo.Terminal.Prompts as Z.Term.Prompts
import qualified Zamazingo.Text as Z.Text


-- * CLI Options


data EditOptions = MkEditOptions
  { editOptInteractive :: !Bool
  , editOptItemId :: !(Maybe T.Text)
  , editOptTitle :: !(Update T.Text)
  , editOptBody :: !(Update Item.Commons.BodySource)
  , editOptAssignees :: !(Update [T.Text])
  , editOptStatus :: !(Update Project.ProjectItemStatus)
  , editOptIteration :: !(Update Integer)
  , editOptDeadline :: !(Update Time.Day)
  , editOptImpact :: !(Update Project.ProjectItemImpact)
  , editOptScope :: !(Update Project.ProjectItemScope)
  , editOptSeverity :: !(Update Project.ProjectItemSeverity)
  , editOptRisk :: !(Update Project.ProjectItemRisk)
  , editOptFootprint :: !(Update Project.ProjectItemFootprint)
  , editOptComplexity :: !(Update Project.ProjectItemComplexity)
  , editOptConfidence :: !(Update Project.ProjectItemConfidence)
  , editOptIssueType :: !(Update Project.IssueType)
  }
  deriving (Show, Eq, Generic)


editOptionsParser :: OA.Parser EditOptions
editOptionsParser =
  MkEditOptions
    <$> interactiveP
    <*> itemIdP
    <*> titleP
    <*> bodyP
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
    itemIdP = OA.optional (T.pack <$> OA.strOption (OA.long "id" <> OA.metavar "ITEM_ID" <> OA.help "Project item ID."))
    titleP = UpdateSet . T.pack <$> OA.strOption (OA.long "title" <> OA.metavar "TITLE" <> OA.help "Item title.") <|> pure UpdateKeep
    bodyP = UpdateSet <$> Item.Commons.bodySourceParser <|> pure UpdateKeep
    assigneesP =
      asum
        [ UpdateClear <$ OA.flag' () (OA.long "no-assignees" <> OA.help "Clear assignees.")
        , UpdateSet <$> some (T.pack <$> OA.strOption (OA.long "assignee" <> OA.metavar "LOGIN" <> OA.help "Assignee login (repeatable)."))
        , pure UpdateKeep
        ]
    statusP = updateFieldParser "status" (Item.Commons.parseEnumOption Project.projectItemStatusLabel) "Item status."
    iterationP = updateFieldParser "iteration" OA.auto "Iteration number."
    deadlineP = updateFieldParser "deadline" OA.auto "Deadline date."
    impactP = updateFieldParser "impact" (Item.Commons.parseEnumOption Project.projectItemImpactLabel) "Impact level."
    scopeP = updateFieldParser "scope" (Item.Commons.parseEnumOption Project.projectItemScopeLabel) "Scope level."
    severityP = updateFieldParser "severity" (Item.Commons.parseEnumOption Project.projectItemSeverityLabel) "Severity level."
    riskP = updateFieldParser "risk" (Item.Commons.parseEnumOption Project.projectItemRiskLabel) "Risk level."
    footprintP = updateFieldParser "footprint" (Item.Commons.parseEnumOption Project.projectItemFootprintLabel) "Footprint level."
    complexityP = updateFieldParser "complexity" (Item.Commons.parseEnumOption Project.projectItemComplexityLabel) "Complexity level."
    confidenceP = updateFieldParser "confidence" (Item.Commons.parseEnumOption Project.projectItemConfidenceLabel) "Confidence level."
    issueTypeP = updateFieldParser "issue-type" (Item.Commons.parseEnumOption Project.issueTypeLabel) "Issue type (org repos only)."


-- * Runner


runEdit :: Config -> EditOptions -> IO ExitCode
runEdit _cfg opts = do
  isTty <- hIsTerminalDevice stdin
  when (editOptInteractive opts && not isTty) $ die "Interactive mode requires a TTY."
  itemId <- case editOptItemId opts of
    Just pid -> pure pid
    Nothing -> if isTty then promptItemId else die "Missing required --id."
  (project, item) <- Item.Commons.loadProjectItem itemId
  projectConfig <- Item.Commons.loadProjectConfig project
  bodyUpdate <- resolveBodyUpdate (editOptBody opts)
  let current = currentFromItem item
      defaults = defaultsFromUpdates opts bodyUpdate current
      needsPrompt = isTty && (not (hasUpdates opts) || editOptInteractive opts)
  inputs <- if needsPrompt then promptInputs projectConfig item defaults else pure (toInputs defaults)
  applyEdits projectConfig item inputs current
  mDbId <- Commons.ghGetItemFullDatabaseId itemId
  case mDbId of
    Right dbId -> putStrLn [i|Updated: #{itemId}\nURL: #{Item.Commons.projectItemUrl projectConfig dbId}|]
    Left _ -> putStrLn [i|Updated: #{itemId}|]
  pure ExitSuccess


data CurrentItem = MkCurrentItem
  { currentTitle :: !T.Text
  , currentBody :: !(Maybe T.Text)
  , currentAssignees :: ![T.Text]
  , currentStatus :: !(Maybe Project.ProjectItemStatus)
  , currentIteration :: !(Maybe Integer)
  , currentDeadline :: !(Maybe Time.Day)
  , currentImpact :: !(Maybe Project.ProjectItemImpact)
  , currentScope :: !(Maybe Project.ProjectItemScope)
  , currentSeverity :: !(Maybe Project.ProjectItemSeverity)
  , currentRisk :: !(Maybe Project.ProjectItemRisk)
  , currentFootprint :: !(Maybe Project.ProjectItemFootprint)
  , currentComplexity :: !(Maybe Project.ProjectItemComplexity)
  , currentConfidence :: !(Maybe Project.ProjectItemConfidence)
  , currentScore :: !(Maybe Milli)
  , currentIssueType :: !(Maybe Project.IssueType)
  }


data EditInputs = MkEditInputs
  { editTitle :: !T.Text
  , editBody :: !(Maybe T.Text)
  , editAssignees :: ![T.Text]
  , editStatus :: !(Maybe Project.ProjectItemStatus)
  , editIteration :: !(Maybe Integer)
  , editDeadline :: !(Maybe Time.Day)
  , editImpact :: !(Maybe Project.ProjectItemImpact)
  , editScope :: !(Maybe Project.ProjectItemScope)
  , editSeverity :: !(Maybe Project.ProjectItemSeverity)
  , editRisk :: !(Maybe Project.ProjectItemRisk)
  , editFootprint :: !(Maybe Project.ProjectItemFootprint)
  , editComplexity :: !(Maybe Project.ProjectItemComplexity)
  , editConfidence :: !(Maybe Project.ProjectItemConfidence)
  , editIssueType :: !(Maybe Project.IssueType)
  }


currentFromItem :: Project.ProjectItem -> CurrentItem
currentFromItem Project.MkProjectItem {..} =
  MkCurrentItem
    { currentTitle = projectItemTitle
    , currentBody = projectItemBody
    , currentAssignees = foldMap NE.toList projectItemAssignees
    , currentStatus = projectItemStatus
    , currentIteration = projectItemIteration
    , currentDeadline = projectItemDeadline
    , currentImpact = projectItemImpact
    , currentScope = projectItemScope
    , currentSeverity = projectItemSeverity
    , currentRisk = projectItemRisk
    , currentFootprint = projectItemFootprint
    , currentComplexity = projectItemComplexity
    , currentConfidence = projectItemConfidence
    , currentScore = projectItemScore
    , currentIssueType = case projectItemContent of
        Project.ProjectItemContentIssue issue -> Project.issueContentIssueType issue
        _ -> Nothing
    }


type EditDefaults =
  ( Maybe T.Text
  , Maybe T.Text
  , [T.Text]
  , Maybe Project.ProjectItemStatus
  , Maybe Integer
  , Maybe Time.Day
  , Maybe Project.ProjectItemImpact
  , Maybe Project.ProjectItemScope
  , Maybe Project.ProjectItemSeverity
  , Maybe Project.ProjectItemRisk
  , Maybe Project.ProjectItemFootprint
  , Maybe Project.ProjectItemComplexity
  , Maybe Project.ProjectItemConfidence
  , Maybe Project.IssueType
  )


defaultsFromUpdates :: EditOptions -> Update T.Text -> CurrentItem -> EditDefaults
defaultsFromUpdates MkEditOptions {..} bodyUpdate current =
  ( Just (applyTextUpdate editOptTitle (currentTitle current))
  , applyMaybeUpdate bodyUpdate (currentBody current)
  , applyListUpdate editOptAssignees (currentAssignees current)
  , applyMaybeUpdate editOptStatus (currentStatus current)
  , applyMaybeUpdate editOptIteration (currentIteration current)
  , applyMaybeUpdate editOptDeadline (currentDeadline current)
  , applyMaybeUpdate editOptImpact (currentImpact current)
  , applyMaybeUpdate editOptScope (currentScope current)
  , applyMaybeUpdate editOptSeverity (currentSeverity current)
  , applyMaybeUpdate editOptRisk (currentRisk current)
  , applyMaybeUpdate editOptFootprint (currentFootprint current)
  , applyMaybeUpdate editOptComplexity (currentComplexity current)
  , applyMaybeUpdate editOptConfidence (currentConfidence current)
  , applyMaybeUpdate editOptIssueType (currentIssueType current)
  )


toInputs :: EditDefaults -> EditInputs
toInputs (mTitle, mBody, assignees, status, iteration, deadline, impact, scope, severity, risk, footprint, complexity, confidence, issueType) =
  MkEditInputs
    { editTitle = fromMaybe "" mTitle
    , editBody = mBody
    , editAssignees = assignees
    , editStatus = status
    , editIteration = iteration
    , editDeadline = deadline
    , editImpact = impact
    , editScope = scope
    , editSeverity = severity
    , editRisk = risk
    , editFootprint = footprint
    , editComplexity = complexity
    , editConfidence = confidence
    , editIssueType = issueType
    }


applyEdits :: ProjectConfig.ProjectConfig -> Project.ProjectItem -> EditInputs -> CurrentItem -> IO ()
applyEdits cfg item MkEditInputs {..} current = do
  let itemId = Project.projectItemId item
      content = Project.projectItemContent item
      titleChanged = editTitle /= currentTitle current
      bodyChanged = editBody /= currentBody current
      assigneesChanged = editAssignees /= currentAssignees current
      issueTypeChanged = editIssueType /= currentIssueType current
  -- Apply content-specific updates, returning whether anything changed.
  contentOrAssigneesChanged <- case content of
    Project.ProjectItemContentDraftIssue draft -> do
      -- Draft items: title, body, and assignees are all set in one mutation.
      let changed = titleChanged || bodyChanged || assigneesChanged
      when changed $ do
        assigneeIds <- Item.Commons.resolveAssigneeIds editAssignees
        Commons.ghUpdateDraftIssue (Project.draftIssueContentId draft) editTitle editBody assigneeIds
          >>= either die Z.Control.discard
      pure changed
    Project.ProjectItemContentIssue issue -> do
      mIssueTypeId <-
        if issueTypeChanged
          then do
            orgTypes <- Item.Commons.getRepoOrgIssueTypes (Project.issueContentRepository issue)
            Item.Commons.resolveIssueTypeId orgTypes editIssueType
          else pure Nothing
      let contentChanged = titleChanged || bodyChanged || issueTypeChanged
      when contentChanged $ updateIssue (Project.issueContentId issue) editTitle editBody mIssueTypeId
      when assigneesChanged $ updateAssignableAssignees (Project.issueContentId issue) (currentAssignees current) editAssignees
      pure (contentChanged || assigneesChanged)
    Project.ProjectItemContentPullRequest pr -> do
      let contentChanged = titleChanged || bodyChanged
      when contentChanged $ updatePullRequest (Project.pullRequestContentId pr) editTitle editBody
      when assigneesChanged $ updateAssignableAssignees (Project.pullRequestContentId pr) (currentAssignees current) editAssignees
      pure (contentChanged || assigneesChanged)
  updates <- either die pure $ buildFieldUpdates cfg current MkEditInputs {..}
  if null updates && not contentOrAssigneesChanged
    then putStrLn "Nothing to update."
    else forM_ updates $ \Item.Commons.FieldUpdate {..} -> Item.Commons.updateProjectField cfg itemId fieldUpdateId fieldUpdateValue


-- * Prompting


promptItemId :: IO T.Text
promptItemId = do
  mLine <- Z.Term.Prompts.text "Project item ID" Nothing
  case mLine of
    Just txt | not (T.null txt) -> pure txt
    _ -> promptItemId


promptInputs :: ProjectConfig.ProjectConfig -> Project.ProjectItem -> EditDefaults -> IO EditInputs
promptInputs cfg item defaults = do
  let (mTitle, mBody, assignees, status, iteration, deadline, impact, scope, severity, risk, footprint, complexity, confidence, issueType) = defaults
  let titleDefault = fromMaybe "" mTitle
  editTitle <- fromMaybe titleDefault <$> Z.Term.Prompts.text "Title" (Just titleDefault)
  editBody <- Z.Term.Prompts.multilineText "Body" mBody
  editAssignees <- Item.Commons.promptAssignees assignees
  orgIssueTypes <- case Project.projectItemContent item of
    Project.ProjectItemContentIssue issue -> Item.Commons.getRepoOrgIssueTypes (Project.issueContentRepository issue)
    _ -> pure []
  editIssueType <- Item.Commons.promptSelectOptional "Issue Type" Project.issueTypeLabel issueType (Item.Commons.matchingIssueTypes orgIssueTypes)
  editStatus <- Item.Commons.promptSelectOptional "Status" Project.projectItemStatusLabel status Z.Base.enumerate
  editIteration <- Item.Commons.promptSelectOptional "Iteration" Z.Text.tshow iteration (fmap fst (Item.Commons.projectConfigIterations cfg))
  editDeadline <- Item.Commons.promptDayOptional "Deadline" deadline
  editImpact <- Item.Commons.promptSelectOptional "Impact" Project.projectItemImpactLabel impact Z.Base.enumerate
  editScope <- Item.Commons.promptSelectOptional "Scope" Project.projectItemScopeLabel scope Z.Base.enumerate
  editSeverity <- Item.Commons.promptSelectOptional "Severity" Project.projectItemSeverityLabel severity Z.Base.enumerate
  editRisk <- Item.Commons.promptSelectOptional "Risk" Project.projectItemRiskLabel risk Z.Base.enumerate
  editFootprint <- Item.Commons.promptSelectOptional "Footprint" Project.projectItemFootprintLabel footprint Z.Base.enumerate
  editComplexity <- Item.Commons.promptSelectOptional "Complexity" Project.projectItemComplexityLabel complexity Z.Base.enumerate
  editConfidence <- Item.Commons.promptSelectOptional "Confidence" Project.projectItemConfidenceLabel confidence Z.Base.enumerate
  pure MkEditInputs {..}


-- * GitHub IO


updateIssue :: T.Text -> T.Text -> Maybe T.Text -> Maybe T.Text -> IO ()
updateIssue issueId title body mIssueTypeId =
  Commons.ghUpdateIssue issueId title body mIssueTypeId >>= either die Z.Control.discard


updatePullRequest :: T.Text -> T.Text -> Maybe T.Text -> IO ()
updatePullRequest prId title body =
  Commons.ghUpdatePullRequest prId title body >>= either die Z.Control.discard


updateAssignableAssignees :: T.Text -> [T.Text] -> [T.Text] -> IO ()
updateAssignableAssignees assignableId current desired = do
  currentIds <- Item.Commons.resolveAssigneeIds current
  desiredIds <- Item.Commons.resolveAssigneeIds desired
  let toAdd = desiredIds \\ currentIds
      toRemove = currentIds \\ desiredIds
  unless (null toAdd) $ addAssigneesToAssignable assignableId toAdd
  unless (null toRemove) $ removeAssigneesFromAssignable assignableId toRemove


addAssigneesToAssignable :: T.Text -> [T.Text] -> IO ()
addAssigneesToAssignable _ [] = pure ()
addAssigneesToAssignable assignableId assigneeIds =
  Commons.ghAddAssigneesToAssignable assignableId assigneeIds >>= either die Z.Control.discard


removeAssigneesFromAssignable :: T.Text -> [T.Text] -> IO ()
removeAssigneesFromAssignable _ [] = pure ()
removeAssigneesFromAssignable assignableId assigneeIds =
  Commons.ghRemoveAssigneesFromAssignable assignableId assigneeIds >>= either die Z.Control.discard


buildFieldUpdates :: ProjectConfig.ProjectConfig -> CurrentItem -> EditInputs -> Either String [Item.Commons.FieldUpdate]
buildFieldUpdates cfg current MkEditInputs {..} = do
  let MkCurrentItem {..} = current
  status <- buildSingleSelect "Status" Project.projectItemStatusLabel currentStatus editStatus
  deadline <- buildDate "Deadline" currentDeadline editDeadline
  impact <- buildSingleSelect "Impact" Project.projectItemImpactLabel currentImpact editImpact
  scope <- buildSingleSelect "Scope" Project.projectItemScopeLabel currentScope editScope
  severity <- buildSingleSelect "Severity" Project.projectItemSeverityLabel currentSeverity editSeverity
  risk <- buildSingleSelect "Risk" Project.projectItemRiskLabel currentRisk editRisk
  footprint <- buildSingleSelect "Footprint" Project.projectItemFootprintLabel currentFootprint editFootprint
  complexity <- buildSingleSelect "Complexity" Project.projectItemComplexityLabel currentComplexity editComplexity
  confidence <- buildSingleSelect "Confidence" Project.projectItemConfidenceLabel currentConfidence editConfidence
  iteration <- buildIteration "Iteration" currentIteration editIteration
  score <- buildScoreUpdate currentScore
  pure $ catMaybes [status, iteration, deadline, impact, scope, severity, risk, footprint, complexity, confidence, score]
  where
    buildSingleSelect fieldLabel labelFn currentVal desiredVal
      | currentVal == desiredVal = Right Nothing
      | otherwise = do
          field <- Item.Commons.requireSingleSelect cfg fieldLabel
          case desiredVal of
            Nothing ->
              pure . Just $
                Item.Commons.FieldUpdate
                  (ProjectConfig.projectConfigFieldSingleSelectId field)
                  (Aeson.object ["singleSelectOptionId" Aeson..= Aeson.Null])
            Just value -> do
              optionId <- Item.Commons.selectOptionId fieldLabel (labelFn value) (ProjectConfig.projectConfigFieldSingleSelectOptions field)
              pure . Just $
                Item.Commons.FieldUpdate
                  (ProjectConfig.projectConfigFieldSingleSelectId field)
                  (Aeson.object ["singleSelectOptionId" Aeson..= optionId])
    buildIteration fieldLabel currentVal desiredVal
      | currentVal == desiredVal = Right Nothing
      | otherwise = do
          field <- Item.Commons.requireIteration cfg fieldLabel
          case desiredVal of
            Nothing ->
              pure . Just $
                Item.Commons.FieldUpdate
                  (ProjectConfig.projectConfigFieldIterationId field)
                  (Aeson.object ["iterationId" Aeson..= Aeson.Null])
            Just value -> do
              iterId <- Item.Commons.selectIterationId fieldLabel value (ProjectConfig.projectConfigFieldIterationConfiguration field)
              pure . Just $
                Item.Commons.FieldUpdate
                  (ProjectConfig.projectConfigFieldIterationId field)
                  (Aeson.object ["iterationId" Aeson..= iterId])
    buildDate fieldLabel currentVal desiredVal
      | currentVal == desiredVal = Right Nothing
      | otherwise = do
          field <- Item.Commons.requireCommon cfg fieldLabel
          Item.Commons.ensureDataType fieldLabel "DATE" (ProjectConfig.projectConfigFieldCommonDataType field)
          pure . Just $
            Item.Commons.FieldUpdate
              (ProjectConfig.projectConfigFieldCommonId field)
              (Aeson.object ["date" Aeson..= maybe Aeson.Null Aeson.toJSON desiredVal])
    buildNumber fieldLabel currentVal desiredVal
      | currentVal == desiredVal = Right Nothing
      | otherwise = do
          field <- Item.Commons.requireCommon cfg fieldLabel
          Item.Commons.ensureDataType fieldLabel "NUMBER" (ProjectConfig.projectConfigFieldCommonDataType field)
          pure . Just $
            Item.Commons.FieldUpdate
              (ProjectConfig.projectConfigFieldCommonId field)
              (Aeson.object ["number" Aeson..= maybe Aeson.Null (Aeson.toJSON :: Milli -> Aeson.Value) desiredVal])
    buildScoreUpdate scoreNow =
      case (editImpact, editScope, editSeverity, editRisk, editConfidence, editFootprint, editComplexity) of
        (Just impactVal, Just scopeVal, Just severityVal, Just riskVal, Just confidenceVal, Just footprintVal, Just complexityVal) ->
          buildNumber
            "Score"
            scoreNow
            (Just (Project.projectItemScoreEstimate impactVal scopeVal severityVal riskVal confidenceVal footprintVal complexityVal))
        _ -> Right Nothing


-- * Misc Helpers


data Update a
  = UpdateKeep
  | UpdateClear
  | UpdateSet !a
  deriving (Show, Eq)


hasUpdates :: EditOptions -> Bool
hasUpdates MkEditOptions {..} =
  or
    [ editOptTitle /= UpdateKeep
    , editOptBody /= UpdateKeep
    , editOptAssignees /= UpdateKeep
    , editOptStatus /= UpdateKeep
    , editOptIteration /= UpdateKeep
    , editOptDeadline /= UpdateKeep
    , editOptImpact /= UpdateKeep
    , editOptScope /= UpdateKeep
    , editOptSeverity /= UpdateKeep
    , editOptRisk /= UpdateKeep
    , editOptFootprint /= UpdateKeep
    , editOptComplexity /= UpdateKeep
    , editOptConfidence /= UpdateKeep
    , editOptIssueType /= UpdateKeep
    ]


resolveBodyUpdate :: Update Item.Commons.BodySource -> IO (Update T.Text)
resolveBodyUpdate = \case
  UpdateKeep -> pure UpdateKeep
  UpdateClear -> pure UpdateClear
  UpdateSet src -> UpdateSet <$> Item.Commons.resolveBodySource src


updateFieldParser :: T.Text -> OA.ReadM a -> String -> OA.Parser (Update a)
updateFieldParser name reader helpText =
  asum
    [ UpdateSet <$> OA.option reader (OA.long (T.unpack name) <> OA.metavar (fmap toUpper (T.unpack name)) <> OA.help helpText)
    , UpdateClear <$ OA.flag' () (OA.long ("no-" <> T.unpack name) <> OA.help ("Clear " <> helpText))
    , pure UpdateKeep
    ]


applyMaybeUpdate :: Update a -> Maybe a -> Maybe a
applyMaybeUpdate update current =
  case update of
    UpdateKeep -> current
    UpdateClear -> Nothing
    UpdateSet value -> Just value


applyTextUpdate :: Update T.Text -> T.Text -> T.Text
applyTextUpdate update current =
  case update of
    UpdateSet value -> value
    _ -> current


applyListUpdate :: Update [a] -> [a] -> [a]
applyListUpdate update current =
  case update of
    UpdateKeep -> current
    UpdateClear -> []
    UpdateSet value -> value
