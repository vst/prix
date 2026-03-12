{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , editOptUrgency :: !(Update Project.ProjectItemUrgency)
  , editOptImpact :: !(Update Project.ProjectItemImpact)
  , editOptReach :: !(Update Project.ProjectItemReach)
  , editOptSize :: !(Update Project.ProjectItemSize)
  , editOptDifficulty :: !(Update Project.ProjectItemDifficulty)
  , editOptConfidence :: !(Update Project.ProjectItemConfidence)
  , editOptTheme :: !(Update Project.ProjectItemTheme)
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
    <*> urgencyP
    <*> impactP
    <*> reachP
    <*> sizeP
    <*> difficultyP
    <*> confidenceP
    <*> themeP
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
    urgencyP = updateFieldParser "urgency" (Item.Commons.parseEnumOption Project.projectItemUrgencyLabel) "Urgency level."
    impactP = updateFieldParser "impact" (Item.Commons.parseEnumOption Project.projectItemImpactLabel) "Impact level."
    reachP = updateFieldParser "reach" (Item.Commons.parseEnumOption Project.projectItemReachLabel) "Reach level."
    sizeP = updateFieldParser "size" (Item.Commons.parseEnumOption Project.projectItemSizeLabel) "Size level."
    difficultyP = updateFieldParser "difficulty" (Item.Commons.parseEnumOption Project.projectItemDifficultyLabel) "Difficulty level."
    confidenceP = updateFieldParser "confidence" (Item.Commons.parseEnumOption Project.projectItemConfidenceLabel) "Confidence level."
    themeP = updateFieldParser "theme" (Item.Commons.parseEnumOption Project.projectItemThemeLabel) "Strategic theme."


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
  inputs <- if needsPrompt then promptInputs projectConfig defaults else pure (toInputs defaults)
  applyEdits projectConfig item inputs current
  pure ExitSuccess


data CurrentItem = MkCurrentItem
  { currentTitle :: !T.Text
  , currentBody :: !(Maybe T.Text)
  , currentAssignees :: ![T.Text]
  , currentStatus :: !(Maybe Project.ProjectItemStatus)
  , currentIteration :: !(Maybe Integer)
  , currentUrgency :: !(Maybe Project.ProjectItemUrgency)
  , currentImpact :: !(Maybe Project.ProjectItemImpact)
  , currentReach :: !(Maybe Project.ProjectItemReach)
  , currentSize :: !(Maybe Project.ProjectItemSize)
  , currentDifficulty :: !(Maybe Project.ProjectItemDifficulty)
  , currentConfidence :: !(Maybe Project.ProjectItemConfidence)
  , currentTheme :: !(Maybe Project.ProjectItemTheme)
  , currentScore :: !(Maybe Milli)
  }


data EditInputs = MkEditInputs
  { editTitle :: !T.Text
  , editBody :: !(Maybe T.Text)
  , editAssignees :: ![T.Text]
  , editStatus :: !(Maybe Project.ProjectItemStatus)
  , editIteration :: !(Maybe Integer)
  , editUrgency :: !(Maybe Project.ProjectItemUrgency)
  , editImpact :: !(Maybe Project.ProjectItemImpact)
  , editReach :: !(Maybe Project.ProjectItemReach)
  , editSize :: !(Maybe Project.ProjectItemSize)
  , editDifficulty :: !(Maybe Project.ProjectItemDifficulty)
  , editConfidence :: !(Maybe Project.ProjectItemConfidence)
  , editTheme :: !(Maybe Project.ProjectItemTheme)
  }


currentFromItem :: Project.ProjectItem -> CurrentItem
currentFromItem Project.MkProjectItem {..} =
  MkCurrentItem
    { currentTitle = projectItemTitle
    , currentBody = projectItemBody
    , currentAssignees = foldMap NE.toList projectItemAssignees
    , currentStatus = projectItemStatus
    , currentIteration = projectItemIteration
    , currentUrgency = projectItemUrgency
    , currentImpact = projectItemImpact
    , currentReach = projectItemReach
    , currentSize = projectItemSize
    , currentDifficulty = projectItemDifficulty
    , currentConfidence = projectItemConfidence
    , currentTheme = projectItemTheme
    , currentScore = projectItemScore
    }


type EditDefaults =
  ( Maybe T.Text
  , Maybe T.Text
  , [T.Text]
  , Maybe Project.ProjectItemStatus
  , Maybe Integer
  , Maybe Project.ProjectItemUrgency
  , Maybe Project.ProjectItemImpact
  , Maybe Project.ProjectItemReach
  , Maybe Project.ProjectItemSize
  , Maybe Project.ProjectItemDifficulty
  , Maybe Project.ProjectItemConfidence
  , Maybe Project.ProjectItemTheme
  )


defaultsFromUpdates :: EditOptions -> Update T.Text -> CurrentItem -> EditDefaults
defaultsFromUpdates MkEditOptions {..} bodyUpdate current =
  ( Just (applyTextUpdate editOptTitle (currentTitle current))
  , applyMaybeUpdate bodyUpdate (currentBody current)
  , applyListUpdate editOptAssignees (currentAssignees current)
  , applyMaybeUpdate editOptStatus (currentStatus current)
  , applyMaybeUpdate editOptIteration (currentIteration current)
  , applyMaybeUpdate editOptUrgency (currentUrgency current)
  , applyMaybeUpdate editOptImpact (currentImpact current)
  , applyMaybeUpdate editOptReach (currentReach current)
  , applyMaybeUpdate editOptSize (currentSize current)
  , applyMaybeUpdate editOptDifficulty (currentDifficulty current)
  , applyMaybeUpdate editOptConfidence (currentConfidence current)
  , applyMaybeUpdate editOptTheme (currentTheme current)
  )


toInputs :: EditDefaults -> EditInputs
toInputs (mTitle, mBody, assignees, status, iteration, urgency, impact, reach, size, difficulty, confidence, theme) =
  MkEditInputs
    { editTitle = fromMaybe "" mTitle
    , editBody = mBody
    , editAssignees = assignees
    , editStatus = status
    , editIteration = iteration
    , editUrgency = urgency
    , editImpact = impact
    , editReach = reach
    , editSize = size
    , editDifficulty = difficulty
    , editConfidence = confidence
    , editTheme = theme
    }


applyEdits :: ProjectConfig.ProjectConfig -> Project.ProjectItem -> EditInputs -> CurrentItem -> IO ()
applyEdits cfg item MkEditInputs {..} current = do
  let itemId = Project.projectItemId item
      content = Project.projectItemContent item
      titleChanged = editTitle /= currentTitle current
      bodyChanged = editBody /= currentBody current
      assigneesChanged = editAssignees /= currentAssignees current
  when (titleChanged || bodyChanged) $ updateContent content editTitle editBody
  when assigneesChanged $ updateAssignees content (currentAssignees current) editAssignees
  updates <- either die pure $ buildFieldUpdates cfg current MkEditInputs {..}
  if null updates && not titleChanged && not bodyChanged && not assigneesChanged
    then putStrLn "Nothing to update."
    else forM_ updates $ \Item.Commons.FieldUpdate {..} -> Item.Commons.updateProjectField cfg itemId fieldUpdateId fieldUpdateValue


-- * Prompting


promptItemId :: IO T.Text
promptItemId = do
  mLine <- Z.Term.Prompts.text "Project item ID" Nothing
  case mLine of
    Just txt | not (T.null txt) -> pure txt
    _ -> promptItemId


promptInputs :: ProjectConfig.ProjectConfig -> EditDefaults -> IO EditInputs
promptInputs cfg defaults = do
  let (mTitle, mBody, assignees, status, iteration, urgency, impact, reach, size, difficulty, confidence, theme) = defaults
  let titleDefault = fromMaybe "" mTitle
  editTitle <- fromMaybe titleDefault <$> Z.Term.Prompts.text "Title" (Just titleDefault)
  editBody <- Z.Term.Prompts.multilineText "Body" mBody
  editAssignees <- Item.Commons.promptAssignees assignees
  editStatus <- Item.Commons.promptSelectOptional "Status" Project.projectItemStatusLabel status Z.Base.enumerate
  editIteration <- Item.Commons.promptSelectOptional "Iteration" Z.Text.tshow iteration (fmap fst (Item.Commons.projectConfigIterations cfg))
  editUrgency <- Item.Commons.promptSelectOptional "Urgency" Project.projectItemUrgencyLabel urgency Z.Base.enumerate
  editImpact <- Item.Commons.promptSelectOptional "Impact" Project.projectItemImpactLabel impact Z.Base.enumerate
  editReach <- Item.Commons.promptSelectOptional "Reach" Project.projectItemReachLabel reach Z.Base.enumerate
  editSize <- Item.Commons.promptSelectOptional "Size" Project.projectItemSizeLabel size Z.Base.enumerate
  editDifficulty <- Item.Commons.promptSelectOptional "Difficulty" Project.projectItemDifficultyLabel difficulty Z.Base.enumerate
  editConfidence <- Item.Commons.promptSelectOptional "Confidence" Project.projectItemConfidenceLabel confidence Z.Base.enumerate
  editTheme <- Item.Commons.promptSelectOptional "Theme" Project.projectItemThemeLabel theme Z.Base.enumerate
  pure MkEditInputs {..}


-- * GitHub IO


updateContent :: Project.ProjectItemContent -> T.Text -> Maybe T.Text -> IO ()
updateContent content title body =
  case content of
    Project.ProjectItemContentDraftIssue draft -> updateDraftIssue (Project.draftIssueContentId draft) title body
    Project.ProjectItemContentIssue issue -> updateIssue (Project.issueContentId issue) title body
    Project.ProjectItemContentPullRequest pr -> updatePullRequest (Project.pullRequestContentId pr) title body


updateDraftIssue :: T.Text -> T.Text -> Maybe T.Text -> IO ()
updateDraftIssue draftId title body =
  Commons.ghUpdateDraftIssue draftId title body >>= either die Z.Control.discard


updateIssue :: T.Text -> T.Text -> Maybe T.Text -> IO ()
updateIssue issueId title body =
  Commons.ghUpdateIssue issueId title body >>= either die Z.Control.discard


updatePullRequest :: T.Text -> T.Text -> Maybe T.Text -> IO ()
updatePullRequest prId title body =
  Commons.ghUpdatePullRequest prId title body >>= either die Z.Control.discard


updateAssignees :: Project.ProjectItemContent -> [T.Text] -> [T.Text] -> IO ()
updateAssignees content current desired =
  case content of
    Project.ProjectItemContentDraftIssue _ -> die "Assignees are not supported for draft issues."
    Project.ProjectItemContentIssue issue -> updateAssignableAssignees (Project.issueContentId issue) current desired
    Project.ProjectItemContentPullRequest pr -> updateAssignableAssignees (Project.pullRequestContentId pr) current desired


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
  urgency <- buildSingleSelect "Urgency" Project.projectItemUrgencyLabel currentUrgency editUrgency
  impact <- buildSingleSelect "Impact" Project.projectItemImpactLabel currentImpact editImpact
  reach <- buildSingleSelect "Reach" Project.projectItemReachLabel currentReach editReach
  size <- buildSingleSelect "Size" Project.projectItemSizeLabel currentSize editSize
  difficulty <- buildSingleSelect "Difficulty" Project.projectItemDifficultyLabel currentDifficulty editDifficulty
  confidence <- buildSingleSelect "Confidence" Project.projectItemConfidenceLabel currentConfidence editConfidence
  theme <- buildSingleSelect "Theme" Project.projectItemThemeLabel currentTheme editTheme
  iteration <- buildIteration "Iteration" currentIteration editIteration
  let buildScoreUpdate scoreNow =
        case (editUrgency, editReach, editImpact, editConfidence, editSize, editDifficulty) of
          (Just u, Just r, Just impactVal, Just c, Just s, Just d) ->
            buildNumber "Score" scoreNow (Just (Project.projectItemPriority u r impactVal c s d))
          _ -> Right Nothing
  score <- buildScoreUpdate currentScore
  pure $ catMaybes [status, iteration, urgency, impact, reach, size, difficulty, confidence, theme, score]
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
    buildNumber fieldLabel currentVal desiredVal
      | currentVal == desiredVal = Right Nothing
      | otherwise = do
          field <- Item.Commons.requireCommon cfg fieldLabel
          Item.Commons.ensureDataType fieldLabel "NUMBER" (ProjectConfig.projectConfigFieldCommonDataType field)
          pure . Just $
            Item.Commons.FieldUpdate
              (ProjectConfig.projectConfigFieldCommonId field)
              (Aeson.object ["number" Aeson..= maybe Aeson.Null (Aeson.toJSON :: Milli -> Aeson.Value) desiredVal])


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
    , editOptUrgency /= UpdateKeep
    , editOptImpact /= UpdateKeep
    , editOptReach /= UpdateKeep
    , editOptSize /= UpdateKeep
    , editOptDifficulty /= UpdateKeep
    , editOptConfidence /= UpdateKeep
    , editOptTheme /= UpdateKeep
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
