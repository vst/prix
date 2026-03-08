{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Prix.ProjectItemEdit (
  EditOptions (..),
  editOptionsParser,
  runEdit,
) where

import Control.Applicative (asum, some, (<|>))
import Control.Exception (IOException, try)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.Types as AesonTypes
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Char (toUpper)
import Data.Fixed (Milli)
import Data.List (find, (\\))
import qualified Data.List.NonEmpty as NE
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


data EditOptions = MkEditOptions
  { editOptInteractive :: !Bool
  , editOptItemId :: !(Maybe T.Text)
  , editOptTitle :: !(Update T.Text)
  , editOptBody :: !(Update BodySource)
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


data Update a
  = UpdateKeep
  | UpdateClear
  | UpdateSet !a
  deriving (Show, Eq)


data BodySource
  = BodySourceText !T.Text
  | BodySourceFile !FilePath
  deriving (Show, Eq)


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
    bodyP = UpdateSet <$> bodySourceParser <|> pure UpdateKeep
    assigneesP =
      asum
        [ UpdateClear <$ OA.flag' () (OA.long "no-assignees" <> OA.help "Clear assignees.")
        , UpdateSet <$> some (T.pack <$> OA.strOption (OA.long "assignee" <> OA.metavar "LOGIN" <> OA.help "Assignee login (repeatable)."))
        , pure UpdateKeep
        ]
    statusP = updateFieldParser "status" (parseEnumOption Project.projectItemStatusLabel) "Item status."
    iterationP = updateFieldParser "iteration" OA.auto "Iteration number."
    urgencyP = updateFieldParser "urgency" (parseEnumOption Project.projectItemUrgencyLabel) "Urgency level."
    impactP = updateFieldParser "impact" (parseEnumOption Project.projectItemImpactLabel) "Impact level."
    reachP = updateFieldParser "reach" (parseEnumOption Project.projectItemReachLabel) "Reach level."
    sizeP = updateFieldParser "size" (parseEnumOption Project.projectItemSizeLabel) "Size level."
    difficultyP = updateFieldParser "difficulty" (parseEnumOption Project.projectItemDifficultyLabel) "Difficulty level."
    confidenceP = updateFieldParser "confidence" (parseEnumOption Project.projectItemConfidenceLabel) "Confidence level."
    themeP = updateFieldParser "theme" (parseEnumOption Project.projectItemThemeLabel) "Strategic theme."


runEdit :: Config -> EditOptions -> IO ExitCode
runEdit _cfg opts = do
  isTty <- hIsTerminalDevice stdin
  when (editOptInteractive opts && not isTty) $
    die "Interactive mode requires a TTY."
  itemId <-
    case editOptItemId opts of
      Just pid -> pure pid
      Nothing ->
        if isTty
          then HL.runInputT HL.defaultSettings promptItemId
          else die "Missing required --id."
  (project, item) <- loadProjectItem itemId
  projectConfig <- loadProjectConfig project
  bodyUpdate <- resolveBodyUpdate (editOptBody opts)
  let current = currentFromItem item
      defaults = defaultsFromUpdates opts bodyUpdate current
      needsPrompt = isTty && (not (hasUpdates opts) || editOptInteractive opts)
  inputs <-
    if needsPrompt
      then HL.runInputT HL.defaultSettings $ promptInputs projectConfig defaults
      else pure (toInputs defaults)
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
  when (titleChanged || bodyChanged) $
    updateContent content editTitle editBody
  when assigneesChanged $
    updateAssignees content (currentAssignees current) editAssignees
  updates <- case buildFieldUpdates cfg current MkEditInputs {..} of
    Left err -> die err
    Right xs -> pure xs
  if null updates && not titleChanged && not bodyChanged && not assigneesChanged
    then putStrLn "Nothing to update."
    else forM_ updates $ \FieldUpdate {..} -> updateProjectField cfg itemId fieldUpdateId fieldUpdateValue


-- * Prompting


promptItemId :: HL.InputT IO T.Text
promptItemId = do
  mLine <- HL.getInputLine "Project item ID: "
  case mLine of
    Nothing -> promptItemId
    Just "" -> promptItemId
    Just s -> pure (T.pack s)


promptInputs :: ProjectConfig.ProjectConfig -> EditDefaults -> HL.InputT IO EditInputs
promptInputs cfg defaults = do
  let (mTitle, mBody, assignees, status, iteration, urgency, impact, reach, size, difficulty, confidence, theme) = defaults
  title <- promptText "Title" (Just (fromMaybe "" mTitle))
  body <- promptOptionalText "Body" mBody
  assignees' <- promptAssignees assignees
  status' <- promptSelectOptional "Status" (statusOptions cfg) status
  iteration' <- promptIteration cfg iteration
  urgency' <- promptSelectOptional "Urgency" (urgencyOptions cfg) urgency
  impact' <- promptSelectOptional "Impact" (impactOptions cfg) impact
  reach' <- promptSelectOptional "Reach" (reachOptions cfg) reach
  size' <- promptSelectOptional "Size" (sizeOptions cfg) size
  difficulty' <- promptSelectOptional "Difficulty" (difficultyOptions cfg) difficulty
  confidence' <- promptSelectOptional "Confidence" (confidenceOptions cfg) confidence
  theme' <- promptSelectOptional "Theme" (themeOptions cfg) theme
  pure
    MkEditInputs
      { editTitle = fromMaybe "" title
      , editBody = body
      , editAssignees = assignees'
      , editStatus = status'
      , editIteration = iteration'
      , editUrgency = urgency'
      , editImpact = impact'
      , editReach = reach'
      , editSize = size'
      , editDifficulty = difficulty'
      , editConfidence = confidence'
      , editTheme = theme'
      }


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
      | T.toLower (T.pack s) == "e" -> liftIO (editBodyWithEditor def)
      | otherwise -> pure (Just (T.pack s))


editBodyWithEditor :: Maybe T.Text -> IO (Maybe T.Text)
editBodyWithEditor def = do
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
        Just val -> pure (Just val)


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
      case readMaybeInteger s of
        Nothing -> HL.outputStrLn "Please enter a number." >> promptIteration cfg def
        Just val -> pure (Just val)


-- * Project Config + Items


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


loadProjectConfig :: Project.Project -> IO ProjectConfig.ProjectConfig
loadProjectConfig project = do
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
    Right configs ->
      case find matches configs of
        Nothing -> die "Matching project config not found. Run `prix project sync` to refresh config."
        Just cfg -> pure cfg
  where
    matches cfg =
      ProjectConfig.projectConfigNumber cfg == fromIntegral (Project.projectMetaNumber (Project.projectMeta project))
        && Commons.ownerLogin (ProjectConfig.projectConfigOwner cfg) == Commons.ownerLogin (Project.projectOwner project)


-- * GraphQL


data FieldUpdate = FieldUpdate
  { fieldUpdateId :: !T.Text
  , fieldUpdateValue :: !Aeson.Value
  }


updateContent :: Project.ProjectItemContent -> T.Text -> Maybe T.Text -> IO ()
updateContent content title body =
  case content of
    Project.ProjectItemContentDraftIssue draft ->
      updateDraftIssue (Project.draftIssueContentId draft) title body
    Project.ProjectItemContentIssue issue ->
      updateIssue (Project.issueContentId issue) title body
    Project.ProjectItemContentPullRequest pr ->
      updatePullRequest (Project.pullRequestContentId pr) title body


updateDraftIssue :: T.Text -> T.Text -> Maybe T.Text -> IO ()
updateDraftIssue draftId title body = do
  let query =
        [i|mutation($draftId:ID!, $title:String!, $body:String) {
  updateProjectV2DraftIssue(input: {draftIssueId:$draftId, title:$title, body:$body}) {
    projectItem { id }
  }
}|]
      vars =
        [ ("draftId", Aeson.String draftId)
        , ("title", Aeson.String title)
        , ("body", maybe Aeson.Null Aeson.String body)
        ]
  res <- runGhGraphqlValue query vars
  case res of
    Left err -> die err
    Right _ -> pure ()


updateIssue :: T.Text -> T.Text -> Maybe T.Text -> IO ()
updateIssue issueId title body = do
  let query =
        [i|mutation($issueId:ID!, $title:String!, $body:String) {
  updateIssue(input: {id:$issueId, title:$title, body:$body}) {
    issue { id }
  }
}|]
      vars =
        [ ("issueId", Aeson.String issueId)
        , ("title", Aeson.String title)
        , ("body", maybe Aeson.Null Aeson.String body)
        ]
  res <- runGhGraphqlValue query vars
  case res of
    Left err -> die err
    Right _ -> pure ()


updatePullRequest :: T.Text -> T.Text -> Maybe T.Text -> IO ()
updatePullRequest prId title body = do
  let query =
        [i|mutation($pullRequestId:ID!, $title:String!, $body:String) {
  updatePullRequest(input: {pullRequestId:$pullRequestId, title:$title, body:$body}) {
    pullRequest { id }
  }
}|]
      vars =
        [ ("pullRequestId", Aeson.String prId)
        , ("title", Aeson.String title)
        , ("body", maybe Aeson.Null Aeson.String body)
        ]
  res <- runGhGraphqlValue query vars
  case res of
    Left err -> die err
    Right _ -> pure ()


updateAssignees :: Project.ProjectItemContent -> [T.Text] -> [T.Text] -> IO ()
updateAssignees content current desired =
  case content of
    Project.ProjectItemContentDraftIssue _ ->
      die "Assignees are not supported for draft issues."
    Project.ProjectItemContentIssue issue ->
      updateAssignableAssignees (Project.issueContentId issue) current desired
    Project.ProjectItemContentPullRequest pr ->
      updateAssignableAssignees (Project.pullRequestContentId pr) current desired


updateAssignableAssignees :: T.Text -> [T.Text] -> [T.Text] -> IO ()
updateAssignableAssignees assignableId current desired = do
  currentIds <- resolveAssigneeIds current
  desiredIds <- resolveAssigneeIds desired
  let toAdd = desiredIds \\ currentIds
      toRemove = currentIds \\ desiredIds
  unless (null toAdd) $ addAssigneesToAssignable assignableId toAdd
  unless (null toRemove) $ removeAssigneesFromAssignable assignableId toRemove


addAssigneesToAssignable :: T.Text -> [T.Text] -> IO ()
addAssigneesToAssignable _ [] = pure ()
addAssigneesToAssignable assignableId assigneeIds = do
  let query =
        [i|mutation($assignableId:ID!, $assigneeIds:[ID!]!) {
  addAssigneesToAssignable(input: {assignableId:$assignableId, assigneeIds:$assigneeIds}) {
    clientMutationId
  }
}|]
      vars =
        [ ("assignableId", Aeson.String assignableId)
        , ("assigneeIds", Aeson.toJSON assigneeIds)
        ]
  res <- runGhGraphqlValue query vars
  case res of
    Left err -> die err
    Right _ -> pure ()


removeAssigneesFromAssignable :: T.Text -> [T.Text] -> IO ()
removeAssigneesFromAssignable _ [] = pure ()
removeAssigneesFromAssignable assignableId assigneeIds = do
  let query =
        [i|mutation($assignableId:ID!, $assigneeIds:[ID!]!) {
  removeAssigneesFromAssignable(input: {assignableId:$assignableId, assigneeIds:$assigneeIds}) {
    clientMutationId
  }
}|]
      vars =
        [ ("assignableId", Aeson.String assignableId)
        , ("assigneeIds", Aeson.toJSON assigneeIds)
        ]
  res <- runGhGraphqlValue query vars
  case res of
    Left err -> die err
    Right _ -> pure ()


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


buildFieldUpdates :: ProjectConfig.ProjectConfig -> CurrentItem -> EditInputs -> Either String [FieldUpdate]
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
    buildSingleSelect fieldLabel labelFn currentVal desiredVal
      | currentVal == desiredVal = Right Nothing
      | otherwise = do
          field <- requireSingleSelect cfg fieldLabel
          case desiredVal of
            Nothing ->
              pure . Just $
                FieldUpdate
                  (ProjectConfig.projectConfigFieldSingleSelectId field)
                  (Aeson.object ["singleSelectOptionId" Aeson..= Aeson.Null])
            Just value -> do
              optionId <- selectOptionId fieldLabel (labelFn value) (ProjectConfig.projectConfigFieldSingleSelectOptions field)
              pure . Just $
                FieldUpdate
                  (ProjectConfig.projectConfigFieldSingleSelectId field)
                  (Aeson.object ["singleSelectOptionId" Aeson..= optionId])
    buildIteration fieldLabel currentVal desiredVal
      | currentVal == desiredVal = Right Nothing
      | otherwise = do
          field <- requireIteration cfg fieldLabel
          case desiredVal of
            Nothing ->
              pure . Just $
                FieldUpdate
                  (ProjectConfig.projectConfigFieldIterationId field)
                  (Aeson.object ["iterationId" Aeson..= Aeson.Null])
            Just value -> do
              iterId <- selectIterationId fieldLabel value (ProjectConfig.projectConfigFieldIterationConfiguration field)
              pure . Just $
                FieldUpdate
                  (ProjectConfig.projectConfigFieldIterationId field)
                  (Aeson.object ["iterationId" Aeson..= iterId])
    buildNumber fieldLabel currentVal desiredVal
      | currentVal == desiredVal = Right Nothing
      | otherwise = do
          field <- requireCommon cfg fieldLabel
          ensureDataType fieldLabel "NUMBER" (ProjectConfig.projectConfigFieldCommonDataType field)
          pure . Just $
            FieldUpdate
              (ProjectConfig.projectConfigFieldCommonId field)
              (Aeson.object ["number" Aeson..= maybe Aeson.Null (Aeson.toJSON :: Milli -> Aeson.Value) desiredVal])


-- * Project Config Accessors


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
  | expected == actual = Right ()
  | otherwise = Left ("Field " <> T.unpack fieldLabel <> " has wrong data type. Expected " <> T.unpack expected <> ".")


findFieldByName :: T.Text -> [ProjectConfig.ProjectConfigField] -> Maybe ProjectConfig.ProjectConfigField
findFieldByName name =
  find $ \field -> T.toLower (fieldName field) == T.toLower name
  where
    fieldName = \case
      ProjectConfig.ProjectConfigFieldCommon field -> ProjectConfig.projectConfigFieldCommonName field
      ProjectConfig.ProjectConfigFieldSingleSelect field -> ProjectConfig.projectConfigFieldSingleSelectName field
      ProjectConfig.ProjectConfigFieldIteration field -> ProjectConfig.projectConfigFieldIterationName field


selectOptionId :: T.Text -> T.Text -> [ProjectConfig.ProjectConfigSingleSelectOption] -> Either String T.Text
selectOptionId fieldName optionName options =
  case find matches options of
    Nothing -> Left ("Unknown " <> T.unpack fieldName <> " value: " <> T.unpack optionName)
    Just opt -> Right (ProjectConfig.projectConfigSingleSelectOptionId opt)
  where
    matches opt = T.toLower (ProjectConfig.projectConfigSingleSelectOptionName opt) == T.toLower optionName


selectIterationId :: T.Text -> Integer -> ProjectConfig.ProjectConfigIterationConfiguration -> Either String T.Text
selectIterationId fieldName number config =
  case find matches (ProjectConfig.projectConfigIterationConfigurationIterations config) of
    Nothing -> Left ("Unknown " <> T.unpack fieldName <> " value: " <> show number)
    Just iter -> Right (ProjectConfig.projectConfigIterationId iter)
  where
    matches iter =
      case iterationNumberFromTitle (ProjectConfig.projectConfigIterationTitle iter) of
        Nothing -> False
        Just n -> n == number


projectConfigIterations :: ProjectConfig.ProjectConfig -> [(Integer, T.Text)]
projectConfigIterations cfg =
  let iterations =
        concatMap
          ( \case
              ProjectConfig.ProjectConfigFieldIteration field ->
                ProjectConfig.projectConfigIterationConfigurationIterations
                  (ProjectConfig.projectConfigFieldIterationConfiguration field)
              _ -> []
          )
          (ProjectConfig.projectConfigFields cfg)
   in mapMaybe
        ( \iter -> do
            num <- iterationNumberFromTitle (ProjectConfig.projectConfigIterationTitle iter)
            pure (num, ProjectConfig.projectConfigIterationTitle iter)
        )
        iterations


iterationNumberFromTitle :: T.Text -> Maybe Integer
iterationNumberFromTitle title = do
  numText <- T.stripPrefix "Iteration " title
  readMaybeInteger (T.unpack numText)


-- * GraphQL Helpers


runGhGraphqlValue :: T.Text -> [(T.Text, Aeson.Value)] -> IO (Either String Aeson.Value)
runGhGraphqlValue query vars = do
  let payload =
        Aeson.object
          [ "query" Aeson..= query
          , "variables" Aeson..= Aeson.object (fmap (first AesonKey.fromText) vars)
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


extractUserId :: Aeson.Value -> Either String T.Text
extractUserId =
  AesonTypes.parseEither . Aeson.withObject "response" $ \obj -> do
    dat <- obj Aeson..: "data"
    usr <- dat Aeson..: "user"
    usr Aeson..: "id"


-- * Misc Helpers


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


resolveBodyUpdate :: Update BodySource -> IO (Update T.Text)
resolveBodyUpdate = \case
  UpdateKeep -> pure UpdateKeep
  UpdateClear -> pure UpdateClear
  UpdateSet src -> UpdateSet <$> resolveBodySource src


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


selectByInput :: T.Text -> [(T.Text, a)] -> Maybe a
selectByInput input options =
  case readMaybeInt (T.unpack input) of
    Just idx
      | idx > 0 && idx <= length options -> Just (snd (options !! (idx - 1)))
    _ -> lookupExact
  where
    lookupExact = lookup (T.toLower input) lowered
    lowered = fmap (first T.toLower) options


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


enumOptions :: (Bounded a, Enum a) => (a -> T.Text) -> [(T.Text, a)]
enumOptions labelFn =
  fmap (\a -> (labelFn a, a)) [minBound .. maxBound]


parseEnumOption :: (Bounded a, Enum a) => (a -> T.Text) -> OA.ReadM a
parseEnumOption labelFn =
  OA.eitherReader $ \s ->
    let val = T.toLower (T.pack s)
        options = [minBound .. maxBound]
        matches a = T.toLower (labelFn a) == val
     in case find matches options of
          Nothing -> Left "Invalid option."
          Just a -> Right a


splitComma :: T.Text -> [T.Text]
splitComma =
  fmap T.strip . T.splitOn "," . T.filter (/= '\n')


readMaybeInt :: String -> Maybe Int
readMaybeInt s =
  case reads s of
    [(n, "")] -> Just n
    _ -> Nothing


readMaybeInteger :: String -> Maybe Integer
readMaybeInteger s =
  case reads s of
    [(n, "")] -> Just n
    _ -> Nothing
