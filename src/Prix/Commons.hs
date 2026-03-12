{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Prix.Commons where

import qualified Autodocodec as ADC
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.FileEmbed (embedStringFile)
import Data.String.Interpolate (i)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Exit (ExitCode (..), die)
import System.IO (hPutStrLn, stderr)
import qualified System.Process.Typed as TP


-- * Owner


data Owner = MkOwner
  { ownerType :: !OwnerType
  , ownerLogin :: !T.Text
  , ownerUrl :: !T.Text
  , ownerAvatarUrl :: !(Maybe T.Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Owner)


instance ADC.HasCodec Owner where
  codec =
    ADC.object "Owner" $
      MkOwner
        <$> ADC.requiredField "type" "Owner Type" ADC..= ownerType
        <*> ADC.requiredField "login" "Owner Login" ADC..= ownerLogin
        <*> ADC.requiredField "url" "Owner URL" ADC..= ownerUrl
        <*> ADC.requiredField "avatarUrl" "Owner Avatar URL" ADC..= ownerAvatarUrl


data OwnerType
  = OwnerTypeUser
  | OwnerTypeOrganization
  deriving (Show, Eq, Generic, Enum, Bounded)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via ADC.Autodocodec OwnerType


instance ADC.HasCodec OwnerType where
  codec = ADC.boundedEnumCodec ownerTypeLabel


ownerTypeLabel :: OwnerType -> T.Text
ownerTypeLabel OwnerTypeUser = "USER"
ownerTypeLabel OwnerTypeOrganization = "ORGANIZATION"


-- * Colors


-- | Colors for different options.
--
-- These colors are defined in the GitHub GraphQL API. We are just sticking to
-- them.
data OptionColor
  = OptionColorGray
  | OptionColorBlue
  | OptionColorGreen
  | OptionColorYellow
  | OptionColorOrange
  | OptionColorRed
  | OptionColorPink
  | OptionColorPurple
  deriving (Eq, Show, Ord, Bounded, Enum, Generic)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via ADC.Autodocodec OptionColor


instance ADC.HasCodec OptionColor where
  codec = ADC.boundedEnumCodec optionColorLabel


optionColorLabel :: OptionColor -> T.Text
optionColorLabel OptionColorGray = "GRAY"
optionColorLabel OptionColorBlue = "BLUE"
optionColorLabel OptionColorGreen = "GREEN"
optionColorLabel OptionColorYellow = "YELLOW"
optionColorLabel OptionColorOrange = "ORANGE"
optionColorLabel OptionColorRed = "RED"
optionColorLabel OptionColorPink = "PINK"
optionColorLabel OptionColorPurple = "PURPLE"


-- | Emoji symbol for the given option color.
--
-- >>> fmap optionColorEmoji [OptionColorGray .. OptionColorPurple]
-- ["\11036","\128998","\129001","\129000","\128999","\128997","\129003","\129002"]
optionColorEmoji :: OptionColor -> T.Text
optionColorEmoji OptionColorGray = "⬜" -- White large square as 'gray'
optionColorEmoji OptionColorBlue = "🟦"
optionColorEmoji OptionColorGreen = "🟩"
optionColorEmoji OptionColorYellow = "🟨"
optionColorEmoji OptionColorOrange = "🟧"
optionColorEmoji OptionColorRed = "🟥"
optionColorEmoji OptionColorPink = "🟫" -- No pink square, hence the brown square
optionColorEmoji OptionColorPurple = "🟪"


-- * Org Issue Types


data OrgIssueType = MkOrgIssueType
  { orgIssueTypeId :: !T.Text
  , orgIssueTypeName :: !T.Text
  }
  deriving (Show, Eq)


instance Aeson.FromJSON OrgIssueType where
  parseJSON = Aeson.withObject "OrgIssueType" $ \o ->
    MkOrgIssueType
      <$> o Aeson..: "id"
      <*> o Aeson..: "name"


-- * GH helpers


ghGetRateLimitRemaining :: IO Integer
ghGetRateLimitRemaining = do
  res <- runProcessRead "gh" ["api", "rate_limit", "--jq", ".rate.remaining"]
  case res of
    Left err -> printProcessResultError "gh-api-rate-limit" err >> die "Exiting..."
    Right sv -> pure sv


-- | Fetch the issue types defined for a GitHub organization.
-- Returns an empty list if the login is not an organization or has no issue types.
ghGetOrgIssueTypes :: T.Text -> IO [OrgIssueType]
ghGetOrgIssueTypes org = do
  let query = $(embedStringFile "./src/Prix/extras/lookup_org_issue_types.gql") :: T.Text
  res <-
    runProcessBLC
      "gh"
      [ "api"
      , "graphql"
      , "--field"
      , [i|query=#{query}|]
      , "--field"
      , [i|org=#{org}|]
      , "--jq"
      , ".data.organization.issueTypes.nodes // []"
      ]
  case res of
    Left _ -> pure []
    Right sv -> case Aeson.eitherDecode sv of
      Left _ -> pure []
      Right types -> pure types


ghLookupUserId :: T.Text -> IO (Either String T.Text)
ghLookupUserId login = do
  let query = $(embedStringFile "./src/Prix/extras/lookup_user_id.gql") :: T.Text
  res <-
    runProcessBLC
      "gh"
      [ "api"
      , "graphql"
      , "--field"
      , [i|query=#{query}|]
      , "--field"
      , [i|login=#{login}|]
      , "--jq"
      , ".data.user.id"
      ]
  pure $ case res of
    Left _er -> Left [i|Failed to resolve user: #{login}|]
    Right sv -> Right (T.strip (T.pack (BLC.unpack sv)))


ghLookupRepoId :: T.Text -> T.Text -> IO (Either String T.Text)
ghLookupRepoId owner name = do
  let query = $(embedStringFile "./src/Prix/extras/lookup_repo_id.gql") :: T.Text
  res <-
    runProcessBLC
      "gh"
      [ "api"
      , "graphql"
      , "--field"
      , [i|query=#{query}|]
      , "--field"
      , [i|owner=#{owner}|]
      , "--field"
      , [i|name=#{name}|]
      , "--jq"
      , ".data.repository.id"
      ]
  pure $ case res of
    Left _er -> Left [i|Failed to resolve repo: #{owner}/#{name}|]
    Right sv -> Right (T.strip (T.pack (BLC.unpack sv)))


ghCreateIssue :: T.Text -> T.Text -> T.Text -> [T.Text] -> Maybe T.Text -> IO (Either String T.Text)
ghCreateIssue repoId title body assigneeIds mIssueTypeId = do
  let query = $(embedStringFile "./src/Prix/extras/create_issue.gql") :: T.Text
  let input =
        Aeson.object
          [ "query" Aeson..= query
          , "variables"
              Aeson..= Aeson.object
                [ "repoId" Aeson..= repoId
                , "title" Aeson..= title
                , "body" Aeson..= body
                , "assigneeIds" Aeson..= assigneeIds
                , "issueTypeId" Aeson..= mIssueTypeId
                ]
          ]
  res <-
    runProcessStdinBLC
      "gh"
      [ "api"
      , "graphql"
      , "--input"
      , "-"
      , "--jq"
      , ".data.createIssue.issue.id"
      ]
      (Aeson.encode input)
  pure $ case res of
    Left _er -> Left [i|Failed to create issue in repo #{repoId}|]
    Right sv -> Right (T.strip (T.pack (BLC.unpack sv)))


ghAddContentToProject :: T.Text -> T.Text -> IO (Either String T.Text)
ghAddContentToProject projectId contentId = do
  let query = $(embedStringFile "./src/Prix/extras/add_content_to_project.gql") :: T.Text
  res <-
    runProcessBLC
      "gh"
      [ "api"
      , "graphql"
      , "--field"
      , [i|query=#{query}|]
      , "--field"
      , [i|projectId=#{projectId}|]
      , "--field"
      , [i|contentId=#{contentId}|]
      , "--jq"
      , ".data.addProjectV2ItemById.item.id"
      ]
  pure $ case res of
    Left _er -> Left [i|Failed to add content #{contentId} to project #{projectId}|]
    Right sv -> Right (T.strip (T.pack (BLC.unpack sv)))


ghUpdateProjectItemField :: Aeson.ToJSON a => T.Text -> T.Text -> T.Text -> a -> IO (Either String T.Text)
ghUpdateProjectItemField projectId itemId fieldId value = do
  let query = $(embedStringFile "./src/Prix/extras/update_project_item_field.gql") :: T.Text
  let input =
        Aeson.object
          [ "query" Aeson..= query
          , "variables"
              Aeson..= Aeson.object
                [ "projectId" Aeson..= projectId
                , "itemId" Aeson..= itemId
                , "fieldId" Aeson..= fieldId
                , "value" Aeson..= value
                ]
          ]
  res <-
    runProcessStdinBLC
      "gh"
      [ "api"
      , "graphql"
      , "--input"
      , "-"
      , "--jq"
      , ".data.updateProjectV2ItemFieldValue.projectV2Item.id"
      ]
      (Aeson.encode input)
  pure $ case res of
    Left err -> Left [i|Failed to update project item field #{fieldId} for item #{itemId} in project #{projectId}\n\n  #{err}|]
    Right sv -> Right (T.strip (T.pack (BLC.unpack sv)))


ghAddAssigneesToAssignable :: T.Text -> [T.Text] -> IO (Either String T.Text)
ghAddAssigneesToAssignable assignableId assigneeIds = do
  let query = $(embedStringFile "./src/Prix/extras/add_assignees_to_assignable.gql") :: T.Text
  let input =
        Aeson.object
          [ "query" Aeson..= query
          , "variables"
              Aeson..= Aeson.object
                [ "assignableId" Aeson..= assignableId
                , "assigneeIds" Aeson..= assigneeIds
                ]
          ]
  res <-
    runProcessStdinBLC
      "gh"
      [ "api"
      , "graphql"
      , "--input"
      , "-"
      , "--jq"
      , ".data.addAssigneesToAssignable.clientMutationId"
      ]
      (Aeson.encode input)
  pure $ case res of
    Left _er -> Left [i|Failed to add assignees to assignable #{assignableId}|]
    Right sv -> Right (T.strip (T.pack (BLC.unpack sv)))


ghRemoveAssigneesFromAssignable :: T.Text -> [T.Text] -> IO (Either String T.Text)
ghRemoveAssigneesFromAssignable assignableId assigneeIds = do
  let query = $(embedStringFile "./src/Prix/extras/remove_assignees_from_assignable.gql") :: T.Text
  let input =
        Aeson.object
          [ "query" Aeson..= query
          , "variables"
              Aeson..= Aeson.object
                [ "assignableId" Aeson..= assignableId
                , "assigneeIds" Aeson..= assigneeIds
                ]
          ]
  res <-
    runProcessStdinBLC
      "gh"
      [ "api"
      , "graphql"
      , "--input"
      , "-"
      , "--jq"
      , ".data.removeAssigneesFromAssignable.clientMutationId"
      ]
      (Aeson.encode input)
  pure $ case res of
    Left _er -> Left [i|Failed to remove assignees from assignable #{assignableId}|]
    Right sv -> Right (T.strip (T.pack (BLC.unpack sv)))


ghUpdateDraftIssue :: T.Text -> T.Text -> Maybe T.Text -> [T.Text] -> IO (Either String T.Text)
ghUpdateDraftIssue draftId title mBody assigneeIds = do
  let query = $(embedStringFile "./src/Prix/extras/update_draft_issue.gql") :: T.Text
  let input =
        Aeson.object
          [ "query" Aeson..= query
          , "variables"
              Aeson..= Aeson.object
                [ "draftId" Aeson..= draftId
                , "title" Aeson..= title
                , "body" Aeson..= mBody
                , "assigneeIds" Aeson..= assigneeIds
                ]
          ]
  res <-
    runProcessStdinBLC
      "gh"
      [ "api"
      , "graphql"
      , "--input"
      , "-"
      , "--jq"
      , ".data.updateProjectV2DraftIssue.draftIssue.id"
      ]
      (Aeson.encode input)
  pure $ case res of
    Left _er -> Left [i|Failed to update draft issue #{draftId} #{_er}|]
    Right sv -> Right (T.strip (T.pack (BLC.unpack sv)))


ghCreateDraftItem :: T.Text -> T.Text -> Maybe T.Text -> [T.Text] -> IO (Either String T.Text)
ghCreateDraftItem projectId title mBody assigneeIds = do
  let query = $(embedStringFile "./src/Prix/extras/create_draft_item.gql") :: T.Text
  let input =
        Aeson.object
          [ "query" Aeson..= query
          , "variables"
              Aeson..= Aeson.object
                [ "projectId" Aeson..= projectId
                , "title" Aeson..= title
                , "body" Aeson..= mBody
                , "assigneeIds" Aeson..= assigneeIds
                ]
          ]
  res <-
    runProcessStdinBLC
      "gh"
      [ "api"
      , "graphql"
      , "--input"
      , "-"
      , "--jq"
      , ".data.addProjectV2DraftIssue.projectItem.id"
      ]
      (Aeson.encode input)
  pure $ case res of
    Left _er -> Left [i|Failed to create draft item in project #{projectId}: #{_er}|]
    Right sv -> Right (T.strip (T.pack (BLC.unpack sv)))


ghUpdateIssue :: T.Text -> T.Text -> Maybe T.Text -> Maybe T.Text -> IO (Either String T.Text)
ghUpdateIssue issueId title mBody mIssueTypeId = do
  let query = $(embedStringFile "./src/Prix/extras/update_issue.gql") :: T.Text
  let input =
        Aeson.object
          [ "query" Aeson..= query
          , "variables"
              Aeson..= Aeson.object
                [ "issueId" Aeson..= issueId
                , "title" Aeson..= title
                , "body" Aeson..= mBody
                , "issueTypeId" Aeson..= mIssueTypeId
                ]
          ]
  res <-
    runProcessStdinBLC
      "gh"
      [ "api"
      , "graphql"
      , "--input"
      , "-"
      , "--jq"
      , ".data.updateIssue.issue.id"
      ]
      (Aeson.encode input)
  pure $ case res of
    Left _er -> Left [i|Failed to update issue #{issueId}|]
    Right sv -> Right (T.strip (T.pack (BLC.unpack sv)))


ghUpdatePullRequest :: T.Text -> T.Text -> Maybe T.Text -> IO (Either String T.Text)
ghUpdatePullRequest pullRequestId title mBody = do
  let query = $(embedStringFile "./src/Prix/extras/update_pull_request.gql") :: T.Text
  let input =
        Aeson.object
          [ "query" Aeson..= query
          , "variables"
              Aeson..= Aeson.object
                [ "pullRequestId" Aeson..= pullRequestId
                , "title" Aeson..= title
                , "body" Aeson..= mBody
                ]
          ]
  res <-
    runProcessStdinBLC
      "gh"
      [ "api"
      , "graphql"
      , "--input"
      , "-"
      , "--jq"
      , ".data.updatePullRequest.pullRequest.id"
      ]
      (Aeson.encode input)
  pure $ case res of
    Left _er -> Left [i|Failed to update pull request #{pullRequestId}|]
    Right sv -> Right (T.strip (T.pack (BLC.unpack sv)))


-- * Helpers


type ProcessResult a = Either ProcessResultError a


type ProcessResultError = (Int, BL.ByteString, BL.ByteString)


printProcessResultError :: T.Text -> ProcessResultError -> IO ()
printProcessResultError label (code, out, err) = do
  hPutStrLn stderr (T.unpack label <> " failed with exit code " <> show code)
  unless (BL.null out) $ do
    hPutStrLn stderr "Standard Output:"
    BLC.hPutStrLn stderr out
  unless (BL.null err) $ do
    hPutStrLn stderr "Standard Error:"
    BLC.hPutStrLn stderr err


runProcessBLC :: T.Text -> [T.Text] -> IO (ProcessResult BL.ByteString)
runProcessBLC cmd args = do
  let process = TP.setStdout TP.byteStringOutput . TP.setStderr TP.byteStringOutput $ TP.proc (T.unpack cmd) (fmap T.unpack args)
  (exitCode, out, err) <- TP.readProcess process
  pure $ case exitCode of
    ExitSuccess -> Right out
    ExitFailure c -> Left (c, out, err)


runProcessStdinBLC :: T.Text -> [T.Text] -> BL.ByteString -> IO (ProcessResult BL.ByteString)
runProcessStdinBLC cmd args input = do
  let process =
        TP.setStdin (TP.byteStringInput input)
          . TP.setStdout TP.byteStringOutput
          . TP.setStderr TP.byteStringOutput
          $ TP.proc (T.unpack cmd) (fmap T.unpack args)
  (exitCode, out, err) <- TP.readProcess process
  pure $ case exitCode of
    ExitSuccess -> Right out
    ExitFailure c -> Left (c, out, err)


runProcessJSON :: Aeson.FromJSON a => T.Text -> [T.Text] -> IO (ProcessResult a)
runProcessJSON cmd args = do
  res <- runProcessBLC cmd args
  case res of
    Left err -> pure (Left err)
    Right out -> case Aeson.eitherDecode out of
      Left err2 -> pure (Left (-1, out, BLC.pack err2))
      Right val -> pure (Right val)


runProcessRead :: Read a => T.Text -> [T.Text] -> IO (ProcessResult a)
runProcessRead cmd args =
  runProcessBLC cmd args >>= \case
    Left err -> pure (Left err)
    Right out -> case reads (BLC.unpack out) of
      [(val, _)] -> pure (Right val)
      _ -> pure (Left (-1, out, "Failed to parse output"))
