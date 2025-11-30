{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Prix.Project where

import qualified Autodocodec as ADC
import Control.Applicative ((<|>))
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Fixed (Milli)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Time as Time
import GHC.Generics (Generic)
import qualified Options.Applicative as OA
import Prix.Config (ConfigProject (..), Owner (..))
import System.Exit (ExitCode (..), die)
import System.IO (hPutStrLn, stderr)
import qualified System.Process.Typed as TP
import qualified Zamazingo.Text as Z.Text


-- * Project


data Project = MkProject
  { projectOwner :: !ProjectOwner
  , projectMeta :: !ProjectMeta
  , projectItems :: ![ProjectItem]
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Project)


instance ADC.HasCodec Project where
  codec =
    ADC.object "Project" $
      MkProject
        <$> ADC.requiredField "owner" "Project Owner" ADC..= projectOwner
        <*> ADC.requiredField "meta" "Project Metadata" ADC..= projectMeta
        <*> ADC.requiredField "items" "Project Items" ADC..= projectItems


-- * Project Owner


data ProjectOwner = MkProjectOwner
  { projectOwnerType :: !OwnerType
  , projectOwnerLogin :: !T.Text
  , projectOwnerUrl :: !T.Text
  , projectOwnerAvatarUrl :: !(Maybe T.Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectOwner)


instance ADC.HasCodec ProjectOwner where
  codec =
    ADC.object "ProjectOwner" $
      MkProjectOwner
        <$> ADC.requiredField "type" "Owner Type" ADC..= projectOwnerType
        <*> ADC.requiredField "login" "Owner Login" ADC..= projectOwnerLogin
        <*> ADC.requiredField "url" "Owner URL" ADC..= projectOwnerUrl
        <*> ADC.requiredField "avatarUrl" "Avatar URL" ADC..= projectOwnerAvatarUrl


-- * Project Meta


data ProjectMeta = MkProjectMeta
  { projectMetaNumber :: !Integer
  , projectMetaTitle :: !T.Text
  , projectMetaUrl :: !T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectMeta)


instance ADC.HasCodec ProjectMeta where
  codec =
    ADC.object "ProjectMeta" $
      MkProjectMeta
        <$> ADC.requiredField "number" "Project Number" ADC..= projectMetaNumber
        <*> ADC.requiredField "title" "Project Title" ADC..= projectMetaTitle
        <*> ADC.requiredField "url" "Project URL" ADC..= projectMetaUrl


-- * Project Item


data ProjectItem = MkProjectItem
  { projectItemId :: !T.Text
  , projectItemStatus :: !(Maybe ProjectItemStatus)
  , projectItemIteration :: !(Maybe Integer)
  , projectItemUrgency :: !(Maybe ProjectItemUrgency)
  , projectItemImpact :: !(Maybe ProjectItemImpact)
  , projectItemReach :: !(Maybe ProjectItemReach)
  , projectItemSize :: !(Maybe ProjectItemSize)
  , projectItemDifficulty :: !(Maybe ProjectItemDifficulty)
  , projectItemConfidence :: !(Maybe ProjectItemConfidence)
  , projectItemTheme :: !(Maybe ProjectItemTheme)
  , projectItemScore :: !(Maybe Milli)
  , projectItemTitle :: !T.Text
  , projectItemCreatedAt :: !Time.UTCTime
  , projectItemAssignee :: !(Maybe T.Text)
  , projectItemBody :: !(Maybe T.Text)
  , projectItemContent :: !ProjectItemContent
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectItem)


instance ADC.HasCodec ProjectItem where
  codec =
    ADC.object "ProjectItem" $
      MkProjectItem
        <$> ADC.requiredField "id" "Item ID" ADC..= projectItemId
        <*> ADC.requiredField "status" "Item Status" ADC..= projectItemStatus
        <*> ADC.requiredField "iteration" "Iteration ID" ADC..= projectItemIteration
        <*> ADC.requiredField "urgency" "Urgency Level" ADC..= projectItemUrgency
        <*> ADC.requiredField "impact" "Impact Level" ADC..= projectItemImpact
        <*> ADC.requiredField "reach" "Reach Level" ADC..= projectItemReach
        <*> ADC.requiredField "size" "Size Level" ADC..= projectItemSize
        <*> ADC.requiredField "difficulty" "Difficulty Level" ADC..= projectItemDifficulty
        <*> ADC.requiredField "confidence" "Confidence Level" ADC..= projectItemConfidence
        <*> ADC.requiredField "theme" "Strategic Theme" ADC..= projectItemTheme
        <*> ADC.requiredField "score" "Priority Score" ADC..= projectItemScore
        <*> ADC.requiredField "title" "Item Title" ADC..= projectItemTitle
        <*> ADC.requiredField "createdAt" "Creation Time" ADC..= projectItemCreatedAt
        <*> ADC.requiredField "assignee" "Assignee Login" ADC..= projectItemAssignee
        <*> ADC.requiredField "body" "Item Body" ADC..= projectItemBody
        <*> ADC.requiredField "content" "Item Content" ADC..= projectItemContent


-- ** Content


data ProjectItemContent
  = ProjectItemContentDraftIssue !DraftIssueContent
  | ProjectItemContentIssue !IssueContent
  | ProjectItemContentPullRequest !PullRequestContent
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectItemContent)


instance ADC.HasCodec ProjectItemContent where
  codec = ADC.object "ProjectItemContent" ADC.objectCodec


instance ADC.HasObjectCodec ProjectItemContent where
  objectCodec = ADC.discriminatedUnionCodec "type" enc dec
    where
      draftIssueCodec = ADC.requiredField "value" "Draft Issue Content"
      issueCodec = ADC.requiredField "value" "Issue Content"
      pullRequestCodec = ADC.requiredField "value" "Pull Request Content"
      enc = \case
        ProjectItemContentDraftIssue d -> ("DRAFT_ISSUE", ADC.mapToEncoder d draftIssueCodec)
        ProjectItemContentIssue i -> ("ISSUE", ADC.mapToEncoder i issueCodec)
        ProjectItemContentPullRequest p -> ("PULL_REQUEST", ADC.mapToEncoder p pullRequestCodec)
      dec =
        HashMap.fromList
          [ ("DRAFT_ISSUE", ("DRAFT_ISSUE", ADC.mapToDecoder ProjectItemContentDraftIssue draftIssueCodec))
          , ("ISSUE", ("ISSUE", ADC.mapToDecoder ProjectItemContentIssue issueCodec))
          , ("PULL_REQUEST", ("PULL_REQUEST", ADC.mapToDecoder ProjectItemContentPullRequest pullRequestCodec))
          ]


-- *** Draft


newtype DraftIssueContent = MkDraftIssueContent
  { draftIssueContentId :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec DraftIssueContent)


instance ADC.HasCodec DraftIssueContent where
  codec =
    ADC.object "DraftIssueContent" $
      MkDraftIssueContent
        <$> ADC.requiredField "id" "Draft Issue ID" ADC..= draftIssueContentId


-- *** Issue


data IssueContent = MkIssueContent
  { issueContentId :: !T.Text
  , issueContentRepository :: !T.Text
  , issueContentNumber :: !Integer
  , issueContentUrl :: !T.Text
  , issueContentState :: !IssueState
  , issueContentStateReason :: !(Maybe IssueStateReason)
  , issueContentIssueType :: !(Maybe IssueType)
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec IssueContent)


instance ADC.HasCodec IssueContent where
  codec =
    ADC.object "IssueContent" $
      MkIssueContent
        <$> ADC.requiredField "id" "Issue ID" ADC..= issueContentId
        <*> ADC.requiredField "repository" "Repository Name" ADC..= issueContentRepository
        <*> ADC.requiredField "number" "Issue Number" ADC..= issueContentNumber
        <*> ADC.requiredField "url" "Issue URL" ADC..= issueContentUrl
        <*> ADC.requiredField "state" "Issue State" ADC..= issueContentState
        <*> ADC.requiredField "stateReason" "State Reason" ADC..= issueContentStateReason
        <*> ADC.requiredField "issueType" "Issue Type" ADC..= issueContentIssueType


-- | Type of the issue.
data IssueType
  = -- | A specific piece of work
    IssueTypeTask
  | -- | An unexpected problem or behavior
    IssueTypeBug
  | -- | A request, idea or new functionality
    IssueTypeFeature
  | -- | A large initiative that groups multiple related issues
    IssueTypeEpic
  | -- | Documentation-related work
    IssueTypeDocs
  deriving (Eq, Show, Ord, Bounded, Enum)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec IssueType)


instance ADC.HasCodec IssueType where
  codec = ADC.boundedEnumCodec issueTypeLabel


issueTypeLabel :: IssueType -> T.Text
issueTypeLabel IssueTypeTask = "Task"
issueTypeLabel IssueTypeBug = "Bug"
issueTypeLabel IssueTypeFeature = "Feature"
issueTypeLabel IssueTypeEpic = "Epic"
issueTypeLabel IssueTypeDocs = "Docs"


data IssueState
  = IssueStateOpen
  | IssueStateClosed
  deriving (Eq, Show, Ord, Generic, Enum, Bounded)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via ADC.Autodocodec IssueState


instance ADC.HasCodec IssueState where
  codec = ADC.boundedEnumCodec issueStateLabel


issueStateLabel :: IssueState -> T.Text
issueStateLabel IssueStateOpen = "OPEN"
issueStateLabel IssueStateClosed = "CLOSED"


issueStateEmoji :: IssueState -> T.Text
issueStateEmoji IssueStateOpen = "🟢"
issueStateEmoji IssueStateClosed = "🔴"


data IssueStateReason
  = IssueStateReasonReopened
  | IssueStateReasonNotPlanned
  | IssueStateReasonCompleted
  | IssueStateReasonDuplicate
  deriving (Eq, Show, Ord, Generic, Enum, Bounded)


instance ADC.HasCodec IssueStateReason where
  codec = ADC.boundedEnumCodec issueStateReasonLabel


issueStateReasonLabel :: IssueStateReason -> T.Text
issueStateReasonLabel IssueStateReasonReopened = "REOPENED"
issueStateReasonLabel IssueStateReasonNotPlanned = "NOT_PLANNED"
issueStateReasonLabel IssueStateReasonCompleted = "COMPLETED"
issueStateReasonLabel IssueStateReasonDuplicate = "DUPLICATE"


issueStateReasonEmoji :: IssueStateReason -> T.Text
issueStateReasonEmoji IssueStateReasonReopened = "♻️"
issueStateReasonEmoji IssueStateReasonNotPlanned = "❌"
issueStateReasonEmoji IssueStateReasonCompleted = "✅"
issueStateReasonEmoji IssueStateReasonDuplicate = "⿻"


-- *** Pull Request


data PullRequestContent = MkPullRequestContent
  { pullRequestContentId :: !T.Text
  , pullRequestContentRepository :: !T.Text
  , pullRequestContentNumber :: !Integer
  , pullRequestContentUrl :: !T.Text
  , pullRequestContentState :: !PullRequestState
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec PullRequestContent)


instance ADC.HasCodec PullRequestContent where
  codec =
    ADC.object "PullRequestContent" $
      MkPullRequestContent
        <$> ADC.requiredField "id" "Pull Request ID" ADC..= pullRequestContentId
        <*> ADC.requiredField "repository" "Repository Name" ADC..= pullRequestContentRepository
        <*> ADC.requiredField "number" "Pull Request Number" ADC..= pullRequestContentNumber
        <*> ADC.requiredField "url" "Pull Request URL" ADC..= pullRequestContentUrl
        <*> ADC.requiredField "state" "Pull Request State" ADC..= pullRequestContentState


data PullRequestState
  = PullRequestStateOpen
  | PullRequestStateClosed
  | PullRequestStateMerged
  deriving (Eq, Show, Ord, Generic, Enum, Bounded)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via ADC.Autodocodec PullRequestState


instance ADC.HasCodec PullRequestState where
  codec = ADC.boundedEnumCodec pullRequestStateLabel


pullRequestStateLabel :: PullRequestState -> T.Text
pullRequestStateLabel PullRequestStateOpen = "OPEN"
pullRequestStateLabel PullRequestStateClosed = "CLOSED"
pullRequestStateLabel PullRequestStateMerged = "MERGED"


pullRequestStateEmoji :: PullRequestState -> T.Text
pullRequestStateEmoji PullRequestStateOpen = "🟢"
pullRequestStateEmoji PullRequestStateClosed = "🔴"
pullRequestStateEmoji PullRequestStateMerged = "🟣"


-- ** Properties


-- *** Status


-- | An issue can be in one of these following statuses, representing its current position in the workflow.
data ProjectItemStatus
  = -- | New and unreviewed, no one has looked at it or decided what to do
    ProjectItemStatusInbox
  | -- | Reviewed but unclear, needs more information or a decision before it can move forward
    ProjectItemStatusTriage
  | -- | Clear and ready, the issue is understood but not yet scheduled
    ProjectItemStatusBacklog
  | -- | Scheduled and prioritized, work is expected to begin in an upcoming iteration
    ProjectItemStatusPlanned
  | -- | In progress, someone is currently working on the issue
    ProjectItemStatusActive
  | -- | Completed and verified, no further work is needed
    ProjectItemStatusDone
  deriving (Eq, Show, Ord, Bounded, Enum)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectItemStatus)


instance ADC.HasCodec ProjectItemStatus where
  codec = ADC.boundedEnumCodec projectItemStatusLabel


projectItemStatusLabel :: ProjectItemStatus -> T.Text
projectItemStatusLabel ProjectItemStatusInbox = "Inbox"
projectItemStatusLabel ProjectItemStatusTriage = "Triage"
projectItemStatusLabel ProjectItemStatusBacklog = "Backlog"
projectItemStatusLabel ProjectItemStatusPlanned = "Planned"
projectItemStatusLabel ProjectItemStatusActive = "Active"
projectItemStatusLabel ProjectItemStatusDone = "Done"


projectItemStatusColor :: ProjectItemStatus -> OptionColor
projectItemStatusColor ProjectItemStatusInbox = OptionColorGray
projectItemStatusColor ProjectItemStatusTriage = OptionColorBlue
projectItemStatusColor ProjectItemStatusBacklog = OptionColorGreen
projectItemStatusColor ProjectItemStatusPlanned = OptionColorYellow
projectItemStatusColor ProjectItemStatusActive = OptionColorRed
projectItemStatusColor ProjectItemStatusDone = OptionColorPurple


projectItemStatusEmoji :: ProjectItemStatus -> T.Text
projectItemStatusEmoji =
  optionColorEmoji . projectItemStatusColor


projectItemStatusColorLabel :: ProjectItemStatus -> T.Text
projectItemStatusColorLabel s =
  optionColorEmoji (projectItemStatusColor s) <> " " <> projectItemStatusLabel s


-- *** Urgency


-- | The urgency of an issue, which indicates how urgent it is to address the issue.
--
-- >>> minBound :: ProjectItemUrgency
-- ProjectItemUrgencyLow
-- >>> maxBound :: ProjectItemUrgency
-- ProjectItemUrgencyHigh
-- >>> [ProjectItemUrgencyLow .. ProjectItemUrgencyHigh]
-- [ProjectItemUrgencyLow,ProjectItemUrgencyMedium,ProjectItemUrgencyHigh]
--
-- __Urgency Examples:__
--
-- These are some examples of how to determine the urgency of an issue:
--
-- +-----------------------------------------+---------+-----------+
-- | Issue Type                              | Urgency | Impact    |
-- +=========================================+=========+===========+
-- | Client deadline                         | High    | Medium    |
-- +-----------------------------------------+---------+-----------+
-- | Legal compliance for upcoming audit     | High    | Critical  |
-- +-----------------------------------------+---------+-----------+
-- | Long-term platform upgrade              | Low     | High      |
-- +-----------------------------------------+---------+-----------+
-- | Minor typo in UI                        | Low     | Low       |
-- +-----------------------------------------+---------+-----------+
--
-- __Medium Urgency Examples:__
--
-- Identifying medium urgency can be tricky. Here are some examples:
--
-- +-----------------------------------------+-----------------------------------------------------------+
-- | Example                                 | Why it is Medium Urgency                                  |
-- +=========================================+===========================================================+
-- | Write customer onboarding guide         | Needed to improve retention, but not blocking sign-ups    |
-- +-----------------------------------------+-----------------------------------------------------------+
-- | Upgrade Nginx config for TLS 1.3        | Security/performance boost, but no active incidents       |
-- +-----------------------------------------+-----------------------------------------------------------+
-- | Prepare audit docs due next month       | Time-bound, but not urgent this week                      |
-- +-----------------------------------------+-----------------------------------------------------------+
-- | Add telemetry to new feature            | Helps future decisions, but feature already shipped       |
-- +-----------------------------------------+-----------------------------------------------------------+
-- | Refactor legacy utility module          | Important for long-term maintainability, but not critical |
-- +-----------------------------------------+-----------------------------------------------------------+
data ProjectItemUrgency
  = -- | No time pressure, can be postponed
    ProjectItemUrgencyLow
  | -- | Should be done this iteration or next (important but not blocking)
    ProjectItemUrgencyMedium
  | -- | Must be done immediately or this week (blocking, deadline)
    ProjectItemUrgencyHigh
  deriving (Eq, Show, Ord, Bounded, Enum)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectItemUrgency)


instance ADC.HasCodec ProjectItemUrgency where
  codec = ADC.boundedEnumCodec projectItemUrgencyLabel


projectItemUrgencyLabel :: ProjectItemUrgency -> T.Text
projectItemUrgencyLabel ProjectItemUrgencyLow = "low"
projectItemUrgencyLabel ProjectItemUrgencyMedium = "medium"
projectItemUrgencyLabel ProjectItemUrgencyHigh = "high"


-- *** Impact


-- | The impact of an issue, which indicates how much value it brings to the business or project.
--
-- >>> minBound :: ProjectItemImpact
-- ProjectItemImpactLow
-- >>> maxBound :: ProjectItemImpact
-- ProjectItemImpactCritical
-- >>> [ProjectItemImpactLow .. ProjectItemImpactCritical]
-- [ProjectItemImpactLow,ProjectItemImpactMedium,ProjectItemImpactHigh,ProjectItemImpactCritical]
data ProjectItemImpact
  = -- | Nice-to-have or speculative (e.g., minor enhancement, research)
    ProjectItemImpactLow
  | -- | Operationally useful (e.g., performance improvement, bug fix)
    ProjectItemImpactMedium
  | -- | Supports a major initiative (e.g., new feature, platform upgrade)
    ProjectItemImpactHigh
  | -- | Directly moves a core business KPI (e.g., revenue, user growth)
    ProjectItemImpactCritical
  deriving (Eq, Show, Ord, Bounded, Enum)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectItemImpact)


instance ADC.HasCodec ProjectItemImpact where
  codec = ADC.boundedEnumCodec projectItemImpactLabel


projectItemImpactLabel :: ProjectItemImpact -> T.Text
projectItemImpactLabel ProjectItemImpactLow = "low"
projectItemImpactLabel ProjectItemImpactMedium = "medium"
projectItemImpactLabel ProjectItemImpactHigh = "high"
projectItemImpactLabel ProjectItemImpactCritical = "critical"


-- *** Reach


-- | The reach of an issue, which indicates how many users, clients or stakeholders are affected by the issue.
--
-- >>> minBound :: ProjectItemReach
-- ProjectItemReachInternal
-- >>> maxBound :: ProjectItemReach
-- ProjectItemReachWide
-- >>> [ProjectItemReachInternal .. ProjectItemReachWide]
-- [ProjectItemReachInternal,ProjectItemReachNarrow,ProjectItemReachModerate,ProjectItemReachWide]
data ProjectItemReach
  = -- | Affects only staff, internal systems, or non-customer
    ProjectItemReachInternal
  | -- | Affects a few users, a corner case, or small subset
    ProjectItemReachNarrow
  | -- | Affects a specific segment, team, or high-value group
    ProjectItemReachModerate
  | -- | Affects nearly all users, clients, or key stakeholders
    ProjectItemReachWide
  deriving (Eq, Show, Ord, Bounded, Enum)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectItemReach)


instance ADC.HasCodec ProjectItemReach where
  codec = ADC.boundedEnumCodec projectItemReachLabel


projectItemReachLabel :: ProjectItemReach -> T.Text
projectItemReachLabel ProjectItemReachInternal = "internal"
projectItemReachLabel ProjectItemReachNarrow = "narrow"
projectItemReachLabel ProjectItemReachModerate = "moderate"
projectItemReachLabel ProjectItemReachWide = "wide"


-- *** Confidence


-- | Confidence in the value of an issue, which indicates how certain we are about the value it will bring.
--
-- >>> minBound :: ProjectItemConfidence
-- ProjectItemConfidenceWeak
-- >>> maxBound :: ProjectItemConfidence
-- ProjectItemConfidenceStrong
-- >>> [ProjectItemConfidenceWeak .. ProjectItemConfidenceStrong]
-- [ProjectItemConfidenceWeak,ProjectItemConfidenceModerate,ProjectItemConfidenceStrong]
data ProjectItemConfidence
  = -- | Little evidence, mostly speculative or intuition-based
    ProjectItemConfidenceWeak
  | -- | Some evidence or assumptions, with moderate uncertainty
    ProjectItemConfidenceModerate
  | -- | Strong data, direct user feedback, or well-supported experience
    ProjectItemConfidenceStrong
  deriving (Eq, Show, Ord, Bounded, Enum)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectItemConfidence)


instance ADC.HasCodec ProjectItemConfidence where
  codec = ADC.boundedEnumCodec projectItemConfidenceLabel


projectItemConfidenceLabel :: ProjectItemConfidence -> T.Text
projectItemConfidenceLabel ProjectItemConfidenceWeak = "weak"
projectItemConfidenceLabel ProjectItemConfidenceModerate = "moderate"
projectItemConfidenceLabel ProjectItemConfidenceStrong = "strong"


-- *** Size


-- | The size of a deliverable, which indicates how much work is involved in producing or changing it.
--
-- >>> minBound :: ProjectItemSize
-- ProjectItemSizeSmall
-- >>> maxBound :: ProjectItemSize
-- ProjectItemSizeLarge
-- >>> [ProjectItemSizeSmall .. ProjectItemSizeLarge]
-- [ProjectItemSizeSmall,ProjectItemSizeMedium,ProjectItemSizeLarge]
--
-- __Examples:__
--
-- +--------+-------+----------------------------------------------------+-------------------------------------------------+
-- | Label  | Value | Description                                        | Example Deliverables                            |
-- +========+=======+====================================================+=================================================+
-- | small  | 0     | Localized change or single-file/module             | Bug fix, config tweak, 1-page doc               |
-- +--------+-------+----------------------------------------------------+-------------------------------------------------+
-- | medium | 1     | Cohesive unit across a few components/artifacts    | CLI tool, small feature, multi-section doc      |
-- +--------+-------+----------------------------------------------------+-------------------------------------------------+
-- | large  | 2     | Cross-cutting change or multi-system deliverable   | Subsystem refactor, full workflow, new doc site |
-- +--------+-------+----------------------------------------------------+-------------------------------------------------+
--
-- __More Examples:__
--
-- +--------+-----------------------------------------------+----------------------------------+----------------------------------+
-- | Size   | Software Example                              | Docs Example                     | Ops Example                      |
-- +========+===============================================+==================================+==================================+
-- | small  | Edit config, fix bug in 1 file                | Update 1 README or page section  | Restart service, patch 1 node    |
-- +--------+-----------------------------------------------+----------------------------------+----------------------------------+
-- | medium | Add API endpoint, refactor a module           | Write new guide, revise full doc | Rework backup job, migrate DB    |
-- +--------+-----------------------------------------------+----------------------------------+----------------------------------+
-- | large  | Cross-cutting auth layer, redesign UI         | Write full doc site or playbook  | Redesign infra, deploy region    |
-- +--------+-----------------------------------------------+----------------------------------+----------------------------------+
--
-- __Tips for Estimating Size:__
--
-- * Focus on scope and boundaries of the change (not time or difficulty).
-- * Consider:
--
--     * Number of files, modules or systems touched
--     * Number of deliverables or user-facing artifacts
--     * Breadth of integration required
data ProjectItemSize
  = -- | Localized change or single-file/module
    ProjectItemSizeSmall
  | -- | A cohesive unit across a few components or artifacts
    ProjectItemSizeMedium
  | -- | Cross-cutting change or multi-system deliverable
    ProjectItemSizeLarge
  deriving (Eq, Show, Ord, Bounded, Enum)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectItemSize)


instance ADC.HasCodec ProjectItemSize where
  codec = ADC.boundedEnumCodec projectItemSizeLabel


projectItemSizeLabel :: ProjectItemSize -> T.Text
projectItemSizeLabel ProjectItemSizeSmall = "small"
projectItemSizeLabel ProjectItemSizeMedium = "medium"
projectItemSizeLabel ProjectItemSizeLarge = "large"


-- *** Difficulty


-- | The difficulty of a task, which indicates how hard it is to reason about or execute.
--
-- >>> minBound :: ProjectItemDifficulty
-- ProjectItemDifficultyEasy
-- >>> maxBound :: ProjectItemDifficulty
-- ProjectItemDifficultyHard
-- >>> [ProjectItemDifficultyEasy .. ProjectItemDifficultyHard]
-- [ProjectItemDifficultyEasy,ProjectItemDifficultyMedium,ProjectItemDifficultyHard]
--
-- __Examples:__
--
-- +--------+-------+-------------------------------------------------+---------------------------------------------------------+
-- | Label  | Value | Description                                     | Example Characteristics                                 |
-- +========+=======+=================================================+=========================================================+
-- | easy   | 0     | Straightforward, routine, low risk              | CRUD updates, static content, scripted change           |
-- +--------+-------+-------------------------------------------------+---------------------------------------------------------+
-- | medium | 1     | Requires judgment, abstraction, or coordination | Feature design, moderate refactor, multi-system insight |
-- +--------+-------+-------------------------------------------------+---------------------------------------------------------+
-- | hard   | 2     | Conceptually complex, uncertain, or high-risk   | Non-obvious design, domain modeling, async logic        |
-- +--------+-------+-------------------------------------------------+---------------------------------------------------------+
--
-- __Some More Examples:__
--
-- +------------+----------------------------------------------------+---------------------------------------------+--------------------------------------------------------+
-- | Difficulty | Software Example                                   | Docs Example                                | Ops Example                                            |
-- +============+====================================================+=============================================+========================================================+
-- | easy       | Fix typo, change constant, add a log line          | Reformat text, fix broken link, typo        | Restart service, update cron schedule                  |
-- +------------+----------------------------------------------------+---------------------------------------------+--------------------------------------------------------+
-- | medium     | Add feature with some logic, refactor a module     | Draft a new guide from known process        | Tune database, script automation, configure TLS        |
-- +------------+----------------------------------------------------+---------------------------------------------+--------------------------------------------------------+
-- | hard       | Design new abstraction, async processing, ORM swap | Document complex architecture, write policy | Migrate production DB, failover planning, IAM overhaul |
-- +------------+----------------------------------------------------+---------------------------------------------+--------------------------------------------------------+
--
-- __Tips for Estimating Difficulty:__
--
-- Think about:
--
-- - Ambiguity or novelty
-- - Number of decision points or edge cases
-- - Coordination with other people, services, or environments
--
-- Two tasks of the same size can have very different difficulty scores.
data ProjectItemDifficulty
  = -- | Straightforward, routine, low risk
    ProjectItemDifficultyEasy
  | -- | Requires judgment, abstraction, or coordination
    ProjectItemDifficultyMedium
  | -- | Conceptually complex, uncertain, or high-risk
    ProjectItemDifficultyHard
  deriving (Eq, Show, Ord, Bounded, Enum)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectItemDifficulty)


instance ADC.HasCodec ProjectItemDifficulty where
  codec = ADC.boundedEnumCodec projectItemDifficultyLabel


projectItemDifficultyLabel :: ProjectItemDifficulty -> T.Text
projectItemDifficultyLabel ProjectItemDifficultyEasy = "easy"
projectItemDifficultyLabel ProjectItemDifficultyMedium = "medium"
projectItemDifficultyLabel ProjectItemDifficultyHard = "hard"


-- *** Theme


-- | Strategic themes help align work with business goals and priorities. Each issue
-- should be assigned to a theme that reflects its strategic value and purpose. It
-- will also inform the reach and impact of the issue.
--
-- +---------------+----------------------------------------------------------------+--------------------------------------------+
-- | Theme         | Description                                                    | Use if…                                    |
-- +===============+================================================================+============================================+
-- | @growth@      | Revenue, acquisition, onboarding, feature adoption             | Helps acquire or monetize users            |
-- +---------------+----------------------------------------------------------------+--------------------------------------------+
-- | @retention@   | Satisfaction, reliability, bug fixes, usability                | Improves user happiness or engagement      |
-- +---------------+----------------------------------------------------------------+--------------------------------------------+
-- | @efficiency@  | Cost savings, automation, internal tools, developer experience | Saves time, money, or staff effort         |
-- +---------------+----------------------------------------------------------------+--------------------------------------------+
-- | @compliance@  | Regulatory, legal, audit, privacy, certifications              | Required by law, contract, or regulation   |
-- +---------------+----------------------------------------------------------------+--------------------------------------------+
-- | @maintenance@ | Upkeep, tech debt, upgrades, refactoring                       | Keeps systems stable and sustainable       |
-- +---------------+----------------------------------------------------------------+--------------------------------------------+
-- | @learning@    | Research, prototyping, exploration                             | Explores a new idea or reduces future risk |
-- +---------------+----------------------------------------------------------------+--------------------------------------------+
data ProjectItemTheme
  = -- | Helps acquire or monetize users
    ProjectItemThemeGrowth
  | -- | Improves user happiness or engagement
    ProjectItemThemeRetention
  | -- | Saves time, money, or staff effort
    ProjectItemThemeEfficiency
  | -- | Required by law, contract, or regulation
    ProjectItemThemeCompliance
  | -- | Keeps systems stable and sustainable
    ProjectItemThemeMaintenance
  | -- | Explores a new idea or reduces future risk
    ProjectItemThemeLearning
  deriving (Eq, Show, Ord, Bounded, Enum)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectItemTheme)


instance ADC.HasCodec ProjectItemTheme where
  codec = ADC.boundedEnumCodec projectItemThemeLabel


projectItemThemeLabel :: ProjectItemTheme -> T.Text
projectItemThemeLabel ProjectItemThemeGrowth = "growth"
projectItemThemeLabel ProjectItemThemeRetention = "retention"
projectItemThemeLabel ProjectItemThemeEfficiency = "efficiency"
projectItemThemeLabel ProjectItemThemeCompliance = "compliance"
projectItemThemeLabel ProjectItemThemeMaintenance = "maintenance"
projectItemThemeLabel ProjectItemThemeLearning = "learning"


-- *** Effort


-- | The effort estimate for an issue, which indicates how much time it will take to complete.
--
-- __Guidelines:__
--
-- 1. If size or difficulty is unknown:
--
--     - Time estimate is unknown
--     - Issue is __not actionable__
--     - Create a new issue to __estimate it__
--
-- 2. 1 estimate-point is 1 pomodoro-time. Example equivalences:
-- 3. Pomodoro-time may vary from individual to individual. The default is a __focused 25-minute block__.
--
-- __Effort Estimate Formula:__
--
-- \[
-- \mbox{Effort Estimate} = 2^{(\mbox{Size} + \mbox{Difficulty})}
-- \]
--
-- __Quick Reference Table:__
--
-- +------------------+----------+------------+----------+
-- | Size  Difficulty | easy (0) | medium (1) | hard (2) |
-- +==================+==========+============+==========+
-- | small (0)        | 1        | 2          | 4        |
-- +------------------+----------+------------+----------+
-- | medium (1)       | 2        | 4          | 8        |
-- +------------------+----------+------------+----------+
-- | large (2)        | 4        | 8          | 16       |
-- +------------------+----------+------------+----------+
--
-- __Examples:__
--
-- +--------+----------------------------------------------------------------+
-- | Effort | Example Task                                                   |
-- +========+================================================================+
-- | 1      | Update a config, fix a typo, restart a service                 |
-- +--------+----------------------------------------------------------------+
-- | 4      | Add a new CLI command, refactor a mid-size module              |
-- +--------+----------------------------------------------------------------+
-- | 8      | Build multi-part feature across UI & backend, draft full doc   |
-- +--------+----------------------------------------------------------------+
-- | 16     | Migrate infrastructure, implement new authentication system    |
-- +--------+----------------------------------------------------------------+
projectItemEffortEstimate :: ProjectItemSize -> ProjectItemDifficulty -> Integer
projectItemEffortEstimate size difficulty =
  let sizeFactor = projectItemSizeFactor size
      difficultyFactor = projectItemDifficultyFactor difficulty
   in 2 ^ (sizeFactor + difficultyFactor)


-- *** Priority


-- | Project item priority.
--
-- \[
-- \mbox{Score} = \mbox{Urgency} \times \left(\frac{\mbox{Reach} \times \mbox{Impact} \times \mbox{Confidence}}{\mbox{Effort}}\right)
-- \]
--
-- The lowest priority can be:
--
-- >>> projectItemPriority ProjectItemUrgencyLow ProjectItemReachInternal ProjectItemImpactLow ProjectItemConfidenceWeak ProjectItemSizeLarge ProjectItemDifficultyHard
-- 0.006
--
-- The highest priority can be:
--
-- >>> projectItemPriority ProjectItemUrgencyHigh ProjectItemReachWide ProjectItemImpactCritical ProjectItemConfidenceStrong ProjectItemSizeSmall ProjectItemDifficultyEasy
-- 18.000
projectItemPriority
  :: ProjectItemUrgency
  -> ProjectItemReach
  -> ProjectItemImpact
  -> ProjectItemConfidence
  -> ProjectItemSize
  -> ProjectItemDifficulty
  -> Milli
projectItemPriority urgency reach impact confidence size difficulty =
  let urgencyFactor = projectItemUrgencyFactor urgency
      reachFactor = projectItemReachFactor reach
      impactFactor = projectItemImpactFactor impact
      confidenceFactor = projectItemConfidenceFactor confidence
      effortEstimate = projectItemEffortEstimate size difficulty
   in if effortEstimate == 0
        then 0.0
        else urgencyFactor * (reachFactor * impactFactor * confidenceFactor / fromIntegral effortEstimate)


projectItemUrgencyFactor :: ProjectItemUrgency -> Milli
projectItemUrgencyFactor ProjectItemUrgencyLow = 0.7
projectItemUrgencyFactor ProjectItemUrgencyMedium = 1.0
projectItemUrgencyFactor ProjectItemUrgencyHigh = 1.5


projectItemImpactFactor :: ProjectItemImpact -> Milli
projectItemImpactFactor ProjectItemImpactLow = 1
projectItemImpactFactor ProjectItemImpactMedium = 2
projectItemImpactFactor ProjectItemImpactHigh = 3
projectItemImpactFactor ProjectItemImpactCritical = 4


projectItemReachFactor :: ProjectItemReach -> Milli
projectItemReachFactor ProjectItemReachInternal = 0.5
projectItemReachFactor ProjectItemReachNarrow = 1.0
projectItemReachFactor ProjectItemReachModerate = 2.0
projectItemReachFactor ProjectItemReachWide = 3.0


projectItemConfidenceFactor :: ProjectItemConfidence -> Milli
projectItemConfidenceFactor ProjectItemConfidenceWeak = 0.3
projectItemConfidenceFactor ProjectItemConfidenceModerate = 0.6
projectItemConfidenceFactor ProjectItemConfidenceStrong = 1.0


projectItemSizeFactor :: ProjectItemSize -> Integer
projectItemSizeFactor ProjectItemSizeSmall = 0
projectItemSizeFactor ProjectItemSizeMedium = 1
projectItemSizeFactor ProjectItemSizeLarge = 2


projectItemDifficultyFactor :: ProjectItemDifficulty -> Integer
projectItemDifficultyFactor ProjectItemDifficultyEasy = 0
projectItemDifficultyFactor ProjectItemDifficultyMedium = 1
projectItemDifficultyFactor ProjectItemDifficultyHard = 2


-- * Auxiliary Types


-- ** Iteration


data IterationQuery
  = IterationQueryPrevious
  | IterationQueryCurrent
  | IterationQueryNext
  | IterationQueryDate !Time.Day
  | IterationQueryIteration !Integer
  deriving (Eq, Show)


iterationQueryParser :: OA.Parser IterationQuery
iterationQueryParser =
  previous <|> current <|> next <|> bydate <|> absolute
  where
    previous = IterationQueryPrevious <$ OA.flag' () (OA.long "previous" <> OA.help "Select previous iteration")
    current = IterationQueryCurrent <$ OA.flag' () (OA.long "current" <> OA.help "Select current iteration")
    next = IterationQueryNext <$ OA.flag' () (OA.long "next" <> OA.help "Select next iteration")
    bydate = IterationQueryDate <$> OA.option OA.auto (OA.long "date" <> OA.metavar "YYYY-MM-DD" <> OA.help "Select iteration by date (YYYY-MM-DD format)")
    absolute = IterationQueryIteration <$> OA.option OA.auto (OA.long "iter" <> OA.metavar "N" <> OA.help "Select iteration number N")


queryIteration :: Integer -> Time.Day -> IterationQuery -> IO Integer
queryIteration days inception query = do
  reference <- case query of
    IterationQueryPrevious -> Time.addDays (-7) . Time.utctDay <$> Time.getCurrentTime
    IterationQueryCurrent -> Time.utctDay <$> Time.getCurrentTime
    IterationQueryNext -> Time.addDays 7 . Time.utctDay <$> Time.getCurrentTime
    IterationQueryDate date -> pure date
    IterationQueryIteration n -> pure $ Time.addDays (max n 1 * days) inception
  pure $ getIteration days inception reference


-- | Get the iteration number based on the number of days per iteration,
-- the inception date, and the reference date.
--
-- The inception date is considered as the Monday of the week that the inception date falls in.
--
-- >>> getIteration 7 (read "2022-06-06") (read "2025-07-20")
-- 162
-- >>> getIteration 7 (read "2022-06-06") (read "2025-07-21")
-- 163
-- >>> getIteration 7 (read "2022-06-06") (read "2025-07-26")
-- 163
-- >>> getIteration 7 (read "2022-06-06") (read "2025-07-27")
-- 163
-- >>> getIteration 7 (read "2022-06-06") (read "2025-07-28")
-- 164
getIteration :: Integer -> Time.Day -> Time.Day -> Integer
getIteration days inception reference =
  let -- Get the Monday of the week that the inception date falls in:
      d1 = getMondayOfWeek inception
      -- Get the Monday of the week that the reference date falls in:
      d2 = getMondayOfWeek reference
   in -- Calculate the number of iterations based on the difference in days:
      Time.diffDays d2 d1 `div` fromIntegral days


-- | Get the Monday of the week that the given day falls in.
--
-- >>> getMondayOfWeek (read "2025-07-21")
-- 2025-07-21
-- >>> getMondayOfWeek (read "2025-07-22")
-- 2025-07-21
-- >>> getMondayOfWeek (read "2025-07-26")
-- 2025-07-21
-- >>> getMondayOfWeek (read "2025-07-27")
-- 2025-07-21
-- >>> getMondayOfWeek (read "2025-07-28")
-- 2025-07-28
-- >>> getMondayOfWeek (read "2025-07-29")
-- 2025-07-28
getMondayOfWeek :: Time.Day -> Time.Day
getMondayOfWeek =
  Time.weekFirstDay Time.Monday


-- ** Colors


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
  deriving (Eq, Show, Ord, Bounded, Enum)


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


-- ** Owner Type


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


-- * IO


ghGetRateLimitRemaining :: IO Integer
ghGetRateLimitRemaining = do
  res <- runProcessRead "gh" ["api", "rate_limit", "--jq", ".rate.remaining"]
  case res of
    Left err -> printProcessResultError "gh-api-rate-limit" err >> die "Exiting..."
    Right sv -> pure sv


getProjectData :: ConfigProject -> IO (Either ProcessResultError Project)
getProjectData (MkConfigProject owner number) = do
  let (entity, handle_) = case owner of
        OwnerUser l -> ("--user", l)
        OwnerOrganization l -> ("--org", l)
  runProcessJSON "gh-prix-project-item-list" [entity, handle_, "--project", Z.Text.tshow number]


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


-- Orphan Instances

instance ADC.HasCodec Milli where
  codec = ADC.codecViaAeson "Milli"
