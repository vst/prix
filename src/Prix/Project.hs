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
import qualified Data.Aeson as Aeson
import Data.Fixed (Milli)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Time as Time
import GHC.Generics (Generic)
import qualified Options.Applicative as OA
import qualified Prix.Commons as Commons
import Prix.Config (ConfigProject (..), Owner (..))
import qualified Zamazingo.Text as Z.Text


-- * Project


data Project = MkProject
  { projectOwner :: !Commons.Owner
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
  , projectItemDeadline :: !(Maybe Time.Day)
  , projectItemImpact :: !(Maybe ProjectItemImpact)
  , projectItemScope :: !(Maybe ProjectItemScope)
  , projectItemSeverity :: !(Maybe ProjectItemSeverity)
  , projectItemRisk :: !(Maybe ProjectItemRisk)
  , projectItemFootprint :: !(Maybe ProjectItemFootprint)
  , projectItemComplexity :: !(Maybe ProjectItemComplexity)
  , projectItemConfidence :: !(Maybe ProjectItemConfidence)
  , projectItemScore :: !(Maybe Milli)
  , projectItemTitle :: !T.Text
  , projectItemCreatedAt :: !Time.UTCTime
  , projectItemAssignees :: !(Maybe (NE.NonEmpty T.Text))
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
        <*> ADC.requiredField "deadline" "Deadline" ADC..= projectItemDeadline
        <*> ADC.requiredField "impact" "Impact Level" ADC..= projectItemImpact
        <*> ADC.requiredField "scope" "Scope Level" ADC..= projectItemScope
        <*> ADC.requiredField "severity" "Severity Level" ADC..= projectItemSeverity
        <*> ADC.requiredField "risk" "Risk Level" ADC..= projectItemRisk
        <*> ADC.requiredField "footprint" "Footprint Level" ADC..= projectItemFootprint
        <*> ADC.requiredField "complexity" "Complexity Level" ADC..= projectItemComplexity
        <*> ADC.requiredField "confidence" "Confidence Level" ADC..= projectItemConfidence
        <*> ADC.requiredField "score" "Priority Score" ADC..= projectItemScore
        <*> ADC.requiredField "title" "Item Title" ADC..= projectItemTitle
        <*> ADC.requiredField "createdAt" "Creation Time" ADC..= projectItemCreatedAt
        <*> ADC.requiredField "assignees" "Assignee Logins" ADC..= projectItemAssignees
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


projectItemStatusColor :: ProjectItemStatus -> Commons.OptionColor
projectItemStatusColor ProjectItemStatusInbox = Commons.OptionColorGray
projectItemStatusColor ProjectItemStatusTriage = Commons.OptionColorBlue
projectItemStatusColor ProjectItemStatusBacklog = Commons.OptionColorGreen
projectItemStatusColor ProjectItemStatusPlanned = Commons.OptionColorYellow
projectItemStatusColor ProjectItemStatusActive = Commons.OptionColorRed
projectItemStatusColor ProjectItemStatusDone = Commons.OptionColorPurple


projectItemStatusEmoji :: ProjectItemStatus -> T.Text
projectItemStatusEmoji =
  Commons.optionColorEmoji . projectItemStatusColor


projectItemStatusColorLabel :: ProjectItemStatus -> T.Text
projectItemStatusColorLabel s =
  Commons.optionColorEmoji (projectItemStatusColor s) <> " " <> projectItemStatusLabel s


-- *** Impact


-- | The impact of an issue, which indicates how much value it brings to the business or project.
--
-- >>> minBound :: ProjectItemImpact
-- ProjectItemImpactLow
-- >>> maxBound :: ProjectItemImpact
-- ProjectItemImpactHigh
-- >>> [ProjectItemImpactLow .. ProjectItemImpactHigh]
-- [ProjectItemImpactLow,ProjectItemImpactMedium,ProjectItemImpactHigh]
data ProjectItemImpact
  = -- | Nice-to-have or speculative (e.g., minor enhancement, research)
    ProjectItemImpactLow
  | -- | Operationally useful (e.g., performance improvement, bug fix)
    ProjectItemImpactMedium
  | -- | Supports a major initiative (e.g., new feature, platform upgrade)
    ProjectItemImpactHigh
  deriving (Eq, Show, Ord, Bounded, Enum)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectItemImpact)


instance ADC.HasCodec ProjectItemImpact where
  codec = ADC.boundedEnumCodec projectItemImpactLabel


projectItemImpactLabel :: ProjectItemImpact -> T.Text
projectItemImpactLabel ProjectItemImpactLow = "low"
projectItemImpactLabel ProjectItemImpactMedium = "medium"
projectItemImpactLabel ProjectItemImpactHigh = "high"


-- *** Scope


-- | The scope of an issue, which indicates how broadly the work crosses system
-- or organizational boundaries.
--
-- >>> minBound :: ProjectItemScope
-- ProjectItemScopeIsolated
-- >>> maxBound :: ProjectItemScope
-- ProjectItemScopeGlobal
-- >>> [ProjectItemScopeIsolated .. ProjectItemScopeGlobal]
-- [ProjectItemScopeIsolated,ProjectItemScopeConnected,ProjectItemScopeGlobal]
data ProjectItemScope
  = ProjectItemScopeIsolated
  | ProjectItemScopeConnected
  | ProjectItemScopeGlobal
  deriving (Eq, Show, Ord, Bounded, Enum)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectItemScope)


instance ADC.HasCodec ProjectItemScope where
  codec = ADC.boundedEnumCodec projectItemScopeLabel


projectItemScopeLabel :: ProjectItemScope -> T.Text
projectItemScopeLabel ProjectItemScopeIsolated = "isolated"
projectItemScopeLabel ProjectItemScopeConnected = "connected"
projectItemScopeLabel ProjectItemScopeGlobal = "global"


-- *** Severity


data ProjectItemSeverity
  = ProjectItemSeverityLow
  | ProjectItemSeverityMedium
  | ProjectItemSeverityHigh
  deriving (Eq, Show, Ord, Bounded, Enum)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectItemSeverity)


instance ADC.HasCodec ProjectItemSeverity where
  codec = ADC.boundedEnumCodec projectItemSeverityLabel


projectItemSeverityLabel :: ProjectItemSeverity -> T.Text
projectItemSeverityLabel ProjectItemSeverityLow = "low"
projectItemSeverityLabel ProjectItemSeverityMedium = "medium"
projectItemSeverityLabel ProjectItemSeverityHigh = "high"


-- *** Risk


data ProjectItemRisk
  = ProjectItemRiskLow
  | ProjectItemRiskMedium
  | ProjectItemRiskHigh
  deriving (Eq, Show, Ord, Bounded, Enum)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectItemRisk)


instance ADC.HasCodec ProjectItemRisk where
  codec = ADC.boundedEnumCodec projectItemRiskLabel


projectItemRiskLabel :: ProjectItemRisk -> T.Text
projectItemRiskLabel ProjectItemRiskLow = "low"
projectItemRiskLabel ProjectItemRiskMedium = "medium"
projectItemRiskLabel ProjectItemRiskHigh = "high"


-- *** Confidence


-- | Confidence in the value of an issue, which indicates how certain we are about the value it will bring.
--
-- >>> minBound :: ProjectItemConfidence
-- ProjectItemConfidenceLow
-- >>> maxBound :: ProjectItemConfidence
-- ProjectItemConfidenceHigh
-- >>> [ProjectItemConfidenceLow .. ProjectItemConfidenceHigh]
-- [ProjectItemConfidenceLow,ProjectItemConfidenceMedium,ProjectItemConfidenceHigh]
data ProjectItemConfidence
  = -- | Little evidence, mostly speculative or intuition-based
    ProjectItemConfidenceLow
  | -- | Some evidence or assumptions, with moderate uncertainty
    ProjectItemConfidenceMedium
  | -- | Strong data, direct user feedback, or well-supported experience
    ProjectItemConfidenceHigh
  deriving (Eq, Show, Ord, Bounded, Enum)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectItemConfidence)


instance ADC.HasCodec ProjectItemConfidence where
  codec = ADC.boundedEnumCodec projectItemConfidenceLabel


projectItemConfidenceLabel :: ProjectItemConfidence -> T.Text
projectItemConfidenceLabel ProjectItemConfidenceLow = "low"
projectItemConfidenceLabel ProjectItemConfidenceMedium = "medium"
projectItemConfidenceLabel ProjectItemConfidenceHigh = "high"


-- *** Footprint


-- | The footprint of a deliverable, which indicates how much work surface area
-- is involved in producing or changing it.
--
-- >>> minBound :: ProjectItemFootprint
-- ProjectItemFootprintSmall
-- >>> maxBound :: ProjectItemFootprint
-- ProjectItemFootprintLarge
-- >>> [ProjectItemFootprintSmall .. ProjectItemFootprintLarge]
-- [ProjectItemFootprintSmall,ProjectItemFootprintMedium,ProjectItemFootprintLarge]
data ProjectItemFootprint
  = ProjectItemFootprintSmall
  | -- | A cohesive unit across a few components or artifacts
    ProjectItemFootprintMedium
  | -- | Cross-cutting change or multi-system deliverable
    ProjectItemFootprintLarge
  deriving (Eq, Show, Ord, Bounded, Enum)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectItemFootprint)


instance ADC.HasCodec ProjectItemFootprint where
  codec = ADC.boundedEnumCodec projectItemFootprintLabel


projectItemFootprintLabel :: ProjectItemFootprint -> T.Text
projectItemFootprintLabel ProjectItemFootprintSmall = "small"
projectItemFootprintLabel ProjectItemFootprintMedium = "medium"
projectItemFootprintLabel ProjectItemFootprintLarge = "large"


-- *** Complexity


-- | The complexity of a task, which indicates how hard it is to reason about or execute.
--
-- >>> minBound :: ProjectItemComplexity
-- ProjectItemComplexityLow
-- >>> maxBound :: ProjectItemComplexity
-- ProjectItemComplexityHigh
-- >>> [ProjectItemComplexityLow .. ProjectItemComplexityHigh]
-- [ProjectItemComplexityLow,ProjectItemComplexityMedium,ProjectItemComplexityHigh]
data ProjectItemComplexity
  = ProjectItemComplexityLow
  | -- | Requires judgment, abstraction, or coordination
    ProjectItemComplexityMedium
  | -- | Conceptually complex, uncertain, or high-risk
    ProjectItemComplexityHigh
  deriving (Eq, Show, Ord, Bounded, Enum)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ProjectItemComplexity)


instance ADC.HasCodec ProjectItemComplexity where
  codec = ADC.boundedEnumCodec projectItemComplexityLabel


projectItemComplexityLabel :: ProjectItemComplexity -> T.Text
projectItemComplexityLabel ProjectItemComplexityLow = "low"
projectItemComplexityLabel ProjectItemComplexityMedium = "medium"
projectItemComplexityLabel ProjectItemComplexityHigh = "high"


-- *** Effort


-- | The effort estimate for an issue.
--
-- \[
-- \mbox{Effort Estimate} = 2^{(\mbox{Footprint} + \mbox{Complexity})}
-- \]
--
-- __Quick Reference Table:__
--
-- +------------------------+---------+------------+----------+
-- | Footprint Complexity   | low (0) | medium (1) | high (2) |
-- +========================+=========+============+==========+
-- | small (0)              | 1       | 2          | 4        |
-- +------------------------+---------+------------+----------+
-- | medium (1)             | 2       | 4          | 8        |
-- +------------------------+---------+------------+----------+
-- | large (2)              | 4       | 8          | 16       |
-- +------------------------+---------+------------+----------+
--
-- >>> projectItemEffortEstimate ProjectItemFootprintSmall ProjectItemComplexityLow
-- 1
-- >>> projectItemEffortEstimate ProjectItemFootprintLarge ProjectItemComplexityHigh
-- 16
projectItemEffortEstimate :: ProjectItemFootprint -> ProjectItemComplexity -> Integer
projectItemEffortEstimate footprint complexity =
  let footprintFactor = projectItemFootprintFactor footprint
      complexityFactor = projectItemComplexityFactor complexity
   in 2 ^ (footprintFactor + complexityFactor)


-- *** Score


-- | Provisional project item score.
--
-- This formula is intentionally provisional and subject to calibration as the
-- new field ontology is used in practice.
--
-- \[
-- \mbox{Score} = \frac{((\mbox{Impact} \times \mbox{Scope}) + \mbox{Severity} + \mbox{Risk}) \times \mbox{Confidence}}{\mbox{Effort}}
-- \]
--
-- The lowest score can be:
--
-- >>> projectItemScoreEstimate ProjectItemImpactLow ProjectItemScopeIsolated ProjectItemSeverityLow ProjectItemRiskLow ProjectItemConfidenceLow ProjectItemFootprintLarge ProjectItemComplexityHigh
-- 0.018
--
-- The highest score can be:
--
-- >>> projectItemScoreEstimate ProjectItemImpactHigh ProjectItemScopeGlobal ProjectItemSeverityHigh ProjectItemRiskHigh ProjectItemConfidenceHigh ProjectItemFootprintSmall ProjectItemComplexityLow
-- 13.000
projectItemScoreEstimate
  :: ProjectItemImpact
  -> ProjectItemScope
  -> ProjectItemSeverity
  -> ProjectItemRisk
  -> ProjectItemConfidence
  -> ProjectItemFootprint
  -> ProjectItemComplexity
  -> Milli
projectItemScoreEstimate impact scope severity risk confidence footprint complexity =
  let impactFactor = projectItemImpactFactor impact
      scopeFactor = projectItemScopeFactor scope
      severityFactor = projectItemSeverityFactor severity
      riskFactor = projectItemRiskFactor risk
      confidenceFactor = projectItemConfidenceFactor confidence
      effortEstimate = projectItemEffortEstimate footprint complexity
      numerator = ((impactFactor * scopeFactor) + severityFactor + riskFactor) * confidenceFactor
   in if effortEstimate == 0
        then 0.0
        else numerator / fromIntegral effortEstimate


projectItemImpactFactor :: ProjectItemImpact -> Milli
projectItemImpactFactor ProjectItemImpactLow = 1
projectItemImpactFactor ProjectItemImpactMedium = 2
projectItemImpactFactor ProjectItemImpactHigh = 3


projectItemScopeFactor :: ProjectItemScope -> Milli
projectItemScopeFactor ProjectItemScopeIsolated = 1
projectItemScopeFactor ProjectItemScopeConnected = 2
projectItemScopeFactor ProjectItemScopeGlobal = 3


projectItemSeverityFactor :: ProjectItemSeverity -> Milli
projectItemSeverityFactor ProjectItemSeverityLow = 0
projectItemSeverityFactor ProjectItemSeverityMedium = 1
projectItemSeverityFactor ProjectItemSeverityHigh = 2


projectItemRiskFactor :: ProjectItemRisk -> Milli
projectItemRiskFactor ProjectItemRiskLow = 0
projectItemRiskFactor ProjectItemRiskMedium = 1
projectItemRiskFactor ProjectItemRiskHigh = 2


projectItemConfidenceFactor :: ProjectItemConfidence -> Milli
projectItemConfidenceFactor ProjectItemConfidenceLow = 0.3
projectItemConfidenceFactor ProjectItemConfidenceMedium = 0.6
projectItemConfidenceFactor ProjectItemConfidenceHigh = 1.0


projectItemFootprintFactor :: ProjectItemFootprint -> Integer
projectItemFootprintFactor ProjectItemFootprintSmall = 0
projectItemFootprintFactor ProjectItemFootprintMedium = 1
projectItemFootprintFactor ProjectItemFootprintLarge = 2


projectItemComplexityFactor :: ProjectItemComplexity -> Integer
projectItemComplexityFactor ProjectItemComplexityLow = 0
projectItemComplexityFactor ProjectItemComplexityMedium = 1
projectItemComplexityFactor ProjectItemComplexityHigh = 2


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


-- * IO


getProjectData :: ConfigProject -> IO (Either Commons.ProcessResultError Project)
getProjectData (MkConfigProject owner number) = do
  let (entity, handle_) = case owner of
        OwnerUser l -> ("--user", l)
        OwnerOrganization l -> ("--org", l)
  Commons.runProcessJSON "gh-prix-project-item-list" [entity, handle_, "--project", Z.Text.tshow number]


-- Orphan Instances

instance ADC.HasCodec Milli where
  codec = ADC.codecViaAeson "Milli"
