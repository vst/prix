{-# LANGUAGE OverloadedStrings #-}

-- | This module provides top-level definitions for the CLI program.
module Prix.Cli where

import Control.Applicative ((<**>))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Options.Applicative as OA
import qualified Path.IO as PIO
import Prix.Config (Config (..), ConfigProjects (..), readConfig, readConfigFromFile)
import qualified Prix.Meta as Meta
import Prix.Project (IterationQuery, iterationQueryParser, queryIteration)
import System.Exit (ExitCode (..), die)


-- * Entrypoint


-- | CLI program entrypoint.
cli :: IO ExitCode
cli =
  OA.execParser (OA.info opts desc) >>= runOptions
  where
    opts = optionsParser <**> infoOptVersion <**> OA.helper
    desc =
      OA.fullDesc
        <> OA.progDesc "Top Level Commands"
        <> infoModHeader
        <> infoModFooter


-- * Options and Commands


-- | CLI options and commands.
data Options = MkOptions
  { optionsConfig :: !(Maybe FilePath)
  , optionsCommand :: !Command
  }
  deriving (Show, Eq)


optionsParser :: OA.Parser Options
optionsParser =
  MkOptions
    <$> OA.optional (OA.strOption (OA.long "config" <> OA.short 'c' <> OA.metavar "FILE" <> OA.help "Path to configuration file"))
    <*> commandsParser


-- | CLI commands.
data Command
  = CommandVersion Bool
  | CommandProject !ProjectCommand
  deriving (Show, Eq)


commandsParser :: OA.Parser Command
commandsParser =
  OA.hsubparser
    ( OA.command "version" (OA.info versionParser infoModVersion)
        <> OA.command "project" (OA.info projectCommandParser infoModProject)
    )
  where
    infoModVersion = OA.fullDesc <> infoModHeader <> OA.progDesc "Show version and build information."
    infoModProject = OA.fullDesc <> infoModHeader <> OA.progDesc "Project management commands."


versionParser :: OA.Parser Command
versionParser =
  CommandVersion <$> OA.switch (OA.short 'j' <> OA.long "json" <> OA.help "Format output in JSON.")


-- | CLI commands for project management.
newtype ProjectCommand
  = ProjectCommandIter IterationQuery
  deriving (Show, Eq)


projectCommandParser :: OA.Parser Command
projectCommandParser =
  CommandProject
    <$> OA.hsubparser
      ( OA.command "iter" (OA.info projectIterParser infoModProjectIter)
      )
  where
    infoModProjectIter = OA.fullDesc <> infoModHeader <> OA.progDesc "Project iteration commands."


projectIterParser :: OA.Parser ProjectCommand
projectIterParser =
  ProjectCommandIter
    <$> iterationQueryParser


-- * Interpreter


runOptions :: Options -> IO ExitCode
runOptions (MkOptions mCfgPath cmd) =
  case cmd of
    CommandVersion json -> doVersion json
    CommandProject pcmd -> _readConfig >>= (`runCommandProject` pcmd)
  where
    _readConfig = readCliConfig mCfgPath


runCommandProject :: Config -> ProjectCommand -> IO ExitCode
runCommandProject cfg (ProjectCommandIter q) = doProjectIter cfg q


-- * Performance


-- | @project iter@ CLI command program.
doProjectIter :: Config -> IterationQuery -> IO ExitCode
doProjectIter cfg q = do
  let inception = configProjectsInception . configProjects $ cfg
  date <- queryIteration 7 inception q
  print date
  pure ExitSuccess


-- | @version@ CLI command program.
doVersion :: Bool -> IO ExitCode
doVersion True = BLC.putStrLn (Aeson.encode Meta.buildInfo) >> pure ExitSuccess
doVersion False = TIO.putStrLn (Meta.prettyBuildInfo Meta.buildInfo) >> pure ExitSuccess


-- * Helpers


-- | Version option parser.
infoOptVersion :: OA.Parser (a -> a)
infoOptVersion =
  OA.infoOption Meta.versionString $
    OA.short 'v'
      <> OA.long "version"
      <> OA.help "Show application version and exit"


-- | Header 'OA.InfoMod'.
infoModHeader :: OA.InfoMod a
infoModHeader =
  OA.header (T.unpack (Meta.name <> " - " <> Meta.title <> " v" <> Meta.versionText))


-- | Footer 'OA.InfoMod'.
infoModFooter :: OA.InfoMod a
infoModFooter =
  OA.footer "See <https://github.com/vst/prix> for help and feedback."


-- | Tests a parser with given arguments.
runParserTest :: OA.Parser a -> [String] -> OA.ParserResult a
runParserTest parser =
  OA.execParserPure (OA.prefs prefs) (OA.info (parser <**> OA.helper) infomod)
  where
    prefs = OA.showHelpOnError <> OA.helpLongEquals <> OA.helpShowGlobals
    infomod = OA.fullDesc <> OA.progDesc "Test Parser" <> OA.header "testparser - especially for doctests"


-- | Tests an IO parser with given arguments.
runParserTestIO :: OA.Parser (IO a) -> [String] -> IO (Either String ())
runParserTestIO p as =
  case runParserTest p as of
    OA.Success _ -> pure (Right ())
    OA.Failure f -> pure (Left (show f))
    OA.CompletionInvoked _ -> pure (Right ())


-- | Attempts to read the configuration file from the default location or from a specified path.
readCliConfig :: Maybe FilePath -> IO Config
readCliConfig mCfgPath =
  case mCfgPath of
    Nothing -> do
      eCfg <- readConfig
      case eCfg of
        Left err -> die $ "Failed to read configuration: " <> err
        Right cfg -> pure cfg
    Just cfgPath -> do
      path <- PIO.resolveFile' cfgPath
      eCfg <- readConfigFromFile path
      case eCfg of
        Left err -> die $ "Failed to read configuration from file: " <> err
        Right cfg -> pure cfg
