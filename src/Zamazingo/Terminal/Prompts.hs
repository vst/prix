{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Zamazingo.Terminal.Prompts where

import Control.Exception (bracket_)
import Control.Monad (replicateM_, when)
import Data.Foldable (for_)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Console.ANSI as Ansi
import System.Environment
import System.Exit
import System.IO
import qualified System.IO.Temp as Temp
import qualified System.Process.Typed as TP


-- * Single-Line Text Prompt


text :: T.Text -> Maybe T.Text -> IO (Maybe T.Text)
text label mDef = withPromptMode $ do
  allocateArea 2
  bracket_ (pure ()) (cleanup 1 >> putStrLn "") loop
  where
    shownDefault = fromMaybe "" mDef

    loop = do
      Ansi.restoreCursor
      Ansi.clearLine
      renderPromptLine label shownDefault
      putStr " "
      hFlush stdout

      input <- gatherInput []
      case input of
        Nothing -> do
          Ansi.restoreCursor
          Ansi.clearLine
          renderPromptLine label "— cancelled —"
          putStrLn ""
          hFlush stdout
          pure Nothing
        Just txt ->
          if T.null txt
            then pure mDef
            else pure (Just txt)

    gatherInput acc = do
      c <- getChar
      case c of
        '\n' -> do
          putStrLn ""
          pure (Just (T.pack (reverse acc)))
        '\r' -> do
          putStrLn ""
          pure (Just (T.pack (reverse acc)))
        '\ESC' -> pure Nothing
        '\DEL' ->
          backspace acc
        '\BS' ->
          backspace acc
        ch -> do
          putChar ch
          hFlush stdout
          gatherInput (ch : acc)

    backspace [] = gatherInput []
    backspace (_ : xs) = do
      putStr "\b \b"
      hFlush stdout
      gatherInput xs


-- * Multi-Line Text Prompt


-- | This opens the editor defined by the `EDITOR` environment variable,
-- allowing the user to input.
multilineText :: T.Text -> Maybe T.Text -> IO (Maybe T.Text)
multilineText label mDef = withPromptMode $ do
  allocateArea 2
  bracket_ (pure ()) (cleanup 1 >> putStrLn "") loop
  where
    shownValue =
      case mDef of
        Just t | not (T.null t) -> "<existing text>"
        _ -> ""

    hint = "[enter to skip, 'e' to edit]"

    loop = do
      Ansi.restoreCursor
      Ansi.clearLine
      renderPromptLine label shownValue
      putStr " "
      renderText styDimmed hint
      putStrLn ""
      hFlush stdout

      readKey >>= \case
        KeyEnter -> pure mDef
        KeyChar 'e' -> do
          cleanup 1
          editBody mDef
        KeyChar 'E' -> do
          cleanup 1
          editBody mDef
        KeyEsc -> do
          Ansi.restoreCursor
          Ansi.clearLine
          renderPromptLine label "— cancelled —"
          putStrLn ""
          hFlush stdout
          pure Nothing
        _ -> loop


-- * Choose Prompt


-- | Displays a prompt with a list of options, allowing the user to navigate and select one.
choose :: Eq a => T.Text -> (a -> T.Text) -> Maybe a -> [a] -> IO (Maybe a)
choose _ _ _ [] = pure Nothing
choose label asText mDef items = withPromptMode $ do
  allocateArea heightPane
  let initialSelected = case mDef of
        Just def -> fromMaybe 0 (List.elemIndex def items)
        Nothing -> 0
  bracket_ (pure ()) (cleanup heightMenu >> putStrLn "") (loop initialSelected)
  where
    itemCount = length items
    heightMenu = 1 + itemCount
    heightPane = 1 + heightMenu

    loop selected = do
      redraw selected
      readKey >>= \case
        KeyUp -> loop ((selected - 1) `mod` itemCount)
        KeyDown -> loop ((selected + 1) `mod` itemCount)
        KeyChar 'k' -> loop ((selected - 1) `mod` itemCount)
        KeyChar 'j' -> loop ((selected + 1) `mod` itemCount)
        KeyEnter -> pure (Just (items !! selected))
        KeyEsc -> cancel
        KeyChar 'q' -> cancel
        _ -> loop selected

    redraw selected = do
      Ansi.restoreCursor
      Ansi.clearLine
      renderPromptLine label (asText (items !! selected))
      putStrLn ""
      renderPromptHint "Use ↑/↓ or j/k, Enter to confirm, q/Esc to cancel"
      Ansi.cursorDownLine 1
      for_ (zip [0 :: Int ..] items) $ \(i, item) -> do
        Ansi.clearLine
        renderChooseItem (i == selected) (asText item)
        when (i < itemCount - 1) (Ansi.cursorDownLine 1)
      Ansi.restoreCursor
      hFlush stdout

    cancel = do
      Ansi.restoreCursor
      Ansi.clearLine
      renderPromptLine label "— cancelled —"
      putStrLn ""
      renderPromptHint "Use ↑/↓ or j/k, Enter to confirm, q/Esc to cancel"
      hFlush stdout
      pure Nothing


-- ** Helpers


-- | Renders a single option, highlighting the currently selected one.
renderChooseItem :: Bool -> T.Text -> IO ()
renderChooseItem isSelected label = do
  renderOptionPrefix '○' '◉' isSelected
  renderText sty label
  where
    sty = if isSelected then stySelect else styNormal


-- ** Examples


-- | Example usage of the "choose" prompt with a list of text options.
exampleChoose1 :: IO ()
exampleChoose1 = do
  let fruits = ["apple", "banana", "cherry", "date"]
  result <- choose "Pick a fruit: " id (Just "cherry") fruits
  putStrLn ""
  case result of
    Just fruit -> putStrLn $ "You chose: " <> T.unpack fruit
    Nothing -> putStrLn "You cancelled the prompt."


-- | Example usage of the "choose" prompt with a list of arbitrary options,
-- demonstrating how to use a custom label function.
exampleChoose2 :: IO ()
exampleChoose2 = do
  let options = [(True, "Yes"), (False, "No")]
  result <- choose "Do you want to continue? " snd Nothing options
  putStrLn ""
  case result of
    Just (True, _) -> putStrLn "You chose Yes!"
    Just (False, _) -> putStrLn "You chose No!"
    Nothing -> putStrLn "You cancelled the prompt."


-- * Commons


-- ** Rendering


-- | Renders the prefix for an option, showing different markers for the
-- selected/unselected one.
renderOptionPrefix :: Char -> Char -> Bool -> IO ()
renderOptionPrefix c _ False = putStr (c : " ")
renderOptionPrefix _ c True = renderStyle stySelect >> putStr (c : " ") >> renderReset


-- | Renders the prompt label and the currently selected value.
renderPromptLine :: T.Text -> T.Text -> IO ()
renderPromptLine label value = do
  renderStyle styPrompt
  TIO.putStr label
  renderReset
  putChar ' '
  renderText stySelect value
  renderReset


-- | Renders the hint text in a dimmed style.
renderPromptHint :: T.Text -> IO ()
renderPromptHint hint = do
  renderStyle styDimmed
  TIO.putStr hint
  renderReset


-- | Renders the label of an option with the given style.
renderText :: [Ansi.SGR] -> T.Text -> IO ()
renderText s l = do
  renderStyle s
  TIO.putStr l
  renderReset


-- | Renders the given style.
renderStyle :: [Ansi.SGR] -> IO ()
renderStyle =
  Ansi.setSGR


-- | Resets the terminal style to default.
renderReset :: IO ()
renderReset =
  Ansi.setSGR styReset


-- ** Styles


-- | Style for the prompt label.
styPrompt :: [Ansi.SGR]
styPrompt =
  [ Ansi.SetConsoleIntensity Ansi.BoldIntensity
  , Ansi.SetColor Ansi.Foreground Ansi.Vivid Ansi.Blue
  ]


-- | Style for the selected option.
stySelect :: [Ansi.SGR]
stySelect =
  [ Ansi.SetConsoleIntensity Ansi.BoldIntensity
  , Ansi.SetColor Ansi.Foreground Ansi.Vivid Ansi.Green
  ]


-- | Style for unselected options.
styNormal :: [Ansi.SGR]
styNormal =
  [ Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.White
  ]


-- | Style for the hint text.
styDimmed :: [Ansi.SGR]
styDimmed =
  [ Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Black
  ]


-- | Style to reset all attributes.
styReset :: [Ansi.SGR]
styReset =
  [ Ansi.Reset
  ]


-- ** Events


-- | Representation of key events for the "choose" prompt.
data Key
  = KeyUp
  | KeyDown
  | KeyEnter
  | KeyEsc
  | KeyChar Char
  deriving (Eq, Show)


-- | Reads a key event from the terminal, handling escape sequences for arrow keys.
readKey :: IO Key
readKey = do
  c1 <- getChar
  case c1 of
    '\ESC' -> do
      more <- hReady stdin
      if not more
        then pure KeyEsc
        else do
          c2 <- getChar
          case c2 of
            '[' -> do
              c3 <- getChar
              pure $ case c3 of
                'A' -> KeyUp
                'B' -> KeyDown
                _ -> KeyEsc
            _ ->
              pure KeyEsc
    '\n' -> pure KeyEnter
    '\r' -> pure KeyEnter
    c -> pure (KeyChar c)


-- ** Controls


withPromptMode :: IO a -> IO a
withPromptMode =
  bracket_ setup teardown
  where
    setup = do
      hSetBuffering stdin NoBuffering
      hSetBuffering stdout NoBuffering
      hSetEcho stdin False
      Ansi.hideCursor
    teardown = do
      Ansi.setSGR [Ansi.Reset]
      Ansi.showCursor
      hSetEcho stdin True
      hSetBuffering stdin LineBuffering
      hSetBuffering stdout LineBuffering


allocateArea :: Int -> IO ()
allocateArea height = do
  reserveArea height
  Ansi.cursorUpLine height
  Ansi.saveCursor


reserveArea :: Int -> IO ()
reserveArea n = do
  replicateM_ n (putStrLn "")
  hFlush stdout


cleanup :: Int -> IO ()
cleanup height = do
  Ansi.restoreCursor
  Ansi.cursorDownLine 1
  clearPromptInner height
  Ansi.restoreCursor
  Ansi.setSGR [Ansi.Reset]
  hFlush stdout


clearPromptInner :: Int -> IO ()
clearPromptInner n = do
  for_ [0 .. n - 1] $ \i -> do
    Ansi.clearLine
    when (i < n - 1) (Ansi.cursorDownLine 1)
  hFlush stdout


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
