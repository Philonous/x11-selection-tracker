{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | An example module.
module Clipboard
  ( waitForSelection
  , withClipboard
  )
where

import           Control.Concurrent        (threadDelay)
import           Control.Monad
import qualified Control.Monad.Catch       as Ex
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import           Data.Char
import qualified Data.Foldable             as Foldable
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import qualified Data.Text.IO              as Text
import           Data.Word
import           Foreign.Ptr
import qualified Graphics.X11.Xlib         as X11
import qualified Graphics.X11.Xlib.Extras  as X11
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           System.IO

import           Language.C.Inline         as C

C.include "X11/extensions/Xfixes.h"

getNextEvent :: X11.Display -> IO X11.Event
getNextEvent display = X11.allocaXEvent $ \ev -> do
  X11.nextEvent display ev
  X11.getEvent ev


withDisplay :: String -> (X11.Display -> IO a) -> IO a
withDisplay name = Ex.bracket (X11.openDisplay name) X11.closeDisplay

withWindow :: X11.Display -> (X11.Window -> IO c) -> IO c
withWindow display = Ex.bracket createWindow destroyWindow
  where
    createWindow = X11.createSimpleWindow display (X11.defaultRootWindow display)
                                          0 0 1 1 0 0 0
    destroyWindow = X11.destroyWindow display

getClipboardString :: X11.Display -> X11.Window -> [X11.Atom] -> IO (Maybe Text)
getClipboardString display window clipboards = do
    inp <- X11.internAtom display "clipboard_get" False
    target <- X11.internAtom display "UTF8_STRING" True
    X11.xConvertSelection display (head clipboards) target inp window X11.currentTime
    clipboardInputWait display window inp

clipboardInputWait :: X11.Display -> X11.Window -> X11.Atom -> IO (Maybe Text)
clipboardInputWait display window inp = do
    ev <- getNextEvent display
    if X11.ev_event_type ev == X11.selectionNotify
        then fmap toText <$> X11.getWindowProperty8 display inp window
        else clipboardInputWait display window inp
  where
    toText = Text.decodeUtf8 . BS.pack . map (fromIntegral . fromEnum @CChar)

selectionChangedEvent :: X11.EventType
selectionChangedEvent = 87 -- TODO: use constant in C sources or something

waitForSelection :: X11.Display -> X11.Window -> X11.Atom-> IO (Maybe Text)
waitForSelection display window clipboard = do
    ev <- getNextEvent display
    if X11.ev_event_type ev == selectionChangedEvent
      then getClipboardString display window [clipboard]
      else waitForSelection display window clipboard

C.verbatim "typedef u_int64_t uint64_t;"

selectSelectionInput :: X11.Display -> X11.Window -> X11.Atom -> IO ()
selectSelectionInput (X11.Display display) win selection = do
  let disp = castPtr display
      -- w = castPtr win
  [C.block| void {
     unsigned long mask = XFixesSetSelectionOwnerNotifyMask;
     XFixesSelectSelectionInput( $(void* disp)
                               , $(uint64_t win)
                               , $(uint64_t selection)
                               , mask
                               );
     return;
    }

   |]


withClipboard :: (Text -> IO b) -> IO ()
withClipboard f = withDisplay "" $ \display ->
                  withWindow display $ \window -> do
  clipboard <- X11.internAtom display "CLIPBOARD" False
  selectSelectionInput display window clipboard
  _ <- forever $ do
    str <- waitForSelection display window clipboard
    Foldable.forM_ str f
  return ()
