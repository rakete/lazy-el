{-# OPTIONS_GHC -fglasgow-exts #-}
import DBus.Types
import DBus.Client.Simple

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Event
import Graphics.X11

import Control.Concurrent (threadDelay, forkIO, killThread, ThreadId)
import Control.Monad

import System.Exit (exitWith, ExitCode(..))
import System.IO

import Foreign.C.Types

-- testFocusEvent e@(AnyEvent { ev_event_type = et }) | et == focusIn =
--                                                        do io $ putStrLn "FocusIn"
--                                                           return $ All True
--                                                    | et == focusOut =
--                                                        do io $ putStrLn "FocusOut"
--                                                           return $ All True
--                                                    | et == createNotify =
--                                                        do io $ putStrLn "CreateNotify"
--                                                           return $ All True
-- testFocusEvent e@(CrossingEvent { ev_event_type = et }) | et == enterNotify =
--                                                             do io $ putStrLn "EnterNotify"
--                                                                return $ All True
--                                                         | et == leaveNotify =
--                                                             do io $ putStrLn "LeaveNotify"
--                                                                return $ All True
-- testFocusEvent e@(DestroyWindowEvent {}) = do
--   when ((ev_event_type e) == destroyNotify) $ io $ putStrLn "DestroyNotify"
--   return $ All True
-- testFocusEvent e@(PropertyEvent { ev_atom = a, ev_window = w }) = do
--   d <- asks display
--   root <- asks theRoot
--   c <- io $ internAtom d "_NET_CURRENT_DESKTOP" True
--   when (w == root && a == c) $ do
--     v :: (Maybe [CInt]) <- io $ rawGetWindowProperty 32 d a root
--     io $ putStrLn $ show v
--   return $ All True
-- testFocusEvent _ = return $ All True

handleEvent :: Display -> Window -> Event -> IO ()
handleEvent d w e@(PropertyEvent { ev_atom = a }) = do
    c <- internAtom d "_NET_CURRENT_DESKTOP" True
    when (a == c) $ do
      v :: (Maybe [CInt]) <- rawGetWindowProperty 32 d a w
      putStrLn $ show v
    return ()

eventLoop :: Display -> Window -> IO ()
eventLoop d w = allocaXEvent $ \e -> do
                     nextEvent d e
                     ev <- getEvent e
                     handleEvent d w ev
                     eventLoop d w

waitForQuit :: ThreadId -> IO ()
waitForQuit t = do
  c <- hGetChar stdin
  if c == 'q'
   then do
      killThread t
      return ()
   else waitForQuit t

main :: IO ()
main = do
  dpy <- openDisplay ""
  let dflt = defaultScreen dpy
      border = blackPixel dpy dflt
      background = whitePixel dpy dflt
  rootw <- rootWindow dpy dflt
  selectInput dpy rootw $ propertyChangeMask
  forkIO (eventLoop dpy rootw) >>= waitForQuit

