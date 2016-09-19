{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module ReflexSDL where

import Reflex
import Reflex.Host.Class (newEventWithTriggerRef, runHostFrame, fireEvents)
import Control.Monad (forever)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import Data.Dependent.Sum (DSum ((:=>)))
import qualified SDL
import SDL.Event (eventPayload)

type SDLEvent t = Event t SDL.EventPayload

type SDLApp t m a = (Reflex t, MonadHold t m, MonadFix m)
                => SDLEvent t
                -> m (Behavior t a)

host :: (forall t m. SDLApp t m a) -> (a -> IO ()) -> IO ()
host guest actuate =
  -- leaving the comments from original Reflex' host example till I get enough intuition about it
  runSpiderHost $ do
    -- Create an event to be used as input.
    -- It will fire wehenver we use eTriggerRef.
    (e, eTriggerRef) <- newEventWithTriggerRef

    -- Evaluate our user's program to set up the data flow graph.
    -- This usually only needs to be done once; the user can change the data
    -- flow graph arbitrarily in response to events.
    --
    -- runHostFrame is an efficient way of running a computation that
    -- can build arbitrary data flow graphs using 'hold' and 'sample'.
    --
    -- (The pure combinators in the Reflex class can be used in any context,
    -- so they don't need any special treatment - but inside runHostFrame is
    -- as good a place as any to run them.)
    bOutput <- runHostFrame $ guest e

    -- Begin our event processing loop.
    forever $ do
      polledEv <- liftIO SDL.pollEvent
      case polledEv of
        Nothing -> return ()
        Just ev -> do
          -- Retrieve the current event trigger.
          mETrigger <- liftIO $ readIORef eTriggerRef
          -- Use the trigger to deliver the event.
          case mETrigger of
            Nothing -> return ()
            Just eTrigger -> do
              let payload = eventPayload ev
              fireEvents [eTrigger :=> Identity payload]

          -- Retrieve the current output of the user's program and display it.
          output <- runHostFrame $ sample bOutput
          -- output it
          liftIO $ actuate output
