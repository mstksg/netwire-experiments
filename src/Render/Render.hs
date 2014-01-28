module Render.Render where

import Control.Wire

newtype Backend s e m r a = Backend {
    runBackend ::
         (a -> r)                           -- render function
      -> Wire s e m (Event RenderEvent) a   -- wire
      -> IO ()
  }

data RenderEvent = RenderKeyDown RenderKeyData
                 | RenderKeyUp RenderKeyData
                 | RenderMouseDown (Int,Int) RenderMouseButton
                 | RenderMouseUp (Int,Int) RenderMouseButton
                 | RenderQuit
                 | RenderNullEvent
                 | RenderUnknownEvent

data RenderKeyData = RenderKeyData { renderKeyDataKey :: Int
                                   , renderKeyDataModifiers :: [RenderKeyModifier]
                                   }

data RenderKeyModifier = RenderKeyShift
                       | RenderKeyAlt
                       | RenderKeyCtrl

data RenderMouseButton = RenderMouseLeft
                       | RenderMouseMiddle
                       | RenderMouseRight
                       | RenderMouseWheelUp
                       | RenderMouseWheelDown
                       | RenderMouseButtonUnknown
