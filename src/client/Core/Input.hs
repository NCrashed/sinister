{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module Core.Input(
    MouseButton(..)
  , KeyState(..)
  , Key(..)
  , SpecialKey(..)
  , Modifiers(..)
  , InputEvent(..)
  , isConnectEvent
  , isDisconnectEvent
  , isConnectionError
  , isKeyEvent
  , isMouseMoveEvent
  , ToKey(..)
  , ToKeyState(..)
  , ToModifiers(..)
  , ToSpecialKey(..)
  , ToMouseButton(..)
  ) where 

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.DeriveTH

import Data.Text (Text)

-- | Describes mouse button
data MouseButton = 
    LeftButton
  | MiddleButton
  | RightButton
  | WheelUp
  | WheelDown
  | AdditionalButton !Int 
  deriving (Generic, Eq, Show, Ord)

instance NFData MouseButton

-- | Current state of key/button
data KeyState = Up | Down | Repeating
  deriving (Generic, Eq, Show, Ord)

instance NFData KeyState

-- | Any key
data Key = 
    CharKey !Char 
  | SpecialKey !SpecialKey 
  | MouseButton !MouseButton
  deriving (Generic, Eq, Show, Ord)

instance NFData Key

-- | Special keys on keyboard
data SpecialKey = 
    KeyF1
  | KeyF2
  | KeyF3
  | KeyF4
  | KeyF5
  | KeyF6
  | KeyF7
  | KeyF8
  | KeyF9
  | KeyF10
  | KeyF11
  | KeyF12
  | KeyF13
  | KeyF14
  | KeyF15
  | KeyF16
  | KeyF17
  | KeyF18
  | KeyF19
  | KeyF20
  | KeyF21
  | KeyF22
  | KeyF23
  | KeyF24
  | KeyF25
  | KeyLeft
  | KeyUp
  | KeyRight
  | KeyDown
  | KeyPageUp
  | KeyPageDown
  | KeyHome
  | KeyEnd 
  | KeyInsert
  | KeyNumLock
  | KeyBegin
  | KeyDelete
  | KeyShiftL
  | KeyShiftR
  | KeyCtrlL
  | KeyCtrlR
  | KeyAltL
  | KeyAltR
  | KeyWorld1
  | KeyWorld2
  | KeyEscape
  | KeyEnter
  | KeyTab 
  | KeyBackspace
  | KeyCapsLock
  | KeyScrollLock
  | KeyPrintScreen
  | KeyPause
  | KeyPad0
  | KeyPad1
  | KeyPad2
  | KeyPad3
  | KeyPad4
  | KeyPad5
  | KeyPad6
  | KeyPad7
  | KeyPad8
  | KeyPad9
  | KeyPadDecimal
  | KeyPadDivide
  | KeyPadMultiply
  | KeyPadSubtract
  | KeyPadAdd 
  | KeyPadEnter
  | KeyPadEqual
  | KeySuperL
  | KeySuperR
  | KeyMenu
  | KeyUnknown !Int
  deriving (Generic, Eq, Show, Ord)

instance NFData SpecialKey

data Modifiers = Modifiers {
    shift :: !KeyState
  , ctrl :: !KeyState
  , alt :: !KeyState
  , super :: !KeyState
  } deriving (Generic, Eq, Ord, Show)

instance NFData Modifiers

-- | Events that is inputs for client simulation
data InputEvent = 
    ConnectEvent -- ^ Connection with server is established and player logged in
  | DisconnectEvent !Text -- ^ When error occured and server drops connection (or player disconnect by himself)
  | ConnectionError !Text -- ^ When error occured before logging is finished
  | KeyEvent !Double !Double !Key !KeyState !Modifiers -- ^ When user click/press/release key in window, coordinates from 0 to 1, left upper corner is origin
  | MouseMoveEvent !Double !Double -- ^ Holds current mouse coordinates from 0 to 1, left upper corner is origin
  deriving (Generic, Eq, Show)

$(derive makeIs ''InputEvent)

instance NFData InputEvent where

class ToKey a where 
  toKey :: a -> Key 

class ToKeyState a where
  toKeyState :: a -> KeyState 

class ToModifiers a where 
  toModifiers :: a -> Modifiers

class ToSpecialKey a where 
  toSpecialKey :: a -> SpecialKey 

class ToMouseButton a where 
  toMouseButton :: a -> MouseButton