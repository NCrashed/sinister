{-# OPTIONS_GHC -fno-warn-orphans #-}
module Core.InputGLFW() where 

import Core.Input 
import qualified Graphics.UI.GLFW as GLFW

instance ToKey GLFW.Key where 
  toKey k = case k of 
    GLFW.Key'Space -> CharKey ' '
    GLFW.Key'Apostrophe -> CharKey '\''
    GLFW.Key'Comma -> CharKey ','
    GLFW.Key'Minus -> CharKey '-'
    GLFW.Key'Period -> CharKey '.'
    GLFW.Key'Slash -> CharKey '/'
    GLFW.Key'0 -> CharKey '0'
    GLFW.Key'1 -> CharKey '1'
    GLFW.Key'2 -> CharKey '2'
    GLFW.Key'3 -> CharKey '3'
    GLFW.Key'4 -> CharKey '4'
    GLFW.Key'5 -> CharKey '5'
    GLFW.Key'6 -> CharKey '6'
    GLFW.Key'7 -> CharKey '7'
    GLFW.Key'8 -> CharKey '8'
    GLFW.Key'9 -> CharKey '9'
    GLFW.Key'Semicolon -> CharKey ';'
    GLFW.Key'Equal -> CharKey '='
    GLFW.Key'A -> CharKey 'a'
    GLFW.Key'B -> CharKey 'b'
    GLFW.Key'C -> CharKey 'c'
    GLFW.Key'D -> CharKey 'd'
    GLFW.Key'E -> CharKey 'e'
    GLFW.Key'F -> CharKey 'f'
    GLFW.Key'G -> CharKey 'g'
    GLFW.Key'H -> CharKey 'i'
    GLFW.Key'J -> CharKey 'j'
    GLFW.Key'K -> CharKey 'k'
    GLFW.Key'L -> CharKey 'l'
    GLFW.Key'M -> CharKey 'm'
    GLFW.Key'N -> CharKey 'n'
    GLFW.Key'O -> CharKey 'o'
    GLFW.Key'P -> CharKey 'p'
    GLFW.Key'Q -> CharKey 'q'
    GLFW.Key'R -> CharKey 'r'
    GLFW.Key'S -> CharKey 's'
    GLFW.Key'T -> CharKey 't'
    GLFW.Key'U -> CharKey 'u'
    GLFW.Key'V -> CharKey 'v'
    GLFW.Key'W -> CharKey 'w'
    GLFW.Key'X -> CharKey 'x'
    GLFW.Key'Y -> CharKey 'y'
    GLFW.Key'Z -> CharKey 'z'
    GLFW.Key'LeftBracket -> CharKey '['
    GLFW.Key'Backslash -> CharKey '\\'
    GLFW.Key'RightBracket -> CharKey ']'
    GLFW.Key'GraveAccent -> CharKey '`'
    v -> SpecialKey $ toSpecialKey v 

instance ToKey GLFW.MouseButton where 
  toKey k = MouseButton $ toMouseButton k 

instance ToMouseButton GLFW.MouseButton where 
  toMouseButton k = case k of 
    GLFW.MouseButton'1 -> LeftButton
    GLFW.MouseButton'2 -> RightButton
    GLFW.MouseButton'3 -> MiddleButton
    GLFW.MouseButton'4 -> AdditionalButton 4
    GLFW.MouseButton'5 -> AdditionalButton 5
    GLFW.MouseButton'6 -> AdditionalButton 6
    GLFW.MouseButton'7 -> AdditionalButton 7
    GLFW.MouseButton'8 -> AdditionalButton 8

instance ToKeyState GLFW.KeyState where 
  toKeyState s = case s of
    GLFW.KeyState'Pressed -> Down 
    GLFW.KeyState'Released -> Up
    GLFW.KeyState'Repeating -> Repeating

instance ToKeyState GLFW.MouseButtonState where 
  toKeyState s = case s of
    GLFW.MouseButtonState'Pressed -> Down 
    GLFW.MouseButtonState'Released -> Up

instance ToKeyState Bool where 
  toKeyState True = Down 
  toKeyState False = Up 

instance ToModifiers GLFW.ModifierKeys where 
  toModifiers (GLFW.ModifierKeys shift' ctrl' alt' super') = Modifiers (toKeyState shift') (toKeyState ctrl') (toKeyState alt') (toKeyState super')

instance ToSpecialKey GLFW.Key where 
  toSpecialKey k = case k of 
    GLFW.Key'World1 -> KeyWorld1
    GLFW.Key'World2 -> KeyWorld2
    GLFW.Key'Escape -> KeyEscape
    GLFW.Key'Enter -> KeyEnter
    GLFW.Key'Tab -> KeyTab
    GLFW.Key'Backspace -> KeyBackspace
    GLFW.Key'Insert -> KeyInsert
    GLFW.Key'Delete -> KeyDelete
    GLFW.Key'Right -> KeyRight
    GLFW.Key'Left -> KeyLeft
    GLFW.Key'Down -> KeyDown
    GLFW.Key'Up -> KeyUp
    GLFW.Key'PageUp -> KeyPageUp
    GLFW.Key'PageDown -> KeyPageDown
    GLFW.Key'Home -> KeyHome
    GLFW.Key'End -> KeyEnd
    GLFW.Key'CapsLock -> KeyCapsLock
    GLFW.Key'ScrollLock -> KeyScrollLock
    GLFW.Key'NumLock -> KeyNumLock
    GLFW.Key'PrintScreen -> KeyPrintScreen
    GLFW.Key'Pause -> KeyPause
    GLFW.Key'F1 -> KeyF1
    GLFW.Key'F2 -> KeyF2
    GLFW.Key'F3 -> KeyF3
    GLFW.Key'F4 -> KeyF4
    GLFW.Key'F5 -> KeyF5
    GLFW.Key'F6 -> KeyF6
    GLFW.Key'F7 -> KeyF7
    GLFW.Key'F8 -> KeyF8
    GLFW.Key'F9 -> KeyF9
    GLFW.Key'F10 -> KeyF10
    GLFW.Key'F11 -> KeyF11
    GLFW.Key'F12 -> KeyF12
    GLFW.Key'F13 -> KeyF13
    GLFW.Key'F14 -> KeyF14
    GLFW.Key'F15 -> KeyF15
    GLFW.Key'F16 -> KeyF16
    GLFW.Key'F17 -> KeyF17
    GLFW.Key'F18 -> KeyF18
    GLFW.Key'F19 -> KeyF19
    GLFW.Key'F20 -> KeyF20
    GLFW.Key'F21 -> KeyF21
    GLFW.Key'F22 -> KeyF22
    GLFW.Key'F23 -> KeyF23
    GLFW.Key'F24 -> KeyF24
    GLFW.Key'F25 -> KeyF25
    GLFW.Key'Pad0 -> KeyPad0
    GLFW.Key'Pad1 -> KeyPad1
    GLFW.Key'Pad2 -> KeyPad2
    GLFW.Key'Pad3 -> KeyPad3
    GLFW.Key'Pad4 -> KeyPad4
    GLFW.Key'Pad5 -> KeyPad5
    GLFW.Key'Pad6 -> KeyPad6
    GLFW.Key'Pad7 -> KeyPad7
    GLFW.Key'Pad8 -> KeyPad8
    GLFW.Key'Pad9 -> KeyPad9
    GLFW.Key'PadDecimal -> KeyPadDecimal
    GLFW.Key'PadDivide -> KeyPadDivide
    GLFW.Key'PadMultiply -> KeyPadMultiply
    GLFW.Key'PadSubtract -> KeyPadSubtract
    GLFW.Key'PadAdd -> KeyPadAdd
    GLFW.Key'PadEnter -> KeyPadEnter
    GLFW.Key'PadEqual -> KeyPadEqual
    GLFW.Key'LeftShift -> KeyShiftL
    GLFW.Key'LeftControl -> KeyCtrlL
    GLFW.Key'LeftAlt -> KeyAltL
    GLFW.Key'LeftSuper -> KeySuperL
    GLFW.Key'RightShift -> KeyShiftR
    GLFW.Key'RightControl -> KeyCtrlR
    GLFW.Key'RightAlt -> KeyAltR
    GLFW.Key'RightSuper -> KeySuperR
    GLFW.Key'Menu -> KeyMenu
    v -> KeyUnknown (fromEnum v)