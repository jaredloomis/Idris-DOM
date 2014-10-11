module Event

%default total
%access public

data Event : Type where
  MkEvent : Ptr -> Event

data EventType : Type where
    Click : EventType
    DoubleClick : EventType
    MouseDown : EventType
    MouseMove : EventType
    MouseOver : EventType
    MouseOut : EventType
    MouseUp : EventType
    KeyDown : EventType
    KeyUp : EventType
    KeyPress : EventType
    Abort : EventType
    Error : EventType
    Load : EventType
    Resize : EventType
    Scroll : EventType
    Unload : EventType
    Blur : EventType
    Change : EventType
    Focus : EventType
    Reset : EventType
    Select : EventType
    Submit : EventType

instance Show EventType where
    show Click = "click"
    show DoubleClick = "dblclick"
    show MouseDown = "mousedown"
    show MouseMove = "mousemove"
    show MouseOver = "mouseover"
    show MouseOut = "mouseout"
    show MouseUp = "mouseup"
    show KeyDown = "keydown"
    show KeyUp = "keyup"
    show KeyPress = "keypress"
    show Abort = "abort"
    show Error = "error"
    show Load = "load"
    show Resize = "resize"
    show Scroll = "scroll"
    show Unload = "unload"
    show Blur = "blur"
    show Change = "change"
    show Focus = "focus"
    show Reset = "reset"
    show Select = "select"
    show Submit = "submit"

eventMouseX : Event -> IO Int
eventMouseX (MkEvent ev) =
    mkForeign (FFun "%0.clientX" [FPtr] FInt) ev

eventMouseY : Event -> IO Int
eventMouseY (MkEvent ev) =
    mkForeign (FFun "%0.clientY" [FPtr] FInt) ev

eventMousePos : Event -> IO (Int, Int)
eventMousePos ev = do
    x <- eventMouseX ev
    y <- eventMouseY ev
    return (x, y)

eventKeycode : Event -> IO Char
eventKeycode (MkEvent ev) = cast `map`
    mkForeign (FFun "%0.keyCode" [FPtr] FInt) ev
