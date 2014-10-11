module Style

import RawStyle
import Attr
import Element

%default total
%access public

%include JavaScript "jsLib.js"

getComputedStyle : Element -> IO Style
getComputedStyle (MkElem element) = MkStyle `map`
    mkForeign (FFun "window.getComputedStyle(%0, null)" [FPtr] FPtr) element

getStyleRaw : Element -> IO Style
getStyleRaw (MkElem element) = MkStyle `map`
    mkForeign (FFun "%0.style" [FPtr] FPtr) element

withStyle : Attr Style a -> Attr Element a
withStyle (MkAttr getter setter) =
    let getter' = \el => getComputedStyle el >>= getter
        setter' = \el, val => setter !(getStyleRaw el) val
    in MkAttr getter' setter'

-- These are probably bad.
{-
getRealStyle : Element -> IO Style
getRealStyle (MkElem element) = MkStyle `map`
    mkForeign (FFun "getStyleObject(%0)" [FPtr] FPtr) element

setStyle : Element -> Style -> IO ()
setStyle (MkElem element) (MkStyle style) =
    mkForeign (FFun "%0.style = %1" [FPtr, FPtr] FUnit) element style

style : Attr Element Style
style = MkAttr getRealStyle setStyle
-}
