module RawStyle

import Attr

%default total
%access public

-- data Style' a = MkStyle' Ptr

data Style = MkStyle Ptr

data JsLength = Percent Float | Pixels Int

error : String -> a
error {a} str = unsafePerformIO $
    mkForeign (FFun "console.log(%0)" [FString] (FAny a)) str

instance Num JsLength where
    (Percent x) + (Percent y) = Percent (x+y)
    (Pixels x) + (Pixels y) = Pixels (x+y)
    _ + _ = error "JsLength.(+): Attempted to add Pixels and Percent."

    (Percent x) - (Percent y) = Percent (x-y)
    (Pixels x) - (Pixels y) = Pixels (x-y)
    _ - _ = error "JsLength.(-): Attempted to subtract Pixels and Percent."

    (Percent x) * (Percent y) = Percent (x*y)
    (Pixels x) * (Pixels y) = Pixels (x*y)
    _ * _ = error "JsLength.(*): Attempted to multiply Pixels and Percent."

    fromInteger = Pixels . fromInteger

    abs (Pixels x) = Pixels $ abs x
    abs (Percent x) = Percent $ abs x

instance Cast JsLength Float where
    cast (Percent p) = p
    cast (Pixels p) = cast p
instance Cast Float JsLength where
    cast = Pixels . cast

partial toIntPixels : JsLength -> Int
toIntPixels (Pixels p) = p

instance Show JsLength where
    show (Percent p) = show p ++ "%"
    show (Pixels p) = show p ++ "px"

instance Cast String JsLength where
    cast "" = Pixels 0
    cast str =
        if "%" `isSuffixOf` str
            then Percent . cast . pack $ takeWhile (/='%') (unpack str)
        else Pixels . cast . pack $ takeWhile (not . isAlpha) (unpack str)

data JsTime =
    Seconds Float
  | Milliseconds Float

instance Show JsTime where
    show (Seconds s) = show s ++ "s"
    show (Milliseconds ms) = show ms ++ "ms"

instance Cast String JsTime where
    cast str =
        (if "ms" `isSuffixOf` str
            then Milliseconds
         else Seconds) . cast . pack $
            takeWhile (not . isAlpha) (unpack str)

data Align =
    Stretch
  | Center
  | FlexStart
  | FlexEnd
  | Baseline

instance Show Align where
    show Stretch = "stretch"
    show Center = "center"
    show FlexStart = "flex-start"
    show FlexEnd = "flex-end"
    show Baseline = "baseline"

instance Cast String Align where
    cast "stretch" = Stretch
    cast "center" = Center
    cast "flex-start" = FlexStart
    cast "flex-end" = FlexEnd
    cast "baseline" = Baseline
    cast _ = Stretch

data PlayState = Running | Paused

instance Show PlayState where
    show Running = "running"
    show Paused = "paused"

instance Cast String PlayState where
    cast "paused" = Paused
    cast _ = Running

data BackgroundAttachment = Scroll | Fixed | Local

instance Show BackgroundAttachment where
    show Scroll = "scroll"
    show Fixed = "fixed"
    show Local = "local"

instance Cast String BackgroundAttachment where
    cast "fixed" = Fixed
    cast "local" = Local
    cast _ = Scroll

data RepeatStyle = Repeat | RepeatX | RepeatY | NoRepeat

instance Show RepeatStyle where
    show Repeat = "repeat"
    show RepeatX = "repeat-x"
    show RepeatY = "repeat-y"
    show NoRepeat = "no-repeat"

instance Cast String RepeatStyle where
    cast "repeat-x" = RepeatX
    cast "repeat-y" = RepeatY
    cast "no-repeat" = NoRepeat
    cast _ = Repeat

data Box = BorderBox | PaddingBox | ContentBox

instance Show Box where
    show BorderBox = "border-box"
    show PaddingBox = "padding-box"
    show ContentBox = "content-box"

instance Cast String Box where
    cast "padding-box" = PaddingBox
    cast "content-box" = ContentBox
    cast _ = BorderBox

data BackgroundSize =
    MkBackgroundSize (Vect 2 JsLength)
  | Cover
  | Contain
  | Auto

instance Show BackgroundSize where
    show (MkBackgroundSize [x, y]) = show x ++ " " ++ show y
    show Cover = "cover"
    show Contain = "contain"
    show Auto = "auto"

instance Cast String BackgroundSize where
    cast "cover" = Cover
    cast "contain" = Contain
    cast "auto" = Auto
    cast str =
        let (x,y) = break (==' ') str
        in MkBackgroundSize [cast x, cast y]

data Side = Top | Bottom | Left | Right

instance Show Side where
    show Top = "top"
    show Bottom = "bottom"
    show Left = "left"
    show Right = "right"

instance Cast String Side where
    cast "bottom" = Bottom
    cast "left" = Left
    cast "right" = Right
    cast _ = Top

data ClearStyle = None | ClearSide Side | Both

instance Show ClearStyle where
    show None = "none"
    show (ClearSide side) = show side
    show Both = "both"

instance Cast String ClearStyle where
    cast "left" = ClearSide Left
    cast "right" = ClearSide Right
    cast "both" = Both
    cast _ = None

data Direction = LtR | RtL

instance Show Direction where
    show LtR = "ltr"
    show RtL = "rtl"

instance Cast String Direction where
    cast "rtl" = RtL
    cast _ = LtR

getAlignContent : Style -> IO Align
getAlignContent (MkStyle style) = cast `map`
    mkForeign (FFun "%0.alignContent" [FPtr] FString) style
setAlignContent : Style -> Align -> IO ()
setAlignContent (MkStyle style) (val) =
    mkForeign (FFun "%0.alignContent = %1" [FPtr, FString] FUnit)
              style (show val)
alignContent : Attr Style Align
alignContent =
    let getter = getAlignContent
        setter = setAlignContent
    in MkAttr getter setter

getAlignItems : Style -> IO Align
getAlignItems (MkStyle style) = cast `map`
    mkForeign (FFun "%0.alignItems" [FPtr] FString) style
setAlignItems : Style -> Align -> IO ()
setAlignItems (MkStyle style) (val) =
    mkForeign (FFun "%0.alignItems = %1" [FPtr, FString] FUnit)
              style (show val)
alignItems : Attr Style Align
alignItems =
    let getter = getAlignItems
        setter = setAlignItems
    in MkAttr getter setter

getAlignSelf : Style -> IO Align
getAlignSelf (MkStyle style) = cast `map`
    mkForeign (FFun "%0.alignSelf" [FPtr] FString) style
setAlignSelf : Style -> Align -> IO ()
setAlignSelf (MkStyle style) (val) =
    mkForeign (FFun "%0.alignSelf = %1" [FPtr, FString] FUnit)
              style (show val)
alignSelf : Attr Style Align
alignSelf =
    let getter = getAlignSelf
        setter = setAlignSelf
    in MkAttr getter setter

getAnimation : Style -> IO String
getAnimation (MkStyle style) =
    mkForeign (FFun "%0.animation" [FPtr] FString) style
setAnimation : Style -> String -> IO ()
setAnimation (MkStyle style) (val) =
    mkForeign (FFun "%0.animation = %1" [FPtr, FString] FUnit) style val
animation : Attr Style String
animation =
    let getter = getAnimation
        setter = setAnimation
    in MkAttr getter setter

getAnimationName : Style -> IO String
getAnimationName (MkStyle style) = 
    mkForeign (FFun "%0.animationName" [FPtr] FString) style
setAnimationName : Style -> String -> IO ()
setAnimationName (MkStyle style) (val) =
    mkForeign (FFun "%0.animationName = %1" [FPtr, FString] FUnit) style val
animationName : Attr Style String
animationName =
    let getter = getAnimationName
        setter = setAnimationName
    in MkAttr getter setter

getAnimationPlayState : Style -> IO PlayState
getAnimationPlayState (MkStyle style) = cast `map`
    mkForeign (FFun "%0.animationPlayState" [FPtr] FString) style
setAnimationPlayState : Style -> PlayState -> IO ()
setAnimationPlayState (MkStyle style) (val) =
    mkForeign (FFun "%0.animationPlayState = %1" [FPtr, FString] FUnit)
              style (show val)
animationPlayState : Attr Style PlayState
animationPlayState =
    let getter = getAnimationPlayState
        setter = setAnimationPlayState
    in MkAttr getter setter

getBackground : Style -> IO String
getBackground (MkStyle style) = 
    mkForeign (FFun "%0.background" [FPtr] FString) style
setBackground : Style -> String -> IO ()
setBackground (MkStyle style) (val) =
    mkForeign (FFun "%0.background = %1" [FPtr, FString] FUnit) style val
background : Attr Style String
background =
    let getter = getBackground
        setter = setBackground
    in MkAttr getter setter

getBackgroundAttachment : Style -> IO BackgroundAttachment
getBackgroundAttachment (MkStyle style) = cast `map`
    mkForeign (FFun "%0.backgroundAttachment" [FPtr] FString) style
setBackgroundAttachment : Style -> BackgroundAttachment -> IO ()
setBackgroundAttachment (MkStyle style) (val) =
    mkForeign (FFun "%0.backgroundAttachment = %1" [FPtr, FString] FUnit)
              style (show val)
backgroundAttachment : Attr Style BackgroundAttachment
backgroundAttachment =
    let getter = getBackgroundAttachment
        setter = setBackgroundAttachment
    in MkAttr getter setter

getBackgroundColor : Style -> IO String
getBackgroundColor (MkStyle style) = 
    mkForeign (FFun "%0.backgroundColor" [FPtr] FString) style
setBackgroundColor : Style -> String -> IO ()
setBackgroundColor (MkStyle style) (val) =
    mkForeign (FFun "%0.backgroundColor = %1" [FPtr, FString] FUnit) style val
backgroundColor : Attr Style String
backgroundColor =
    let getter = getBackgroundColor
        setter = setBackgroundColor
    in MkAttr getter setter

getBackgroundImage : Style -> IO String
getBackgroundImage (MkStyle style) = 
    mkForeign (FFun "%0.backgroundImage" [FPtr] FString) style
setBackgroundImage : Style -> String -> IO ()
setBackgroundImage (MkStyle style) (val) =
    mkForeign (FFun "%0.backgroundImage = %1" [FPtr, FString] FUnit) style val
backgroundImage : Attr Style String
backgroundImage =
    let getter = getBackgroundImage
        setter = setBackgroundImage
    in MkAttr getter setter

getBackgroundPosition : Style -> IO String
getBackgroundPosition (MkStyle style) = 
    mkForeign (FFun "%0.backgroundPosition" [FPtr] FString) style
setBackgroundPosition : Style -> String -> IO ()
setBackgroundPosition (MkStyle style) (val) =
    mkForeign (FFun "%0.backgroundPosition = %1" [FPtr, FString] FUnit) style val
backgroundPosition : Attr Style String
backgroundPosition =
    let getter = getBackgroundPosition
        setter = setBackgroundPosition
    in MkAttr getter setter

getBackgroundRepeat : Style -> IO RepeatStyle
getBackgroundRepeat (MkStyle style) = cast `map`
    mkForeign (FFun "%0.backgroundRepeat" [FPtr] FString) style
setBackgroundRepeat : Style -> RepeatStyle -> IO ()
setBackgroundRepeat (MkStyle style) (val) =
    mkForeign (FFun "%0.backgroundRepeat = %1" [FPtr, FString] FUnit)
              style (show val)
backgroundRepeat : Attr Style RepeatStyle
backgroundRepeat =
    let getter = getBackgroundRepeat
        setter = setBackgroundRepeat
    in MkAttr getter setter

getBackgroundClip : Style -> IO Box
getBackgroundClip (MkStyle style) = cast `map`
    mkForeign (FFun "%0.backgroundClip" [FPtr] FString) style
setBackgroundClip : Style -> Box -> IO ()
setBackgroundClip (MkStyle style) (val) =
    mkForeign (FFun "%0.backgroundClip = %1" [FPtr, FString] FUnit)
              style (show val)
backgroundClip : Attr Style Box
backgroundClip =
    let getter = getBackgroundClip
        setter = setBackgroundClip
    in MkAttr getter setter

getBackgroundOrigin : Style -> IO Box
getBackgroundOrigin (MkStyle style) = cast `map`
    mkForeign (FFun "%0.backgroundOrigin" [FPtr] FString) style
setBackgroundOrigin : Style -> Box -> IO ()
setBackgroundOrigin (MkStyle style) (val) =
    mkForeign (FFun "%0.backgroundOrigin = %1" [FPtr, FString] FUnit)
              style (show val)
backgroundOrigin : Attr Style Box
backgroundOrigin =
    let getter = getBackgroundOrigin
        setter = setBackgroundOrigin
    in MkAttr getter setter

getBackgroundSize : Style -> IO BackgroundSize
getBackgroundSize (MkStyle style) = cast `map`
    mkForeign (FFun "%0.backgroundSize" [FPtr] FString) style
setBackgroundSize : Style -> BackgroundSize -> IO ()
setBackgroundSize (MkStyle style) (val) =
    mkForeign (FFun "%0.backgroundSize = %1" [FPtr, FString] FUnit)
              style (show val)
backgroundSize : Attr Style BackgroundSize
backgroundSize =
    let getter = getBackgroundSize
        setter = setBackgroundSize
    in MkAttr getter setter

getBackfaceVisibility : Style -> IO Bool
getBackfaceVisibility (MkStyle style) =
    (\x => if x=="hidden" then False else True) `map`
        mkForeign (FFun "%0.backfaceVisibility" [FPtr] FString) style
setBackfaceVisibility : Style -> Bool -> IO ()
setBackfaceVisibility (MkStyle style) (val) =
    let valStr = if val then "visible" else "hidden"
    in mkForeign (FFun "%0.backfaceVisibility = %1" [FPtr, FString] FUnit)
                 style valStr
backfaceVisibility : Attr Style Bool
backfaceVisibility =
    let getter = getBackfaceVisibility
        setter = setBackfaceVisibility
    in MkAttr getter setter

getBorder : Style -> IO String
getBorder (MkStyle style) = 
    mkForeign (FFun "%0.border" [FPtr] FString) style
setBorder : Style -> String -> IO ()
setBorder (MkStyle style) (val) =
    mkForeign (FFun "%0.border = %1" [FPtr, FString] FUnit) style val
border : Attr Style String
border =
    let getter = getBorder
        setter = setBorder
    in MkAttr getter setter

getBorderBottom : Style -> IO String
getBorderBottom (MkStyle style) = 
    mkForeign (FFun "%0.borderBottom" [FPtr] FString) style
setBorderBottom : Style -> String -> IO ()
setBorderBottom (MkStyle style) (val) =
    mkForeign (FFun "%0.borderBottom = %1" [FPtr, FString] FUnit) style val
borderBottom : Attr Style String
borderBottom =
    let getter = getBorderBottom
        setter = setBorderBottom
    in MkAttr getter setter

getBorderBottomColor : Style -> IO String
getBorderBottomColor (MkStyle style) = 
    mkForeign (FFun "%0.borderBottomColor" [FPtr] FString) style
setBorderBottomColor : Style -> String -> IO ()
setBorderBottomColor (MkStyle style) (val) =
    mkForeign (FFun "%0.borderBottomColor = %1" [FPtr, FString] FUnit) style val
borderBottomColor : Attr Style String
borderBottomColor =
    let getter = getBorderBottomColor
        setter = setBorderBottomColor
    in MkAttr getter setter

getBorderBottomLeftRadius : Style -> IO JsLength
getBorderBottomLeftRadius (MkStyle style) = cast `map`
    mkForeign (FFun "%0.borderBottomLeftRadius" [FPtr] FString) style
setBorderBottomLeftRadius : Style -> JsLength -> IO ()
setBorderBottomLeftRadius (MkStyle style) (val) =
    mkForeign (FFun "%0.borderBottomLeftRadius = %1" [FPtr, FString] FUnit)
              style (show val)
borderBottomLeftRadius : Attr Style JsLength
borderBottomLeftRadius =
    let getter = getBorderBottomLeftRadius
        setter = setBorderBottomLeftRadius
    in MkAttr getter setter

getBorderBottomRightRadius : Style -> IO JsLength
getBorderBottomRightRadius (MkStyle style) = cast `map`
    mkForeign (FFun "%0.borderBottomRightRadius" [FPtr] FString) style
setBorderBottomRightRadius : Style -> JsLength -> IO ()
setBorderBottomRightRadius (MkStyle style) (val) =
    mkForeign (FFun "%0.borderBottomRightRadius = %1" [FPtr, FString] FUnit)
              style (show val)
borderBottomRightRadius : Attr Style JsLength
borderBottomRightRadius =
    let getter = getBorderBottomRightRadius
        setter = setBorderBottomRightRadius
    in MkAttr getter setter

getBorderBottomStyle : Style -> IO String
getBorderBottomStyle (MkStyle style) = 
    mkForeign (FFun "%0.borderBottomStyle" [FPtr] FString) style
setBorderBottomStyle : Style -> String -> IO ()
setBorderBottomStyle (MkStyle style) (val) =
    mkForeign (FFun "%0.borderBottomStyle = %1" [FPtr, FString] FUnit) style val
borderBottomStyle : Attr Style String
borderBottomStyle =
    let getter = getBorderBottomStyle
        setter = setBorderBottomStyle
    in MkAttr getter setter

getBorderBottomWidth : Style -> IO JsLength
getBorderBottomWidth (MkStyle style) = cast `map`
    mkForeign (FFun "%0.borderBottomWidth" [FPtr] FString) style
setBorderBottomWidth : Style -> JsLength -> IO ()
setBorderBottomWidth (MkStyle style) (val) =
    mkForeign (FFun "%0.borderBottomWidth = %1" [FPtr, FString] FUnit)
              style (show val)
borderBottomWidth : Attr Style JsLength
borderBottomWidth =
    let getter = getBorderBottomWidth
        setter = setBorderBottomWidth
    in MkAttr getter setter

getBorderCollapse : Style -> IO Bool
getBorderCollapse (MkStyle style) =
    (\x => if x=="collapse" then True else False) `map`
        mkForeign (FFun "%0.borderCollapse" [FPtr] FString) style
setBorderCollapse : Style -> Bool -> IO ()
setBorderCollapse (MkStyle style) (val) =
    let valStr = if val then "collapse" else "seperate"
    in mkForeign (FFun "%0.borderCollapse = %1" [FPtr, FString] FUnit)
                 style valStr
borderCollapse : Attr Style Bool
borderCollapse =
    let getter = getBorderCollapse
        setter = setBorderCollapse
    in MkAttr getter setter

getBorderColor : Style -> IO String
getBorderColor (MkStyle style) = 
    mkForeign (FFun "%0.borderColor" [FPtr] FString) style
setBorderColor : Style -> String -> IO ()
setBorderColor (MkStyle style) (val) =
    mkForeign (FFun "%0.borderColor = %1" [FPtr, FString] FUnit) style val
borderColor : Attr Style String
borderColor =
    let getter = getBorderColor
        setter = setBorderColor
    in MkAttr getter setter

getBorderImage : Style -> IO String
getBorderImage (MkStyle style) = 
    mkForeign (FFun "%0.borderImage" [FPtr] FString) style
setBorderImage : Style -> String -> IO ()
setBorderImage (MkStyle style) (val) =
    mkForeign (FFun "%0.borderImage = %1" [FPtr, FString] FUnit) style val
borderImage : Attr Style String
borderImage =
    let getter = getBorderImage
        setter = setBorderImage
    in MkAttr getter setter

getBorderImageOutset : Style -> IO String
getBorderImageOutset (MkStyle style) = 
    mkForeign (FFun "%0.borderImageOutset" [FPtr] FString) style
setBorderImageOutset : Style -> String -> IO ()
setBorderImageOutset (MkStyle style) (val) =
    mkForeign (FFun "%0.borderImageOutset = %1" [FPtr, FString] FUnit) style val
borderImageOutset : Attr Style String
borderImageOutset =
    let getter = getBorderImageOutset
        setter = setBorderImageOutset
    in MkAttr getter setter

getBorderImageRepeat : Style -> IO String
getBorderImageRepeat (MkStyle style) = 
    mkForeign (FFun "%0.borderImageRepeat" [FPtr] FString) style
setBorderImageRepeat : Style -> String -> IO ()
setBorderImageRepeat (MkStyle style) (val) =
    mkForeign (FFun "%0.borderImageRepeat = %1" [FPtr, FString] FUnit) style val
borderImageRepeat : Attr Style String
borderImageRepeat =
    let getter = getBorderImageRepeat
        setter = setBorderImageRepeat
    in MkAttr getter setter

getBorderImageSlice : Style -> IO String
getBorderImageSlice (MkStyle style) = 
    mkForeign (FFun "%0.borderImageSlice" [FPtr] FString) style
setBorderImageSlice : Style -> String -> IO ()
setBorderImageSlice (MkStyle style) (val) =
    mkForeign (FFun "%0.borderImageSlice = %1" [FPtr, FString] FUnit) style val
borderImageSlice : Attr Style String
borderImageSlice =
    let getter = getBorderImageSlice
        setter = setBorderImageSlice
    in MkAttr getter setter

getBorderImageSource : Style -> IO String
getBorderImageSource (MkStyle style) = 
    mkForeign (FFun "%0.borderImageSource" [FPtr] FString) style
setBorderImageSource : Style -> String -> IO ()
setBorderImageSource (MkStyle style) (val) =
    mkForeign (FFun "%0.borderImageSource = %1" [FPtr, FString] FUnit) style val
borderImageSource : Attr Style String
borderImageSource =
    let getter = getBorderImageSource
        setter = setBorderImageSource
    in MkAttr getter setter

getBorderImageWidth : Style -> IO JsLength
getBorderImageWidth (MkStyle style) = cast `map`
    mkForeign (FFun "%0.borderImageWidth" [FPtr] FString) style
setBorderImageWidth : Style -> JsLength -> IO ()
setBorderImageWidth (MkStyle style) (val) =
    mkForeign (FFun "%0.borderImageWidth = %1" [FPtr, FString] FUnit)
              style (show val)
borderImageWidth : Attr Style JsLength
borderImageWidth =
    let getter = getBorderImageWidth
        setter = setBorderImageWidth
    in MkAttr getter setter

getBorderLeft : Style -> IO String
getBorderLeft (MkStyle style) = 
    mkForeign (FFun "%0.borderLeft" [FPtr] FString) style
setBorderLeft : Style -> String -> IO ()
setBorderLeft (MkStyle style) (val) =
    mkForeign (FFun "%0.borderLeft = %1" [FPtr, FString] FUnit) style val
borderLeft : Attr Style String
borderLeft =
    let getter = getBorderLeft
        setter = setBorderLeft
    in MkAttr getter setter

getBorderLeftColor : Style -> IO String
getBorderLeftColor (MkStyle style) = 
    mkForeign (FFun "%0.borderLeftColor" [FPtr] FString) style
setBorderLeftColor : Style -> String -> IO ()
setBorderLeftColor (MkStyle style) (val) =
    mkForeign (FFun "%0.borderLeftColor = %1" [FPtr, FString] FUnit) style val
borderLeftColor : Attr Style String
borderLeftColor =
    let getter = getBorderLeftColor
        setter = setBorderLeftColor
    in MkAttr getter setter

getBorderLeftStyle : Style -> IO String
getBorderLeftStyle (MkStyle style) = 
    mkForeign (FFun "%0.borderLeftStyle" [FPtr] FString) style
setBorderLeftStyle : Style -> String -> IO ()
setBorderLeftStyle (MkStyle style) (val) =
    mkForeign (FFun "%0.borderLeftStyle = %1" [FPtr, FString] FUnit) style val
borderLeftStyle : Attr Style String
borderLeftStyle =
    let getter = getBorderLeftStyle
        setter = setBorderLeftStyle
    in MkAttr getter setter

getBorderLeftWidth : Style -> IO JsLength
getBorderLeftWidth (MkStyle style) = cast `map`
    mkForeign (FFun "%0.borderLeftWidth" [FPtr] FString) style
setBorderLeftWidth : Style -> JsLength -> IO ()
setBorderLeftWidth (MkStyle style) (val) =
    mkForeign (FFun "%0.borderLeftWidth = %1" [FPtr, FString] FUnit)
              style (show val)
borderLeftWidth : Attr Style JsLength
borderLeftWidth =
    let getter = getBorderLeftWidth
        setter = setBorderLeftWidth
    in MkAttr getter setter

getBorderRadius : Style -> IO JsLength
getBorderRadius (MkStyle style) = cast `map`
    mkForeign (FFun "%0.borderRadius" [FPtr] FString) style
setBorderRadius : Style -> JsLength -> IO ()
setBorderRadius (MkStyle style) (val) =
    mkForeign (FFun "%0.borderRadius = %1" [FPtr, FString] FUnit)
              style (show val)
borderRadius : Attr Style JsLength
borderRadius =
    let getter = getBorderRadius
        setter = setBorderRadius
    in MkAttr getter setter

getBorderRight : Style -> IO String
getBorderRight (MkStyle style) = 
    mkForeign (FFun "%0.borderRight" [FPtr] FString) style
setBorderRight : Style -> String -> IO ()
setBorderRight (MkStyle style) (val) =
    mkForeign (FFun "%0.borderRight = %1" [FPtr, FString] FUnit) style val
borderRight : Attr Style String
borderRight =
    let getter = getBorderRight
        setter = setBorderRight
    in MkAttr getter setter

getBorderRightColor : Style -> IO String
getBorderRightColor (MkStyle style) = 
    mkForeign (FFun "%0.borderRightColor" [FPtr] FString) style
setBorderRightColor : Style -> String -> IO ()
setBorderRightColor (MkStyle style) (val) =
    mkForeign (FFun "%0.borderRightColor = %1" [FPtr, FString] FUnit) style val
borderRightColor : Attr Style String
borderRightColor =
    let getter = getBorderRightColor
        setter = setBorderRightColor
    in MkAttr getter setter

getBorderRightStyle : Style -> IO String
getBorderRightStyle (MkStyle style) = 
    mkForeign (FFun "%0.borderRightStyle" [FPtr] FString) style
setBorderRightStyle : Style -> String -> IO ()
setBorderRightStyle (MkStyle style) (val) =
    mkForeign (FFun "%0.borderRightStyle = %1" [FPtr, FString] FUnit) style val
borderRightStyle : Attr Style String
borderRightStyle =
    let getter = getBorderRightStyle
        setter = setBorderRightStyle
    in MkAttr getter setter

getBorderRightWidth : Style -> IO JsLength
getBorderRightWidth (MkStyle style) = cast `map`
    mkForeign (FFun "%0.borderRightWidth" [FPtr] FString) style
setBorderRightWidth : Style -> JsLength -> IO ()
setBorderRightWidth (MkStyle style) (val) =
    mkForeign (FFun "%0.borderRightWidth = %1" [FPtr, FString] FUnit)
              style (show val)
borderRightWidth : Attr Style JsLength
borderRightWidth =
    let getter = getBorderRightWidth
        setter = setBorderRightWidth
    in MkAttr getter setter

getBorderSpacing : Style -> IO JsLength
getBorderSpacing (MkStyle style) = cast `map`
    mkForeign (FFun "%0.borderSpacing" [FPtr] FString) style
setBorderSpacing : Style -> JsLength -> IO ()
setBorderSpacing (MkStyle style) (val) =
    mkForeign (FFun "%0.borderSpacing = %1" [FPtr, FString] FUnit)
              style (show val)
borderSpacing : Attr Style JsLength
borderSpacing =
    let getter = getBorderSpacing
        setter = setBorderSpacing
    in MkAttr getter setter

getBorderTop : Style -> IO String
getBorderTop (MkStyle style) = 
    mkForeign (FFun "%0.borderTop" [FPtr] FString) style
setBorderTop : Style -> String -> IO ()
setBorderTop (MkStyle style) (val) =
    mkForeign (FFun "%0.borderTop = %1" [FPtr, FString] FUnit) style val
borderTop : Attr Style String
borderTop =
    let getter = getBorderTop
        setter = setBorderTop
    in MkAttr getter setter

getBorderTopColor : Style -> IO String
getBorderTopColor (MkStyle style) = 
    mkForeign (FFun "%0.borderTopColor" [FPtr] FString) style
setBorderTopColor : Style -> String -> IO ()
setBorderTopColor (MkStyle style) (val) =
    mkForeign (FFun "%0.borderTopColor = %1" [FPtr, FString] FUnit) style val
borderTopColor : Attr Style String
borderTopColor =
    let getter = getBorderTopColor
        setter = setBorderTopColor
    in MkAttr getter setter

getBorderTopLeftRadius : Style -> IO JsLength
getBorderTopLeftRadius (MkStyle style) = cast `map`
    mkForeign (FFun "%0.borderTopLeftRadius" [FPtr] FString) style
setBorderTopLeftRadius : Style -> JsLength -> IO ()
setBorderTopLeftRadius (MkStyle style) (val) =
    mkForeign (FFun "%0.borderTopLeftRadius = %1" [FPtr, FString] FUnit)
              style (show val)
borderTopLeftRadius : Attr Style JsLength
borderTopLeftRadius =
    let getter = getBorderTopLeftRadius
        setter = setBorderTopLeftRadius
    in MkAttr getter setter

getBorderTopRightRadius : Style -> IO JsLength
getBorderTopRightRadius (MkStyle style) = cast `map`
    mkForeign (FFun "%0.borderTopRightRadius" [FPtr] FString) style
setBorderTopRightRadius : Style -> JsLength -> IO ()
setBorderTopRightRadius (MkStyle style) (val) =
    mkForeign (FFun "%0.borderTopRightRadius = %1" [FPtr, FString] FUnit)
              style (show val)
borderTopRightRadius : Attr Style JsLength
borderTopRightRadius =
    let getter = getBorderTopRightRadius
        setter = setBorderTopRightRadius
    in MkAttr getter setter

getBorderTopStyle : Style -> IO String
getBorderTopStyle (MkStyle style) = 
    mkForeign (FFun "%0.borderTopStyle" [FPtr] FString) style
setBorderTopStyle : Style -> String -> IO ()
setBorderTopStyle (MkStyle style) (val) =
    mkForeign (FFun "%0.borderTopStyle = %1" [FPtr, FString] FUnit) style val
borderTopStyle : Attr Style String
borderTopStyle =
    let getter = getBorderTopStyle
        setter = setBorderTopStyle
    in MkAttr getter setter

getBorderTopWidth : Style -> IO JsLength
getBorderTopWidth (MkStyle style) = cast `map`
    mkForeign (FFun "%0.borderTopWidth" [FPtr] FString) style
setBorderTopWidth : Style -> JsLength -> IO ()
setBorderTopWidth (MkStyle style) (val) =
    mkForeign (FFun "%0.borderTopWidth = %1" [FPtr, FString] FUnit)
              style (show val)
borderTopWidth : Attr Style JsLength
borderTopWidth =
    let getter = getBorderTopWidth
        setter = setBorderTopWidth
    in MkAttr getter setter

getBorderWidth : Style -> IO JsLength
getBorderWidth (MkStyle style) = cast `map`
    mkForeign (FFun "%0.borderWidth" [FPtr] FString) style
setBorderWidth : Style -> JsLength -> IO ()
setBorderWidth (MkStyle style) (val) =
    mkForeign (FFun "%0.borderWidth = %1" [FPtr, FString] FUnit)
              style (show val)
borderWidth : Attr Style JsLength
borderWidth =
    let getter = getBorderWidth
        setter = setBorderWidth
    in MkAttr getter setter

getBottom : Style -> IO String
getBottom (MkStyle style) = 
    mkForeign (FFun "%0.bottom" [FPtr] FString) style
setBottom : Style -> String -> IO ()
setBottom (MkStyle style) (val) =
    mkForeign (FFun "%0.bottom = %1" [FPtr, FString] FUnit) style val
bottom : Attr Style String
bottom =
    let getter = getBottom
        setter = setBottom
    in MkAttr getter setter

getBoxDecorationBreak : Style -> IO String
getBoxDecorationBreak (MkStyle style) = 
    mkForeign (FFun "%0.boxDecorationBreak" [FPtr] FString) style
setBoxDecorationBreak : Style -> String -> IO ()
setBoxDecorationBreak (MkStyle style) (val) =
    mkForeign (FFun "%0.boxDecorationBreak = %1" [FPtr, FString] FUnit) style val
boxDecorationBreak : Attr Style String
boxDecorationBreak =
    let getter = getBoxDecorationBreak
        setter = setBoxDecorationBreak
    in MkAttr getter setter

getBoxShadow : Style -> IO String
getBoxShadow (MkStyle style) = 
    mkForeign (FFun "%0.boxShadow" [FPtr] FString) style
setBoxShadow : Style -> String -> IO ()
setBoxShadow (MkStyle style) (val) =
    mkForeign (FFun "%0.boxShadow = %1" [FPtr, FString] FUnit) style val
boxShadow : Attr Style String
boxShadow =
    let getter = getBoxShadow
        setter = setBoxShadow
    in MkAttr getter setter

getBoxSizing : Style -> IO Box
getBoxSizing (MkStyle style) = cast `map`
    mkForeign (FFun "%0.boxSizing" [FPtr] FString) style
setBoxSizing : Style -> Box -> IO ()
setBoxSizing (MkStyle style) (val) =
    mkForeign (FFun "%0.boxSizing = %1" [FPtr, FString] FUnit)
              style (show val)
boxSizing : Attr Style Box
boxSizing =
    let getter = getBoxSizing
        setter = setBoxSizing
    in MkAttr getter setter

getCaptionSide : Style -> IO Side
getCaptionSide (MkStyle style) = cast `map`
    mkForeign (FFun "%0.captionSide" [FPtr] FString) style
setCaptionSide : Style -> Side -> IO ()
setCaptionSide (MkStyle style) (val) =
    mkForeign (FFun "%0.captionSide = %1" [FPtr, FString] FUnit)
              style (show val)
captionSide : Attr Style Side
captionSide =
    let getter = getCaptionSide
        setter = setCaptionSide
    in MkAttr getter setter

getClear : Style -> IO ClearStyle
getClear (MkStyle style) = cast `map`
    mkForeign (FFun "%0.clear" [FPtr] FString) style
setClear : Style -> ClearStyle -> IO ()
setClear (MkStyle style) (val) =
    mkForeign (FFun "%0.clear = %1" [FPtr, FString] FUnit)
              style (show val)
clear : Attr Style ClearStyle
clear =
    let getter = getClear
        setter = setClear
    in MkAttr getter setter

getClip : Style -> IO String
getClip (MkStyle style) = 
    mkForeign (FFun "%0.clip" [FPtr] FString) style
setClip : Style -> String -> IO ()
setClip (MkStyle style) (val) =
    mkForeign (FFun "%0.clip = %1" [FPtr, FString] FUnit) style val
clip : Attr Style String
clip =
    let getter = getClip
        setter = setClip
    in MkAttr getter setter

getColor : Style -> IO String
getColor (MkStyle style) = 
    mkForeign (FFun "%0.color" [FPtr] FString) style
setColor : Style -> String -> IO ()
setColor (MkStyle style) (val) =
    mkForeign (FFun "%0.color = %1" [FPtr, FString] FUnit) style val
color : Attr Style String
color =
    let getter = getColor
        setter = setColor
    in MkAttr getter setter

getColumnCount : Style -> IO String
getColumnCount (MkStyle style) = 
    mkForeign (FFun "%0.columnCount" [FPtr] FString) style
setColumnCount : Style -> String -> IO ()
setColumnCount (MkStyle style) (val) =
    mkForeign (FFun "%0.columnCount = %1" [FPtr, FString] FUnit) style val
columnCount : Attr Style String
columnCount =
    let getter = getColumnCount
        setter = setColumnCount
    in MkAttr getter setter

getColumnGap : Style -> IO JsLength
getColumnGap (MkStyle style) = cast `map`
    mkForeign (FFun "%0.columnGap" [FPtr] FString) style
setColumnGap : Style -> JsLength -> IO ()
setColumnGap (MkStyle style) (val) =
    mkForeign (FFun "%0.columnGap = %1" [FPtr, FString] FUnit)
              style (show val)
columnGap : Attr Style JsLength
columnGap =
    let getter = getColumnGap
        setter = setColumnGap
    in MkAttr getter setter

getColumnRule : Style -> IO String
getColumnRule (MkStyle style) = 
    mkForeign (FFun "%0.columnRule" [FPtr] FString) style
setColumnRule : Style -> String -> IO ()
setColumnRule (MkStyle style) (val) =
    mkForeign (FFun "%0.columnRule = %1" [FPtr, FString] FUnit) style val
columnRule : Attr Style String
columnRule =
    let getter = getColumnRule
        setter = setColumnRule
    in MkAttr getter setter

getColumnRuleColor : Style -> IO String
getColumnRuleColor (MkStyle style) = 
    mkForeign (FFun "%0.columnRuleColor" [FPtr] FString) style
setColumnRuleColor : Style -> String -> IO ()
setColumnRuleColor (MkStyle style) (val) =
    mkForeign (FFun "%0.columnRuleColor = %1" [FPtr, FString] FUnit) style val
columnRuleColor : Attr Style String
columnRuleColor =
    let getter = getColumnRuleColor
        setter = setColumnRuleColor
    in MkAttr getter setter

getColumnRuleStyle : Style -> IO String
getColumnRuleStyle (MkStyle style) = 
    mkForeign (FFun "%0.columnRuleStyle" [FPtr] FString) style
setColumnRuleStyle : Style -> String -> IO ()
setColumnRuleStyle (MkStyle style) (val) =
    mkForeign (FFun "%0.columnRuleStyle = %1" [FPtr, FString] FUnit) style val
columnRuleStyle : Attr Style String
columnRuleStyle =
    let getter = getColumnRuleStyle
        setter = setColumnRuleStyle
    in MkAttr getter setter

getColumnRuleWidth : Style -> IO JsLength
getColumnRuleWidth (MkStyle style) = cast `map`
    mkForeign (FFun "%0.columnRuleWidth" [FPtr] FString) style
setColumnRuleWidth : Style -> JsLength -> IO ()
setColumnRuleWidth (MkStyle style) (val) =
    mkForeign (FFun "%0.columnRuleWidth = %1" [FPtr, FString] FUnit)
              style (show val)
columnRuleWidth : Attr Style JsLength
columnRuleWidth =
    let getter = getColumnRuleWidth
        setter = setColumnRuleWidth
    in MkAttr getter setter

getColumns : Style -> IO String
getColumns (MkStyle style) = 
    mkForeign (FFun "%0.columns" [FPtr] FString) style
setColumns : Style -> String -> IO ()
setColumns (MkStyle style) (val) =
    mkForeign (FFun "%0.columns = %1" [FPtr, FString] FUnit) style val
columns : Attr Style String
columns =
    let getter = getColumns
        setter = setColumns
    in MkAttr getter setter

getColumnSpan : Style -> IO String
getColumnSpan (MkStyle style) = 
    mkForeign (FFun "%0.columnSpan" [FPtr] FString) style
setColumnSpan : Style -> String -> IO ()
setColumnSpan (MkStyle style) (val) =
    mkForeign (FFun "%0.columnSpan = %1" [FPtr, FString] FUnit) style val
columnSpan : Attr Style String
columnSpan =
    let getter = getColumnSpan
        setter = setColumnSpan
    in MkAttr getter setter

getColumnWidth : Style -> IO JsLength
getColumnWidth (MkStyle style) = cast `map`
    mkForeign (FFun "%0.columnWidth" [FPtr] FString) style
setColumnWidth : Style -> JsLength -> IO ()
setColumnWidth (MkStyle style) (val) =
    mkForeign (FFun "%0.columnWidth = %1" [FPtr, FString] FUnit)
              style (show val)
columnWidth : Attr Style JsLength
columnWidth =
    let getter = getColumnWidth
        setter = setColumnWidth
    in MkAttr getter setter

getContent : Style -> IO String
getContent (MkStyle style) = 
    mkForeign (FFun "%0.content" [FPtr] FString) style
setContent : Style -> String -> IO ()
setContent (MkStyle style) (val) =
    mkForeign (FFun "%0.content = %1" [FPtr, FString] FUnit) style val
content : Attr Style String
content =
    let getter = getContent
        setter = setContent
    in MkAttr getter setter

getCounterIncrement : Style -> IO String
getCounterIncrement (MkStyle style) = 
    mkForeign (FFun "%0.counterIncrement" [FPtr] FString) style
setCounterIncrement : Style -> String -> IO ()
setCounterIncrement (MkStyle style) (val) =
    mkForeign (FFun "%0.counterIncrement = %1" [FPtr, FString] FUnit) style val
counterIncrement : Attr Style String
counterIncrement =
    let getter = getCounterIncrement
        setter = setCounterIncrement
    in MkAttr getter setter

getCounterReset : Style -> IO String
getCounterReset (MkStyle style) = 
    mkForeign (FFun "%0.counterReset" [FPtr] FString) style
setCounterReset : Style -> String -> IO ()
setCounterReset (MkStyle style) (val) =
    mkForeign (FFun "%0.counterReset = %1" [FPtr, FString] FUnit) style val
counterReset : Attr Style String
counterReset =
    let getter = getCounterReset
        setter = setCounterReset
    in MkAttr getter setter

getCursor : Style -> IO String
getCursor (MkStyle style) = 
    mkForeign (FFun "%0.cursor" [FPtr] FString) style
setCursor : Style -> String -> IO ()
setCursor (MkStyle style) (val) =
    mkForeign (FFun "%0.cursor = %1" [FPtr, FString] FUnit) style val
cursor : Attr Style String
cursor =
    let getter = getCursor
        setter = setCursor
    in MkAttr getter setter

getDirection : Style -> IO Direction
getDirection (MkStyle style) = cast `map`
    mkForeign (FFun "%0.direction" [FPtr] FString) style
setDirection : Style -> Direction -> IO ()
setDirection (MkStyle style) (val) =
    mkForeign (FFun "%0.direction = %1" [FPtr, FString] FUnit)
              style (show val)
direction : Attr Style Direction
direction =
    let getter = getDirection
        setter = setDirection
    in MkAttr getter setter

getDisplay : Style -> IO String
getDisplay (MkStyle style) = 
    mkForeign (FFun "%0.display" [FPtr] FString) style
setDisplay : Style -> String -> IO ()
setDisplay (MkStyle style) (val) =
    mkForeign (FFun "%0.display = %1" [FPtr, FString] FUnit) style val
display : Attr Style String
display =
    let getter = getDisplay
        setter = setDisplay
    in MkAttr getter setter

getEmptyCells : Style -> IO Bool
getEmptyCells (MkStyle style) =
    (\x => not $ x=="hide") `map`
        mkForeign (FFun "%0.emptyCells" [FPtr] FString) style
setEmptyCells : Style -> Bool -> IO ()
setEmptyCells (MkStyle style) (val) =
    let valStr = if val then "show" else "hide"
    in mkForeign (FFun "%0.emptyCells = %1" [FPtr, FString] FUnit)
                 style (show val)
emptyCells : Attr Style Bool
emptyCells =
    let getter = getEmptyCells
        setter = setEmptyCells
    in MkAttr getter setter

getFlex : Style -> IO String
getFlex (MkStyle style) = 
    mkForeign (FFun "%0.flex" [FPtr] FString) style
setFlex : Style -> String -> IO ()
setFlex (MkStyle style) (val) =
    mkForeign (FFun "%0.flex = %1" [FPtr, FString] FUnit) style val
flex : Attr Style String
flex =
    let getter = getFlex
        setter = setFlex
    in MkAttr getter setter

getFlexBasis : Style -> IO JsLength
getFlexBasis (MkStyle style) = cast `map`
    mkForeign (FFun "%0.flexBasis" [FPtr] FString) style
setFlexBasis : Style -> JsLength -> IO ()
setFlexBasis (MkStyle style) (val) =
    mkForeign (FFun "%0.flexBasis = %1" [FPtr, FString] FUnit)
              style (show val)
flexBasis : Attr Style JsLength
flexBasis =
    let getter = getFlexBasis
        setter = setFlexBasis
    in MkAttr getter setter

getFlexDirection : Style -> IO String
getFlexDirection (MkStyle style) = 
    mkForeign (FFun "%0.flexDirection" [FPtr] FString) style
setFlexDirection : Style -> String -> IO ()
setFlexDirection (MkStyle style) (val) =
    mkForeign (FFun "%0.flexDirection = %1" [FPtr, FString] FUnit) style val
flexDirection : Attr Style String
flexDirection =
    let getter = getFlexDirection
        setter = setFlexDirection
    in MkAttr getter setter

getFlexFlow : Style -> IO String
getFlexFlow (MkStyle style) = 
    mkForeign (FFun "%0.flexFlow" [FPtr] FString) style
setFlexFlow : Style -> String -> IO ()
setFlexFlow (MkStyle style) (val) =
    mkForeign (FFun "%0.flexFlow = %1" [FPtr, FString] FUnit) style val
flexFlow : Attr Style String
flexFlow =
    let getter = getFlexFlow
        setter = setFlexFlow
    in MkAttr getter setter

getFlexGrow : Style -> IO String
getFlexGrow (MkStyle style) = 
    mkForeign (FFun "%0.flexGrow" [FPtr] FString) style
setFlexGrow : Style -> String -> IO ()
setFlexGrow (MkStyle style) (val) =
    mkForeign (FFun "%0.flexGrow = %1" [FPtr, FString] FUnit) style val
flexGrow : Attr Style String
flexGrow =
    let getter = getFlexGrow
        setter = setFlexGrow
    in MkAttr getter setter

getFlexShrink : Style -> IO String
getFlexShrink (MkStyle style) = 
    mkForeign (FFun "%0.flexShrink" [FPtr] FString) style
setFlexShrink : Style -> String -> IO ()
setFlexShrink (MkStyle style) (val) =
    mkForeign (FFun "%0.flexShrink = %1" [FPtr, FString] FUnit) style val
flexShrink : Attr Style String
flexShrink =
    let getter = getFlexShrink
        setter = setFlexShrink
    in MkAttr getter setter

getFlexWrap : Style -> IO String
getFlexWrap (MkStyle style) = 
    mkForeign (FFun "%0.flexWrap" [FPtr] FString) style
setFlexWrap : Style -> String -> IO ()
setFlexWrap (MkStyle style) (val) =
    mkForeign (FFun "%0.flexWrap = %1" [FPtr, FString] FUnit) style val
flexWrap : Attr Style String
flexWrap =
    let getter = getFlexWrap
        setter = setFlexWrap
    in MkAttr getter setter

getCssFloat : Style -> IO String
getCssFloat (MkStyle style) = 
    mkForeign (FFun "%0.cssFloat" [FPtr] FString) style
setCssFloat : Style -> String -> IO ()
setCssFloat (MkStyle style) (val) =
    mkForeign (FFun "%0.cssFloat = %1" [FPtr, FString] FUnit) style val
cssFloat : Attr Style String
cssFloat =
    let getter = getCssFloat
        setter = setCssFloat
    in MkAttr getter setter

getFont : Style -> IO String
getFont (MkStyle style) = 
    mkForeign (FFun "%0.font" [FPtr] FString) style
setFont : Style -> String -> IO ()
setFont (MkStyle style) (val) =
    mkForeign (FFun "%0.font = %1" [FPtr, FString] FUnit) style val
font : Attr Style String
font =
    let getter = getFont
        setter = setFont
    in MkAttr getter setter

getFontFamily : Style -> IO String
getFontFamily (MkStyle style) = 
    mkForeign (FFun "%0.fontFamily" [FPtr] FString) style
setFontFamily : Style -> String -> IO ()
setFontFamily (MkStyle style) (val) =
    mkForeign (FFun "%0.fontFamily = %1" [FPtr, FString] FUnit) style val
fontFamily : Attr Style String
fontFamily =
    let getter = getFontFamily
        setter = setFontFamily
    in MkAttr getter setter

getFontSize : Style -> IO String
getFontSize (MkStyle style) = 
    mkForeign (FFun "%0.fontSize" [FPtr] FString) style
setFontSize : Style -> String -> IO ()
setFontSize (MkStyle style) (val) =
    mkForeign (FFun "%0.fontSize = %1" [FPtr, FString] FUnit) style val
fontSize : Attr Style String
fontSize =
    let getter = getFontSize
        setter = setFontSize
    in MkAttr getter setter

getFontStyle : Style -> IO String
getFontStyle (MkStyle style) = 
    mkForeign (FFun "%0.fontStyle" [FPtr] FString) style
setFontStyle : Style -> String -> IO ()
setFontStyle (MkStyle style) (val) =
    mkForeign (FFun "%0.fontStyle = %1" [FPtr, FString] FUnit) style val
fontStyle : Attr Style String
fontStyle =
    let getter = getFontStyle
        setter = setFontStyle
    in MkAttr getter setter

getFontVariant : Style -> IO String
getFontVariant (MkStyle style) = 
    mkForeign (FFun "%0.fontVariant" [FPtr] FString) style
setFontVariant : Style -> String -> IO ()
setFontVariant (MkStyle style) (val) =
    mkForeign (FFun "%0.fontVariant = %1" [FPtr, FString] FUnit) style val
fontVariant : Attr Style String
fontVariant =
    let getter = getFontVariant
        setter = setFontVariant
    in MkAttr getter setter

getFontWeight : Style -> IO String
getFontWeight (MkStyle style) = 
    mkForeign (FFun "%0.fontWeight" [FPtr] FString) style
setFontWeight : Style -> String -> IO ()
setFontWeight (MkStyle style) (val) =
    mkForeign (FFun "%0.fontWeight = %1" [FPtr, FString] FUnit) style val
fontWeight : Attr Style String
fontWeight =
    let getter = getFontWeight
        setter = setFontWeight
    in MkAttr getter setter

getFontSizeAdjust : Style -> IO String
getFontSizeAdjust (MkStyle style) = 
    mkForeign (FFun "%0.fontSizeAdjust" [FPtr] FString) style
setFontSizeAdjust : Style -> String -> IO ()
setFontSizeAdjust (MkStyle style) (val) =
    mkForeign (FFun "%0.fontSizeAdjust = %1" [FPtr, FString] FUnit) style val
fontSizeAdjust : Attr Style String
fontSizeAdjust =
    let getter = getFontSizeAdjust
        setter = setFontSizeAdjust
    in MkAttr getter setter

getFontStretch : Style -> IO String
getFontStretch (MkStyle style) = 
    mkForeign (FFun "%0.fontStretch" [FPtr] FString) style
setFontStretch : Style -> String -> IO ()
setFontStretch (MkStyle style) (val) =
    mkForeign (FFun "%0.fontStretch = %1" [FPtr, FString] FUnit) style val
fontStretch : Attr Style String
fontStretch =
    let getter = getFontStretch
        setter = setFontStretch
    in MkAttr getter setter

getHangingPunctuation : Style -> IO String
getHangingPunctuation (MkStyle style) = 
    mkForeign (FFun "%0.hangingPunctuation" [FPtr] FString) style
setHangingPunctuation : Style -> String -> IO ()
setHangingPunctuation (MkStyle style) (val) =
    mkForeign (FFun "%0.hangingPunctuation = %1" [FPtr, FString] FUnit) style val
hangingPunctuation : Attr Style String
hangingPunctuation =
    let getter = getHangingPunctuation
        setter = setHangingPunctuation
    in MkAttr getter setter

getHeight : Style -> IO JsLength
getHeight (MkStyle style) = cast `map`
    mkForeign (FFun "%0.height" [FPtr] FString) style
setHeight : Style -> JsLength -> IO ()
setHeight (MkStyle style) (val) =
    mkForeign (FFun "%0.height = %1" [FPtr, FString] FUnit)
              style (show val)
height : Attr Style JsLength
height =
    let getter = getHeight
        setter = setHeight
    in MkAttr getter setter

getHyphens : Style -> IO String
getHyphens (MkStyle style) = 
    mkForeign (FFun "%0.hyphens" [FPtr] FString) style
setHyphens : Style -> String -> IO ()
setHyphens (MkStyle style) (val) =
    mkForeign (FFun "%0.hyphens = %1" [FPtr, FString] FUnit) style val
hyphens : Attr Style String
hyphens =
    let getter = getHyphens
        setter = setHyphens
    in MkAttr getter setter

getIcon : Style -> IO String
getIcon (MkStyle style) = 
    mkForeign (FFun "%0.icon" [FPtr] FString) style
setIcon : Style -> String -> IO ()
setIcon (MkStyle style) (val) =
    mkForeign (FFun "%0.icon = %1" [FPtr, FString] FUnit) style val
icon : Attr Style String
icon =
    let getter = getIcon
        setter = setIcon
    in MkAttr getter setter

getImageOrientation : Style -> IO String
getImageOrientation (MkStyle style) = 
    mkForeign (FFun "%0.imageOrientation" [FPtr] FString) style
setImageOrientation : Style -> String -> IO ()
setImageOrientation (MkStyle style) (val) =
    mkForeign (FFun "%0.imageOrientation = %1" [FPtr, FString] FUnit) style val
imageOrientation : Attr Style String
imageOrientation =
    let getter = getImageOrientation
        setter = setImageOrientation
    in MkAttr getter setter

getJustifyContent : Style -> IO String
getJustifyContent (MkStyle style) = 
    mkForeign (FFun "%0.justifyContent" [FPtr] FString) style
setJustifyContent : Style -> String -> IO ()
setJustifyContent (MkStyle style) (val) =
    mkForeign (FFun "%0.justifyContent = %1" [FPtr, FString] FUnit) style val
justifyContent : Attr Style String
justifyContent =
    let getter = getJustifyContent
        setter = setJustifyContent
    in MkAttr getter setter

getLeft : Style -> IO JsLength
getLeft (MkStyle style) = cast `map`
    mkForeign (FFun "%0.left" [FPtr] FString) style
setLeft : Style -> JsLength -> IO ()
setLeft (MkStyle style) (val) =
    mkForeign (FFun "%0.left = %1" [FPtr, FString] FUnit)
              style (show val)
left : Attr Style JsLength
left =
    let getter = getLeft
        setter = setLeft
    in MkAttr getter setter

getLetterSpacing : Style -> IO JsLength
getLetterSpacing (MkStyle style) = cast `map`
    mkForeign (FFun "%0.letterSpacing" [FPtr] FString) style
setLetterSpacing : Style -> JsLength -> IO ()
setLetterSpacing (MkStyle style) (val) =
    mkForeign (FFun "%0.letterSpacing = %1" [FPtr, FString] FUnit)
              style (show val)
letterSpacing : Attr Style JsLength
letterSpacing =
    let getter = getLetterSpacing
        setter = setLetterSpacing
    in MkAttr getter setter

getLineHeight : Style -> IO JsLength
getLineHeight (MkStyle style) = cast `map`
    mkForeign (FFun "%0.lineHeight" [FPtr] FString) style
setLineHeight : Style -> JsLength -> IO ()
setLineHeight (MkStyle style) (val) =
    mkForeign (FFun "%0.lineHeight = %1" [FPtr, FString] FUnit)
              style (show val)
lineHeight : Attr Style JsLength
lineHeight =
    let getter = getLineHeight
        setter = setLineHeight
    in MkAttr getter setter

getListStyle : Style -> IO String
getListStyle (MkStyle style) = 
    mkForeign (FFun "%0.listStyle" [FPtr] FString) style
setListStyle : Style -> String -> IO ()
setListStyle (MkStyle style) (val) =
    mkForeign (FFun "%0.listStyle = %1" [FPtr, FString] FUnit) style val
listStyle : Attr Style String
listStyle =
    let getter = getListStyle
        setter = setListStyle
    in MkAttr getter setter

getListStyleImage : Style -> IO String
getListStyleImage (MkStyle style) = 
    mkForeign (FFun "%0.listStyleImage" [FPtr] FString) style
setListStyleImage : Style -> String -> IO ()
setListStyleImage (MkStyle style) (val) =
    mkForeign (FFun "%0.listStyleImage = %1" [FPtr, FString] FUnit) style val
listStyleImage : Attr Style String
listStyleImage =
    let getter = getListStyleImage
        setter = setListStyleImage
    in MkAttr getter setter

getListStylePosition : Style -> IO String
getListStylePosition (MkStyle style) = 
    mkForeign (FFun "%0.listStylePosition" [FPtr] FString) style
setListStylePosition : Style -> String -> IO ()
setListStylePosition (MkStyle style) (val) =
    mkForeign (FFun "%0.listStylePosition = %1" [FPtr, FString] FUnit) style val
listStylePosition : Attr Style String
listStylePosition =
    let getter = getListStylePosition
        setter = setListStylePosition
    in MkAttr getter setter

getListStyleType : Style -> IO String
getListStyleType (MkStyle style) = 
    mkForeign (FFun "%0.listStyleType" [FPtr] FString) style
setListStyleType : Style -> String -> IO ()
setListStyleType (MkStyle style) (val) =
    mkForeign (FFun "%0.listStyleType = %1" [FPtr, FString] FUnit) style val
listStyleType : Attr Style String
listStyleType =
    let getter = getListStyleType
        setter = setListStyleType
    in MkAttr getter setter

getMargin : Style -> IO String
getMargin (MkStyle style) = 
    mkForeign (FFun "%0.margin" [FPtr] FString) style
setMargin : Style -> String -> IO ()
setMargin (MkStyle style) (val) =
    mkForeign (FFun "%0.margin = %1" [FPtr, FString] FUnit) style val
margin : Attr Style String
margin =
    let getter = getMargin
        setter = setMargin
    in MkAttr getter setter

getMarginBottom : Style -> IO JsLength
getMarginBottom (MkStyle style) = cast `map`
    mkForeign (FFun "%0.marginBottom" [FPtr] FString) style
setMarginBottom : Style -> JsLength -> IO ()
setMarginBottom (MkStyle style) (val) =
    mkForeign (FFun "%0.marginBottom = %1" [FPtr, FString] FUnit)
              style (show val)
marginBottom : Attr Style JsLength
marginBottom =
    let getter = getMarginBottom
        setter = setMarginBottom
    in MkAttr getter setter

getMarginLeft : Style -> IO JsLength
getMarginLeft (MkStyle style) = cast `map`
    mkForeign (FFun "%0.marginLeft" [FPtr] FString) style
setMarginLeft : Style -> JsLength -> IO ()
setMarginLeft (MkStyle style) (val) =
    mkForeign (FFun "%0.marginLeft = %1" [FPtr, FString] FUnit)
              style (show val)
marginLeft : Attr Style JsLength
marginLeft =
    let getter = getMarginLeft
        setter = setMarginLeft
    in MkAttr getter setter

getMarginRight : Style -> IO JsLength
getMarginRight (MkStyle style) = cast `map`
    mkForeign (FFun "%0.marginRight" [FPtr] FString) style
setMarginRight : Style -> JsLength -> IO ()
setMarginRight (MkStyle style) (val) =
    mkForeign (FFun "%0.marginRight = %1" [FPtr, FString] FUnit)
              style (show val)
marginRight : Attr Style JsLength
marginRight =
    let getter = getMarginRight
        setter = setMarginRight
    in MkAttr getter setter

getMarginTop : Style -> IO JsLength
getMarginTop (MkStyle style) = cast `map`
    mkForeign (FFun "%0.marginTop" [FPtr] FString) style
setMarginTop : Style -> JsLength -> IO ()
setMarginTop (MkStyle style) (val) =
    mkForeign (FFun "%0.marginTop = %1" [FPtr, FString] FUnit)
              style (show val)
marginTop : Attr Style JsLength
marginTop =
    let getter = getMarginTop
        setter = setMarginTop
    in MkAttr getter setter

getMaxHeight : Style -> IO JsLength
getMaxHeight (MkStyle style) = cast `map`
    mkForeign (FFun "%0.maxHeight" [FPtr] FString) style
setMaxHeight : Style -> JsLength -> IO ()
setMaxHeight (MkStyle style) (val) =
    mkForeign (FFun "%0.maxHeight = %1" [FPtr, FString] FUnit)
              style (show val)
maxHeight : Attr Style JsLength
maxHeight =
    let getter = getMaxHeight
        setter = setMaxHeight
    in MkAttr getter setter

getMaxWidth : Style -> IO JsLength
getMaxWidth (MkStyle style) = cast `map`
    mkForeign (FFun "%0.maxWidth" [FPtr] FString) style
setMaxWidth : Style -> JsLength -> IO ()
setMaxWidth (MkStyle style) (val) =
    mkForeign (FFun "%0.maxWidth = %1" [FPtr, FString] FUnit)
              style (show val)
maxWidth : Attr Style JsLength
maxWidth =
    let getter = getMaxWidth
        setter = setMaxWidth
    in MkAttr getter setter

getMinHeight : Style -> IO JsLength
getMinHeight (MkStyle style) = cast `map`
    mkForeign (FFun "%0.minHeight" [FPtr] FString) style
setMinHeight : Style -> JsLength -> IO ()
setMinHeight (MkStyle style) (val) =
    mkForeign (FFun "%0.minHeight = %1" [FPtr, FString] FUnit)
              style (show val)
minHeight : Attr Style JsLength
minHeight =
    let getter = getMinHeight
        setter = setMinHeight
    in MkAttr getter setter

getMinWidth : Style -> IO JsLength
getMinWidth (MkStyle style) = cast `map`
    mkForeign (FFun "%0.minWidth" [FPtr] FString) style
setMinWidth : Style -> JsLength -> IO ()
setMinWidth (MkStyle style) (val) =
    mkForeign (FFun "%0.minWidth = %1" [FPtr, FString] FUnit)
              style (show val)
minWidth : Attr Style JsLength
minWidth =
    let getter = getMinWidth
        setter = setMinWidth
    in MkAttr getter setter

getNavDown : Style -> IO String
getNavDown (MkStyle style) = 
    mkForeign (FFun "%0.navDown" [FPtr] FString) style
setNavDown : Style -> String -> IO ()
setNavDown (MkStyle style) (val) =
    mkForeign (FFun "%0.navDown = %1" [FPtr, FString] FUnit) style val
navDown : Attr Style String
navDown =
    let getter = getNavDown
        setter = setNavDown
    in MkAttr getter setter

getNavIndex : Style -> IO String
getNavIndex (MkStyle style) = 
    mkForeign (FFun "%0.navIndex" [FPtr] FString) style
setNavIndex : Style -> String -> IO ()
setNavIndex (MkStyle style) (val) =
    mkForeign (FFun "%0.navIndex = %1" [FPtr, FString] FUnit) style val
navIndex : Attr Style String
navIndex =
    let getter = getNavIndex
        setter = setNavIndex
    in MkAttr getter setter

getNavLeft : Style -> IO String
getNavLeft (MkStyle style) = 
    mkForeign (FFun "%0.navLeft" [FPtr] FString) style
setNavLeft : Style -> String -> IO ()
setNavLeft (MkStyle style) (val) =
    mkForeign (FFun "%0.navLeft = %1" [FPtr, FString] FUnit) style val
navLeft : Attr Style String
navLeft =
    let getter = getNavLeft
        setter = setNavLeft
    in MkAttr getter setter

getNavRight : Style -> IO String
getNavRight (MkStyle style) = 
    mkForeign (FFun "%0.navRight" [FPtr] FString) style
setNavRight : Style -> String -> IO ()
setNavRight (MkStyle style) (val) =
    mkForeign (FFun "%0.navRight = %1" [FPtr, FString] FUnit) style val
navRight : Attr Style String
navRight =
    let getter = getNavRight
        setter = setNavRight
    in MkAttr getter setter

getNavUp : Style -> IO String
getNavUp (MkStyle style) = 
    mkForeign (FFun "%0.navUp" [FPtr] FString) style
setNavUp : Style -> String -> IO ()
setNavUp (MkStyle style) (val) =
    mkForeign (FFun "%0.navUp = %1" [FPtr, FString] FUnit) style val
navUp : Attr Style String
navUp =
    let getter = getNavUp
        setter = setNavUp
    in MkAttr getter setter

getOpacity : Style -> IO Float
getOpacity (MkStyle style) = cast `map`
    mkForeign (FFun "%0.opacity" [FPtr] FString) style
setOpacity : Style -> Float -> IO ()
setOpacity (MkStyle style) (val) =
    mkForeign (FFun "%0.opacity = %1" [FPtr, FFloat] FUnit)
              style val
opacity : Attr Style Float
opacity =
    let getter = getOpacity
        setter = setOpacity
    in MkAttr getter setter

getOrder : Style -> IO String
getOrder (MkStyle style) = 
    mkForeign (FFun "%0.order" [FPtr] FString) style
setOrder : Style -> String -> IO ()
setOrder (MkStyle style) (val) =
    mkForeign (FFun "%0.order = %1" [FPtr, FString] FUnit) style val
order : Attr Style String
order =
    let getter = getOrder
        setter = setOrder
    in MkAttr getter setter

getOrphans : Style -> IO String
getOrphans (MkStyle style) = 
    mkForeign (FFun "%0.orphans" [FPtr] FString) style
setOrphans : Style -> String -> IO ()
setOrphans (MkStyle style) (val) =
    mkForeign (FFun "%0.orphans = %1" [FPtr, FString] FUnit) style val
orphans : Attr Style String
orphans =
    let getter = getOrphans
        setter = setOrphans
    in MkAttr getter setter

getOutline : Style -> IO String
getOutline (MkStyle style) = 
    mkForeign (FFun "%0.outline" [FPtr] FString) style
setOutline : Style -> String -> IO ()
setOutline (MkStyle style) (val) =
    mkForeign (FFun "%0.outline = %1" [FPtr, FString] FUnit) style val
outline : Attr Style String
outline =
    let getter = getOutline
        setter = setOutline
    in MkAttr getter setter

getOutlineColor : Style -> IO String
getOutlineColor (MkStyle style) = 
    mkForeign (FFun "%0.outlineColor" [FPtr] FString) style
setOutlineColor : Style -> String -> IO ()
setOutlineColor (MkStyle style) (val) =
    mkForeign (FFun "%0.outlineColor = %1" [FPtr, FString] FUnit) style val
outlineColor : Attr Style String
outlineColor =
    let getter = getOutlineColor
        setter = setOutlineColor
    in MkAttr getter setter

getOutlineOffset : Style -> IO JsLength
getOutlineOffset (MkStyle style) = cast `map`
    mkForeign (FFun "%0.outlineOffset" [FPtr] FString) style
setOutlineOffset : Style -> JsLength -> IO ()
setOutlineOffset (MkStyle style) (val) =
    mkForeign (FFun "%0.outlineOffset = %1" [FPtr, FString] FUnit)
              style (show val)
outlineOffset : Attr Style JsLength
outlineOffset =
    let getter = getOutlineOffset
        setter = setOutlineOffset
    in MkAttr getter setter

getOutlineStyle : Style -> IO String
getOutlineStyle (MkStyle style) = 
    mkForeign (FFun "%0.outlineStyle" [FPtr] FString) style
setOutlineStyle : Style -> String -> IO ()
setOutlineStyle (MkStyle style) (val) =
    mkForeign (FFun "%0.outlineStyle = %1" [FPtr, FString] FUnit) style val
outlineStyle : Attr Style String
outlineStyle =
    let getter = getOutlineStyle
        setter = setOutlineStyle
    in MkAttr getter setter

getOutlineWidth : Style -> IO String
getOutlineWidth (MkStyle style) = 
    mkForeign (FFun "%0.outlineWidth" [FPtr] FString) style
setOutlineWidth : Style -> String -> IO ()
setOutlineWidth (MkStyle style) (val) =
    mkForeign (FFun "%0.outlineWidth = %1" [FPtr, FString] FUnit) style val
outlineWidth : Attr Style String
outlineWidth =
    let getter = getOutlineWidth
        setter = setOutlineWidth
    in MkAttr getter setter

getOverflow : Style -> IO String
getOverflow (MkStyle style) = 
    mkForeign (FFun "%0.overflow" [FPtr] FString) style
setOverflow : Style -> String -> IO ()
setOverflow (MkStyle style) (val) =
    mkForeign (FFun "%0.overflow = %1" [FPtr, FString] FUnit) style val
overflow : Attr Style String
overflow =
    let getter = getOverflow
        setter = setOverflow
    in MkAttr getter setter

getOverflowX : Style -> IO String
getOverflowX (MkStyle style) = 
    mkForeign (FFun "%0.overflowX" [FPtr] FString) style
setOverflowX : Style -> String -> IO ()
setOverflowX (MkStyle style) (val) =
    mkForeign (FFun "%0.overflowX = %1" [FPtr, FString] FUnit) style val
overflowX : Attr Style String
overflowX =
    let getter = getOverflowX
        setter = setOverflowX
    in MkAttr getter setter

getOverflowY : Style -> IO String
getOverflowY (MkStyle style) = 
    mkForeign (FFun "%0.overflowY" [FPtr] FString) style
setOverflowY : Style -> String -> IO ()
setOverflowY (MkStyle style) (val) =
    mkForeign (FFun "%0.overflowY = %1" [FPtr, FString] FUnit) style val
overflowY : Attr Style String
overflowY =
    let getter = getOverflowY
        setter = setOverflowY
    in MkAttr getter setter

getPadding : Style -> IO String
getPadding (MkStyle style) = 
    mkForeign (FFun "%0.padding" [FPtr] FString) style
setPadding : Style -> String -> IO ()
setPadding (MkStyle style) (val) =
    mkForeign (FFun "%0.padding = %1" [FPtr, FString] FUnit) style val
padding : Attr Style String
padding =
    let getter = getPadding
        setter = setPadding
    in MkAttr getter setter

getPaddingBottom : Style -> IO JsLength
getPaddingBottom (MkStyle style) = cast `map`
    mkForeign (FFun "%0.paddingBottom" [FPtr] FString) style
setPaddingBottom : Style -> JsLength -> IO ()
setPaddingBottom (MkStyle style) (val) =
    mkForeign (FFun "%0.paddingBottom = %1" [FPtr, FString] FUnit)
              style (show val)
paddingBottom : Attr Style JsLength
paddingBottom =
    let getter = getPaddingBottom
        setter = setPaddingBottom
    in MkAttr getter setter

getPaddingLeft : Style -> IO JsLength
getPaddingLeft (MkStyle style) = cast `map`
    mkForeign (FFun "%0.paddingLeft" [FPtr] FString) style
setPaddingLeft : Style -> JsLength -> IO ()
setPaddingLeft (MkStyle style) (val) =
    mkForeign (FFun "%0.paddingLeft = %1" [FPtr, FString] FUnit)
              style (show val)
paddingLeft : Attr Style JsLength
paddingLeft =
    let getter = getPaddingLeft
        setter = setPaddingLeft
    in MkAttr getter setter

getPaddingRight : Style -> IO JsLength
getPaddingRight (MkStyle style) = cast `map`
    mkForeign (FFun "%0.paddingRight" [FPtr] FString) style
setPaddingRight : Style -> JsLength -> IO ()
setPaddingRight (MkStyle style) (val) =
    mkForeign (FFun "%0.paddingRight = %1" [FPtr, FString] FUnit)
              style (show val)
paddingRight : Attr Style JsLength
paddingRight =
    let getter = getPaddingRight
        setter = setPaddingRight
    in MkAttr getter setter

getPaddingTop : Style -> IO JsLength
getPaddingTop (MkStyle style) = cast `map`
    mkForeign (FFun "%0.paddingTop" [FPtr] FString) style
setPaddingTop : Style -> JsLength -> IO ()
setPaddingTop (MkStyle style) (val) =
    mkForeign (FFun "%0.paddingTop = %1" [FPtr, FString] FUnit)
              style (show val)
paddingTop : Attr Style JsLength
paddingTop =
    let getter = getPaddingTop
        setter = setPaddingTop
    in MkAttr getter setter

getPageBreakAfter : Style -> IO String
getPageBreakAfter (MkStyle style) = 
    mkForeign (FFun "%0.pageBreakAfter" [FPtr] FString) style
setPageBreakAfter : Style -> String -> IO ()
setPageBreakAfter (MkStyle style) (val) =
    mkForeign (FFun "%0.pageBreakAfter = %1" [FPtr, FString] FUnit) style val
pageBreakAfter : Attr Style String
pageBreakAfter =
    let getter = getPageBreakAfter
        setter = setPageBreakAfter
    in MkAttr getter setter

getPageBreakBefore : Style -> IO String
getPageBreakBefore (MkStyle style) = 
    mkForeign (FFun "%0.pageBreakBefore" [FPtr] FString) style
setPageBreakBefore : Style -> String -> IO ()
setPageBreakBefore (MkStyle style) (val) =
    mkForeign (FFun "%0.pageBreakBefore = %1" [FPtr, FString] FUnit) style val
pageBreakBefore : Attr Style String
pageBreakBefore =
    let getter = getPageBreakBefore
        setter = setPageBreakBefore
    in MkAttr getter setter

getPageBreakInside : Style -> IO String
getPageBreakInside (MkStyle style) = 
    mkForeign (FFun "%0.pageBreakInside" [FPtr] FString) style
setPageBreakInside : Style -> String -> IO ()
setPageBreakInside (MkStyle style) (val) =
    mkForeign (FFun "%0.pageBreakInside = %1" [FPtr, FString] FUnit) style val
pageBreakInside : Attr Style String
pageBreakInside =
    let getter = getPageBreakInside
        setter = setPageBreakInside
    in MkAttr getter setter

getPerspective : Style -> IO String
getPerspective (MkStyle style) = 
    mkForeign (FFun "%0.perspective" [FPtr] FString) style
setPerspective : Style -> String -> IO ()
setPerspective (MkStyle style) (val) =
    mkForeign (FFun "%0.perspective = %1" [FPtr, FString] FUnit) style val
perspective : Attr Style String
perspective =
    let getter = getPerspective
        setter = setPerspective
    in MkAttr getter setter

getPerspectiveOrigin : Style -> IO String
getPerspectiveOrigin (MkStyle style) = 
    mkForeign (FFun "%0.perspectiveOrigin" [FPtr] FString) style
setPerspectiveOrigin : Style -> String -> IO ()
setPerspectiveOrigin (MkStyle style) (val) =
    mkForeign (FFun "%0.perspectiveOrigin = %1" [FPtr, FString] FUnit) style val
perspectiveOrigin : Attr Style String
perspectiveOrigin =
    let getter = getPerspectiveOrigin
        setter = setPerspectiveOrigin
    in MkAttr getter setter

getPosition : Style -> IO String
getPosition (MkStyle style) = 
    mkForeign (FFun "%0.position" [FPtr] FString) style
setPosition : Style -> String -> IO ()
setPosition (MkStyle style) (val) =
    mkForeign (FFun "%0.position = %1" [FPtr, FString] FUnit) style val
position : Attr Style String
position =
    let getter = getPosition
        setter = setPosition
    in MkAttr getter setter

getQuotes : Style -> IO String
getQuotes (MkStyle style) = 
    mkForeign (FFun "%0.quotes" [FPtr] FString) style
setQuotes : Style -> String -> IO ()
setQuotes (MkStyle style) (val) =
    mkForeign (FFun "%0.quotes = %1" [FPtr, FString] FUnit) style val
quotes : Attr Style String
quotes =
    let getter = getQuotes
        setter = setQuotes
    in MkAttr getter setter

getResize : Style -> IO String
getResize (MkStyle style) = 
    mkForeign (FFun "%0.resize" [FPtr] FString) style
setResize : Style -> String -> IO ()
setResize (MkStyle style) (val) =
    mkForeign (FFun "%0.resize = %1" [FPtr, FString] FUnit) style val
resize : Attr Style String
resize =
    let getter = getResize
        setter = setResize
    in MkAttr getter setter

getRight : Style -> IO JsLength
getRight (MkStyle style) = cast `map`
    mkForeign (FFun "%0.right" [FPtr] FString) style
setRight : Style -> JsLength -> IO ()
setRight (MkStyle style) (val) =
    mkForeign (FFun "%0.right = %1" [FPtr, FString] FUnit)
              style (show val)
right : Attr Style JsLength
right =
    let getter = getRight
        setter = setRight
    in MkAttr getter setter

getTableLayout : Style -> IO String
getTableLayout (MkStyle style) = 
    mkForeign (FFun "%0.tableLayout" [FPtr] FString) style
setTableLayout : Style -> String -> IO ()
setTableLayout (MkStyle style) (val) =
    mkForeign (FFun "%0.tableLayout = %1" [FPtr, FString] FUnit) style val
tableLayout : Attr Style String
tableLayout =
    let getter = getTableLayout
        setter = setTableLayout
    in MkAttr getter setter

getTabSize : Style -> IO String
getTabSize (MkStyle style) = 
    mkForeign (FFun "%0.tabSize" [FPtr] FString) style
setTabSize : Style -> String -> IO ()
setTabSize (MkStyle style) (val) =
    mkForeign (FFun "%0.tabSize = %1" [FPtr, FString] FUnit) style val
tabSize : Attr Style String
tabSize =
    let getter = getTabSize
        setter = setTabSize
    in MkAttr getter setter

getTextAlign : Style -> IO String
getTextAlign (MkStyle style) = 
    mkForeign (FFun "%0.textAlign" [FPtr] FString) style
setTextAlign : Style -> String -> IO ()
setTextAlign (MkStyle style) (val) =
    mkForeign (FFun "%0.textAlign = %1" [FPtr, FString] FUnit) style val
textAlign : Attr Style String
textAlign =
    let getter = getTextAlign
        setter = setTextAlign
    in MkAttr getter setter

getTextAlignLast : Style -> IO String
getTextAlignLast (MkStyle style) = 
    mkForeign (FFun "%0.textAlignLast" [FPtr] FString) style
setTextAlignLast : Style -> String -> IO ()
setTextAlignLast (MkStyle style) (val) =
    mkForeign (FFun "%0.textAlignLast = %1" [FPtr, FString] FUnit) style val
textAlignLast : Attr Style String
textAlignLast =
    let getter = getTextAlignLast
        setter = setTextAlignLast
    in MkAttr getter setter

getTextDecoration : Style -> IO String
getTextDecoration (MkStyle style) = 
    mkForeign (FFun "%0.textDecoration" [FPtr] FString) style
setTextDecoration : Style -> String -> IO ()
setTextDecoration (MkStyle style) (val) =
    mkForeign (FFun "%0.textDecoration = %1" [FPtr, FString] FUnit) style val
textDecoration : Attr Style String
textDecoration =
    let getter = getTextDecoration
        setter = setTextDecoration
    in MkAttr getter setter

getTextDecorationColor : Style -> IO String
getTextDecorationColor (MkStyle style) = 
    mkForeign (FFun "%0.textDecorationColor" [FPtr] FString) style
setTextDecorationColor : Style -> String -> IO ()
setTextDecorationColor (MkStyle style) (val) =
    mkForeign (FFun "%0.textDecorationColor = %1" [FPtr, FString] FUnit) style val
textDecorationColor : Attr Style String
textDecorationColor =
    let getter = getTextDecorationColor
        setter = setTextDecorationColor
    in MkAttr getter setter

getTextDecorationLine : Style -> IO String
getTextDecorationLine (MkStyle style) = 
    mkForeign (FFun "%0.textDecorationLine" [FPtr] FString) style
setTextDecorationLine : Style -> String -> IO ()
setTextDecorationLine (MkStyle style) (val) =
    mkForeign (FFun "%0.textDecorationLine = %1" [FPtr, FString] FUnit) style val
textDecorationLine : Attr Style String
textDecorationLine =
    let getter = getTextDecorationLine
        setter = setTextDecorationLine
    in MkAttr getter setter

getTextDecorationStyle : Style -> IO String
getTextDecorationStyle (MkStyle style) = 
    mkForeign (FFun "%0.textDecorationStyle" [FPtr] FString) style
setTextDecorationStyle : Style -> String -> IO ()
setTextDecorationStyle (MkStyle style) (val) =
    mkForeign (FFun "%0.textDecorationStyle = %1" [FPtr, FString] FUnit) style val
textDecorationStyle : Attr Style String
textDecorationStyle =
    let getter = getTextDecorationStyle
        setter = setTextDecorationStyle
    in MkAttr getter setter

getTextIndent : Style -> IO String
getTextIndent (MkStyle style) = 
    mkForeign (FFun "%0.textIndent" [FPtr] FString) style
setTextIndent : Style -> String -> IO ()
setTextIndent (MkStyle style) (val) =
    mkForeign (FFun "%0.textIndent = %1" [FPtr, FString] FUnit) style val
textIndent : Attr Style String
textIndent =
    let getter = getTextIndent
        setter = setTextIndent
    in MkAttr getter setter

getTextJustify : Style -> IO String
getTextJustify (MkStyle style) = 
    mkForeign (FFun "%0.textJustify" [FPtr] FString) style
setTextJustify : Style -> String -> IO ()
setTextJustify (MkStyle style) (val) =
    mkForeign (FFun "%0.textJustify = %1" [FPtr, FString] FUnit) style val
textJustify : Attr Style String
textJustify =
    let getter = getTextJustify
        setter = setTextJustify
    in MkAttr getter setter

getTextOverflow : Style -> IO String
getTextOverflow (MkStyle style) = 
    mkForeign (FFun "%0.textOverflow" [FPtr] FString) style
setTextOverflow : Style -> String -> IO ()
setTextOverflow (MkStyle style) (val) =
    mkForeign (FFun "%0.textOverflow = %1" [FPtr, FString] FUnit) style val
textOverflow : Attr Style String
textOverflow =
    let getter = getTextOverflow
        setter = setTextOverflow
    in MkAttr getter setter

getTextShadow : Style -> IO String
getTextShadow (MkStyle style) = 
    mkForeign (FFun "%0.textShadow" [FPtr] FString) style
setTextShadow : Style -> String -> IO ()
setTextShadow (MkStyle style) (val) =
    mkForeign (FFun "%0.textShadow = %1" [FPtr, FString] FUnit) style val
textShadow : Attr Style String
textShadow =
    let getter = getTextShadow
        setter = setTextShadow
    in MkAttr getter setter

getTextTransform : Style -> IO String
getTextTransform (MkStyle style) = 
    mkForeign (FFun "%0.textTransform" [FPtr] FString) style
setTextTransform : Style -> String -> IO ()
setTextTransform (MkStyle style) (val) =
    mkForeign (FFun "%0.textTransform = %1" [FPtr, FString] FUnit) style val
textTransform : Attr Style String
textTransform =
    let getter = getTextTransform
        setter = setTextTransform
    in MkAttr getter setter

getTop : Style -> IO JsLength
getTop (MkStyle style) = cast `map`
    mkForeign (FFun "%0.top" [FPtr] FString) style
setTop : Style -> JsLength -> IO ()
setTop (MkStyle style) (val) =
    mkForeign (FFun "%0.top = %1" [FPtr, FString] FUnit)
              style (show val)
top : Attr Style JsLength
top =
    let getter = getTop
        setter = setTop
    in MkAttr getter setter

getTransform : Style -> IO String
getTransform (MkStyle style) = 
    mkForeign (FFun "%0.transform" [FPtr] FString) style
setTransform : Style -> String -> IO ()
setTransform (MkStyle style) (val) =
    mkForeign (FFun "%0.transform = %1" [FPtr, FString] FUnit) style val
transform : Attr Style String
transform =
    let getter = getTransform
        setter = setTransform
    in MkAttr getter setter

getTransformOrigin : Style -> IO String
getTransformOrigin (MkStyle style) = 
    mkForeign (FFun "%0.transformOrigin" [FPtr] FString) style
setTransformOrigin : Style -> String -> IO ()
setTransformOrigin (MkStyle style) (val) =
    mkForeign (FFun "%0.transformOrigin = %1" [FPtr, FString] FUnit) style val
transformOrigin : Attr Style String
transformOrigin =
    let getter = getTransformOrigin
        setter = setTransformOrigin
    in MkAttr getter setter

getTransformStyle : Style -> IO String
getTransformStyle (MkStyle style) = 
    mkForeign (FFun "%0.transformStyle" [FPtr] FString) style
setTransformStyle : Style -> String -> IO ()
setTransformStyle (MkStyle style) (val) =
    mkForeign (FFun "%0.transformStyle = %1" [FPtr, FString] FUnit) style val
transformStyle : Attr Style String
transformStyle =
    let getter = getTransformStyle
        setter = setTransformStyle
    in MkAttr getter setter

getTransition : Style -> IO String
getTransition (MkStyle style) = 
    mkForeign (FFun "%0.transition" [FPtr] FString) style
setTransition : Style -> String -> IO ()
setTransition (MkStyle style) (val) =
    mkForeign (FFun "%0.transition = %1" [FPtr, FString] FUnit) style val
transition : Attr Style String
transition =
    let getter = getTransition
        setter = setTransition
    in MkAttr getter setter

getTransitionProperty : Style -> IO String
getTransitionProperty (MkStyle style) = 
    mkForeign (FFun "%0.transitionProperty" [FPtr] FString) style
setTransitionProperty : Style -> String -> IO ()
setTransitionProperty (MkStyle style) (val) =
    mkForeign (FFun "%0.transitionProperty = %1" [FPtr, FString] FUnit) style val
transitionProperty : Attr Style String
transitionProperty =
    let getter = getTransitionProperty
        setter = setTransitionProperty
    in MkAttr getter setter

getTransitionDuration : Style -> IO JsTime
getTransitionDuration (MkStyle style) = cast `map`
    mkForeign (FFun "%0.transitionDuration" [FPtr] FString) style
setTransitionDuration : Style -> JsTime -> IO ()
setTransitionDuration (MkStyle style) (val) =
    mkForeign (FFun "%0.transitionDuration = %1" [FPtr, FString] FUnit)
              style (show val)
transitionDuration : Attr Style JsTime
transitionDuration =
    let getter = getTransitionDuration
        setter = setTransitionDuration
    in MkAttr getter setter

getTransitionTimingFunction : Style -> IO String
getTransitionTimingFunction (MkStyle style) = 
    mkForeign (FFun "%0.transitionTimingFunction" [FPtr] FString) style
setTransitionTimingFunction : Style -> String -> IO ()
setTransitionTimingFunction (MkStyle style) (val) =
    mkForeign (FFun "%0.transitionTimingFunction = %1" [FPtr, FString] FUnit) style val
transitionTimingFunction : Attr Style String
transitionTimingFunction =
    let getter = getTransitionTimingFunction
        setter = setTransitionTimingFunction
    in MkAttr getter setter

getTransitionDelay : Style -> IO JsTime
getTransitionDelay (MkStyle style) = cast `map`
    mkForeign (FFun "%0.transitionDelay" [FPtr] FString) style
setTransitionDelay : Style -> JsTime -> IO ()
setTransitionDelay (MkStyle style) (val) =
    mkForeign (FFun "%0.transitionDelay = %1" [FPtr, FString] FUnit)
              style (show val)
transitionDelay : Attr Style JsTime
transitionDelay =
    let getter = getTransitionDelay
        setter = setTransitionDelay
    in MkAttr getter setter

getUnicodeBidi : Style -> IO String
getUnicodeBidi (MkStyle style) = 
    mkForeign (FFun "%0.unicodeBidi" [FPtr] FString) style
setUnicodeBidi : Style -> String -> IO ()
setUnicodeBidi (MkStyle style) (val) =
    mkForeign (FFun "%0.unicodeBidi = %1" [FPtr, FString] FUnit) style val
unicodeBidi : Attr Style String
unicodeBidi =
    let getter = getUnicodeBidi
        setter = setUnicodeBidi
    in MkAttr getter setter

getVerticalAlign : Style -> IO String
getVerticalAlign (MkStyle style) = 
    mkForeign (FFun "%0.verticalAlign" [FPtr] FString) style
setVerticalAlign : Style -> String -> IO ()
setVerticalAlign (MkStyle style) (val) =
    mkForeign (FFun "%0.verticalAlign = %1" [FPtr, FString] FUnit) style val
verticalAlign : Attr Style String
verticalAlign =
    let getter = getVerticalAlign
        setter = setVerticalAlign
    in MkAttr getter setter

getVisibility : Style -> IO String
getVisibility (MkStyle style) = 
    mkForeign (FFun "%0.visibility" [FPtr] FString) style
setVisibility : Style -> String -> IO ()
setVisibility (MkStyle style) (val) =
    mkForeign (FFun "%0.visibility = %1" [FPtr, FString] FUnit) style val
visibility : Attr Style String
visibility =
    let getter = getVisibility
        setter = setVisibility
    in MkAttr getter setter

getWhiteSpace : Style -> IO String
getWhiteSpace (MkStyle style) = 
    mkForeign (FFun "%0.whiteSpace" [FPtr] FString) style
setWhiteSpace : Style -> String -> IO ()
setWhiteSpace (MkStyle style) (val) =
    mkForeign (FFun "%0.whiteSpace = %1" [FPtr, FString] FUnit) style val
whiteSpace : Attr Style String
whiteSpace =
    let getter = getWhiteSpace
        setter = setWhiteSpace
    in MkAttr getter setter

getWidth : Style -> IO JsLength
getWidth (MkStyle style) = cast `map`
    mkForeign (FFun "%0.width" [FPtr] FString) style
setWidth : Style -> JsLength -> IO ()
setWidth (MkStyle style) (val) =
    mkForeign (FFun "%0.width = %1" [FPtr, FString] FUnit)
              style (show val)
width : Attr Style JsLength
width =
    let getter = getWidth
        setter = setWidth
    in MkAttr getter setter

getWordBreak : Style -> IO String
getWordBreak (MkStyle style) = 
    mkForeign (FFun "%0.wordBreak" [FPtr] FString) style
setWordBreak : Style -> String -> IO ()
setWordBreak (MkStyle style) (val) =
    mkForeign (FFun "%0.wordBreak = %1" [FPtr, FString] FUnit) style val
wordBreak : Attr Style String
wordBreak =
    let getter = getWordBreak
        setter = setWordBreak
    in MkAttr getter setter

getWordSpacing : Style -> IO String
getWordSpacing (MkStyle style) = 
    mkForeign (FFun "%0.wordSpacing" [FPtr] FString) style
setWordSpacing : Style -> String -> IO ()
setWordSpacing (MkStyle style) (val) =
    mkForeign (FFun "%0.wordSpacing = %1" [FPtr, FString] FUnit) style val
wordSpacing : Attr Style String
wordSpacing =
    let getter = getWordSpacing
        setter = setWordSpacing
    in MkAttr getter setter

getWordWrap : Style -> IO String
getWordWrap (MkStyle style) = 
    mkForeign (FFun "%0.wordWrap" [FPtr] FString) style
setWordWrap : Style -> String -> IO ()
setWordWrap (MkStyle style) (val) =
    mkForeign (FFun "%0.wordWrap = %1" [FPtr, FString] FUnit) style val
wordWrap : Attr Style String
wordWrap =
    let getter = getWordWrap
        setter = setWordWrap
    in MkAttr getter setter

getWindows : Style -> IO String
getWindows (MkStyle style) = 
    mkForeign (FFun "%0.windows" [FPtr] FString) style
setWindows : Style -> String -> IO ()
setWindows (MkStyle style) (val) =
    mkForeign (FFun "%0.windows = %1" [FPtr, FString] FUnit) style val
windows : Attr Style String
windows =
    let getter = getWindows
        setter = setWindows
    in MkAttr getter setter

getZIndex : Style -> IO String
getZIndex (MkStyle style) = 
    mkForeign (FFun "%0.zIndex" [FPtr] FString) style
setZIndex : Style -> String -> IO ()
setZIndex (MkStyle style) (val) =
    mkForeign (FFun "%0.zIndex = %1" [FPtr, FString] FUnit) style val
zIndex : Attr Style String
zIndex =
    let getter = getZIndex
        setter = setZIndex
    in MkAttr getter setter
