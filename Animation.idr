module Animation

import Attr
import Style
import Element

%default total
%access public
%include JavaScript "jsLib.js"

{-
class Animate a where
    step : ElementProp a -> Element -> Int -> Easing -> IO ()

data AnimateProp : Type where
    MkAnimProp : Animate a => ElementProp a -> AnimateProp

-- "instance Cast a a" isn't defined in Prelude, so I have to make this
-- instance so that the "(Num a, Cast a Float, Cast Float a) => Animate a"
-- below works with Floats.
instance Cast Float Float where
    cast = id

instance (Num a, Cast a Float, Cast Float a) => Animate a where
    step (attr := finalVal) element duration easing = do
        curVal <- get element attr
        let stepVal = cast $ cast (finalVal - curVal) / cast duration
        mystyle <- get element style
        set element [attr :~ (+stepVal)]

stepAll : List AnimateProp -> Element -> Int -> Easing -> IO ()
stepAll ops element duration easing =
    stepAll' ops
  where
    stepFunc : Animate a => ElementProp a -> IO ()
    stepFunc prop = step prop element duration easing

    stepAll' : List AnimateProp -> IO ()
    stepAll' ((attr := val) :: ops) = do
        stepFunc (AssignProp attr val)
        stepAll' ops
    stepAll' ((attr :~ func) :: ops) = do
        attrVal <- get element attr
        let newVal = func attrVal
        stepFunc (AssignProp attr newVal)
        stepAll' ops
    stepAll' ((attr :!= val) :: ops) = do
        attrVal <- get element attr
        newVal <- val
        stepFunc (AssignProp attr newVal)
        stepAll' ops
    stepAll' ((attr :!~ func) :: ops) = do
        attrVal <- get element attr
        newVal <- func attrVal
        stepFunc (AssignProp attr newVal)
        stepAll' ops
    stepAll' [] = return ()

||| Animate using requestAnimationFrame internally.
animate'' : Element -> List AnimateProp -> Int -> Easing -> IO () -> IO ()
animate'' element ops duration easing complete = do
    let stepFunc = stepAll ops element duration easing
    numLoop duration stepFunc
  where
    numLoop : Int -> IO () -> IO ()
    numLoop i func =
        if i <= 0
            then complete
        else do
            print i
            func
            requestAnimationFrame (numLoop (assert_smaller i (i-1)) func)
                                  element

||| Animate using setInterval internally.
animate' : Element -> List AnimateProp -> Int -> Easing -> IO () -> IO ()
animate' element ops duration easing complete = do
    let stepFunc = stepAll ops element duration easing
    interval <- setInterval' duration (createStepFunc stepFunc) 16
    return ()
  where
    createStepFunc : IO () -> (Interval -> Int -> IO Int)
    createStepFunc action = \interval, count =>
        if count <= 1
            then do
                clearInterval interval
                complete
                return 0
        else do
            action
            return (count - 1)
-}

data Timeout = MkTimeout Ptr
data Interval = MkInterval Ptr

data Easing = Linear | Swing

instance Eq Easing where
    Swing == Swing = True
    Linear == Linear = True
    _ == _ = False

data ElementProp : Type -> Type where
    (:=)  : Attr Element a -> a           -> ElementProp a
    (:~)  : Attr Element a -> (a -> a)    -> ElementProp a
    (:!=) : Attr Element a -> IO a        -> ElementProp a
    (:!~) : Attr Element a -> (a -> IO a) -> ElementProp a

------------
-- Raw JS --
------------

setTimeout : IO () -> Float -> IO Timeout
setTimeout f ms = MkTimeout `map`
    mkForeign (FFun "setTimeoutIdris(%0, %1)"
              [FFunction FString (FAny (IO ())), FFloat] FPtr)
              (const f) ms

clearTimeout : Timeout -> IO ()
clearTimeout (MkTimeout ptr) =
    mkForeign (FFun "clearTimeout(%0)"
              [FPtr] FUnit) ptr

requestAnimationFrame : IO () -> Element -> IO ()
requestAnimationFrame func (MkElem element) =
    mkForeign (FFun "requestAnimationFrameIdris(%0, %1)"
              [FFunction FString (FAny (IO ())), FPtr] FUnit)
              (const func) element

setInterval : IO () -> Float -> IO Interval
setInterval f ms = MkInterval `map`
    mkForeign (FFun "setIntervalIdris(%0, %1)"
              [FFunction FString (FAny (IO ())), FFloat] FPtr)
              (const f) ms

setIntervalPrim' : a -> (JsPair Ptr a -> IO a) -> Float -> IO Interval
setIntervalPrim' {a} initial f ms = MkInterval `map`
    mkForeign (FFun "setIntervalIdris2(%0, %1, %2)"
              [FAny a,
              FFunction (FAny (JsPair Ptr a)) (FAny (IO a)), FFloat] FPtr)
              initial f ms

setInterval' : a -> (Interval -> a -> IO a) -> Float -> IO Interval
setInterval' initial f ms =
    let newFunc = \pair => do
            first <- getFirst pair
            second <- getSecond pair
            f (MkInterval first) second
    in setIntervalPrim' initial newFunc ms

clearInterval : Interval -> IO ()
clearInterval (MkInterval ptr) =
    mkForeign (FFun "clearInterval(%0)"
              [FPtr] FUnit) ptr

-----------
-- Tween --
-----------

class Tweenable a where
    genTween : ElementProp a -> Element -> Int -> Easing -> IO (List a)

instance Tweenable Float where
    genTween (attr := finalVal) element duration easing = do
        when (easing /= Linear) $
            putStrLn "genTween: Given easing other than Linear. Using Linear."
        curVal <- get element attr
        let stepVal = (finalVal - curVal) / cast duration
        return $ map (\x => curVal + (stepVal * cast x)) [0..duration]
    genTween (attr :~ func) element duration easing = do
        finalVal <- func `map` get element attr
        assert_total $ genTween (attr := finalVal) element duration easing
    genTween (attr :!= finalIO) element duration easing = do
        finalVal <- finalIO
        assert_total $ genTween (attr := finalVal) element duration easing
    genTween (attr :!~ funcIO) element duration easing = do
        finalVal <- get element attr >>= funcIO
        assert_total $ genTween (attr := finalVal) element duration easing

||| Animate using Tweens. This is usually the most
||| efficient way to animate.
animTween : Tweenable a => ElementProp a -> Element ->
            Int -> Easing -> IO () -> IO ()
animTween prop element duration easing onComplete = do
    frames <- genTween prop element duration easing
    loop frames (elemPropAttr prop)
  where
    elemPropAttr : ElementProp a -> Attr Element a
    elemPropAttr (attr := _) = attr
    elemPropAttr (attr :~ _) = attr
    elemPropAttr (attr :!= _) = attr
    elemPropAttr (attr :!~ _) = attr

    loop : List a -> Attr Element a -> IO ()
    loop [] _ = onComplete
    loop (x :: xs) attr = do
        set element [attr := x]
        requestAnimationFrame (loop xs attr) element

instance [ShowGlue] (a -> String) => Show a where
  show = %instance

mkShow : {a : Type} -> (a -> String) -> Show a
mkShow s = ShowGlue @{s}
