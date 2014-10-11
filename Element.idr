module Element

import Event
import Attr

%default total
%access public

%include JavaScript "jsLib.js"

data Element = MkElem Ptr
data NodeList = MkNodeList Ptr

data Rectangle = MkRectangle Ptr

-----------------------------------------------
-- Getting fold-like event listeners to work --
-----------------------------------------------

data JsPair : Type -> Type -> Type where
    MkJsPair : Ptr -> JsPair a b

mkJsPair : a -> b -> IO (JsPair a b)
mkJsPair {a} {b} first second = MkJsPair `map`
    mkForeign (FFun "new JsPair(%0, %1)"
              [FAny a, FAny b]
              FPtr)
              first second

getFirst : JsPair a b -> IO a
getFirst {a} {b} pair =
    mkForeign (FFun "%0.getFirst()"
              [FAny (JsPair a b)]
              (FAny a))
              pair

getSecond : JsPair a b -> IO b
getSecond {a} {b} pair =
    mkForeign (FFun "%0.getSecond()"
              [FAny (JsPair a b)]
              (FAny b))
              pair

addEventListenerAccumPrim :
    EventType -> (JsPair Ptr a -> IO a) -> a -> Element -> IO ()
addEventListenerAccumPrim {a} eventType f val (MkElem element) =
    mkForeign (FFun "addEventListenerAccumPair(%0, %1, %2, %3)"
                 [FPtr, FString,
                  FFunction (FAny (JsPair Ptr a)) (FAny (IO a)),
                  FAny a]
                 FUnit)
                 element (show eventType) f val

addEventListenerAccum :
    EventType -> (Event -> a -> IO a) -> a -> Element -> IO ()
addEventListenerAccum eventType f val elem =
    let newFunc = \pair => do
            e <- getFirst pair
            a <- getSecond pair
            f (MkEvent e) a
    in addEventListenerAccumPrim eventType newFunc val elem

foldOn : EventType -> a -> Element -> (Event -> a -> IO a) -> IO ()
foldOn event a element f = addEventListenerAccum event f a element

foldEvent : EventType -> (Event -> a -> IO a) -> a -> Element -> IO ()
foldEvent = addEventListenerAccum

-- Other stuff

length : NodeList -> IO Int
length (MkNodeList nl) =
    mkForeign (FFun "%0.length" [FPtr] FInt) nl

infixr 3 !?
(!?) : NodeList -> Int -> IO (Maybe Element)
(!?) (MkNodeList nl) i = do
    len <- length $ MkNodeList nl
    if i < len
        then Just . MkElem `map`
            mkForeign (FFun "%0.item(%1)" [FPtr, FInt] FPtr) nl i
    else return Nothing

addEventListener : EventType -> Element -> (Event -> IO ()) -> IO ()
addEventListener eventType (MkElem element) f =
    let eventStr = show eventType
    in mkForeign (FFun "%0.addEventListener(%1, %2)"
                 [FPtr, FString, FFunction FPtr (FAny (IO ()))]
                 FUnit)
                 element eventStr (f . MkEvent)

onEvent : EventType -> Element -> (Event -> IO ()) -> IO ()
onEvent = addEventListener

removeEventListener : Element -> EventType -> IO ()
removeEventListener (MkElem elem) eventType =
    let eventStr = show eventType
    in mkForeign (FFun "%0.removeEventListener(%1)" [FPtr, FString] FUnit)
                 elem eventStr

appendChild : Element -> Element -> IO ()
appendChild (MkElem parent) (MkElem child) =
    mkForeign (FFun "%0.appendChild(%1)" [FPtr, FPtr] FUnit)
              parent child

childNodes : Element -> IO NodeList
childNodes (MkElem parent) = MkNodeList `map`
    mkForeign (FFun "%0.childNodes" [FPtr] FPtr) parent

getClassName : Element -> IO String
getClassName (MkElem elem) =
    mkForeign (FFun "%0.className" [FPtr] FString) elem

setClassName : Element -> String -> IO ()
setClassName (MkElem elem) name =
    mkForeign (FFun "%0.className = %1" [FPtr, FString] FUnit)
              elem name

cloneNode : Element -> Bool -> IO Element
cloneNode (MkElem elem) deep = MkElem `map`
    mkForeign (FFun "%0.cloneNode(%1)" [FPtr, FInt] FPtr) elem (toInt deep)
  where
    toInt : Bool -> Int
    toInt True = 1
    toInt False = 0

compareDocumentPosition : Element -> Element -> IO Int
compareDocumentPosition (MkElem elem1) (MkElem elem2) =
    mkForeign (FFun "%0.compareDocumentPosition(%1)" [FPtr, FPtr] FInt)
              elem1 elem2

getContentEditable : Element -> IO Bool
getContentEditable (MkElem elem) = toBool `map`
    mkForeign (FFun "%0.contentEditable" [FPtr] FInt) elem
  where
    toBool : Int -> Bool
    toBool 0 = False
    toBool _ = True

setContentEditable : Element -> Bool -> IO ()
setContentEditable (MkElem elem) editable =
    mkForeign (FFun "%0.contentEditable = %1" [FPtr, FInt] FUnit)
              elem (toInt editable)
  where
    toInt : Bool -> Int
    toInt False = 0
    toInt True  = 1

getDir : Element -> IO String
getDir (MkElem elem) =
    mkForeign (FFun "%0.dir" [FPtr] FString) elem

setDir : Element -> String -> IO ()
setDir (MkElem elem) d =
    mkForeign (FFun "%0.dir = %1" [FPtr, FString] FUnit) elem d

firstChild : Element -> IO Element
firstChild (MkElem elem) = MkElem `map`
    mkForeign (FFun "%0.firstChild" [FPtr] FPtr) elem

getAttribute : Element -> String -> IO String
getAttribute (MkElem elem) attr =
    mkForeign (FFun "%0.getAttribute(%1)" [FPtr, FString] FString)
              elem attr

hasAttribute : Element -> String -> IO Bool
hasAttribute (MkElem elem) attr = toBool `map`
    mkForeign (FFun "%0.hasAttribute(%1)" [FPtr, FString] FInt) elem attr
  where
    toBool : Int -> Bool
    toBool 0 = False
    toBool _ = True

hasAttributes : Element -> IO Bool
hasAttributes (MkElem elem) = toBool `map`
    mkForeign (FFun "%0.hasAttributes()" [FPtr] FInt) elem
  where
    toBool : Int -> Bool
    toBool 0 = False
    toBool _ = True

setAttribute : Element -> String -> String -> IO ()
setAttribute (MkElem elem) attr val =
    mkForeign (FFun "%0.setAttribute(%1, %2)" [FPtr, FString, FString] FUnit)
              elem attr val

-- TODO: accessKey,attributes,getAttributeNode,getFeature,
-- getUserData

getBoundingClientRec : Element -> IO Rectangle
getBoundingClientRec (MkElem element) = MkRectangle `map`
    mkForeign (FFun "%0.getBoundingClientRect()" [FPtr] FPtr) element

recWidth : Rectangle -> IO Float
recWidth (MkRectangle rec) =
    mkForeign (FFun "%0.width" [FPtr] FFloat) rec

recHeight : Rectangle -> IO Float
recHeight (MkRectangle rec) =
    mkForeign (FFun "%0.height" [FPtr] FFloat) rec


getWidth : Element -> IO Float
getWidth element = getBoundingClientRec element >>= recWidth
getHeight : Element -> IO Float
getHeight element = getBoundingClientRec element >>= recHeight

setWidth : Element -> Float -> IO ()
setWidth (MkElem element) width =
    mkForeign (FFun "%0.style.width = %1" [FPtr, FFloat] FUnit)
              element width
setHeight : Element -> Float -> IO ()
setHeight (MkElem element) width =
    mkForeign (FFun "%0.style.width = %1" [FPtr, FFloat] FUnit)
              element width

width : Attr Element Float
width =
    MkAttr getWidth setWidth

height : Attr Element Float
height =
    MkAttr getHeight setHeight

getClientWidth : Element -> IO Float
getClientWidth (MkElem element) =
    mkForeign (FFun "%0.clientWidth" [FPtr] FFloat) element

getClientHeight : Element -> IO Float
getClientHeight (MkElem element) =
    mkForeign (FFun "%0.clientHeight" [FPtr] FFloat) element


-- Only use with images.
naturalWidth : Element -> IO Float
naturalWidth (MkElem elem) =
    mkForeign (FFun "%0.naturalWidth" [FPtr] FFloat) elem

-- Only use with images.
naturalHeight : Element -> IO Float
naturalHeight (MkElem elem) =
    mkForeign (FFun "%0.naturalHeight" [FPtr] FFloat) elem
