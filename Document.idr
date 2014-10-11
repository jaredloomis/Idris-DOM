module Document

import Element
import Event
import Attr
import Array

%default total
%access public

---------------------------------
-- Direct conversion of JS DOM --
---------------------------------

baseURI : IO String
baseURI = mkForeign (FFun "document.baseURI" [] FString)

body : IO Element
body = MkElem `map`
    mkForeign (FFun "document.body" [] FPtr)

createElement : String -> IO Element
createElement elem = MkElem `map`
    mkForeign (FFun "document.createElement(%0)" [FString] FPtr) elem

documentElement : IO Element
documentElement = MkElem `map`
    mkForeign (FFun "document.documentElement" [] FPtr)

-- XXX: The actual return type is "Text Node object".
--      Can "Element" be used in place of "Text Node object"
createTextNode : String -> IO Element
createTextNode text = MkElem `map`
    mkForeign (FFun "%0.createTextNode(%1)" [FString] FPtr) text

documentURI : IO String
documentURI = mkForeign (FFun "document.documentURI" [] FString)

domain : IO String
domain = mkForeign (FFun "document.domain" [] FString)

domConfig : IO String
domConfig = mkForeign (FFun "document.domConfig" [] FString)

forms : IO (Array Element)
forms = MkArray `map`
    mkForeign (FFun "document.forms" [] FPtr)

getElementById : String -> IO Element
getElementById i = MkElem `map`
    mkForeign (FFun "document.getElementById(%0)" [FString] FPtr) i

getElementByName : String -> IO NodeList
getElementByName n = MkNodeList `map`
    mkForeign (FFun "document.getElementByName(%0)" [FString] FPtr) n

head : IO Element
head = MkElem `map` mkForeign (FFun "document.head" [] FPtr)

-- w3schools claims the return type is "collection",
-- idk what they are talking about. NodeList may work.
images : IO NodeList
images = MkNodeList `map` mkForeign (FFun "document.images" [] FPtr)

importNode : Element -> Bool -> IO Element
importNode (MkElem elem) deep = MkElem `map`
    mkForeign (FFun "document.importNode(%0, %1)" [FInt, FPtr] FPtr)
              (toInt deep) elem
  where
    toInt : Bool -> Int
    toInt True = 1
    toInt False = 0

inputEncoding : IO String
inputEncoding = mkForeign (FFun "document.inputEncoding" [] FString)

lastModified : IO String
lastModified = mkForeign (FFun "document.lastModified" [] FString)

links : IO (Array Element)
links = MkArray `map` mkForeign (FFun "document.links" [] FPtr)

normalize : IO ()
normalize = mkForeign (FFun "document.normalize()" [] FUnit)

querySelectorAll : String -> IO NodeList
querySelectorAll q = MkNodeList `map`
    mkForeign (FFun "document.querySelectorAll(%0)" [FString] FPtr) q

querySelector : String -> IO Element
querySelector q = MkElem `map`
    mkForeign (FFun "document.querySelector(%0)" [FString] FPtr) q

readyState : IO String
readyState = mkForeign (FFun "document.readyState" [] FString)

referrer : IO String
referrer = mkForeign (FFun "document.referrer" [] FString)

title : IO String
title = mkForeign (FFun "document.title" [] FString)

url : IO String
url = mkForeign (FFun "document.url" [] FString)

write : String -> IO ()
write str =
    mkForeign (FFun "document.write(%0)" [FString] FUnit) str

writeln : String -> IO ()
writeln str =
    mkForeign (FFun "document.writeln(%0)" [FString] FUnit) str

-- TODO: adoptNode,anchors,applets,cookie,close,createComment
-- createDocumentFragment,createAttribute,doctype,embeds,
-- implementation,open,scripts

getElementByClassName : String -> IO NodeList
getElementByClassName c = MkNodeList `map`
    mkForeign (FFun "document.getElementByClassName(%0)" [FString] FPtr) c

getElementByTagName : String -> IO NodeList
getElementByTagName tn = MkNodeList `map`
    mkForeign (FFun "document.getElementByTagName(%0)" [FString] FPtr) tn

--------------
-- My Stuff --
--------------

-- Allow JQuery syntax.
syntax "$(" [q] ")" = querySelectorAll q
syntax "@(" [q] ")" = querySelector q
