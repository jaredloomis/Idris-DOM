module Attr

import Control.Category

%default total
%access public

data Attr : Type -> Type -> Type where
    MkAttr : (o -> IO a) -> (o -> a -> IO ()) -> Attr o a

data AttrOp : Type -> Type where
    (:=)  : Attr o a -> a           -> AttrOp o
    (:~)  : Attr o a -> (a -> a)    -> AttrOp o
    (:!=) : Attr o a -> IO a        -> AttrOp o
    (:!~) : Attr o a -> (a -> IO a) -> AttrOp o
infixr 1 :=, :~, :!=, :!~

set : o -> List (AttrOp o) -> IO ()
set obj attrs = forM_ attrs $ \x => case x of
    MkAttr _      setter :=  x => setter obj x
    MkAttr getter setter :~  f => getter obj >>= setter obj . f
    MkAttr _      setter :!= x => x >>= setter obj
    MkAttr getter setter :!~ f => getter obj >>= f >>= setter obj
  where
    (>>) : IO a -> IO b -> IO b
    (>>) a b = a >>= \_ => b

    forM_ : List a -> (a -> IO b) -> IO ()
    forM_ xs f = foldr (>>) (return ()) (map f xs)

get : o -> Attr o a -> IO a
get obj (MkAttr getter _) = getter obj

-- Attr b c . Attr a b = Attr a c

printAny : a -> IO ()
printAny {a} x =
    mkForeign (FFun "console.log(%0)" [FAny a] FUnit) x

compose : Attr b c -> Attr a b -> Attr a c
compose (MkAttr g1 s1) (MkAttr g2 s2) =
    let getter = \a => g2 a >>= g1
        setter = \a, c => do
            val <- get a (MkAttr g2 s2)
            set val [(MkAttr g1 s1) := c]
            {-
            b <- g2 a

            printAny b
            s1 b c

            c <- g1 b
            s2 a b
            -}

    in MkAttr getter setter

instance Category Attr where
    (.) = compose
    id = MkAttr return (const $ const $ return ())


