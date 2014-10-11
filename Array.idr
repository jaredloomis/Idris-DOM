module Array

data Array a = MkArray Ptr

length : Array a -> Int
length (MkArray ptr) = unsafePerformIO $
    mkForeign (FFun "%0.length" [FPtr] FInt) ptr

null : Array a -> Bool
null array = length array == 0

partial index : Int -> Array a -> a
index {a} i (MkArray ptr) = unsafePerformIO $
    mkForeign (FFun "%0[%1]" [FPtr, FInt] (FAny a)) ptr i

generate : Int -> (Int -> a) -> Array a
generate {a} len gen = unsafePerformIO $ MkArray `map`
    mkForeign (FFun
        "function(){var ret=[];for(var i=0;i < %0;++i){ret[i]=%1(i);};return ret;}()"
        [FInt, FFunction FInt (FAny a)] FPtr) len gen

instance Functor Array where
    map f arr = generate (length arr) (f . flip index arr)
