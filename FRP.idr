module FRP

import Debug.Trace

import Document
import Animation
import Element
import Event

%default total

%include JavaScript "ioref.js"

-----------
-- IORef --
-----------

data IORef : Type -> Type where
    MkIORef : Ptr -> IORef a

newIORef : a -> IO (IORef a)
newIORef x = MkIORef `map`
    mkForeign (FFun "newIORefRaw(%0)" [FAny a] FPtr) x

newIORef' : a -> IORef a
newIORef' x = unsafePerformIO $ newIORef x

readIORef : IORef a -> IO a
readIORef {a} (MkIORef ref) =
    mkForeign (FFun "readIORefRaw(%0)" [FPtr] (FAny a)) ref

writeIORef : IORef a -> a -> IO ()
writeIORef {a} (MkIORef ref) val =
    mkForeign (FFun "writeIORefRaw(%0, %1)" [FPtr, FAny a] FUnit) ref val

modifyIORef : IORef a -> (a -> a) -> IO ()
modifyIORef ref f = readIORef ref >>= writeIORef ref . f

nullJS : a
nullJS {a} = unsafePerformIO $
    mkForeign (FFun "null" [] (FAny a))

---------------
-- Idris API --
---------------

data Signal : Type -> Type where
    MkSig : Bool ->           -- | Connected to world.
                              --   (addEventListener or
                              --    setInterval has been called on
                              --    this signal's recv, or one of its
                              --    ancestor's recv.)
            IORef a ->        -- | Time-varying value.
            IO () ->          -- | Initialize value.
            IORef (IO ()) ->  -- | Recieve Event.
            Signal a

getSigVal : Signal a -> IO a
getSigVal (MkSig _ ref _ _) = readIORef ref

getSigRef : Signal a -> IORef a
getSigRef (MkSig _ ref _ _) = ref

getSigRecv : Signal a -> IORef (IO ())
getSigRecv (MkSig _ _ _ recv) = recv

getSigInit : Signal a -> IO ()
getSigInit (MkSig _ _ initS _) = initS

sigConnected : Signal a -> Bool
sigConnected (MkSig con _ _ _) = con

sigInit : Signal a -> IO ()
sigInit (MkSig _ _ sinit _) = sinit

addChildRecv : Signal a -> IORef (IO ()) -> IO ()
addChildRecv (MkSig _ _ _ parentRecv) childRecv =
    modifyIORef parentRecv ($> flatten (readIORef childRecv))

mkSig' : Foldable t => t (Signal b) -> (IORef a -> IO ()) -> Signal a
mkSig' xs action =
    let this = newIORef' nullJS
        thisRecv = newIORef' $ action this
        thisInit = do
            traverse_ sigInit xs
            traverse_ (flip addChildRecv thisRecv) xs
    in MkSig (any sigConnected xs) this thisInit thisRecv

mkSig : IO a -> Signal b -> (IORef a -> IO ()) -> Signal a
mkSig initS x action =
    let this = newIORef' nullJS
        thisRecv = newIORef' $ action this
        thisInit = do
            sigInit x
            initS >>= writeIORef this
            addChildRecv x thisRecv
    in MkSig (sigConnected x) this thisInit thisRecv

mkSig2 : IO a -> Signal b -> Signal c -> (IORef a -> IO ()) -> Signal a
mkSig2 initS x y action =
    let this = newIORef' nullJS
        thisRecv = newIORef' $ action this
        thisInit = do
            sigInit x $> sigInit y
            initS >>= writeIORef this
            addChildRecv x thisRecv
            addChildRecv y thisRecv
        connected = sigConnected x || sigConnected y
    in MkSig connected this thisInit thisRecv

mkSig3 : IO a -> Signal b -> Signal c -> Signal d ->
         (IORef a -> IO ()) -> Signal a
mkSig3 initS x y z action =
    let this = newIORef' nullJS
        thisRecv = newIORef' $ action this
        thisInit = do
            sigInit x $> sigInit y $> sigInit z
            initS >>= writeIORef this
            addChildRecv x thisRecv
            addChildRecv y thisRecv
            addChildRecv z thisRecv
        connected = sigConnected x || sigConnected y || sigConnected z
    in MkSig connected this thisInit thisRecv

mkSig4 : IO a -> Signal b -> Signal c -> Signal d -> Signal e ->
         (IORef a -> IO ()) -> Signal a
mkSig4 initS x y z w action =
    let this = newIORef' nullJS
        thisRecv = newIORef' $ action this
        thisInit = do
            sigInit x $> sigInit y $> sigInit z $> sigInit w
            initS >>= writeIORef this
            addChildRecv x thisRecv
            addChildRecv y thisRecv
            addChildRecv z thisRecv
            addChildRecv w thisRecv
        connected = sigConnected x || sigConnected y ||
                    sigConnected z || sigConnected w
    in MkSig connected this thisInit thisRecv

-- Technically not Semigroup, l <+> r /= r <+> l
instance Semigroup (Signal a) where
    (<+>) sig1 sig2 =
        mkSig2 (return nullJS) sig1 sig2 $ \this =>
            getSigVal sig1 >>= writeIORef this

instance Monoid a => Monoid (Signal a) where
    neutral =
        MkSig False (newIORef' neutral) (return ())
                    (newIORef' $ return ())

instance Functor Signal where
    map f sig =
        let initVal = f `map` getSigVal sig
        in mkSig initVal sig $ \this => do
            val <- getSigVal sig
            writeIORef this (f val)

instance Applicative Signal where
    pure x =
        let recv = newIORef' $ return ()
        in MkSig False (newIORef' x)
                 (return ())
                 recv
    (<$>) funcSig valSig =
        let initS = do
                func <- getSigVal funcSig
                val <- getSigVal valSig
                return $ func val
        in mkSig2 initS funcSig valSig $ \this => do
            func <- getSigVal funcSig
            val <- getSigVal valSig
            writeIORef this (func val)

instance Monad Signal where
    (>>=) (MkSig connected valRef valInit valRecv) func =
        mkSig nullJS (MkSig connected valRef valInit valRecv)
            $ \this => do
                val <- readIORef valRef
                let MkSig newCon newRef newInit newRecv = func val
                modifyIORef valRecv ($> flatten (readIORef newRecv))
                newVal <- readIORef newRef
                writeIORef this newVal

fps : Int -> Signal Float
fps desiredFPS =
    let lastFrame = newIORef' 0.0
        msPerFrame = 1000.0 / cast desiredFPS
        delta = newIORef' 0.0
        thisRecv = newIORef' $ do
            lastTime <- readIORef lastFrame
            curTime <- getTime
            writeIORef delta (curTime - lastTime)
            writeIORef lastFrame curTime
        thisInit = do
            getTime >>= writeIORef lastFrame
            setInterval (flatten $ readIORef thisRecv) msPerFrame
            return ()
    in MkSig True delta thisInit thisRecv
  where
    getTime : IO Float
    getTime = mkForeign (FFun "(new Date()).getTime()" [] FFloat)

infixl 4 <~
(<~) : (a -> b) -> Signal a -> Signal b
(<~) = map
infixl 4 ~~
(~~) : Signal (a -> b) -> Signal a -> Signal b
(~~) = (<$>)

lift : (a -> b) -> Signal a -> Signal b
lift = map
lift2 : (a -> b -> c) -> Signal a -> Signal b -> Signal c
lift2 f sig1 sig2 =
    let initS = do
            val1 <- getSigVal sig1
            val2 <- getSigVal sig2
            return $ f val1 val2
    in mkSig2 initS sig1 sig2 $ \this => do
        val1 <- getSigVal sig1
        val2 <- getSigVal sig2
        writeIORef this (f val1 val2)
lift3 : (a -> b -> c -> d) -> Signal a -> Signal b -> Signal c -> Signal d
lift3 f sig1 sig2 sig3 =
    let initS = do
            val1 <- getSigVal sig1
            val2 <- getSigVal sig2
            val3 <- getSigVal sig3
            return $ f val1 val2 val3
    in mkSig3 initS sig1 sig2 sig3 $ \this => do
        val1 <- getSigVal sig1
        val2 <- getSigVal sig2
        val3 <- getSigVal sig3
        writeIORef this (f val1 val2 val3)
lift4 : (a -> b -> c -> d -> e) ->
        Signal a -> Signal b -> Signal c -> Signal d -> Signal e
lift4 f sig1 sig2 sig3 sig4 =
    let initS = do
            val1 <- getSigVal sig1
            val2 <- getSigVal sig2
            val3 <- getSigVal sig3
            val4 <- getSigVal sig4
            return $ f val1 val2 val3 val4
    in mkSig4 initS sig1 sig2 sig3 sig4 $ \this => do
        val1 <- getSigVal sig1
        val2 <- getSigVal sig2
        val3 <- getSigVal sig3
        val4 <- getSigVal sig4
        writeIORef this (f val1 val2 val3 val4)

instance Num a => Num (Signal a) where
    (+) = lift2 (+)
    (-) = lift2 (-)
    (*) = lift2 (*)
    abs = lift abs
    fromInteger = pure . fromInteger

instance Integral a => Integral (Signal a) where
    div = lift2 div
    mod = lift2 mod

effectful : IO a -> Signal a
effectful action =
    let this = newIORef' nullJS
        thisRecv = action >>= writeIORef this
    in MkSig False this thisRecv (newIORef' thisRecv)

merge : Signal a -> Signal a -> Signal a
merge = (<+>)
merges : Vect (S n) (Signal a) -> Signal a
merges = foldr1 merge

combine : List (Signal a) -> Signal (List a)
combine = foldr (lift2 (::)) (pure [])

count : Num b => Signal a -> Signal b
count from =
    mkSig (return 0) from $ \this => do
        curVal <- readIORef this
        writeIORef this (curVal+1)

liftM : (a -> IO b) -> Signal a -> Signal b
liftM f from =
    let initS = getSigVal from >>= f
    in mkSig initS from $ \this => do
        val <- getSigVal from
        f val >>= writeIORef this

foldp : (a -> b -> b) -> b -> Signal a -> Signal b
foldp func ival from =
    mkSig (return ival) from $ \this => do
        aVal <- getSigVal from
        currentVal <- readIORef this
        writeIORef this (func aVal currentVal)

foldpM : (a -> b -> IO b) -> b -> Signal a -> Signal b
foldpM func ival from =
    mkSig (return ival) from $ \this => do
        aVal <- getSigVal from
        currentVal <- readIORef this
        func aVal currentVal >>= writeIORef this

mousePosition : Signal (Int, Int)
mousePosition =
    let this = newIORef' (0,0)
        recv = newIORef' $ return ()
        sigInit = do
            doc <- documentElement
            addEventListener MouseMove doc $ \ev => do
                eventMousePos ev >>= writeIORef this
                flatten $ readIORef recv
    in MkSig True this sigInit recv

keyDown : Signal Char
keyDown =
    let this = newIORef' $ '\0'
        recv = newIORef' $ return ()
        sigInit = do
            doc <- documentElement
            addEventListener KeyDown doc $ \ev => do
                eventKeycode ev >>= writeIORef this
                flatten $ readIORef recv
    in MkSig True this sigInit recv

keyPress : Signal Char
keyPress =
    let this = newIORef' $ '\0'
        recv = newIORef' $ return ()
        sigInit = do
            doc <- documentElement
            addEventListener KeyPress doc $ \ev => do
                eventKeycode ev >>= writeIORef this
                flatten $ readIORef recv
    in MkSig True this sigInit recv

keyUp : Signal Char
keyUp =
    let this = newIORef' $ '\0'
        recv = newIORef' $ return ()
        sigInit = do
            doc <- documentElement
            addEventListener KeyUp doc $ \ev => do
                eventKeycode ev >>= writeIORef this
                flatten $ readIORef recv
    in MkSig True this sigInit recv

isDown : Char -> Signal Bool
isDown myKey =
    let this = newIORef' False
        recv = newIORef' $ return ()
        sigInit = do
            doc <- documentElement
            addEventListener KeyPress doc $ \ev => do
                key <- eventKeycode ev
                if key == myKey
                    then do
                        writeIORef this True
                        flatten $ readIORef recv
                else writeIORef this False
    in MkSig True this sigInit recv

start : Show a => Signal a -> IO ()
start (MkSig connected ref sigInit recv) = do
    sigInit
    when (not connected) $ do
        setInterval (flatten $ readIORef recv) 16
        return ()

secondSig : Signal Int
secondSig = foldpM
    (\a, b => putStrLn ("secondSig: " ++ show b) $> return (b+1))
    0 (pure 3)

mySig : Signal Int
mySig = foldpM func 0 (isDown 'a') <+>
        foldpM (\a,b=>print a$>return b) 0 (fps 1)
  where
    func : Bool -> Int -> IO Int
    func a b = do
        print "Pressed 'a'"
        return b

sigTest : IO ()
sigTest = start mySig
