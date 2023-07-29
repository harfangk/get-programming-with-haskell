f :: (Num a) => a -> a
f = (+ 2)

f2 :: (Num a) => a -> IO a
f2 x = return (x + 2)

allFmap :: (Monad m) => (a -> b) -> m a -> m b
allFmap func ma = ma >>= (\x -> return (func x))

-- >>= :: m a -> (a -> m b) -> m b

allApp :: (Monad m) => m (a -> b) -> m a -> m b
allApp func ma = func >>= (\xf -> ma >>= (\xa -> return (xf xa)))

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing func = Nothing
bind (Just x) func = func x
