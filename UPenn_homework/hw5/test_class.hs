{-
{-# LANGUAGE MultiParamTypeClasses #-}
-}


{-
class Coll s a where
    empty :: s
    insert :: s -> a -> s
-}

{-
class Coll s a | s -> a where
    empty :: s 
    insert :: s -> a -> s 
-}

{-
class Coll s a where 
    empty :: s a 
    insert :: s a -> a -> s a 
-}

