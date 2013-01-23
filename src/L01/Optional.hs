module L01.Optional where

--  class Optional<A> {
--    Optional(A a) {} // Full
--    Optional() {} // Empty
--  }
data Optional a = Full a | Empty deriving (Eq, Show)

-- :: is of type
mapOptional :: (a -> b) -> (Optional a -> Optional b)
mapOptional _ Empty    = Empty
mapOptional f (Full x) = Full (f x)

bindOptional :: Optional a -> (a -> Optional b) -> Optional b
bindOptional Empty _    = Empty
bindOptional (Full a) f = f a

(??) :: Optional a -> a -> a
Empty ?? d  = d
Full a ?? _ = a

(<+>) :: Optional a -> Optional a -> Optional a
Empty <+> o = o
k <+> _     = k

data Swizzle a b = Swiz a | Swoz b | Swaz

swink :: Optional String -> Optional String
swink Empty = Full "hi"
swink (Full "xyz") = Empty
swink (Full anythingElse) = Full (reverse anythingElse)
