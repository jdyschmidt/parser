module UUID (
    UUID,
    IDGen,
    nextID,
    discardUniqueness,
    (<=<)
) where


----------------

data UUID = UUID Int deriving Eq
instance Show UUID where
    show (UUID i) = 'u' : (show i)
instance Ord UUID where
    UUID lhs <= UUID rhs = lhs <= rhs

data IDGen a = IDGen (UUID -> (a, UUID))
instance Monad IDGen where
    IDGen c1 >>= fc2 = IDGen (\u0 -> let (r, u1) = c1 u0
                                         IDGen c2 = fc2 r in
                                        c2 u1)
    return k         = IDGen (\u -> (k, u))
instance Applicative IDGen where
    pure k = IDGen (\u -> (k, u))
    m1 <*> m2 = m1 >>= (\x -> m2 >>= (pure . x))
instance Functor IDGen where
    fmap f m = pure f <*> m

nextID :: IDGen UUID
nextID = IDGen (\u -> let UUID i = u in (u, UUID (i+1)))

discardUniqueness :: IDGen a -> a
discardUniqueness (IDGen c) = fst $ c $ UUID 0


(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)  
f <=< g = (\x -> g x >>= f)  