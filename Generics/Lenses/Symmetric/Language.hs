{-# LANGUAGE TupleSections, FunctionalDependencies, RankNTypes, TypeFamilies, ExistentialQuantification, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

module Generics.Lenses.Symmetric.Language where

import Control.Monad.State (State(..),StateT(..),MonadState)
import qualified Control.Monad.State as State
import Control.Monad.Trans.Class
import Control.Monad
import GHC.InOut
import Control.Monad.Box


-- * Asymmetric Lenses

data Lens m s v = Lens { get :: StateT s m v, put :: v -> StateT s m (), create :: v -> m s }

-- | This is not strictly a lens
boxLns :: Box k m => Lens m s (k s)
boxLns = Lens get' put' create' where
	get' = State.get >>= lift . box
	put' s = lift (unbox s) >>= State.put
	create' = unbox

unboxLns :: Box k m => Lens m (k s) s
unboxLns = Lens get' put' create' where
	get' = State.get >>= lift . unbox
	put' s = do
		box <- State.get
		lift $ inbox box (const $ return s)
		return ()
	create' = box

idLns :: Monad m => Lens m a a
idLns = Lens State.get State.put return

infixr 9 >.>
(>.>) :: Monad m => Lens m a b -> Lens m b c -> Lens m a c
l1 >.> l2 = Lens get' put' create' where
	get' = do
		x <- State.get
		y <- lift $ State.evalStateT (get l1) x
		z <- lift $ State.evalStateT (get l2) y
		return z
	put' z' = do
		x <- State.get
		y <- lift $ State.evalStateT (get l1) x
		y' <- lift $ State.execStateT (put l2 z') y
		x' <- lift $ State.execStateT (put l1 y') x
		State.put x'
	create' z' = create l2 z' >>= create l1

fstLns :: Monad m => Lens m (a,b) a
fstLns = fstLns' (const $ fail "fstLns")

fstLns' :: Monad m => (a -> m b) -> Lens m (a,b) a
fstLns' f = Lens get' put' create' where
	get' = State.get >>= return . fst
	put' x = State.modify (\(_,y) -> (x,y))
	create' x = f x >>= \y -> return (x,y)

sndLns :: Monad m => Lens m (a,b) b
sndLns = sndLns' (const $ fail "sndLns")

sndLns' :: Monad m => (b -> m a) -> Lens m (a,b) b
sndLns' f = Lens get' put' create' where
	get' = State.get >>= return . snd
	put' y = State.modify (\(x,_) -> (x,y))
	create' y = f y >>= \x -> return (x,y)

addfstOneLns :: Monad m => Lens m a ((),a)
addfstOneLns = Lens get' put' create' where
	get' = State.get >>= \x -> return ((),x)
	put' ((),x) = State.put x
	create' ((),x) = return x

addsndOneLns :: Monad m => Lens m a (a,())
addsndOneLns = Lens get' put' create' where
	get' = State.get >>= \x -> return (x,())
	put' (x,()) = State.put x
	create' (x,()) = return x

infix 7 >*>
(>*>) :: Monad m => Lens m x z -> Lens m y w -> Lens m (x,y) (z,w)
l1 >*> l2 = Lens get' put' create' where
	get' = do
		(x,y) <- State.get
		z <- lift $ State.evalStateT (get l1) x
		w <- lift $ State.evalStateT (get l2) y
		return (z,w)
	put' (z',w') = do
		(x,y) <- State.get
		x' <- lift $ State.execStateT (put l1 z') x
		y' <- lift $ State.execStateT (put l2 w') y
		State.put (x',y')
	create' (z',w') = do
		x' <- create l1 z'
		y' <- create l2 w'
		return (x',y')

uninlLns :: Monad m => Lens m (Either a b) a
uninlLns = uninlNonLns (fail "uninlLns")

-- | This more general combinator is not a lens, but is still useful internally
uninlNonLns :: Monad m => m a -> Lens m (Either a b) a
uninlNonLns def = Lens get' put' create' where
	get' = State.get >>= \e -> case e of
		Left x -> return x
		Right y -> lift def
	put' x = State.put (Left x)
	create' x = return (Left x)

uninrLns :: Monad m => Lens m (Either a b) b
uninrLns = uninrNonLns (fail "uninrLns")

-- | This more general combinator is not a lens, but is still useful internally
uninrNonLns :: Monad m => m b -> Lens m (Either a b) b
uninrNonLns def = Lens get' put' create' where
	get' = State.get >>= \e -> case e of
		Left x -> lift def
		Right y -> return y
	put' y = State.put (Right y)
	create' y = return (Right y)

liftStateTLens :: Monad m => Lens m s v -> StateT v m r -> StateT s m r
liftStateTLens l m = do
	s <- State.get
	(r,v') <- get l >>= lift . State.runStateT m
	put l v'
	return r

innLns :: (InOut a,Monad m) => Lens m (F a) a
innLns = Lens get' put' create'
	where get' = liftM inn State.get
	      put' v = State.put (out v)
	      create' v = return (out v)

outLns :: (InOut a,Monad m) => Lens m a (F a)
outLns = Lens get' put' create' where
	get' = liftM out State.get
	put' v = State.put (inn v)
	create' v = return (inn v)

-- * Symmetric Lenses

data SymLens m k c a b = SymLens { missing :: Box k m => m (k c), putr :: Box k m => a -> StateT (k c) m b, putl :: Box k m => b -> StateT (k c) m a }

-- ** Monadic combinators

-- runs an effect in both directions; the boolean allows to configure what happens in each
effectSym :: (Box k m,Box k n) => (n c -> m c) -> (forall x . Bool -> c -> n x -> m x) -> SymLens n k c a b -> SymLens m k c a b
effectSym g f l = SymLens missing' putr' putl' where
	missing' = g (missing l >>= unbox) >>= box
	putr' x = State.get >>= \k -> State.mapStateT (\n -> unbox k >>= \c -> f True c n) $ putr l x
	putl' y = State.get >>= \k -> State.mapStateT (\n -> unbox k >>= \c -> f False c n) $ putl l y

evalStateTSym :: (Box k (StateT s m),Box k m) => s -> SymLens (StateT s m) k c a b -> SymLens m k c a b
evalStateTSym s l = effectSym (\m -> State.evalStateT m s) (\b c m -> State.evalStateT m s) l

getStateSym :: (Box k m,MonadState s m) => SymLens m k s () s
getStateSym = addSym State.get

modifyStateWithSym :: (Box k m,MonadState s m) => (Bool -> a -> s -> s) -> SymLens m k a a ()
modifyStateWithSym f = effectSym id (\b a m -> do { x <- m; State.modify (\s -> f b a s) ; return x }) (termSym $ fail "putStateSym")

liftTSym :: (Box k m,Box k (t m),MonadTrans t) => SymLens m k c a b -> SymLens (t m) k c a b
liftTSym l = effectSym lift (\b -> const lift) l

-- ** General Combinators

idSym :: Box k m => SymLens m k () a a
idSym = SymLens (box ()) return return

dualSym :: SymLens m k c a b -> SymLens m k c b a
dualSym l = l { putr = putl l, putl = putr l }

liftSym :: Box k m => Lens m (k c) (k c') -> SymLens m k c' a b -> SymLens m k c a b
liftSym lc l = SymLens missing' putr' putl' where
	missing' = do
		c' <- missing l
		c <- create lc c'
		return c
	putr' x = liftStateTLens lc (putr l x)
	putl' y = liftStateTLens lc (putl l y)

liftSym' :: Box k m => Lens m c (k c') -> SymLens m k c' a b -> SymLens m k c a b
liftSym' lc l = SymLens missing' putr' putl' where
	missing' = do
		c' <- missing l
		c <- create (unboxLns >.> lc) c'
		return c
	putr' x = liftStateTLens (unboxLns >.> lc) (putr l x)
	putl' y = liftStateTLens (unboxLns >.> lc) (putl l y)

termSym :: Box k m => m a -> SymLens m k a a ()
termSym mc = SymLens (mc >>= box) putr' putl' where
	putr' x = liftStateTLens unboxLns (State.put x) >> return ()
	putl' () = State.get >>= lift . unbox

addSym :: Box k m => m a -> SymLens m k a () a
addSym c = dualSym (termSym c)

infixr 9 <.>
(<.>) :: Box k m => SymLens m k c1 a b -> SymLens m k c2 b c -> SymLens m k (k c1,k c2) a c
l1 <.> l2 = SymLens missing' putr' putl' where
	missing' = missing l1 >>= \c1 -> missing l2 >>= \c2 -> box (c1,c2)
	putr' x = do
		y <- liftStateTLens (unboxLns >.> fstLns) $ putr l1 x
		z <- liftStateTLens (unboxLns >.> sndLns) $ putr l2 y
		return z
	putl' z = do
		y <- liftStateTLens (unboxLns >.> sndLns) $ putl l2 z
		x <- liftStateTLens (unboxLns >.> fstLns) $ putl l1 y
		return x

infixr 9 <!.>
(<!.>) :: Box k m => SymLens m k () a b -> SymLens m k c2 b c -> SymLens m k c2 a c
l1 <!.> l2 = liftSym (addfstOneLns >.> (boxLns >*> idLns) >.> boxLns) (l1 <.> l2)

infixr 9 <.!>
(<.!>) :: Box k m => SymLens m k c1 a b -> SymLens m k () b c -> SymLens m k c1 a c
l1 <.!> l2 = liftSym (addsndOneLns >.> (idLns >*> boxLns) >.> boxLns) (l1 <.> l2)

-- ** Products

fstSym :: Box k m => m (a -> b) -> SymLens m k (a -> b) (a,b) a
fstSym mf = SymLens (mf >>= box) putr' putl' where
	putr' (x,y) = liftStateTLens unboxLns $ State.put (const y) >> return x
	putl' x = State.get >>= lift . unbox >>= \f -> return (x,f x)

-- the most possibly general version that performs an equality instead of forcing the original value to be restored
fstEqSym :: (Box k m,Eq a) => (a -> m b) -> SymLens m k (Maybe (a,b)) (a,b) a
fstEqSym mf = SymLens (box Nothing) putr' putl' where
	putr' (x,y) = liftStateTLens unboxLns $ State.put (Just (x,y)) >> return x
	putl' x = State.get >>= lift . unbox >>= \s -> case s of
		Just (x',y') -> if x==x' then return (x,y') else lift (mf x) >>= return . (x,)
		Nothing -> lift (mf x) >>= return . (x,)

sndSym :: Box k m => m (b -> a) -> SymLens m k (b -> a) (a,b) b
sndSym mf = SymLens (mf >>= box) putr' putl' where
	putr' (x,y) = liftStateTLens unboxLns $ State.put (const x) >> return y
	putl' y = State.get >>= lift . unbox >>= \f -> return (f y,y)

sndSym' :: Box k m => m a -> SymLens m k (a) (a,b) b
sndSym' mf = SymLens (mf >>= box) putr' putl' where
	putr' (x,y) = liftStateTLens unboxLns $ State.put x >> return y
	putl' y = State.get >>= lift . unbox >>= \x -> return (x,y)

addFstSym :: Box k m => m (b -> a) -> SymLens m k (b -> a) b (a,b)
addFstSym f = dualSym (sndSym f)

addSndSym :: Box k m => m (a -> b) -> SymLens m k (a -> b) a (a,b)
addSndSym f = dualSym (fstSym f)

addSndEqSym f = dualSym (fstEqSym f)

-- | Diagonal symmetric lens. The boolean function chooses between left or right elements and is used when the values differ
diagSym :: (Eq a,Box k m) => m ((a,a) -> Bool) -> SymLens m k (Maybe (Either a a)) a (a,a)
diagSym mp = SymLens missing' putr' putl' where
	missing' = box Nothing -- there is a default option here
	putr' x = liftStateTLens unboxLns $ State.get >>= \e -> case e of
		Nothing -> return (x,x)
		Just (Left y) -> if x == y
			then State.put Nothing >> return (x,x)
			else State.put (Just $ Left y) >> return (y,x)
		Just (Right y) -> if x == y
			then State.put Nothing >> return (x,x)
			else State.put (Just $ Right y) >> return (x,y)
	putl' (x,x') = liftStateTLens unboxLns $ if x == x'
		then State.put Nothing >> return x
		else lift mp >>= \p -> if p (x,x')
			then State.put (Just $ Right x') >> return x
			else State.put (Just $ Left x) >> return x'

infix 7 <*>
(<*>) :: Box k m => SymLens m k c1 a c -> SymLens m k c2 b d -> SymLens m k (k c1,k c2) (a,b) (c,d)
l1 <*> l2 = SymLens missing' putr' putl' where
	missing' = missing l1 >>= \c1 -> missing l2 >>= \c2 -> box (c1,c2)
	putr' (x,y) = do
		z <- liftStateTLens (unboxLns >.> fstLns) $ putr l1 x
		w <- liftStateTLens (unboxLns >.> sndLns) $ putr l2 y
		return (z,w)
	putl' (z,w) = do
		x <- liftStateTLens (unboxLns >.> fstLns) $ putl l1 z
		y <- liftStateTLens (unboxLns >.> sndLns) $ putl l2 w
		return (x,y)

infix 7 <!*>
(<!*>) :: Box k m => SymLens m k () a c -> SymLens m k c2 b d -> SymLens m k c2 (a,b) (c,d)
l1 <!*> l2 = liftSym (addfstOneLns >.> (boxLns >*> idLns) >.> boxLns) (l1 <*> l2)

infix 7 <*!>
l1 <*!> l2 = liftSym (addsndOneLns >.> (idLns >*> boxLns) >.> boxLns) (l1 <*> l2)

-- ** Sums

inlSym :: Box k m => m (b -> a) -> SymLens m k (b -> a,Maybe b) a (Either a b)
inlSym mf = SymLens missing' putr' putl' where
	missing' = mf >>= \f -> box (f,Nothing) -- there is a default option here
	putr' x = liftStateTLens unboxLns $ State.get >>= \(f,my) -> case my of
		Nothing -> State.put (const x,Nothing) >> return (Left x)
		Just y -> State.put (const x,Just y) >> return (Right y)
	putl' (Left x) = liftStateTLens unboxLns $ State.put (const x,Nothing) >> return x
	putl' (Right y) = liftStateTLens unboxLns $ State.get >>= \(f,my) -> State.put (f,Just y) >> return (f y)

inrSym :: Box k m => m (a -> b) -> SymLens m k (a -> b,Maybe a) b (Either a b)
inrSym mf = SymLens missing' putr' putl' where
	missing' = mf >>= \f -> box (f,Nothing) -- there is a default option here
	putr' y = liftStateTLens unboxLns $ State.get >>= \(f,mx) -> case mx of
		Nothing -> State.put (const y,Nothing) >> return (Right y)
		Just x -> State.put (const y,Just x) >> return (Left x)
	putl' (Left x) = liftStateTLens unboxLns $ State.get >>= \(f,mx) -> State.put (f,Just x) >> return (f x)
	putl' (Right y) = liftStateTLens unboxLns $ State.put (const y,Nothing) >> return y

switchSym :: Box k m => m (a -> Bool) -> SymLens m k (a -> Bool) (Either a a) a
switchSym mf = SymLens missing' putr' putl' where
	missing' = mf >>= box
	putr' (Left x) = liftStateTLens unboxLns (State.put $ const True) >> return x
	putr' (Right x) = liftStateTLens unboxLns (State.put $ const False) >> return x
	putl' x = State.get >>= lift . unbox >>= \f -> if f x then return (Left x) else return (Right x)

condSym :: Box k m => m (a -> Bool) -> SymLens m k (a -> Bool) a (Either a a)
condSym mf = dualSym (switchSym mf)

infix 5 <+>
(<+>) :: Box k m => SymLens m k c1 x z -> SymLens m k c2 y w -> SymLens m k (k c1,k c2) (Either x y) (Either z w)
l1 <+> l2 = SymLens missing' putr' putl' where
	missing' = missing l1 >>= \c1 -> missing l2 >>= \c2 -> box (c1,c2)
	putr' (Left x) = liftM Left $ liftStateTLens (unboxLns >.> fstLns) $ putr l1 x
	putr' (Right y) = liftM Right $ liftStateTLens (unboxLns >.> sndLns) $ putr l2 y
	putl' (Left z) = liftM Left $ liftStateTLens (unboxLns >.> fstLns) $ putl l1 z
	putl' (Right w) = liftM Right $ liftStateTLens (unboxLns >.> sndLns) $ putl l2 w

infix 5 <!+>
(<!+>) :: Box k m => SymLens m k () x z -> SymLens m k c2 y w -> SymLens m k c2 (Either x y) (Either z w)
l1 <!+> l2 = liftSym (addfstOneLns >.> (boxLns >*> idLns) >.> boxLns) (l1 <+> l2)

infix 5 <+!>
(<+!>) :: Box k m => SymLens m k c1 x z -> SymLens m k () y w -> SymLens m k c1 (Either x y) (Either z w)
l1 <+!> l2 = liftSym (addsndOneLns >.> (idLns >*> boxLns) >.> boxLns) (l1 <+> l2)

-- | Forgetful symmetric sum lens with left-biased missing
infix 5 <++>
(<++>) :: Box k m => SymLens m k c1 x z -> SymLens m k c2 y w -> SymLens m k (Either (k c1) (k c2)) (Either x y) (Either z w)
l1 <++> l2 = SymLens missing' putr' putl' where
	missing' = liftM Left (missing l1) >>= box
	putr' (Left x) = liftM Left $ liftStateTLens (unboxLns >.> uninlNonLns (missing l1)) $ putr l1 x
	putr' (Right y) = liftM Right $ liftStateTLens (unboxLns >.> uninrNonLns (missing l2)) $ putr l2 y
	putl' (Left x) = liftM Left $ liftStateTLens (unboxLns >.> uninlNonLns (missing l1)) $ putl l1 x
	putl' (Right y) = liftM Right $ liftStateTLens (unboxLns >.> uninrNonLns (missing l2)) $ putl l2 y

-- | Forgetful symmetric sum lens with right-biased missing
infix 5 <+++>
(<+++>) :: Box k m => SymLens m k c1 x z -> SymLens m k c2 y w -> SymLens m k (Either (k c2) (k c1)) (Either x y) (Either z w)
l1 <+++> l2 = coswapSym <!.> ((l2 <++> l1) <.!> coswapSym)

-- ** Isomorphisms

isoSym :: Box k m => (a -> b) -> (b -> a) -> SymLens m k () a b
isoSym f g = SymLens (box ()) (return . f) (return . g)

addfstOneSym :: Box k m => SymLens m k () a ((),a)
addfstOneSym = isoSym (\x -> ((),x)) (\((),x) -> x)

remfstOneSym :: Box k m => SymLens m k () ((),a) a
remfstOneSym = dualSym addfstOneSym

addsndOneSym :: Box k m => SymLens m k () a (a,())
addsndOneSym = isoSym (\x -> (x,())) (\(x,()) -> x)

remsndOneSym :: Box k m => SymLens m k () (a,()) a
remsndOneSym = dualSym addsndOneSym

outSym :: (Box k m,InOut a) => SymLens m k () a (F a)
outSym = isoSym out inn

innSym :: (Box k m,InOut a) => SymLens m k () (F a) a
innSym = isoSym inn out

swapSym :: Box k m => SymLens m k () (a,b) (b,a)
swapSym = isoSym swap swap

assoclSym :: Box k m => SymLens m k () (a,(b,c)) ((a,b),c)
assoclSym = isoSym assocl assocr   

assocrSym :: Box k m => SymLens m k () ((a,b),c) (a,(b,c)) 
assocrSym = isoSym assocr assocl

sublSym :: Box k m => SymLens m k () ((a,c),b) ((a,b),c)
sublSym = isoSym subl subl 

subrSym :: Box k m => SymLens m k () (b,(a,c)) (a,(b,c))
subrSym = isoSym subr subr

distpSym :: Box k m => SymLens m k () ((a,c),(b,d)) ((a,b),(c,d))
distpSym = isoSym distp distp

coswapSym :: Box k m => SymLens m k () (Either a b) (Either b a)
coswapSym = isoSym coswap coswap

cosublSym :: Box k m => SymLens m k () (Either (Either a c) b) (Either (Either a b) c)
cosublSym = isoSym cosubl cosubl

cosubrSym :: Box k m => SymLens m k () (Either b (Either a c)) (Either a (Either b c))
cosubrSym = isoSym cosubr cosubr

coassocrSym :: Box k m => SymLens m k () (Either (Either a b) c) (Either a (Either b c))
coassocrSym = isoSym coassocr coassocl

coassoclSym :: Box k m => SymLens m k () (Either a (Either b c)) (Either (Either a b) c)
coassoclSym = isoSym coassocl coassocr

undistlSym :: Box k m => SymLens m k () (Either (a,c) (b,c)) (Either a b,c)
undistlSym = isoSym undistl distl

distlSym :: Box k m => SymLens m k () (Either a b,c) (Either (a,c) (b,c))
distlSym = isoSym distl undistl

distrSym :: Box k m => SymLens m k () (a,Either b c) (Either (a,b) (a,c))
distrSym = isoSym distr undistr

undistrSym :: Box k m => SymLens m k () (Either (a,b) (a,c)) (a,Either b c)
undistrSym = isoSym undistr distr

distsSym :: Box k m => SymLens m k () (Either a b,Either c d) (Either (Either (a,c) (a,d)) (Either (b,c) (b,d)))
distsSym = distlSym <!.> (distrSym <!+> distrSym)

undistsSym :: Box k m => SymLens m k () (Either (Either (a,c) (a,d)) (Either (b,c) (b,d))) (Either a b,Either c d)
undistsSym = (undistrSym <!+> undistrSym) <.!> undistlSym

-- * Unidirectional functions

swap (x,y) = (y,x)
assocl (x,(y,z)) = ((x,y),z)
assocr ((x,y),z) = (x,(y,z))
subl ((x,y),z) = ((x,z),y)
subr (x,(y,z)) = (y,(x,z))
distp ((x,y),(z,w)) = ((x,z),(y,w))

coswap = either Right Left
coassocl = either (Left . Left) (either (Left . Right) Right)
coassocr = either (either Left (Right . Left)) (Right . Right)
cosubl = either (either (Left . Left) Right) (Left . Right)
cosubr = either (Right . Left) (either Left (Right . Right))

distl (ab,c) = either (\a -> Left (a,c)) (\b -> Right (b,c)) ab
undistl = either (\(a,c) -> (Left a,c)) (\(b,c) -> (Right b,c))

distr (a,bc) = either (\b -> Left (a,b)) (\c -> Right (a,c)) bc
undistr = either (\(a,b) -> (a,Left b)) (\(a,c) -> (a,Right c))





