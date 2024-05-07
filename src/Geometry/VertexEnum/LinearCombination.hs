{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
module Geometry.VertexEnum.LinearCombination
  ( LinearCombination (..)
  , Var
  , newVar
  , VarIndex
  , linearCombination
  , constant
  , cst
  , toRationalLinearCombination
  )
  where
import           Data.AdditiveGroup ( AdditiveGroup(zeroV, negateV, (^+^)) )
import           Data.IntMap.Strict ( IntMap, mergeWithKey )
import qualified Data.IntMap.Strict as IM
import           Data.List          ( intercalate )
import           Data.Tuple         ( swap )
import           Data.VectorSpace   ( linearCombo, VectorSpace(..) )

newtype LinearCombination a = LinearCombination (IntMap a)

toRationalLinearCombination :: Real a => LinearCombination a -> LinearCombination Rational
toRationalLinearCombination (LinearCombination imap) = LinearCombination (IM.map toRational imap)

instance (Eq a) => Eq (LinearCombination a) where
  (==) :: LinearCombination a -> LinearCombination a -> Bool
  (==) (LinearCombination x) (LinearCombination y) = x == y

instance (Show a) => Show (LinearCombination a) where
  show :: LinearCombination a -> String
  show (LinearCombination x) =
    intercalate " + " $
      map (\(i, r) -> if i == 0
                      then show r
                      else show r ++ "*x" ++ show i
          )
          (IM.toAscList x)

instance Num a => AdditiveGroup (LinearCombination a) where
  zeroV :: LinearCombination a
  zeroV = LinearCombination (IM.singleton 0 0)
  (^+^) :: LinearCombination a -> LinearCombination a -> LinearCombination a
  (^+^) (LinearCombination imap1) (LinearCombination imap2) =
    LinearCombination
    (mergeWithKey (\_ x y -> Just (x+y)) id id imap1 imap2)
  negateV :: LinearCombination a -> LinearCombination a
  negateV (LinearCombination imap) = LinearCombination (IM.map negate imap)

instance Num a => VectorSpace (LinearCombination a) where
  type Scalar (LinearCombination a) = a
  (*^) :: Scalar (LinearCombination a) -> LinearCombination a -> LinearCombination a
  (*^) lambda (LinearCombination imap) =
    LinearCombination (IM.map (*lambda) imap)

type Var a = LinearCombination a
type VarIndex = Int

-- | new variable
newVar :: Num a => VarIndex -> Var a
newVar i = if i >= 0
            then LinearCombination (IM.singleton i 1)
            else error "newVar: negative index"

-- | linear combination from list of terms
linearCombination :: Num a => [(a, Var a)] -> LinearCombination a
linearCombination terms = linearCombo (map swap terms)
--  LinearCombination (IM.fromListWith (+) (map swap terms))

-- | constant linear combination
constant :: a -> LinearCombination a
constant x = LinearCombination (IM.singleton 0 x)

-- | alias for `constant`
cst :: a -> LinearCombination a
cst = constant
