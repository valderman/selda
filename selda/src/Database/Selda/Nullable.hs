{-# LANGUAGE TypeFamilies, TypeOperators, ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Convenience facilities for working with nullable columns.
module Database.Selda.Nullable
  ( NonNull, (:?~)
  , nonNull, restrict', (?!)
  , (?==), (?/=), (?>), (?<), (?>=), (?<=), (?+), (?-), (?*), (?/)
  ) where
import Database.Selda
import Database.Selda.Unsafe (cast)
import Database.Selda.Selectors
import Database.Selda.Column
import Unsafe.Coerce

-- | Two SQL types which are identical modulo nullability.
type a :?~ b =
  ( NonNull a ~ NonNull b
  , SqlType (NonNull a)
  , SqlType (NonNull b)
  )

type family NonNull a where
  NonNull (Maybe a) = a
  NonNull a         = a

-- | Unconditionally convert a nullable value into a non-nullable one,
--   using the standard SQL null-coalescing behavior.
fromNullable :: SqlType (NonNull a) => Col s a -> Col s (NonNull a)
fromNullable = unsafeCoerce

(?==), (?/=) :: (a :?~ b, SqlType a, Same s t) => Col s a -> Col t b -> Col s (Maybe Bool)

(?>), (?<), (?>=), (?<=) :: (a :?~ b, SqlOrd (NonNull a), Same s t)
                         => Col s a -> Col t b -> Col s (Maybe Bool)

(?+), (?-), (?*) :: (a :?~ b, Num (NonNull a), Same s t)
                 => Col s a -> Col t b -> Col s (Maybe (NonNull a))

(?/) :: (a :?~ b, Fractional (Col s (NonNull a)), Same s t)
     => Col s a -> Col t b -> Col s (Maybe (NonNull a))

a ?== b = cast $ fromNullable a .== fromNullable b
a ?/= b = cast $ fromNullable a ./= fromNullable b
a ?>  b = cast $ fromNullable a .>  fromNullable b
a ?<  b = cast $ fromNullable a .<  fromNullable b
a ?>= b = cast $ fromNullable a .>= fromNullable b
a ?<= b = cast $ fromNullable a .<= fromNullable b
a ?+  b = cast $ fromNullable a +   fromNullable b
a ?-  b = cast $ fromNullable a -   fromNullable b
a ?*  b = cast $ fromNullable a *   fromNullable b
a ?/  b = cast $ fromNullable a /   fromNullable b
infixl 4 ?==
infixl 4 ?/=
infixl 4 ?>
infixl 4 ?<
infixl 4 ?>=
infixl 4 ?<=
infixl 6 ?+
infixl 6 ?-
infixl 7 ?*
infixl 7 ?/

-- | Selector indexing, overloaded to work on nullable as well as non-nullable
--   rows.
(?!) :: forall s t a. SqlType a
     => Row s t -> Selector (NonNull t) a -> Col s (Coalesce (Maybe a))
(Many xs) ?! i = case xs !! (selectorIndex i) of Untyped x -> One (unsafeCoerce x)
infixl 9 ?!

-- | Converts a nullable column into a non-nullable one, yielding the empty
--   result set if the column is null.
nonNull :: (Same s t, SqlType a) => Col s (Maybe a) -> Query t (Col t a)
nonNull x = do
  restrict (not_ $ isNull x)
  return (fromNullable x)

-- | Restrict a query using a nullable expression.
--   Equivalent to @restrict . ifNull false@.
restrict' :: Same s t => Col s (Maybe Bool) -> Query t ()
restrict' = restrict . fromNullable
