{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}

module HKTDemo where

import Data.Kind

-- >>> toCSV ["a", "b", "c"]
-- "\"a\",\"b\",\"c\""

toCSV :: (Show a) => [a] -> String
toCSV =
  let addField :: (Show a) => String -> a -> String
      addField s a = s <> "," <> show a

      dropLeadingComma :: String -> String
      dropLeadingComma [] = []
      dropLeadingComma s = case s of
        ',' : s' -> s'
        _ -> s
   in dropLeadingComma . foldl addField ""

-- >>> import Data.List.NonEmpty
-- >>> toCSV' (3 :| [1,2,4])
-- "3,1,2,4"

-- >>> toCSV' $ Just 1
-- "1"

-- >>> toCSV' @Maybe @Int $ Just 1
-- "1"

toCSV' ::
  forall (t :: Type -> Type) (a :: Type).
  (Foldable t, Show a) =>
  t a -> String
toCSV' =
  let addField :: (Show a) => String -> a -> String
      addField s a = s <> "," <> show a

      dropLeadingComma :: String -> String
      dropLeadingComma s = case s of
        ',' : s' -> s'
        _ -> s
   in dropLeadingComma . foldl addField ""

-- >>> pick [1,2,3] [4,5]
-- [1,2,3,4,5]
-- >>> pick (Just 1) Nothing
-- Just 1
-- >>> foldl1 pick [Nothing, Nothing, Just 2, Nothing]
-- Just 2
class Select (f :: Type -> Type) where
  empty :: f a
  pick :: f a -> f a -> f a

instance Select Maybe where
  empty :: Maybe a
  empty = Nothing
  pick :: Maybe a -> Maybe a -> Maybe a
  pick Nothing a = a
  pick a _ = a

instance Select [] where
  empty :: [a]
  empty = []
  pick :: [a] -> [a] -> [a]
  pick = (<>)
