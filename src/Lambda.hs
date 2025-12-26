{-# LANGUAGE OverloadedStrings #-}

module Lambda where

import Data.Kind (Type)
import Data.Text (Text)

type Name :: Type
type Name = Text

type Lambda :: Type
data Lambda
  = Var Name
  | Name :-> Lambda
  | Lambda :$ Lambda
  | Name := Lambda
  deriving (Show)

prettyP :: Lambda -> Text
prettyP (Var x) = x
prettyP (x :-> term) = "λ" <> x <> "." <> prettyP term
prettyP (x := body) = x <> "=" <> prettyP body
prettyP (lambda :$ body) = "(" <> prettyP lambda <> " " <> prettyP body <> ")"

id' :: Lambda
id' = "id" := ("x" :-> Var "x")

-- >>> import Text.Show.Unicode (uprint)
-- >>> uprint $ prettyP id'

zero :: Lambda
zero = "f" :-> ("x" :-> Var "x")

-- >>> import Text.Show.Unicode (uprint)
-- >>> uprint $ prettyP zero

one :: Lambda
one = "f" :-> ("x" :-> (Var "f" :$ Var "x"))

-- >>> import Text.Show.Unicode (uprint)
-- >>> uprint $ prettyP one
