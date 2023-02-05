module Html.Markup
  ( Document
  , Structure(..)
  , Context(..)
  )
where

import Numeric.Natural

type Document
  = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving Show

data Context
  = CtxHeading Natural String
  | CtxParagraph [String]
  | CtxUnorderedList [String]
  | CtxOrderedList [String]
  | CtxCodeBlock [String]