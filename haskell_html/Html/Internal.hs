module Html.Internal where
import Html.Markup

-- * Types

newtype Html
  = Html String

newtype SimpleStructure
  = SimpleStructure String

type Title
  = String


-- * Variables / Example variables

example1 :: Document
example1 = [Paragraph "Hello, world!"]

example2 :: Document
example2 = [Heading 1 "Welcome", Paragraph "To this tutorial about Haskell."]

example3 :: Document
example3 = [
            Paragraph "Remember that multiple lines with no separation \
            \are grouped together to a single paragraph but list items \
            \remain separate."
            , OrderedList 
                ["Item 1 of a list"
                , "Item 2 of the same list"
                ] 
            ]

example4 :: Document
example4 =
  [ Heading 1 "Compiling programs with ghc"
  , Paragraph "Running ghc invokes the Glasgow Haskell Compiler (GHC), and can be used to compile Haskell modules and programs into native executables and libraries."
  , Paragraph "Create a new Haskell source file named hello.hs, and write the following code in it:"
  , CodeBlock
    [ "main = putStrLn \"Hello, Haskell!\""
    ]
  , Paragraph "Now, we can compile the program by invoking ghc with the file name:"
  , CodeBlock
    [ "➜ ghc hello.hs"
    , "[1 of 1] Compiling Main             ( hello.hs, hello.o )"
    , "Linking hello ..."
    ]
  , Paragraph "GHC created the following files:"
  , UnorderedList
    [ "hello.hi - Haskell interface file"
    , "hello.o - Object file, the output of the compiler before linking"
    , "hello (or hello.exe on Microsoft Windows) - A native runnable executable."
    ]
  , Paragraph "GHC will produce an executable when the source file satisfies both conditions:"
  , OrderedList
    [ "Defines the main function in the source file"
    , "Defines the module name to be Main, or does not have a module declaration"
    ]
  , Paragraph "Otherwise, it will only produce the .o and .hi files."
  ]


-- * EDSL

html_ :: Title -> SimpleStructure -> Html
html_ title content =
  Html
    ( el "html"
      ( el "head" (el "title" (escape title))
        <> el "body" (getSimpleStructureString content)
      )
    )

p_ :: String -> SimpleStructure
p_ = SimpleStructure . el "p" . escape

h1_ :: String -> SimpleStructure
h1_ = SimpleStructure . el "h1" . escape

code_ :: String -> SimpleStructure
code_ = SimpleStructure . el "pre" . escape

ul_ :: [SimpleStructure] -> SimpleStructure
ul_  = SimpleStructure . el "ul" . concatMap (el "li" . getSimpleStructureString)

ol_ :: [SimpleStructure] -> SimpleStructure
ol_  = SimpleStructure . el "ol" . concatMap (el "li" . getSimpleStructureString)


append_ :: SimpleStructure -> SimpleStructure -> SimpleStructure
append_ c1 c2 =
  SimpleStructure (getSimpleStructureString c1 <> getSimpleStructureString c2)

-- * Render

render :: Html -> String
render html =
  case html of
    Html str -> str

-- * Utilities

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getSimpleStructureString :: SimpleStructure -> String
getSimpleStructureString content =
  case content of
    SimpleStructure str -> str

escape :: String -> String
escape =
  let
    escapeChar c =
      case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> [c]
  in
    concat . map escapeChar

-- * Parsing
parse :: String -> Document
parse = parseLines [] . lines

parseLines :: [String] -> [String] -> Document
parseLines currentParagraph txts =
  let
    paragraph = Paragraph (unlines (reverse currentParagraph))
  in
    case txts of
      [] -> [paragraph]
      currentLine : rest ->
        if trim currentLine == ""
          then
            paragraph : parseLines [] rest
          else
            parseLines (currentLine : currentParagraph) rest

trim :: String -> String
trim = unwords . words





-- map :: (a -> b) -> [a] -> [b]
-- concat :: [[a]] -> [a]
-- It flattens a list of list of something into a list of something. 


{- data Boolean = True | False

data Person = Person String Int 
-- where the first is the name and the second is the age

-- Record syntax
-- Haskell generates getters:
-- name :: Person -> String
-- age :: Person -> Int
data Person2
  = Person2
    { pName :: String
    , pAge :: Int
    }

-- Can be instantiated by Person "Gil" 32 or Person { name = "Gil", age = 32 }

-- Tuple
data Tuple a b = Tuple a b

-- Either

data Either a b = ALeft a | ARight b 


-- Recursion
add n m =
  if m /= 0
    then add (increment n) (decrement m)
    else n

-}

replicate2 :: Int -> a -> [a]
replicate2 n x =
  if n <= 0    -- recognizing the base case
    then
      []       -- the solution for the base case
    else
        x : replicate2 (n - 1) x
  --   ---  -------------------
  --    ^           ^
  --    |           |
  --    |           +-------- reduction
  --    |
  --    +--- mitigation