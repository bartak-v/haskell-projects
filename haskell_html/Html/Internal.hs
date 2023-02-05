module Html.Internal where
import Html.Markup
import Data.Binary
import Data.Maybe

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


-- Append
instance Semigroup SimpleStructure where
  (<>) c1 c2 =
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



-- Excercises
-- | A data type representing colors
data Color
  = RGB Word8 Word8 Word8

getBluePart :: Color -> Word8
getBluePart color =
  case color of
    RGB _ _ blue -> blue

data Brightness
  = Dark
  | Bright

data EightColor
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

data AnsiColor
  = AnsiColor Brightness EightColor

ansiColorToVGA :: AnsiColor -> Color
ansiColorToVGA ansicolor =
  case ansicolor of
    AnsiColor Dark Black ->
      RGB 0 0 0
    AnsiColor Bright Black ->
      RGB 85 85 85
    AnsiColor Dark Red ->
      RGB 170 0 0
    AnsiColor Bright Red ->
      RGB 255 85 85
    -- and so on

isBright :: AnsiColor -> Bool
isBright color =
  case color of
    AnsiColor Bright _ -> True
    AnsiColor Dark _ -> False

ansiToUbuntu :: AnsiColor -> Color
ansiToUbuntu ansiColor =
  case ansiColor of
    AnsiColor brightness color ->
      case brightness of
        Dark ->
          case color of
            Black -> RGB 0 0 0
            Red -> RGB 194 54 33
            Green -> RGB 37 188 36
            Yellow -> RGB 173 173 39
            Blue -> RGB 73 46 225
            Magenta -> RGB 211 56 211
            Cyan -> RGB 51 187 200
            White -> RGB 203 204 205
        
        Bright ->
          case color of
            Black -> RGB 129 131 131
            Red -> RGB 252 57 31
            Green -> RGB 49 231 34
            Yellow -> RGB 234 236 35
            Blue -> RGB 88 51 255
            Magenta -> RGB 249 53 248
            Cyan -> RGB 20 240 240
            White -> RGB 233 235 235

isEmpty :: [a] -> Bool
isEmpty list =
  case listToMaybe list of
    Nothing -> True
    Just _ -> False

isEmpty2 :: [a] -> Bool
isEmpty2 list =
  case list of
    [] -> True
    _ : _ -> False

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