main :: IO()
main = putStrLn myhtml

myhtml :: String
myhtml = makeHtml "Hello title" "Hello, world!"

makeHtml :: String -> String -> String
makeHtml title content = html_ (head_ (title_ title) <> body_ content)

-- <> is operator for string concatenation
head_ :: String -> String
head_ head =
    "<head>" <> head <> "</head>"

title_ :: String -> String
title_ title = 
    "<title>" <> title <> "</title>"

-- Partial application

html_ :: String -> String
html_ = el "html" 

body_ :: String -> String
body_ = el "body" 

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"