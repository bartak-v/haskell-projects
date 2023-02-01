main :: IO()
main = putStrLn myhtml

myhtml :: String
myhtml = 
    makeHtml 
        "Hello title" 
        (h1_ "Hello, world!" <> p_ "world")

makeHtml :: String -> String -> String
makeHtml title content = html_ (head_ (title_ title) <> body_ content)

-- <> is operator for string concatenation
head_ :: String -> String
head_  = el "head"

title_ :: String -> String
title_  = el "title"

html_ :: String -> String
html_ = el "html" 

body_ :: String -> String
body_ = el "body" 

h1_ :: String -> String
h1_ = el "h1"

p_ :: String -> String
p_ = el "p"

-- Partial application
el :: String -> String -> String
el tag content =
    "<" <> tag <> ">" <> content <> "</" <> tag <> ">"


-- Anonymous lambda function
--three :: Integer
--three = (\num1 -> \num2 -> num1 + num2) 1 2

--three2 :: Integer
--three2 = (\num1 num2 -> num1 + num2) 1 2