module Main (main) where

infixr 5 				:<|>
infixr 6 				:<>
infixr 6 				<>

data DOC = NIL
		 | DOC :<> DOC
		 | NEST Int DOC
		 | TEXT String
		 | LINE
		 | DOC :<|> DOC
		 deriving Show


nil :: DOC
nil 					= NIL

(<>) :: DOC -> DOC -> DOC
x <> y 					= x :<> y

nest :: Int -> DOC -> DOC
nest i x 				= NEST i x

text :: String -> DOC
text s 					= TEXT s

line :: DOC
line 					= LINE



data Tree               = Node String [Tree]

showTree :: Tree -> DOC
showTree (Node s ts)    = text s <> showBracket ts

showBracket :: [Tree] -> DOC
showBracket []          = nil
showBracket ts          = text "[" <>
                          nest 2 (line <> showTrees ts) <>
				          line <> text "]"

showTrees :: [Tree] -> DOC
showTrees [t]           = showTree t
showTrees (t:ts)        = showTree t <> text "," <> line <> showTrees ts


tree :: Tree
tree 					= Node "aaa" [
							Node "bbbbb" [],
							Node "eee" [],
							Node "ffff" []
							]

main :: IO ()
main = putStrLn (show (showTrees [tree]))
