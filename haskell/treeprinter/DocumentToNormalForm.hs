module Main (main) where

infixr 6 				<>



data Doc = Nil
		 | String `Text` Doc
		 | Int `Line` Doc
		 deriving Show

	
nil :: Doc
nil 					= Nil

(<>) :: Doc -> Doc -> Doc
(s `Text` x) <> y		= s `Text` (x <> y)
(i `Line` x) <> y		= i `Line` (x <> y)
Nil <> y				= y

nest :: Int -> Doc -> Doc
nest i (s `Text` x) 	= s `Text` nest i x
nest i (j `Line` x)		= (i+j) `Line` nest i x
nest i Nil				= Nil

	 
	 
text :: String -> Doc
text s 					= s `Text` Nil

line :: Doc
line 					= 0 `Line` Nil 

copy i x				= [x | _ <- [1..i] ]

layout :: Doc -> String
layout (s `Text` x)		= s ++ layout x
layout (i `Line` x)		= '\n' : copy i ' ' ++ layout x
layout Nil				= ""

data Tree               = Node String [Tree]

showTree :: Tree -> Doc
showTree (Node s ts)    = text s <> showBracket ts

showBracket :: [Tree] -> Doc
showBracket []          = nil
showBracket ts          = text "[" <>
                          nest 2 (line <> showTrees ts) <>
				          line <> text "]"

showTrees :: [Tree] -> Doc
showTrees [t]           = showTree t
showTrees (t:ts)        = showTree t <> text "," <> line <> showTrees ts


tree :: Tree
tree 					= Node "aaa" [
							Node "bbbbb" [
							Node "ccc" [],
							Node "dd" []
							],
							Node "eee" [],
							Node "ffff" [
								Node "gg" [],
								Node "hhh" [],
								Node "ii" []
								]
							]

main :: IO ()
main = putStrLn (layout (showTrees [tree]))
