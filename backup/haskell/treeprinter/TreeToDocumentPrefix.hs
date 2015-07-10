module Main (main) where


data DOC = NIL
		 | UNION DOC DOC
		 | NEST Int DOC
		 | TEXT String
		 | LINE
		 deriving Show


nil :: DOC
nil 					= NIL

union :: DOC -> DOC -> DOC
union x y				= UNION x y

text :: String -> DOC
text s 					= TEXT s

nest :: Int -> DOC -> DOC
nest i line				= NEST i line

line :: DOC
line 					= LINE



data Tree               = Node String [Tree]

showTree :: Tree -> DOC
showTree (Node s ts)    = union (text s) (showBracket ts)

showBracket :: [Tree] -> DOC
showBracket []          = nil
showBracket ts          = union (text "[")
                          (union (nest 2 (union line (showTrees ts)))
				          (union line (text "]")))

showTrees :: [Tree] -> DOC
showTrees [t]           = showTree t
showTrees (t:ts)        = union (showTree t) (union (text ",") (union line (showTrees ts)))


tree :: Tree
tree 					= Node "aaa" [
							Node "bbbbb" [],
							Node "eee" [],
							Node "ffff" []
							]

main :: IO ()
main = putStrLn (show (showTrees [tree]))
