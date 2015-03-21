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


						

data XML 				= Elt String [Att] [XML]
						| Txt String

data Att 				= Att String String

showXML x 				= showXMLs x

showXMLs :: XML -> Doc
showXMLs (Elt n [a] []) 	= text "<" <> showTag n [a] <> text "/>"
showXMLs (Elt n [a] (c:cs)) = text "<" <> showTag n [a] <> text ">" <>
							showXMLs c <>
							text "</" <> text n <> text ">"				
showXMLs (Txt s) 		= text s

showAtt :: Att -> Doc
showAtt (Att n v) 		= text n <> text "=" <> text (quoted v)
showAtts :: [Att] -> Doc
showAtts (x:xs) 		= showAtt x <> showAtts xs
showAtts []				= text ""

quoted :: String -> String
quoted s 				= "\"" ++ s ++ "\""

showTag :: String -> [Att] -> Doc
showTag n [a] 			= text n <> showAtts [a]


xml :: XML
xml 					= Elt "p" [
							Att "color" "red",
							Att "font" "Times",
							Att "size" "10"
							] [
							Txt "Here is some",
							Elt "em" [] [
								Txt "emphasized"
							],
							Txt "text.",
							Txt "Here is a",
							Elt "a" [
								Att "href" "http://www.eg.com/"
							] [
								Txt "link"
							],
								Txt "elsewhere."
							]
-- <p color="red"> Here is some </p>
main :: IO ()
main = putStrLn (layout (showXMLs xml))
