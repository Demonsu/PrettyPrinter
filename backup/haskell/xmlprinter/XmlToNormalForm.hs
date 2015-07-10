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

folddoc :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
folddoc f []						= nil
folddoc f [x]						= x
folddoc f (x:xs)					= f x (folddoc f xs)

group :: [Doc] -> Doc
group (x:xs) 						= x <> group xs
group []							= Nil

showFill :: (XML -> [Doc]) -> [XML] -> [Doc]
showFill f []						= [nil]
showFill f (x:xs)					= concat (map f (x:xs))


showXML :: XML -> Doc
showXML x 							= folddoc (<>) (showXMLs x)

showXMLs :: XML -> [Doc]
showXMLs (Elt n [] []) 				= [text "<" <> text "" <> text "/>"]
showXMLs (Elt n [] (c:cs)) 			= [text "<" <> text "" <> text ">" <>
									  group (showFill showXMLs (c:cs)) <>
									  text "</" <> text n <> text ">"]
showXMLs (Elt n (a:xs) []) 			= [text "<" <> showTag n (a:xs) <> text "/>"]
showXMLs (Elt n (a:xs) (c:cs))	 	= [text "<" <> showTag n (a:xs) <> text ">" <>
									  group (showFill showXMLs (c:cs)) <>
									  text "</" <> text n <> text ">"]				
showXMLs (Txt s) 					= [text s]

showAtt :: Att -> Doc
showAtt (Att n v) 					= text n <> text "=" <> text (quoted v)

showAtts :: [Att] -> Doc
showAtts (x:xs) 					= showAtt x <> showAtts xs
showAtts []							= text ""

quoted :: String -> String
quoted s 							= "\"" ++ s ++ "\""

showTag :: String -> [Att] -> Doc
showTag n (a:xs) 					= text n <> showAtts (a:xs)
showTag n [] 						= text n <> showAtts []

xml :: XML
xml 					= Elt "p" [
							Att "color" "red",
							Att "font" "Times",
							Att "size" "10"
							] [
							Txt "Here is some",
							Elt "em" [
							
							] [
								Txt "emphasized"
							]
							]
-- <p color="red"> Here is some </p>
main :: IO ()
main = putStrLn (layout (showXML xml))
