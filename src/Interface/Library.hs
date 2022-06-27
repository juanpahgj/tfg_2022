module Interface.Library (

-- * Exported functions

parser

) where

import Parser.Parser (parseTPDB, parseTPDB_XML, parseCOPS)
import Parser.Grammar (TRS)
import Text.ParserCombinators.Parsec.Error(ParseError)

-- | Accepted formats
data Format = TPDB | COPS | XMLTPDB

parser :: String -> Maybe Format -> TRS --String
parser s format =
    do
        case format of 
            Just TPDB -> 
                  case parseTPDB s of
                     Left parseerror
                        -> error$ "Parse Error (Main): " ++ show parseerror
                     Right sys
                        -> sys
            Just XMLTPDB -> 
                  case parseTPDB_XML s of
                     Left parseerror
                        -> error$ "Parse Error (Main): " ++ show parseerror
                     Right sys
                        -> sys
            Just COPS -> 
                  case parseCOPS s of
                     Left parseerror
                        -> error$ "Parse Error (Main): " ++ show parseerror
                     Right sys
                        -> sys
            Nothing -> anyParse s


aivailableFormats :: [(String -> Either ParseError TRS)]
aivailableFormats = [parseTPDB, parseTPDB_XML, parseCOPS]

anyParse :: String -> TRS
anyParse fdata = checkParser $ map (\(p) -> p fdata) aivailableFormats

checkParser [] = (error "Error (CLI): Format not supported") 
checkParser ((Right x):_) = x
checkParser ((Left _):xs) = checkParser xs

