module Interface.Library (

-- * Exported functions

parser

) where

import Parser.Parser (parseTPDB, parseTPDB_XML, parseCOPS)
import Parser.Grammar (TRS)

-- | Accepted formats
data Format = TPDB | COPS | XMLTPDB

parser :: String -> Maybe Format -> String
parser s format =
    do
        case format of 
            Just TPDB -> 
                  case parseTPDB s of
                     Left parseerror
                        -> error$ "Parse Error (Main): " ++ show parseerror
                     Right sys
                        -> "Success: " ++ show sys
            Just XMLTPDB -> 
                  case parseTPDB_XML s of
                     Left parseerror
                        -> error$ "Parse Error (Main): " ++ show parseerror
                     Right sys
                        -> "Success: " ++ show sys
            Just COPS -> 
                  case parseCOPS s of
                     Left parseerror
                        -> error$ "Parse Error (Main): " ++ show parseerror
                     Right sys
                        -> "Success: " ++ show sys
            -- Nothing -> autoparse filename s
