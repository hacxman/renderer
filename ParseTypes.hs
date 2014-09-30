module ParseTypes where
import Data.Complex
import Text.ParserCombinators.Parsec

data Fnc    = NameF (Either ParseError (String, [String])) (Either ParseError [Exp])
    deriving Show

data Exp    = NaryF     String [Exp]
            | VarE      String
            | ConstE    (Complex Double)
	deriving Show


