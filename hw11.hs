import SExpr
import AParser

import Data.Maybe
import Data.Char
import Control.Applicative

parseSpaces :: Parser String
parseSpaces = spaces

parseIdent :: Parser Atom
parseIdent = fmap I ident

parseNumber :: Parser Atom
parseNumber = fmap N posInt

parseAtom :: Parser Atom
parseAtom = parseSpaces *> (parseIdent <|> parseNumber)

parseAtomExpr :: Parser SExpr
parseAtomExpr = fmap A parseAtom

parseOpening :: Parser Char
parseOpening = parseSpaces *> char '(' <* parseSpaces

parseClosing :: Parser Char
parseClosing = parseSpaces *> char ')'

parseCombExpr :: Parser SExpr
parseCombExpr = fmap Comb (parseOpening *>
                            (oneOrMore (parseAtomExpr <|> parseCombExpr))
                         <* parseClosing)

parseSExpr :: Parser SExpr
parseSExpr = parseAtomExpr <|> parseCombExpr

