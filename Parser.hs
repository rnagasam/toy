{-# LANGUAGE OverloadedStrings #-}

module Parser where
import           Control.Applicative   hiding ((<|>))
import           Data.Functor.Identity (Identity)
import qualified Data.Text             as T
import           Text.Parsec
import           Text.Parsec.Expr
import qualified Text.Parsec.Language  as Lang
import           Text.Parsec.Text
import qualified Text.Parsec.Token     as Tok
import           ToyExpr

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style 
