{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module ToyExpr where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Text as T

data Attr
    = Length Double
    | Width Double
    | Height Double
    | Location (Double, Double, Double)
    | Rotation Double
    deriving (Show)

data Part
    = Box [Attr]
    | Cyl [Attr]
    deriving (Show)

data ToyExpr
    = Atomic Part
    | List [ToyExpr]
    | Number Integer
    | String T.Text
    | Bool Bool
    | Let T.Text ToyExpr Expr
    | If ToyExpr ToyExpr ToyExpr
    | Fun IFunc
    | Lambda IFunc EnvCtx

data IFunc = IFunc { fn :: [ToyExpr] -> Eval ToyExpr }

type EnvCtx = Map.Map T.Text ToyExpr

newtype Eval a = Eval { unEval :: ReaderT EnvCtx IO a }
    deriving ( Monad
             , Functor
             , Applicative
             , MonadReader EnvCtx
             , MonadIO )

instance Show ToyExpr where
    show = T.unpack . showExpr
   
showExpr :: ToyExpr -> T.Text
showExpr val =
    case val of
      (Atomic p) -> T.pack $ show p
      (String str) -> T.concat ["\"", str, "\""]
      (Number num) -> T.pack $ show num
      (Bool True) -> "true"
      (Bool False) -> "false"
      (List contents) -> T.concat ["{", T.unwords $ showExpr <$> contents, "}"]
      (Fun _) -> "<function>"
      (Lambda _ _) -> "<lambda>"
