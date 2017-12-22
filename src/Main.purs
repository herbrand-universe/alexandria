module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)


data Lambda = Var String | App Lambda Lambda | Abs String Lambda

instance showLambda :: Show Lambda where
    show (Var x)   = x
    show (Abs x t) = "\\" <> x <> "->" <> show t
    show (App s t) = show s


idL :: Lambda
idL = Abs "x" $ Var "x"


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
