module Main where

import Prelude
import Data.List
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.DOM.Simple.Unsafe.Element
import Data.DOM.Simple.Unsafe.Events
import Data.DOM.Simple.Unsafe.Window
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window
import DOM

brailleToString :: List Braille -> List Char
brailleToString Nil = Nil
brailleToString ((Letter O X O X X X):xs) = brailleToNums xs
brailleToString (x:xs) = (brailleLetter2String x) : (brailleToString xs)

brailleToNums :: List Braille -> List Char
brailleToNums Nil = Nil
brailleToNums ((Letter O O O O O O):xs) = (' ') : (brailleToString xs)
brailleToNums (y:xs) = (brailleToNum y) : brailleToNums xs

brailleToNum :: Braille -> Char
brailleToNum (Letter X O O O O O) = '1'
brailleToNum (Letter X O X O O O) = '2'
brailleToNum (Letter X X O O O O) = '3'
brailleToNum (Letter X X O X O O) = '4'
brailleToNum (Letter X O O X O O) = '5'
brailleToNum (Letter X X X O O O) = '6'
brailleToNum (Letter X X X X O O) = '7'
brailleToNum (Letter X O X X O O) = '8'
brailleToNum (Letter O X X O O O) = '9'
brailleToNum (Letter O X X X O O) = '0'
brailleToNum otherwise = '\t'

brailleLetter2String :: Braille -> Char
brailleLetter2String (Letter O O O O O O) = ' '
brailleLetter2String (Letter X O O O O O) = 'a'
brailleLetter2String (Letter X O X O O O) = 'b'
brailleLetter2String (Letter X X O O O O) = 'c'
brailleLetter2String (Letter X X O X O O) = 'd'
brailleLetter2String (Letter X O O X O O) = 'e'
brailleLetter2String (Letter X X X O O O) = 'f'
brailleLetter2String (Letter X X X X O O) = 'g'
brailleLetter2String (Letter X O X X O O) = 'h'
brailleLetter2String (Letter O X X O O O) = 'i'
brailleLetter2String (Letter O X X X O O) = 'j'
brailleLetter2String (Letter X O O O X O) = 'k'
brailleLetter2String (Letter X O X O X O) = 'l'
brailleLetter2String (Letter X X O O X O) = 'm'
brailleLetter2String (Letter X X O X X O) = 'n'
brailleLetter2String (Letter X O O X X O) = 'o'
brailleLetter2String (Letter X X X O X O) = 'p'
brailleLetter2String (Letter X X X X X O) = 'q'
brailleLetter2String (Letter X O X X X O) = 'r'
brailleLetter2String (Letter O X X O X O) = 's'
brailleLetter2String (Letter O X X X X O) = 't'
brailleLetter2String (Letter X O O O X X) = 'u'
brailleLetter2String (Letter X O X O X X) = 'v'
brailleLetter2String (Letter O X X X O X) = 'w'
brailleLetter2String (Letter X X O O X X) = 'x'
brailleLetter2String (Letter X X O X X X) = 'y'
brailleLetter2String (Letter X O O X X X) = 'z'
brailleLetter2String otherwise = '\t'

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
    log "Hello Sailor"
