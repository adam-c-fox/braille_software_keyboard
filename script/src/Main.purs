module Main where

import Prelude

import Data.Tuple
import Data.List
import React as R
import React.DOM as R
import React.DOM.Props as RP
import Thermite hiding (defaultMain) as T
import Thermite.Try as T

type State = Tuple (List Braille) (Braille)

nullBraille :: Braille
nullBraille = Letter O O O O O O

initialState :: State
initialState = Tuple Nil nullBraille

-- Here is the action type associated with our component:

data Bool = True | False

data Action = Increment Int | Decrement Bool

data Dot = O | X

data Braille = Letter Dot Dot Dot Dot Dot Dot

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


-- The first argument to the render function is a callback which
-- can be used to invoke actions.
--
-- Notice how the action gets attached to event handlers such as
-- onClick.

render :: T.Render State _ _
render dispatch _ state _ =
  [ R.h1' [ R.text "Lesson 2 - Actions" ]
  , R.p' [ R.text "The state is: "
         , R.text (show ((brailleToString (fst state))))
         ]
  , R.p [ RP.className "btn-group" ]
        [ R.button [ RP.className "btn btn-success"
                   , RP.onClick \_ -> dispatch (Increment 1)
                   ]
                   [ R.text "Increment" ]
        , R.button [ RP.className "btn btn-danger"
                   , RP.onClick \_ -> dispatch (Increment 2)
                   ]
                   [ R.text "Decrement" ]
        , R.button [ RP.className "btn btn-danger"
                   , RP.onClick \_ -> dispatch (Increment 3)
                   ]
                   [ R.text "Decrement" ]
        ]
  , R.p [ RP.className "btn-group" ]
        [ R.button [ RP.className "btn btn-success"
                   , RP.onClick \_ -> dispatch (Increment 4)
                   ]
                   [ R.text "Increment" ]
        , R.button [ RP.className "btn btn-danger"
                   , RP.onClick \_ -> dispatch (Increment 5)
                   ]
                   [ R.text "Decrement" ]
        , R.button [ RP.className "btn btn-danger"
                   , RP.onClick \_ -> dispatch (Increment 6)
                   ]
                   [ R.text "Decrement" ]
        ]
  , R.p [ RP.className "btn-group" ]
        [ R.button [ RP.className "btn btn-danger"
                   , RP.onClick \_ -> dispatch (Decrement False)
                   ]
                   [ R.text "Decrement" ]
        , R.button [ RP.className "btn btn-danger"
                   , RP.onClick \_ -> dispatch (Decrement True)
                   ]
                   [ R.text "Decrement" ]
        ]
  , R.p'  [ R.text "Go to "
          , R.a [ RP.href "?gist=0e1b6ed00421ae4ae7b17a919c267199&backend=thermite"
                , RP.target "_top"
                ]
                [ R.text "Lesson 3" ]
          , R.text "."
          ]
  ]

-- The performAction function is responsible for responding to an action
-- by returning a coroutine which emits state updates.
--
-- A simple coroutine emits a single state update using the 'cotransform'
-- function.
--
-- The coroutine type can also be used asynchronously, as we will see in
-- the next lesson.

flipDot :: Dot -> Dot
flipDot O = X
flipDot X = O

adjustBraille :: Int -> Braille -> Braille
adjustBraille 1 (Letter a b c d e f) = Letter (flipDot a) b c d e f
adjustBraille 2 (Letter a b c d e f) = Letter a (flipDot b) c d e f
adjustBraille 3 (Letter a b c d e f) = Letter a b (flipDot c) d e f
adjustBraille 4 (Letter a b c d e f) = Letter a b c (flipDot d) e f
adjustBraille 5 (Letter a b c d e f) = Letter a b c d (flipDot e) f
adjustBraille 6 (Letter a b c d e f) = Letter a b c d e (flipDot f)
adjustBraille n (Letter a b c d e f) = Letter a b c d e f

removeBraille :: List Braille -> List Braille
removeBraille Nil = Nil
removeBraille xs = reverse (f (reverse xs)) where
    f :: List Braille -> List Braille
    f Nil = Nil
    f (x:xs) = xs

performAction :: T.PerformAction _ State _ Action
performAction (Increment n) _ _ = void $ T.modifyState $ \(Tuple state1 state2) -> Tuple (state1) (adjustBraille n state2)
performAction (Decrement True) _ _ = void $ T.modifyState $ \(Tuple state1 state2) -> Tuple (state1 <> (state2:Nil)) nullBraille
performAction (Decrement False) _ _ = void $ T.modifyState $ \(Tuple state1 state2) -> Tuple (removeBraille state1) state2

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render

main = T.defaultMain spec initialState
