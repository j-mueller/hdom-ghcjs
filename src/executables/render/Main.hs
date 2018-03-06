{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main where

import           Control.Lens        hiding (children)
import           Data.HTML
import           Data.String         (IsString (..))
import           Data.VDOM.WebWorker (runWorker)

main :: IO ()
main = runWorker 0 mainView (const id)

mainView :: (DomNode node, IsString (NamespaceType node), IsString (StringType node), Ord (StringType node)) => Int -> node (Handler Int) ()
mainView i = div_
  & children .~ (
    Right [
      button_
        & children .~ (Left "Increase")
        & onClick  ?~ (Just . succ),
      button_
        & children .~ (Left "Decrease")
        & onClick  ?~ (Just . pred),
      div_ & children .~ (Left $ fromString $ show i)])
  & style . at "border" ?~ "1px solid black"
  & attributes . at "x-count" ?~ (fromString $ show i)
