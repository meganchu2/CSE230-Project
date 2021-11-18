{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
--import Language.Haskell.TH
import Test.Tasty
--import Common
import Prelude

import Constants
import FlappyBird
import UI hiding (main)

prop_Height::Bool
prop_Height = height == 30

prop_Width::Bool
prop_Width = width == 40

return []
main =  $forAllProperties quickCheckResult          


