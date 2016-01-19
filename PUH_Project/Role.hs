-- @role.hs
{-# LANGUAGE TemplateHaskell #-}
module Role where

import Database.Persist.TH

data Role = Student Integer -- Academic Year shorthand (2015 for 2015/16)
    | TA Integer Integer -- AY shorthand range (inclusive)
    | Professor deriving (Eq, Ord, Show, Read)
    
derivePersistField "Role"