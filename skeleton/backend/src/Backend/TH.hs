{-# LANGUAGE TemplateHaskell #-}
module Backend.TH where

import Obelisk.Generated.Static

path :: FilePath
path = $(staticFilePath "test")
