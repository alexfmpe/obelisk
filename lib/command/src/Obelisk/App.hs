{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module Obelisk.App where

import Control.Monad.Reader (MonadIO, ReaderT (..), runReaderT)
import System.Directory (XdgDirectory (XdgData), getXdgDirectory)

import Obelisk.CliApp (Obelisk(..), ObeliskT(..), runCli)

runObelisk :: MonadIO m => Obelisk -> ObeliskT m a -> m a
runObelisk c =
    runCli (_obelisk_cliConfig c)
  . flip runReaderT c
  . unObeliskT

getObeliskUserStateDir :: IO FilePath
getObeliskUserStateDir = getXdgDirectory XdgData "obelisk"
