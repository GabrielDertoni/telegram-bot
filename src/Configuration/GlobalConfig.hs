module Configuration.GlobalConfig
  ( Configuration(..)
  , globalConfig
  )
  where

import qualified Dataproviders.WolframDataprovider as Provider
import qualified Dataproviders.FunfactDataprovider as Provider
import qualified Dataproviders.IDKDataprovider     as Provider
import qualified Dataproviders.BotInfoDataprovider as Provider

import qualified Interface.BotInfo                 as I
import qualified Interface.GetFunfact              as I
import qualified Interface.GetIDK                  as I
import qualified Interface.GetAnswer               as I

data Configuration
  = Configuration { wolframDataprovider :: Provider.WolframDataprovider
                  , funfactDataprovider :: Provider.FunfactDataprovider Provider.BotInfoDataprovider
                  , idkDataprovider     :: Provider.IDKDataprovider
                  , botInfoDataprovider :: Provider.BotInfoDataprovider
                  }

globalConfig :: Configuration
globalConfig
  = Configuration { wolframDataprovider = Provider.wolframDataprovider
                  , funfactDataprovider = Provider.funfactDataprovider Provider.botInfoDataprovider
                  , idkDataprovider     = Provider.idkDataprovider
                  , botInfoDataprovider = Provider.botInfoDataprovider
                  }

instance I.BotInfo Configuration where
  getInfo cfg = I.getInfo $ botInfoDataprovider cfg
  setInfo cfg = I.setInfo $ botInfoDataprovider cfg
  getIncFunfactInfo cfg = I.getIncFunfactInfo $ botInfoDataprovider cfg

instance I.GetFunfact Configuration where
  getFunfact cfg = I.getFunfact $ funfactDataprovider cfg

instance I.GetIDK Configuration where
  getIDK cfg = I.getIDK $ idkDataprovider cfg

instance I.GetAnswer Configuration where
  getAnswer cfg = I.getAnswer $ wolframDataprovider cfg

