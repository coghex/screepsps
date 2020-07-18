module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Screeps.Game as Game

main :: Effect Unit
main = do
  game <- Game.getGameGlobal
  startUsed <- Game.getUsed game
  log $ "start: " <> show startUsed
  log "blop"
