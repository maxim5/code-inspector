###
# Copyright © Romain Fouquet, 2012-2015
# Copyright © Clément Joly, 2012-2015
#
# romain.fouquet18@gmail.com
# leowzukw@vmail.me
#
# This file is part of Diseases Killer Academy.
#
# Diseases Killer Academy is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Diseases Killer Academy is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with Diseases Killer Academy.  If not, see http://www.gnu.org/licenses/agpl-3.0.html.
###

'use strict'

class dka.Loader
  constructor: ->
    @fileManifest = [
        src: 'enemies/DiseaseSplash.png' # TODO: rename all files
      ,
        src: 'enemies/Plague.png'
      ,
        src: 'FEL/felSplash.png'
      ,
        src: 'FEL/electron.png'
      ,
        src: 'enemies/HIV.png'
      ,
        src: 'enemies/Tuberculosis.png'
      ,
        src: 'labs/waveFlag1.png'
      ,
        src: 'labs/vaccine1.png'
      ,
        src: 'labs/fever1.png'
      ,
        src: 'labs/back.png'
      ,
        src: 'labs/reset.png'
      ,
        src: 'labs/undo.png'
      ,
        src: 'timeline/timeline.png'
      ,
        src: 'timeline/clock.png'
      ,
        src: 'timeline/level0Bp.png'
      ,
        src: 'timeline/labsBp.png'
      ,
        src: 'timeline/creditsBp.png'
      ,
        src: 'interface/capsuleBP.png'
      ,
        src: 'interface/tabletBP.png'
      ,
        src: 'interface/credits.png'
      ,
        src: 'interface/BaseBP1.png'
      ,
        src: 'interface/tutoBack.png'
      ,
        src: 'interface/tutoSplash1.png'
      ,
        src: 'interface/tutoSplash2.png'
      ,
        src: 'interface/nextButton.png'
      ,
        src: 'interface/closeButton.png'
      ,
        src: 'interface/bpStart.png'
      ,
        src: 'interface/BCell.png'
      ,
        src: 'interface/pauseBck.png'
      ,
        src: 'interface/MacroCell.png'
      ,
        src: 'interface/bpMenu.png'
      ,
        src: 'interface/menuStartBg.png'
      ,
        src: 'interface/pauseButtonHolder.png'
      ,
        src: 'interface/callWave.png'
      ,
        src: 'interface/coinHud.png'
      ,
        src: 'interface/dnaHud.png'
      ,
        src: 'interface/waveHud.png'
      ,
        src: 'interface/educ.png'
      ,
        src: 'interface/subsidies.png'
      ,
        src: 'interface/tutoBubbleSmall.png'
      ,
        src: 'interface/tutoBubbleLarge.png'
      ,
        src: 'interface/feverHalo.png'
      ,
        src: 'interface/gradhat.png'
      ,
        src: 'interface/lifeIco.png'
      ,
        src: 'interface/speedIco.png'
      ,
        src: 'interface/lifeCostIco.png'
      ,
        src: 'interface/virusBar.png'
      ,
        src: 'interface/bacteriumBar.png'
      ,
        src: 'interface/hexMenu.png'
      ,
        src: 'interface/hudBar.png'
      ,
        src: 'interface/msgBalloon.png'
      ,
        src: 'interface/LevelsBP.png'
      ,
        src: 'interface/lifeHud.png'
      ,
        src: 'interface/SettingsPauseBP.png'
      ,
        src: 'interface/LevelsPauseBP.png'
      ,
        src: 'interface/RetryPauseBP.png'
      ,
        src: 'interface/MuteSettingsBP.png'
      ,
        src: 'interface/AutopauseSettingsBP.png'
      ,
        src: 'interface/vaccineBP.png'
      ,
        src: 'interface/mysterious.png'
      ,
        src: 'interface/leftCorner.png'
      ,
        src: 'interface/border.png'
      ,
        src: 'interface/PauseBP.png'
      ,
        src: 'interface/EncyclopediaBP.png'
      ,
        src: 'interface/encyclopediaBk.png'
      ,
        src: 'interface/syringueSP.png'
      ,
        src: 'interface/feverBP.png'
      ,
        src: 'interface/TryAgainBP.png'
      ,
        src: 'interface/VaccineStain.png'
      ,
        src: 'interface/tutoBg.png'
      ,
        src: 'interface/goalMarker.png'
      ,
        src: 'levels/FondTest3.png'
      ,
        src: 'levels/map0.png'
      ,
        src: 'levels/hospital0.png'
      ,
        src: 'towers/antibodyIgA.png'
      ,
        src: 'towers/antibodyIgG.png'
      ,
        src: 'towers/antibodyIgM.png'
      ,
        src: 'towers/Capsule.png'
      ,
        src: 'towers/CapsuleSmoke.png'
      ,
        src: 'towers/CellB1.png'
      ,
        src: 'towers/CellMacrophage.png'
      ,
        src: 'interface/crateBP.png'
      ,
        src: 'towers/towerLocation.png'
      ,
        src: 'towers/ReloadBP.png'
      ,
        src: 'towers/Syringue1.png'
      ,
        src: 'towers/SyringueEff1.png'
      ,
        src: 'towers/Tablet.png'
      ,
        src: 'towers/TabletSmoke.png'
      ,
        src: 'towers/towerPill.png'
      ,
        src: 'towers/TCellPatrol1.png'
      ,
        src: 'towers/Tour2.png'
      ,
        src: 'towers/TowerCellB0.png'
      ,
        src: 'towers/TowerCellMacrophage.png'
      ,
        src: 'interface/upgradeDnaBP.png'
      ,
        src: 'interface/upgradeGoldBP.png'
    ]

    @load()

  load: ->
    @clock = gameStage.addChild new dka.LoadingClock()

    dka.academy.addStageTicker()

    queue = @queue = new createjs.LoadQueue true, 'dist/assets/'
    queue.installPlugin createjs.Sound
    createjs.Sound.alternateExtensions = ['mp3'] # TODO: move this in SoundManager
    queue.on 'progress', @handleProgress, this
    queue.on 'complete', @handleComplete, this
    queue.on 'error', @handleError, this
    queue.loadManifest @fileManifest

  handleProgress: (e) ->
    @clock.update e.progress

  handleComplete: (e) ->
    gameStage.removeAllChildren()

    gameStage.enableMouseOver 20
    gameStage.mouseMoveOutside = true

    @initResources()

    dka.academy.launchMagic()

  handleError: (e) ->
    console.log 'Error Loader: ' + e.item.src # FIXME: do something more clever ;)

  initResources: ->
    for asset of @fileManifest
      assetName = @fileManifest[asset].src.match(/[^\/]*$/)[0].split('.')[0]
      dka.academy.assetManager.addAsset assetName, @queue.getResult(@fileManifest[asset].src)

    (new dka.SpriteSheets).build()
