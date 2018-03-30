/**
 * @overview
 * System that renders all graphical entities. It also creates the components Sprite,
 * Spine and Transform
 * Uses components Transform, Sprite and Spine
 *
 * @example <caption>Transform</caption>
 * {
 *      x: 0, y: 0,         //Position
 *      angle: 0,           //Rotation
 *      sx: 0, sy: 0        //Scale
 *      go: (PIXI.Element)
 * }
 *
 * @example <caption>Sprite</caption>
 * {
 *      frame: "EnemyHex0" //Atlas frame
 * }
 *
 * @example <caption>Spine</caption>
 * {
 *      data: "playerSpine.json", //Name of the json file with the spine data
 *      mixes: [                  //Transition times between spine animations
 *          {
 *              origin: "walk",   //Origin spine animation
 *              destiny: "jump",  //Destiny spine animation
 *              time: 0.2         //Transition time between origin and destiny
 *          },
 *          { "origin": "jump", "destiny": "walk", "time": 0.4 }
 *      ]
 * }
 *
 * @module hanoijs/Systems/RenderSystem
 *
 * @property {PIXI.Stage} _stage - Stage of the game
 * @property {PIXI.DisplayObjectContainer} _currentScene - Current scene of the game
 * @property {any} _renderer - Canvas or WebGL renderer
 * @property {any} _options - Configuration options of the stage
 * @property {boolean} _paused - If the render update is paused or not
 * @property {TimeSystem} _time - Time system
 */

///<reference path="../../../../lib/PIXI.d.ts" />

import EntityManager   = require("hanoijs/EntitySystem/EntityManager");
import EventModule     = require("hanoijs/Events2/EventModule");
import Device          = require("hanoijs/Data/Device");

import TimeSystem      = require("hanoijs/Systems/TimeSystem");
import Transform       = require("hanoijs/Components/Transform");

import Sprite       = require("hanoijs/Components/Sprite");
import SpriteFrame  = require("hanoijs/Components/SpriteFrame");
import Spine        = require("hanoijs/Components/Spine");
import Tiling       = require("hanoijs/Components/Tiling");
import Dialog       = require("hanoijs/Components/Dialog");

var VERSION = "{$version}";
var URL_PREFIX = (VERSION===['{','$','v','e','r','s','i','o','n','}'].join("") ? "" : VERSION+"/");

module RenderSystem {

    declare function requestAnimationFrame(animate:(TS:number)=>void);

    var _stage:PIXI.Stage;
    var _currentScene:PIXI.DisplayObjectContainer;
    var _renderer:any;
    var _options:any;
    var _paused:boolean;

    var _yDisplacement:number = 0;

    var _time:TimeSystem;

     //$("body").append("<p id='fpsCounter'></p>");
     //var counterP =  $("#fpsCounter");
     //var logInterval = 0;

    /**
     * Render initialization function. Sets width and height of canvas element.
     * @method module:hanoijs/Systems/RenderSystem#init
     * @param {any} options - Configuration options of the render system
     */
    export function init(options:any) {
        _options = options;
        _paused = false;
        _time = new TimeSystem();
        _yDisplacement = options.yDisplacement;

        // 1) Init Canvas
        _stage = new PIXI.Stage(0x000000);
        //_renderer = PIXI.autoDetectRenderer(options.width, options.height, null, true);
        if ( options.webgl /*Device.majorVersion() == 8*/ ) {
            _renderer = new PIXI.WebGLRenderer( options.width, options.height, null, true, false);
        } else {
            _renderer = new PIXI.CanvasRenderer( options.width, options.height, null, true);
        }

        document.body.appendChild(<Node>_renderer.view);

        // Origin functions for layout
        Transform.setOriginFunctions(options.width, options.height);

        // Set the renderer to allow atlas Tiling in dialogs.
        Dialog.setRenderer(_renderer);
        Tiling.setRenderer(_renderer);

        // CSS properties
        _renderer.view.style.width = options.css_width;
        _renderer.view.style.height = options.css_height;
        _renderer.view.style.maxWidth = options.max_css_width;
        _renderer.view.style.maxHeight = options.max_css_height;
        _renderer.view.style['max-width'] = options.max_css_width;
        _renderer.view.style['max-height'] = options.max_css_height;

        // 2) Event listeners
        EventModule.attach("game.render.pause", pauseResume);

        // 3) Init RAF update loop
        requestAnimationFrame(update);

        // 4) Hook window resize
        if ( !Device.isIOS() && !Device.isAndroid() ) {
            var resize = (function(){
                if( ((options.width/options.height)*window.innerHeight) >= window.innerWidth ){
                    options.css_width = options.max_css_width   = window.innerWidth + "px";
                    options.css_height = options.max_css_height = (options.height/options.width) * window.innerWidth   + "px";
                }else{
                    options.css_width = options.max_css_width   = (options.width/options.height) * window.innerHeight  + "px";
                    options.css_height = options.max_css_height = window.innerHeight  + "px";
                }
                if( _renderer ){
                    // Css properties
                    _renderer.view.style.width         = options.css_width;
                    _renderer.view.style.height        = options.css_height;
                    _renderer.view.style.maxWidth      = options.max_css_width;
                    _renderer.view.style.maxHeight     = options.max_css_height;
                    _renderer.view.style['max-width']  = options.max_css_width;
                    _renderer.view.style['max-height'] = options.max_css_height;
                }
            }); /*.bind(this)*/;

            resize();
            window.onresize=resize;
        }


    }

    var fps = 60;
    var now;
    var then = Date.now();
    var interval = 1000 / fps;
    var delta;
    var updateRequested = false;

    /**
     * @method module:hanoijs/Systems/RenderSystem#getStageInteractionManager
     * @returns {PIXI.InteractionManager}
     */
    export function getStageInteractionManager():PIXI.InteractionManager {
        return _stage.interactionManager;
    }

    export function getRendererView():any  {
        return _renderer.view;
    }

    /**
     * Update function called by requestAnimationFrame
     * @method module:hanoijs/Systems/RenderSystem#update
     * @private
     */
    function update() {
        updateRequested = false;
        //console.log("Update");
        if (!_paused) {
            requestAnimationFrame(update);
            updateRequested = true;
        }

        now = Date.now();
        delta = now - then;

        if (delta > interval) {
            then = now - (delta % interval);

            // 0) Update Timing
            _time.update();

            // 1) trigger the game.render.update
            EventModule.trigger("game.render.update", []);
            // 2) Draw to canvas
            _renderer.render(_stage);

            EventModule.trigger("after.render.update", []);

            //if (logInterval > 2) {
            //logInterval = 0;
            //counterP.text(_time.getFPS() + " FPS");
            //}
            //logInterval += TimeSystem.deltaTime;

        }

    }

    /**
     * Function to pause and resume the rendering
     * @method module:hanoijs/Systems/RenderSystem#pauseResume
     * @private
     * @param {string} e - event calling the function
     * @param {any[]} args - arguments of the trigger; args[0] {boolean}
     */
    function pauseResume(e, args:any[]) {
        var doPause = args[0];

        if (doPause && !_paused) {
            _paused = true;
            _time.setPause(true);
        } else if (!doPause && _paused) {
            _paused = false;
            _time.setPause(false);
            //requestAnimationFrame( update );
            if (!updateRequested) {
                requestAnimationFrame(update);
                //console.log("REQUESTING ANIMATION FRAME AFTER PAUSE");
            }
        }
    }

    /**
     * Function to remove a scene from the stage
     * @method module:hanoijs/Systems/RenderSystem#removeScene
     * @private
     * @param {string} e - event calling the function
     * @param {any[]} args - arguments of the trigger; args[0] {Scene}
     */
    function removeScene(e, args:any[]) {
        _stage.removeChild(args[0]);
        while (args[0].children.length > 0) {
            args[0].removeChild(args[0].getChildAt(0));
        }
    }

    /**
     * Function to add a scene to the stage. It also adds inside the scene all the
     * entities with Transform component
     * @method module:hanoijs/Systems/RenderSystem#addScene
     * @private
     * @param {string} e - event calling the function
     * @param {any[]} args - arguments of the trigger; args[0] {Scene}
     */
    function addScene(e, args:any[]) {
        // Center the element in the canvas
        _currentScene = args[0];
        //console.log("Adding scene to render system "+_currentScene);
        _currentScene.position.x = Math.floor(_renderer.width / 2);
        _currentScene.position.y = Math.floor(_renderer.height / 2) + _yDisplacement;
        _stage.addChild(_currentScene);

        var entityList = EntityManager.findEntity("Anchor");
        for (var idx in entityList) {
            //console.log("Checking transform of entity " + entityList[idx] );
            var tr = EntityManager.getComponent(entityList[idx], "Transform");
            var an = EntityManager.getComponent(entityList[idx], "Anchor");
            if (tr != null && tr.go != null) {
                //console.log("    Adding "+tr.go.x );
                if (tr.origin == null) {
                    _currentScene.addChild(tr.go);
                    tr.go.children.sort(depthCompare);
                } else {
                    var found = false;
                    var tr2, an2;
                    for (var i = 0; i < entityList.length && !found; i++) {
                        tr2 = EntityManager.getComponent(entityList[i], "Transform");
                        an2 = EntityManager.getComponent(entityList[i], "Anchor");
                        found = an2.id == tr.origin;
                    }
                    if (found) {
                        tr2.go.addChild(tr.go);
                        tr.go.children.sort(depthCompare);
                        tr2.go.children.sort(depthCompare);
                    }
                }
            }
        }

        _currentScene.children.sort(depthCompare);
    }

    // z-index sort algorithm
    function depthCompare(a,b) {
        if( a.z==null ) a.z=0;
        if( b.z==null ) b.z=0;
        if (a.z < b.z) return -1;
        if (a.z > b.z) return 1;
        return 0;
    }



    /**
     * Adds an entity with Transform component to the current scene.
     * @method module:hanoijs/Systems/RenderSystem#addGameObjectToCurrent
     * @private
     * @param {string} e - event calling the function
     * @param {any[]} args - arguments of the trigger; args[0] {DisplayObject}
     */
    function addGameObjectToCurrent(e, args:any[]) {
        // Center the element in the canvas
        var tr = EntityManager.getComponent(args[0], "Transform");
        if (tr != null && tr.go != null) {
            var position = args[1];
            if (position == null) {
                _currentScene.addChild(tr.go);
            } else {
                _currentScene.addChildAt(tr.go,position);
            }
        }
    }

    /*
    function Sprite() {
        var newSprite = {
            go: null
        };

        var frame = null;

        Object.defineProperty(newSprite, "frame", {
            configurable: false, // Immutable properties!
            get: function () {
                return frame;
            },
            set: function (val) {
                //if (newSprite.go == null) {
                    newSprite.go = BasicSprite.fromFrame(val);
                //} else {
                //    //newSprite.go.texture.setFrame(val);
                //}
                frame = val;
            }
        });

        //Object.defineProperty(newSprite, "go", {
        //configurable: false, // Immutable properties!
        //get: function() {
        //return newSprite.go;
        //},
        //set: function(val) {
        //newSprite.go = val;
        //}
        //});
        return newSprite;
    }*/

    /*
    function Spine() {
        var newSpine = {
            go: null
        };

        var data = null, mixes = null;

        Object.defineProperty(newSpine, "data", {
            configurable: false, // Immutable properties!
            get: function () {
                return data;
            },
            set: function (val) {
                if (newSpine.go == null) {
                    newSpine.go = new PIXI.Spine("assets/graphics/resources/" + val);
                }
            }
        });

        Object.defineProperty(newSpine, "mixes", {
            configurable: false, // Immutable properties!
            get: function () {
                return mixes;
            },
            set: function (val) {
                mixes = val;
                if (newSpine.go != null) {
                    for (var i = 0; i < mixes.length; i++) {
                        newSpine.go.stateData.setMixByName(val[i].origin, val[i].destiny,
                            val[i].time);
                    }
                }
            }
        });

        return newSpine;
    }*/

    /*
    function Tiling() {
        var newSprite = {
            go: null
        };

        var frame = null, speed = 0, width = 2, height = 2;

        Object.defineProperty(newSprite, "width", {
            configurable: false, // Immutable properties!
            get: function () {
                return width;
            },
            set: function (val) {
                width = val;
                if( newSprite.go!=null ){
                    newSprite.go.width=val;
                    updateGameObject( newSprite.go );
                }
            }
        });

        Object.defineProperty(newSprite, "height", {
            configurable: false, // Immutable properties!
            get: function () {
                return height;
            },
            set: function (val) {
                height = val;
                if( newSprite.go!=null ){
                    newSprite.go.height=val;
                    updateGameObject( newSprite.go );
                }
            }
        });

        Object.defineProperty(newSprite, "frame", {
            configurable: false, // Immutable properties!
            get: function () {
                return frame;
            },
            set: function (val) {
                newSprite.go = new PIXI.TilingSprite(
                    PIXI.Texture.fromImage(URL_PREFIX+val), width, height);
                updateGameObject( newSprite.go );
            }
        });

        Object.defineProperty(newSprite, "speed", {
            configurable: false, // Immutable properties!
            get: function () {
                return speed;
            },
            set: function (val) {
                speed = val
            }
        });

        return newSprite;
    }*/

    function updateGameObject( go ){
        go.pivot.x = Math.floor(go.width/2);
        go.pivot.y = Math.floor(go.height/2);
        go.hitArea = go.getLocalBounds();
    }

    /**
     * Function that adds the game object of an entity Sprite component to the Transform game object
     * of the same entity
     * @method module:hanoijs/Systems/RenderSystem#createSprite
     * @private
     * @param {string} e - event calling the function
     * @param {any[]} args - arguments of the trigger; args[0] {number}
     */
    function createSprite(e, args:any[]) {
        var t = EntityManager.getComponent(args[0], "Transform");
        var s = EntityManager.getComponent(args[0], "Sprite");
        if (t) {
            t.go = s.go;
            if (s && s.go) {
                s.go.mask = null;
            }
        }
    }

    function createSpriteFrame(e, args:any[]) {
        var t = EntityManager.getComponent(args[0], "Transform");
        var s = EntityManager.getComponent(args[0], "SpriteFrame");
        if (t) {
            t.go = s.go;
        }
    }

    /**
     * Function that adds the game object of an entity Tiling component to the Transform game object
     * of the same entity
     * @method module:hanoijs/Systems/RenderSystem#createTiling
     * @private
     * @param {string} e - event calling the function
     * @param {any[]} args - arguments of the trigger; args[0] {number}
     */
    function createTiling(e, args:any[]) {
        var t = EntityManager.getComponent(args[0], "Transform");
        var s = EntityManager.getComponent(args[0], "Tiling");
        if (t) {
            t.go = s.go;
        }
    }

    /**
     * Function that adds the game object of an entity Spine component to the Transform game object
     * of the same entity
     * @method module:hanoijs/Systems/RenderSystem#createSpine
     * @private
     * @param {string} e - event calling the function
     * @param {any[]} args - arguments of the trigger; args[0] {number}
     */
    function createSpine(e, args:any[]) {
        var t = EntityManager.getComponent(args[0], "Transform");
        var s = EntityManager.getComponent(args[0], "Spine");
        if (t) {
            t.go = s.go;
        }
    }

    /**
     *
     * @param e
     * @param args
     */
    function createDialog(e, args:any[]) {
        var t = EntityManager.getComponent(args[0], "Transform");
        var d = EntityManager.getComponent(args[0], "Dialog");
        if (t) {
            d.constructDialog();
            t.go = d.go;
        }
    }

    /*function GetTransform() {
        return new Transform();
    }*/

    function createTransform(e, args:any[]) {
        var ent = args[0];
        var tr  = args[1];
        if( tr!=null ){
            if( tr.show!=null ){
                EntityManager.attach(ent, tr.show, function (e2, args2) {
                    tr.visible = true;
                });
            }
            if( tr.hide!=null ){
                EntityManager.attach(ent, tr.hide, function (e2, args2) {
                    tr.visible = false;
                });
            }
        }
    }
    function removeTransform(e, args:any[]) {
        var ent = args[0];
        var tr  = args[1];
        if( tr!=null ){
            if( tr.show!=null ){
                EntityManager.detach(ent, tr.show);
            }
            if( tr.hide!=null ){
                EntityManager.detach(ent, tr.hide);
            }
        }
    }

    EventModule.attach("create.Sprite", createSprite);
    EventModule.attach("create.SpriteFrame", createSpriteFrame);
    EventModule.attach("create.Tiling", createTiling);
    EventModule.attach("create.Spine", createSpine);
    EventModule.attach("create.Dialog", createDialog);

    // EventModule.attach("create.Anchor", createAnchor);
    EventModule.attach("create.Transform", createTransform);

    EventModule.attach("remove.Transform", removeTransform);

    EventModule.attach("game.render.add", addScene);
    EventModule.attach("game.render.remove", removeScene);
    // EventModule.attach("game.render.object.add", addGameObjectToCurrent);

    EntityManager.defineComponent("Transform", Transform.factory, 10 );
    EntityManager.defineComponent("Sprite", Sprite.factory, 10 );
    EntityManager.defineComponent("SpriteFrame", SpriteFrame.factory, 10 );
    EntityManager.defineComponent("Spine",  Spine.factory, 10 );
    EntityManager.defineComponent("Tiling", Tiling.factory, 4 );
    EntityManager.defineComponent("Dialog", Dialog.factory, 4 );
    EntityManager.defineComponent("Anchor", function(){
        return { id:null };
    },10);


}

export = RenderSystem;