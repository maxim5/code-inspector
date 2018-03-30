/**
 * @overview System that initializes and manages the different types of animations (Tween, Keyframe, Spine).
 *
 * It listens to events:
 * * "create.Animation"   -> arguments: [entityNumber]
 * * "remove.Animation"   -> arguments: [entityNumber, entityAnimationComponent]
 * * "game.render.update" -> arguments: []
 *
 * Dependant components Transform, Tiling, Animation, Sprite and Spine.
 *
 * @module hanoijs/Systems/AnimationSystem
 *
 * @example <caption>**Animation** component definition details</caption>
 * {
 *      stats: { //Contains the different animations that can be played
 *          fly: {                              //Animation name
 *              start: "player.fly",            //Trigger that starts the animation
 *              end: "player.stop",             //Trigger that stops the animation
 *              triggerOnStart: "notify.start", //Trigger launched when the animation starts
 *              triggerOnEnd: "notify.end",     //Trigger launched when the animation ends
 *              tween: {                        //The animation type (Tween)
 *                  component: "Transform",     //Name of the component to animate
 *                  values: { y: -500 },        //Values to animate
 *                  time: 10,                   //Duration of the animation in sec
 *                  easing: "Linear"            //Type of easing of the animation
 *              }
 *          },
 *          walk: {
 *              start: "player.walk",
 *              end: "player.inflate",
 *              triggerOnStart: "notify.start",
 *              triggerOnEnd: "notify.end",
 *              keyframe: {                     //The animation type (Keyframe)
 *                  fps: 30,                    //Speed of the animation in fps
 *                  start: 0,                   //Number of the first frame of the animation
 *                  end: 5,                     //Number of the last frame of the animation
 *                  loop: true,                 //If the animation must loop or not
 *                  random: false               //Animation loop starts at random frame
 *              }
 *          },
 *          jump: {
 *              start: "player.walk",
 *              spine: [                        //Array of animations to do (in order)
 *                  {
 *                      name: "jump",           //Name of the spine animation
 *                      loop: false,            //If the spine animation has to loop
 *                      delay: 0                //Starting delay of the animation (not valid
 *                                              //for the first animation)
 *                  },
 *                  { name: "stand", loop: true, delay: 0 }
 *              ]
 *          }
 *      }
 * }
 *
 *
 * @property {Object} Easings - Dictionary to translate string code to TWEEN.Easing
 */

///<reference path="../../../../lib/tween.d.ts" />
///<reference path="../../../../lib/PIXI.d.ts" />

import EventModule   = require("hanoijs/Events2/EventModule");

import EntityManager = require("hanoijs/EntitySystem/EntityManager");

import TimeSystem    = require("hanoijs/Systems/TimeSystem");


module AnimationSystem {

    var Easings = {
        "Linear": TWEEN.Easing.Linear.None,
        "Quadratic.InOut": TWEEN.Easing.Quadratic.InOut,
        "Quadratic.In": TWEEN.Easing.Quadratic.In,
        "Quadratic.Out": TWEEN.Easing.Quadratic.Out
    };

    var _paused = false;

    var _pausedFPS;

    /**
     * Initialization function of the animation system.
     * @method module:hanoijs/Systems/AnimationSystem#init
     * @param {any} options - Initialization options
     */
    export function init(options:any) {
        _pausedFPS = {};
    }

    /**
     * Update function of the animation system. It updates the tween animations and
     * the tiled elements
     * @method module:hanoijs/Systems/AnimationSystem#update
     * @private
     * @param {string} e - event calling the function
     * @param {any[]} args - arguments of the trigger (UNUSED)
     */
    function update(e, args:any[]) {
        try {
            if (!_paused) {
                TWEEN.update();
            }
        } catch (e) {
        }
        // updateTiling();
    }

    function pauseAnimation(evt:string, args:any[]) {
        _paused = true;
        var sprites = EntityManager.findComponent("SpriteFrame");
        for (var idx in sprites) {
            _pausedFPS[idx] = sprites[idx].go.animationSpeed;
            sprites[idx].go.animationSpeed = 0;
        }
    }

    function resumeAnimation(evt:string, args:any[]) {
        _paused = false;
        var sprites = EntityManager.findComponent("SpriteFrame");
        for (var idx in sprites) {
            if (sprites[idx] && sprites[idx].go) {
                sprites[idx].go.animationSpeed = _pausedFPS[idx];
            }
        }
    }

    /**
     * Function to update the tile position of the elements with Tiling component
     * @private
     * @method module:hanoijs/Systems/AnimationSystem#updateTiling
     */
    function updateTiling() {
        var entities = EntityManager.findEntity("Tiling");
        var t, s;
        for (var idx in entities) {
            t = EntityManager.getComponent(entities[idx], "Transform");
            s = EntityManager.getComponent(entities[idx], "Tiling");
            if (s.go) {
                s.go.tilePosition.x += s.speed * TimeSystem.deltaTime;
            }
        }
    }

    /**
     * Function to create and configure tween, keyframe and spine animations
     * when the component Animation is created.
     * @method module:hanoijs/Systems/AnimationSystem#configureAnimation
     * @private
     * @param {string} e - event calling the function
     * @param {any[]} args - arguments of the trigger; args[0]: entityNumber
     */
    function configureAnimation(e, args:any[]) {
        var c = EntityManager.getComponent(args[0], "Animation");
        for (var statId in c.stats) {
            var stat = c.stats[statId];
            if (stat.start != null) {
                if (stat.tween != null) {
                    configureTween(args[0], stat);
                } else if (stat.keyframe) {
                    configureKeyframe(args[0], stat);
                } else if (stat.spine) {
                    configureSpine(args[0], stat)
                }
            }
        }
    }

    /**
     * Function to configure a tween animation for a game object
     * @method module:hanoijs/Systems/AnimationSystem#configureTween
     * @private
     * @param {number} entity - number of the entity
     * @param {any} stat - data of the tween animation to create
     */
    function configureTween(entity:number, stat:any) {
        var remainingTime =stat.tween.time * 1000;

        var tween = new TWEEN.Tween(EntityManager.getComponent(entity, stat.tween.component));
        tween.to(stat.tween.values, remainingTime );
        tween.easing(Easings[stat.tween.easing]);
        // tween.delay(0);
        if (stat.tween.repeat) {
            tween.repeat(stat.tween.repeat);
        }
        if (stat.tween.yoyo) {
            tween.yoyo(true);
        } else {
            tween.yoyo(false);
        }

        stat.entityId = entity;

        tween.onComplete(function () {
            remainingTime=0;
            if (stat.triggerOnEnd != null) {
                EventModule.trigger(stat.triggerOnEnd, [stat.entityId]);
            }
        });


        stat._cb_start = stat._cb_start || function (e, args) {
            remainingTime = stat.tween.time * 1000;
            // console.log( "ANIM START: " + remainingTime, stat );
            tween.to(stat.tween.values, remainingTime );
            tween.easing(Easings[stat.tween.easing]);
            tween.start();
            if (stat.triggerOnStart != null) {
                EventModule.trigger(stat.triggerOnStart, [stat.entityId]);
            }
        };
        EntityManager.attach(entity, stat.start, stat._cb_start);

        if (stat.end) {
            stat._cb_end = stat._cb_end || function (e, args) {
                remainingTime=0;
                tween.stop();
            };
            EntityManager.attach(entity, stat.end, stat._cb_end);
        }

        if (stat.pause) {
            stat._cb_pause = stat._cb_pause || function (e, args) {
                if( remainingTime>0 ){
                    remainingTime -= tween.getElapsedTime();
                    // console.log( "ANIM PAUSE: " + remainingTime, stat );
                    tween.stop();
                }
            };
            EntityManager.attach(entity, stat.pause, stat._cb_pause);
        }
        if (stat.resume) {
            stat._cb_resume = stat._cb_resume || function (e, args) {
                if( remainingTime>0 ){
                    // console.log( "ANIM RESUME: " + remainingTime, stat );
                    tween.to(stat.tween.values, remainingTime );
                    tween.easing(Easings[stat.tween.easing]);
                    tween.start();
                }
            };
            EntityManager.attach(entity, stat.resume, stat._cb_resume);
        }

        if (stat.autostart) {
            tween.start();
            if (stat.triggerOnStart != null) {
                EventModule.trigger(stat.triggerOnStart, [stat.entityId]);
            }
        }
        stat.tween.element = tween;
    }

    /**
     * Function to configure a keyframe animation for a movie clip
     * @method module:hanoijs/Systems/AnimationSystem#configureKeyframe
     * @private
     * @param {number} entity - number of the entity
     * @param {any} stat - data of the keyframe animation
     */
    function configureKeyframe(entity:number, stat:any) {
        var transform = EntityManager.getComponent(entity, "Transform");
        var sprite = EntityManager.getComponent(entity, "SpriteFrame");

        if (transform.go && sprite.frames) {
            //for (var i = stat.keyframe.start; i <= stat.keyframe.end; i++) {
            /*sprite.go.textures.push(
             PIXI.Texture.fromFrame(sprite.frame + "-" + i)
             );*/
            //}
            if (stat.keyframe.fps) {
                sprite.go.animationSpeed = stat.keyframe.fps;
            }
            if (stat.keyframe.loop) {
                sprite.go.loop = true;
            }
        }

        stat.entityId = entity;

        if (stat.keyframe.random) {
            stat.keyframe._randomValue = ((stat.keyframe.end - stat.keyframe.start) * Math.random()) >> 0;
        } else {
            stat.keyframe._randomValue = null;
        }

        stat._cb_start = stat._cb_start || function (e, args) {
            if (args[0] == stat.entityId) {
                sprite.setFramesToTextures(stat.keyframe.start, stat.keyframe.end);
                if (stat.keyframe.fps) {
                    sprite.go.animationSpeed = stat.keyframe.fps;
                }
                if (stat.keyframe.loop) {
                    sprite.go.loop = true;
                } else {
                    sprite.go.loop = false;
                }
                if (stat.triggerOnEnd) {
                    sprite.go.onComplete = function () {
                        EventModule.trigger(stat.triggerOnEnd, [stat.entityId]);
                    }
                } else {
                    sprite.go.onComplete = null;
                }
                if (stat.keyframe._randomValue != null) {
                    sprite.go.gotoAndPlay(stat.keyframe._randomValue);
                } else {
                    sprite.go.play();
                }
                //sprite.animations.play( statId, stat.keyframe.fps, false, true);
                if (stat.triggerOnStart != null) {
                    EventModule.trigger(stat.triggerOnStart, [stat.entityId]);
                }
            }
        };
        EntityManager.attach(entity, stat.start, stat._cb_start);

        if (stat.end) {
            stat._cb_end = stat._cb_end || function (e, args) {
                //if (args[0] == stat.entityId) {
                sprite.go.gotoAndStop(0);
                //}
            };
            EntityManager.attach(entity, stat.end, stat._cb_end);
        }
    }

    /**
     * Function to configure a spine animation for a spine element
     * @method module:hanoijs/Systems/AnimationSystem#configureSpine
     * @private
     * @param {number} entity - number of the entity
     * @param {any} stat - data of the spine animation
     */
    function configureSpine(entity:number, stat:any) {
        //console.log("[AnimationSystem] (configureSpine)");
        //var transform = EntityManager.getComponent( entity, "Transform");
        var spine = EntityManager.getComponent(entity, "Spine");

        stat._cb_start = stat._cb_start || function (e, args) {
            spine.go.state.setAnimationByName(stat.spine[0].name, stat.spine[0].loop);
            for (var i = 1; i < stat.spine.length; i++) {
                spine.go.state.addAnimationByName(stat.spine[i].name, stat.spine[i].loop,
                    stat.spine[i].delay
                );
            }
        };
        EntityManager.attach(entity, stat.start, stat._cb_start);

        // Maybe is not necessary the end of the animation
        if (stat.end) {
            stat._cb_end = stat._cb_end || function (e, args) {
                spine.go.state.clearAnimation();
            };
            EntityManager.attach(entity, stat.end, stat._cb_end);
        }
    }

    /**
     * Function that detaches the listeners created by the Animation component
     * @method module:hanoijs/Systems/AnimationSystem#removeAnimation
     * @private
     * @param {string} e - event calling the function
     * @param {any[]} args - arguments of the trigger; args[0]: entityNumber,
     *                                                 args[1]: animationComponent
     */
    function removeAnimation(e, args:any[]) {
        var c = args[1];
        for (var statId in c.stats) {
            var stat = c.stats[statId];
            if (stat.tween != null && stat.tween.element != null ) {
                stat.tween.element.stop();
            }
            if (stat.start != null) {
                EntityManager.detach(args[0], stat.start);
            }
            if (stat.end != null) {
                EntityManager.detach(args[0], stat.end);
            }
            if (stat.pause) {
                EntityManager.detach(args[0], stat.pause);
            }
            if (stat.resume) {
                EntityManager.detach(args[0], stat.resume);
            }
        }
    }

    EventModule.attach("create.Animation", configureAnimation);

    EventModule.attach("remove.Animation", removeAnimation);

    EventModule.attach("game.render.update", update);

    EventModule.attach("animations.pause", pauseAnimation);
    EventModule.attach("animations.resume", resumeAnimation);

}

export = AnimationSystem;