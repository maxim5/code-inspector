"""
data.py
"""

import math

ST_STILL = 0
ST_WALKING = 1 # this is a random walk.
ST_ATTRACTED = 2 # walking to an attractor point
ST_ATTACK = 3
ST_DYING = 4
ST_DEAD = 5 # awaiting cleanip
ST_STILL = 6 # doing nothing.
ST_JUMPING = 7 # jump up;

TWO_PI = 2 * math.pi
