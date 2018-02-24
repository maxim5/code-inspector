#!/usr/bin/env python
# -*- coding: utf-8 -*-
__author__ = 'maxim'

import numpy as np
import contextlib

@contextlib.contextmanager
def print_options(*args, **kwargs):
  original = np.get_printoptions()
  np.set_printoptions(*args, **kwargs)
  try:
    yield
  finally:
    np.set_printoptions(**original)
