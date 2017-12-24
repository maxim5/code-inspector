#!/usr/bin/env python
# -*- coding: utf-8 -*-
__author__ = 'maxim'


import os

def list_all_data_files(root_dir):
  return [
    (os.path.join(root_dir, lang, file_name), lang)
    for lang in (os.listdir(root_dir))
    for file_name in os.listdir(os.path.join(root_dir, lang))
  ]
