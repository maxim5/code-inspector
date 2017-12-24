#!/usr/bin/env python
# -*- coding: utf-8 -*-
__author__ = 'maxim'


import data_provider as data
import tokenizer
import vocab


def build_vocab(data_dir, mode=tokenizer.Mode.BY_LEXEM, min_count=0):
  def token_stream():
    for path, lang in data.list_all_data_files(data_dir):
      with open(path) as file_:
        content = file_.read()
      for token in tokenizer.tokenize(content, mode):
        yield token
      print('File %s done' % path)

  print('Building vocabulary')
  return vocab.build_vocab(token_stream(), min_count)


if __name__ == '__main__':
  token_to_idx, idx_to_token = build_vocab('../data', min_count=20)
  print(idx_to_token)
