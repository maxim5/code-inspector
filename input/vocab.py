#!/usr/bin/env python
# -*- coding: utf-8 -*-
__author__ = 'maxim'


from collections import Counter

def build_vocab(tokens_stream, min_count=0):
  token_to_idx = {}
  idx_to_token = {}
  counter = Counter()
  idx = 0
  for token in tokens_stream:
    if token in token_to_idx:
      continue
    counter[token] += 1
    if counter[token] > min_count:
      token_to_idx[token] = idx
      idx_to_token[idx] = token
      idx += 1
  return token_to_idx, idx_to_token
