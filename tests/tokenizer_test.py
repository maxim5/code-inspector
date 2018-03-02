#!/usr/bin/env python
# -*- coding: utf-8 -*-

__author__ = 'maxim'

import unittest
from input.tokenizer import tokenize_by_char, tokenize_by_lexems


class TokenizerTest(unittest.TestCase):
  def test_simple(self):
    text = 'foo bar'
    self.assertEqual(['f', 'o', 'o', ' ', 'b', 'a', 'r'], tokenize_by_char(text))
    self.assertEqual(['foo', ' ', 'bar'], list(tokenize_by_lexems(text)))

  def test_long(self):
    text = 'foo bar baz foo2  {x?_}!943 322 !# ^\t^& while (a<b): 0x00\n  int a[0:]'
    self.assertEqual(['', ' ', 'bar', ' ', 'baz', ' ', 'foo2', ' ', ' ', '{', 'x', '?', '_', '}!',
                      '943', ' ', '322', ' ', '!#', ' ', '^', '\t', '^&', ' ', 'while', ' ',
                      '(', 'a', '<', 'b', '):', ' ', '0x00', '\n', ' ', ' ', 'int', ' ', 'a', '[', '0', ':]'],
                      list(tokenize_by_lexems(text)))
