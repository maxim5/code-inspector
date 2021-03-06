#!/usr/bin/env python
# -*- coding: utf-8 -*-

__author__ = 'maxim'

import unittest
from input.tokenizer import tokenize_by_char, tokenize_by_lexems


class TokenizerTest(unittest.TestCase):
  def test_one_term(self):
    text = 'foo'
    self.assertEqual(['f', 'o', 'o'], tokenize_by_char(text))
    self.assertEqual(['foo'], list(tokenize_by_lexems(text)))

  def test_two_terms(self):
    text = 'foo bar'
    self.assertEqual(['f', 'o', 'o', ' ', 'b', 'a', 'r'], tokenize_by_char(text))
    self.assertEqual(['foo', ' ', 'bar'], list(tokenize_by_lexems(text)))

  def test_spaces_1(self):
    text = '   '
    self.assertEqual([' ', ' ', ' '], tokenize_by_char(text))
    self.assertEqual([' ', ' ', ' '], list(tokenize_by_lexems(text)))

  def test_spaces_2(self):
    text = '  foo'
    self.assertEqual([' ', ' ', 'f', 'o', 'o'], tokenize_by_char(text))
    self.assertEqual([' ', ' ', 'foo'], list(tokenize_by_lexems(text)))

  def test_chars_1(self):
    text = 'a->b'
    self.assertEqual(['a', '-', '>', 'b'], tokenize_by_char(text))
    self.assertEqual(['a', '->', 'b'], list(tokenize_by_lexems(text)))

  def test_chars_2(self):
    text = '(n):'
    self.assertEqual(['(', 'n', ')', ':'], tokenize_by_char(text))
    self.assertEqual(['(', 'n', '):'], list(tokenize_by_lexems(text)))

  def test_chars_3(self):
    text = '#include <math.h>'
    self.assertEqual(['#', 'include', ' ', '<', 'math', '.', 'h', '>'], list(tokenize_by_lexems(text)))

  def test_identifier_1(self):
    text = 'foo_bar'
    self.assertEqual(['foo_bar'], list(tokenize_by_lexems(text)))

  def test_identifier_2(self):
    text = 'fooBar'
    self.assertEqual(['fooBar'], list(tokenize_by_lexems(text)))

  def test_identifier_3(self):
    text = 'FooBar'
    self.assertEqual(['FooBar'], list(tokenize_by_lexems(text)))

  def test_identifier_4(self):
    text = 'fooBar_FOOBar_foobar012'
    self.assertEqual(['fooBar_FOOBar_foobar012'], list(tokenize_by_lexems(text)))

  def test_long(self):
    text = 'foo bar baz foo2  {x?_}!943 322 !# ^\t^& while (a<b): 0x00\n  int a[0:]'
    self.assertEqual(['foo', ' ', 'bar', ' ', 'baz', ' ', 'foo2', ' ', ' ', '{', 'x', '?', '_', '}!',
                      '943', ' ', '322', ' ', '!#', ' ', '^', '\t', '^&', ' ', 'while', ' ',
                      '(', 'a', '<', 'b', '):', ' ', '0x00', '\n', ' ', ' ', 'int', ' ', 'a', '[', '0', ':]'],
                      list(tokenize_by_lexems(text)))
