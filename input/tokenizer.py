#!/usr/bin/env python
# -*- coding: utf-8 -*-
__author__ = 'maxim'


import string


def tokenize_by_char(text):
  return [ch for ch in text]


def tokenize_by_lexems(text):
  SPACE = 0
  PUNCTUATION = 1
  IDENTIFIER = 2

  punctuation_chars = set(string.punctuation) - set('_')
  identifier_chars = set(string.ascii_letters + string.digits + '_')

  state = SPACE
  prev = -1
  for i, ch in enumerate(text):
    is_punctuation = ch in punctuation_chars
    is_identifier = ch in identifier_chars
    is_space = ch == ' ' or ch == '\t' or ch == '\n'

    if state == SPACE:
      if prev >= 0:
        yield text[prev:i]
        prev = i

    if state == PUNCTUATION:
      if is_identifier or is_space:
        yield text[prev:i]
        prev = i

    if state == IDENTIFIER:
      if is_punctuation or is_space:
        yield text[prev:i]
        prev = i

    if is_space:
      state = SPACE
    elif is_punctuation:
      state = PUNCTUATION
    elif is_identifier:
      state = IDENTIFIER

  if prev < len(text):
    yield text[prev:]


def tokenize(text, mode=True):
  if mode:
    return tokenize_by_lexems(text)
  else:
    return tokenize_by_char(text)


if __name__ == '__main__':
  text = 'foo bar baz foo2  {x?_}!943 322 !# ^\t^& while (a<b): 0x00\n  int a[0:]'

  print(tokenize_by_char(text))
  print()
  print(list(tokenize_by_lexems(text)))
