#!/usr/bin/env python
# -*- coding: utf-8 -*-
__author__ = 'maxim'

import itertools
import numpy as np


def generate_snippets(path, preferred_length=None, coverage=1.0, min_lines=1, max_lines=100):
  with open(path, errors='ignore') as file_:
    lines = file_.readlines()

  current_coverage = 0.0
  total_lines = float(len(lines))
  while current_coverage < coverage:
    length = preferred_length
    if length is None:
      length = np.random.randint(min_lines, max_lines + 1)
    current_coverage += length / total_lines

    yield random_snippet(lines, length)


def random_window(lines, length):
  total_lines = len(lines)
  if total_lines <= length:
    return lines

  start = np.random.randint(total_lines - length)
  end = start + length
  return lines[start:end]


def indent(line):
  space = ' '
  if line.startswith('\t'):
    space = '\t'
  return sum(1 for _ in itertools.takewhile(lambda ch: ch == space, line))


def random_snippet(lines, length):
  for i in range(10):
    snippet = random_window(lines, length)
    if not is_low_quality(snippet):
      break
  min_indent = min([indent(line) for line in snippet])
  unindented = [line[min_indent:] for line in snippet]
  return ''.join(unindented)


def is_low_quality(lines):
  if len(lines) > 10:
    return False

  non_empty_lines = [line for line in lines if len(line) > 0]

  # check if most lines are empty
  empty_num = len(lines) - len(non_empty_lines)
  if 2 * empty_num >= len(lines):
    return True

  # check if the lines are mostly empty
  non_space_chars = sum(sum(not c.isspace() for c in line) for line in non_empty_lines)
  if non_space_chars < 10 * len(lines):
    return True

  # check if all lines are comments
  prefix = common_prefix(non_empty_lines)
  prefix = prefix.strip()
  for s in ['# ', '//', '*', '**', '/*']:
    if prefix.startswith(s):
      return True

  return False


def common_prefix(strings):
  def all_same(x):
    return all(x[0] == y for y in x)

  char_tuples = zip(*strings)
  prefix_tuples = itertools.takewhile(all_same, char_tuples)
  return ''.join(x[0] for x in prefix_tuples)


if __name__ == '__main__':
  txt = """"""
  lines = txt.splitlines(False)
  print(lines)
  print(common_prefix(lines))
  print(is_low_quality(lines))
  print()

  import os
  langs = os.listdir('../data')
  for lang in langs:
    files = os.listdir('../data/%s' % lang)
    file_name = np.random.choice(files)
    file_name = '../data/%s/%s' % (lang, file_name)

    print('~~~~~~~~~~~~ %s ~~~~~~~~~~~~' % lang)
    for snippet in generate_snippets(file_name, coverage=0.1, min_lines=3, max_lines=6):
      print(snippet)
      print('----------------------------------------------------')
    print()
    print()
