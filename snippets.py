#!/usr/bin/env python
# -*- coding: utf-8 -*-
__author__ = 'maxim'

import itertools
import numpy as np


MAX_SNIPPET_SIZE = 20
MIN_SNIPPET_SIZE = 2


def generate_snippets(file_name, preferred_length=None, coverage=1.0):
  with open(file_name) as file_:
    lines = file_.readlines()

  current_coverage = 0.0
  total_lines = float(len(lines))
  while current_coverage < coverage:
    length = preferred_length
    if length is None:
      length = np.random.randint(MIN_SNIPPET_SIZE, MAX_SNIPPET_SIZE + 1)
    current_coverage += length / total_lines

    yield random_snippet(lines, length)


def random_window(lines, length):
  total_lines = len(lines)
  if total_lines < length:
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
  snippet = random_window(lines, length)
  min_indent = min([indent(line) for line in snippet])
  unindented = [line[min_indent:] for line in snippet]
  return ''.join(unindented)


if __name__ == '__main__':
  file_name = 'data/java/Arrays.java'
  file_name = 'data/c/hexagon_controller.c'
  for snippet in generate_snippets(file_name, coverage=2.0):
    print(snippet)
    print('----------------------------------------------------')
