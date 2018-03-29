

'''
data-1.txt is the full test data, as supplied
data-2.txt is approximately half that
data-3.txt is data-1 with errors stripped
data-4.txt is data-1 with CamelCase markers
data-5.txt is data-2 with CamelCase markers
data-6.txt is data-3 with CamelCase markers
'''

from re import compile as compile_
from os.path import dirname, join, exists, isfile

import lepl._performance.dynamic as dynamic


def get_path(postfix, ignore_bad=False):
    path = join(dirname(dynamic.__file__), 'data-' + str(postfix) + '.txt')
    assert ignore_bad or (exists(path) and isfile(path))
    return path


def get_file(postfix, mode='r'):
    return open(get_path(postfix, mode=='w'), mode)


def get_data(postfix):
    with get_file(postfix) as input:
        return input.read()


def make_3():
    text = get_data(1)
    fix = compile_(r'(?m)^(\s*)(?::(?:[A-Z][a-z]*)+)+?(:(?:[A-Z][a-z]*)+.*)$')
    text = fix.sub(r'\1\2', text)
    with get_file(3, 'w') as out:
        out.write(text)


if __name__ == '__main__':
    make_3()
    