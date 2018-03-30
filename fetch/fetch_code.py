#!/usr/bin/env python
# -*- coding: utf-8 -*-
__author__ = 'maxim'

from io import BytesIO
import gzip
import json
import os
import urllib.error
import urllib.parse
import urllib.request

def get_json(url):
  print('GET', url)
  request = urllib.request.Request(url)
  request.add_header('Accept-Encoding', 'gzip')
  response = urllib.request.urlopen(request)
  if response.info().get('Content-Encoding') == 'gzip':
    buf = BytesIO(response.read())
    f = gzip.GzipFile(fileobj=buf)
    data = f.read()
  else:
    data = response.read()
  return json.loads(data)

def fetch(terms, lang):
  if isinstance(terms, list) or isinstance(terms, tuple):
    terms = ' '.join(terms)
  q = '%s lang:%s' % (terms, lang)
  url = 'https://searchcode.com/api/codesearch_I/?q=%s&p=0&per_page=100&lan=%s' % (urllib.parse.quote_plus(q),
                                                                                   urllib.parse.quote_plus(lang))
  contents = get_json(url)
  results = contents.get('results', [])
  urls = {item.get('md5hash', ''):
            (item.get('url', '').replace('/view/', '/raw/'),
             item.get('language', ''),
             item.get('filename', ''))
          for item in results}
  print('Got %s urls' % len(urls))

  for key, (url, language, filename) in urls.items():
    directory = '_fetched/%s' % language
    os.makedirs(directory, exist_ok=True)
    try:
      print('GET', url)
      urllib.request.urlretrieve(url, filename='%s/%s_%s' % (directory, key, filename))
    except urllib.error.HTTPError:
      print('FAIL')


terms = ['stream', 'data', 'manage', 'amazing', 'algorithm', 'interface', 'science', 'implementation']
languages = ['c', 'c++', 'c#', 'clojure', 'css', 'js', 'python', 'java',
             'coffeescript', 'f#', 'go', 'javascript', 'lua', 'matlab', 'pascal', 'perl',
             'php', 'r', 'ruby', 'scala', 'zsh', 'swift', 'typescript',
             'html', 'xhtml', 'Erlang', 'BourneShell', 'SQL', 'SASS', 'LESS', 'VisualBasic', 'ObjectiveC']

def main():
  for lang in languages:
    for term in terms:
      fetch(term, lang)


if __name__ == "__main__":
  main()
