#!/usr/bin/env python
# -*- coding: utf-8 -*-
__author__ = 'maxim'


from collections import deque
import numpy as np
import os

if __name__ == '__main__':
  import snippets
  import tokenizer
  import vocab
else:
  from . import snippets
  from . import tokenizer
  from . import vocab


def list_all_data_files(data_dir):
  return [
    (os.path.join(data_dir, lang, file_name), lang)
    for lang in (os.listdir(data_dir))
    for file_name in os.listdir(os.path.join(data_dir, lang))
  ]


class DataProvider(object):
  def __init__(self, data_dir, mode=tokenizer.Mode.BY_LEXEM):
    self._data_dir = data_dir
    self._all_files = list_all_data_files(data_dir)
    # TODO: split to test and train
    self._mode = mode
    self._labels = None
    self._vocab = None


  def build(self, min_vocab_count=0):
    self._labels = self._build_labels_vocab()
    self._vocab = self._build_main_vocab(min_vocab_count)


  @property
  def vocab_size(self):
    return len(self._vocab.token_to_idx)


  @property
  def classes(self):
    return len(self._labels.token_to_idx)


  def _build_main_vocab(self, min_vocab_count):
    def token_stream():
      for path, lang in self._all_files:
        with open(path) as file_:
          content = file_.read()
        for token in tokenizer.tokenize(content, self._mode):
          yield token
        print('File %s done' % path)

    print('Building vocabulary')
    return vocab.build_vocab(token_stream(), min_vocab_count)


  def _build_labels_vocab(self):
    langs = set(lang for path, lang in self._all_files)
    return vocab.build_vocab(langs, min_count=0)


  def stream_data(self, batch_size, snippet_coverage=1.0, parallel_streams=5, max_size=1000):
    assert self._labels is not None and self._vocab is not None, 'Call `build()` method must be called first'

    files_queue = deque(self._all_files)
    np.random.shuffle(files_queue)

    streamers = deque()
    while files_queue:
      while len(streamers) < parallel_streams and files_queue:
        path, lang = files_queue.popleft()
        streamer = snippets.generate_snippets(path, coverage=snippet_coverage)
        streamers.append((streamer, path, lang))

      # Generate the next batch
      batch_x = np.zeros([batch_size, max_size], dtype=np.int32)
      batch_y = np.zeros([batch_size], dtype=np.int32)
      batch_len = np.zeros([batch_size], dtype=np.int32)
      i = 0
      while streamers and i < batch_size:
        streamer_idx = np.random.randint(len(streamers))
        streamer, path, lang = streamers[streamer_idx]
        snippet = next(streamer, None)

        if snippet is None:
          del streamers[streamer_idx]
        else:
          x, len_ = vocab.encode(snippet, self._vocab, max_size)
          batch_x[i, :] = x
          batch_y[i] = self._labels.token_to_idx[lang]
          batch_len[i] = len_
          i += 1
      yield batch_x, batch_y, batch_len


if __name__ == '__main__':
  provider = DataProvider('../data')
  provider.build(min_vocab_count=20)
  for batch_x, batch_y, batch_len in provider.stream_data(batch_size=2, max_size=10):
    print(batch_x)
    print(batch_y)
    print(batch_len)
    print('-----')
