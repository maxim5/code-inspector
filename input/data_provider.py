#!/usr/bin/env python
# -*- coding: utf-8 -*-
__author__ = 'maxim'


from collections import deque
import enum
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


def get_all_files(data_dir):
  return {
    lang: [os.path.join(data_dir, lang, file_name)
           for file_name in os.listdir(os.path.join(data_dir, lang))]
    for lang in (os.listdir(data_dir))
  }


class Files(enum.Enum):
  TRAIN = 0
  VAL = 1
  TEST = 2


class DataProvider(object):
  def __init__(self, data_dir, mode=tokenizer.Mode.BY_LEXEM):
    self._data_dir = data_dir

    all_files = get_all_files(data_dir)
    self._train_files, self._val_files, self._test_files = [], [], []
    for lang, files in all_files.items():
      n = len(files)
      val_idx, test_idx = int(n * 0.7), int(n * 0.8)
      self._train_files += [(path, lang) for path in files[:val_idx]]
      self._val_files += [(path, lang) for path in files[val_idx:test_idx]]
      self._test_files += [(path, lang) for path in files[test_idx:]]
    self._vocab_files = self._train_files + self._val_files

    self._mode = mode
    self._labels = None
    self._vocab = None


  def build(self, min_vocab_count=0):
    self._labels = self._build_labels_vocab()
    self._vocab = self._build_main_vocab(min_vocab_count)


  @property
  def vocab(self):
    return self._vocab


  @property
  def labels(self):
    return self._labels


  @property
  def vocab_size(self):
    return len(self._vocab.token_to_idx)


  @property
  def classes(self):
    return len(self._labels.token_to_idx)


  def get_files(self, files):
    if files == Files.TRAIN:
      return self._train_files
    if files == Files.VAL:
      return self._val_files
    if files == Files.TEST:
      return self._test_files


  def _build_main_vocab(self, min_vocab_count):
    def token_stream():
      for path, lang in self._vocab_files:
        with open(path) as file_:
          content = file_.read()
        for token in tokenizer.tokenize(content, self._mode):
          yield token
        # print('File %s done' % path)

    print('Building vocabulary')
    return vocab.build_vocab(token_stream(), min_vocab_count)


  def _build_labels_vocab(self):
    langs = set(lang for path, lang in self._vocab_files)
    return vocab.build_vocab(langs, min_count=0)


  def stream_data(self, batch_size, files=Files.TRAIN,
                  snippet_coverage=1.0, snippet_min_lines=3, snippet_max_lines=20,
                  parallel_streams=10, max_tokens=2000):
    assert self._labels is not None and self._vocab is not None, \
      'Call `build()` method must be called first'

    files_queue = deque(self.get_files(files))
    np.random.shuffle(files_queue)

    streamers = deque()
    while files_queue:
      while len(streamers) < parallel_streams and files_queue:
        path, lang = files_queue.popleft()
        streamer = snippets.generate_snippets(path,
                                              coverage=snippet_coverage,
                                              min_lines=snippet_min_lines,
                                              max_lines=snippet_max_lines)
        streamers.append((streamer, path, lang))

      # Generate the next batch
      batch_x = np.zeros([batch_size, max_tokens], dtype=np.int32)
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
          x, len_ = vocab.encode(snippet, self._vocab, max_tokens)
          batch_x[i, :] = x
          batch_y[i] = self._labels.token_to_idx[lang]
          batch_len[i] = len_
          i += 1
      yield batch_x, batch_y, batch_len


if __name__ == '__main__':
  provider = DataProvider('../data')
  provider.build(min_vocab_count=20)
  for batch_x, batch_y, batch_len in provider.stream_data(batch_size=2, max_tokens=10):
    print(batch_x)
    print(batch_y)
    print(batch_len)
    print('-----')
