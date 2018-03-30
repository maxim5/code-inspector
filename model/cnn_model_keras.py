#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Inspired by LeCun paper (2016): "Text Understanding from Scratch"

__author__ = 'maxim'

from keras import *
from keras.layers import *
from keras.utils import to_categorical

from input import DataProvider, Mode, Files

########################################################################################################################
# Data
########################################################################################################################

print('Building vocabulary...')
provider = DataProvider('../data', mode=Mode.BY_CHAR)
provider.build(min_vocab_count=100)
vocab_size = provider.vocab_size
classes = provider.classes
print('Vocab size=%d classes=%d' % (vocab_size, classes))

########################################################################################################################
# Model
########################################################################################################################

def build_model(params):
  sequence_length = params['sequence_length']
  dropout_rates = params['dropout_rates']

  input = Input(shape=(sequence_length, vocab_size))
  convs = []
  for conv in params['conv']:
    conv1d = Conv1D(filters=conv.get('filters_num'),
                    kernel_size=conv.get('filter_size'),
                    padding='valid',
                    activation='relu',
                    strides=1)(input)
    pool1d = MaxPooling1D(pool_size=conv.get('pooling_size'))(conv1d)
    flatten = Flatten()(pool1d)
    convs.append(flatten)
  outputs = Concatenate()(convs)
  graph = Model(inputs=input, outputs=outputs)

  model = Sequential()
  model.add(Dropout(rate=dropout_rates[0], input_shape=(sequence_length, vocab_size)))
  model.add(graph)
  model.add(Dense(units=params['hidden_size']))
  model.add(Dropout(rate=dropout_rates[1]))
  model.add(Dense(units=classes))
  model.add(Activation('softmax'))
  model.compile(loss='sparse_categorical_crossentropy', optimizer='adadelta', metrics=['accuracy'])

  return model

########################################################################################################################
# Training
########################################################################################################################

def main():
  while True:
    # Hyper-parameters
    model_params = dict(
      sequence_length=2048,
      dropout_rates=(0.5, 0.75),
      conv=[
        dict(filters_num=64,  filter_size=3, pooling_size=2),
        dict(filters_num=96,  filter_size=5, pooling_size=5),
        dict(filters_num=128, filter_size=9, pooling_size=9),
        dict(filters_num=160, filter_size=9, pooling_size=9),
      ],
      hidden_size=128
    )
    batch_size = 200
    epochs = 200
    sequence_length = model_params['sequence_length']
    print('\nStart training: params=', model_params)

    K.clear_session()
    model = build_model(model_params)

    def gen(files, steps):
      while True:
        for i, batch in enumerate(provider.stream_snippets(batch_size=batch_size,
                                                           files=files,
                                                           max_tokens=sequence_length,
                                                           parallel_streams=classes)):
          if i == steps:
            break
          batch_x, batch_y, batch_len = batch
          batch_x_one_hot = to_categorical(batch_x, vocab_size)
          yield batch_x_one_hot, batch_y
        if i < steps:
          print(files, 'Not enough steps for one epoch: ', i)
          pass

    model.fit_generator(generator=gen(Files.TRAIN, 500), epochs=epochs, steps_per_epoch=500,
                        validation_data=gen(Files.VAL, 80), validation_steps=80,
                        verbose=1)

    test_result = model.evaluate_generator(generator=gen(Files.TEST, 100), steps=100)
    print('Test results: loss=%.5f accuracy=%.3f' % tuple(test_result))

if __name__ == '__main__':
  main()
