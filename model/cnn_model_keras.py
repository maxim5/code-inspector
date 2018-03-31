#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Inspired by LeCun paper (2016): "Text Understanding from Scratch"

__author__ = 'maxim'

from keras import *
from keras.layers import *
from keras.utils import to_categorical

from input import DataProvider, Mode, Files, util

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
    def random_conv():
      return dict(filters_num=int(np.random.choice([128, 160, 256, 320])),
                  filter_size=int(np.random.choice([2, 3, 5])),
                  pooling_size=int(np.random.choice([2, 3, 5])))
    model_params = dict(
      sequence_length=np.random.choice([1024, 1600, 2048]),
      conv=[random_conv() for _ in range(np.random.randint(2, 6))],
      dropout_rates=(np.random.uniform(0.2, 0.6), np.random.uniform(0.5, 0.8)),
      hidden_size=np.random.choice([96, 128]),
    )
    batch_size = np.random.choice([64, 128, 256])
    epochs = 40
    sequence_length = model_params['sequence_length']
    print()
    print('Start training: batch_size=%d' % batch_size)
    print('Params:', model_params)

    def generator(files, steps):
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
          # print(files, 'Not enough steps for one epoch: ', i)
          pass

    try:
      K.clear_session()
      model = build_model(model_params)

      k = 256 // batch_size
      train_result = model.fit_generator(generator=generator(Files.TRAIN, 500*k), epochs=epochs, steps_per_epoch=500*k,
                                         validation_data=generator(Files.VAL, 80*k), validation_steps=80*k,
                                         verbose=0)
      with util.print_options(precision=3, suppress=True):
        print()
        print('*** Train results ***')
        print('loss:', np.array(train_result.history['loss']))
        print('accuracy:', np.array(train_result.history['acc']))

        print()
        print('*** Val results ***')
        print('val-loss:', np.array(train_result.history['val_loss']))
        print('val-accuracy:', np.array(train_result.history['val_acc']))

      test_result = model.evaluate_generator(generator=generator(Files.TEST, 160*k), steps=160*k)
      print()
      print('*** Test results ***')
      print('loss=%.3f accuracy=%.3f' % tuple(test_result))
    except BaseException as e:
      print('Model failed', e)

if __name__ == '__main__':
  main()
