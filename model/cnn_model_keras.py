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

# Hyper-parameters
sequence_length = 2048
filter_sizes = (3, 5, 9, 9)
pooling_sizes = (2, 5, 9, 9)
num_filters = (64, 96, 128, 160)
dropout_rates = (0.5, 0.75)
hidden_size = 128

input = Input(shape=(sequence_length, vocab_size))
convs = []
for i in range(0, len(filter_sizes)):
  conv = Conv1D(filters=num_filters[i],
                kernel_size=filter_sizes[i],
                padding='valid',
                activation='relu',
                strides=1)(input)
  pool = MaxPooling1D(pool_size=pooling_sizes[i])(conv)
  flatten = Flatten()(pool)
  convs.append(flatten)
out = Concatenate()(convs)
graph = Model(inputs=input, outputs=out)

model = Sequential()
model.add(Dropout(dropout_rates[0], input_shape=(sequence_length, vocab_size)))
model.add(graph)
model.add(Dense(hidden_size))
model.add(Dropout(dropout_rates[1]))
model.add(Dense(classes))
model.add(Activation('softmax'))
model.compile(loss='sparse_categorical_crossentropy', optimizer='adadelta', metrics=['accuracy'])

########################################################################################################################
# Training
########################################################################################################################

batch_size = 200
epochs = 200

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
print(test_result)
