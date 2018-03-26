#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Inspired by LeCun paper (2016): "Text Understanding from Scratch"

__author__ = 'maxim'

from keras import *
from keras.layers import *
from keras.utils import to_categorical

from input import DataProvider, Mode

########################################################################################################################
# Data
########################################################################################################################

print('Building vocabulary...')
provider = DataProvider('../data', mode=Mode.BY_CHAR)
provider.build(min_vocab_count=30)
vocab_size = provider.vocab_size
classes = provider.classes
print('Vocab size=%d classes=%d' % (vocab_size, classes))

########################################################################################################################
# Model
########################################################################################################################

# Hyper-parameters
sequence_length = 2 * 1024
filter_sizes = (3, 5, 9, 19)
pooling_sizes = (2, 5, 9, 19)
num_filters = 128
dropout_rates = (0.3, 0.6)
hidden_size = 128

input = Input(shape=(sequence_length, vocab_size))
convs = []
for i in range(0, len(filter_sizes)):
  conv = Conv1D(filters=num_filters,
                kernel_size=filter_sizes[i],
                padding='valid',
                activation='relu',
                strides=1)(input)
  pool = MaxPooling1D(pool_size=pooling_sizes[i])(conv)
  flatten = Flatten()(pool)
  convs.append(flatten)

if len(filter_sizes) > 1:
  out = Concatenate()(convs)
else:
  out = convs[0]

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

batch_size = 100
steps = 100
epochs = 100

def gen(steps, batch_size):
  while True:
    for batch_x, batch_y, batch_len in provider.stream_snippets(batch_size=batch_size, max_tokens=sequence_length):
      steps -= 1
      if steps == 0:
        return
      batch_x_one_hot = to_categorical(batch_x, vocab_size)
      yield batch_x_one_hot, batch_y

model.fit_generator(gen(steps * epochs + 1, batch_size), epochs=epochs, steps_per_epoch=steps, verbose=1)
