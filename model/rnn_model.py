#!/usr/bin/env python
# -*- coding: utf-8 -*-
__author__ = 'maxim'


import tensorflow as tf


HIDDEN_SIZE = 200
LR = 0.01


# Data

from input import DataProvider

provider = DataProvider('../data')
provider.build(min_vocab_count=20)
vocab_size = provider.vocab_size
classes = provider.classes


# Model

x = tf.placeholder(tf.int32, shape=[None, None])
y = tf.placeholder(tf.int32, shape=[None])

seq_one_hot = tf.one_hot(x, vocab_size)
cell = tf.nn.rnn_cell.GRUCell(HIDDEN_SIZE)
zero_state = cell.zero_state(batch_size=tf.shape(seq_one_hot)[0], dtype=tf.float32)
in_state = tf.placeholder_with_default(input=zero_state, shape=[None, HIDDEN_SIZE])

length = tf.reduce_sum(tf.reduce_max(seq_one_hot, 2), 1)
output, out_state = tf.nn.dynamic_rnn(cell, seq_one_hot, length, in_state)

# Get output of RNN sequence
# output = (?, max_sequence_length, rnn_hidden_size)
# output_last = (?, rnn_hidden_size)
output = tf.transpose(output, [1, 0, 2])
output_last = output[-1]

# fully_connected is syntactic sugar for tf.matmul(w, output) + b
# it will create w and b for us
logits = tf.contrib.layers.fully_connected(output_last, classes, None)
loss = tf.reduce_sum(tf.nn.sparse_softmax_cross_entropy_with_logits(logits=logits, labels=y))
accuracy = tf.reduce_mean(tf.cast(tf.equal(tf.argmax(logits, 1), tf.cast(y, tf.int64)), tf.float32))
global_step = tf.Variable(0, dtype=tf.int32, trainable=False, name='global_step')
optimizer = tf.train.AdamOptimizer(LR).minimize(loss, global_step=global_step)


# Training


import os
def make_dir(directory):
  if not os.path.exists(directory):
    os.makedirs(directory)
make_dir('_temp/checkpoint')

saver = tf.train.Saver(max_to_keep=2)
with tf.Session() as sess:
  writer = tf.summary.FileWriter('_temp/graphs', sess.graph)
  sess.run(tf.global_variables_initializer())

  ckpt = tf.train.get_checkpoint_state(os.path.dirname('_temp/checkpoints/checkpoint'))
  if ckpt and ckpt.model_checkpoint_path:
    saver.restore(sess, ckpt.model_checkpoint_path)

  for epoch in range(10):
    for batch_x, batch_y, batch_len in provider.stream_data(batch_size=100, parallel_streams=10):
      _, loss_val, accuracy_val, iteration = sess.run([optimizer, loss, accuracy, global_step],
                                                      feed_dict={x: batch_x, y: batch_y})
      print('iteration=%d loss=%.3f accuracy=%.3f' % (iteration + 1, loss_val, accuracy_val))
      if (iteration + 1) % 100 == 0:
        saver.save(sess, '_temp/checkpoints/rnn', iteration)
