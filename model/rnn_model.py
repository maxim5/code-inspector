#!/usr/bin/env python
# -*- coding: utf-8 -*-
__author__ = 'maxim'


import os
import tensorflow as tf

from input import *


########################################################################################################################
# Data
########################################################################################################################


provider = DataProvider('../data')
provider.build(min_vocab_count=500)
vocab_size = provider.vocab_size
classes = provider.classes
print('Vocab size=%d classes=%d' % (vocab_size, classes))


########################################################################################################################
# Model
########################################################################################################################


HIDDEN_SIZE = 200

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
logits = tf.layers.dense(output_last, units=classes, activation=None, name='logits')
loss = tf.reduce_mean(tf.nn.sparse_softmax_cross_entropy_with_logits(logits=logits, labels=y))
accuracy = tf.reduce_mean(tf.cast(tf.equal(tf.argmax(logits, 1), tf.cast(y, tf.int64)), tf.float32))
global_step = tf.Variable(0, dtype=tf.int32, trainable=False, name='global_step')
optimizer = tf.train.AdamOptimizer().minimize(loss, global_step=global_step)


########################################################################################################################
# Training
########################################################################################################################


path = '_temp/rnn'
summary_path = os.path.join(path, 'summary')
cur_ckpt_path = os.path.join(path, 'checkpoints', 'cur')
top_ckpt_path = os.path.join(path, 'checkpoints', 'top')
os.makedirs(summary_path, exist_ok=True)
os.makedirs(cur_ckpt_path, exist_ok=True)
os.makedirs(top_ckpt_path, exist_ok=True)


saver = tf.train.Saver(max_to_keep=2)
with tf.Session() as sess:
  writer = tf.summary.FileWriter(summary_path, sess.graph)
  sess.run(tf.global_variables_initializer())

  ckpt = tf.train.get_checkpoint_state(cur_ckpt_path)
  if ckpt and ckpt.model_checkpoint_path:
    saver.restore(sess, ckpt.model_checkpoint_path)

  for epoch in range(10):
    for batch_x, batch_y, batch_len in provider.stream_data(batch_size=64):
      _, loss_val, accuracy_val, step = sess.run([optimizer, loss, accuracy, global_step],
                                                 feed_dict={x: batch_x, y: batch_y})
      print('iteration=%d loss=%.3f accuracy=%.3f' % (step + 1, loss_val, accuracy_val))
      if (step + 1) % 100 == 0:
        saver.save(sess, os.path.join(cur_ckpt_path, 'current'), step)
