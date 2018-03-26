#!/usr/bin/env python
# -*- coding: utf-8 -*-
__author__ = 'maxim'


import os
import tensorflow as tf

from input import *


########################################################################################################################
# Data
########################################################################################################################


print('Building vocabulary...')
provider = DataProvider('../data')
provider.build(min_vocab_count=300)
vocab_size = provider.vocab_size
classes = provider.classes
print('Vocab size=%d classes=%d' % (vocab_size, classes))


########################################################################################################################
# Model
########################################################################################################################


cell_size = 128
embedding_size = 64

x = tf.placeholder(tf.int32, shape=[None, None], name='x')
y = tf.placeholder(tf.int32, shape=[None], name='y')
length = tf.placeholder(tf.int32, shape=[None], name='length')

embeddings = tf.Variable(tf.random_uniform([vocab_size, embedding_size], -1.0, 1.0), name='embeddings')
embed = tf.nn.embedding_lookup(embeddings, x, name='embed')

cell = tf.contrib.rnn.MultiRNNCell([tf.nn.rnn_cell.GRUCell(cell_size) for _ in range(2)])
output, out_state = tf.nn.dynamic_rnn(cell, inputs=embed, sequence_length=length, dtype=tf.float32)
# output = (?, max_sequence_length, rnn_hidden_size)
# output_last = (?, rnn_hidden_size)
output = tf.transpose(output, [1, 0, 2])
output_last = output[-1]

logits = tf.layers.dense(output_last, units=classes, activation=None, name='logits')
predicted_distribution = tf.nn.softmax(logits, name='distribution')
loss = tf.reduce_mean(tf.nn.sparse_softmax_cross_entropy_with_logits(logits=logits, labels=y))
correct_predicted = tf.equal(tf.argmax(logits, 1), tf.cast(y, tf.int64), name='correct-prediction')
accuracy = tf.reduce_mean(tf.cast(correct_predicted, tf.float32), name='accuracy')

global_step = tf.Variable(1, dtype=tf.int32, trainable=False, name='global_step')
optimizer = tf.train.AdamOptimizer()
grads_and_vars = optimizer.compute_gradients(loss)
train_op = optimizer.minimize(loss, global_step=global_step)

tf.summary.scalar('loss', loss)
tf.summary.scalar('accuracy', accuracy)

tf.summary.histogram('x', x)
tf.summary.histogram('length', length)
tf.summary.histogram('logits', logits)
tf.summary.histogram('predicted_distribution', predicted_distribution)
for grad, var in grads_and_vars:
  name = var.name[:-2]
  tf.summary.histogram(name + '-var', var)
  tf.summary.histogram(name + '-grad', grad)
merged = tf.summary.merge_all()


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

cur_saver = tf.train.Saver(max_to_keep=3)
top_saver = tf.train.Saver(max_to_keep=3)
writer = tf.summary.FileWriter(summary_path, tf.get_default_graph())


########################################################################################################################
# Session
########################################################################################################################


with tf.Session() as sess:
  ckpt = tf.train.get_checkpoint_state(cur_ckpt_path)
  if ckpt and ckpt.model_checkpoint_path:
    cur_saver.restore(sess, ckpt.model_checkpoint_path)
  else:
    sess.run(tf.global_variables_initializer())

  for epoch in range(20):
    print('\n--- Epoch %d ---\n' % (epoch + 1))
    for batch_x, batch_y, batch_len in provider.stream_snippets(batch_size=1024, max_tokens=256, snippet_max_lines=10):
      _, loss_val, acc, summary, step = sess.run([train_op, loss, accuracy, merged, global_step],
                                                 feed_dict={x: batch_x, y: batch_y, length: batch_len})

      if step % 10 == 0:
        print('iteration=%d  loss=%4.3f  train-acc=%.5f' % (step, loss_val, acc))
        writer.add_summary(summary, global_step=step)

      if step % 50 == 0:
        cur_saver.save(sess, os.path.join(cur_ckpt_path, 'current'), step)
