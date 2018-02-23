#!/usr/bin/env python
# -*- coding: utf-8 -*-
__author__ = 'maxim'

import os
import numpy as np
import tensorflow as tf

from input import *


########################################################################################################################
# Data
########################################################################################################################


provider = DataProvider('../data')
provider.build(min_vocab_count=200)
vocab_size = provider.vocab_size
classes = provider.classes
print('Vocab size=%d classes=%d' % (vocab_size, classes))

def encode(batch_x):
  batch_size = batch_x.shape[0]
  count_array = np.zeros([batch_size, vocab_size])
  for i in range(batch_size):
    unique, counts = np.unique(batch_x[i], return_counts=True)
    for idx, num in zip(unique, counts):
      if idx >= 0:
        count_array[i, idx] = num
  return count_array


########################################################################################################################
# Model
########################################################################################################################


x = tf.placeholder(shape=[None, vocab_size], dtype=tf.float32, name='x')
y = tf.placeholder(shape=[None], dtype=tf.int32, name='y')
training = tf.placeholder_with_default(False, shape=[], name='training')

hidden1 = tf.layers.dense(x, 128, activation=tf.nn.relu, name='hidden1')
dropout1 = tf.layers.dropout(hidden1, rate=0.1, training=training, name='dropout1')

logits = tf.layers.dense(dropout1, classes, activation=tf.nn.relu, name='logits')
loss = tf.reduce_mean(tf.nn.sparse_softmax_cross_entropy_with_logits(logits=logits, labels=y))
prediction = tf.argmax(logits, 1)
correct_predicted = tf.nn.in_top_k(logits, y, 1, name='top-1')
wrong_predicted = tf.logical_not(correct_predicted, name='not-top-1')
x_misclassified = tf.boolean_mask(x, wrong_predicted, name='misclassified')
accuracy = tf.reduce_mean(tf.cast(correct_predicted, tf.float32), name='accuracy')

global_step = tf.Variable(0, dtype=tf.int32, trainable=False, name='global_step')
optimizer = tf.train.AdamOptimizer()
grads_and_vars = optimizer.compute_gradients(loss)
train_op = optimizer.minimize(loss, global_step=global_step)

tf.summary.scalar('loss', loss)
tf.summary.scalar('accuracy', accuracy)

tf.summary.histogram('x', x)
tf.summary.histogram('hidden-activations-1', hidden1)
for grad, var in grads_and_vars:
  name = var.name[:-2]
  tf.summary.histogram(name + '-var', var)
  tf.summary.histogram(name + '-grad', grad)
merged = tf.summary.merge_all()


########################################################################################################################
# Training
########################################################################################################################


path = '_temp/dnn'
summary_path = os.path.join(path, 'summary')
cur_ckpt_path = os.path.join(path, 'checkpoints', 'cur')
top_ckpt_path = os.path.join(path, 'checkpoints', 'top')
os.makedirs(summary_path, exist_ok=True)
os.makedirs(cur_ckpt_path, exist_ok=True)
os.makedirs(top_ckpt_path, exist_ok=True)

cur_saver = tf.train.Saver(max_to_keep=3)
top_saver = tf.train.Saver(max_to_keep=3)
writer = tf.summary.FileWriter(summary_path, tf.get_default_graph())

def train_loop():
  for batch_x, batch_y, batch_len in provider.stream_data(batch_size=512, files=Files.TRAIN):
    train_step(batch_x, batch_y)

def train_step(batch_x, batch_y):
  fetches = [train_op, loss, accuracy, merged, global_step]
  feed_dict = {x: encode(batch_x), y: batch_y, training: True}
  _, loss_val, acc, summary, step = sess.run(fetches, feed_dict)
  step += 1

  if step % 5 == 0:
    print('iteration=%d  loss=%4.3f  train-acc=%.5f' % (step, loss_val, acc))
    writer.add_summary(summary, global_step=step)

  if step % 10 == 0:
    cur_saver.save(sess, os.path.join(cur_ckpt_path, 'current'), step)

current_top_accuracy = 0.95

def predict(files):
  all_acc = []
  for batch_x, batch_y, batch_len in provider.stream_data(batch_size=1024, files=files):
    acc = sess.run(accuracy, feed_dict={x: encode(batch_x), y: batch_y})
    all_acc.append(acc)

  mean_accuracy = np.mean(all_acc)
  print(files, 'mean accuracy = %.5f' % mean_accuracy)

  global current_top_accuracy
  if files == Files.VAL and mean_accuracy > current_top_accuracy:
    current_top_accuracy = mean_accuracy
    step = sess.run(global_step)
    top_saver.save(sess, os.path.join(top_ckpt_path, 'top-%.5f' % mean_accuracy), step)

def explore(files):
  for batch_x, batch_y, batch_len in provider.stream_data(batch_size=100, files=files):
    acc, pred, idx = sess.run([accuracy, prediction, wrong_predicted],
                              feed_dict={x: encode(batch_x), y: batch_y})
    if acc < 0.7:
      label_decoder = provider.labels.idx_to_token

      x_val = batch_x[idx]
      x_len = batch_len[idx]
      y_pred = pred[idx]
      y_correct = batch_y[idx]
      print('Misclassified snippets:')
      for i in range(x_val.shape[0]):
        # print('Idx:', x_len[i], x_val[i,:x_len[i]])
        print()
        print('Predicted=%s actual=%s' % (label_decoder[y_pred[i]], label_decoder[y_correct[i]]))
        print('~~~~~~~~~~~~~~ Snippet start ~~~~~~~~~~~~~~')
        print(vocab.decode(text=x_val[i], length=x_len[i], vocabulary=provider.vocab))
        print('~~~~~~~~~~~~~~~ Snippet end ~~~~~~~~~~~~~~~')


########################################################################################################################
# Session
########################################################################################################################


with tf.Session() as sess:
  ckpt = tf.train.get_checkpoint_state(cur_ckpt_path)
  if ckpt and ckpt.model_checkpoint_path:
    cur_saver.restore(sess, ckpt.model_checkpoint_path)
  else:
    sess.run(tf.global_variables_initializer())

  for epoch in range(50):
    print('\n--- Epoch %d ---\n' % (epoch + 1))
    train_loop()
    writer.flush()
    predict(Files.VAL)
    predict(Files.TEST)
