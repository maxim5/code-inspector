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


TF_IDF = False

print('Building vocabulary...')
provider = DataProvider('../data')
provider.build(min_vocab_count=300)
vocab_size = provider.vocab_size
classes = provider.classes
print('Vocab size=%d classes=%d' % (vocab_size, classes))

if TF_IDF:
  print('Building IDF...')
  _, idf = provider.build_tf_idf()
  print('IDF shape:', idf.shape)

def encode(batch_x, batch_len):
  batch_size = batch_x.shape[0]
  count_array = np.zeros([batch_size, vocab_size])
  for i in range(batch_size):
    unique, counts = np.unique(batch_x[i,:batch_len[i]], return_counts=True)
    for idx, num in zip(unique, counts):
      if idx >= 0:
        if TF_IDF:
          tf_ = np.log(num) + 1 if num > 0 else 0
          count_array[i, idx] = tf_ * idf[idx]
        else:
          count_array[i, idx] = num
  return count_array


########################################################################################################################
# Model
########################################################################################################################


x = tf.placeholder(shape=[None, vocab_size], dtype=tf.float32, name='x')
y = tf.placeholder(shape=[None], dtype=tf.int32, name='y')
training = tf.placeholder_with_default(False, shape=[], name='training')

reg = tf.contrib.layers.l2_regularizer(0.1)
hidden1 = tf.layers.dense(x, units=128, kernel_regularizer=reg, activation=tf.nn.elu, name='hidden1')
dropout1 = tf.layers.dropout(hidden1, rate=0.5, training=training, name='dropout1')

logits = tf.layers.dense(dropout1, units=classes, kernel_regularizer=reg, activation=None, name='logits')
loss = tf.reduce_mean(tf.nn.sparse_softmax_cross_entropy_with_logits(logits=logits, labels=y))
prediction = tf.argmax(logits, axis=1, name='prediction')
predicted_distribution = tf.nn.softmax(logits, name='distribution')
correct_predicted = tf.equal(tf.argmax(logits, 1), tf.cast(y, tf.int64), name='correct-prediction')
wrong_predicted = tf.logical_not(correct_predicted, name='incorrect-prediction')
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


path = '_temp/dnn_%d' % int(TF_IDF)
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
  for batch_x, batch_y, batch_len in provider.stream_snippets(batch_size=1024, files=Files.TRAIN):
    train_step(batch_x, batch_y, batch_len)

def train_step(batch_x, batch_y, batch_len):
  fetches = [train_op, loss, accuracy, merged, global_step]
  feed_dict = {x: encode(batch_x, batch_len), y: batch_y, training: True}
  _, loss_val, acc, summary, step = sess.run(fetches, feed_dict)
  step += 1

  if step % 5 == 0:
    print('iteration=%d  loss=%4.3f  train-acc=%.5f' % (step, loss_val, acc))
    writer.add_summary(summary, global_step=step)

  if step % 10 == 0:
    cur_saver.save(sess, os.path.join(cur_ckpt_path, 'current'), step)

current_top_accuracy = 0.5

def predict(files):
  all_acc = []
  for batch_x, batch_y, batch_len in provider.stream_snippets(batch_size=1024, files=files):
    acc = sess.run(accuracy, feed_dict={x: encode(batch_x, batch_len), y: batch_y})
    all_acc.append(acc)

  mean_accuracy = np.mean(all_acc)
  marker = ''
  global current_top_accuracy
  if files == Files.VAL and mean_accuracy > current_top_accuracy:
    current_top_accuracy = mean_accuracy
    marker = ' !!!'
    step = sess.run(global_step)
    top_saver.save(sess, os.path.join(top_ckpt_path, 'top-%.5f' % mean_accuracy), step)

  print(files, 'mean accuracy = %.5f%s' % (mean_accuracy, marker))

def explore(files):
  for batch_x, batch_y, batch_len in provider.stream_snippets(batch_size=100, files=files):
    acc, pred, dist, idx = sess.run([accuracy, prediction, predicted_distribution, wrong_predicted],
                                    feed_dict={x: encode(batch_x, batch_len), y: batch_y})
    if acc < 0.9:
      label_decoder = provider.labels.idx_to_token

      x_val = batch_x[idx]
      x_len = batch_len[idx]
      y_pred = pred[idx]
      y_correct = batch_y[idx]
      print('Misclassified snippets:')
      for i in range(x_val.shape[0]):
        with util.print_options(precision=4, suppress=True):
          print('Dist:', dist[i])
          # print('Idx:', x_len[i], x_val[i,:x_len[i]])
          print()
          print('Predicted=%s actual=%s' % (label_decoder[y_pred[i]], label_decoder[y_correct[i]]))
          print('~~~~~~~~~~~~~~ Snippet start ~~~~~~~~~~~~~~')
          print(provider.decode(snippet=x_val[i], length=x_len[i]))
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
