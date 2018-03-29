context("Shogun Examples")
library(shogun)

test_that("classifier_libsvm_modular [Gaussian Kernel]", {
  ## In this example a two-class support vector machine classifier is trained on a
  ## toy data set and the trained classifier is used to predict labels of test
  ## examples. As training algorithm the LIBSVM solver is used with SVM
  ## regularization parameter C=1 and a Gaussian kernel of width 2.1 and the
  ## precision parameter epsilon=1e-5. The example also shows how to retrieve the
  ## support vectors from the train SVM model.
  
  dat.train <- as.matrix(read.table(shikkenTestData('fm_train_real.dat', 'toy')))
  dat.test <- as.matrix(read.table(shikkenTestData('fm_test_real.dat', 'toy')))
  y <- read.table(shikkenTestData('label_train_twoclass.dat'))[,1]
  
  C <- 1.017
  epsilon <- 1e-5
  num_threads <- as.integer(2)
  width <- 2.1
  
  #############################################################################
  ## modular interface & results
  feats_train <- RealFeatures(fm_train_real)
  feats_test <- RealFeatures(fm_test_real)
  kernel <- GaussianKernel(feats_train, feats_train, width)
  labels <- Labels(y)
  svm <- LibSVM(C, kernel, labels)
  dump <- svm$set_epsilon(svm, epsilon)
  dump <- svm$parallel$set_num_threads(svm$parallel, num_threads)
  dump <- svm$train(svm)
  dump <- kernel$init(kernel, feats_train, feats_test)
  lab <- svm$classify(svm)
  out <- lab$get_labels(lab)
  
  #############################################################################
  ## shikken interface & results
  sh <- SVM(dat.train, y, kernel='gaussian', width=2.1, C=1.017,
            epsilon=1e-5, threads=num_threads, svm.engine='libsvm')
  preds.train <- predict(sh)
  preds.test <- predict(sh, dat.test)
  
  #############################################################################
  ## compare
  expect_equal(preds.test, out)
})


test_that("classifier_svmlight_modular [StringKernel]" {
  ## In this example a two-class support vector machine classifier is trained on a
  ## DNA splice-site detection data set and the trained classifier is used to predict
  ## labels on test set. As training algorithm SVM^light is used with SVM
  ## regularization parameter C=1.2 and the Weighted Degree kernel of degree 20 and
  ## the precision parameter epsilon=1e-5.
  fm_train_dna <- as.matrix(read.table(shikkenTestData('data/fm_train_dna.dat')))
  fm_test_dna <- as.matrix(read.table(shikkenTestData('data/fm_test_dna.dat')))
  label_train_dna <- as.real(read.table(shikkenTestData('label_train_dna.dat'))$V1)

  C <- 1.017
  epsilon <- 1e-5
  num_threads <- as.integer(3)
  degree <- as.integer(20)
  
  #############################################################################
  ## modular interface & results
  feats_train <- StringCharFeatures("DNA")
  dump <- feats_train$set_features(feats_train, fm_train_dna)
  feats_test <- StringCharFeatures("DNA")
  dump <- feats_test$set_features(feats_test, fm_test_dna)
  
  kernel <- WeightedDegreeStringKernel(feats_train, feats_train, degree)
  
  labels <- Labels(as.real(label_train_dna))
  
  svm <- SVMLight(C, kernel, labels)
  dump <- svm$set_epsilon(svm, epsilon)
  dump <- svm$parallel$set_num_threads(svm$parallel, num_threads)
  dump <- svm$train(svm)
  # lab.train <- svm$classify()
  
  dump <- kernel$init(kernel, feats_train, feats_test)
  lab <- svm$classify(svm)
  out <- lab$get_labels(lab)
  
  #############################################################################
  ## shikken interface & results
  sh <- SVM(fm_train_dna, label_train_dna, kernel='weighted.degree.string',
            C=C, epsilon=epsilon, threads=threads, degree=degree,
            alphabet="DNA")
  preds.train <- predict(sh)
  preds.test <- predict(sh, fm_test_dna)
  
  #############################################################################
  ## compare
  expect_equal(preds.test, out)
})