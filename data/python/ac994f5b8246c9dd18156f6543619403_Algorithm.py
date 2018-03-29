import numpy as N
import cPickle
import datetime
import sqlite3

class Algorithm:
    def print_params(self):
        """
        prints the params involved into the training to standard output.
        """
        for (k, v) in self.params.iteritems():
            print "%-25s: %s" % (k,v)
    
    def record_to_sql(self, database_path):
        """
        records the results obtain after running the training into a sqlite3 database. The path is given as a parameter. 
        If the database does not exist, then it is created.
        """
        conn = sqlite3.connect(database_path)
        c = conn.cursor()
        # Create table
        c.execute("""create table if not exists results(date text, algorithm text, epochs integer, features integer, running_time integer, RMSE real, training_dataset text, test_dataset text, match real);""")
        # Insert a row of data
        data = (datetime.datetime.now().strftime("%d-%m-%Y_%H:%M"), self.ALGORITHM, self.MAX_EPOCHS, self.MAX_FEATURES, self.running_time, self.rmse, self.TRAINING_DATASET, self.TEST_DATASET, )
        c.execute('insert into results(date, algorithm, epochs, features, running_time, RMSE, training_dataset, test_dataset) values (?,?,?,?,?,?,?,?);', data)
        # Save (commit) the changes
        conn.commit()
        # Close the cursor
        c.close()
        
    def __init__(self, params):
        """
        constructor of the algorithm. params is a dict and it represents the parameters of the training. Bellow is a list of
        a possible configuration:
        
        params = {
            'ALGORITHM'            : 'ISMF', # ISMF or RISMF

            'MIN_IMPROVEMENT'      : 0.0001,  # Minimum improvement required to continue current feature
            'LEARNING_RATE'        : 0.001,   # Learning rate
            'REG_FACTOR'           : 0.015,  
            'MAX_FEATURES'         : 20,      # Number of features to use; or factors
            'DEFAULT_FEATURE_VALUE': 0.1,     # Initialization value for features
            'SQR_INIT'             : 0.01,    # DEFAULT_FEATURE_VALUE * DEFAULT_FEATURE_VALUE
            'MAX_EPOCHS'           : 50,      # Max epochs per feature
            'MIN_EPOCHS'           : 1,

            'MAX_MOVIES'           : 1683,    # Movies in entire training set (+1)
            'MAX_USERS'            : 944,     # Users in entire training set (+1)
            'MAX_RATINGS'          : 100001,  # Ratings in entire training set (+1)

            'TRAINING_DATASET'     : 'dataset/u1.base',
            'TEST_DATASET'         : 'dataset/u1.test',
            'RECORD_RESULTS_TO_SQL': True,
        }
        
        """
        self.params = params
        
        # setup parameters
        self.MIN_IMPROVEMENT       = params['MIN_IMPROVEMENT']
        self.LEARNING_RATE         = params['LEARNING_RATE']
        self.MIN_IMPROVEMENT       = params['MIN_IMPROVEMENT']
        self.LEARNING_RATE         = params['LEARNING_RATE']
        self.REG_FACTOR            = params['REG_FACTOR']
        self.MAX_FEATURES          = params['MAX_FEATURES']
        self.DEFAULT_FEATURE_VALUE = params['DEFAULT_FEATURE_VALUE']
        self.SQR_INIT              = params['SQR_INIT']
        self.MAX_EPOCHS            = params['MAX_EPOCHS']
        self.MIN_EPOCHS            = params['MIN_EPOCHS']
             
        self.MAX_MOVIES            = params['MAX_MOVIES']
        self.MAX_USERS             = params['MAX_USERS']
        self.MAX_RATINGS           = params['MAX_RATINGS']
        
        self.ALGORITHM             = params['ALGORITHM']
        self.TRAINING_DATASET      = params['TRAINING_DATASET']
        self.TEST_DATASET          = params['TEST_DATASET']
        
        self.RECORD_RESULTS_TO_SQL = params['RECORD_RESULTS_TO_SQL']
        
        # setup data structures needed
        
        print 'Initializing user and movies feature matrices ...'
        self.usersItemsMovies = N.empty( (self.MAX_RATINGS, 3), 'i')
        self.movieFeatures = N.empty( (self.MAX_FEATURES, self.MAX_MOVIES), 'f' )  # Array of features by movie
        self.userFeatures  = N.empty( (self.MAX_FEATURES, self.MAX_USERS),  'f' )  # Array of features by customer
        self.cache         = N.empty( (self.MAX_USERS, self.MAX_MOVIES),    'f' )  # self.cache all the residuals

        self.movieFeatures.fill(self.DEFAULT_FEATURE_VALUE)
        self.userFeatures.fill(self.DEFAULT_FEATURE_VALUE)
        
        print 'Initializing algorithm with params: '
        self.print_params()
        
        self.running_time = 0
        self.rmse = 0.0
        
    def read_data(self):
        """
        read data from the file given in the params['TRAINING_DATASET'] parameter. Data format is in tab separated values:
        user_id | movie_id | rating
        """
        
        print 'Reading dataset ...'
        myfile = open(self.TRAINING_DATASET)
        i = 0
        for movie_line in myfile.readlines():
            movie_line = movie_line.rstrip()
            (user_id, movie_id, rating, date) = movie_line.split("\t",  3)
            self.usersItemsMovies[i][0] = user_id
            self.usersItemsMovies[i][1] = movie_id
            self.usersItemsMovies[i][2] = rating
            i += 1

    def predict_rating(self, movie_id, user_id, feature, cache, bTrailing = True):
        """
        Computes the rating at a certain moment for movie_id and user_id.
        """
        # PredictRating
        # - During training there is no need to loop through all of the features
        # - Use a self.cache for the leading features and do a quick calculation for the trailing
        # - The trailing can be optionally removed when calculating a new self.cache value

        # Get self.cached value for old features or default to an average
        if (cache > 0):
            sum = cache
        else:
            sum = 1.0

        # Add contribution of current feature
        sum += self.movieFeatures[feature][movie_id] * self.userFeatures[feature][user_id]
        if (sum > 5.0): sum = 5.0
        if (sum < 1.0): sum = 1.0

        # Add up trailing defaults values
        if (bTrailing):
            sum += (self.MAX_FEATURES - feature - 1) * self.SQR_INIT
            if (sum > 5.0): sum = 5.0
            if (sum < 1.0): sum = 1.0

        return sum

    def compute_features(self):
        """
        compute userFeatured and movieFeatures matrices used to predict the ratings for the users.
        """

        self.rmse = 5.0

        for f in xrange(self.MAX_FEATURES):
            print '   Calculating feature: ', f, ' ---'

            # user and movies feature number f
            p_u = self.userFeatures[f]    # p_u a vector: f1 = [u1,u2,...,un]
            q_i = self.movieFeatures[f]   # q_i a vector: f2 = [m1,m2,...,mn]

            for e in xrange(self.MAX_EPOCHS):
                sqErr     = 0.0
                rmse_last = self.rmse

                # Iterate over all (user_id,movie_id,rating) tuples the dataset
                for v in self.usersItemsMovies:  
                    movie_id = v[1]
                    user_id  = v[0] 
                    rating   = v[2]

                    # Predict rating and calc error
                    err     = rating - self.predict_rating(movie_id, user_id, f, self.cache[user_id][movie_id], True)
                    sqErr  += err * err

                    # self.cache off old feature values
                    cf = p_u[user_id]
                    mf = q_i[movie_id]

                    # Cross-train the features based on the algorithm
                    if self.ALGORITHM == "RISMF":
                        p_u[user_id] += self.LEARNING_RATE * (err * mf - self.REG_FACTOR * cf)
                        q_i[movie_id] += self.LEARNING_RATE * (err * cf - self.REG_FACTOR * mf)
                    elif self.ALGORITHM == "ISMF":
                        p_u[user_id] += self.LEARNING_RATE * err * mf
                        q_i[movie_id] += self.LEARNING_RATE * err * cf
                    
                self.rmse = N.sqrt(sqErr / self.MAX_RATINGS)

                print '     Epoch = ', e, '/', self.MAX_EPOCHS, '; square error = ', sqErr, ' rmse = ', self.rmse

                # Keep looping until you have passed a minimum number
                # of epochs or have stopped making significant progress
                if (e >= self.MIN_EPOCHS and self.rmse > rmse_last - self.MIN_IMPROVEMENT and f != 0): break

            # self.cache off old predictions
            for i, v in enumerate(self.usersItemsMovies):
                self.cache[v[0]][v[1]] = self.predict_rating(v[1], v[0], f, self.cache[v[0]][v[1]], False)
    
    def run(self):
        """
        Run a training. 
        """
        
        self.read_data()

        print 'Calculating features ...'
        start_time = datetime.datetime.now()
        self.compute_features()
        stop_time = datetime.datetime.now()
        self.running_time = (stop_time-start_time).seconds
        
        print 'Saving features ...'
        userFeaturesFile = "results/userFeatures_%s.txt" % datetime.datetime.now().strftime("%d-%m-%Y_%H:%M")
        fOut = open(userFeaturesFile, 'wb')
        cPickle.dump(self.userFeatures, fOut, protocol=-1)
        fOut.close()

        movieFeaturesFile = "results/movieFeatures_%s.txt" % datetime.datetime.now().strftime("%d-%m-%Y_%H:%M")
        fOut = open(movieFeaturesFile, 'wb')
        cPickle.dump(self.movieFeatures, fOut, protocol=-1)
        fOut.close()
        
        if self.RECORD_RESULTS_TO_SQL:
            self.record_to_sql('results/results.sqlite3')
            
        print 'Done.'