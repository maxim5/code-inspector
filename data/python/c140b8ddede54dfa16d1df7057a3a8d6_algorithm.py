import numpy as np


class Algorithm(object):
    """
    This is an abstract superclass for all algorithms.
    """
    # seed for random numbers
    SEED = 42
    # display debug messages
    VERBOSE = False
    # collect statistics
    COLLECT_STATS = False
    # maximum number of iterations
    MAX_ITERATIONS = 25000

    profiles = {}

    def __init__(self, num_vars=0, clauses=[], config={}):
        """
        Initializes the algorithm with the instance and the configuration.
        """
        self.num_vars = np.int_(num_vars)
        self.num_lits = np.int_(2 * num_vars)
        self.raw_clauses = clauses
        self.num_clauses = len(clauses)

        self.__dict__.update(config)

        np.random.seed(self.SEED)

        self.initialize_clauses()

    def initialize_clauses(self):
        """
        Generates full clauses consisting of truth values for positive and negated variables.
        """
        num_clauses = len(self.raw_clauses)
        shape = (num_clauses, 2, self.num_vars)
        clauses = np.zeros(dtype=np.bool, shape=shape)
        for i, clause in enumerate(self.raw_clauses):
            for lit in clause:
                if lit > 0:
                    clauses[i][0][lit - 1] = True
                else:
                    clauses[i][1][-lit - 1] = True
        self.clauses = clauses

    def full_candidate(self, candidate):
        """
        Generate full solution candidate for solution candidate consisting of truth values for positive and negated variables.
        """
        return np.array([candidate, ~candidate])

    def evaluate_full_candidate(self, full_candidate):
        """
        Evaluate a full solution candidate for every clause.
        """
        return np.any(self.clauses & full_candidate, axis=(2, 1))

    def evaluate_candidate(self, candidate):
        """
        Evaluate a solution candidate for every clause.
        """
        return self.evaluate_full_candidate(self.full_candidate(candidate))
