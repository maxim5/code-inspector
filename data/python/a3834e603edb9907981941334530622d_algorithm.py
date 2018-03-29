import numpy as np
import sosat.algorithm as algo


class AntColonyAlgorithm(algo.Algorithm):
    """
    Implementation of ant colony optimization. Evaluation of solution 
    candidates and initialization of clauses is done in abstract 
    superclass Algorithm.
    """

    # number of ants (per iteration), range: [1, inf)
    NUM_ANTS = 250
    # exponential factor for pheromones in probabilities, range: (-inf, inf)
    EXP_PH = np.float_(1)
    # exponential factor for most constrained variable heuristic in probabilities, range: (-inf, inf)
    EXP_MCV = np.float_(0.5)
    # pheromone reduce factor (per iteration), range: (0, 1)
    PH_REDUCE_FACTOR = np.float_(0.15)
    # blur pheromones interval, range: [1, inf)
    BLUR_ITERATIONS = 3
    # basic (maximum) blurring value, range: [0, 1]
    BLUR_BASIC = np.float_(0.9)
    # blurring decline factor, range: [1, inf)
    BLUR_DECLINE = np.float_(50.0)
    # weight adaption heuristic interval (number of evaluations), range: [1, inf)
    WEIGHT_ADAPTION_DURATION = 250 
    # small epsilon to avoid division by zero
    EPSILON = np.float_(0.0000001)
    
    profiles = [
        # first profile is default profile
        {
            'NUM_ANTS': 250,
            'EXP_PH': np.float_(1),
            'EXP_MCV': np.float_(0.5),
            'PH_REDUCE_FACTOR': np.float_(0.15),
            'BLUR_ITERATIONS': 3,
            'BLUR_BASIC': np.float_(0.9),
            'BLUR_DECLINE': np.float_(50.0),
            'WEIGHT_ADAPTION_DURATION': 250,
            'EPSILON': np.float_(0.0000001)
        },
        {
            'NUM_ANTS': 25,
            'EXP_PH': np.float_(1),
            'EXP_MCV': np.float_(1.5),
            'PH_REDUCE_FACTOR': np.float_(0.35),
            'BLUR_ITERATIONS': 5,
            'BLUR_BASIC': np.float_(0.95),
            'BLUR_DECLINE': np.float_(30.0),
            'WEIGHT_ADAPTION_DURATION': 170,
            'EPSILON': np.float_(0.0000001)
        },
        {
            'NUM_ANTS': 10,
            'EXP_PH': np.float_(1),
            'EXP_MCV': np.float_(0.20),
            'PH_REDUCE_FACTOR': np.float_(0.15),
            'BLUR_ITERATIONS': 3,
            'BLUR_BASIC': np.float_(0.5),
            'BLUR_DECLINE': np.float(5.0),
            'WEIGHT_ADAPTION_DURATION': 5,
            'EPSILON': np.float_(0.0001)
        }
    ]
    
    def __init__(self, num_vars=0, clauses=[], config={}):
        """
        Initialize the algorithm.
        """
        super(AntColonyAlgorithm, self).__init__(num_vars, clauses, config)

        self.initialize_constants()
        self.initialize_variables()
        self.initialize_clause_weights()
        self.initialize_pheromones()
        self.initialize_mcv_heuristic()
        self.initialize_probabilities()

    def initialize_clauses(self):
        """
        Generate full clauses with bool and int type.
        """
        super(AntColonyAlgorithm, self).initialize_clauses()
        self.int_clauses = self.clauses.astype(int)

    def initialize_variables(self):
        """
        Initialize variables that are used outside of run().
        """
        # counts the number of evaluations modulo WEIGHT_ADAPTION_DURATION for weight adaption heuristic
        self.candidate_counter = 0

    def initialize_constants(self):
        """
        Initialize constants that depend on the instance.
        """
        # maximum pheromone value
        self.PH_MAX = np.float_(self.num_vars / (1.0 - self.PH_REDUCE_FACTOR))
        # minimum pheromone value
        self.PH_MIN = np.float_(self.PH_MAX / self.num_lits)

    def initialize_clause_weights(self):
        """
        Initializes clause weights for weight adaption duration. Initially
        all clauses are weighted equal.
        """
        self.clause_weights = np.ones(len(self.clauses), dtype=np.int)

    def initialize_mcv_heuristic(self):
        """
        Most Constrained Variable (MCV) heuristic: variables that appear in 
        most clauses are more important and visited more often.
        """
        self.mcv = np.sum(self.int_clauses, axis=0)

    def initialize_pheromones(self):
        """
        Initialize all pheromones to PH_MAX.
        """
        self.pheromones = np.ndarray((2, self.num_vars), dtype=np.float)
        self.pheromones.fill(self.PH_MAX)

    def initialize_probabilities(self):
        """
        Create probabilities array and update probabilities with pheromones.
        """
        self.probabilities = np.ndarray((2, self.num_vars), dtype=np.float)
        self.update_probabilities()

    def update_probabilities(self):
        """
        Update probabilities based on pheromones and MCV heuristic.
        """
        self.probabilities = self.pheromones**self.EXP_PH * self.mcv**self.EXP_MCV

    def choose_literals(self):
        """
        Choose num_vars literals, each with associated probability.
        """
        # reciprocal norm vector for probabilities of positive literals
        normalization_vector = (np.sum(self.probabilities, axis=0) + self.EPSILON) ** -1
        # for each variable decide wheter to take positive or negative literal
        chosen = np.random.rand(self.num_vars) < normalization_vector * self.probabilities[0]
        return chosen

    def evaluate_solution(self, chosen):
        """
        Evaluate a solution candidate. Return quality of solution (with
        weight adaption heuristic) and number of solved clauses.
        """
        self.candidate_counter += 1

        # evaluation function in abstract superclass
        solved_clauses = self.evaluate_candidate(chosen)
        num_solved_clauses = np.sum(solved_clauses)
        # calculate evaluation with weight adaption heuristic
        evaluation = np.sum(solved_clauses * self.clause_weights)

        if self.candidate_counter == self.WEIGHT_ADAPTION_DURATION:
            # increase weights for unsatisfied clauses
            self.clause_weights += ~solved_clauses
            self.candidate_counter = 0

        return evaluation, num_solved_clauses

    def update_pheromones(self, chosen, evaluation):
        """
        Update pheromones based on solution candidate and its evaluation.
        """
        self.pheromones = self.pheromones * (1.0 - self.PH_REDUCE_FACTOR) + self.full_candidate(chosen) * evaluation
        self.update_pheromones_bounds()

    def update_pheromones_bounds(self):
        """
        Make sure that pheromone values stay within PH_MIN and PH_MAX.
        """
        self.pheromones[self.pheromones < self.PH_MIN] = self.PH_MIN
        self.pheromones[self.pheromones > self.PH_MAX] = self.PH_MAX

    def blur_pheromones(self, max_divergence):
        """
        Blur pheromones by a random percental value within 
        [-max_divergence, max_divergence).
        """
        self.pheromones += self.pheromones * (np.random.rand(2, self.num_vars) * max_divergence * 2 - max_divergence)
        self.update_pheromones_bounds()
        self.update_probabilities()

    def run(self):
        """
        Run the ant colony optimization algorithm.
        """
        for i in xrange(self.MAX_ITERATIONS):
            best_solution = None
            best_evaluation = -1
            best_solved = 0

            # simulate ant
            for a in xrange(self.NUM_ANTS):
                # choose literals beased on probabilities
                literals = self.choose_literals()
                # evaluate solution candidate
                evaluation, solved_clauses = self.evaluate_solution(literals)

                if evaluation > best_evaluation:
                    # save best solution candidate
                    best_evaluation = evaluation
                    best_solution = literals 
                    best_solved = solved_clauses

                    if solved_clauses == self.num_clauses:
                        # solution was found
                        return literals

            # update pheromones based on best solution candidate
            self.update_pheromones(best_solution, evaluation)
            # update probabilities
            self.update_probabilities()

            if i > 0 and i % self.BLUR_ITERATIONS == 0:
                # blur pheromones based on how long the algorithm is already running (declining)
                self.blur_pheromones(self.BLUR_BASIC * np.e**(-i/self.BLUR_DECLINE))
