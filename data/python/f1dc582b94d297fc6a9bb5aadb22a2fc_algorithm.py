import numpy as np
import bottleneck as bn
import sosat.algorithm as algo


class WalkSAT(algo.Algorithm):
    P = 0.5

    profiles = [
        {}
    ]

    def __init__(self, num_vars=0, clauses=[], config={}):
        super(WalkSAT, self).__init__(num_vars, clauses, config)

        # initialize random model
        self.model = np.random.choice([True, False], self.num_vars)

    def run(self):
        for iteration in range(self.MAX_ITERATIONS):
            model = self.model

            # check all clauses
            evaluated = self.evaluate_candidate(model)

            # return if model satisfies all clauses
            if np.all(evaluated):
                if self.VERBOSE:
                    print 'c', 'Found solution after {} iterations'.format(iteration)
                return model

            if self.VERBOSE:
                print 'c', 'Satisfied clauses: {} of {}'.format(np.sum(evaluated), self.num_clauses)

            unsatisfied = np.where(np.logical_not(evaluated))[0]
            clause_index = np.random.choice(unsatisfied, 1)[0]
            clause = self.raw_clauses[clause_index]
            if self.P < np.random.random_sample():
                lit = np.random.choice(clause, 1)[0]
                flip = abs(lit) - 1
            else:
                evaluated = []
                for i, lit in enumerate(clause):
                    var = abs(lit)
                    flip = var - 1
                    model[flip] = not model[flip]
                    evaluated.append((np.sum(self.evaluate_candidate(model)), var))
                    model[flip] = not model[flip]  # undo flipping
                flip = max(evaluated, key=lambda x: x[0])[1] - 1
            # variables start with 1, so need to subtract 1
            assert 0 <= flip < self.num_vars, flip
            model[flip] = not model[flip]
        return None
