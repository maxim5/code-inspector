import numpy as np
import sosat.algorithm as algo


class SimulatedAnnealing(algo.Algorithm):
    INITIAL_TEMPERATURE = 15.0
    TEMPERATURE = INITIAL_TEMPERATURE
    ITERATIONS = 5
    ENERGY_MULTIPLICATOR = 8

    def __init__(self, num_vars=0, clauses=[], config={}):
        super(SimulatedAnnealing, self).__init__(num_vars, clauses, config)

    def run(self):
        candidate = np.random.choice([True, False], self.num_vars)
        quality = np.sum(self.evaluate_candidate(candidate))
        TEMPERATURE = self.TEMPERATURE
        while TEMPERATURE > 0:
            max_toggles = 3
            #self.num_clauses / 4
            #int(self.TEMPERATURE / self.INITIAL_TEMPERATURE * self.num_vars * 3 / 4) + 1

            for i in xrange(self.ITERATIONS):
                new_candidate = candidate.copy()

                for j in xrange(np.random.random_integers(0, max_toggles)):
                    where_to_toggle = np.random.random_integers(0, self.num_vars - 1)
                    new_candidate[where_to_toggle] = ~new_candidate[where_to_toggle]

                new_quality = np.sum(self.evaluate_candidate(new_candidate))

                if new_quality == self.num_clauses:
                    return new_candidate

                #print "diff: ", new_quality - quality, "P = ", np.e ** ((new_quality - quality) / self.TEMPERATURE)

                if np.random.rand() < np.e ** ((new_quality - quality) * self.ENERGY_MULTIPLICATOR / TEMPERATURE):
                    candidate, quality = new_candidate, new_quality

            TEMPERATURE = TEMPERATURE - 1

            #if TEMPERATURE % 10 == 0:
            #    print "T = ", TEMPERATURE
            #    print quality, " / ", self.num_clauses

        return candidate
        print quality, " / ", self.num_clauses
