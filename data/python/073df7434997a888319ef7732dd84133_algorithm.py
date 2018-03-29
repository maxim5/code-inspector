import numpy as np
import controllers.bucket as bucket
import controllers.lifetimes as lifetimes

def main(numberOfStates, numberOfCarriers, totalTime, killRate, tau, test):
	print 'starting algorithm'
	N = numberOfStates
	P = numberOfCarriers
	E = np.floor(P)
	H = np.floor(P)

	empty = 0
	electron = 0.5
	hole = 1.5
	exciton = 2

	totalTime = totalTime
	states = np.zeros(N)
	emissionR = np.array([0])
	stateR = np.array([0,0,0,0,0])
	
	lifetime = np.ones(N)+4
	lifetime[0:N*killRate] = 1
	np.random.shuffle(lifetime)
	nonstates  = (lifetime == 1)
	truestates = (lifetime == 5)
	
	for time in xrange(totalTime):
		
		#prep
		time += 1
		emissionCounts = 0

		bucket.populate(states, electron, E)
		bucket.populate(states, hole, H)

		#kill some stuff
		indices = (states[nonstates] == 2) & (time % lifetime[nonstates] == 0) 
		states[indices] = 0

		#counts before record
		nEmpty = states[states == empty].size
		nHoles = states[states == hole].size
		nElectrons = states[states == electron].size

		#true emitting states
		nExcitons = states[(states[truestates] == exciton) & (time%lifetime[truestates]==0)].size
		states[(states[truestates] == exciton) & (time%lifetime[truestates]==0)] = 0

		#Records
		stateR = np.vstack((stateR, [time, nEmpty, nElectrons, nHoles, nExcitons]))
		if nExcitons !=0:
			emissionR = np.vstack((emissionR, [nExcitons]))

	stateR = np.delete(stateR, 0, axis=0)
	emissionR = np.delete(emissionR, 0, axis=0)
	bucket.emission(emissionR,test)
	bucket.state(stateR, test)

	s = emissionR.size
	return [np.average(emissionR[s*0.8:s])]