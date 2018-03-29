import logging
import random
from utils import *
from model import *

# This program calculate the degrees of difficulty and degree of proficiency as well as the consistencies of the quiz items and the quiz takers.
# Note: Difficulty, Proficiency are normalized to range from 0 to range_max = 10,000
# Fuzzy logic is used ranging from 0 to range_max. 0 = false range_max = true. intermediate values are allowed.

def algorithm()


	# DEFINE CONSTANTS
	
	converge_ratio = 100	# Used to determine termination criterion. At first count, converge_min = converge / converge_ratio
	count_max = 200			# Maximum number of times the whole score set is scanned
	damping = 1             # value 1 is fastest convergence assuming no nonlinearities. Larger values slow convergence but deal with non-linearities.
	range_max = 10000 		# maximum range for difficulty and proficiency
	range_half = range_max / 2 # half of range_max. Used in initialization
	noise_amp = 0           # noise amplitude for simulated anealing

	
	# DEFINE VARIABLES
	
	converge = 0			# Measure of convergence, accumulation of residuals during a cycle.
	converge_min = 0		# minimum convergence value at termination of algorithm. Is automatically set in program converge_min = converge / converge_ratio
	count = 0				# Number of of times the whole score set is scanned
	residual = 0            # Difference between measured score and expected score
	gain = 0                # gain to be applied to residual
	annealing = 0           # noise to be applied to residual to achieve simulated annealing, finding global optimum
	nudge_difficulty = 0    # amount of nudge allocated to difficulty
	nudge_proficiency = 0   # amount of nudge allocated to proficiency
	temp = 0                # temporary buffer
	last_cycle = False      # last cycle flag is used to terminate optimization and to store the last residuals in takers and items
	item_variance = 0       # temporary holds intermediate variance calculations
	taker_variance = 0      # temporary holds intermediate variance calculations
	expected_score = 0      # expected score by quiz taker given the item difficulty and the taker's proficiency
	count = 0				# holds the numbe oftimes the whole score set is passed through the Falman filter.
    last_cycle = False		# used to save data on thedatabase on the last cycle through the Kalman filter.
   
    
    # DATABASE REQUIREMENTS

    	# score_list
    	# 	class score_item
        # 		scores.score		# Score by a quiz taker taking a quiz item
        # 		score.residual 
        # 		score.trained

        # quiz_item_list
        # 	class quiz_item
        # 		quiz_item.difficulty
        # 		quiz_item.mean
        # 		quiz_item.variance
        # 		quiz_item.manhattan
        # 		quiz_item.nudges_count
        # 		quiz_item.trained			# True if item has gone through an optimization. Used to freeze difficulty when new data is presented
        
        # quiz_taker_list
        # 	class quiz_taker
        # 		quiz_taker.proficiency               
        # 		quiz_taker.mean
        # 		quiz_taker.variance
        # 		quiz_taker.manhattan
        # 		quiz_taker.nudges_count
        # 		quiz_taker.trained		# True if item has gone through an optimization. Used to freeze proficiency when new data is presented    
        
     
        # SELECT scores with score.trained = False. # This is new data
        
	
    # INPUT AND INITIALIZE SCORE DATA    

        import string
        query = db.GqlQuery("SELECT * FROM ItemScore") 	        # if possible SELECT scores with score.trained = False. # This is new data
        scores = query.fetch(*)
        scores_number = len(scores)
        for score in scores
        	self.residual = 0
        	self.trained = False	# REMOVE THIS INITIALIZATION AFTER TRAINING FIRST BATCH
        
        		
     #INPUT AND INITIALIZE QUIZ_ITEM DATA
        
  
     query = db.GqlQuery("SELECT * FROM QuizItem") 	        # if possible SELECT scores with score.trained = False. # This is new data
        quiz_items = query.fetch(*) 
        item_number = len(quiz_items)
        for quiz_item in quiz_items
        	self.difficulty = range_half
        	self.mean = 0
        	self.variance = 0
        	self.manhattan = 0
        	self.nudges_count = 0
        	self.trained = False 	# REMOVE THIS INITIALIZATION AFTER TRAINING FIRST BATCH
       
        	 	
	#INPUT AND INITIALIZE QUIZ_TAKER DATA        	
        	
        query = db.GqlQuery("SELECT * FROM QuizTaker")
        quiz_takers = query.fetch(*)
        taker_number = len(quiz_takers)
        for quiz_taker in quiz_takers
        	self.proficiency = range_half
        	self.mean = 0
        	self.variance = 0
        	self.manhattan = 0
        	self.nudges_count = 0
        	self.trained = False	# REMOVE THIS INITIALIZATION AFTER TRAINING FIRST BATCH
        	
        	     
       # scores.quiz_taker.proficiency
        
       # Essentially, given a score I want to get the proficiency of the taker.
       # The first table is scores. This table has an entry called quiz_taker
       # The second table is quiz_takers. This table has an entry called proficiency.
       # How do I thread from one to the other? How about:
       # quiz_takers.(scores.quiz_taker).proficiency
        

        converge = converge_min + 1

        # Optimization Main Iterative Loop            
	while ( last_cycle == False ):
		last_cycle =  ( converge < converge_min ) or ( count >= count_max ) # Set last cycle flag on last cycle

		converge = 0	        # Initialize convergence for quiz items

		# SHUFFLE??
		# Need to shuffle score list (or index list pointing to score list) every cycle to achieve unbiased convergence.
		# Use the function shuffle( seq[, random])
		# If computing cost of shuffle is too high apply shuffle every 10 times i.e., if ((count % 10) = 0): shuffle( seq[, random]) # modulus operator: %

		# Reset all nudge counts for the next cycle
		for item in items
			quiz_item.nudges_count = 0         			# set first nudge count to 0.

		for taker in takers
			quiz_taker.nudges_count = 0            		# set first nudge count to 0.

           
		for score in scores: 	                        # Scan all measured scores

			item_index = scores(score).quiz_item 		# Retrieve index of quiz_item
			taker_index = scores(score).quiz_taker  	# Retrieve index of quiz_taker
		               
			expected_score = (quiz_takers[ taker_index ].proficiency - quiz_items[ item_index ].difficulty + range_max)/2 # Fuzzy Expected Answer ranges (0,10000)			
			residual = scores.score  expected_score 	# Residual or Innovation; +ive Residual means performance better than expected
                                                        # Residual ranges +10000, -10000
			converge = max(1073741824, converge + abs(residual)) 	# Convergence monitor. Saturates at 2**30. Max at 2**32
			gain = damping/(damping + count)			# Gain decrease on each successive pass through the scores to iron out the discrepancies. 
			annealing = noise_amp * uniform(0, ((count_max - count)/count_max) * range_max/10) 	# annealing is a random number ramping down from range_max/10

			if (quiz_item[item_index].trained == False ):
				if (quiz_taker[taker_index].trained == False ):
					quiz_item[item_index].nudges_count += 1                				# accumulate number of nudges.
					quiz_taker[taker_index].nudges_count += 1                           # accumulate number of nudges. 
					# Allocate nudge to difficulty and proficiency
					# Note: nudge_count + 1 insures that on the first cycle the nudge is distributed equally 50/50 to diff and prof.
					# Note: quiz_item.nudges_count is a measure of the variance of quiz_item during the optimization process and allows weighing of new information. 
					# Note: quiz_taker.nudges_count is a measure of the variance of quiz_taker during the optimization process  and allows weighing of new information.
					nudge_difficulty = gain * ( residual  + annealing ) / (quiz_item[item_index].nudges_count + 1) # Compute nudge allocation equally between diff and prof.
					nudge_proficiency = gain * ( residual  + annealing ) / (quiz_taker[taker_index].nudges_count + 1) # ompute nudge allocation equally between diff and prof.

					# Update Difficulty
					temp = quiz_item[item_index].difficulty - nudge_difficulty          # Apply correction to difficuly.
					quiz_item[item_index].difficulty = max(range_max, min(0, temp))     # Restrict range (0,10000)
   
					# Update Proficiency
					temp = quiz_taker[index_taker].proficiency + nudge_proficiency      # Apply correction to difficuly.
					quiz_taker[index_taker].proficiency = max(range_max, min(0, temp))	# Restrict range (0,10000)
				
				else:																	# quiz_taker[taker_index].trained == True 
					quiz_item[item_index].nudges_count += 1                             # accumulate number of nudges.
					# Calculate nudge allocation to difficulty and proficiency
					# Note: nudge_count begin at 1. This insures that on the first cycle the nudge is distributed wholly to diff.
					nudge_difficulty = gain * ( residual  + annealing ) / (quiz_item[item_index].nudges_count) # Allocates nudge to Diff. 
					nudge_proficiency = 0

					# Update Difficulty
					temp = quiz_item[item_index].difficulty - nudge_difficulty          # Apply correction to difficuly.
					quiz_item[item_index].difficulty = max(range_max, min(0, temp))	# Restrict range (0,10000)

			else:
				if (score.quiz_taker.trained == False ):
                                    
					quiz_taker[taker_index].nudges_count += 1                                  # accumulate number of nudges.
					# Note: nudge_count begin at 1. This insures that on the first cycle the nudge is distributed wholly to prof.
					nudge_difficulty = 0
					nudge_proficiency = gain * ( residual  + annealing ) / (quiz_taker[taker_index].nudges_count) # Allocates nudge to diff.

					# Update Proficiency
					temp = quiz_taker[taker_index].proficiency + nudge_proficiency             # Apply correction to difficuly.
					quiz_taker[taker_index].proficiency = max(range_max, min(0, temp))	        # Restrict range (0,10000)
                                    
				else:
					nudge_difficulty =  0
					nudge_proficiency = 0                           # Both item and taker are trained. No update necessay


			# Collect Info for monitoring purposes
			if last_cycle: 

				score.residual = residual               # for monitoring, accumulate in residual of quiz item all relevent residuals

				# compute means for residuals associated with quiz items and quiz takers.
				quiz_item[item_index].mean += (residual - quiz_item[item_index].mean) / quiz_item[item_index].nudges_count
				quiz_taker[taker_index].mean += (residual - quiz_taker[taker_index].mean) / quiz_taker[taker_index].nudges_count

				#compute manhattan
				quiz_item[item_index].manhattan += abs(residual)      # accumulate absolute value of residuals
				quiz_taker[taker_index].manhattan += abs(residual)     # accumulate absolute value of residuals

				quiz_item[item_index].trained = True
				quiz_taker[taker_index].trained = True
				score.trained = True     			# increment score usage. Used in sorting scores before processing. Use youngest scores first


				


		if last_cycle:        
                                
			#compute variances. Variances have already been initialized to zero.                                           
			for score in scores
				quiz_item[item_index].variance += (score.residual - quiz_item[item_index].mean)**2
				quiz_taker[taker_index].variance += (score.residual - quiz_taker[taker_index].mean)**2

			quiz_item[item_index].variance = quiz_item[item_index].variance /( quiz_item[item_index].nudges_count -1)
			quiz_taker[taker_index].variance = quiz_taker[taker_index].variance /( quiz_taker[taker_index].nudges_count -1)                        
                                
		if count = 0:
			converge_min = max( 1, (converge / converge_ratio) # set converge_min to a fraction of first converge value.
		count += 1 	# increment loop count. Used in terminating the optimization porcess
                                            

		# Put back in the database the following

		# score.quiz_item.difficulty = 0          # Difficulty of a Quiz item for a given score
		# score.quiz_item.trained = False         # True if item has gone through an optimization. Used to freeze difficulty when new data is presented
		# score.quiz_item.consistency = 0            # to be put back into the DB. This is a measure of consistency. Low value is most consistent

		# score.quiz_taker.proficiency            # Proficiency of a Quiz Taker of a given score
		# score.quiz_taker.trained                # True if item has gone through an optimization. Used to freeze proficiency when new data is presented 
		# score.quiz_taker.consistency            # to be put back into the DB. This is a measure of consistency. Low value is most consistent
 
        #
# end of algorithm


# Following program is needed to monitor the performance of the optimization program
def monitor()
# Requirements:
# As algorithm runs:
# display "converge"

# At the end of the algorithm (using last_cycle flag):
# Display means, variances and manhattans, for each quiz item and for each quiz taker.
# Display should be in sorted fashion, in columns of pairs, for example, (quiz_taker, mean) or (quiz_item, manhattan)
# with highest values on top of the sort
#
                                            
#


                                           