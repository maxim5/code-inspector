## FILE: hashmid-algo.coffee

# Implementation of the algorithm described in the "Algorithm" panel of this poster:
#  http://www.stanford.edu/~jonr1/software-faire-poster.jpg
#
# inputs = globals
hashmidAlgo = () ->
  dispLine 3, "Computing..."
  
  ## THE FUN STARTS HERE:
  lo = 1
  hi = noCapYet = -1
  inc = 10
  
  loop
    ## Ray Binary Search (CS161)
    if hi != noCapYet and hi <= lo
      break
    
    if hi == noCapYet
      winWidth = lo + inc
    else
      winWidth = Math.ceil( (lo + hi) / 2 )
    
    if winWidth >= g_plasmidLen
      hi = g_plasmidLen - 1
      continue
    
    dispLine 4, ("lo " + lo + ", hi " + hi + ", winWidth " + winWidth)
    dispLine 3, ("Are there repeats of length " + winWidth + "?\n")
    
    ## PART ZERO. initialize
    # zero out the hashtable
    g_hashTable.zeroOut()
    
    
    ## PART ONE. first pass
    # find which buckets are double-tapped (baseline is around 20-50 doubled buckets, just due to hash collisions)
    for i in g_allPositions
      sub = subSeqForPos(i, winWidth)
      g_hashTable.push i, sub
    
    shortList = []
    
    for i in [0 ... g_constants.nHashTableBuckets]
      nEl = g_hashTable.getBucket(i).length
      if nEl >= 2 then shortList.push i
    
    
    ## PART TWO. examine the buckets for repeats
    foundAnyRepeat = false
    
    
    for i in [0 ... g_constants.nHashTableBuckets]
      if foundAnyRepeat then break
      
      listOfPositions = g_hashTable.getBucket(i)    
      if listOfPositions.length >= 2
        
        for j in [0 ... listOfPositions.length]
          if foundAnyRepeat then break
          
          for k in [0 ... listOfPositions.length]
            if foundAnyRepeat then break
            
            if k > j  # don't count identities, don't count twice
              subSeqJ = subSeqForPos(listOfPositions[j], winWidth)
              subSeqK = subSeqForPos(listOfPositions[k], winWidth)
              
              if subSeqJ == subSeqK
                dispLine 3, ("Len " + winWidth + " Repeat found!")
                dispLine 1, describeRange(listOfPositions[j], winWidth)
                dispLine 2, describeRange(listOfPositions[k], winWidth)
                dispLine 0, (subSeqJ + " (length " + winWidth + (if winWidth < g_constants.maxRepeatLenToLookFor then ")" else " or more)"))
                
                foundAnyRepeat = true
                longestRepeat = subSeqJ
                longestRepeatPos1 = listOfPositions[j]
                longestRepeatPos2 = listOfPositions[k]
    
    
    ## Ray Binary Search (CS161)
    if foundAnyRepeat
      lo = winWidth
      if lo > g_constants.maxRepeatLenToLookFor then break
    else
      dispLine 3, ("No length " + winWidth + " repeats.\n")
      hi = winWidth - 1
  
      
  dispLine 3, ""
  dispLine 4, ""





