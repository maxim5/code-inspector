class Node
  constructor: (@i, @j, @status) ->

  key: -> "#{@i} - #{@j}"

  getAdjacentNodes: (selector = "FREE") ->
    if selector is "ALL" then return @adjascents
    else
      choices = selector.split " " 
      return (node for node in @adjascents when node.status in choices)

  setAdjacentNodes: (@adjascents) ->

  minDistanceTo: (target) ->
    distX = @i - target.i
    if distX < 0 then distX = 0 - distX
    distY = @j - target.j
    if distY < 0 then distY = 0 - distY
    return distX + distY

  equals: (node) ->
    return if @i == node.i and @j == node.j then true else false

class Board
  constructor: (@statusArray) ->
    @boardArray = []
    @height = statusArray.length
    @width = statusArray[0].length

    for line, i in statusArray
      @boardArray[i]=[]
      for status, j in line
        @boardArray[i][j]=new Node(i,j,status)
        if status == 'START' then @startingNode = @boardArray[i][j]
        if status == 'END' then @endingNode = @boardArray[i][j]


    for i in [0..(@height-1)]
      for j in [0..(@width-1)]
        adjacentNodes = (node for node in [this.getNode(i-1,j), this.getNode(i+1,j), this.getNode(i,j-1), this.getNode(i,j+1)] when node?)
        this.getNode(i,j).setAdjacentNodes(adjacentNodes)

  getNode: (i,j) -> 
    if i >=0 and i < @height and j >=0 and j < @width then return @boardArray[i][j]
    return null

  prettyPrint: ->
    string = ""
    for i in [0..(@height-1)]
      string+="\n"
      for j in [0..(@width-1)]
        switch this.getNode(i,j).status
          when 'START' then string+="B"
          when 'FREE' then string+=" "
          when 'END'  then string+='E'
          else string+='X'
    return string

  key: ->
    string = ""
    for i in [0..(@height-1)]
      for j in [0..(@width-1)]
        switch this.getNode(i,j).status
          when 'START' then string+="B"
          when 'FREE' then string+="F"
          when 'END'  then string+='E'
          else string+='X'
    return string

  advanceStartingNode: (newStart, mode = "ADVANCE") ->
    newStart.status='START'
    if mode == "ADVANCE" then @startingNode.status="VISITED"
    if mode == "ROLLBACK" then @startingNode.status="FREE" 
    @startingNode=newStart


#------------------------------------------------------------
# AStar by Ben Nolan
# http://bennolan.com/2011/04/11/astar-in-coffeescript.html
# Computes the shortest path between 2 nodes
# Usage :
# aStarCalculator = new AStar
# shortestPath = aStarCalculator.findPath(startNode, endNode)
# shortestPath is then an array of nodes or null if none is found
#------------------------------------------------------------
class AStar
  constructor: (@maxHeuristic) ->
    @openNodes = {} # List of openNodes nodes (nodes to be inspected)
    @closedNodes = {} # List of closedNodes nodes (nodes we've already inspected)

  findPath: (start, destination) ->
    # g = 0 #Cost from start to current node
    # h = heuristic(start, destination) #Cost from current node to destination
    # var f = g+h #Cost from start to destination going through the current node

    start.f = @heuristic(start, destination)

    # Push the start node onto the list of openNodes nodes
    # openNodes.push(start) 
    @openNodes[start.key()] = start

    #Keep going while there's nodes in our openNodes list
    while @openNodes
      
      #console.log @openNodes
      #Find the best openNodes node (lowest f value)

      #Alternately, you could simply keep the openNodes list sorted by f value lowest to highest,
      #in which case you always use the first node

      node = { f : Infinity }

      for key, n of @openNodes
        if n.f < node.f
          node = n

      # No nodes remain in openNodes
      if node.f == Infinity
        # No path could be found...
        #console.log "No path could be found"
        return null
        # console.log @closedNodes

      # Check if we've reached our destination
      if node.equals(destination)
        path = [destination]

        while (node != start) # && (node.parentKey)
          node = @closedNodes[node.parentKey]
          path.push node

        path.reverse()

        return path

      # Remove the current node from our openNodes list
      delete @openNodes[node.key()]

      # Push it onto the closedNodes list
      @closedNodes[node.key()] = node

      # Expand our current node
      for n in node.getAdjacentNodes('FREE END') when (!@closedNodes[n.key()]) && (!@openNodes[n.key()]) 
        # console.log(n.key())
        n.f = @heuristic(n, destination)
        n.parentKey = node.key()

        @openNodes[n.key()] = n

        # Prevent really long paths
        ###
        if n.f < @maxHeuristic
          @openNodes[n.key()] = n
        ###
        # else 
        #   @closedNodes[n.key()] = n

  # An A* heurisitic must be admissible, meaning it must never overestimate the
  # distance to the goal. In other words, it must either underestimate or return 
  # exactly the distance to the goal.
  heuristic: (a, b) ->
    a.minDistanceTo(b)


#------------------------------------------------------------
# ResultKeeper : simplest key/value store ever
#------------------------------------------------------------

class ResultKeeper
  constructor: ->
    @store = []

  keep: (key, value) ->
    @store[key] = value

  get: (key) ->
    return @store[key]


#------------------------------------------------------------
# Starting of the Quora resolution algorithm
#------------------------------------------------------------


steps = 0
resultKeeper = new ResultKeeper

getQuoraScore= (board) ->
  if isQuoraSolution(board) then return 1;
  if resultKeeper.get(board.key())? then return resultKeeper.get(board.key())
  if isQuoraImpossible(board) then return 0;

  steps = steps+1

  score = 0

  # If one of the adjascent nodes has only 1 exit, we have to enter
  # it and we cant go into any other node
  forcedEntry = false
  for newStart in board.startingNode.getAdjacentNodes('FREE')
    if newStart.getAdjacentNodes('FREE END').length == 1
      curStart = board.startingNode
      board.advanceStartingNode(newStart)
      score = score+getQuoraScore(board)
      board.advanceStartingNode(curStart,'ROLLBACK')
      forcedEntry = true
      break

  if not forcedEntry
    for newStart in board.startingNode.getAdjacentNodes('FREE')
      curStart = board.startingNode
      board.advanceStartingNode(newStart)
      score = score+getQuoraScore(board)
      board.advanceStartingNode(curStart,'ROLLBACK')


  resultKeeper.keep(board.key(), score)
  return score

isQuoraSolution= (board) ->
  for i in [0..(board.height-1)]
      for j in [0..(board.width-1)]
        if board.getNode(i,j).status == 'FREE' then return false

  if board.startingNode.getAdjacentNodes('END').length == 1 then return true
  else return false

isQuoraImpossible= (board) ->
  if board.startingNode.getAdjacentNodes()?.length == 0 then return true
  
  # Apply A* algorithm to all adjascent nodes, to check that each of them can still
  # reach the exit.
  for node in board.startingNode.getAdjacentNodes('FREE')
    pathToEnd = new AStar(board.width * board.heigth).findPath(node, board.endingNode)
    if not pathToEnd? then return true
  
  return false


#-----------------------------
# This is where the fun begins
#-----------------------------
myBoardArray=[["START", "FREE", "FREE", "FREE", "FREE", "FREE", "FREE"], 
["FREE", "FREE", "FREE", "FREE", "FREE", "FREE", "FREE"],
["FREE", "FREE", "FREE", "FREE", "FREE", "FREE", "FREE"],
["FREE", "FREE", "FREE", "FREE", "FREE", "FREE", "FREE"],
["FREE", "FREE", "FREE", "FREE", "FREE", "FREE", "FREE"],
["FREE", "FREE", "FREE", "FREE", "FREE", "FREE", "FREE"],
["FREE", "FREE", "FREE", "FREE", "FREE", "FREE", "FREE"],
["END", "FREE", "FREE", "FREE", "FREE", "CLOSED", "CLOSED"]]
firstBoard = new Board(myBoardArray)
time1 = new Date().getTime()
score = getQuoraScore(firstBoard)
time2 = new Date().getTime()

durationMS = (time2 - time1)
durationS = durationMS /1000
console.log score+" solutions in "+steps+" steps computed in "+durationS+" seconds ("+(durationMS / steps)+"ms / step)" 