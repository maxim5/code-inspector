//
//  Maze.swift
//  Super Maze
//
//  Created by Rafal Grodzinski on 01/07/2014.
//  Copyright (c) 2014 UnalignedByte. All rights reserved.
//

import Foundation
import CoreGraphics


public enum MazeType {
    case Theta
}


public enum MazeGenerationAlgorithm {
    case RecursiveBacktracker
}


@objc public class Maze {
    public let type: MazeType
    public let nodes: [MazeNodePosition : MazeNode]
    public let levelMultipliers: [Int]
    public let levelSize: CGFloat
    public let wallSize: CGFloat
    
    
    public init(thetaWithLevelMultipliers levelMultipliers: [Int], levelSize: CGFloat, wallSize: CGFloat)
    {
        type = .Theta
        self.levelMultipliers = levelMultipliers
        self.levelSize = levelSize
        self.wallSize = wallSize
        
        var nodes: [MazeNodePosition : MazeNode] = [:]
        
        //first create the nodes
        var indexesCount = 1;
        for var level=0; level<levelMultipliers.count; level++ {
            indexesCount *= levelMultipliers[level]
            for var index=0; index<indexesCount; index++ {
                var nodePosition = MazeNodePosition(level: level, index: index)
                var node = MazeNode(position: nodePosition)
                nodes[nodePosition] = node
            }
        }
        
        //now connect all the nodes with potential paths
        indexesCount = 1;
        for var level=0; level<levelMultipliers.count; level++ {
            indexesCount *= levelMultipliers[level];
            for var index=0; index<indexesCount; index++ {
                let node = nodes[MazeNodePosition(level: level, index: index)]
                
                //try to connect with left neighbour
                var leftIndex = index-1;
                if leftIndex < 0 {
                    leftIndex = indexesCount-1
                }
                node!.connectPotentially(nodes[MazeNodePosition(level: level, index: leftIndex)]!)
                
                //try to connect with right neighbour
                var rightIndex = index+1;
                if rightIndex >= indexesCount {
                    rightIndex = 0
                }
                node!.connectPotentially(nodes[MazeNodePosition(level: level, index: rightIndex)]!)
                
                //then connect with nodes at higher level (if there is a higher level
                if level < levelMultipliers.count-1 {
                    let nextLevelMultiplier = levelMultipliers[level+1]

                    for var nextIndex=0; nextIndex<nextLevelMultiplier; nextIndex++ {
                        let anotherNode = nodes[MazeNodePosition(level: level+1, index: nextLevelMultiplier*index + nextIndex)]
                        
                        node!.connectPotentially(nodes[MazeNodePosition(level: level+1, index: index*nextLevelMultiplier + nextIndex)]!)
                    }
                }
            }
        }
        
        self.nodes = nodes
    }
    
    
    public func generateMaze(fromNode node: MazeNode, usingAlgorithm algorithm: MazeGenerationAlgorithm)
    {
        self.generateMazeRecursion(fromNode: node, usingAlgorithm: algorithm)
        
        //add exit
        let indexesCount = self.indexesCountAtLevel(self.levelMultipliers.count-1)
        let node = self.mazeNode(atLevel: self.levelMultipliers.count-1, atIndex: Int(arc4random())%indexesCount)
        node?.hasExit = true
    }

    
    internal func generateMazeRecursion(fromNode node: MazeNode, usingAlgorithm algorithm: MazeGenerationAlgorithm)
    {
        if algorithm == .RecursiveBacktracker {
            var scrambledPotentialNodes: Array<MazeNode> = []
            var nodeToScramble: MazeNode
            for nodeToScramble in node.potentialPaths.values {
                scrambledPotentialNodes.append(nodeToScramble)
            }
            
            for var i=0; i<scrambledPotentialNodes.count; i++ {
                var tempNode = scrambledPotentialNodes[i]
                var randomIndex = Int(arc4random_uniform(UInt32(scrambledPotentialNodes.count)))
                
                scrambledPotentialNodes[i] = scrambledPotentialNodes[randomIndex]
                scrambledPotentialNodes[randomIndex] = tempNode
            }
            
            var potentialNode: MazeNode
            for potentialNode in scrambledPotentialNodes {
                if node.tryConnecting(potentialNode) {
                    generateMazeRecursion(fromNode: potentialNode, usingAlgorithm: algorithm)
                }
            }
        }
    }
    
    
    public func mazeNode(atLevel level: Int, atIndex index: Int) -> MazeNode?
    {
        return self.nodes[MazeNodePosition(level: level, index: index)]
    }
    
    
    @objc public func ousideWallPolygons(atLevel inLevel: Int) -> [Polygon]
    {
        //sanitize input
        var level = inLevel

        if level > self.levelMultipliers.count - 1 {
            level = self.levelMultipliers.count - 1
        }
        
        var polygons = [Polygon]()
        
        let isOutermost: Bool = level == self.levelMultipliers.count-1
        let nextLevelMultiplier = isOutermost ? 1 : self.levelMultipliers[level+1]
        
        //iterate through level's indexes
        for var index = 0; index < self.indexesCountAtLevel(level); index++ {
            let node = self.mazeNode(atLevel: level, atIndex: index)!
            let anglePerNode = 360.0/CGFloat(indexesCountAtLevel(level))
            
            //iterate through next level's indexes
            for var nextIndex = index*nextLevelMultiplier; nextIndex < (index + 1)*nextLevelMultiplier; nextIndex++ {
                
                //first check if we're connected to the next node
                var isConnected: Bool;
                isConnected = false
                if !isOutermost {
                    let nextNode = self.mazeNode(atLevel: level+1, atIndex: nextIndex)!
                    isConnected = node.paths[nextNode.position] != nil
                }
                
                //wall is full
                if !node.hasExit && (isOutermost || !isConnected) {
                    let startAngle = CGFloat(nextIndex) * (anglePerNode / CGFloat(nextLevelMultiplier))
                    let endAngle = (CGFloat(nextIndex) + 1.0) * (anglePerNode / CGFloat(nextLevelMultiplier))
                    
                    let innerRadius = CGFloat(level) * (self.levelSize + self.wallSize) + self.levelSize
                    let outerRadius = CGFloat(level) * (self.levelSize + self.wallSize) + self.wallSize + self.levelSize
                    
                    let innerVerts = Utils.vertices(fromAngle: startAngle, toAngle: endAngle,
                                                    subdivision: self.subdivisionAtLevel(level), radius: innerRadius)
                    let outerVerts = Utils.vertices(fromAngle: startAngle, toAngle: endAngle,
                                                    subdivision: self.subdivisionAtLevel(level), radius: outerRadius)
                    
                    for var i=0; i<innerVerts.count-1; i++ {
                        let poly = Polygon(v0: innerVerts[i], v1: outerVerts[i], v2: outerVerts[i+1], v3: innerVerts[i+1])
                        polygons.append(poly)
                    }
                //wall is empty
                } else {
                    let angle = (CGFloat(nextIndex)+0.5) * (anglePerNode / CGFloat(nextLevelMultiplier))
                    
                    let startAngle = CGFloat(nextIndex) * (anglePerNode / CGFloat(nextLevelMultiplier))
                    let endAngle = CGFloat(nextIndex + 1) * (anglePerNode / CGFloat(nextLevelMultiplier))
                    
                    let innerRadius = CGFloat(level) * (self.levelSize + self.wallSize) + self.levelSize
                    let outerRadius = CGFloat(level) * (self.levelSize + self.wallSize) + self.wallSize + self.levelSize
                    
                    let innerAngleDelta: CGFloat = atan((self.levelSize*0.4)/innerRadius) * 180.0/CGFloat(M_PI)
                    let outerAngleDelta: CGFloat = atan((self.levelSize*0.4)/outerRadius) * 180.0/CGFloat(M_PI)
                    
                    let innerLeftPoints = Utils.vertices(fromAngle: startAngle, toAngle: angle-innerAngleDelta, subdivision: self.subdivisionAtLevel(level), radius: innerRadius)
                    let outerLeftPoints = Utils.vertices(fromAngle: startAngle, toAngle: angle-outerAngleDelta, subdivision: self.subdivisionAtLevel(level), radius: outerRadius)
                    
                    for var i=0; i<innerLeftPoints.count-1; i++ {
                        let poly = Polygon(v0: innerLeftPoints[i], v1: outerLeftPoints[i], v2: outerLeftPoints[i+1], v3: innerLeftPoints[i+1])
                        polygons.append(poly)
                    }
                    
                    let innerRightPoints = Utils.vertices(fromAngle: angle+innerAngleDelta, toAngle: endAngle, subdivision: self.subdivisionAtLevel(level), radius: innerRadius)
                    let outerRightPoints = Utils.vertices(fromAngle: angle+outerAngleDelta, toAngle: endAngle, subdivision: self.subdivisionAtLevel(level), radius: outerRadius)
                    
                    for var i=0; i<innerRightPoints.count-1; i++ {
                        let poly = Polygon(v0: innerRightPoints[i], v1: outerRightPoints[i], v2: outerRightPoints[i+1], v3: innerRightPoints[i+1])
                        polygons.append(poly)
                    }
                }
            }
        }
        
        return polygons
    }
    
    
    @objc public func wallPolygons(atLevel inLevel: Int) -> [Polygon]
    {
        //sanitize input
        var level = inLevel
        
        if level > self.levelMultipliers.count - 1 {
            level = self.levelMultipliers.count - 1
        }
        
        var polygons = [Polygon]()
        
        //iterate through level's indexes
        for var index = 0; index < self.indexesCountAtLevel(level); index++ {
            let node = self.mazeNode(atLevel: level, atIndex: index)!
            let anglePerNode = 360.0/CGFloat(indexesCountAtLevel(level))
            
            var rightIndex = index+1;
            if rightIndex >= self.indexesCountAtLevel(level) {
                rightIndex = 0
            }
            
            if rightIndex == index {
                continue
            }
            
            let rightNode = self.mazeNode(atLevel: level, atIndex: rightIndex)!
            
            if node.paths[rightNode.position] != nil {
                continue
            }
            
            let angle = anglePerNode * CGFloat(rightIndex)
            
            let innerRadius = CGFloat(level) * (self.levelSize + self.wallSize)
            let outerRadius = CGFloat(level) * (self.levelSize + self.wallSize) + self.levelSize

            let innerAngleDelta: CGFloat = atan((self.wallSize*0.5)/innerRadius) * 180.0/CGFloat(M_PI)
            let outerAngleDelta: CGFloat = atan((self.wallSize*0.5)/outerRadius) * 180.0/CGFloat(M_PI)
            
            let innerPoints = Utils.vertices(fromAngle: angle-innerAngleDelta, toAngle: angle+innerAngleDelta, subdivision: self.subdivisionAtLevel(level), radius: innerRadius)
            let outerPoints = Utils.vertices(fromAngle: angle-outerAngleDelta, toAngle: angle+outerAngleDelta, subdivision: self.subdivisionAtLevel(level), radius: outerRadius)
            
            for var i=0; i<innerPoints.count-1; i++ {
                var poly: Polygon?
                if i >= outerPoints.count-1 {
                    poly = Polygon(v0: innerPoints[i], v1: outerPoints[i], v2: outerPoints[i], v3: innerPoints[i+1])
                } else {
                    poly = Polygon(v0: innerPoints[i], v1: outerPoints[i], v2: outerPoints[i+1], v3: innerPoints[i+1])
                }
                polygons.append(poly!)
            }
        }
        
        return polygons
    }
    
    
    public func subdivisionAtLevel(level: Int) -> Int
    {
        return 20
    }
        
        
    public func indexesCountAtLevel(var level: Int) -> Int
    {
        if level > self.levelMultipliers.count - 1 {
            level = self.levelMultipliers.count - 1
        }
        
        
        var indexesCount = 1
        
        for var i=0; i<=level; i++ {
            indexesCount *= self.levelMultipliers[i]
        }
        
        return indexesCount
    }
}