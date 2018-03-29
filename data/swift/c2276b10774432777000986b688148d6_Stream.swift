//
//  Stream.swift
//  Torrents
//
//  Created by Salazar, Alexandros on 6/10/14.
//  Copyright (c) 2014 nomothetis. All rights reserved.
//

/*
A stream is a data structure that lazily computes its elements on demand. Traversal methods are
defined that allow .
*/
class Stream<T>:Sequence, Generator {
    
    let generator:()->T
    init(theGenerator:() -> T) {
        self.generator = theGenerator
    }
    
    /*
    func map<P>(f:T -> P) -> Stream<P> {
        var memoizedMap = memoizedData.map(f)
        return Stream<P>(theGenerator: { f(self.generator()) }, memo: memoizedMap)
    }
*/
    
    func generate() -> Stream<T>  {
        return self
    }
    
    func next() -> T? {
        return self.generator()
    }
}