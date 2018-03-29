//
//  Stream.swift
//  MiniPlatform
//
//  Created by Robert Diamond on 6/21/14.
//  Copyright (c) 2014 Robert Diamond. All rights reserved.
//

import Foundation

typealias CompletionFunction = (results : AnyObject?, error : NSError?) -> Void
class Stream : NSObject {
    let STREAM_ERROR_DOMAIN = "stream"
    var streamObjects : Array<ModelObject>?
    var error : NSError?
    var api : TwitterAPI?

    init()
    {
        streamObjects = Array<ModelObject>()
        super.init()
    }

    /**
     * Returns the smallest ID in streamObjects
     */
    func minID() -> Int64?
    {
        return streamObjects?.reduce(nil) {
            (minValue : Int64?, element) in
            return (!minValue.getLogicValue() || (element.ID < minValue! && element.ID >= 0)) ? element.ID : minValue
        }
    }

    func maxID() -> Int64?
    {
        return streamObjects?.reduce(nil) {
            (maxValue : Int64?, element) in
            return (!maxValue.getLogicValue() || element.ID > maxValue!) ? element.ID : maxValue
        }
    }

    func load(#minID : Int64?, maxID : Int64?, completion : CompletionFunction)
    {
        // does nothing, implemented in subclasses
    }

    func loadTop(completion : CompletionFunction)
    {
        self.load(minID: self.maxID(), maxID: nil, completion: completion)
    }

    func loadBottom(completion : CompletionFunction)
    {
        var min_id = self.minID()
        min_id = (min_id) ? min_id! - 1 : min_id
        self.load(minID: nil, maxID: min_id, completion: completion)
    }

    func integrateItems(newItems : [ModelObject]?)
    {
        if let items = newItems {
            var integrated = (self.streamObjects) ? self.streamObjects! : [ModelObject]()
            integrated = integrated.filter() {
                (item) in
                return item.ID >= 0
            }
            integrated.extend(items)
            integrated.sort() {
                (a,b) in
                return a.ID > b.ID
            }
            self.streamObjects = integrated
        }
    }
}