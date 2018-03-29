//
//  Data.swift
//  EMIApp
//
//  Created by Alex on 16/10/15.
//  Copyright ÂŠ 2015 Alex Steiner. All rights reserved.
//

import Foundation
import CoreLocation

class EAData {
    let location:CLLocationCoordinate2D
    let name:String
    let value:Double
    let type:EADataType
    
    init(type:EADataType, location:CLLocationCoordinate2D, name:String, value:Double) {
        self.type = type
        self.location = location
        self.name = name
        self.value = value
    }
}