//
//  Sample.m
//  NSOperationSample
//
//  Created by Taisuke Fujita on 11/05/27.
//  Copyright 2011 Colopl, Inc. All rights reserved.
//

#import "Sample.h"


@implementation Sample

- (void)doSomething {
    NSLog(@"%@", @"Test");
}

- (void)dealloc {
     NSLog(@"%@", @"Dealloc!!!");
    [super dealloc];
}

@end
