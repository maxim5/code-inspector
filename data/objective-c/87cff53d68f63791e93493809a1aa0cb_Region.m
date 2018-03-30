//
//  Region.m
//  ZhaoPin_WWZ
//
//  Created by Ibokan on 13-2-19.
//  Copyright (c) 2013ĺš´ Ibokan. All rights reserved.
//

#import "Region.h"

@implementation Region
@synthesize code = _code,text = _text,city = _city;

-(id)init
{
    if(self = [super init]){
        self.city = [[NSMutableArray alloc] init];
    }
    return  self;
}

-(void)dealloc
{
    [_city release],_city = nil;
    [_code release],_code = nil;
    [_text release],_text = nil;
    [super dealloc];
}

@end
