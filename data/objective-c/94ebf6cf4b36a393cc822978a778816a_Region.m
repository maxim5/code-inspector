//
//  Region.m
//  rasp
//
//  Created by Jelle Vandebeeck on 29/03/11.
//  Copyright 2011 10to1. All rights reserved.
//

#import "Region.h"

#import "Country.h"

@implementation Region

@synthesize name, countries;

#pragma mark - Initializer

- (id)initWithCountries:(NSArray *)someCountries {
    if (!(self = [super init])) return nil;
    
    self.countries = [NSMutableArray array];
    for (NSString *countryName in someCountries) {
        Country *country = [[Country alloc] init];
        country.name = countryName;
        [self.countries addObject:country];
        [country release];
    }
    
    return self;
}

- (void)dealloc {
    self.name = nil;
    self.countries = nil;
    
    [super dealloc];
}

@end
