//
//  Region.m
//  CraigsList
//
//  Created by Mathieu Van der Haegen on 09/07/09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import "Region.h"


@implementation Region

@synthesize name;
@synthesize locations;

-(id) initWithName: (NSString*) thename {
	if (self = [super init]) {
		self.name = thename;
	}
	return self;
}

-(id) initWithName: (NSString*) thename andLocations:(NSArray*) thelocations {
	if (self = [super init]) {
		self.name = thename;
		self.locations = thelocations;
	}
	return self;
}

- (void)dealloc {
    [super dealloc];
}


@end
