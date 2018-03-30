//
//  Call.m
//  OAuthConsumer
//
//  Created by Ribbit Corporation on 5/7/10.
//  Copyright 2010 Ribbit Corporation. All rights reserved.
//

#import "Call.h"
#import "SBJSON.h"
#import "SignedRequest.h"
#import "OAServiceTicket.h"


@implementation Call

@synthesize legs;
@synthesize callURI;
@synthesize callID;
@synthesize callerId;
@synthesize mode;
@synthesize announce;
@synthesize startTime;
@synthesize endTime;
@synthesize duration;
@synthesize success;
@synthesize active;
@synthesize recording;
@synthesize outbound;
@synthesize recordings;

-(id)initWithConfig:(RibbitConfig*)ribbitConfig {
	self = [super init];
	self.config = ribbitConfig;
	NSLog(@"self.config = %@", self.config);
	self.legs = [[NSMutableArray alloc] initWithObjects:nil];
	self.recordings = [[NSMutableArray alloc] initWithObjects:nil];
	
	return self;
}

-(id)initWithDictionary:(NSDictionary*)dictionary ribbitConfig:(RibbitConfig*)ribbitconfig {
	[super init];
	self.config = ribbitconfig;
	callID = [dictionary objectForKey:@"id"];
    duration = [dictionary objectForKey:@"duration"];
    startTime = [dictionary objectForKey:@"startTime"];
    endTime = [dictionary objectForKey:@"endTime"];
    success = (BOOL*)[dictionary objectForKey:@"success"];
    active = (BOOL*)[dictionary objectForKey:@"active"];
    recording = (BOOL*)[dictionary objectForKey:@"recording"];
	outbound = (BOOL*)[dictionary objectForKey:@"outbound"];

	self.legs = [[NSMutableArray alloc] initWithObjects:nil];
	self.recordings = [[NSMutableArray alloc] initWithObjects:nil];
	
	NSArray *legsArray = [[[NSArray alloc]init] arrayByAddingObjectsFromArray:[dictionary objectForKey:@"legs"]];
	int i;
	for (i=0; i < [legsArray count]; i++) {
		NSDictionary *legDict = (NSDictionary*)[legsArray objectAtIndex:i];
		// parse multileg object here
		CallLeg *callLeg = [[CallLeg alloc] init];
		callLeg.callLegId = [legDict objectForKey:@"id"];
		callLeg.startTime = [legDict objectForKey:@"startTime"];
		callLeg.endTime = [legDict objectForKey:@"startTime"];
		callLeg.duration = [legDict objectForKey:@"startTime"];
		callLeg.playing = [legDict objectForKey:@"startTime"];
		callLeg.recording = [legDict objectForKey:@"startTime"];
		callLeg.status = [legDict objectForKey:@"startTime"];
		[legs addObject:callLeg];
		[legDict release];
	}
	
	NSArray *recordingsArray = [[[NSArray alloc] init  ]arrayByAddingObjectsFromArray:[dictionary objectForKey:@"recordings"]];
	for (i=0; i < [recordingsArray count]; i++) {
		NSString *recording = [recordingsArray objectAtIndex:i];
		[recordings addObject:recording];
		[recording release];
	}
	[dictionary release];
	return self;
}

-(void)addLeg:(CallLeg*)leg {
	NSLog(@"here %@", [leg callLegId]);
	
	[legs addObject:leg];
	NSLog(@"here legs = %@", legs);
//	NSLog(@"here counts %@", [legs count]);
}

-(void)startCall {
	if (config.accountId == NULL) {
		// raise exception here, TODO figure out exact format
	}
	NSMutableArray *ids;
	NSMutableDictionary *dict = [[NSMutableDictionary alloc] init];
	@try {
		// collect ids
		ids = [[NSMutableArray alloc] init];
		int i;
		for(i = 0; i < [legs count]; i++) {
			CallLeg *leg = [legs objectAtIndex:i];
			NSLog(@"leg %@", leg.callLegId);
			[ids addObject:leg.callLegId];
		}

		[dict setObject:ids forKey:@"legs"];

		if (callerId != nil && [callerId length] > 0) {
			[dict setObject:callerId forKey:@"callerId"];
		}
		if (callerId != nil && [callerId length] > 0) {
			[dict setObject:mode forKey:@"mode"];
		}
		if (announce != nil && [announce length] > 0) {
			[dict setObject:announce forKey:@"announce"];
		}
	}
	@catch (NSException * e) {
		//
		NSLog(@"error = %@", e);
	}
	@finally {
		//
	}
	
	NSString *url = [[config.endpoint stringByAppendingString:@"calls/"] stringByAppendingString: config.accountId];

	[dict setObject:url forKey:@"url"];
	NSError *jsonerror;
	SBJSON *json = [SBJSON new];
	
	NSString *body = [json stringWithObject:dict error:&jsonerror];
	[dict setObject:body forKey:@"json"];
	
	
	SignedRequest *signedRequest = [[SignedRequest alloc] initWithConfig:config];
	[signedRequest httpRequestWithDictionary:dict];
	// Figure out why it doesn't wait for the response
	
	NSArray *chunks = [response componentsSeparatedByString: @"/"];
	callID = [chunks objectAtIndex:[chunks count] - 1 ];
	callURI = response;
}

-(void)updateCallWithDictionary:(NSMutableDictionary *)dictionary {
	if (config.accountId == NULL) {
		// raise exception here, TODO figure out exact format
	}

	NSMutableString *url = [[NSMutableString alloc]init];
	[url appendString:[config.endpoint stringByAppendingString:@"calls/"] stringByAppendingString: config.accountId];
	[url appendString:@"/"];
	[url appendString:[dictionary objectForKey:@"callID"]];
	
	
	SignedRequest *signedRequest = [[SignedRequest alloc] initWithConfig:config];
	[dictionary setObject:@"PUT" forKey:@"method"];
	[dictionary setObject:url forKey:@"url"];
	
	NSError *jsonerror;
	SBJSON *json = [SBJSON new];
	
	NSString *body = [json stringWithObject:dictionary error:&jsonerror];
	[dictionary setObject:body forKey:@"json"];
	
	
	[signedRequest httpRequestWithDictionary:dictionary];
}

-(void)dropCall {
	NSMutableDictionary *dict = [[NSMutableDictionary alloc] init];
	[dict setObject:@"true" forKey:@"active"];
	[self updateCallWithDictionary:dict];
}

@end
