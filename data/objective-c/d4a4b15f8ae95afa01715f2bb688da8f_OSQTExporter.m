//
//  OSQTExporter.m
//  OSQTExporter
//
//  Created by Andy Matuschak on 8/7/05.
//  Copyright 2005 Pixen. All rights reserved.
//

#import "OSQTExporter.h"
#import "OSProgressPopup.h"

#import <QTKit/QTKit.h>	

@implementation OSQTExporter

- (void)dealloc
{
	[qtMovie release];
	[super dealloc];
}

- (void)addImage:(NSImage *)image forLength:(NSTimeInterval)seconds
{
	// Paste the passed image on a white background so transparency doesn't look black.
	NSImage *newImage = [[[NSImage alloc] initWithSize:[image size]] autorelease];
	[newImage lockFocus];
	[[NSColor whiteColor] set];
	NSRectFill(NSMakeRect(0, 0, [image size].width, [image size].height));
	[image compositeToPoint:NSZeroPoint 
								operation:NSCompositeSourceOver];
	[newImage unlockFocus];
	
	if (!qtMovie)
	{
		//FIXME: for some reason, just doing
		//[[alloc] init] and setEditable results in
		//an image to which frames can't be appended. weird!
		qtMovie = [[QTMovie alloc] initToWritableData:[NSMutableData data] error:NULL];
		qtMovie.delegate = self;
	}
	[qtMovie addImage:newImage 
				forDuration:QTMakeTime(seconds * 600, 600) 
		 withAttributes:[NSDictionary dictionaryWithObject:@"png " 
																								forKey:QTAddImageCodecType]];
}

- (void)exportToPath:(NSString *)path parentWindow:(NSWindow *)newParentWindow
{	 
	NSError *error=nil;
	//Is it really OK to export a png-encoded movie? Sure!
//	NSDictionary * attributes = [NSDictionary dictionaryWithObjectsAndKeys:
//															 [NSNumber numberWithBool:YES], QTMovieExport,
//															 [NSNumber numberWithLong:'M4VH'], QTMovieExportType, nil];
	if(![qtMovie writeToFile:path
						withAttributes:nil
										 error:&error]) {
		if(error) {
			[newParentWindow presentError:error];
		}
	}
}

- (BOOL)movie:(QTMovie *)movie 
shouldContinueOperation:(NSString *)op 
		withPhase:(QTMovieOperationPhase)phase 
		atPercent:(NSNumber *)percent 
withAttributes:(NSDictionary *)attributes
{
	OSProgressPopup *pop = [OSProgressPopup sharedProgressPopup];
	switch (phase)
	{
		case QTMovieOperationBeginPhase:
			[pop setCanCancel:YES];
			[pop beginOperationWithStatusText:@"Exporting Movie..." parentWindow:parentWindow];
			break;
		case QTMovieOperationUpdatePercentPhase:
			[[OSProgressPopup sharedProgressPopup] setProgress:[percent doubleValue] * 100];
			break;
		case QTMovieOperationEndPhase:
			[[OSProgressPopup sharedProgressPopup] endOperation];
			break;
	}
	
	// This is such an amazing hack. But it was provided by Apple!
	NSButton *cancelButton = [pop valueForKey:@"cancelButton"];
	NSWindow *popup = [pop valueForKey:@"window"];
	NSEvent *event = 
	[popup nextEventMatchingMask:NSLeftMouseUpMask 
										 untilDate:[NSDate distantPast] 
												inMode:NSDefaultRunLoopMode 
											 dequeue:YES];
	if (event && NSPointInRect([event locationInWindow], [cancelButton frame]))
	{
		[cancelButton performClick:self];
		[self movie:movie 
shouldContinueOperation:nil 
			withPhase:QTMovieOperationEndPhase 
			atPercent:nil 
 withAttributes:nil];
		return NO;
	}
	return YES;
}

@end
