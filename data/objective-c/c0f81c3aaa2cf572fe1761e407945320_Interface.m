//
//  Interface.m
//  Event Horizon
//
//  Created by Paul Dorman on Sun Jun 27 2004.
//  Copyright (c) 2004 Paul Dorman. All rights reserved.
//

#import <Carbon/Carbon.h>

#import "Interface.h"
#import "Controller.h"

extern void DoUpdate();
EventLoopTimerUPP GetTimerUPP();
extern void initGL();

extern NSWindow *window;
extern EventLoopTimerRef gTimer;
extern BOOL toggleFull;
extern Rect gRectPort;
extern AGLContext gaglContext;

@implementation Interface

- init{
	self = [super init];
	timer = [NSTimer scheduledTimerWithTimeInterval:0 target:self selector:@selector(timer) userInfo:nil repeats:YES];
	[NSBundle loadNibNamed:@"MainMenu.nib" owner:self];
	return self;
}

- (void) timer{
	DoUpdate();
}

- (void) terminate:(id)sender{
	[NSApp terminate:sender];
}

- (void) hideOtherApplications:(id)sender{
	[NSApp hideOtherApplications:sender];
}

- (void) hide:(id)sender{
	[NSApp hide:sender];
}

- (void) unhideAllApplications:(id)sender{
	[NSApp unhideAllApplications:sender];
}

- (void) orderFrontStandardAboutPanel:(id)sender{
	[NSApp orderFrontStandardAboutPanel:sender];
}

- (void) fullScreen:(id)sender{
	toggleFull = YES;
}

- (void) changeResolution:(id)sender{
	NSArray *res = [[sender title] componentsSeparatedByString:@"x"];
	int x = [[res objectAtIndex:0] intValue];
	int y = [[res objectAtIndex:1] intValue];
	void *ref;

	[window setContentSize:NSMakeSize(x,y)];
	ref = [window windowRef];
	GetWindowPortBounds( ref, &gRectPort );
	aglSetDrawable( gaglContext, GetWindowPort(ref) );
	initGL();
	sys->screenWidth = gRectPort.right - gRectPort.left;
	sys->screenHeight = gRectPort.bottom - gRectPort.top;
}

- (void) applicationDidBecomeActive:(NSNotification *)n{
/*	if( sys ){
		DoUpdate();
		sys->pause = NO;
		timer = [NSTimer scheduledTimerWithTimeInterval:0 target:self selector:@selector(timer) userInfo:nil repeats:YES];
		InstallEventLoopTimer (GetCurrentEventLoop(), 0, 0.0001, GetTimerUPP(), 0, &gTimer);	
		[window makeKeyAndOrderFront:self];
		[NSApp updateWindows];
	}*/
}

- (void) applicationDidResignActive:(NSNotification *)n{
/*	if( sys ){
		sys->pause = YES;
		DoUpdate();
		[timer invalidate];
		RemoveEventLoopTimer( gTimer );
	}*/
}

- (void) mouseDown:(NSEvent *)event{
	// don't beep
}

@end
