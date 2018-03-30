//
//  LibraryMapViewController.m
//  UCIMobile
//
//  Created by Yoon Lee on 6/20/10.
//  Copyright 2010 leeyc. All rights reserved.
//

#import "LibraryMapViewController.h"


@implementation LibraryMapViewController
@synthesize book, direction, direction2;

@synthesize webViews, mapView, segment;

- (id)initWithPageNumber:(int)page 
{
	
	if (page==1) 
	{
		if (self == [super initWithNibName:@"LibraryMapViewController" bundle:nil]) 
		{
			pageNumber = page;
		}
	}
	
    return self;
}

- (void)viewDidLoad 
{
	direction.numberOfLines = 0;
	self.mapView.layer.cornerRadius = 15.0;
	self.webViews.layer.cornerRadius = 15.0;
	self.view.layer.borderWidth = 2;
	self.view.layer.borderColor = [RGB(197, 204, 212) CGColor];
	self.mapView.layer.borderWidth = 2;
	self.mapView.layer.borderColor = [RGB(197, 204, 212) CGColor];
	self.webViews.layer.borderWidth = 2;
	self.webViews.layer.borderColor = [RGB(197, 204, 212) CGColor];
	NSBundle *mainBundle = [NSBundle mainBundle];	
	selectedSound =  [[SoundEffect alloc] initWithContentsOfFile:[mainBundle pathForResource:@"noise" ofType:@"wav"]];
	
	[self locateMELibrary];
}

- (void)didReceiveMemoryWarning 
{
    // Releases the view if it doesn't have a superview.
    [super didReceiveMemoryWarning];
    
    // Release any cached data, images, etc that aren't in use.
}

- (void)viewDidUnload 
{
    [super viewDidUnload];
    // Release any retained subviews of the main view.
    // e.g. self.myOutlet = nil;
}

- (BOOL) locateMELibrary 
{
	NSString *libraryLocation = book.location;
	BOOL atLeastLocationFound = NO;
	
	NSRange range = [libraryLocation rangeOfString:@"Langson"];
	[[UIApplication sharedApplication] setNetworkActivityIndicatorVisible:YES];
	if(range.location != NSNotFound) 
	{
		atLeastLocationFound = YES;
		//PREPATION FOR LANGSON LIBRARY
		//1:LOCATION
		//
		MKCoordinateRegion campusRegion;
		campusRegion.center.latitude = 33.647205;
		campusRegion.center.longitude = -117.841212;
		campusRegion.span.latitudeDelta = 0.0012872;
		campusRegion.span.longitudeDelta = 0.0019863;
		
		[self.mapView setRegion:campusRegion animated:YES];
		[[UIApplication sharedApplication] setNetworkActivityIndicatorVisible:NO];
		isLangsons = YES;
		[self segmentChanges:YES];
	}
	range = [libraryLocation rangeOfString:@"Science"];
	if (range.location != NSNotFound) 
	{
		atLeastLocationFound = YES;
		//PREPATION FOR SCIENCE LIBRARY
		//1:LOCATION
		//
		MKCoordinateRegion campusRegion;
		campusRegion.center.latitude = 33.645848;
		campusRegion.center.longitude = -117.8462;
		campusRegion.span.latitudeDelta = 0.0012872;
		campusRegion.span.longitudeDelta = 0.0019863;
		
		[self.mapView setRegion:campusRegion animated:YES];
		[[UIApplication sharedApplication] setNetworkActivityIndicatorVisible:NO];
		isLangsons = NO;
		//2:BOOK LOCATION
		[self segmentChanges:NO];
	}
	
	[segment addTarget:self action:@selector(segmentManagement)forControlEvents:UIControlEventValueChanged];
	segment.selectedSegmentIndex = 0;
	[self segmentManagement];
	return atLeastLocationFound;
}

- (void) segmentChanges:(BOOL)isLangson 
{
	
	if (isLangson) 
	{
		[segment setTitle:@"Base" forSegmentAtIndex:0];
		[segment setTitle:@"2nd" forSegmentAtIndex:1];
		[segment setTitle:@"3rd" forSegmentAtIndex:2];
		[segment setTitle:@"4th" forSegmentAtIndex:3];
	}
	else if(!isLangson) {
		[segment setTitle:@"2nd" forSegmentAtIndex:0];
		[segment setTitle:@"4th" forSegmentAtIndex:1];
		[segment setTitle:@"5th" forSegmentAtIndex:2];
		[segment setTitle:@"6th" forSegmentAtIndex:3];
	}
}

- (void) segmentManagement 
{
	[selectedSound play];
	NSString *loadPDF = @"";
	if (isLangsons) {
		direction.hidden = YES;
		if (segment.selectedSegmentIndex==0) 
		{
			loadPDF = @"base";
//			direction.text = @"(AC, AP30, D839, KF105, HM1, JX233)";
//			direction2.text = @"(Z791, Z881, ZA4050, USGOV, TT)";
		}
		else if(segment.selectedSegmentIndex==1)
		{
			loadPDF = @"l2nd";
//			direction.text = @"()";
//			direction2.text = @"()";
		}
		else if(segment.selectedSegmentIndex==2)
		{
			loadPDF = @"l3rd";
//			direction.text = @"()";
		}
		else if(segment.selectedSegmentIndex==3)
		{
			loadPDF = @"l4th";
//			direction.text = @"()";
		}
	}
	else
	{
		direction.hidden = NO;
		if (segment.selectedSegmentIndex==0) 
		{
			loadPDF = @"2nd";
			direction.text = @"(Periodical Exists)";
		}
		else if(segment.selectedSegmentIndex==1)
		{
			loadPDF = @"4th";
			direction.text = @"(AS, Q, QA, QB, QC, QD, QH)";
		}
		else if(segment.selectedSegmentIndex==2)
		{
			loadPDF = @"5th";
			direction.text = @"(QD, QP, QZ, QK, T, TA, TJ, TS, W1, WH)";
		}
		else if(segment.selectedSegmentIndex==3)
		{
			loadPDF = @"6th";
			direction.text = @"(TS, TK, W1, WH, WP, ZWZ)";
		}
	}

	NSURL *pdfURL = [NSURL fileURLWithPath:[[NSBundle mainBundle] pathForResource:loadPDF ofType:@"pdf"]];
	[webViews loadRequest:[NSURLRequest requestWithURL:pdfURL]];
}

- (void)dealloc 
{
	[direction2 release];
	direction2 = nil;
	[direction release];
	direction = nil;
	[webViews release];
	webViews = nil;
	[mapView release];
	mapView = nil;
	[selectedSound release];
	selectedSound = nil;
	[segment release];
	segment = nil;
    [super dealloc];
}


@end
