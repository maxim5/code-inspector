//
//  AboutViewController.m
//  MARL
//
//  Created by Kevin Murphy on 10/21/12.
//  Copyright (c) 2012 New York University. All rights reserved.
//

#import "AboutViewController.h"

@interface AboutViewController ()

@end

@implementation AboutViewController

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        self.view.backgroundColor= RGBA(200, 200, 200, 1.0);
        self.view.alpha = 0.8;
        self.title = @"About MARL @ NYU";
        // Custom initialization
        
        
        
    }
    return self;
}

- (void) viewWillAppear:(BOOL)animated {
    self.navigationController.view.superview.bounds = CGRectMake(0, 0, self.view.bounds.size.width, 350);//it's important to do this after
    self.navigationController.navigationBar.frame = CGRectOffset(self.navigationController.navigationBar.frame, 0, -1);
    self.navigationController.navigationBar.tintColor = RGBA(36, 10, 119, 1);
    //self.navigationController.navigationBar.alpha = 0.7f;
    //self.navigationController.navigationBar.translucent = YES;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
    

	// Do any additional setup after loading the view.
}

- (void) dismiss {
    [self.view.window removeGestureRecognizer:gesture];
    [self dismissModalViewControllerAnimated:YES];
}

- (void) viewDidAppear:(BOOL)animated {
    
    //self.view.superview.center = self.view.center;
    
    
    gesture = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(handleTapBehind:)];
    
    [gesture setNumberOfTapsRequired:1];
    gesture.cancelsTouchesInView = NO; //So the user can still interact with controls in the modal view
    [self.view.window addGestureRecognizer:gesture];

    UILabel *people = [[UILabel alloc] initWithFrame:CGRectMake(20, 10, self.view.frame.size.width-40, 110)];
    people.text = @"Juan Pablo Bello - Project Manager\nLangdon Crawford - Project Manager\nKevin Murphy - Lead Developer\nAshlyn Kersten - Video Production\nJoshua Chang - Video Production";
    people.textAlignment = NSTextAlignmentCenter;
    people.numberOfLines = 0;
    [people setBackgroundColor:[UIColor clearColor]];
    [people setTextColor:[UIColor colorWithRed:0.22 green:0.22 blue:0.27 alpha:1]];
    [people setShadowColor: [UIColor whiteColor]];
    [people setShadowOffset: CGSizeMake(0.0, 1.0)];
    [people setFont:[UIFont boldSystemFontOfSize:15]];
    
    UILabel *about = [[UILabel alloc] initWithFrame:CGRectMake(20, 10+people.frame.origin.y+people.frame.size.height, self.view.frame.size.width-40, 80)];
    NSLog(@"%f", self.view.frame.size.width);
    [about setText:@"MARL groups scholars from music theory, technology and composition, computer and information science, interactive media and media studies, to explore the intersection between music, computation and science. The objective is to combine techniques and methodologies from the arts, the humanities and the sciences to (a) understand and model human cognitive abilities in music, and (b) innovate the analysis, organization and creation of music."];
    about.numberOfLines = 0;
    about.textAlignment = NSTextAlignmentCenter;
    about.lineBreakMode = NSLineBreakByWordWrapping;
    [about setBackgroundColor:[UIColor clearColor]];
    [about setTextColor:[UIColor colorWithRed:0.22 green:0.22 blue:0.27 alpha:1]];
    [about setShadowColor: [UIColor whiteColor]];
    [about setShadowOffset: CGSizeMake(0.0, 1.0)];
    [about setFont:[UIFont systemFontOfSize:14]];
    [about sizeToFit];
    
    UILabel *copyRight = [[UILabel alloc] initWithFrame:CGRectMake(140, 10+about.frame.origin.y+about.frame.size.height, self.view.frame.size.width/2-160, 30)];
    copyRight.text = @"ÂŠ MARL 2012 - ";
    [copyRight setBackgroundColor:[UIColor clearColor]];
    [copyRight setTextColor:[UIColor colorWithRed:0.22 green:0.22 blue:0.27 alpha:1]];
    [copyRight setShadowColor: [UIColor whiteColor]];
    [copyRight setShadowOffset: CGSizeMake(0.0, 1.0)];
    [copyRight setFont:[UIFont boldSystemFontOfSize:13]];
    copyRight.textAlignment = NSTextAlignmentRight;

    UIButton *link = [UIButton buttonWithType:UIButtonTypeCustom];
    link.frame = CGRectOffset(copyRight.frame, copyRight.frame.size.width-15, 0);
    link.frame = CGRectMake(link.frame.origin.x, link.frame.origin.y, 200, link.frame.size.height);
    [link addTarget:self action:@selector(loadMARLWebsite) forControlEvents:UIControlEventTouchUpInside];
    [link setTitleEdgeInsets:UIEdgeInsetsMake(0.0, 0.0, 0.0, 0.0 )];
    [link setTitle:@"http://marl.smusic.nyu.edu/" forState:UIControlStateNormal];
    [link.titleLabel setTextColor:[UIColor colorWithRed:0.22 green:0.22 blue:0.27 alpha:1]];
    link.titleLabel.font = [UIFont systemFontOfSize:13];
    link.titleLabel.frame = link.bounds;
    [link.titleLabel setTextColor:[UIColor colorWithRed:0.22 green:0.22 blue:0.27 alpha:1]];
    [link.titleLabel setShadowColor: [UIColor whiteColor]];
    [link.titleLabel setShadowOffset: CGSizeMake(0.0, 1.0)];
    [link.titleLabel setFont:[UIFont boldSystemFontOfSize:13]];
    link.titleLabel.textAlignment = NSTextAlignmentLeft;
    linkButton = link;
    [self.view addSubview:about];
    [self.view addSubview:people];
    [self.view addSubview:copyRight];
    [self.view addSubview:link];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (void) touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event {
    [self dismiss];
    NSLog(@"Touch");
}

- (void) handleTapBehind:(UITapGestureRecognizer*) gestureRecognizer {
    CGPoint loc = [gestureRecognizer locationInView:self.view];
    if(CGRectContainsPoint(linkButton.frame, loc)) {
        
    } else {
        NSLog(@"Tap");
        [self dismiss];
    }
}

- (void) loadMARLWebsite {
    NSLog(@"Tap");
    [[UIApplication sharedApplication] openURL:[NSURL URLWithString: @"http://marl.smusic.nyu.edu"]];
    
}

@end
