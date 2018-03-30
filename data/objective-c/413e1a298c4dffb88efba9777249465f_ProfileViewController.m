//
//  ProfileViewController.m
//  PicsTagger
//
//  Created by Alberto Baggio on 19/02/13.
//  Copyright (c) 2013 cappuccino at work. All rights reserved.
//

#import "ProfileViewController.h"

@interface ProfileViewController ()

@end

@implementation ProfileViewController

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
    CGSize screenSize = [[UIScreen mainScreen] bounds].size;
    CGRect screenBounds = [[UIScreen mainScreen] bounds];
    if (screenBounds.size.height == 568) {
        [self.view setBackgroundColor:[UIColor colorWithPatternImage:[UIImage imageNamed:@"ProfileDetail_568.png"]]]; }
    else {
        [self.view setBackgroundColor:[UIColor colorWithPatternImage:[UIImage imageNamed:@"ProfileDetail.png"]]];}
    lines = [[UIImageView alloc] initWithFrame:CGRectMake(screenSize.width/2 - 135, 130, 271, 261)];
    [lines setImage:[UIImage imageNamed:@"lines.png"]];
    [self.view addSubview:lines];
    backButton = [UIButton buttonWithType:UIButtonTypeCustom];
    [backButton setFrame:CGRectMake(20, 25, 34, 35)];
    [backButton setBackgroundImage:[UIImage imageNamed:@"backArrow.png"] forState:UIControlStateNormal];
    [backButton addTarget:self action:@selector(closeProfile) forControlEvents:UIControlEventTouchUpInside];
    [self.view addSubview:backButton];
    logOut = [UIButton buttonWithType:UIButtonTypeCustom];
    [logOut setBackgroundImage:[UIImage imageNamed:@"logOutBtn.png"] forState:UIControlStateNormal];
    [logOut setFrame:CGRectMake(1, screenSize.height-100, 318, 80)];
    [logOut addTarget:self action:@selector(logOut) forControlEvents:UIControlEventTouchUpInside];
    email = [[UIButton alloc] initWithFrame:CGRectMake(24,130, 111, 111)];
    [email setBackgroundImage:[UIImage imageNamed:@"Profile.png"] forState:UIControlStateNormal];
    //[email setText:[[BundleWrapper sharedBundle] userLogin]];
    [email addTarget:self action:@selector(showLoginInfo) forControlEvents:UIControlEventTouchUpInside];
    [self.view addSubview:email];
    [self.view addSubview:logOut];
    tellFriend = [UIButton buttonWithType:UIButtonTypeCustom];
    [tellFriend setBackgroundImage:[UIImage imageNamed:@"Friends.png"] forState:UIControlStateNormal];
    [tellFriend setFrame:CGRectMake(185, 130, 111, 111)];
    [tellFriend addTarget:self action:@selector(facebookPost) forControlEvents:UIControlEventTouchUpInside];
    [self.view addSubview:tellFriend];
    rateUs = [UIButton buttonWithType:UIButtonTypeCustom];
    [rateUs setBackgroundImage:[UIImage imageNamed:@"star.png"] forState:UIControlStateNormal];
    [rateUs setFrame:CGRectMake(24, 278, 111, 111)];
    [rateUs addTarget:self action:@selector(rate) forControlEvents:UIControlEventTouchUpInside];
    [self.view addSubview:rateUs];
    credits = [UIButton buttonWithType:UIButtonTypeCustom];
    [credits setBackgroundImage:[UIImage imageNamed:@"Info.png"] forState:UIControlStateNormal];
    [credits setFrame:CGRectMake(185, 278, 111, 111)];
    [self.view addSubview:credits];
    
    if (screenBounds.size.height == 568) {
        
    }
    else {
        [email setFrame:CGRectMake(24, 100, 111, 111)];
        [tellFriend setFrame:CGRectMake(185, 100, 111, 111)];
        [rateUs setFrame:CGRectMake(24, 248, 111, 111)];
        [credits setFrame:CGRectMake(185, 248, 111, 111)];
        [lines setFrame:CGRectMake(screenSize.width/2 - 135, 100, 271, 261)];
    }

}

- (void) closeProfile {
    [self.navigationController popViewControllerAnimated:YES];
}

- (void) logOut {
    
    [[BundleWrapper sharedBundle] setLogin:@""];
    [[BundleWrapper sharedBundle] setLoged:@"False"];
    [[BundleWrapper sharedBundle] setPassword:@""];
    [self.navigationController pushViewController:[[LoginController alloc] init] animated:YES];
    
}

- (void) showLoginInfo {
    [[TKAlertCenter defaultCenter] postAlertWithMessage:[[BundleWrapper sharedBundle] userLogin]];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (void)facebookPost {
    if([SLComposeViewController isAvailableForServiceType:SLServiceTypeFacebook]) //check if Facebook Account is linked
    {
        mySLComposerSheet = [[SLComposeViewController alloc] init]; //initiate the Social Controller
        mySLComposerSheet = [SLComposeViewController composeViewControllerForServiceType:SLServiceTypeFacebook]; //Tell him with what social plattform to use it, e.g. facebook or twitter
        [mySLComposerSheet setInitialText:[NSString stringWithFormat:@"What an amazing app!!!!! "]]; //the message you want to post
        //[mySLComposerSheet addImage:yourimage]; //an image you could post
        [self presentViewController:mySLComposerSheet animated:YES completion:nil];
    }
    [mySLComposerSheet setCompletionHandler:^(SLComposeViewControllerResult result) {
        NSString *output;
        switch (result) {
            case SLComposeViewControllerResultCancelled:
                output = @"Action Cancelled";
                break;
            case SLComposeViewControllerResultDone:
                output = @"Post Successfull";
                break;
            default:
                break;
        } //check if everythink worked properly. Give out a message on the state.
        //UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Facebook" message:output delegate:nil cancelButtonTitle:@"Ok" otherButtonTitles:nil];
        //[alert show];
    }];
}

- (void) rate {
    
    NSString *myAppID = @"562556739";
    NSString* url = [NSString stringWithFormat: @"itms-apps://ax.itunes.apple.com/WebObjects/MZStore.woa/wa/viewContentsUserReviews?type=Purple+Software&id=%@", myAppID];
    [[UIApplication sharedApplication] openURL: [NSURL URLWithString: url]];

}
@end
