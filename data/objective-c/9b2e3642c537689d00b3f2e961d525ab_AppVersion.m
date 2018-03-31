/*
    The MIT License (MIT)
    Copyright (c) 2014 Michael Gorski
*/

#import <Cordova/CDV.h>
#import "AssetsLibrary/AssetsLibrary.h"
#import "AppVersion.h"


@implementation AppVersion

- (void) getVersionNumber:(CDVInvokedUrlCommand*)command
{
	CDVPluginResult* pluginResult = nil;
	NSString* version = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"CFBundleShortVersionString"]; // release version number

	pluginResult = [CDVPluginResult resultWithStatus:CDVCommandStatus_OK messageAsString:version];

	[self.commandDelegate sendPluginResult:pluginResult callbackId:command.callbackId];
}

- (void) getBuildNumber:(CDVInvokedUrlCommand*)command
{
	CDVPluginResult* pluginResult = nil;
	NSString* version = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"CFBundleVersion"]; // build version number

	pluginResult = [CDVPluginResult resultWithStatus:CDVCommandStatus_OK messageAsString:version];

	[self.commandDelegate sendPluginResult:pluginResult callbackId:command.callbackId];
}

- (void) geAlbumPermission:(CDVInvokedUrlCommand*)command
{
	CDVPluginResult* pluginResult = nil;
        ALAuthorizationStatus status = [ALAssetsLibrary authorizationStatus];
	
	NSString* permission = @"1";
	if (status == ALAuthorizationStatusDenied) {
           permission = @"0";
        }
	
	
	pluginResult = [CDVPluginResult resultWithStatus:CDVCommandStatus_OK messageAsString:permission];
	
	[self.commandDelegate sendPluginResult:pluginResult callbackId:command.callbackId];
}



@end
