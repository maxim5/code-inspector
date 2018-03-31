//
//  Sounds.m
//  Pong3D
//
//  Created by Jure Ham on 12/22/10.
//  Copyright 2010 __MyCompanyName__. All rights reserved.
//

#import "Sounds.h"
#import "Namespace.Pong3D.h"
#import "Retronator.Xni.Framework.Content.h"

Sounds *instance;

@implementation Sounds

+ (void) initializeWithGane:(Game *)game {
	instance = [[Sounds alloc] initWithGame:game];
	[game.components addComponent:instance];
}

- (void) initialize {
	racket = [self.game.content load:@"racket"];
	wall = [self.game.content load:@"wall"];
	
	lose = [self.game.content load:@"lose"];
	win = [self.game.content load:@"win"];
	
	hit = [self.game.content load:@"hit"];
	miss = [self.game.content load:@"miss"];
}

- (void) play:(NSInteger) effect {
	switch (effect) {
		case WALL_SOUND:
			[wall play];
			break;
		case RACKET_SOUND:
			[racket play];
			break;
		case WIN_SOUND:
			[win play];
			break;
		case LOSE_SOUND:
			[lose play];
			break;
		case HIT_SOUND:
			[hit play];
			break;
		case MISS_SOUND:
			[miss play];
			break;
	}
}


+ (void) play:(NSInteger) effect {
	[instance play:effect];
	NSLog(@"play %d", effect);
}

@end
