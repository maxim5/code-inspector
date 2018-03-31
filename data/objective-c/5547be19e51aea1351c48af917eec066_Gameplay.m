/* SimpsonsSpotGame - File: Gameplay.m
 *
 * Copyright (C) 20/10/11  Jonathan Pinto Sperafico
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#import "Gameplay.h"

static const int BALL_RADIUS = 30;

@implementation Gameplay

@synthesize viewController;

//=============================================================================================//
// Singleton initialization
static Gameplay *instance = nil;

+(Gameplay*)getInstance
{
    @synchronized([Gameplay class])
    {
        if(!instance){
            [Gameplay init];
        }
        
        return instance;
    }
    
    return nil;
    
}

+(id)init
{
    @synchronized([Gameplay class])
    {
        NSAssert(instance == nil, @"Attempted to allocate a second instance of a singleton");
        instance = [super alloc];
        return instance;
    }
    
    return nil;
}
//=============================================================================================//


-(id)init
{
    [super init];
    
    self.viewController = [[MyViewController alloc] initWithNibName:@"MainView" bundle:nil];
    
    srandom(time(0));
    spots = [[NSMutableArray alloc] init];
    lives = [[NSMutableArray alloc] init];
    
    //=================== HIT ====================================
    
    NSString *soundPath = [[NSBundle mainBundle] pathForResource:@"hit"
                                                          ofType:@"wav"];
    
    NSURL *fileURL = [[NSURL alloc] initFileURLWithPath:soundPath];
    
    hitPlayer = [[AVAudioPlayer alloc] initWithContentsOfURL:fileURL
                                                       error:nil];
    hitPlayer.volume = 0.1;
    
    //=================== MISS ==================================
    
    soundPath = [[NSBundle mainBundle] pathForResource:@"miss"
                                                ofType:@"wav"];
    
    
    [fileURL release];
    fileURL = [[NSURL alloc] initFileURLWithPath:soundPath];
    
    
    missPlayer = [[AVAudioPlayer alloc] initWithContentsOfURL:fileURL
                                                        error:nil];
    missPlayer.volume = 0.7;
    
    //=================== DISAPPEAR =============================
    
    soundPath = [[NSBundle mainBundle] pathForResource:@"disappear"
                                                ofType:@"wav"];
    
    
    [fileURL release];
    fileURL = [[NSURL alloc] initFileURLWithPath:soundPath];
    
    disappearPlayer = [[AVAudioPlayer alloc] initWithContentsOfURL:fileURL
                                                             error:nil];
    disappearPlayer.volume = 0.3;
    
    
    //=================== THEME ================================
    
    soundPath = [[NSBundle mainBundle] pathForResource:@"theme"
                                                ofType:@"mp3"];
    
    [fileURL release];
    fileURL = [[NSURL alloc] initFileURLWithPath:soundPath];
    backgroundPlayer = [[AVAudioPlayer alloc] initWithContentsOfURL:fileURL
                                                              error:nil];
    backgroundPlayer.volume = 0.5;
    backgroundPlayer.numberOfLoops = -1; //Infinit loop
    [backgroundPlayer play];
    
    [fileURL release];
    
    [self resetGame];
    
    return self;
}


-(void) resetGame
{
    [spots removeAllObjects];
    spotsTouched = 0;
    score = 0;
    level = 1;
    surprize = false;
    
    gameOver = NO;
    
    //Add 3 lives to the player
    for (int i = 0; i < 3; i++)
    {
        UIImageView *life = [[UIImageView alloc] 
                             initWithImage:[UIImage imageNamed:@"life.png"]];
        
        CGRect frame = CGRectMake(10 + 60 * i, 380, 60, 60);
        life.frame = frame;
        [lives addObject:life];
        [viewController addLifeToSubView:life];
        [life release];
        
    }
    
    //Create 3 Donuts with the time space of 1 second
    [self addNewSpot:[[BehaviorSimple alloc] init]];
    [self performSelector:@selector(addNewSpot:)
               withObject:[[BehaviorSimple alloc] init]
               afterDelay:1.0];
    [self performSelector:@selector(addNewSpot:)
               withObject:[[BehaviorSimple alloc] init]
               afterDelay:2.0];
    
    //Load up the bundle variable storage in the device.
    NSNumber *highScore =
    [[NSUserDefaults standardUserDefaults] valueForKey:@"highScore"];
    
    if (highScore == nil)
        highScore = [NSNumber numberWithInt:0];
    
    
    [viewController resetLabelsAndInitHighScoreWith:highScore];
}

-(void) addNewSpotFrom:(Donut *)spot withBehavior:(Behavior *)behavior
{
    Donut *temp = [[Donut alloc] initWithImage:[UIImage imageNamed:@"donut5.png"]
                                         Frame:spot.frame
                                   andBehavior:behavior
                   ];
    
    [spots addObject:temp];
    [viewController addDonutToSubView:temp];
    [temp release];
    
}

-(void) addNewSpot:(Behavior *)behavior
{
    float viewWidth = 320;
    float viewHeight = 360;
    
    float x = random() % (int)(viewWidth - 2 * BALL_RADIUS);
    float y = random() % (int)(viewHeight - 2 * BALL_RADIUS);
    
    Donut *temp = [[Donut alloc] initWithImage:[UIImage imageNamed:@"donut1.png"]
                                 Frame:CGRectMake(x, y, BALL_RADIUS*2, BALL_RADIUS*2)
                                 andBehavior:behavior
                   ];
    
    [spots addObject:temp];
    [viewController addDonutToSubView:temp];
    [temp release];
    
}

-(void) touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{

    BOOL hitSpot = NO;
    
    //For eacg touch in touches array, verify if some touch is near to some Donut
    for (UITouch *touch in touches)
    {
        
        CGPoint point = [viewController getTouchLocationInView:touch];
        
        for (int i = spots.count - 1; i >= 0 && !hitSpot; i--)
        {
            Donut *spot = [spots objectAtIndex:i];
            
            CGRect frame = [[spot.layer presentationLayer] frame];
            
            
            CGPoint origin = CGPointMake(frame.origin.x + frame.size.width /
                                         2, frame.origin.y + frame.size.height / 2);
            
            float distance = 
            pow(origin.x - point.x, 2) + pow(origin.y - point.y, 2);
            distance = sqrt(distance);
            
            
            if (distance <= frame.size.width / 2)
            {
                [self touchedSpot:spot];
                hitSpot = YES;
            }
        }
    }
    
    //If every touch in the array don't hit any donut, make a miss effect e decrease the score.
    if (!hitSpot)
    {
        [missPlayer play];
        score -= 20 * level;
        
        if(score < 0)
            score = 0;

        [viewController setScoreText:
         [NSString stringWithFormat:@"Score: %i", score]];

    }
}

-(void) addScore:(int)amount
{
    ++spotsTouched;
    score += amount * level;
    hitPlayer.currentTime = 0;
    [hitPlayer play];
    
    //Update the value in the view controller
    [viewController setScoreText:[NSString stringWithFormat:@"Score: %i", score]];
}

-(void) touchedSpot:(Donut *)spot
{
    //Verify if the spot was already touched
    if(spot.touched == false)
    {
        
        //Clear every animation in the Donut View
        CGRect frame = [[spot.layer presentationLayer] frame];
        [spot.layer removeAllAnimations];
        spot.frame = frame;
        [spot setNeedsDisplay];
        
        //Set another animation to the Donut View
        [spot applyTouchedAnimation];
        
        //Counter to increase the level for each 10 right hits
        if (spotsTouched % 10 == 0)
        {
            ++level;
            
            //Update the level in the View
            [viewController setLevelText:
             [NSString stringWithFormat:@"Level: %i", level]];
            
            //Turn on the surprise element
            surprize = true;
            
            //Create another life if the lives count is lower than 5
            if (lives.count < 5)
            {
                
                UIImageView *life = [[UIImageView alloc] 
                                     initWithImage:[UIImage imageNamed:@"life.png"]];
                
                CGRect frame = CGRectMake(10 + 60 * lives.count, 380, 60, 60);
                life.frame = frame;
                [viewController addLifeToSubView:life];
                [lives addObject:life];
                [life release];
            }
        }
        
        //If the level is greater than 3 and if the surprize variable is true, the game will create one Donut if an Different Behavior
        if (level >= 3 && surprize == true)
        {
            surprize = false;
            [self addNewSpot:[[BehaviorMultiply alloc] init]];
        }
        
    }
    
}

-(BOOL)isGameOver
{
    return gameOver;
}

-(void)playDisappear
{
    [disappearPlayer play];
}

-(void)removeSpot:(Donut *)spot
{
    [spots removeObject:spot];  
    [spot removeFromSuperview];
}

-(void)removeLife
{
    UIImageView *life = [lives lastObject];
    [lives removeLastObject];
    [life removeFromSuperview];
}

-(int)getLifeCount
{
    return lives.count;
}

-(void)setEndGame
{
    for (UIImageView *spot in spots)
    {
        [spot removeFromSuperview];
        [spot.layer removeAllAnimations];
    }
    
    gameOver = YES;
    
    NSString *message =
    [NSString stringWithFormat:@"Score: %i", score];
    
    NSUserDefaults *defaults = [NSUserDefaults standardUserDefaults];
    
    int highScore = [[defaults valueForKey:@"highScore"] intValue];
    
    if (score > highScore)
        
        [defaults setValue:[NSNumber numberWithInt:score]
                    forKey:@"highScore"];
    
    [viewController createAletWithMessage:message];
}

@end
