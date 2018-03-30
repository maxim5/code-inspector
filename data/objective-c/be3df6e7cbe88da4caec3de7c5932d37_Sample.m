//
//  Sample.m
//  fluidinfo
//
//  Created by Barbara Shirtcliff on 9/11/11.
//

#import "Sample.h"
#import "Value.h"
#import "Tag.h"

@implementation Sample
@synthesize rating, comment, dateRead;
- (id) initWithAbout:(NSString *)a Rating:(NSInteger)r Comment:(NSString *)c andDate:(NSString *)d
{
    self = a != NULL ? [super initWithAbout:a] : [super init];
    if (self) {
        [self setRating:[Tag cleanTagWithName:@"rating" andPath:@"test"]];
        [self setComment:[Tag cleanTagWithName:@"comment" andPath:@"test"]];
        [self setDateRead:[Tag cleanTagWithName:@"date read" andPath:@"test"]];
        [self setTag:[self rating] withValue:[[Value alloc] initWithValue:[NSNumber numberWithInt:r]]];
        [self setTag:[self comment] withValue:[[Value alloc] initWithValue:c]];
        [self setTag:[self dateRead] withValue:[[Value alloc] initWithValue:d]];        
    }
    return self;
}
@end
