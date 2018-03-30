#import "sample-object.h"

@implementation SampleObject

- (SampleObject*) initWithNumber: (int) n
{
  if (! [super init])
    return nil;
    
  number = n;
  return self;
}

- (NSString*) description
{
  NSString *description = [[NSString alloc] initWithFormat: @"SampleObject [%d]", number];
  return [description autorelease];
}

@end

