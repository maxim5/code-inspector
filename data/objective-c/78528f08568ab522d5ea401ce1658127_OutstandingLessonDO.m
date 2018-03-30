//
//  OutstandingLessonDO.m
//  StreetDog
//
//  Created by Duncan Campbell on 11/3/12.
//  Copyright (c) 2012 Duncan A. Campbell. All rights reserved.
//

#import "OutstandingLessonDO.h"
#import "CourseDO.h"

@implementation OutstandingLessonDO

static NSString* const kLesson = @"lesson";
static NSString* const kStatus = @"status";

#pragma mark - Initialisation
- (id)initWithLesson:(LessonDO*)theLesson {
    
    self = [super init];
    if (self) {
        lesson = theLesson;
        status = kOutstandingLessonStatusTypeAvailable;
    }
    return self;
}

- (id)initWithCoder:(NSCoder *)decoder {
    if((self = [super init])) {
        status = [[decoder decodeObjectForKey:kStatus] intValue];
        lesson = [decoder decodeObjectForKey:kLesson];
    }
    return self;
}
- (void)encodeWithCoder:(NSCoder *)encoder {
    [encoder encodeObject:[NSNumber numberWithInt:status] forKey:kStatus];
    [encoder encodeObject:lesson forKey:kLesson];
}

- (NSString*)description {return lesson.identifier;}

#pragma mark - Getters
- (LessonDO*)lesson {return lesson;}
- (OutstandingLessonStatusType)status {return status;}

- (void)setCompleted {
    status = kOutstandingLessonStatusTypeCompleted;
}
@end
