//
//  UserDO.m
//  StreetDog
//
//  Created by Duncan Campbell on 10/25/12.
//  Copyright (c) 2012 Duncan A. Campbell. All rights reserved.
//

#import "UserDO.h"

@implementation UserDO 

static NSString* const kIdentifier = @"userID";
static NSString* const kGender = @"gender";
static NSString* const kCourse = @"course";
static NSString* const kCurrentConversation = @"currentConversation";
static NSString* const kOutstandingLessons = @"outstandingLessons";

#pragma mark - Initialisation
- (id)initWithCourse:(CourseDO*)theCourse {
    self = [super init];
    if (self) {
        identifier = @"1";
        gender = kGenderUnspecified;
        
        course = theCourse;
        currentConversation = course.firstConversation;
        outstandingLessons = [NSMutableArray array];
    }
    return self;
}

- (id)initWithCoder:(NSCoder *)decoder {
    if((self = [super init])) {
        identifier = [decoder decodeObjectForKey:kIdentifier];
        gender = [[decoder decodeObjectForKey:kGender] intValue];
        course = [decoder decodeObjectForKey:kCourse];
        currentConversation = [decoder decodeObjectForKey:kCurrentConversation];
        outstandingLessons = [decoder decodeObjectForKey:kOutstandingLessons];
    }
    return self;
}
- (void)encodeWithCoder:(NSCoder *)encoder {
    [encoder encodeObject:identifier forKey:kIdentifier];
    [encoder encodeObject:[NSNumber numberWithInt:gender] forKey:kGender];
    [encoder encodeObject:course forKey:kCourse];
    [encoder encodeObject:currentConversation forKey:kCurrentConversation];
    [encoder encodeObject:outstandingLessons forKey:kOutstandingLessons];
}

#pragma mark - Getters
- (NSString*)identifier {return identifier;}

- (Gender)gender {return gender;}
- (void)setIsMale:(BOOL)setValue {
    if (setValue == true) {
        gender = kGenderMale;
    } else {
        gender = kGenderFemale;
    }
}

- (ConversationDO*)currentConversation {return currentConversation;}

- (NSArray*)outstandingLessons {
    return outstandingLessons;
}
- (void)setOutstandingLesson:(LessonDO*)lesson {
    
    bool isLessonAlreadyExists = false;
    for (OutstandingLessonDO *outstandingLesson in outstandingLessons) {
        if (outstandingLesson.lesson == lesson) {
            isLessonAlreadyExists = true;
            break;
        }
    }
    
    if (!isLessonAlreadyExists) {
        OutstandingLessonDO *outstandingLessonDO = [[OutstandingLessonDO alloc] initWithLesson:lesson];
        [outstandingLessons addObject:outstandingLessonDO];
        [DataController.sharedHelper saveData];
    }
}

- (void)removeOutstandingLesson:(LessonDO*)lesson {
    
    for (OutstandingLessonDO *outstandingLesson in outstandingLessons) {
        if (outstandingLesson.lesson == lesson) {
            [outstandingLesson setCompleted];
            [DataController.sharedHelper saveData];
            return;
        }
    }
}
@end
