//
//  OutstandingLessonVO.m
//  StreetDog
//
//  Created by Duncan Campbell on 11/3/12.
//  Copyright (c) 2012 Duncan A. Campbell. All rights reserved.
//

#import "OutstandingLessonVO.h"
#import "CourseDO.h"

@implementation OutstandingLessonVO

- (id)initWithOutstandingLessonDO:(OutstandingLessonDO*)outstandingLesson {
    
    self = [super init];
    if (self) {
        isLesson = true;
        lesson = outstandingLesson.lesson;
        conversation = nil;
        title = outstandingLesson.lesson.title;
        status = outstandingLesson.status;
    }
    return self;
}

- (id)initWithConversationDO:(ConversationDO*)nextConversation withStatusType:(OutstandingLessonStatusType)theStatus {
    self = [super init];
    if (self) {
        isLesson = false;
        lesson = nil;
        conversation = nextConversation;
        title = nextConversation.title;
        status = theStatus;
    }
    return self;
}

- (NSString*)description {return title;}

- (BOOL)isLesson {return isLesson;}
- (NSString*)title {return title;}
- (OutstandingLessonStatusType)status {return status;}
- (LessonDO*)lesson {return lesson;}
- (ConversationDO*)conversation {return conversation;}
@end
