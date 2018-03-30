//
// Copyright 2013 Hiroki Ata
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#import "HASampleTest.h"
#import "HAEntityManager.h"
#import "HATableEntityMigration.h"
#import "HATableEntity.h"

@interface HASampleTestSample1 : HATableEntity

+ (NSString*) tableName;

@property NSString* name;
@property NSString* details;
@property NSInteger price;

@end

@implementation HASampleTestSample1

+ (NSString*) tableName
{
    return @"sample1";
}

@synthesize name;
@synthesize details;
@synthesize price;

@end



@implementation HASampleTest

- (void)setUp
{
    [super setUp];
    dbFilePath = [NSTemporaryDirectory() stringByAppendingString:@"/HAEntity_HASampleTest.sqlite"];
}

- (void)tearDown
{
    [[HAEntityManager instanceForPath:dbFilePath] remove];
    
    NSError* error;
    NSFileManager* manager = [NSFileManager defaultManager];
    if ([manager fileExistsAtPath:dbFilePath]) {
        [manager removeItemAtPath:dbFilePath error:&error];
    }

    [super tearDown];
}

- (void) testSample1
{
    // Init sqlite file.
    HAEntityManager* manager = [HAEntityManager instanceForPath:dbFilePath];

    // Create a new table based on the class properties.
    HATableEntityMigration* migration = [[HATableEntityMigration alloc] initWithVersion:1 entityClasses:[HASampleTestSample1 class], nil];
    [manager addEntityMigrating:migration];
    [manager upToHighestVersion];

    // Create a new instance and then set new values.
    HASampleTestSample1* sample = HASampleTestSample1.new;
    sample.name = @"sample";
    sample.details = @"testSample1 details";
    sample.price = 101;

    // Then save it.
    [sample save];

    // Get it from memory(Or use the same instance.)
    sample = [HASampleTestSample1 find_first];
    STAssertEqualObjects(@"sample", sample.name, @"Verify name is saved successfully.");
    STAssertEqualObjects(@"testSample1 details", sample.details, @"Verify details is saved successfully.");
    STAssertEquals(101, sample.price, @"Verify price is saved successfully.");
}

- (void) testSample2
{
    // Init sqlite file.
    HAEntityManager* manager = [HAEntityManager instanceForPath:dbFilePath];
    HASampleTestSample1* sample = nil;
    NSArray* result = nil;
    NSUInteger correct;

    // Create a new table based on the class properties.
    HATableEntityMigration* migration = [[HATableEntityMigration alloc] initWithVersion:1 entityClasses:[HASampleTestSample1 class], nil];
    [manager addEntityMigrating:migration];
    [manager upToHighestVersion];
    
    // Create new instances and then set new values.
    for (int i = 0;i < 10;i++) {
        sample = HASampleTestSample1.new;
        sample.name = [NSString stringWithFormat:@"foo %d", i];
        sample.details = [NSString stringWithFormat:@"bar %d", i];
        sample.price = 100 + i;
        [sample save];
    }

    // Run select.
    result = [HASampleTestSample1 select:@"price FROM sample1 WHERE name = ?" params:@"foo 0", nil];
    correct = 1;
    STAssertEquals(correct, result.count, @"Verify query result.");
    sample = [result objectAtIndex:0];
    STAssertEquals(100, sample.price, @"First row should be returned.");

    // Run where.
    result = [HASampleTestSample1 where:@"price < ?" params:[NSNumber numberWithInt:105], nil];
    correct = 5;
    STAssertEquals(correct, result.count, @"Verify query result.");

    // order
    result = [HASampleTestSample1 order_by:@"price desc"];
    correct = 10;
    STAssertEquals(correct, result.count, @"Verify query result.");
    sample = [result objectAtIndex:0];
    STAssertEquals(109, sample.price, @"Last row should be returned.");

}




@end
