//
//  TaskViewController.m
//  CityGame
//
//  Created by Sven Timmermans on 6/02/13.
//  Copyright (c) 2013 Wim. All rights reserved.
//

#define kBgQueue dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0) //defines the background queue

#import "NSDataAdditions.h"
#import "LogViewController.h"
#import "AppDelegate.h"
#import "TaskViewController.h"

@interface TaskViewController ()

@end

@implementation TaskViewController

NSMutableArray *taskArray;

- (void)viewDidLoad
{
    [super viewDidLoad];
	// Do any additional setup after loading the view, typically from a nib.
    taskArray = [[NSMutableArray alloc] init];
    [NSTimer scheduledTimerWithTimeInterval:10 target:self
                                                       selector:@selector(tick) userInfo:nil repeats:YES];
}

-(void)tick{
    
    AppDelegate *appDelegate = [[UIApplication sharedApplication] delegate];
    NSDate *methodStart = [NSDate date];
    
    if ([appDelegate.role isEqualToString:@"Prooi"]){
        // background queue maken en data ophalen
        dispatch_async(kBgQueue, ^{
            NSData* data = [NSData dataWithContentsOfURL:
                            [NSURL URLWithString:[NSString stringWithFormat:@"http://webservice.citygamephl.be/CityGameWS/resources/generic/getLatestTask/%@",appDelegate.gameID]]];
            
            // roept functie fetchedData op om de data correct weer te geven
            [self performSelectorOnMainThread:@selector(fetchedData:) withObject:data waitUntilDone:YES];
            
            // logged hoe lang het duurde voor alles (inclusief tonen) duurde
            NSTimeInterval executionTime = [[NSDate date] timeIntervalSinceDate:methodStart];
            [LogViewController logMethodDuration:executionTime :@"Totaal opdracht ophalen"];
            
        });
    }else{
        // background queue maken en data ophalen
        dispatch_async(kBgQueue, ^{
            NSData* data = [NSData dataWithContentsOfURL:
                            [NSURL URLWithString:[NSString stringWithFormat:@"http://webservice.citygamephl.be/CityGameWS/resources/generic/getLatestTaskHunter/%@", appDelegate.gameID]]];
            
            // logged hoe lang het duurde om de data op te halen
            NSTimeInterval executionTime = [[NSDate date] timeIntervalSinceDate:methodStart];
            [LogViewController logMethodDuration:executionTime :@"data van opdracht ophalen"];
            
            [self performSelectorOnMainThread:@selector(fetchedData:) withObject:data waitUntilDone:YES];
            
            // logged hoe lang het duurde voor alles (inclusief tonen) duurde
            executionTime = [[NSDate date] timeIntervalSinceDate:methodStart];
            [LogViewController logMethodDuration:executionTime :@"Totaal opdracht ophalen"];
            
            if(appDelegate.taskID!=NULL){
                // controleren of de taak van de Jager voltooid is
                NSData* data = [NSData dataWithContentsOfURL:
                                [NSURL URLWithString:[NSString stringWithFormat:@"http://webservice.citygamephl.be/CityGameWS/resources/generic/taskHunterCompleted/%@",appDelegate.taskID]]];
                //De gegevens uit de json halen
                NSError* error;
                NSDictionary* json = [NSJSONSerialization
                                        JSONObjectWithData:data //1
                                        options:kNilOptions
                                        error:&error];
                    
                appDelegate.approved = [json objectForKey:@"completed"];
            }
        });
    }
}

- (void)fetchedData:(NSData *)responseData {
    AppDelegate *appDelegate = [[UIApplication sharedApplication] delegate];
    
    //De gegevens uit de json halen
    NSError* error;
    NSDictionary* json = [NSJSONSerialization
                          JSONObjectWithData:responseData //1
                          
                          options:kNilOptions
                          error:&error];
    
    // Haalt de waarden van de prooi op
    NSNumber* taskID = [json objectForKey:@"taskID"];
    appDelegate.taskID = taskID;
    NSString* location = [json objectForKey:@"location"];
    NSString* description = [json objectForKey:@"description"];
    appDelegate.taskDescription = description;
    NSNumber* longitude = [json objectForKey:@"longitude"];
    appDelegate.taskLongitude = longitude;
    NSNumber* latitude = [json objectForKey:@"latitude"];
    appDelegate.taskLatitude = latitude;
    
    if([json objectForKey:@"photo"]){
        NSString* photo = [json objectForKey:@"photo"];
        [self imageFromString :photo];
    }
    
    // vergelijkt de data om te kijken of het een nieuwe opdracht is
    NSString *tempString;
    tempString = [NSString stringWithFormat: @"Task: %@ at %@: longitude: %@ and latitude: %@", description, location, longitude, latitude];
    if (appDelegate.lastTask == NULL){
        [taskArray addObject: tempString];
        appDelegate.lastTask = tempString;
        _lblTask.text = [NSString stringWithFormat:@"%@", tempString];
        
    } else if (![appDelegate.lastTask isEqualToString: tempString]){
        [taskArray addObject: tempString];
        appDelegate.lastTask = tempString;
        // waarde voor het tonen van de jagers/prooi op waar zetten
        
        int tijd = 60;
        
        if ([appDelegate.role isEqualToString:@"Prooi"]){
            // controleren of de task van
            appDelegate.approved = @"true";
            tijd = tijd * appDelegate.intervalPrey.intValue;
            [NSTimer scheduledTimerWithTimeInterval:tijd target:self
                                           selector:@selector(tick2) userInfo:nil repeats:NO];
        }
        
        _lblTask.text = [NSString stringWithFormat:@"%@", tempString];
    }
    //NSLog(@"%@",taskArray);
}

-(void)tick2{
    AppDelegate *appDelegate = [[UIApplication sharedApplication] delegate];
    appDelegate.approved = @"false";
}

- (void)fotoData:(NSData *)responseData {
    //parse out the json data
    NSError* error;
    NSArray* json = [NSJSONSerialization
                     JSONObjectWithData:responseData //1
                     
                     options:kNilOptions
                     error:&error];
    
    for (NSDictionary *user in json) {
        // Gets the values of the different hunters
        NSString* description = [user objectForKey:@"description"];
        NSLog(@"%@",description);
        NSString* photo = [user objectForKey:@"photo"];
        [self imageFromString :photo];
    }
}

-(void)imageFromString:(NSString *)base64Image{
    //base64 string converteren naar afbeelding
    NSData *data;
    data = [NSData dataWithBase64EncodedString:base64Image];
    _imageView.image = [UIImage imageWithData:data];
}


- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (void)viewDidUnload {
    [self setLblTask:nil];
    [self setImageView:nil];
    [super viewDidUnload];
}
@end
