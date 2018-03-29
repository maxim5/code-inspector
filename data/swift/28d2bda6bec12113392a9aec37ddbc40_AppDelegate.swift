//
//  AppDelegate.swift
//  CoreDataSample
//
//  Created by guille on 25/06/14.
//  Copyright (c) 2014 Guillermo Gonzalez. All rights reserved.
//

import UIKit

@UIApplicationMain
class AppDelegate: UIResponder, UIApplicationDelegate {
                            
    var window: UIWindow?


    func application(application: UIApplication, didFinishLaunchingWithOptions launchOptions: NSDictionary?) -> Bool {
        testCoreData()
        return true
    }

    func testCoreData() {
        let documentsDirectory = NSSearchPathForDirectoriesInDomains(.DocumentDirectory, .UserDomainMask, true)[0] as String
        let path = documentsDirectory.stringByAppendingPathComponent("heroes.db")
        let store = ManagedStore(path: path)
        
        let batman = Superhero.insert(inManagedObjectContext: store.managedObjectContext)
        batman.name = "Batman"
        batman.secretIdentity = "Bruce Wayne"
        
        let ironman = Superhero.insert(inManagedObjectContext: store.managedObjectContext)
        ironman.name = "Iron Man"
        ironman.secretIdentity = "Tony Stark"
        
        var error: NSError?
        store.managedObjectContext.save(&error)
        
        if error {
            // TODO: handle error
        }
        
        var heroes = store.managedObjectContext.executeFetchRequest(Superhero.fetchRequest(), error: &error)
        
        if error {
            // TODO: handle error
        }
        
        println(heroes)
        
        let backgroundContext = store.temporaryManagedObjectContext(.PrivateQueueConcurrencyType)
        
        backgroundContext.performBlock {
            var hulk = Superhero.insert(inManagedObjectContext: backgroundContext)
            hulk.name = "Hulk"
            hulk.secretIdentity = "Bruce Banner"
            
            backgroundContext.save(nil);
        }
    }
    
    func applicationWillResignActive(application: UIApplication) {
        // Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
        // Use this method to pause ongoing tasks, disable timers, and throttle down OpenGL ES frame rates. Games should use this method to pause the game.
    }

    func applicationDidEnterBackground(application: UIApplication) {
        // Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later.
        // If your application supports background execution, this method is called instead of applicationWillTerminate: when the user quits.
    }

    func applicationWillEnterForeground(application: UIApplication) {
        // Called as part of the transition from the background to the inactive state; here you can undo many of the changes made on entering the background.
    }

    func applicationDidBecomeActive(application: UIApplication) {
        // Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
    }

    func applicationWillTerminate(application: UIApplication) {
        // Called when the application is about to terminate. Save data if appropriate. See also applicationDidEnterBackground:.
    }


}

