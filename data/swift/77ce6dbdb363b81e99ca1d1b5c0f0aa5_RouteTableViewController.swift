//
//  StepsTableViewController.swift
//  MKDirectionsSample
//
//  Created by kumagai on 2014/06/14.
//  Copyright (c) 2014年 kumagai. All rights reserved.
//

import UIKit

class RouteTableViewController: UITableViewController {
    
    let routes = ["外回り1", "外回り2"]
    
    override func numberOfSectionsInTableView(tableView: UITableView?) -> Int {
        // #warning Potentially incomplete method implementation.
        // Return the number of sections.
        return 1
    }
    
    override func tableView(tableView: UITableView?, numberOfRowsInSection section: Int) -> Int {
        // #warning Incomplete method implementation.
        // Return the number of rows in the section.
        return routes.count
    }
    
    override func tableView(tableView: UITableView?, cellForRowAtIndexPath indexPath: NSIndexPath?) -> UITableViewCell? {
        let cell = tableView!.dequeueReusableCellWithIdentifier("Cell", forIndexPath: indexPath) as RouteTableViewCell
        // Configure the cell...
        cell.routeLabel.text = routes[indexPath!.row]
        return cell
    }
    
    // 画面を閉じるボタンが押された時の処理
    @IBAction func didTapCloseButton(sender : AnyObject) {
        dismissViewControllerAnimated(true, nil);
    }
}
