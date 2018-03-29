//
//  ShoppingEntriesViewController.swift
//  personalbook
//
//  Created by Adarsh Pastakia on 6/17/14.
//  Copyright (c) 2014 Adarsh Pastakia. All rights reserved.
//

import UIKit

class ShoppingEntriesViewController: UITableViewController, UISplitViewControllerDelegate {

    var masterPopoverController: UIPopoverController? = nil
	
	override func awakeFromNib() {
		super.awakeFromNib()
		if UIDevice.currentDevice().userInterfaceIdiom == .Pad {
		    self.clearsSelectionOnViewWillAppear = false
		    self.preferredContentSize = CGSize(width: 320.0, height: 600.0)
		}
	}
	
	override func viewDidLoad() {
		super.viewDidLoad()
		
		self.navigationItem.rightBarButtonItem.enabled = false
	}
	
	override func didReceiveMemoryWarning() {
		super.didReceiveMemoryWarning()
		// Dispose of any resources that can be recreated.
	}
	
	// #pragma mark - Table view data source
	
	override func numberOfSectionsInTableView(tableView: UITableView?) -> Int {
		// #warning Potentially incomplete method implementation.
		// Return the number of sections.
		return 0
	}
	
	override func tableView(tableView: UITableView?, numberOfRowsInSection section: Int) -> Int {
		// #warning Incomplete method implementation.
		// Return the number of rows in the section.
		return 0
	}
	
	/*
	override func tableView(tableView: UITableView?, cellForRowAtIndexPath indexPath: NSIndexPath?) -> UITableViewCell? {
	let cell = tableView.dequeueReusableCellWithIdentifier("reuseIdentifier", forIndexPath: indexPath)
	
	// Configure the cell...
	
	return cell
	}
	*/
	
	/*
	// Override to support conditional editing of the table view.
	override func tableView(tableView: UITableView?, canEditRowAtIndexPath indexPath: NSIndexPath?) -> Bool {
	// Return NO if you do not want the specified item to be editable.
	return true
	}
	*/
	
	/*
	// Override to support editing the table view.
	override func tableView(tableView: UITableView?, commitEditingStyle editingStyle: UITableViewCellEditingStyle, forRowAtIndexPath indexPath: NSIndexPath?) {
	if editingStyle == .Delete {
	// Delete the row from the data source
	tableView.deleteRowsAtIndexPaths([indexPath], withRowAnimation: .Fade)
	} else if editingStyle == .Insert {
	// Create a new instance of the appropriate class, insert it into the array, and add a new row to the table view
	}
	}
	*/
	
	/*
	// Override to support rearranging the table view.
	override func tableView(tableView: UITableView?, moveRowAtIndexPath fromIndexPath: NSIndexPath?, toIndexPath: NSIndexPath?) {
	
	}
	*/
	
	/*
	// Override to support conditional rearranging of the table view.
	override func tableView(tableView: UITableView?, canMoveRowAtIndexPath indexPath: NSIndexPath?) -> Bool {
	// Return NO if you do not want the item to be re-orderable.
	return true
	}
	*/
	
	/*
	// #pragma mark - Navigation
	
	// In a storyboard-based application, you will often want to do a little preparation before navigation
	override func prepareForSegue(segue: UIStoryboardSegue?, sender: AnyObject?) {
	// Get the new view controller using [segue destinationViewController].
	// Pass the selected object to the new view controller.
	}
	*/
	
	
	// #pragma mark - Split view
	func splitViewController(splitController: UISplitViewController, willHideViewController viewController: UIViewController, withBarButtonItem barButtonItem: UIBarButtonItem, forPopoverController popoverController: UIPopoverController) {
		barButtonItem.title = "Accounts" // NSLocalizedString(@"Master", @"Master")
		self.navigationItem.setLeftBarButtonItem(barButtonItem, animated: true)
		popoverController.backgroundColor = UIColor.redColor()
		self.masterPopoverController = popoverController
	}
	
	func splitViewController(splitController: UISplitViewController, willShowViewController viewController: UIViewController, invalidatingBarButtonItem barButtonItem: UIBarButtonItem) {
		// Called when the view is shown again in the split view, invalidating the button and popover controller.
		self.navigationItem.setLeftBarButtonItem(nil, animated: true)
		self.masterPopoverController = nil
	}
	func splitViewController(splitController: UISplitViewController, collapseSecondaryViewController secondaryViewController: UIViewController, ontoPrimaryViewController primaryViewController: UIViewController) -> Bool {
		// Return true to indicate that we have handled the collapse by doing nothing; the secondary controller will be discarded.
		return true
	}
	
	// dont hide master
	func splitViewController(svc: UISplitViewController!, shouldHideViewController vc: UIViewController!, inOrientation orientation: UIInterfaceOrientation) -> Bool {
		return false
	}
}
