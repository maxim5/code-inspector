//
//  AccountsListViewController.swift
//  personalbook
//
//  Created by Adarsh Pastakia on 6/18/14.
//  Copyright (c) 2014 Adarsh Pastakia. All rights reserved.
//

import UIKit
import CoreData

class AccountListViewController: UITableViewController, NSFetchedResultsControllerDelegate {
	
	let CellIdentifier = "AccountCell"
	
	var detailViewController: AccountEntriesViewController? = nil
	var managedObjectContext = Statics.appDelegate.managedObjectContext
	
	override func viewDidLoad() {
		super.viewDidLoad()
		
		if let split = self.splitViewController {
			let controllers = split.viewControllers
			self.detailViewController = controllers[controllers.endIndex-1].topViewController as? AccountEntriesViewController
		}
		
		self.tableView.registerNib(UINib(nibName: CellIdentifier, bundle: NSBundle.mainBundle()), forCellReuseIdentifier: CellIdentifier)
		self.tableView.registerNib(UINib(nibName: NO_DATA, bundle: NSBundle.mainBundle()), forCellReuseIdentifier: NO_DATA)
	}
	
	override func didReceiveMemoryWarning() {
		super.didReceiveMemoryWarning()
		// Dispose of any resources that can be recreated.
	}
	
	// #pragma mark - Table view data source
	override func numberOfSectionsInTableView(tableView: UITableView) -> Int {
		return self.fetchedResultsController.sections.count + 1
	}
	
	override func tableView(tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
		if(section == self.fetchedResultsController.sections.count) {
			let sectionInfo = self.fetchedResultsController.sections[0] as NSFetchedResultsSectionInfo
			return sectionInfo.numberOfObjects==0 ? 1 : 0
		}
		let sectionInfo = self.fetchedResultsController.sections[section] as NSFetchedResultsSectionInfo
		return sectionInfo.numberOfObjects
	}
	
	override func tableView(tableView: UITableView, cellForRowAtIndexPath indexPath: NSIndexPath) -> UITableViewCell {
		if indexPath.section == self.fetchedResultsController.sections.count {
			let cell = tableView.dequeueReusableCellWithIdentifier(NO_DATA, forIndexPath: indexPath) as NoDataCell
			cell.titleLabel.text = "No Accounts Found"
			cell.detailLabel.text = "To add a new account tap the + button"
			
			tableView.scrollEnabled = false
			
			return cell
		}
		
		tableView.scrollEnabled = true
		
		let cell = tableView.dequeueReusableCellWithIdentifier(CellIdentifier, forIndexPath: indexPath) as AccountCell
		self.configureCell(cell, atIndexPath: indexPath)
		
		return cell
	}
	
	override func tableView(tableView: UITableView, canEditRowAtIndexPath indexPath: NSIndexPath) -> Bool {
		// Return false if you do not want the specified item to be editable.
		return true
	}
	
	override func tableView(tableView: UITableView, commitEditingStyle editingStyle: UITableViewCellEditingStyle, forRowAtIndexPath indexPath: NSIndexPath) {
		if editingStyle == .Delete {
			let context = self.fetchedResultsController.managedObjectContext
			context.deleteObject(self.fetchedResultsController.objectAtIndexPath(indexPath) as Account)
			
			var error: NSError? = nil
			if !context.save(&error) {
				// Replace this implementation with code to handle the error appropriately.
				// abort() causes the application to generate a crash log and terminate. You should not use this function in a shipping application, although it may be useful during development.
				//println("Unresolved error \(error), \(error.userInfo)")
				abort()
			}
		}
	}
	
	override func tableView(tableView: UITableView!, heightForRowAtIndexPath indexPath: NSIndexPath!) -> CGFloat {
		return 52
	}
	
	override func tableView(tableView: UITableView, didSelectRowAtIndexPath indexPath: NSIndexPath) {
		let sectionInfo = self.fetchedResultsController.sections[0] as NSFetchedResultsSectionInfo
		if sectionInfo.numberOfObjects == 0 { return }
		if UIDevice.currentDevice().userInterfaceIdiom == .Pad {
			let object = self.fetchedResultsController.objectAtIndexPath(indexPath) as Account
			self.detailViewController!.account = object
		}
		else {
			self.performSegueWithIdentifier("AccountEntries", sender: self)
		}
	}
	
	override func tableView(tableView: UITableView!, accessoryButtonTappedForRowWithIndexPath indexPath: NSIndexPath!) {
		self.performSegueWithIdentifier("EditAccount", sender: indexPath)
	}
	
	func configureCell(cell: AccountCell, atIndexPath indexPath: NSIndexPath) {
		let object = self.fetchedResultsController.objectAtIndexPath(indexPath) as Account
		cell.account = object
	}
	
	// #pragma mark - Segues
	override func prepareForSegue(segue: UIStoryboardSegue, sender: AnyObject?) {
		if segue.identifier == "AccountEntries" {
			let indexPath = self.tableView.indexPathForSelectedRow()
			let object = self.fetchedResultsController.objectAtIndexPath(indexPath) as Account
			((segue.destinationViewController as UINavigationController).topViewController as AccountEntriesViewController).account = object
		}
		if segue.identifier == "EditAccount" {
			let object = self.fetchedResultsController.objectAtIndexPath(sender as NSIndexPath) as Account
			(segue.destinationViewController as AccountEditViewController).account = object
		}
		if segue.identifier == "AccountEditor" {
			(segue.destinationViewController as AccountEditorViewController).underlyingView = view
		}
	}
	
	override func shouldPerformSegueWithIdentifier(identifier: String!, sender: AnyObject!) -> Bool {
		if identifier == "AccountEditor" {
			let sectionInfo = self.fetchedResultsController.sections[0] as NSFetchedResultsSectionInfo
			if sectionInfo.numberOfObjects >= 2 {
				Helper.showPurchaseAlert("Only 2 accounts can be added.\nTo add more please purchase the full version")
				return false
			}
		}
		return true
	}
	
	// #pragma mark - Fetched results controller
	var fetchedResultsController: NSFetchedResultsController {
	if _fetchedResultsController != nil {
		return _fetchedResultsController!
		}
		
		let fetchRequest = NSFetchRequest()
		// Edit the entity name as appropriate.
		let entity = NSEntityDescription.entityForName("Accounts", inManagedObjectContext: self.managedObjectContext)
		fetchRequest.entity = entity
		
		// Set the batch size to a suitable number.
		fetchRequest.fetchBatchSize = 20
		
		// Edit the sort key as appropriate.
		fetchRequest.sortDescriptors = [NSSortDescriptor(key: "name", ascending: true)]
		
		// Edit the section name key path and cache name if appropriate.
		// nil for section name key path means "no sections".
		let aFetchedResultsController = NSFetchedResultsController(fetchRequest: fetchRequest, managedObjectContext: self.managedObjectContext, sectionNameKeyPath: nil, cacheName: "AccountsMaster")
		aFetchedResultsController.delegate = self
		_fetchedResultsController = aFetchedResultsController
		NSFetchedResultsController.deleteCacheWithName("AccountsMaster")
		
		var error: NSError? = nil
		if !_fetchedResultsController!.performFetch(&error) {
			// Replace this implementation with code to handle the error appropriately.
			// abort() causes the application to generate a crash log and terminate. You should not use this function in a shipping application, although it may be useful during development.
			//println("Unresolved error \(error), \(error.userInfo)")
			abort()
		}
		
		return _fetchedResultsController!
	}
	var _fetchedResultsController: NSFetchedResultsController? = nil
	
	func controllerWillChangeContent(controller: NSFetchedResultsController) {
		self.tableView.beginUpdates()
	}
	
	func controller(controller: NSFetchedResultsController, didChangeSection sectionInfo: NSFetchedResultsSectionInfo, atIndex sectionIndex: Int, forChangeType type: NSFetchedResultsChangeType) {
		switch type {
		case NSFetchedResultsChangeInsert:
			self.tableView.insertSections(NSIndexSet(index: sectionIndex), withRowAnimation: .Fade)
		case NSFetchedResultsChangeDelete:
			self.tableView.deleteSections(NSIndexSet(index: sectionIndex), withRowAnimation: .Fade)
		default:
			return
		}
	}
	
	func controller(controller: NSFetchedResultsController, didChangeObject anObject: AnyObject, atIndexPath indexPath: NSIndexPath, forChangeType type: NSFetchedResultsChangeType, newIndexPath: NSIndexPath) {
		switch type {
		case NSFetchedResultsChangeInsert:
			if(tableView.numberOfRowsInSection(1) == 1) { tableView.deleteRowsAtIndexPaths([NSIndexPath(forRow: 0, inSection: 1)], withRowAnimation: .Fade) }
			tableView.insertRowsAtIndexPaths([newIndexPath], withRowAnimation: .Fade)
		case NSFetchedResultsChangeDelete:
			if(tableView.numberOfRowsInSection(0) == 1 && tableView.numberOfRowsInSection(1) == 0) { tableView.insertRowsAtIndexPaths([NSIndexPath(forRow: 0, inSection: 1)], withRowAnimation: .Fade) }
			tableView.deleteRowsAtIndexPaths([indexPath], withRowAnimation: .Fade)
		case NSFetchedResultsChangeUpdate:
			self.configureCell(tableView.cellForRowAtIndexPath(indexPath) as AccountCell, atIndexPath: indexPath)
		case NSFetchedResultsChangeMove:
			tableView.deleteRowsAtIndexPaths([indexPath], withRowAnimation: .Fade)
			tableView.insertRowsAtIndexPaths([newIndexPath], withRowAnimation: .Fade)
		default:
			return
		}
	}
	
	func controllerDidChangeContent(controller: NSFetchedResultsController) {
		self.tableView.endUpdates()
	}
	
}
