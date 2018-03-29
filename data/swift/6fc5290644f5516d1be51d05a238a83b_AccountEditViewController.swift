//
//  AccountEditViewController.swift
//  personalbook
//
//  Created by Adarsh Pastakia on 6/20/14.
//  Copyright (c) 2014 Adarsh Pastakia. All rights reserved.
//

import UIKit
import CoreData

class AccountEditViewController: UITableViewController, NSFetchedResultsControllerDelegate {
	
	let CellIdentifier = "RecurringEntryCell"
	
	var managedObjectContext = Statics.appDelegate.managedObjectContext
	
	var headerView:AccountEdit?
	var account: Account? {
	didSet {
		// Update the view.
		self.navigationItem.title = account!.name
		self.navigationItem.rightBarButtonItem.enabled = account != nil
		
		_fetchedResultsController = nil
		self.tableView.reloadData()
	}
	}
	
	override func viewDidLoad() {
		super.viewDidLoad()
		Helper.showIndicator()
		
		self.tableView.registerNib(UINib(nibName: CellIdentifier, bundle: NSBundle.mainBundle()), forCellReuseIdentifier: CellIdentifier)
		self.tableView.registerNib(UINib(nibName: NO_DATA, bundle: NSBundle.mainBundle()), forCellReuseIdentifier: NO_DATA)
	}
	
	
	override func viewDidAppear(animated: Bool) {
		Helper.hideIndicator()
	}
	
	override func didReceiveMemoryWarning() {
		super.didReceiveMemoryWarning()
		// Dispose of any resources that can be recreated.
	}
	
	// #pragma mark - Table view data source
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
			cell.titleLabel.text = "No Recurring Entries Added"
			cell.detailLabel.text = "To add a new recurring entry tap the + button"
			
			tableView.scrollEnabled = false
			
			return cell
		}
		
		tableView.scrollEnabled = true
		
		let cell = tableView.dequeueReusableCellWithIdentifier(CellIdentifier, forIndexPath: indexPath) as RecurringEntryCell
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
			context.deleteObject(self.fetchedResultsController.objectAtIndexPath(indexPath) as RecurringEntry)
			
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
		return 76
	}
	
	override func tableView(tableView: UITableView!, heightForHeaderInSection section: Int) -> CGFloat {
		if section == 0 && account { return 76 }
		return 0
	}
	
	override func tableView(tableView: UITableView!, viewForHeaderInSection section: Int) -> UIView! {
		if(section == 0 && account) {
			headerView = AccountEdit.getView()
			headerView!.configureCell(account!)
			headerView!.btAdd.addTarget(self, action: "accessoryTapped:", forControlEvents: .TouchUpInside)
			
			return headerView
		}
		return nil
	}
	
	func accessoryTapped(sender:AnyObject) {
		let sectionInfo = self.fetchedResultsController.sections[0] as NSFetchedResultsSectionInfo
		if sectionInfo.numberOfObjects >= 5 {
			Helper.showPurchaseAlert("Only 5 recurring entries can be added.\nTo add more please purchase the full version")
		}
		else {
			self.performSegueWithIdentifier("RecurringEditor", sender: self)
		}
	}
	
	func configureCell(cell: RecurringEntryCell, atIndexPath indexPath: NSIndexPath) {
		let object = self.fetchedResultsController.objectAtIndexPath(indexPath) as RecurringEntry
		cell.entry = object
	}
	
	// #pragma mark - Segues
	override func prepareForSegue(segue: UIStoryboardSegue, sender: AnyObject?) {
		if segue.identifier == "RecurringEditor" {
			(segue.destinationViewController as RecurringEditorViewController).account = account
			(segue.destinationViewController as RecurringEditorViewController).underlyingView = view
		}
	}
	
	@IBAction func updateAccount() {
		if headerView!.tfName.text.stringByTrimmingCharactersInSet(.whitespaceCharacterSet()) != "" { account!.name = headerView!.tfName.text.stringByTrimmingCharactersInSet(.whitespaceCharacterSet()) }
		account!.desc = headerView!.tfDesc.text.stringByTrimmingCharactersInSet(.whitespaceCharacterSet())
		
		var error: NSError? = nil
		if !managedObjectContext.save(&error) {
			println("Unresolved error \(error), \(error!.userInfo)")
			Helper.showError(error.description)
		}
		self.navigationController.popViewControllerAnimated(true)
	}
	
	@IBAction func cancelUpdate() {
		self.navigationController.popViewControllerAnimated(true)
	}
	
	// #pragma mark - Fetched results controller
	var fetchedResultsController: NSFetchedResultsController {
	if _fetchedResultsController != nil {
		return _fetchedResultsController!
		}
		
		let fetchRequest = NSFetchRequest()
		// Edit the entity name as appropriate.
		let entity = NSEntityDescription.entityForName("RecurringEntries", inManagedObjectContext: self.managedObjectContext)
		fetchRequest.entity = entity
		
		// Set the batch size to a suitable number.
		fetchRequest.fetchBatchSize = 20
		
		// Edit the sort key as appropriate.
		fetchRequest.sortDescriptors = [NSSortDescriptor(key: "enabled", ascending: false), NSSortDescriptor(key: "desc", ascending: true)]
		
		fetchRequest.predicate = NSPredicate(format: "self.account = %@", account!)
		
		// Edit the section name key path and cache name if appropriate.
		// nil for section name key path means "no sections".
		let aFetchedResultsController = NSFetchedResultsController(fetchRequest: fetchRequest, managedObjectContext: self.managedObjectContext, sectionNameKeyPath: nil, cacheName: "RecurringMaster")
		aFetchedResultsController.delegate = self
		_fetchedResultsController = aFetchedResultsController
		NSFetchedResultsController.deleteCacheWithName("RecurringMaster")
		
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
			self.configureCell(tableView.cellForRowAtIndexPath(indexPath) as RecurringEntryCell, atIndexPath: indexPath)
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
