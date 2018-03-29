//
//  AccountEntriesViewController.swift
//  personalbook
//
//  Created by Adarsh Pastakia on 6/17/14.
//  Copyright (c) 2014 Adarsh Pastakia. All rights reserved.
//

import UIKit
import CoreData

class AccountEntriesViewController: UITableViewController, UISplitViewControllerDelegate, NSFetchedResultsControllerDelegate {
	
	let CellIdentifier = "AccountEntryCell"
	
	var masterPopoverController: UIPopoverController? = nil
	var managedObjectContext = Statics.appDelegate.managedObjectContext
	
	var headerView:AccountHeader?
	var account: Account? = nil {
	didSet {
		// Update the view.
		self.navigationItem.title = account?.name
		self.navigationItem.rightBarButtonItem.enabled = account != nil
		
		_fetchedResultsController = nil
		self.tableView.reloadData()
		
		account?.addObserver(self, forKeyPath: "name", options: NSKeyValueObservingOptions.New, context: nil)
	}
	}
	
	override func observeValueForKeyPath(keyPath: String!, ofObject object: AnyObject!, change: NSDictionary!, context: CMutableVoidPointer) {
		self.navigationItem.title = account?.name
	}
	
	override func awakeFromNib() {
		super.awakeFromNib()
		if UIDevice.currentDevice().userInterfaceIdiom == .Pad {
			self.clearsSelectionOnViewWillAppear = false
			self.preferredContentSize = CGSize(width: 320.0, height: 600.0)
		}
	}
	
	override func viewDidLoad() {
		super.viewDidLoad()
		Helper.showIndicator()
		
		self.navigationItem.title = account?.name
		self.navigationItem.rightBarButtonItem.enabled = account != nil
		
		if self.masterPopoverController != nil {
			self.masterPopoverController!.dismissPopoverAnimated(true)
		}
		
		self.tableView.registerNib(UINib(nibName: CellIdentifier, bundle: NSBundle.mainBundle()), forCellReuseIdentifier: CellIdentifier)
		self.tableView.registerNib(UINib(nibName: NO_DATA, bundle: NSBundle.mainBundle()), forCellReuseIdentifier: NO_DATA)
	}
	
	override func viewDidAppear(animated: Bool) {
		Helper.hideIndicator()
	}
	
	override func viewDidDisappear(animated: Bool) {
		account?.removeObserver(self, forKeyPath: "name", context: nil)
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
			
			if account {
				cell.titleLabel.text = "No Entries Found"
				cell.detailLabel.text = "To add a new entry tap the + button"
			}
			else {
				cell.titleLabel.text = "Select an account"
				cell.detailLabel.text = ""
			}
			tableView.scrollEnabled = false
			
			return cell
		}
		
		tableView.scrollEnabled = true
		
		let cell = tableView.dequeueReusableCellWithIdentifier(CellIdentifier, forIndexPath: indexPath) as AccountEntryCell
		self.configureCell(cell, atIndexPath: indexPath)
		return cell
	}
	
	override func tableView(tableView: UITableView, canEditRowAtIndexPath indexPath: NSIndexPath) -> Bool {
		// Return false if you do not want the specified item to be editable.
		if indexPath.section == 1 { return false }
		let entry = self.fetchedResultsController.objectAtIndexPath(indexPath) as AccountEntry
		return entry.date.after(NSDate(timeIntervalSinceNow: NSDate.hourInterval(-48)))
	}
	
	override func tableView(tableView: UITableView, commitEditingStyle editingStyle: UITableViewCellEditingStyle, forRowAtIndexPath indexPath: NSIndexPath) {
		if editingStyle == .Delete {
			let context = self.fetchedResultsController.managedObjectContext
			let entry = self.fetchedResultsController.objectAtIndexPath(indexPath) as AccountEntry
			
			context.deleteObject(entry)
			
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
	}
	
	func configureCell(cell: AccountEntryCell, atIndexPath indexPath: NSIndexPath) {
		let object = self.fetchedResultsController.objectAtIndexPath(indexPath) as AccountEntry
		cell.entry = object
	}
	
	override func tableView(tableView: UITableView!, heightForHeaderInSection section: Int) -> CGFloat {
		if section == 0 && account { return 52 }
		return 0
	}
	
	override func tableView(tableView: UITableView!, viewForHeaderInSection section: Int) -> UIView! {
		if(section == 0 && account) {
			headerView = AccountHeader.getView()
			headerView!.configureCell(account!)
			return headerView
		}
		return nil
	}
	
	
	// #pragma mark - Navigation
	override func prepareForSegue(segue: UIStoryboardSegue, sender: AnyObject?) {
		if segue.identifier == "EntryEditor" {
			(segue.destinationViewController as AccountEntryEditorViewController).account = account
			(segue.destinationViewController as AccountEntryEditorViewController).underlyingView = view
		}
	}
	
	func showFilters() {
		//uiactio
	}
	
	
	// #pragma mark - Fetched results controller
	var fetchedResultsController: NSFetchedResultsController {
	if _fetchedResultsController != nil {
		return _fetchedResultsController!
		}
		
		let fetchRequest = NSFetchRequest()
		// Edit the entity name as appropriate.
		let entity = NSEntityDescription.entityForName("AccountEntries", inManagedObjectContext: self.managedObjectContext)
		fetchRequest.entity = entity
		
		// Set the batch size to a suitable number.
		fetchRequest.fetchBatchSize = 20
		
		fetchRequest.predicate = NSPredicate(format: "self.account = %@", account? ? account! : nil)
		//if account { fetchRequest.predicate = NSPredicate(format: "self.account = %@", account!) }
		//else { fetchRequest.predicate = NSPredicate(format: "self.account = 0") }
		
		// Edit the sort key as appropriate.
		fetchRequest.sortDescriptors = [NSSortDescriptor(key: "date", ascending: false)]
		
		// Edit the section name key path and cache name if appropriate.
		// nil for section name key path means "no sections".
		let aFetchedResultsController = NSFetchedResultsController(fetchRequest: fetchRequest, managedObjectContext: self.managedObjectContext, sectionNameKeyPath: nil, cacheName: "AccountsDetail")
		aFetchedResultsController.delegate = self
		_fetchedResultsController = aFetchedResultsController
		NSFetchedResultsController.deleteCacheWithName("AccountsDetail")
		
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
			self.configureCell(tableView.cellForRowAtIndexPath(indexPath) as AccountEntryCell, atIndexPath: indexPath)
		case NSFetchedResultsChangeMove:
			tableView.deleteRowsAtIndexPaths([indexPath], withRowAnimation: .Fade)
			tableView.insertRowsAtIndexPaths([newIndexPath], withRowAnimation: .Fade)
		default:
			return
		}
		headerView!.configureCell(account!)
	}
	
	func controllerDidChangeContent(controller: NSFetchedResultsController) {
		self.tableView.endUpdates()
	}
	
	
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
