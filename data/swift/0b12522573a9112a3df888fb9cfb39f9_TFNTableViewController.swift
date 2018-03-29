//
//  TFNTableViewController.swift
//  MiniPlatform
//
//  Created by Robert Diamond on 6/24/14.
//  Copyright (c) 2014 Robert Diamond. All rights reserved.
//

import Foundation
import UIKit

// TODO: stream, rowadapterfactory, and sectionadapter can be generics,
// when this won't crash the compiler
class TFNTableViewController : UITableViewController {
    let DEFAULT_CELL_HEIGHT : CGFloat = 100.0

    var stream : Stream?
    var rowAdapters : Dictionary<String,RowAdapter>?
    var sectionAdapter : SectionAdapter?
    var minID : Int64?
    var maxID : Int64?
    var navigationDelegate : NavigationDelegate?
    
    var sections : Array<Array<ModelObject>>?
    {
    didSet {
        dispatch_async(dispatch_get_main_queue()) {
            self.tableView.reloadData()
        }
    }
    }

    init(coder aDecoder: NSCoder)
    {
        super.init(coder: aDecoder)
        rowAdapters = self._defaultRowAdapters()
    }

    override func viewDidLoad()
    {
        // TODO: install default row adapters, e.g. String
        super.viewDidLoad()
        self.tableView.rowHeight = UITableViewAutomaticDimension
        self.refreshControl?.addTarget(self, action: "refresh:", forControlEvents: .ValueChanged)
    }

    func _defaultRowAdapters() -> Dictionary<String,RowAdapter>
    {
        var newRowAdapters = Dictionary<String,RowAdapter>()
        newRowAdapters[NSString(CString: class_getName(ErrorItem),encoding: NSUTF8StringEncoding)] = ErrorItemRowAdapter()
        newRowAdapters[NSString(CString: class_getName(FooterItem),encoding: NSUTF8StringEncoding)] = FooterItemRowAdapter()
        return newRowAdapters
    }

    func refresh(sender: AnyObject)
    {
        self.loadTop()
    }

    override func numberOfSectionsInTableView(tableView: UITableView!) -> Int
    {
        return (sections) ? sections!.count : 0
    }

    override func tableView(tableView: UITableView!, numberOfRowsInSection section: Int) -> Int
    {
        if sections.getLogicValue() && section < sections!.count {
            return sections![section].count
        } else {
            return 0
        }
    }

    func itemAtIndexPath(indexPath : NSIndexPath!) -> ModelObject?
    {
        if indexPath != nil && sections?.count > indexPath.section && sections![indexPath.section].count > indexPath.row {
            return sections![indexPath.section][indexPath.row]
        } else {
            return nil
        }
    }

    override func tableView(tableView: UITableView!, cellForRowAtIndexPath indexPath: NSIndexPath!) -> UITableViewCell!
    {
        var cell : UITableViewCell? = nil;
        if sections?.count > indexPath.section && sections![indexPath.section].count > indexPath.row {
            let item : ModelObject = sections![indexPath.section][indexPath.row]
            let itemClass = NSString(CString: class_getName((item as AnyObject).dynamicType),encoding: NSUTF8StringEncoding)
            if rowAdapters![itemClass].getLogicValue() {
                cell = rowAdapters?[itemClass]?.cellForItem(item,tableViewController: self)
            }
        }

        return cell
    }

    override func tableView(tableView: UITableView!, heightForRowAtIndexPath indexPath: NSIndexPath!) -> CGFloat
    {
        var height : CGFloat?
        if sections?.count > indexPath.section && sections![indexPath.section].count > indexPath.row {
            let item : ModelObject = sections![indexPath.section][indexPath.row]
            let itemClass = NSString(CString: class_getName((item as AnyObject).dynamicType),encoding: NSUTF8StringEncoding)
            if rowAdapters![itemClass].getLogicValue() {
                height = rowAdapters?[itemClass]?.heightForItem(item,tableViewController: self)
            }
        }
        return (height) ? height! : 0
    }

    override func tableView(tableView: UITableView!, estimatedHeightForRowAtIndexPath indexPath: NSIndexPath!) -> CGFloat
    {
        var height : CGFloat?
        if sections?.count > indexPath.section && sections![indexPath.section].count > indexPath.row {
            let item : ModelObject = sections![indexPath.section][indexPath.row]
            let itemClass = NSString(CString: class_getName((item as AnyObject).dynamicType),encoding: NSUTF8StringEncoding)
            if rowAdapters![itemClass].getLogicValue() {
                height = rowAdapters?[itemClass]?.estimatedHeightForItem(item,tableViewController: self)
            }
        }
        return (height.getLogicValue()) ? height! : DEFAULT_CELL_HEIGHT
    }

    override func tableView(tableView: UITableView!, didSelectRowAtIndexPath indexPath: NSIndexPath!)
    {
        if sections?.count > indexPath.section && sections![indexPath.section].count > indexPath.row {
            let item : ModelObject = sections![indexPath.section][indexPath.row]
            let itemClass = NSString(CString: class_getName((item as AnyObject).dynamicType),encoding: NSUTF8StringEncoding)
            if let tIndexPath = indexPath {
                if rowAdapters![itemClass].getLogicValue() {
                    // This may call right back into this class - beware confusion
                    rowAdapters?[itemClass]?.didSelectItem(item, tableViewController: self, indexPath: tIndexPath)
                }
            }
        }
    }

    override func tableView(tableView: UITableView!, heightForHeaderInSection section: Int) -> CGFloat
    {
        return (self.sectionAdapter.getLogicValue()) ? self.sectionAdapter!.tableViewController(self, heightForHeaderInSection: section) : 0
    }

    override func tableView(tableView: UITableView!, heightForFooterInSection section: Int) -> CGFloat
    {
        return (self.sectionAdapter.getLogicValue()) ? self.sectionAdapter!.tableViewController(self, heightForFooterInSection: section) : 0
    }

    override func tableView(tableView: UITableView!, viewForHeaderInSection section: Int) -> UIView!
    {
        return (self.sectionAdapter.getLogicValue()) ? self.sectionAdapter!.tableViewController(self, viewForHeaderInSection: section) : nil
    }

    override func tableView(tableView: UITableView!, viewForFooterInSection section: Int) -> UIView!
    {
        return (self.sectionAdapter.getLogicValue()) ? self.sectionAdapter!.tableViewController(self, viewForFooterInSection: section) : nil
    }

    override func tableView(tableView: UITableView!, estimatedHeightForHeaderInSection section: Int) -> CGFloat
    {
        return (self.sectionAdapter.getLogicValue()) ? self.sectionAdapter!.tableViewController(self, estimatedHeightForHeaderInSection: section) : 0
    }
    override func tableView(tableView: UITableView!, estimatedHeightForFooterInSection section: Int) -> CGFloat
    {
        return (self.sectionAdapter.getLogicValue()) ? self.sectionAdapter!.tableViewController(self, estimatedHeightForFooterInSection: section) : 0
    }

    func update()
    {
        self.sections = self.sectionAdapter?.sectionArray(self.stream?.streamObjects)
    }

    func loadTop()
    {
        self.loadTop() {(results : AnyObject?, error : NSError?) in /* do nothing */}
    }

    func loadTop(completion: CompletionFunction)
    {
        stream?.loadTop() {
            (results : AnyObject?, error : NSError?) in
            self.refreshControl?.endRefreshing()
            if let errorval = error {
                if errorval.code == 403 || errorval.code == 401 {
                    TFNTwitter.sharedTwitter.currentAccount = nil
                    self.navigationDelegate?.loginIfNeeded(fromViewController: self)
                } else {
                    let e = ErrorItem()
                    e.error = errorval
                    self.sections = [[e]]
                }
            } else {
                self.update()
            }
            completion(results: results, error: error)
        }
    }

    func loadBottom(indexPath : NSIndexPath?)
    {
        stream?.loadBottom() {
            (results : AnyObject?, error : NSError?) in
            self.refreshControl?.endRefreshing()
            if let errorval = error {
                if errorval.code == 403 {
                    TFNTwitter.sharedTwitter.currentAccount = nil
                    self.navigationDelegate?.loginIfNeeded(fromViewController: self)
                }
            } else {
                self.update()
            }
        }
    }
    
    override func viewWillAppear(animated: Bool)
    {
        super.viewWillAppear(animated)
        self.refreshControl?.beginRefreshing()
        self.loadTop()
    }
}
