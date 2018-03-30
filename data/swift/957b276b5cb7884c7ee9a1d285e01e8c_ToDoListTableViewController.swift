//
//  ToDoListTableViewController.swift
//  Swift To-do List
//
//  Created by Paul Yu on 13/6/14.
//  Copyright (c) 2014 Paul Yu. All rights reserved.
//

import UIKit

@objc(ToDoListTableViewController) class ToDoListTableViewController: UITableViewController {
    
    @IBAction func unwindToList(segue :UIStoryboardSegue) {
        let source : AddToDoItemViewController = segue.sourceViewController as AddToDoItemViewController
        let item = source.toDoItem
        if item.itemName.utf16count > 0
        {
            toDoItems.append(item)
            tableView.reloadData()
        }
    }
    
    var toDoItems : ToDoItem[] = []
    
    // Init
    init(coder aDecoder: NSCoder!) {
        super.init(coder: aDecoder)
    }
 
    func loadInitialData() -> Void
    {
        var todos = ["Buy Milk", "Check email", "Clean kitchen", "Take out trash", "Feed dog", "Buy eggs", "Read a book", "Go for a run", "Call John", "Mow the lawn"]
        for todoName in todos {
            var item1 : ToDoItem = ToDoItem()
            item1.itemName = todoName
            toDoItems.append(item1)
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()

        loadInitialData()
        // Uncomment the following line to preserve selection between presentations
        // self.clearsSelectionOnViewWillAppear = false

        // Uncomment the following line to display an Edit button in the navigation bar for this view controller.
        // self.navigationItem.rightBarButtonItem = self.editButtonItem
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }

    // #pragma mark - Table view data source

    override func numberOfSectionsInTableView(tableView: UITableView?) -> Int {
         // Return the number of sections.
        return 1
    }

    override func tableView(tableView: UITableView?, numberOfRowsInSection section: Int) -> Int {
        // #warning Incomplete method implementation.
        // Return the number of rows in the section.
        return toDoItems.count
    }

    
    override func tableView(tableView: UITableView?, cellForRowAtIndexPath indexPath: NSIndexPath?) -> UITableViewCell? {
        let cell = tableView!.dequeueReusableCellWithIdentifier("ListPrototypeCell", forIndexPath: indexPath) as? UITableViewCell
        if !cell {
            let cell = UITableViewCell(style: UITableViewCellStyle.Value1, reuseIdentifier: "ListPrototypeCell")
        }
        // Configure the cell...
        let todo = toDoItems[indexPath!.row]
        cell!.textLabel.text = todo.itemName
        if todo.completed {
            cell!.accessoryType = UITableViewCellAccessoryType.Checkmark
        } else {
            cell!.accessoryType = UITableViewCellAccessoryType.None
        }
        
        return cell
    }
    
    //#pragma mark - Table view delegate
    
    override func tableView(tableView: UITableView?, didSelectRowAtIndexPath indexPath: NSIndexPath?)
    {
        tableView!.deselectRowAtIndexPath(indexPath!, animated: false)
        var todo = toDoItems[indexPath!.row]
        todo.completed = !todo.completed
        tableView!.reloadRowsAtIndexPaths([indexPath!], withRowAnimation: UITableViewRowAnimation.None)
    }
    
    // Override to support conditional editing of the table view.
    override func tableView(tableView: UITableView?, canEditRowAtIndexPath indexPath: NSIndexPath?) -> Bool {
        // Return NO if you do not want the specified item to be editable.
        return true
    }
    

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

}
