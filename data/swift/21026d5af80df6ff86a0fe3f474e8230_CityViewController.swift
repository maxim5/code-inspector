//
//  CityViewController.swift
//  FlickR
//
//  Created by Jonathan Schmidt on 05/06/2014.
//  Copyright (c) 2014 Matelli. All rights reserved.
//

import UIKit
import CoreLocation

class CityViewController: UITableViewController {

    var cities = City.allCities()

    override func viewDidLoad() {
        super.viewDidLoad()

        refreshControl = UIRefreshControl()
        refreshControl.addTarget(self, action: "localizeCities", forControlEvents: UIControlEvents.ValueChanged)
        
        // Uncomment the following line to preserve selection between presentations
        // self.clearsSelectionOnViewWillAppear = false

        // Uncomment the following line to display an Edit button in the navigation bar for this view controller.
        // self.navigationItem.rightBarButtonItem = self.editButtonItem
    }
    
    override func viewWillAppear(animated: Bool) {
        super.viewWillAppear(animated)
        cities = City.allCities()
        self.tableView.reloadData()
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }
    
    func localizeCities() {
        for index in 0..cities.count {
            let city = cities[index]
            let location = CLLocation(latitude: city.latitude.doubleValue, longitude: city.longitude.doubleValue)
            CLGeocoder().reverseGeocodeLocation(location, completionHandler: {(placemarks, error) -> Void in for placemark in placemarks as CLPlacemark[] {
                if (city.name == "Quelque-part" || city.name == "Lieu Inconnu") && !placemark.locality.isEmpty {
                    city.name = placemark.locality
                } else if (city.name == "Quelque-part" || city.name == "Lieu Inconnu") && !placemark.country.isEmpty {
                    city.name = placemark.country
                } else if city.name == "Quelque-part"{
                    city.name = "Lieu Inconnu"
                }
                }
                City.appDelegate().saveContext()
                self.tableView.reloadRowsAtIndexPaths([NSIndexPath(forRow: index, inSection: 0)], withRowAnimation: UITableViewRowAnimation.Automatic)
                })
        }
        refreshControl.endRefreshing()
    }

    // #pragma mark - Table view data source

    override func numberOfSectionsInTableView(tableView: UITableView?) -> Int {
        // #warning Potentially incomplete method implementation.
        // Return the number of sections.
        return 1
    }

    override func tableView(tableView: UITableView?, numberOfRowsInSection section: Int) -> Int {
        // #warning Incomplete method implementation.
        // Return the number of rows in the section.
        return cities.count
    }

    
    override func tableView(tableView: UITableView?, cellForRowAtIndexPath indexPath: NSIndexPath?) -> UITableViewCell? {
        let cell = tableView!.dequeueReusableCellWithIdentifier("cityCell") as UITableViewCell

        // Configure the cell...

        let city = cities[indexPath!.row]
        
        cell.textLabel.text = city.name
        cell.detailTextLabel.text = "Lat : \(city.latitude) // Lon : \(city.longitude)"
        
        return cell
    }
    

    
    // Override to support conditional editing of the table view.
    override func tableView(tableView: UITableView?, canEditRowAtIndexPath indexPath: NSIndexPath?) -> Bool {
        // Return NO if you do not want the specified item to be editable.
        return true
    }
    

    
    // Override to support editing the table view.
    override func tableView(tableView: UITableView?, commitEditingStyle editingStyle: UITableViewCellEditingStyle, forRowAtIndexPath indexPath: NSIndexPath?) {
        if editingStyle == .Delete {
            cities[indexPath!.row].destroy()
            cities.removeAtIndex(indexPath!.row)
            //!!!: What the FUCK !? replace that with deleteRowsAtIndexPaths once it works again :(
            //tableView?.deleteRowsAtIndexPaths([indexPath], withRowAnimation: UITableViewRowAnimation.Fade)
            tableView?.reloadSections(NSIndexSet(index: 0), withRowAnimation: UITableViewRowAnimation.Automatic)
        } else if editingStyle == .Insert {
            // Create a new instance of the appropriate class, insert it into the array, and add a new row to the table view
        }    
    }
    

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

    
    // #pragma mark - Navigation

    // In a storyboard-based application, you will often want to do a little preparation before navigation
    override func prepareForSegue(segue: UIStoryboardSegue?, sender: AnyObject?) {
        // Get the new view controller using [segue destinationViewController].
        // Pass the selected object to the new view controller.
        if segue?.identifier == "toPictureViewController" {
            let city = cities[self.tableView.indexPathForSelectedRow().row]
            let destination = segue?.destinationViewController as PictureViewController
            destination.location = FlickRPicture.Location(latitude: city.latitude.doubleValue, longitude: city.longitude.doubleValue)
        }
    }
    

}
