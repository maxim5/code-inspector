//
//  MTZBackgroundViewController.swift
//  Matt Zanchelli
//
//  Created by Matt Zanchelli on 6/7/14.
//  Copyright (c) 2014 Matt Zanchelli. All rights reserved.
//

import UIKit

@objc(MTZBackgroundViewController)
class BackgroundViewController: UIViewController, UITableViewDataSource, UITableViewDelegate {

	@IBOutlet var tableView : UITableView
	
//	var content : TimelineEvent[]
	var content : NSArray = []
	
	init(coder aDecoder: NSCoder!) {
		super.init(coder: aDecoder)
	}
	
	override func viewDidLoad() {
		super.viewDidLoad()
		
		tableView.contentInset = UIEdgeInsets(top: 44+31, left: 0, bottom: 0, right: 0)
		
		let e0 = TimelineEvent(date: "Early Life", title: "Interested in Computers & Computer Graphics.", descriptionString: "As a kid, I was fascinated with computers and what was possible with them. I made artwork in various programs to give to my parents. I much preferred drawing with computer programs over traditional crayons and markers.", image: UIImage(named: "Computer Graphics.jpg"))
		
		let e1 = TimelineEvent(date: "Dec. 2004", title: "Owned my first iPad.", descriptionString: "As I unboxed the silver iPod mini, my eyes lit up. I suddenly knew what I would be doing for the rest of my life: building incredible products and customer experiences like this one. Every day of my life is working towards that goal.", image: UIImage(named: "iPod.jpg"))
		
		let e2 = TimelineEvent(date: "Aug. 2006", title: "Bought my first Mac.", descriptionString: "After a couple years of collecting iPods, I knew it was time to also get a Mac. I used them occasionally at school and found them to be much more enjoyable to use than any other computer I had ever used before.", image: UIImage(named:"MacBook.jpg"))
		
		let e3 = TimelineEvent(date: "Aug. 2007", title: "Started High School.", descriptionString: "By this time, I was developing my interest in design. I spent a lot of time on design forums, like MacThemes, where I was inspired by and learned from fantastic Mac designers. I started designing icons and user interfaces for fun.", image: UIImage(named: "Started HS.jpg"))
		
		let e4 = TimelineEvent(date: "Oct. 2007", title: "Began Freelancing.", descriptionString: "I had enough experience with designing and building websites that I could do freelance design and web work. I loved my time designing logos, business cards, and websites for friends and locals in my community.", image: UIImage(named: "10th Grade.jpg"))
		
		let e5 = TimelineEvent(date: "Apr. 2008", title: "Started DJ BMZ", descriptionString: "I became very interested in music and started collecting equipment to playback and manipulate music. My neighbour and I started DJing parties and other events. Eventually that turned into a profitable business thatâs still running to this day.", image: UIImage(named: "DJ BMZ.jpg"))
		
		let e6 = TimelineEvent(date: "Aug. 2011", title: "Started College.", descriptionString: "I figured out that the design Iâm most interested in was in software, so I decided to go to school for Computer Science. I met by best friend, Peter, who taught me how to take what I learned in class and build native apps for Mac and iOS.", image: UIImage(named: "College.jpg"))
		
		content = [e0, e1, e2, e3, e4, e5, e6]
	}
	
	override func viewDidAppear(animated: Bool) {
		super.viewDidAppear(animated)
		tableView.flashScrollIndicators()
	}
	
	func flashScrollIndicators() {
		tableView.flashScrollIndicators()
	}
	
	func scrollToTop() {
		tableView.setContentOffset(CGPoint(x: 0, y: -self.tableView.contentInset.top), animated: true);
	}
	
	// MARK: UITableViewDataSource
	
	func tableView(tableView: UITableView?, cellForRowAtIndexPath indexPath: NSIndexPath?) -> UITableViewCell? {
		
		let cell = tableView!.dequeueReusableCellWithIdentifier("Event", forIndexPath: indexPath!) as UITableViewCell
		
//		if ( !cell ) {
//			cell = [[UITableViewCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:@"Event"];
//			cell.selectionStyle = UITableViewCellSelectionStyleNone;
//		}
		
		if let path = indexPath {
			let timelineStripeView = cell.contentView?.viewWithTag(222)
			if !timelineStripeView {
				let view = UIView(frame: CGRect(x: 128, y: 0, width: 1, height: cell.frame.size.height))
				view.tag = 222
				view.backgroundColor = UIColor(white: 0.85, alpha: 1)
				cell.contentView?.addSubview(view)
			}
			
			var timelineEventView = cell.contentView?.viewWithTag(111) as TimelineEventView?
			if !timelineEventView {
				let view = TimelineEventView(frame: cell.bounds)
				view.tag = 111
				cell.contentView?.addSubview(view)
				timelineEventView = view
			}
			
			// WARNING: If content was a TimelineEvent[], no need to "as"
			timelineEventView?.setUpWithTimelineEvent(content[path.row] as TimelineEvent)
		}
		
		return cell
	}
	
	func numberOfSectionsInTableView(tableView: UITableView) -> NSInteger {
		return 1
	}
	
	func tableView(tableView: UITableView, numberOfRowsInSection: NSInteger) -> NSInteger {
		return content.count
	}
}