//
//  EditImageView.swift
//  OEM-app
//
//  Created by caiming on 14-6-23.
//  Copyright (c) 2014ๅนด caiming. All rights reserved.
//

import UIKit
protocol EditImageViewDelegate : NSObjectProtocol {
    
    func EditImageViewSuccess()
}

class EditImageView: UIView ,UICollectionViewDelegate,UICollectionViewDataSource,FiterViewDelegate{

    var imageView : UIImageView!
    var cancelBtn : UIButton!
    var delegate : EditImageViewDelegate!
    var items : NSArray!
    var selectIdx : NSInteger!
    var collectionView : UICollectionView!
    var collectionViewLayout : UICollectionViewFlowLayout!
    var fiterView : FiterView!
    
    init(frame: CGRect) {
        super.init(frame: frame)
        // Initialization code
        self.backgroundColor = UIColor(white: 99.0/255.0, alpha: 1.0)
    }
    
    func initSubviews(){
        imageView = UIImageView(frame : self.bounds)
        imageView.contentMode = .ScaleAspectFit
        imageView.autoresizingMask = .FlexibleWidth | .FlexibleWidth;
        imageView.image = UIImage(contentsOfFile:(items[selectIdx] as Item).item_url)
        self.addSubview(imageView)
        
        collectionViewLayout = UICollectionViewFlowLayout()
        collectionViewLayout.sectionInset = UIEdgeInsetsMake(0, 0, 0, 0)
        collectionViewLayout.minimumLineSpacing = 0
        collectionViewLayout.itemSize = self.bounds.size
        collectionViewLayout.scrollDirection = .Horizontal
        
        collectionView = UICollectionView(frame: self.bounds, collectionViewLayout:collectionViewLayout)
        collectionView.delegate = self
        collectionView.dataSource = self
        collectionView.pagingEnabled = true
        collectionView.clipsToBounds = false
        collectionView.backgroundColor = UIColor.clearColor()
        collectionView.showsHorizontalScrollIndicator = false
        collectionView.registerClass(PhotosCollectionViewCell.self, forCellWithReuseIdentifier:"Cell")
        self.addSubview(collectionView)
        collectionView.hidden  = true
        var indexPath =  NSIndexPath(forItem : selectIdx ,inSection : 0) as NSIndexPath!
        collectionView.scrollToItemAtIndexPath(indexPath,atScrollPosition:.Left, animated:false)
        
        cancelBtn = UIButton.buttonWithType(UIButtonType.Custom) as UIButton
        cancelBtn.addTarget(self, action:"clickCancelBtn",forControlEvents :.TouchUpInside)
        cancelBtn.setImage(UIImage(named:"btn_back@2x"),forState:.Normal)
        cancelBtn.frame = CGRect(x:16,y:22,width:34,height:34)
        cancelBtn.alpha = 0
        self.addSubview(cancelBtn)
        
        fiterView = FiterView(frame:CGRect(x:0,y:0,width:320,height:62))
        fiterView.alpha = 0
        fiterView.delegate = self
        fiterView.initSubviews()
        self.addSubview(fiterView)
    }
    
    func numberOfSectionsInCollectionView(collectionView: UICollectionView?) -> Int {
        //#warning Incomplete method implementation -- Return the number of sections
        return 1
    }
    
    func collectionView(collectionView: UICollectionView?, numberOfItemsInSection section: Int) -> Int {
        //#warning Incomplete method implementation -- Return the number of items in the section
        return self.items.count
    }
    
    func collectionView(collectionView: UICollectionView?, cellForItemAtIndexPath indexPath: NSIndexPath!) -> UICollectionViewCell? {
        let cell = collectionView?.dequeueReusableCellWithReuseIdentifier("Cell", forIndexPath: indexPath) as PhotosCollectionViewCell
        cell.photoImageView.image = UIImage(contentsOfFile:(items[indexPath.row] as Item).item_url)
        // Configure the cell
        return cell
    }
    
    override func layoutSubviews()
    {
        imageView.frame = self.bounds
        collectionView.frame = self.bounds
        collectionViewLayout.itemSize = self.bounds.size
        var indexPath =  NSIndexPath(forItem : selectIdx ,inSection : 0) as NSIndexPath!
        collectionView.scrollToItemAtIndexPath(indexPath,atScrollPosition:.Left, animated:false)
        fiterView.frame = CGRect(x:0,y:self.bounds.size.height - 76,width:self.bounds.size.width,height:62)
        
    }
    
    
    func collectionView(collectionView: UICollectionView!, didSelectItemAtIndexPath indexPath: NSIndexPath!){
        var cell  = collectionView.cellForItemAtIndexPath(indexPath) as PhotosCollectionViewCell!
        
        
        if self.cancelBtn.alpha > 0{
        
        hidefiterView()
            
        }else{
            
            showfiterView()
//           fiterView.setCurrentImage(UIImage(contentsOfFile:(items[indexPath.row] as Item).item_url))

        }
        
        
    }
    
    
    func showfiterView(){
        
            UIView.animateWithDuration(0.25,delay:0,options:.CurveEaseInOut,animations:{
            
            self.cancelBtn.alpha = 1
            self.fiterView.alpha = 1
            
            },completion:{ mbool in
            
            
            })
        
        
    }
    
    func hidefiterView()
    {
        UIView.animateWithDuration(0.25,delay:0,options:.CurveEaseInOut,animations:{
            
            self.cancelBtn.alpha = 0
            self.fiterView.alpha = 0
            
            },completion:{ mbool in
                
                
            })

    }
    func fiterViewViewChooseFiter(index : NSInteger!)
    {
        var cell = collectionView.visibleCells()[0] as PhotosCollectionViewCell
        var indexPath = collectionView.indexPathsForVisibleItems()[0] as NSIndexPath
        
        cell.photoImageView.image = Fiterimage.imageWithImage(UIImage(contentsOfFile:(items[indexPath.row] as Item).item_url),index:index)

    }
    /*
    // Only override drawRect: if you perform custom drawing.
    // An empty implementation adversely affects performance during animation.
    override func drawRect(rect: CGRect)
    {
        // Drawing code
    }
    */
    func clickCancelBtn()
    {
        self.delegate?.EditImageViewSuccess()
    }
}
