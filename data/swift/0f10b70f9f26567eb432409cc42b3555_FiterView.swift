//
//  FiterView.swift
//  OEM-app
//
//  Created by caiming on 14-6-23.
//  Copyright (c) 2014年 caiming. All rights reserved.
//

import UIKit
protocol FiterViewDelegate : NSObjectProtocol {
    
    func fiterViewViewChooseFiter(index : NSInteger!)
}
class FiterView: UIView ,UICollectionViewDelegate,UICollectionViewDataSource{

    var collectionView : UICollectionView!
    var collectionViewLayout : UICollectionViewFlowLayout!
    var items : NSArray!
    var selectIdx : NSIndexPath!
    var currentImage: UIImage!
    var exampleImage: UIImage!
    var delegate : FiterViewDelegate!
    init(frame: CGRect) {
        
        super.init(frame: frame)
        // Initialization code
    }

    func initSubviews(){
        
        collectionViewLayout = UICollectionViewFlowLayout()
        collectionViewLayout.sectionInset = UIEdgeInsetsMake(0, 10, 0, 10)
        collectionViewLayout.minimumLineSpacing = 10
        collectionViewLayout.itemSize = CGSize(width:self.bounds.size.height, height:self.bounds.size.height)
        collectionViewLayout.scrollDirection = .Horizontal
        
        collectionView = UICollectionView(frame: self.bounds, collectionViewLayout:collectionViewLayout)
        collectionView.delegate = self
        collectionView.dataSource = self
        collectionView.clipsToBounds = false
        collectionView.backgroundColor = UIColor.clearColor()
        collectionView.showsHorizontalScrollIndicator = false
        collectionView.registerClass(FiterCollectionViewCell.self, forCellWithReuseIdentifier:"Cell")
        self.addSubview(collectionView)
        items = ["原图","LOMO","黑白","复古","哥特","锐色","淡雅","酒红","青柠","浪漫","光晕","蓝调","梦幻","夜色"]
        
        exampleImage = UIImage(named:"example")
        selectIdx = NSIndexPath(forItem : 0 ,inSection : 0) as NSIndexPath!
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
        let cell = collectionView?.dequeueReusableCellWithReuseIdentifier("Cell", forIndexPath: indexPath) as FiterCollectionViewCell
        cell.photoImageView.image = Fiterimage.imageWithImage(exampleImage,index:indexPath.item)
        
        if selectIdx == indexPath
        {
            cell.backgroundColor = UIColor.yellowColor()

        }else
        {
            cell.backgroundColor = UIColor.clearColor()

        }
        
        // Configure the cell
        return cell
    }
    
    
    func collectionView(collectionView: UICollectionView!, didSelectItemAtIndexPath indexPath: NSIndexPath!){
        
        var cell  = collectionView.cellForItemAtIndexPath(indexPath) as FiterCollectionViewCell!
        
        if selectIdx
        {
            var oldCell  = collectionView.cellForItemAtIndexPath(selectIdx) as FiterCollectionViewCell!
            if oldCell{
             oldCell.backgroundColor = UIColor.clearColor()
            }
        }
        
        cell.backgroundColor = UIColor.yellowColor()
        selectIdx = indexPath
        
        self.delegate.fiterViewViewChooseFiter(selectIdx.item)

        
    }
    
//    func setCurrentImage(image: UIImage!)
//    {
////        currentImage = image
//        
//        collectionView.reloadData()
//    }
    /*
    // Only override drawRect: if you perform custom drawing.
    // An empty implementation adversely affects performance during animation.
    override func drawRect(rect: CGRect)
    {
        // Drawing code
    }
    */

}
