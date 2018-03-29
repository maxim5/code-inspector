//
//  SEProductsScannerViewController.swift
//  sechi
//
//  Created by duxiaoyang on 2014-06-14.
//  Copyright (c) 2014 TopCoder. All rights reserved.
//

import AVFoundation

/**
 * Protocol used by SEProductsScannerViewController to inform it's delegate about code that was read or canceling reading process.
 * @author karolszafranski, duxiaoyang
 * @version 1.0
 */
@objc protocol SEProductsScannerViewControllerDelegate: NSObjectProtocol {

    /**
     *  Method called after reading a bar code.
     *  @param productsScannerViewController SEProductsScannerViewController that read the code
     *  @param string string with decoded text of the barcode
     */
    func productsScannerViewController(productsScannerViewController: SEProductsScannerViewController, didReadString string: String)

    /**
     *  Method called if SEProducstsScannerViewController want to be dismissed.
     *  @param productsScannerViewController SEProductsScannerViewController object that want's to be dismissed.
     */
    func productsScannerViewControllerDidCancelReading(productsScannerViewController: SEProductsScannerViewController)

}

/*!
 * @discussion View controller used for displaying a code scanner used in SEProductsListViewController.
 * Changes in version 1.1 (Sechi Mobile iOS8 Swift App Update):
 * -- Update to follow the new Swift syntax.
 * @author karolszafranski, duxiaoyang
 * @version 1.1
 * @since 1.0
 */
class SEProductsScannerViewController: SEViewController, AVCaptureMetadataOutputObjectsDelegate {

    /**
     *  Delegate of the view controller
     */
    weak var delegate: SEProductsScannerViewControllerDelegate?

    /**
     *  View with preview of the camera
     */
    @IBOutlet weak var previewView: UIView!

    /**
     *  Current status of the scanner
     */
    @IBOutlet weak var statusLabel: UILabel!

    /**
     *  BOOL indicating if scanner is currently trying to find, read and decode value.
     */
    var isReading: Bool!

    /**
     *  Capture session used for video capture
     */
    var captureSession: AVCaptureSession!

    /**
     *  Video preview layer used for displaying current content seen by the camera
     */
    var videoPreviewLayer: AVCaptureVideoPreviewLayer!

    /**
     *  Tap gesture recognizer used for informing delegate that the scanner wants to be dismissed.
     */
    var tapGestureRecognizer: UITapGestureRecognizer!

    /**
     *  Setup default property values and tap gesture recognizer.
     */
    override func viewDidLoad()    {
        super.viewDidLoad()
        prefersStatusBarHidden()
        
        if self.respondsToSelector("setNeedsStatusBarAppearanceUpdate") {
            self.setNeedsStatusBarAppearanceUpdate()
        }

        self.isReading = false
        self.captureSession = nil
        
        self.tapGestureRecognizer = UITapGestureRecognizer(target: self, action: "gestureRecognizerDidRecognize:")
        self.previewView.addGestureRecognizer(self.tapGestureRecognizer)
        
        startReading()
    }

    override func prefersStatusBarHidden() -> Bool {
        return true
    }

    override func childViewControllerForStatusBarHidden() -> UIViewController? {
        return nil
    }

    /**
     *  Stops reading and inform delegate that scanner wants to be dismissed.
     *  @param gestureRecognizer gesture recognizer object that called the message
     */
    func gestureRecognizerDidRecognize(gestureRecognizer: UIGestureRecognizer) {
        stopReading()
        self.delegate?.productsScannerViewControllerDidCancelReading(self)
    }

    /**
     *  Setup AVCaptureSession and start reading the codes from the camera stream.
     *  @return YES if scanner did setup correctly and is waiting for code to show in camera. NO otherwise.
     */
    func startReading() -> Bool {
        var error: NSError?
        
        let captureDevice = AVCaptureDevice.defaultDeviceWithMediaType(AVMediaTypeVideo)
        
        let input = AVCaptureDeviceInput(device: captureDevice, error: &error)
        
        if error != nil {
            NSLog("%@", error!.localizedDescription)
            return false
        }
        
        self.captureSession = AVCaptureSession()
        self.captureSession.addInput(input)
        
        let captureMetadataOutput = AVCaptureMetadataOutput()
        self.captureSession.addOutput(captureMetadataOutput)
        
        let dispatchQueue = dispatch_queue_create("myQueue", nil)
        captureMetadataOutput.setMetadataObjectsDelegate(self, queue: dispatchQueue)
        captureMetadataOutput.metadataObjectTypes = [AVMetadataObjectTypeCode39Code, AVMetadataObjectTypeCode39Mod43Code]
        
        self.videoPreviewLayer = AVCaptureVideoPreviewLayer(session: self.captureSession)
        self.videoPreviewLayer.videoGravity = AVLayerVideoGravityResizeAspectFill
        self.videoPreviewLayer.frame = self.previewView.layer.bounds
        self.previewView.layer.addSublayer(self.videoPreviewLayer)
        
        self.captureSession.startRunning()

        self.previewView.bringSubviewToFront(self.statusLabel)
        
        return true
    }

    /**
     *  Stops capture and reading the codes.
     */
    func stopReading() {
        self.captureSession?.stopRunning()
        self.captureSession = nil
        
        self.videoPreviewLayer?.removeFromSuperlayer()
    }


    /**
     *  Method called by AVCaptureMetadataOutput objects, when metadata object is found.
     *  This method informs
     *  @param captureOutput   captureOutput that found metadata object
     *  @param metadataObjects metadataObjects found
     *  @param connection      connection of capture input and capture output
     */
    func captureOutput(captureOutput: AVCaptureOutput, didOutputMetadataObjects metadataObjects: [AnyObject], fromConnection connection: AVCaptureConnection) {
        if metadataObjects.count > 0 {
            
            let metadataObj = metadataObjects[0] as AVMetadataMachineReadableCodeObject
            
            if metadataObj.type == AVMetadataObjectTypeCode39Code || metadataObj.type == AVMetadataObjectTypeCode39Mod43Code {
                dispatch_async(dispatch_get_main_queue(), {
            		self.statusLabel.text = metadataObj.stringValue
            	})

            	dispatch_async(dispatch_get_main_queue(), {
            		self.stopReading()
            	})
                
                self.delegateReadedCode(metadataObj.stringValue)
                
                self.isReading = false

            }
        }
    }


    /**
     *  Inform the delegate that the metadata object was read.
     *  @param readedCode decoded metadata message
     */
    func delegateReadedCode(readedCode: String) {
        self.delegate?.productsScannerViewController(self, didReadString: readedCode)
    }

}
