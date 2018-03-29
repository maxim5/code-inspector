//
//  FormViewController.swift
//  Eventual
//
//  Created by Peng Wang on 10/6/14.
//  Copyright (c) 2014 Eventual App. All rights reserved.
//

import UIKit

@objc(ETFormViewController) class FormViewController: UIViewController {

    // MARK: - Input State
    
    var currentInputView: UIView?
    var previousInputView: UIView?
    
    var shouldGuardSegues = true
    private var isAttemptingDismissal = false
    private var waitingSegueIdentifier: String? // Temporarily track the segue that needs to wait.
    
    // Override this default implementation if custom focusing is desired.
    func focusInputView(view: UIView) -> Bool {
        let responder = view as UIResponder
        return responder.becomeFirstResponder()
    }
    // Override this default implementation if custom blurring is desired.
    func blurInputView(view: UIView) -> Bool {
        let responder = view as UIResponder
        return responder.resignFirstResponder()
    }
    // Override this default implementation if input view has separate dismissal.
    func shouldDismissalSegueWaitForInputView(view: UIView) -> Bool {
        return view is UITextField || view is UITextView
    }
    // This must be overridden for dismissal segues to be active.
    func isDismissalSegue(identifier: String) -> Bool {
        return false
    }
    // Override this default implementation if input view has separate dismissal.
    func dismissalWaitDurationForInputView(view: UIView?) -> NSTimeInterval {
        return 0.3
    }

    func initializeInputViewsWithFormDataObject() {
        for valueKeyPath in self.formDataValueToInputViewKeyPathsMap.keys {
            self.updateInputViewWithFormDataValue(valueKeyPath, commit: true)
        }
    }

    func shiftCurrentInputViewToView(view: UIView?) {
        // Guard.
        if view === self.currentInputView { return }
        // Re-focus previously focused input.
        let shouldRefocus = view == nil && !self.isAttemptingDismissal
        if shouldRefocus, let previousInputView = self.previousInputView {
            self.focusInputView(previousInputView)
            // Update.
            self.currentInputView = previousInputView
            return
        }
        var canPerformWaitingSegue = view == nil
        var shouldPerformWaitingSegue = canPerformWaitingSegue
        // Blur currently focused input.
        if let currentInputView = self.currentInputView {
            self.blurInputView(currentInputView)
            if canPerformWaitingSegue {
                shouldPerformWaitingSegue = self.shouldDismissalSegueWaitForInputView(currentInputView)
            }
        }
        // Update.
        self.previousInputView = self.currentInputView
        self.currentInputView = view
        // Retry any waiting segues.
        if shouldPerformWaitingSegue {
            self.performDismissalSegueWithWaitDuration()
        }
    }

    func performDismissalSegueWithWaitDuration() {
        if let identifier = self.waitingSegueIdentifier {
            self.isAttemptingDismissal = false
            let duration = self.dismissalWaitDurationForInputView(self.previousInputView)
            dispatch_after(duration) {
                self.performSegueWithIdentifier(identifier, sender: self)
                self.waitingSegueIdentifier = nil
            }
        }
    }
    
    // MARK: Overrides
    
    override func shouldPerformSegueWithIdentifier(identifier: String?, sender: AnyObject?) -> Bool {
        if let identifier = identifier where self.shouldGuardSegues {
            var should = self.currentInputView == nil
            // Set up waiting segue.
            if !should && self.isDismissalSegue(identifier) {
                self.isAttemptingDismissal = true
                self.waitingSegueIdentifier = identifier
                self.previousInputView = nil
                self.shiftCurrentInputViewToView(nil)
            }
            return should
        }
        return super.shouldPerformSegueWithIdentifier(identifier, sender: sender)
    }
    
    // MARK: - Data Handling
    
    var revalidatePerChange = true
    
    var dismissAfterSaveSegueIdentifier: String? {
        return nil
    }
    
    var validationResult: (isValid: Bool, error: NSError?) = (false, nil) {
        didSet { self.didValidateFormData() }
    }
    
    @IBAction func completeEditing(sender: AnyObject) {
        let (didSave, error) = self.saveFormData()
        if let error = error {
            self.didReceiveErrorOnFormSave(error)
        }
        if !didSave {
            self.toggleErrorPresentation(true)
        } else {
            if let identifier = self.dismissAfterSaveSegueIdentifier
                   where self.shouldPerformSegueWithIdentifier(identifier, sender: self)
            {
                self.performSegueWithIdentifier(identifier, sender: self)
            }
            self.didSaveFormData()
        }
    }

    func saveFormData() -> (didSave: Bool, error: NSError?) {
        fatalError("Unimplemented method.")
    }
    func validateFormData() -> (isValid: Bool, error: NSError?) {
        fatalError("Unimplemented method.")
    }
    func toggleErrorPresentation(visible: Bool) {
        fatalError("Unimplemented method.")
    }
    // Override this for data update handling.
    func didChangeFormDataValue(value: AnyObject?, atKeyPath keyPath: String) {}
    // Override this for custom save error handling.
    func didReceiveErrorOnFormSave(error: NSError) {}
    // Override this for custom save success handling.
    func didSaveFormData() {}
    // Override this for custom validation handling.
    func didValidateFormData() {}
    
    // MARK: - Data Binding

    var formDataObject: AnyObject {
        fatalError("Unimplemented accessor.")
    }
    var formDataValueToInputViewKeyPathsMap: [String: AnyObject] {
        fatalError("Unimplemented accessor.")
    }
    // Override this default implementation if custom observer adding is desired.
    func setUpFormDataObjectForKVO(options: NSKeyValueObservingOptions = .Initial | .New | .Old) {
        for valueKeyPath in self.formDataValueToInputViewKeyPathsMap.keys {
            self.formDataObject.addObserver(self, forKeyPath: valueKeyPath, options: options, context: &sharedObserverContext)
        }
    }
    // Override this default implementation if custom observer removal is desired.
    func tearDownFormDataObjectForKVO() {
        for valueKeyPath in self.formDataValueToInputViewKeyPathsMap.keys {
            self.formDataObject.removeObserver(self, forKeyPath: valueKeyPath, context: &sharedObserverContext)
        }
    }
    func infoForInputView(view: UIView) -> (valueKeyPath: String, emptyValue: AnyObject) {
        fatalError("Unimplemented accessor.")
    }
    // Override this default implementation if custom data updating is desired.
    func updateFormDataForInputView(view: UIView, validated: Bool = false) {
        let rawValue: AnyObject? = self.valueForInputView(view)
        let (valueKeyPath, emptyValue: AnyObject) = self.infoForInputView(view)
        var value: AnyObject? = rawValue
        var error: NSError?
        // TODO: KVC validation support.
        if !validated || self.formDataObject.validateValue(&value, forKeyPath: valueKeyPath, error: &error) {
            self.formDataObject.setValue(value ?? emptyValue, forKeyPath: valueKeyPath)
        }
        if let error = error {
            println("Validation error: \(error)")
        }
        if validated {
            if let viewKeyPaths = self.formDataValueToInputViewKeyPathsMap[valueKeyPath] as? [String] {
                // FIXME: This may cause redundant setting.
                for viewKeyPath in viewKeyPaths {
                    if let view = self.valueForKeyPath(viewKeyPath) as? UIView {
                        self.setValue(value ?? emptyValue, forInputView: view)
                    }
                }
            } else {
                self.setValue(value ?? emptyValue, forInputView: view)
            }
        }
    }
    // Override this default implementation if custom view updating is desired.
    func updateInputViewsWithFormDataObject(customFormDataObject: AnyObject? = nil) {
        var formDataObject: AnyObject = customFormDataObject ?? self.formDataObject
        for valueKeyPath in self.formDataValueToInputViewKeyPathsMap.keys {
            self.updateInputViewWithFormDataValue(valueKeyPath, commit: true)
        }
    }
    // Override this default implementation if custom value setting is desired.
    func updateInputViewWithFormDataValue(valueKeyPath: String, commit shouldCommit: Bool = false) {
        // Arrays are supported for multiple inputs mapping to same value key-path.
        if let viewKeyPath: AnyObject = self.formDataValueToInputViewKeyPathsMap[valueKeyPath] {
            let viewKeyPaths: [String]
            if let array = viewKeyPath as? [String] {
                viewKeyPaths = array
            } else if let string = viewKeyPath as? String {
                viewKeyPaths = [ string ]
            } else {
                fatalError("Unsupported view key-path type.")
            }
            for viewKeyPath in viewKeyPaths {
                if let value: AnyObject = formDataObject.valueForKeyPath(valueKeyPath),
                       view = self.valueForKeyPath(viewKeyPath) as? UIView
                {
                    self.setValue(value, forInputView: view, commit: shouldCommit)
                }
            }
        }
    }
    // Override this default implementation if custom value getting is desired.
    func valueForInputView(view: UIView) -> AnyObject? {
        if let textField = view as? UITextField {
            return textField.text
        } else if let textView = view as? UITextView {
            return textView.text
        } else if let datePicker = view as? UIDatePicker {
            return datePicker.date
        } else {
            fatalError("Unsupported input-view type")
        }
        return nil
    }
    // Override this default implementation if custom value setting is desired.
    func setValue(value: AnyObject, forInputView view: UIView, commit shouldCommit: Bool = false) {
        if let text = value as? String {
            if let textField = view as? UITextField {
                if text == textField.text { return }
                textField.text = text
            } else if let textView = view as? UITextView {
                if text == textView.text { return }
                textView.text = text
            }
        } else if let date = value as? NSDate, datePicker = view as? UIDatePicker {
            if date == datePicker.date { return }
            datePicker.date = date
        } else {
            fatalError("Unsupported input-view type")
        }
        if shouldCommit {
            self.didCommitValueForInputView(view)
        }
    }
    // Override this for custom value commit handling.
    func didCommitValueForInputView(view: UIView) {}
    
    // MARK: Overrides
    
    override func observeValueForKeyPath(keyPath: String, ofObject object: AnyObject,
                  change: [NSObject: AnyObject], context: UnsafeMutablePointer<Void>)
    {
        if context != &sharedObserverContext {
            return super.observeValueForKeyPath(keyPath, ofObject: object, change: change, context: context)
        }
        let (oldValue: AnyObject?, newValue: AnyObject?, didChange) = change_result(change)
        if !didChange { return }
        if let formDataObject = self.formDataObject as? NSObject,
               object = object as? NSObject
               where (object === formDataObject)
        {
            self.didChangeFormDataValue(newValue, atKeyPath: keyPath)
            if self.revalidatePerChange {
                self.validationResult = self.validateFormData()
            }
        }
    }
    
}

// MARK: - UITextViewDelegate

extension FormViewController: UITextViewDelegate {
    
    func textViewDidBeginEditing(textView: UITextView) {
        self.shiftCurrentInputViewToView(textView)
    }

    func textViewDidChange(textView: UITextView) {
        self.updateFormDataForInputView(textView)
    }

    func textViewDidEndEditing(textView: UITextView) {
        self.updateFormDataForInputView(textView, validated: true)
        self.didCommitValueForInputView(textView)
        if self.currentInputView == textView {
            self.shiftCurrentInputViewToView(nil)
        }
    }
    
}

// MARK: - UIDatePicker Handling

extension FormViewController {
    
    func datePickerDidChange(datePicker: UIDatePicker) {
        self.updateFormDataForInputView(datePicker, validated: true)
    }
    
    func datePickerDidEndEditing(datePicker: UIDatePicker) {
        if self.currentInputView == datePicker {
            self.shiftCurrentInputViewToView(nil)
        }
    }
    
}