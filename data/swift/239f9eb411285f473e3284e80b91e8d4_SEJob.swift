//
//  SEJob.swift
//  sechi
//
//  Created by duxiaoyang on 2014-06-08.
//  Copyright (c) 2014 TopCoder. All rights reserved.
//

/*!
 * @discussion The CoreData managed object for job.
 * Changes in version 1.1 (Sechi Mobile iOS8 Swift App Update):
 * -- Update to follow the new Swift syntax.
 * -- Change removed field to NSNumber (boolean value).
 * -- Remove photos array and add 3 fields for picture URLs.
 * -- Add property and methods to manage pictures.
 * @author karolszafranski, duxiaoyang
 * @version 1.1
 * @since 1.0
 */
class SEJob: NSManagedObject {
    
    /**
     * Property for saving _c5_source field returned from API for job object.
     */
    @NSManaged var c5Source: String!
    
    /**
     * Property for saving client_account__c field returned from API for job object.
     */
    @NSManaged var clientAccountC: String!
    
    /**
     * Property for saving client_contact__c field returned from API for job object.
     */
    @NSManaged var clientContactC: String!
    
    /**
     * Property for saving client_name__c field returned from API for job object.
     */
    @NSManaged var clientNameC: String!
    
    /**
     * Property for saving contact_name__c field returned from API for job object.
     */
    @NSManaged var contactNameC: String!
    
    /**
     * Property for saving createddate field returned from API for job object.
     */
    @NSManaged var createdDate: NSDate!
    
    /**
     * Property for saving id field returned from API for job object.
     */
    @NSManaged var identifier: NSNumber!
    
    /**
     * Property for saving info_text__c field returned from API for job object.
     */
    @NSManaged var infoTextC: String!
    
    /**
     * Property for saving isdeleted field returned from API for job object.
     */
    @NSManaged var removed: NSNumber!
    
    /**
     * Property for saving job_address__c field returned from API for job object.
     */
    @NSManaged var jobAddressC: String!
    
    /**
     * Property for saving job_end_time__c field returned from API for job object.
     */
    @NSManaged var jobEndTimeC: NSDate!
    
    /**
     * Property for saving job_name__c field returned from API for job object.
     */
    @NSManaged var jobNameC: String!
    
    /**
     * Property for saving job_start_time__c field returned from API for job object.
     */
    @NSManaged var jobStartTimeC: NSDate!
    
    /**
     * Property for saving lastmodifieddate field returned from API for job object.
     */
    @NSManaged var lastModifiedDate: NSDate!
    
    /**
     * Property for saving latitude__c field returned from API for job object.
     */
    @NSManaged var latitudeC: NSNumber!
    
    /**
     * Property for saving longitude__c field returned from API for job object.
     */
    @NSManaged var longitudeC: NSNumber!
    
    /**
     * Property for saving name field returned from API for job object.
     */
    @NSManaged var name: String!
    
    /**
     * Property for saving notes__c field returned from API for job object.
     */
    @NSManaged var notesC: String!
    
    /**
     * Property for saving phone__c field returned from API for job object.
     */
    @NSManaged var phoneC: String!
    
    /**
     * Property for saving sfid field returned from API for job object.
     */
    @NSManaged var sfid: String!
    
    /**
     * Property for saving status__c field returned from API for job object.
     */
    @NSManaged var statusC: String!
    
    /**
     * Property for saving picture_s3_url__c field returned from API for job object.
     */
    @NSManaged var pictureS3UrlC: String!
    
    /**
     * Property for saving picture_s3_url_1__c field returned from API for job object.
     */
    @NSManaged var pictureS3Url1C: String!
    
    /**
     * Property for saving picture_s3_url_2__c field returned from API for job object.
     */
    @NSManaged var pictureS3Url2C: String!
    
    /**
     * Property for getting pictures associated with the job.
     */
    var pictures: [String] {
        get {
            var result = [String]()
            if pictureS3UrlC? != nil {
                result.append(pictureS3UrlC)
            }
            if pictureS3Url1C? != nil {
                result.append(pictureS3Url1C)
            }
            if pictureS3Url2C? != nil {
                result.append(pictureS3Url2C)
            }
            return result
        }
    }
    
    /**
     * Adds a new picture to the job.
     * @param picture the URL of the picture.
     */
    func addPicture(picture: String) {
        switch pictures.count {
        case 0:
            pictureS3UrlC = picture
        case 1:
            pictureS3Url1C = picture
        case 2:
            pictureS3Url2C = picture
        default:
            return
        }
    }
    
    /**
     * Removes a picture from the job.
     * @param index the index of the picture to remove.
     */
    func removePicture(index: Int) {
        switch index {
        case 0:
            pictureS3UrlC = nil
        case 1:
            pictureS3Url1C = nil
        case 2:
            pictureS3Url2C = nil
        default:
            return
        }
    }

    /**
     *  Dictionary of values needed for creating RKEntityMapping
     *
     *  @return dictionary of API key to property mappings
     */
    class var elementToPropertyMappings: [String: String] {
        get {
            return [
                "id": "identifier",
                "name": "name",
                "job_name__c": "jobNameC",
                "job_address__c": "jobAddressC",
                "notes__c": "notesC",
                "info_text__c": "infoTextC",
                "contact_name__c": "contactNameC",
                "client_name__c": "clientNameC",
                "sfid": "sfid",
                "client_contact__c": "clientContactC",
                "client_account__c": "clientAccountC",
                "status__c": "statusC",
                "phone__c": "phoneC",
                "isdeleted": "removed",
                "_c5_source": "c5Source",
                "longitude__c": "longitudeC",
                "latitude__c": "latitudeC",
                "job_end_time__c": "jobEndTimeC",
                "job_start_time__c": "jobStartTimeC",
                "createddate": "createdDate",
                "lastmodifieddate": "lastModifiedDate",
                "picture_s3_url__c": "pictureS3UrlC",
                "picture_s3_url_1__c": "pictureS3Url1C",
                "picture_s3_url_2__c": "pictureS3Url2C"
            ]
        }
    }

    /**
     *  Dictionary of values needed for creating RKEntityMapping
     *
     *  @return dictionary of property to API key mappings
     */
    class var propertyMappingsForPost: [String: String] {
        get {
            return [
                "jobNameC": "job_name__c",
                "jobAddressC": "job_address__c",
                "phoneC": "phone__c",
                "infoTextC": "info_text__c",
                "clientContactC": "client_contact__c",
                "contactNameC": "contact_name__c",
                "clientNameC": "client_name__c",
                "clientAccountC": "client_account__c",
                "pictureS3UrlC": "picture_s3_url__c",
                "pictureS3Url1C": "picture_s3_url_1__c",
                "pictureS3Url2C": "picture_s3_url_2__c"
            ]
        }
    }

    /**
     *  Dictionary of values needed for creating RKEntityMapping
     *
     *  @return dictionary of property to API key mappings
     */
    class var propertyMappingsForPut: [String: String] {
        get {
            return [
                "identifier": "id",
                "jobNameC": "job_name__c",
                "jobAddressC": "job_address__c",
                "phoneC": "phone__c",
                "jobEndTimeC": "job_end_time__c",
                "jobStartTimeC": "job_start_time__c",
                "statusC": "status__c",
                "notesC": "notes__c",
                "infoTextC": "info_text__c",
                "pictureS3UrlC": "picture_s3_url__c",
                "pictureS3Url1C": "picture_s3_url_1__c",
                "pictureS3Url2C": "picture_s3_url_2__c"
            ]
        }
    }

    /**
     * Gets the mapping for this entity.
     * @param managedObjectStore the managed object store.
     * @return the entity mapping.
     */
    class func responseMappingForManagedObjectStore(managedObjectStore: RKManagedObjectStore) -> RKEntityMapping {
        let entityMapping = RKEntityMapping(forEntityForName:"SEJob", inManagedObjectStore: managedObjectStore)
        entityMapping.addAttributeMappingsFromDictionary(self.elementToPropertyMappings)
        entityMapping.identificationAttributes = ["identifier"]
        
        return entityMapping
    }
}
