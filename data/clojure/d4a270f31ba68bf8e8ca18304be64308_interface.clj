
(ns picasagrok.samus.db.interface)

(defprotocol TanarkhDatabase
	"An abstraction over the different databases (and mocked databases) that we can use."
	;; (create-entry db {:md5 "3493abdeffe33", :tags #{"Spike", "Buffy, The Vampire Slayer"}, :data (make-array Byte/TYPE 10), :type :image, :extension "jpg"})
	(createEntry [this Ewithmd5] [this md5 E] "Creates an entry with the information found in the hash argument.")
	(deleteEntryByMd5 [this md5] "")
	(retrieveTable [this] "")
	(retrieveAllEntries [this] "")
	(retrieveEntryByMd5 [this md5] "")
	(retrieveLogs [this] "")
	(clearTable [this] "")
	(clearLogs [this] "")
	(clearAll [this] "")
	(shortDescription [this] "")
	)
	
