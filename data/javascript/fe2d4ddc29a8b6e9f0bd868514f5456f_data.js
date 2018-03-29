// **********************************************
// 	Data Access Object 
// **********************************************

function DAO () {
	// Setup SQL statements
	// Categories
	var sqlCreateCategoriesTable = "CREATE TABLE IF NOT EXISTS" + 
		"'categories' (value INTEGER PRIMARY KEY, label TEXT); GO;";
	var sqlCreateCategory = "INSERT INTO 'categories' (label) VALUES (?); GO;";
	var sqlUpdateCategory = "UPDATE categories SET label=? WHERE value=?; GO;";
	var sqlRetrieveCategories = "SELECT * FROM categories; GO;";
	var sqlDeleteCategory = "DELETE FROM categories WHERE value=?; GO;";
	
	// Vehicles
	var sqlCreateVehiclesTable = "CREATE TABLE IF NOT EXISTS 'vehicles' " + 
		"(value INTEGER PRIMARY KEY, label TEXT, lastmileage INTEGER, rate REAL); GO;";
	var sqlCreateVehicle = "INSERT INTO 'vehicles' (label, lastmileage, rate) " +
		"VALUES (?, ?, ?); GO;";
	var sqlUpdateVehicle = 	"UPDATE 'vehicles' SET label=?, lastmileage=?, rate=? " +
		"WHERE value=?; GO;";
	var sqlRetrieveVehicles = "SELECT * FROM vehicles; GO;";
	var sqlDeleteVehicle = "DELETE FROM vehicles WHERE value=?; GO;";
	
	// MileageEvents
	var sqlCreateMileageEventsTable = "CREATE TABLE IF NOT EXISTS 'mileageEvents' " +
		"(id INTEGER PRIMARY KEY, date INTEGER, endDate INTEGER, dateformat TEXT, " +
		"begMiles TEXT, endMiles TEXT, mileage TEXT, purpose TEXT, " + 
		"destination TEXT, notes TEXT, category INTEGER, vehicle INTEGER); GO;";
	var sqlCreateMileageEvent = "INSERT INTO 'mileageEvents' (date, endDate, " +
		"dateformat, begMiles, endMiles, mileage, purpose, destination, notes, " +
		"category, vehicle) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?); GO;";
	var sqlUpdateMileageEvent = "UPDATE 'mileageEvents' SET date=?, endDate=?, " +
		"dateformat=?, begMiles=?, endMiles=?, mileage=?, purpose=?, " +
		"destination=?, notes=?, category=?, vehicle=? WHERE id=?; GO;";
	var sqlUpdateMileageEventsWithCategory = "UPDATE 'mileageEvents' " +
		"SET category=1 WHERE category=?; GO;";
	var sqlUpdateMileageEventsWithVehicle = "UPDATE 'mileageEvents' " +
		"SET vehicle=1 WHERE vehicle=?; GO;";	
	var sqlRetrieveMileageEvents = "SELECT * FROM mileageEvents; GO;";
	var sqlDeleteMileageEvent = "DELETE FROM mileageEvents WHERE id=?; GO;";
	var sqlPurgeMileageEvents = "DELETE FROM mileageEvents WHERE (date > ? AND date < ?); GO;";
	var sqlGetEarliestDate = "SELECT MIN(date) AS test FROM 'mileageEvents'; GO;";

// **********************************************
// Initialize database
// **********************************************
	this.init = function() {
		//Mojo.Log.info("Entering db init");

		this.db = null;
		var	databaseName = "ext:MilesDB", 				// required
			version = "0.1", 							// required
			displayName = Miles.appName + " database"; 	// optional


	    this.db = openDatabase(databaseName, version, displayName);
		
		if (!this.db) {
			this.errorHandler(null, {code: '-111', message: 'Could not create database!'});
		}
	    this.db.transaction((function (inTransaction) {
			inTransaction.executeSql(sqlCreateCategoriesTable, [], function() { }, 
				this.errorHandler
			);
	    	inTransaction.executeSql(sqlCreateVehiclesTable, [], function() { }, 
				this.errorHandler
			);
	    	inTransaction.executeSql(sqlCreateMileageEventsTable, [], 
				function () { }, 
				this.errorHandler
			);
	    }).bind(this));
		//Mojo.Log.info("****** Leaving db init *******");
  }; // End init().
  
// **********************************************
// Category functions
// **********************************************

	this.createCategory = function (inCategoryObject, inCallback) {
		//Mojo.Log.info("Entering db createCategory()");
		
		var cat = inCategoryObject;

	    this.db.transaction((function (inTransaction) { 
		inTransaction.executeSql(sqlCreateCategory, [ cat.label ], 
			function(inTransaction, inResultSet){
				//Mojo.Log.info("DB results: %j", inResultSet);
				var results = inResultSet.insertId;
				inCallback(results);
			},
			this.errorHandler);
	    }).bind(this));

  	}; // End createCategory().

	// Update Category
	this.updateCategory = function (inCategoryObject) {
		//Mojo.Log.info("Entering db updateCategory()");
		
		var cat = inCategoryObject;
		
		this.db.transaction((function (inTransaction) {
			inTransaction.executeSql(sqlUpdateCategory, 
				[cat.label, cat.value],
				function() { },
				this.errorHandler);
	    }).bind(this));

	}; // End updateCategory().
	
	this.retrieveCategories = function (inCallBack) {
		//Mojo.Log.info("Entering db retrieveCategories()");
		
		this.db.transaction((function (inTransaction) {
			inTransaction.executeSql(sqlRetrieveCategories,
			[ ],
			function (inTransaction, inResultSet) {
				//Mojo.Log.info("Retrieve Categories Success");
				var results = [], i;
				if (inResultSet.rows) {
					for (i = 0; i < inResultSet.rows.length; i++) {
						//Mojo.Log.info("Result 1: %j", inResultSet.rows.item(i));
						// Use clone of object to avoid problems with immutability
						results.push(Object.clone(inResultSet.rows.item(i)));
					}
				}
				//Mojo.Log.info("Category Results in db: %j", results);
				inCallBack(results);
			},
			this.errorHandler);
		 }).bind(this));
	};

	this.deleteCategory = function (inCategory) {
		//Mojo.Log.info("Entering db deleteCategory()");
		// Reset any categories with this value to default (0) value
		// Delete Category
		this.db.transaction((function (inTransaction) {
			inTransaction.executeSql(sqlDeleteCategory,
				[inCategory], 
				function(){
					Mojo.Log.info("Deleted Category", inCategory);
				},
				this.errorHandler);
		 }).bind(this));
	};
	
	this.updateEventsWithCategory = function (inCategory) {
		Mojo.Log.info("Entering db updateEventsWithCategory()");
		// Reset any categories with this value to default (0) value
		this.db.transaction((function (inTransaction) {
			inTransaction.executeSql(sqlUpdateMileageEventsWithCategory,
				[inCategory],
				function (inTransaction, inResultSet) { 
					Mojo.Log.info("SQL =", sqlUpdateMileageEventsWithCategory);
					Mojo.Log.info("Updated MileageEvents With Category", inCategory);
					Mojo.Log.info("Results: %j", inResultSet.rows);
				},
				function(inTransaction, inError) {
				    Mojo.Controller.errorDialog(
				     	"DAO ERROR - (" + inError.code + ") : " + inError.message
				    );
				}
			);
		 }).bind(this));
	};
	
// **********************************************
// Vehicle functions
// **********************************************

	this.createVehicle = function  (inVehicleObject, inCallback) {
		//Mojo.Log.info("Entering db createVehicle()");
		var veh = inVehicleObject;

	    this.db.transaction((function (inTransaction) { 
	    	inTransaction.executeSql(sqlCreateVehicle, 
				[ veh.label, veh.lastmileage, veh.rate ], 
				function(inTransaction, inResultSet) {
					//Mojo.Log.info("DB results: %j", inResultSet);
					var results = inResultSet.insertId;
					inCallback(results);
				},
				this.errorHandler);
		 }).bind(this));

  	}; // End createVehicle().

	// Update Vehicle
	this.updateVehicle = function (inVehicleObject, inCallback) {
		//Mojo.Log.info("Entering db updateVehicle()");
		var veh = inVehicleObject;
		
		this.db.transaction((function (inTransaction) {
			inTransaction.executeSql(sqlUpdateVehicle, 
				[veh.label, veh.lastmileage, veh.rate, veh.value],
				inCallback(),
				this.errorHandler);
		 }).bind(this));
	}; // End updateVehicle().
	
	this.retrieveVehicles = function (inCallBack) {
		//Mojo.Log.info("Entering db retrieveVehicles()");
		
		this.db.transaction((function (inTransaction) {
			inTransaction.executeSql(sqlRetrieveVehicles,
				[ ],
				function (inTransaction, inResultSet) {
					var results = [], i;
					if (inResultSet.rows) {
						for (i = 0; i < inResultSet.rows.length; i++) {
							// Use clone of object to avoid problems with immutability
							results.push(Object.clone(inResultSet.rows.item(i)));
						}
					}
					inCallBack(results);
				},
				this.errorHandler);
		 }).bind(this));
	};

	this.deleteVehicle = function (inVehicle) {
		//Mojo.Log.info("Entering db deleteVehicle()");
		// Reset any event vehicles with this value to default (0) value
		// Delete Vehicle
		this.db.transaction((function (inTransaction) {
			inTransaction.executeSql(sqlDeleteVehicle,
				[inVehicle], 
				function(){
				},
				this.errorHandler);
			inTransaction.executeSql(sqlUpdateMileageEventsWithVehicle,
				[inVehicle],
				function () { },
				this.errorHandler);
		 }).bind(this));
	};

// **********************************************
// MileageEvent functions
// **********************************************

	this.createMileageEvent = function  (inMileageEventObject, inCallback) {
		var mile = inMileageEventObject;

	    this.db.transaction((function (inTransaction) { 
	    	inTransaction.executeSql(sqlCreateMileageEvent, 
				[ mile.date, mile.endDate, mile.dateformat, mile.begMiles,
					mile.endMiles, mile.mileage, mile.purpose, mile.destination,
					mile.notes, mile.category, mile.vehicle ], 
				function(inTransaction, inResultSet){
					//Mojo.Log.info("DB results: %j", inResultSet);
					var results = inResultSet.insertId;
					inCallback(results);
				},
				this.errorHandler);
		 }).bind(this));

  	}; // End createMileageEvent().

	// Update MileageEvent
	this.updateMileageEvent = function (inMileageEventObject, inCallback) {
		var mile = inMileageEventObject;
		
		this.db.transaction((function (inTransaction) {
			inTransaction.executeSql(sqlUpdateMileageEvent, 
				[ mile.date, mile.endDate, mile.dateformat, mile.begMiles,
					mile.endMiles, mile.mileage, mile.purpose, mile.destination,
					mile.notes, mile.category, mile.vehicle, mile.id ], 
				inCallback(), 
				this.errorHandler);
		 }).bind(this));
	}; // End updateMileageEvent().
	
	this.retrieveMileageEvents = function (inCallBack) {
		this.db.transaction((function (inTransaction) {
			inTransaction.executeSql(sqlRetrieveMileageEvents,
			[ ],
			function (inTransaction, inResultSet) {
				var results = [], i;
				if (inResultSet.rows) {
					for (i = 0; i < inResultSet.rows.length; i++) {
						// Use clone of object to avoid problems with immutability
						results.push(Object.clone(inResultSet.rows.item(i)));
					}
				}
				inCallBack(results);
			},
				this.errorHandler);
		 }).bind(this));
	}; // End retrieveMileageEvents().

	// Delete a single mileage event
	this.deleteMileageEvent = function (inEventId) {
		// Delete MileageEvent
		this.db.transaction((function (inTransaction) {
			inTransaction.executeSql(sqlDeleteMileageEvent,
				[inEventId], 
				function ( ) {
				},
				this.errorHandler);
		 }).bind(this));
	};
	
	// Delete mileage events within a date range
	this.purgeMileageEvents = function (inDate1, inDate2) {
		// Delete MileageEvent
		this.db.transaction((function (inTransaction) {
			inTransaction.executeSql(sqlPurgeMileageEvents,
				[inDate1, inDate2], 
				function ( ) {
				},
				this.errorHandler);
		 }).bind(this));
	};

	// Select earliest beginDate in database
	this.getEarliestDate = function (inCallBack) {
		this.db.transaction((function (inTransaction, inResultSet) {
			inTransaction.executeSql(sqlGetEarliestDate,
				[],
				function(inTransaction, inResultSet){
					inCallBack(inResultSet.rows.item(0).test);
				},
				this.errorHandler);
		 }).bind(this));
	};

// **********************************************
// Error Handler
// **********************************************

	this.errorHandler = function(inTransaction, inError) {
		Mojo.Log.info("DAO Error", inError.message);
	    Mojo.Controller.errorDialog(
	     	"DAO ERROR - (" + inError.code + ") : " + inError.message
	    );

  	}; // End errorHandler().
}

var dao = new DAO();
