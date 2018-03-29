package com.alphadog.tribe.db.test;

import java.util.List;

import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.test.ActivityInstrumentationTestCase2;

import com.alphadog.tribe.activities.ReviewListingActivity;
import com.alphadog.tribe.db.TribeDatabase;
import com.alphadog.tribe.db.ReviewsTable;
import com.alphadog.tribe.db.Table;
import com.alphadog.tribe.models.Review;

public class ReviewTableTest extends ActivityInstrumentationTestCase2<ReviewListingActivity> {
	
	private TribeDatabase testDBInstance;
	private SQLiteDatabase db;
	private Table<Review> reviewsTable;
	
	public ReviewTableTest() {
		super("com.alphadog.tribe", ReviewListingActivity.class);
	}
	
	@Override
	public void setUp() throws Exception {
		testDBInstance = new TribeDatabase(getActivity(), "GRAPEVINE_TEST");
		reviewsTable = new ReviewsTable(testDBInstance);
		db = testDBInstance.getWritableDatabase();
	}
	
	public void testCreationOfNewReviewRecord() {
		String heading = "I hate this place really!";
		String description = "This place has been a real pain in butt, like forever now.";
		Review reviewRecord = new Review(-1, heading, description, "http://temp.com/image","123456", "456733",1, null, null, null); 
		
		reviewRecord = reviewsTable.create(reviewRecord);
		
		assertNotSame(-1, reviewRecord.getId());
		assertEquals(heading, reviewRecord.getHeading());
		assertEquals(description, reviewRecord.getDescription());
		assertEquals("http://temp.com/image", reviewRecord.getImageUrl());
		assertEquals("456733", reviewRecord.getLatitude());
		assertEquals("123456", reviewRecord.getLongitude());
		assertEquals(true, reviewRecord.isLike());
	}

	public void testLookupOfReviewsWithReviewId() {
		
		String description1 = "This place is so sucky, that I almost never ever go there!";
		String description2 = "This is going to be a big surprise for you guys, it's amazing";
		String heading1 = "Bad Place";
		String heading2 = "Good Place";
		Review reviewRecordOne = new Review(-1, heading1, description1, "http://temp.com/image","123456", "456733",1, null, null, null); 
		Review reviewRecordTwo = new Review(-1, heading2, description2, "http://temp.com/image","123456", "456733",0, null, null, null); 
		
		reviewRecordOne = reviewsTable.create(reviewRecordOne);
		reviewRecordTwo = reviewsTable.create(reviewRecordTwo);
		
		Review fetchedReview = reviewsTable.findById(reviewRecordOne.getId());
		
		assertEquals(reviewRecordOne, fetchedReview);
	}
	
	public void testLookupOfAllReviewRecords() {
		String description1 = "This place is so sucky, that I almost never ever go there!";
		String description2 = "This is going to be a big surprise for you guys, it's amazing";
		String heading1 = "Bad Place";
		String heading2 = "Good Place";
		Review reviewRecordOne = new Review(-1, heading1, description1, "http://temp.com/image","123456", "456733",1, null, null, null); 
		Review reviewRecordTwo = new Review(-1, heading2, description2, "http://temp.com/image","123456", "456733",0, null, null, null); 
		
		reviewRecordOne = reviewsTable.create(reviewRecordOne);
		reviewRecordTwo = reviewsTable.create(reviewRecordTwo);
		
		List<Review> reviewRecords = reviewsTable.findAll();
		
		assertEquals(2, reviewRecords.size());
		assertEquals(reviewRecordTwo, reviewRecords.get(1));
		assertEquals(reviewRecordOne, reviewRecords.get(0));
	}
	
	@Override
	public void tearDown() throws Exception {
		dbCleanup("DELETE FROM REVIEWS;");
		
		try {
			//to compress the unused space and clean pages left after cleaning tables
			db.execSQL("VACUUM;");
		} catch(SQLException sqle) {
			fail();
			sqle.printStackTrace();
		} finally {
			testDBInstance.close();
		}
		
		getActivity().finish();
		super.tearDown();
	}
	
	//Since android doesn't provide db transaction methods, we'll clean up db ourselves
	//This is dependent on real code which it shouldn't be but for now I think it's ok.
	private void dbCleanup(final String sqlStatement) {
		db.beginTransaction();
		try {
			db.execSQL(sqlStatement);
			db.setTransactionSuccessful();
		} catch(SQLException sqle) {
			sqle.printStackTrace();
			fail();
		}finally {
			db.endTransaction();
		}
	}
}
