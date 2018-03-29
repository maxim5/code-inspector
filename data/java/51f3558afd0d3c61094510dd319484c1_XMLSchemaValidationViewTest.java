package com.sri.learningregistry.couchdb.views;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import com.sri.learningregistry.couchdb.viewserver.interfaces.MapDoc;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Unit test for simple App.
 */
public class XMLSchemaValidationViewTest 
    extends TestCase
{
    /**
     * Create the test case
     *
     * @param testName name of the test case
     */
    public XMLSchemaValidationViewTest( String testName )
    {
        super( testName );
    }

    /**
     * @return the suite of tests being tested
     */
    public static Test suite()
    {
        return new TestSuite( XMLSchemaValidationViewTest.class );
    }

    /**
     * Rigourous Test :-)
     */
    public void testValidateSchema() throws JSONException
    {
    	String doc_string = "{\r\n   \"_id\": \"000fb240d1cf4876ba691dbf42d8f052\",\r\n   \"_rev\": \"1-e4db0406f98c1f491800ff66108a3b0f\",\r\n   \"doc_type\": \"resource_data\",\r\n   \"resource_locator\": \"http://www.nationalgeographic.com/xpeditions/lessons/15/gk2/tornadowhat.html\",\r\n   \"update_timestamp\": \"2011-10-27T00:41:34.022420Z\",\r\n   \"resource_data\": \"<nsdl_dc:nsdl_dc xmlns:nsdl_dc=\\\"http://ns.nsdl.org/nsdl_dc_v1.02/\\\" xmlns:dc=\\\"http://purl.org/dc/elements/1.1/\\\" xmlns:dct=\\\"http://purl.org/dc/terms/\\\" xmlns:xsi=\\\"http://www.w3.org/2001/XMLSchema-instance\\\" xmlns=\\\"http://www.openarchives.org/OAI/2.0/\\\" schemaVersion=\\\"1.02.000\\\" xsi:schemaLocation=\\\"http://ns.nsdl.org/nsdl_dc_v1.02/ http://ns.nsdl.org/schemas/nsdl_dc/nsdl_dc_v1.02.xsd\\\">\\n  <dc:identifier xsi:type=\\\"dct:URI\\\">http://www.nationalgeographic.com/xpeditions/lessons/15/gk2/tornadowhat.html</dc:identifier>\\n  <dct:isPartOf xsi:type=\\\"dct:URI\\\">http://www.nationalgeographic.com/xpeditions/index.html</dct:isPartOf>\\n  <dct:conformsTo xsi:type=\\\"dct:URI\\\">http://purl.org/ASN/resources/S103E245</dct:conformsTo>\\n  <dct:conformsTo xsi:type=\\\"dct:URI\\\">http://purl.org/ASN/resources/S103E24D</dct:conformsTo>\\n  <dc:type xsi:type=\\\"dct:DCMIType\\\">InteractiveResource</dc:type>\\n  <dc:type xsi:type=\\\"nsdl_dc:NSDLType\\\">Instructional Material</dc:type>\\n  <dc:type xsi:type=\\\"nsdl_dc:NSDLType\\\">Lesson/Lesson Plan</dc:type>\\n  <dc:format xsi:type=\\\"dct:IMT\\\">text/html</dc:format>\\n  <dc:language xsi:type=\\\"dct:RFC3066\\\">en</dc:language>\\n  <dc:title>What is a Tornado?</dc:title>\\n  <dc:subject>Atmospheric science</dc:subject>\\n  <dc:subject>Natural hazards</dc:subject>\\n  <dc:subject xsi:type=\\\"nsdl_dc:GEM\\\">Science</dc:subject>\\n  <dc:subject xsi:type=\\\"nsdl_dc:GEM\\\">Earth science</dc:subject>\\n  <dc:subject xsi:type=\\\"nsdl_dc:GEM\\\">Physical sciences</dc:subject>\\n  <dc:subject xsi:type=\\\"nsdl_dc:GEM\\\">Meteorology</dc:subject>\\n  <dc:subject xsi:type=\\\"nsdl_dc:GEM\\\">Astronomy</dc:subject>\\n  <dc:subject xsi:type=\\\"nsdl_dc:GEM\\\">Space sciences</dc:subject>\\n  <dc:description>In this lesson plan students will learn the basics about how tornadoes are formed and when and where they are most likely to occur. They will learn that the United States is the country most vulnerable to tornadoes and why most tornadoes occur in Tornado Alley. Students will create a tornado in a bottle. Finally, they will draw pictures of tornadoes of different shapes and sizes and review basic safety information about what to do in case of a tornado.</dc:description>\\n  <dc:publisher>National Geographic Society</dc:publisher>\\n  <dc:rights>Copyright 2003 National Geographic Society. All rights reserved.</dc:rights>\\n  <dct:educationLevel xsi:type=\\\"nsdl_dc:NSDLEdLevel\\\">Elementary School</dct:educationLevel>\\n</nsdl_dc:nsdl_dc>\",\r\n   \"keys\": [\r\n       \"dlese.org\",\r\n       \"Astronomy\",\r\n       \"lr-test-data\",\r\n       \"Science\",\r\n       \"Meteorology\",\r\n       \"Atmospheric science\",\r\n       \"en\",\r\n       \"Space sciences\",\r\n       \"Elementary School\",\r\n       \"DLESE Community Collection\",\r\n       \"Physical sciences\",\r\n       \"Natural hazards\",\r\n       \"Earth science\"\r\n   ],\r\n   \"TOS\": {\r\n       \"submission_attribution\": \"The National Science Digital Library\",\r\n       \"submission_TOS\": \"http://nsdl.org/help/?pager=termsofuse\"\r\n   },\r\n   \"resource_data_type\": \"metadata\",\r\n   \"payload_schema_locator\": \"http://ns.nsdl.org/nsdl_dc_v1.02/ http://ns.nsdl.org/schemas/nsdl_dc/nsdl_dc_v1.02.xsd\",\r\n   \"payload_placement\": \"inline\",\r\n   \"payload_schema\": [\r\n       \"nsdl_dc\"\r\n   ],\r\n   \"node_timestamp\": \"2011-10-27T00:41:34.022420Z\",\r\n   \"digital_signature\": {\r\n       \"key_location\": [\r\n           \"http://pool.sks-keyservers.net:11371/pks/lookup?op=get&search=0xBFF13965146B1740\",\r\n           \"https://keyserver2.pgp.com/vkd/DownloadKey.event?keyid=0xBFF13965146B1740\"\r\n       ],\r\n       \"key_owner\": \"The National Science Digital Library <nsdlsupport@nsdl.ucar.edu>\",\r\n       \"signing_method\": \"LR-PGP.1.0\",\r\n       \"signature\": \"-----BEGIN PGP SIGNED MESSAGE-----\\nHash: SHA1\\n\\ncbdf9ddba0e4912eca9422d4c8734496ff7446f65da825bf31461ae861cab554\\n-----BEGIN PGP SIGNATURE-----\\nVersion: GnuPG v1.4.10 (GNU/Linux)\\n\\niQIcBAEBAgAGBQJOqKiqAAoJEN3QUpzaJ39H194QALwLZqcWy2M99pqEo6qNqZGy\\n+Hs1JREHDR9X2wZ8/ygmx2Yy4WoLXz8rH1mn6TDTbgHWa1DYuB6qUauBlHVuArap\\nGqZmmUeDiEvA23X4rtUW73epCu7Mid/dWE7peqni1firYcHCfRba1flr0T/R6VcV\\nS7EW0Svok5uLN6GcgG1CQLaZz0BLEYoSdmgOKwWKr+96O9UPUH4CM004GKCBujne\\n3aB/LJ9XQwli/h79sdKusk/VUhJa+6aBKtUo1XkwK8W7E8z+sSP2H4YCNy8YRHtZ\\ndifAc98GYiwbQAwsznDcuDc1C4El/s25c6wddn3vEcnny3MtCbWbdiunRuDqnRz2\\nR+zurZEQXSpMBPpcXQDs3Y4UWUUYOIysskYm5XKBYiuZ5aU+PGYE8jtGwkZLHkLB\\nPfAB1+cqlgnCDexlAnpCcmOgraTkU4Q0AnhlV8ot6goP9oAVlyO/zEY0UbB9SVax\\n/rqzDScj/KCDa1AbDh857BxoYPKEYUu+Y0vg40ytf1v2oLE+j4PKBM2C5reiVeOY\\nwz23x/eBS0gnGSPDYt6y4vx5Jwuceo2jjV00XRXPE+bsF/qUxN7K9IfdbGl/JWTz\\nNr82DuqfG9WALe2j6mgv2YBQAGN3glmIybr9Osm3nGfGe1o6wOGViEo2iW6v56oS\\niIjqyzKGlXe6YKBrevTD\\n=2jmp\\n-----END PGP SIGNATURE-----\\n\"\r\n   },\r\n   \"create_timestamp\": \"2011-10-27T00:41:34.022420Z\",\r\n   \"doc_version\": \"0.23.0\",\r\n   \"active\": true,\r\n   \"publishing_node\": \"d94cc48d93744cf88ba833c7f7a2c2b7\",\r\n   \"doc_ID\": \"000fb240d1cf4876ba691dbf42d8f052\",\r\n   \"identity\": {\r\n       \"signer\": \"The National Science Digital Library <nsdlsupport@nsdl.ucar.edu>\",\r\n       \"submitter\": \"SRI International on behalf of The National Science Digital Library\",\r\n       \"submitter_type\": \"agent\",\r\n       \"curator\": \"The National Science Digital Library\"\r\n   }\r\n}";
    	JSONObject doc = new JSONObject(doc_string);
    	
    	MapDoc v = new XMLSchemaValidationView();
    	JSONArray result = v.map_doc(doc);
    	
        assertEquals("[[\""+doc.getString("_id")+"\",null]]", result.toString());
    }
    
    
    public void testMultipleSchema() throws JSONException
    {
    	for(int i=0; i<10; i++) {
    		testValidateSchema();
    	}
    }
    
    
    public void testValidateBadSchema() throws JSONException
    {
    	String doc_string = "{\r\n   \"_id\": \"000fb240d1cf4876ba691dbf42d8f052\",\r\n   \"_rev\": \"1-e4db0406f98c1f491800ff66108a3b0f\",\r\n   \"doc_type\": \"resource_data\",\r\n   \"resource_locator\": \"http://www.nationalgeographic.com/xpeditions/lessons/15/gk2/tornadowhat.html\",\r\n   \"update_timestamp\": \"2011-10-27T00:41:34.022420Z\",\r\n   \"resource_data\": \"<nsdl_dc:nsdl_dc xmlns:dc=\\\"http://purl.org/dc/elements/1.1/\\\" xmlns:dct=\\\"http://purl.org/dc/terms/\\\" xmlns:xsi=\\\"http://www.w3.org/2001/XMLSchema-instance\\\" xmlns=\\\"http://www.openarchives.org/OAI/2.0/\\\" schemaVersion=\\\"1.02.000\\\" xsi:schemaLocation=\\\"http://ns.nsdl.org/nsdl_dc_v1.02/ http://ns.nsdl.org/schemas/nsdl_dc/nsdl_dc_v1.02.xsd\\\">\\n  <dc:identifier xsi:type=\\\"dct:URI\\\">http://www.nationalgeographic.com/xpeditions/lessons/15/gk2/tornadowhat.html</dc:identifier>\\n  <dct:isPartOf xsi:type=\\\"dct:URI\\\">http://www.nationalgeographic.com/xpeditions/index.html</dct:isPartOf>\\n  <dct:conformsTo xsi:type=\\\"dct:URI\\\">http://purl.org/ASN/resources/S103E245</dct:conformsTo>\\n  <dct:conformsTo xsi:type=\\\"dct:URI\\\">http://purl.org/ASN/resources/S103E24D</dct:conformsTo>\\n  <dc:type xsi:type=\\\"dct:DCMIType\\\">InteractiveResource</dc:type>\\n  <dc:type xsi:type=\\\"nsdl_dc:NSDLType\\\">Instructional Material</dc:type>\\n  <dc:type xsi:type=\\\"nsdl_dc:NSDLType\\\">Lesson/Lesson Plan</dc:type>\\n  <dc:format xsi:type=\\\"dct:IMT\\\">text/html</dc:format>\\n  <dc:language xsi:type=\\\"dct:RFC3066\\\">en</dc:language>\\n  <dc:title>What is a Tornado?</dc:title>\\n  <dc:subject>Atmospheric science</dc:subject>\\n  <dc:subject>Natural hazards</dc:subject>\\n  <dc:subject xsi:type=\\\"nsdl_dc:GEM\\\">Science</dc:subject>\\n  <dc:subject xsi:type=\\\"nsdl_dc:GEM\\\">Earth science</dc:subject>\\n  <dc:subject xsi:type=\\\"nsdl_dc:GEM\\\">Physical sciences</dc:subject>\\n  <dc:subject xsi:type=\\\"nsdl_dc:GEM\\\">Meteorology</dc:subject>\\n  <dc:subject xsi:type=\\\"nsdl_dc:GEM\\\">Astronomy</dc:subject>\\n  <dc:subject xsi:type=\\\"nsdl_dc:GEM\\\">Space sciences</dc:subject>\\n  <dc:description>In this lesson plan students will learn the basics about how tornadoes are formed and when and where they are most likely to occur. They will learn that the United States is the country most vulnerable to tornadoes and why most tornadoes occur in Tornado Alley. Students will create a tornado in a bottle. Finally, they will draw pictures of tornadoes of different shapes and sizes and review basic safety information about what to do in case of a tornado.</dc:description>\\n  <dc:publisher>National Geographic Society</dc:publisher>\\n  <dc:rights>Copyright 2003 National Geographic Society. All rights reserved.</dc:rights>\\n  <dct:educationLevel xsi:type=\\\"nsdl_dc:NSDLEdLevel\\\">Elementary School</dct:educationLevel>\\n</nsdl_dc:nsdl_dc>\",\r\n   \"keys\": [\r\n       \"dlese.org\",\r\n       \"Astronomy\",\r\n       \"lr-test-data\",\r\n       \"Science\",\r\n       \"Meteorology\",\r\n       \"Atmospheric science\",\r\n       \"en\",\r\n       \"Space sciences\",\r\n       \"Elementary School\",\r\n       \"DLESE Community Collection\",\r\n       \"Physical sciences\",\r\n       \"Natural hazards\",\r\n       \"Earth science\"\r\n   ],\r\n   \"TOS\": {\r\n       \"submission_attribution\": \"The National Science Digital Library\",\r\n       \"submission_TOS\": \"http://nsdl.org/help/?pager=termsofuse\"\r\n   },\r\n   \"resource_data_type\": \"metadata\",\r\n   \"payload_schema_locator\": \"http://ns.nsdl.org/nsdl_dc_v1.02/ http://ns.nsdl.org/schemas/nsdl_dc/nsdl_dc_v1.02.xsd\",\r\n   \"payload_placement\": \"inline\",\r\n   \"payload_schema\": [\r\n       \"nsdl_dc\"\r\n   ],\r\n   \"node_timestamp\": \"2011-10-27T00:41:34.022420Z\",\r\n   \"digital_signature\": {\r\n       \"key_location\": [\r\n           \"http://pool.sks-keyservers.net:11371/pks/lookup?op=get&search=0xBFF13965146B1740\",\r\n           \"https://keyserver2.pgp.com/vkd/DownloadKey.event?keyid=0xBFF13965146B1740\"\r\n       ],\r\n       \"key_owner\": \"The National Science Digital Library <nsdlsupport@nsdl.ucar.edu>\",\r\n       \"signing_method\": \"LR-PGP.1.0\",\r\n       \"signature\": \"-----BEGIN PGP SIGNED MESSAGE-----\\nHash: SHA1\\n\\ncbdf9ddba0e4912eca9422d4c8734496ff7446f65da825bf31461ae861cab554\\n-----BEGIN PGP SIGNATURE-----\\nVersion: GnuPG v1.4.10 (GNU/Linux)\\n\\niQIcBAEBAgAGBQJOqKiqAAoJEN3QUpzaJ39H194QALwLZqcWy2M99pqEo6qNqZGy\\n+Hs1JREHDR9X2wZ8/ygmx2Yy4WoLXz8rH1mn6TDTbgHWa1DYuB6qUauBlHVuArap\\nGqZmmUeDiEvA23X4rtUW73epCu7Mid/dWE7peqni1firYcHCfRba1flr0T/R6VcV\\nS7EW0Svok5uLN6GcgG1CQLaZz0BLEYoSdmgOKwWKr+96O9UPUH4CM004GKCBujne\\n3aB/LJ9XQwli/h79sdKusk/VUhJa+6aBKtUo1XkwK8W7E8z+sSP2H4YCNy8YRHtZ\\ndifAc98GYiwbQAwsznDcuDc1C4El/s25c6wddn3vEcnny3MtCbWbdiunRuDqnRz2\\nR+zurZEQXSpMBPpcXQDs3Y4UWUUYOIysskYm5XKBYiuZ5aU+PGYE8jtGwkZLHkLB\\nPfAB1+cqlgnCDexlAnpCcmOgraTkU4Q0AnhlV8ot6goP9oAVlyO/zEY0UbB9SVax\\n/rqzDScj/KCDa1AbDh857BxoYPKEYUu+Y0vg40ytf1v2oLE+j4PKBM2C5reiVeOY\\nwz23x/eBS0gnGSPDYt6y4vx5Jwuceo2jjV00XRXPE+bsF/qUxN7K9IfdbGl/JWTz\\nNr82DuqfG9WALe2j6mgv2YBQAGN3glmIybr9Osm3nGfGe1o6wOGViEo2iW6v56oS\\niIjqyzKGlXe6YKBrevTD\\n=2jmp\\n-----END PGP SIGNATURE-----\\n\"\r\n   },\r\n   \"create_timestamp\": \"2011-10-27T00:41:34.022420Z\",\r\n   \"doc_version\": \"0.23.0\",\r\n   \"active\": true,\r\n   \"publishing_node\": \"d94cc48d93744cf88ba833c7f7a2c2b7\",\r\n   \"doc_ID\": \"000fb240d1cf4876ba691dbf42d8f052\",\r\n   \"identity\": {\r\n       \"signer\": \"The National Science Digital Library <nsdlsupport@nsdl.ucar.edu>\",\r\n       \"submitter\": \"SRI International on behalf of The National Science Digital Library\",\r\n       \"submitter_type\": \"agent\",\r\n       \"curator\": \"The National Science Digital Library\"\r\n   }\r\n}";
    	JSONObject doc = new JSONObject(doc_string);
    	
    	MapDoc v = new XMLSchemaValidationView();
    	JSONArray result = v.map_doc(doc);
    	
        assertEquals("[]", result.toString());
    }
}
