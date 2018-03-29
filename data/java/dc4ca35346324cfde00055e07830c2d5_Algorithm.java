package fr.univtln.ganne882.project2007.algos;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;

import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;

/**
 * An instance of Creator describes an algorithm within its all usefull attributes
 * @author  Gregory ANNE
 */
public class Algorithm {

	//this boolean indicates if the algorithm has recently
	//been modificated by a student
	boolean isModificatedByAStudent;
	int indAlgo;
	int indLevel_;
	String name;
	String domain_;
	String type_;
	String description;
	String codeC;
	String functionHeader;
	String algoProposition;
	static Logger logs = Logger.getRootLogger();
	
	/**
	 * This constructor aim is to build a new algorithm,
	 * new attributes will be filled through the interface
	 */
	public Algorithm (){
	    	PropertyConfigurator.configure("log4j.prop");
			indAlgo = 0;
			indLevel_ = 4;
			name = "";
			domain_ = "";
			type_ = "";
			description = "";
			codeC = "";
			functionHeader = "";
			algoProposition = "";
			isModificatedByAStudent = false;
	}//constructor()
	
	/**
	 * This constructor creates an instance of algorithm with
	 * attributes already present in the database
	 * @param theIDAlgo
	 * @param theName
	 * @param theDomain_
	 * @param theType_
	 * @param theLevel_
	 * @param theDescription_
	 * @param theCodeC
	 * @param theFunctionHeader
	 * @param theAlgoProposition
	 */
	public Algorithm (boolean isModificated, int theIDAlgo, String theName,
			String theDomain_, String theType_, int theLevel_, String theDescription_,
			String theCodeC, String theFunctionHeader, String theAlgoProposition){
	    	PropertyConfigurator.configure("log4j.prop");
			isModificatedByAStudent = isModificated;
			indAlgo = theIDAlgo;
			indLevel_ = theLevel_;
			name = theName;
			domain_ = theDomain_;
			type_ = theType_;
			description = theDescription_;
			codeC = theCodeC;
			functionHeader = theFunctionHeader;
			algoProposition = theAlgoProposition;
	}//constructor
	
	/**
	 * This function adds a new Algorithm to the db
	 */
	public void addToDB (){
		//loading Driver
	    try {
	        Class.forName("org.postgresql.Driver");  //$NON-NLS-1$
	    } catch (Exception e) {
	    	logs.error("no driver");
	    }//catch
	    try {
		    String url = "jdbc:postgresql://localhost/portable"; //$NON-NLS-1$
		    Connection conn = DriverManager.getConnection(url, "portable", "gregory"); //$NON-NLS-1$ //$NON-NLS-2$
			Statement st = conn.createStatement();
			String queryBeginLevel = (indLevel_ < 4)? ", Level_) " : ") ";
			String queryEndLevel = (indLevel_ < 4)? ("' , "+Integer.toString(indLevel_)+")") : "')";
			logs.debug("type_  : |"+type_+"|");
			String queryBeginType = ((type_.equals("ITR")) || (type_.equals("REC")))? ", Type_, " : ", ";
			String queryEndType = ((type_.equals("ITR")) || (type_.equals("REC")))? ("', '" + type_ +  "', '"):"', '";
			String queryBeginDomain, queryEndDomain;
			if (domain_.equals("NUM") || domain_.equals("GRA") || domain_.equals("DON") || domain_.equals("COD") || domain_.equals("GEO")){
				queryEndDomain = ("', '" + domain_); queryBeginDomain = ", Domain_";				
			} else {
				queryEndDomain = ""; queryBeginDomain = "";
			}//else
			String queryBeginDescription = (description.equals(""))? "":" Description, " ;
			String queryBeginCodeC = (codeC.equals(""))? "": "CodeC, ";
			String queryEndDescription = (description.equals(""))? "":(description + "', '") ;
			String queryEndCodeC = (codeC.equals(""))? "": (codeC + "', '");
				// functionHeader and algoPorposition can be filled by a void value
			String query = "insert into Algorithm (News, Name" + queryBeginDomain + queryBeginType + queryBeginDescription +  //$NON-NLS-1$
			queryBeginCodeC +" functionheader, algoproposition" + queryBeginLevel +  //$NON-NLS-1$
			"values ("+ isModificatedByAStudent + ", '" + name + queryEndDomain + queryEndType +  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
			queryEndDescription + queryEndCodeC + functionHeader + "', '" + algoProposition + queryEndLevel; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			logs.debug("query executed : "+query);
			st.executeUpdate(query);
			logs.info("new algo added to db : "+name);
			st.close();
			conn.close();
		} catch (SQLException e) {
			logs.error(e.getMessage());
	    }//catch
	}//addToDB
	
	/**
	 * This getter function is necessary because Swing components constructors have to be defined with a String and overriding  toString method doesn't allow  to cast a Creator in String
	 * @return  String
	 * @uml.property  name="name"
	 */
	public String getName() {
		return name;
	}//getName

	/**
	 * gets the algorithm identification serial number
	 * @return  int
	 * @uml.property  name="indAlgo"
	 */
	public int getIndAlgo() {
		return indAlgo;
	}//getIDAlgo

	/**
	 * This overriding method allows to fill lists with
	 * Algorithms instead of only their name
	 * @return String
	 * @Override
	 */
	public String toString() {
		return name;
	}//toString

	/**
	 * When an algo is proposed by a student, this method will allow the user to store it in the db
	 * @return  String
	 * @uml.property  name="algoProposition"
	 */
	public String getAlgoProposition() {
		return algoProposition;
	}

	/**
	 * gets the URL where the description of the algo is given
	 * @return  String
	 * @uml.property  name="codeC"
	 */
	public String getCodeC() {
		return codeC;
	}//getcodec

	/**
	 * gets the URL where the HTML description of the algo is available
	 * @return  String
	 * @uml.property  name="description"
	 */
	public String getDescription() {
		return description;
	}//getdescription

	/**
	 * gets the 3  symbols domain descriptor
	 * @return  String
	 * @uml.property  name="domain_"
	 */
	public String getDomain_() {
		return domain_;
	}//getdomain

	/**
	 * gets the function header of an algo, this will permit students to propose algos
	 * @return  String
	 * @uml.property  name="functionHeader"
	 */
	public String getFunctionHeader() {
		return functionHeader;
	}//getfunctionheader

	/**
	 * gets the level of difficulty (between 0 and 3) if level = 4, this is a new algo, and level has not been evaluated yet
	 * @return  int
	 * @uml.property  name="indLevel_"
	 */
	public int getIndLevel_() {
		return indLevel_;
	}//getlevel

	/**
	 * gets the 3 symbols type of the algo
	 * @return  String
	 * @uml.property  name="type_"
	 */
	public String getType_() {
		return type_;
	}//gettype

	/**
	 * To indicate to the teacher if a student
	 * has added his idea of the algorithm
	 * @return boolean
	 */
	public boolean hasBeenRecentlyModificated() {
		return isModificatedByAStudent;
	}//hasbeenrecentlymodificated
	
	/**
	 * method that allows teachers and for some attributes students, 
	 * to modificate the attributes directly from the program.
	 * @param IDAlgo
	 * @param isModificatedString
	 * @param theName
	 * @param theDomain_
	 * @param theType_
	 * @param theLevel_
	 * @param theDescription_
	 * @param theCodeC
	 * @param theFunctionHeader
	 * @param theAlgoProposition
	 */
	public void updateAttributes(){
		//loading Driver
	    try {
	        Class.forName("org.postgresql.Driver");  //$NON-NLS-1$
	    } catch (Exception e) {
	    	logs.error("no driver");
	    }//catch
	    try {
		    String url = "jdbc:postgresql://localhost/portable"; //$NON-NLS-1$
		    Connection conn = DriverManager.getConnection(url, "portable", "gregory"); //$NON-NLS-1$ //$NON-NLS-2$
			Statement st = conn.createStatement();
			//some attributes must not be erased.
			String queryLevel = (indLevel_ < 4)? ("', level_ = '" + indLevel_) : "";
			String queryType = ((type_.equals("ITR")) || (type_.equals("REC")))? ("', type_ = '" + type_) : "";
			String queryDomain = (domain_.equals("NUM") || domain_.equals("GRA") || domain_.equals("DON") || domain_.equals("COD") || domain_.equals("GEO"))?
					("', domain_ = '" + domain_) : "";
		    String queryProposition = (algoProposition.equals(""))? "" : ("' , algoproposition = '"+ algoProposition);
		    String querycodec = (codeC.equals(""))? "" : ("' , codec = '" + codeC);
		    String querydescription = (description.equals(""))? "" : ("' ,  description = '" + description);
		    String queryfunctionheader = (functionHeader.equals(""))? "": ("', functionheader = '" + functionHeader);
			String query = "update Algorithm set news = " +isModificatedByAStudent+ 
			"  , name = '" + name + querydescription + queryDomain + queryType + queryLevel
			+ querycodec + queryfunctionheader + queryProposition +
			"' where IDAlgo = "+ Integer.toString(indAlgo);
			logs.info("query : "+query);
			int indHasHappenedCorrectly = st.executeUpdate(query);
			if (indHasHappenedCorrectly < 1) 
				addToDB();
			else {
				logs.info("query : "+query);
				logs.info("the algo \""+name+"\" has been modificated in the db");
			}
			st.close();
			conn.close();
		} catch (SQLException e) {
			logs.error(e.getMessage());
	    }//catch
	}//update algo
	
}//class creator
