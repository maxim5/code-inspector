package com.courses;
/**
 * Load hardcoded course information when app installed.
 * later replaced by call to CAO site
 * @author Jennifer Flynn
 *
 */
public class InitCourseDb {
	final static Course[] COURSES = {
			new Course(
					"AL703",
					"Computer Network Management",
					"http://www.ait.ie/aboutaitandathlone/courses/bscincomputernetworkmanagement/",
					"Athlone Institute of Technology", "155", "7"),
			new Course(
					"AL704",
					"Computer Engineering",
					"http://www.ait.ie/aboutaitandathlone/courses/bengincomputerengineeringabinitio/",
					"Athlone Institute of Technology", "180", "7"),
			new Course(
					"AL750",
					"Business Computing",
					"http://www.ait.ie/aboutaitandathlone/courses/bachelorofscienceinbusinesscomputing/",
					"Athlone Institute of Technology", "170", "7"),
			new Course("BN012", "Engineering (Computer Engineering)",
					"http://www.itb.ie/site/courses/bn012.htm",
					"Institute of Technology, Blanchardstown", "180", "7"),
			new Course("BN013", "Computing (Information Technology)",
					"http://www.itb.ie/site/courses/bn013.htm",
					"Institute of Technology, Blanchardstown", "180", "7"),
			new Course("BN104", "Computing (Information Technology)",
					"http://www.itb.ie/site/courses/bn104.htm",
					"Institute of Technology, Blanchardstown", "220", "8"),
			new Course("BN117", "Engineering (Computer Engineering)",
					"http://www.itb.ie/StudyatITB/bn106.html",
					"Institute of Technology, Blanchardstown", "", "8"),
			new Course("CK401", "Computer Science",
					"http://www.ucc.ie/en/CK401",
					"University College Cork (NUI)", "330", "8"),
			new Course("CR016", "Computing", "http://www.cit.ie/course/CR016",
					"Cork Institute of Technology", "280", "7"),
			new Course("CR116", "Software Development & Computer Networking",
					"http://www.cit.ie/course/CR116",
					"Cork Institute of Technology", "290", "8"),
			new Course(
					"CW208",
					"Computer Games Development",
					"http://www.itcarlow.ie/study-at-itc/science/computing-networking/cw208.htm",
					"Institute of Technology, Carlow", "325", "8"),
			new Course(
					"CW217",
					"Computer Systems Management",
					"http://www.itcarlow.ie/study-at-itc/science/computing-networking/cw217.htm",
					"Institute of Technology, Carlow", "235", "7"),
			new Course(
					"CW248",
					"Computer Systems Management",
					"http://www.itcarlow.ie/study-at-itc/science/computing-networking/cw248.htm",
					"Institute of Technology, Carlow", "290", "8"),
			new Course(
					"DB525",
					"Marketing with Digital Media & Cloud Computing",
					"http://www.dbs.ie/Marketing/BA-Hons-Marketing-Digital-Media-Cloud-Computing.htm",
					"Dublin Business School & DBS School of Arts", "", "8"),
			new Course(
					"DB526",
					"Business Information Systems with Cloud Computing",
					"http://www.dbs.ie/Business-InformationTechnology/BA-Hons-Business-Information-Systems-Cloud.htm",
					"Dublin Business School & DBS School of Arts", "", "8"),
			new Course(
					"DB574",
					"Business in Information Technology",
					"http://www.dbs.ie/ba-business-information-technology/tabid/421/",
					"Dublin Business School & DBS School of Arts", "105", "7"),
			new Course(
					"DC120",
					"Enterprise Computing",
					"http://www.dcu.ie/prospective/deginfo.php?classname=EC&mode=full&originating_school=",
					"Dublin City University", "330", "8"),
			new Course(
					"DC121",
					"Computer Applications",
					"http://www.dcu.ie/prospective/deginfo.php?classname=CA&degree_description=B.Sc.+in+Computer+Applications+%28Bachelor+Honours+Degree%29",
					"Dublin City University", "340", "8"),
			new Course("DK721", "Computing (incorporating 3 award options)",
					"http://www.dkit.ie/courses/dk721",
					"Dundalk Institute of Technology", "165", "7"),
			new Course("DK820", "Computing in Games Development",
					"http://www.dkit.ie/courses/dk820",
					"Dundalk Institute of Technology", "330", "8"),
			new Course("DK821", "Computing",
					"http://www.dkit.ie/courses/dk821",
					"Dundalk Institute of Technology", "300", "8"),
			new Course(
					"DL131",
					"Computing in Multimedia Programming",
					"http://www.iadt.ie/en/ProspectiveStudents/SchoolsCourses/SchoolofCreativeTechnologies/BScinComputinginMultimediaProgramming/",
					"Dun Laoghaire Institute of Art, Design and Technology",
					"300", "7"),
			new Course("DN201", "Computer Science",
					"https://myucd.ucd.ie/program.do?programID=73",
					"University College Dublin (NUI)", "410", "8"),
			new Course(
					"DT007",
					"Automotive Management & Technology",
					"http://www.dit.ie/study/undergraduate/programmes/automotivemanagementtechnologydt007/",
					"Dublin Institute of Technology", "270", "7"),
			new Course(
					"DT028",
					"Transport Operations & Technology",
					"http://www.dit.ie/study/undergraduate/programmes/transportoperationstechnologydt028/",
					"Dublin Institute of Technology", "255", "8"),
			new Course(
					"DT081",
					"Computer & Communications Engineering",
					"http://www.dit.ie/study/undergraduate/programmes/computercommunicationsengineeringdt081/",
					"Dublin Institute of Technology", "320", "8"),
			new Course(
					"DT169",
					"Timber Product Technology",
					"http://www.dit.ie/study/undergraduate/programmes/timberproducttechnologydt169/",
					"Dublin Institute of Technology", "", "7"),
			new Course(
					"DT175",
					"Architectural Technology",
					"http://www.dit.ie/study/undergraduate/programmes/architecturaltechnologydt175/",
					"Dublin Institute of Technology", "320", "8"),
			new Course(
					"DT211",
					"Computing",
					"http://www.dit.ie/study/undergraduate/programmes/computingdt211/",
					"Dublin Institute of Technology", "320", "8"),
			new Course(
					"DT222",
					"Physics Technology",
					"http://www.dit.ie/study/undergraduate/programmes/physicstechnologydt222/",
					"Dublin Institute of Technology", "305", "8"),
			new Course(
					"DT228",
					"Computer Science",
					"http://www.dit.ie/study/undergraduate/programmes/computersciencedt228/",
					"Dublin Institute of Technology", "350", "8"),
			new Course(
					"DT354",
					"Business Computing",
					"http://www.dit.ie/study/undergraduate/programmes/businesscomputingdt354/",
					"Dublin Institute of Technology", "350", "8"),
			new Course(
					"DT596",
					"Print  Digital Media & Technology Management",
					"http://www.dit.ie/study/undergraduate/programmes/printanddigitalmediatechnologymanagementdt596/",
					"Dublin Institute of Technology", "260", "8"),
			new Course(
					"GA570",
					"Computer & Electronic Engineering",
					"http://www.gmit.ie/engineering/electronic/level-7-programmes/beng-cee.html",
					"Galway-Mayo Institute of Technology", "255", "7"),
			new Course(
					"GA572",
					"Computer & Energy Systems",
					"http://www.gmit.ie/engineering/electronic/level-7-programmes/bsc-ces.html",
					"Galway-Mayo Institute of Technology", "255", "7"),
			new Course(
					"GA775",
					"Computing in Software Development",
					"http://www.gmit.ie/science/level-7-programmes/bsc-software-development.html",
					"Galway-Mayo Institute of Technology", "250", "7"),
			new Course(
					"GA776",
					"Business Computing & Digital Media",
					"http://www.gmit.ie/science/level-7-programmes/bsc-digital-media.html",
					"Galway-Mayo Institute of Technology", "240", "7"),
			new Course("GC335", "Computing - Limerick",
					"http://www.gcl.ie/bachelor-of-science-in-computing/",
					"Griffith College", "0", "7"),
			new Course("GC430", "Computing Science (Dublin)",
					"http://www.gcd.ie/bsch", "Griffith College", "205", "8"),
			new Course("GC435", "Computing (Dublin)", "http://www.gcd.ie/bsco",
					"Griffith College", "0", "7"),
			new Course(
					"GY350",
					"Computer Science & Information Technology",
					"http://www.nuigalway.ie/courses/undergraduate-courses/computer-science-and-information-technology.html",
					"National University of Ireland, Galway", "390", "8"),
			new Course(
					"GY406",
					"Electronic & Computer Engineering",
					"http://www.nuigalway.ie/courses/undergraduate-courses/electronic-and-computers.html",
					"National University of Ireland, Galway", "405", "8"),
			new Course("LC239", "Computer Networks & Systems Management",
					"http://www.lit.ie/courses/LC239",
					"Limerick Institute of Technology", "245", "8"),
			new Course("LC403", "Computing - Thurles",
					"http://www.lit.ie/courses/LC403",
					"Limerick Institute of Technology", "155", "7"),
			new Course("LC404",
					"Computing - Smart Sustainable Energy - Thurles",
					"http://www.lit.ie/courses/LC404",
					"Limerick Institute of Technology", "140", "7"),
			new Course("LC415", "Computing - Software Development - Thurles",
					"http://www.lit.ie/courses/LC415",
					"Limerick Institute of Technology", "0", "8"),
			new Course("LC416", "Computing - IT Support - Thurles",
					"http://www.lit.ie/courses/LC416",
					"Limerick Institute of Technology", "165", "7"),
			new Course("LC418",
					"Computing - Games Design & Development - Thurles",
					"http://www.lit.ie/courses/LC418",
					"Limerick Institute of Technology", "240", "8"),
			new Course("LC420",
					"Computing - Smart Sustainable Energy - Thurles",
					"http://www.lit.ie/courses/LC420",
					"Limerick Institute of Technology", "265", "8"),
			new Course("LM051", "Computer Systems",
					"http://www.ul.ie/courses/LM051.shtml",
					"University of Limerick", "340", "8"),
			new Course("LM074", "Computer Aided Engineering & Design",
					"http://www.ul.ie/courses/LM074.shtml",
					"University of Limerick", "410", "8"),
			new Course("LM110", "Multimedia & Computer Games Development",
					"http://www.ul.ie/courses/LM110.shtml",
					"University of Limerick", "330", "8"),
			new Course("LM118", "Electronic & Computer Engineering",
					"http://www.ul.ie/courses/LM118.shtml",
					"University of Limerick", "405", "8"),
			new Course(
					"LY627",
					"Computer Engineering",
					"http://www.lyit.ie/courses/electronicsmechanicalengineering/ly627/",
					"Letterkenny Institute of Technology", "140", "7"),
			new Course("LY707", "Computer Games Development",
					"http://www.lyit.ie/courses/computing/ly707/",
					"Letterkenny Institute of Technology", "200", "7"),
			new Course("LY708", "Applied Computing",
					"http://www.lyit.ie/courses/computing/ly708/",
					"Letterkenny Institute of Technology", "250", "8"),
			new Course("LY717", "Computing with Business Applications",
					"http://www.lyit.ie/courses/computing/ly717/",
					"Letterkenny Institute of Technology", "150", "7"),
			new Course("LY737", "Computer Security & Digital Forensics",
					"http://www.lyit.ie/courses/computing/ly737/",
					"Letterkenny Institute of Technology", "145", "7"),
			new Course("LY747",
					"Multimedia & Digital Entertainment Technology",
					"http://www.lyit.ie/courses/computing/ly747/",
					"Letterkenny Institute of Technology", "140", "7"),
			new Course(
					"MH140",
					"Computer Science & Software Engineering Arts",
					"http://www.nuim.ie/courses/?TARGET=QUALIFICATION&MODE=VIEW&QUALIFICATION_CODE=CSEA&SUBJECT_CODE=&OFFERING_CODE=U_HONS_DEGREES",
					"National University of Ireland, Maynooth", "", "8"),
			new Course(
					"MH203",
					"Computer Science & Software Engineering",
					"http://www.nuim.ie/courses/?TARGET=QUALIFICATION&MODE=VIEW&QUALIFICATION_CODE=CSEN&SUBJECT_CODE=&OFFERING_CODE=U_HONS_DEGREES",
					"National University of Ireland, Maynooth", "355", "8"),
			new Course(
					"MH301",
					"Electronic Engineering with Computers",
					"http://www.nuim.ie/courses/?TARGET=QUALIFICATION&MODE=VIEW&QUALIFICATION_CODE=ENGCP&SUBJECT_CODE=&OFFERING_CODE=U_HONS_DEGREES",
					"National University of Ireland, Maynooth", "350", "8"),
			new Course(
					"MH304",
					"Common entry to Computer  Electronic & Communications Engineering",
					"http://www.nuim.ie/courses/?TARGET=QUALIFICATION&MODE=VIEW&QUALIFICATION_CODE=ENG&SUBJECT_CODE=&OFFERING_CODE=U_HONS_DEGREES",
					"National University of Ireland, Maynooth", "350", "8"),
			new Course(
					"NC003",
					"Computing",
					"http://www.ncirl.ie/Programmes_Courses/Full-time-Courses/BSc-Honours-in-Computing-BSHCIFSC1",
					"National College of Ireland (NCI)", "270", "8"),
			new Course(
					"SG131",
					"Computing - Games Development",
					"http://courses.itsligo.ie/business-humanities-courses/computing/bsc-computing-in-games-development-sg-131/",
					"Institute of Technology, Sligo", "270", "7"),
			new Course(
					"SG136",
					"Computing - Software Development",
					"http://courses.itsligo.ie/business-humanities-courses/computing/bsc-computing-in-software-development-sg-136/",
					"Institute of Technology, Sligo", "255", "7"),
			new Course(
					"SG137",
					"Computing - Systems & Networking",
					"http://courses.itsligo.ie/business-humanities-courses/computing/bsc-computing-in-systems-and-networking-sg-137/",
					"Institute of Technology, Sligo", "235", "7"),
			new Course(
					"TA312",
					"Computing",
					"http://www.it-tallaght.ie/index.cfm/page/course?id=62&modeofstudyEntryId=1",
					"Institute of Technology, Tallaght", "255", "7"),
			new Course(
					"TA322",
					"Computing",
					"http://www.it-tallaght.ie/index.cfm/page/course?id=61&modeofstudyEntryId=1",
					"Institute of Technology, Tallaght", "290", "8"),
			new Course("TL315", "Computing with Software Development",
					"http://www.ittralee.ie/cao/TL315",
					"Institute of Technology, Tralee", "240", "7"),
			new Course("TL325", "Computing with Games Development",
					"http://www.ittralee.ie/cao/TL325",
					"Institute of Technology, Tralee", "220", "7"),
			new Course("TL330", "Computing with Games Development",
					"http://www.ittralee.ie/cao/TL330",
					"Institute of Technology, Tralee", "300", "8"),
			new Course("TL355", "Computing with Multimedia",
					"http://www.ittralee.ie/cao/TL355",
					"Institute of Technology, Tralee", "230", "7"),
			new Course("TL360", "Computing with Multimedia",
					"http://www.ittralee.ie/cao/TL360",
					"Institute of Technology, Tralee", "295", "8"),
			new Course(
					"TR033",
					"Computer Science",
					"http://www.tcd.ie/courses/undergraduate/az/course.php?id=64",
					"Trinity College", "385", "8"),
			new Course(
					"TR082",
					"Business & Computing",
					"http://www.tcd.ie/courses/undergraduate/az/course.php?id=273",
					"Trinity College", "420", "8"),
			new Course("WD028", "Applied Computing", "http://www.wit.ie/WD028",
					"Waterford Institute of Technology", "300", "8"),
			new Course("WD161", "Computer Forensics & Security",
					"http://www.wit.ie/WD161",
					"Waterford Institute of Technology", "300", "8") };

}