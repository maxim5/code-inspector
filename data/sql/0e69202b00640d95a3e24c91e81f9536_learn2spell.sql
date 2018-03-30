-- MySQL dump 10.13  Distrib 5.1.49, for Win64 (unknown)
--
-- Host: localhost    Database: learn2spell
-- ------------------------------------------------------
-- Server version	5.1.49-community

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `lang_pair`
--

DROP TABLE IF EXISTS `lang_pair`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `lang_pair` (
  `from_lang` varchar(100) NOT NULL,
  `to_lang` varchar(100) NOT NULL,
  `from_image` varchar(1000) NOT NULL,
  `to_image` varchar(1000) NOT NULL,
  PRIMARY KEY (`from_lang`,`to_lang`),
  KEY `ix_lesson_from_lang` (`from_lang`),
  KEY `ix_lesson_to_lang` (`to_lang`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `lang_pair`
--

LOCK TABLES `lang_pair` WRITE;
/*!40000 ALTER TABLE `lang_pair` DISABLE KEYS */;
INSERT INTO `lang_pair` VALUES ('English','Chinese','img/lang_headers/English.png','img/lang_headers/Chinese.png');
/*!40000 ALTER TABLE `lang_pair` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `lang_pairs`
--

DROP TABLE IF EXISTS `lang_pairs`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `lang_pairs` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `from_lang` varchar(100) NOT NULL,
  `to_lang` varchar(100) NOT NULL,
  `from_image` varchar(1000) NOT NULL,
  `to_image` varchar(1000) NOT NULL,
  `updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  UNIQUE KEY `from_lang` (`from_lang`,`to_lang`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `lang_pairs`
--

LOCK TABLES `lang_pairs` WRITE;
/*!40000 ALTER TABLE `lang_pairs` DISABLE KEYS */;
INSERT INTO `lang_pairs` VALUES (1,'English','Chinese','img/lang_headers/English.png','img/lang_headers/Chinese.png','2012-03-05 04:58:35');
/*!40000 ALTER TABLE `lang_pairs` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `lesson_votes`
--

DROP TABLE IF EXISTS `lesson_votes`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `lesson_votes` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `lesson` int(11) NOT NULL,
  `user` int(11) NOT NULL,
  `up_down` int(11) NOT NULL DEFAULT '1',
  `updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  KEY `lesson` (`lesson`),
  KEY `user` (`user`),
  CONSTRAINT `lesson_votes_ibfk_1` FOREIGN KEY (`lesson`) REFERENCES `lessons` (`id`),
  CONSTRAINT `lesson_votes_ibfk_2` FOREIGN KEY (`user`) REFERENCES `users` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `lesson_votes`
--

LOCK TABLES `lesson_votes` WRITE;
/*!40000 ALTER TABLE `lesson_votes` DISABLE KEYS */;
INSERT INTO `lesson_votes` VALUES (1,2,2,-1,'2012-03-05 05:00:05'),(2,2,1,1,'2012-03-05 05:00:05');
/*!40000 ALTER TABLE `lesson_votes` ENABLE KEYS */;
UNLOCK TABLES;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'STRICT_TRANS_TABLES,NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
/*!50003 CREATE*/ /*!50017 DEFINER=`learn`@`localhost`*/ /*!50003 TRIGGER check_lesson_votes BEFORE INSERT ON lesson_votes
FOR EACH ROW 
BEGIN
	IF NEW.up_down < -1 THEN
		SET NEW.up_down = -1;
	END IF;
	IF NEW.up_down > -1 THEN
		SET NEW.up_down = 1;
	END IF;
END */;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'STRICT_TRANS_TABLES,NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
/*!50003 CREATE*/ /*!50017 DEFINER=`learn`@`localhost`*/ /*!50003 TRIGGER checkup_lesson_votes BEFORE UPDATE ON lesson_votes
FOR EACH ROW 
BEGIN
	IF NEW.up_down < -1 THEN
		SET NEW.up_down = -1;
	END IF;
	IF NEW.up_down > -1 THEN
		SET NEW.up_down = 1;
	END IF;
END */;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;

--
-- Table structure for table `lessons`
--

DROP TABLE IF EXISTS `lessons`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `lessons` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `user` int(11) NOT NULL DEFAULT '2',
  `lang_pair` int(11) NOT NULL,
  `title` varchar(100) NOT NULL,
  `public` tinyint(1) NOT NULL DEFAULT '0',
  `description` varchar(5000) DEFAULT NULL,
  `updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  UNIQUE KEY `lang_pair` (`lang_pair`,`title`,`user`),
  KEY `user` (`user`),
  CONSTRAINT `lessons_ibfk_1` FOREIGN KEY (`lang_pair`) REFERENCES `lang_pairs` (`id`),
  CONSTRAINT `lessons_ibfk_2` FOREIGN KEY (`user`) REFERENCES `users` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=4 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `lessons`
--

LOCK TABLES `lessons` WRITE;
/*!40000 ALTER TABLE `lessons` DISABLE KEYS */;
INSERT INTO `lessons` VALUES (1,2,1,'Lesson 404',0,'Missing Lesson','2012-03-05 04:59:40'),(2,2,1,'Lesson 1',0,'Introduce Yourself','2012-03-05 04:59:40'),(3,2,1,'Lesson 2',0,'Say Goodbye','2012-03-05 04:59:41');
/*!40000 ALTER TABLE `lessons` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `page_sections`
--

DROP TABLE IF EXISTS `page_sections`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `page_sections` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `lang_pair` int(11) NOT NULL,
  `user` int(11) NOT NULL DEFAULT '1',
  `lesson` int(11) DEFAULT NULL,
  `page` varchar(100) NOT NULL,
  `name` varchar(100) NOT NULL,
  `text` varchar(5000) DEFAULT NULL,
  `updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  UNIQUE KEY `lang_pair` (`lang_pair`,`user`,`lesson`,`page`,`name`),
  KEY `lesson` (`lesson`),
  KEY `user` (`user`),
  CONSTRAINT `page_sections_ibfk_1` FOREIGN KEY (`lang_pair`) REFERENCES `lang_pairs` (`id`),
  CONSTRAINT `page_sections_ibfk_2` FOREIGN KEY (`lesson`) REFERENCES `lessons` (`id`),
  CONSTRAINT `page_sections_ibfk_3` FOREIGN KEY (`user`) REFERENCES `users` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=6 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `page_sections`
--

LOCK TABLES `page_sections` WRITE;
/*!40000 ALTER TABLE `page_sections` DISABLE KEYS */;
INSERT INTO `page_sections` VALUES (1,1,2,NULL,'home.php','title','English->Chinese','2012-03-05 05:00:19'),(2,1,1,NULL,'home.php','title','English<-?!->Chiner','2012-03-05 05:00:19'),(3,1,3,NULL,'home.php','new_lesson_button','Add new Lesson','2012-03-05 07:14:38'),(4,1,3,NULL,'editsection.php','new_section_button','Add new Section','2012-03-05 07:15:09'),(5,1,3,NULL,'editsection.php','title','Edit Sections','2012-03-05 07:19:36');
/*!40000 ALTER TABLE `page_sections` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `section_votes`
--

DROP TABLE IF EXISTS `section_votes`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `section_votes` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `section` int(11) NOT NULL,
  `user` int(11) NOT NULL,
  `up_down` int(11) NOT NULL DEFAULT '1',
  `updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  UNIQUE KEY `section` (`section`,`user`),
  KEY `user` (`user`),
  CONSTRAINT `section_votes_ibfk_1` FOREIGN KEY (`section`) REFERENCES `page_sections` (`id`),
  CONSTRAINT `section_votes_ibfk_2` FOREIGN KEY (`user`) REFERENCES `users` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `section_votes`
--

LOCK TABLES `section_votes` WRITE;
/*!40000 ALTER TABLE `section_votes` DISABLE KEYS */;
INSERT INTO `section_votes` VALUES (1,1,1,1,'2012-03-05 05:00:33'),(2,2,1,-1,'2012-03-05 05:00:33');
/*!40000 ALTER TABLE `section_votes` ENABLE KEYS */;
UNLOCK TABLES;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'STRICT_TRANS_TABLES,NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
/*!50003 CREATE*/ /*!50017 DEFINER=`learn`@`localhost`*/ /*!50003 TRIGGER check_section_votes BEFORE INSERT ON section_votes
FOR EACH ROW 
BEGIN
	IF NEW.up_down < -1 THEN
		SET NEW.up_down = -1;
	END IF;
	IF NEW.up_down > -1 THEN
		SET NEW.up_down = 1;
	END IF;
END */;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = utf8 */ ;
/*!50003 SET character_set_results = utf8 */ ;
/*!50003 SET collation_connection  = utf8_general_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = 'STRICT_TRANS_TABLES,NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' */ ;
DELIMITER ;;
/*!50003 CREATE*/ /*!50017 DEFINER=`learn`@`localhost`*/ /*!50003 TRIGGER checkup_section_votes BEFORE UPDATE ON section_votes
FOR EACH ROW 
BEGIN
	IF NEW.up_down < -1 THEN
		SET NEW.up_down = -1;
	END IF;
	IF NEW.up_down > -1 THEN
		SET NEW.up_down = 1;
	END IF;
END */;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;

--
-- Table structure for table `users`
--

DROP TABLE IF EXISTS `users`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `users` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `username` varbinary(200) NOT NULL,
  `email` varbinary(500) NOT NULL,
  `password` varbinary(200) NOT NULL,
  `salt` varchar(3) NOT NULL,
  `nickname` varchar(100) DEFAULT NULL,
  `updated` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  UNIQUE KEY `username` (`username`)
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `users`
--

LOCK TABLES `users` WRITE;
/*!40000 ALTER TABLE `users` DISABLE KEYS */;
INSERT INTO `users` VALUES (1,'Fearless Leader','dev@learn2spell.com','`╬°Y`ш!\'╝╫▄j\'E','abc','Dev','2012-03-05 04:59:04'),(2,'setup','qa@learn2spell.com','Rйт~_\nрIN╙bВ3≥ЕV╪÷⌠sы\"b&#BЧЩ9А╝','123','setup','2012-03-05 04:59:05'),(3,'https://me.yahoo.com/deuseldorf#7f5a9','enterclevernamehere@yahoo.com','iЬ≤│йBadСTд\nе▌П','e94','Dues','2012-03-05 06:55:44');
/*!40000 ALTER TABLE `users` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2012-03-05  2:20:53
