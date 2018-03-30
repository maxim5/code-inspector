-- ---------------------------------------------
-- Example data for tagging-related-articles.php
-- ---------------------------------------------

-- ----------------------------
-- Table structure for `articles`
-- ----------------------------
DROP TABLE IF EXISTS `articles`;
CREATE TABLE `articles` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `date_created` datetime NOT NULL,
  `date_modified` datetime NOT NULL,
  `title` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=20 DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

-- ----------------------------
-- Records of articles
-- ----------------------------
INSERT INTO `articles` VALUES ('4', '2010-08-22 02:11:11', '2012-01-02 03:28:00', 'MySQL order by hack');
INSERT INTO `articles` VALUES ('5', '2010-08-24 05:47:43', '2012-02-14 05:21:02', 'FAQ with jQuery');
INSERT INTO `articles` VALUES ('6', '2010-09-05 04:04:07', '2012-02-10 05:00:02', 'Portfolio with jQuery');
INSERT INTO `articles` VALUES ('8', '2011-12-31 05:57:00', '2012-01-21 05:48:45', 'Google Analytics for mobile on PHP4');
INSERT INTO `articles` VALUES ('9', '2012-01-18 09:26:39', '2012-02-10 04:59:04', 'Delphi operator overloading');
INSERT INTO `articles` VALUES ('10', '2012-02-27 02:08:26', '2012-03-02 05:11:52', 'AJAX loading gallery with HighSlide');
INSERT INTO `articles` VALUES ('11', '2012-06-09 09:36:20', '2012-06-09 09:36:59', 'ReplaceStrings function');
INSERT INTO `articles` VALUES ('12', '2012-06-16 04:15:05', '2012-06-16 04:15:05', 'Create HTML table');
INSERT INTO `articles` VALUES ('13', '2012-08-04 05:19:06', '2012-08-09 03:43:11', 'Using a PHP array for MySQL table definition');
INSERT INTO `articles` VALUES ('14', '2012-09-21 07:36:29', '2012-09-21 07:36:29', 'Using PHP/MySQL with error checking');
INSERT INTO `articles` VALUES ('15', '2012-09-22 03:23:58', '2012-10-20 13:14:41', 'Using PHP/MySQLi with error checking');
INSERT INTO `articles` VALUES ('16', '2012-09-28 04:04:10', '2012-10-20 13:14:26', 'Using PHP/PDO with error checking');
INSERT INTO `articles` VALUES ('17', '2012-10-20 13:12:57', '2012-10-22 03:53:07', 'Introduction to PHP\'s Object Orientation');
INSERT INTO `articles` VALUES ('18', '2012-12-07 05:26:22', '2012-12-07 05:26:22', 'Using PHP/SQLite2 with error checking');
INSERT INTO `articles` VALUES ('19', '2012-12-07 07:12:39', '2012-12-07 07:12:39', 'Using PHP/SQLite3 with error checking');

-- ----------------------------
-- Table structure for `articles_tags`
-- ----------------------------
DROP TABLE IF EXISTS `articles_tags`;
CREATE TABLE `articles_tags` (
  `article_id` bigint(20) unsigned NOT NULL,
  `tag_id` bigint(20) unsigned NOT NULL,
  UNIQUE KEY `idx_article_tag` (`article_id`,`tag_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

-- ----------------------------
-- Records of articles_tags
-- ----------------------------
INSERT INTO `articles_tags` VALUES ('4', '5');
INSERT INTO `articles_tags` VALUES ('4', '6');
INSERT INTO `articles_tags` VALUES ('4', '8');
INSERT INTO `articles_tags` VALUES ('4', '9');
INSERT INTO `articles_tags` VALUES ('5', '10');
INSERT INTO `articles_tags` VALUES ('5', '11');
INSERT INTO `articles_tags` VALUES ('5', '17');
INSERT INTO `articles_tags` VALUES ('6', '10');
INSERT INTO `articles_tags` VALUES ('6', '12');
INSERT INTO `articles_tags` VALUES ('6', '17');
INSERT INTO `articles_tags` VALUES ('7', '14');
INSERT INTO `articles_tags` VALUES ('7', '15');
INSERT INTO `articles_tags` VALUES ('7', '16');
INSERT INTO `articles_tags` VALUES ('7', '17');
INSERT INTO `articles_tags` VALUES ('8', '18');
INSERT INTO `articles_tags` VALUES ('8', '19');
INSERT INTO `articles_tags` VALUES ('8', '20');
INSERT INTO `articles_tags` VALUES ('8', '22');
INSERT INTO `articles_tags` VALUES ('9', '14');
INSERT INTO `articles_tags` VALUES ('9', '15');
INSERT INTO `articles_tags` VALUES ('9', '16');
INSERT INTO `articles_tags` VALUES ('9', '17');
INSERT INTO `articles_tags` VALUES ('10', '10');
INSERT INTO `articles_tags` VALUES ('10', '23');
INSERT INTO `articles_tags` VALUES ('10', '24');
INSERT INTO `articles_tags` VALUES ('11', '14');
INSERT INTO `articles_tags` VALUES ('11', '17');
INSERT INTO `articles_tags` VALUES ('11', '26');
INSERT INTO `articles_tags` VALUES ('12', '22');
INSERT INTO `articles_tags` VALUES ('12', '27');
INSERT INTO `articles_tags` VALUES ('12', '28');
INSERT INTO `articles_tags` VALUES ('12', '29');
INSERT INTO `articles_tags` VALUES ('13', '5');
INSERT INTO `articles_tags` VALUES ('13', '22');
INSERT INTO `articles_tags` VALUES ('13', '29');
INSERT INTO `articles_tags` VALUES ('13', '30');
INSERT INTO `articles_tags` VALUES ('14', '5');
INSERT INTO `articles_tags` VALUES ('14', '22');
INSERT INTO `articles_tags` VALUES ('15', '5');
INSERT INTO `articles_tags` VALUES ('15', '22');
INSERT INTO `articles_tags` VALUES ('15', '32');
INSERT INTO `articles_tags` VALUES ('16', '5');
INSERT INTO `articles_tags` VALUES ('16', '22');
INSERT INTO `articles_tags` VALUES ('16', '32');
INSERT INTO `articles_tags` VALUES ('17', '22');
INSERT INTO `articles_tags` VALUES ('17', '32');
INSERT INTO `articles_tags` VALUES ('18', '22');
INSERT INTO `articles_tags` VALUES ('18', '33');
INSERT INTO `articles_tags` VALUES ('19', '22');
INSERT INTO `articles_tags` VALUES ('19', '33');

-- ----------------------------
-- Table structure for `tags`
-- ----------------------------
DROP TABLE IF EXISTS `tags`;
CREATE TABLE `tags` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `idx_name` (`name`)
) ENGINE=InnoDB AUTO_INCREMENT=34 DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

-- ----------------------------
-- Records of tags
-- ----------------------------
INSERT INTO `tags` VALUES ('23', 'ajax');
INSERT INTO `tags` VALUES ('19', 'analytics');
INSERT INTO `tags` VALUES ('9', 'cast');
INSERT INTO `tags` VALUES ('30', 'class');
INSERT INTO `tags` VALUES ('27', 'create');
INSERT INTO `tags` VALUES ('14', 'delphi');
INSERT INTO `tags` VALUES ('13', 'demo');
INSERT INTO `tags` VALUES ('17', 'example');
INSERT INTO `tags` VALUES ('11', 'faq');
INSERT INTO `tags` VALUES ('8', 'find-in-set');
INSERT INTO `tags` VALUES ('26', 'function');
INSERT INTO `tags` VALUES ('24', 'gallery');
INSERT INTO `tags` VALUES ('18', 'google');
INSERT INTO `tags` VALUES ('7', 'hack');
INSERT INTO `tags` VALUES ('28', 'html');
INSERT INTO `tags` VALUES ('25', 'javascript');
INSERT INTO `tags` VALUES ('10', 'jquery');
INSERT INTO `tags` VALUES ('20', 'mobile');
INSERT INTO `tags` VALUES ('5', 'mysql');
INSERT INTO `tags` VALUES ('32', 'oop');
INSERT INTO `tags` VALUES ('15', 'operator');
INSERT INTO `tags` VALUES ('6', 'order-by');
INSERT INTO `tags` VALUES ('16', 'overloading');
INSERT INTO `tags` VALUES ('22', 'php');
INSERT INTO `tags` VALUES ('21', 'php4');
INSERT INTO `tags` VALUES ('12', 'portfolio');
INSERT INTO `tags` VALUES ('33', 'sqlite');
INSERT INTO `tags` VALUES ('29', 'table');
