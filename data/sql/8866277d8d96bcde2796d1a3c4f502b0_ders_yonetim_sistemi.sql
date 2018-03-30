/*
Navicat MySQL Data Transfer

Source Server         : local::MySql
Source Server Version : 50522
Source Host           : localhost:3306
Source Database       : ders_yonetim_sistemi

Target Server Type    : MYSQL
Target Server Version : 50522
File Encoding         : 65001

Date: 2012-05-05 08:29:17
*/

SET FOREIGN_KEY_CHECKS=0;

-- ----------------------------
-- Table structure for `department`
-- ----------------------------
DROP TABLE IF EXISTS `department`;
CREATE TABLE `department` (
  `id` int(10) NOT NULL AUTO_INCREMENT,
  `name` varchar(50) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `name` (`name`)
) ENGINE=InnoDB AUTO_INCREMENT=9 DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of department
-- ----------------------------
INSERT INTO `department` VALUES ('1', 'Bilgisayar Mühendisliği');
INSERT INTO `department` VALUES ('5', 'Biyoloji');
INSERT INTO `department` VALUES ('3', 'Fizik');
INSERT INTO `department` VALUES ('2', 'Gıda Mühendisliği');
INSERT INTO `department` VALUES ('4', 'Kimya');
INSERT INTO `department` VALUES ('7', 'Matematik Öğretmenliği');
INSERT INTO `department` VALUES ('6', 'Psikoloji');
INSERT INTO `department` VALUES ('8', 'Tıp');

-- ----------------------------
-- Table structure for `enroll`
-- ----------------------------
DROP TABLE IF EXISTS `enroll`;
CREATE TABLE `enroll` (
  `id` int(10) NOT NULL AUTO_INCREMENT,
  `lesson_fk` int(11) DEFAULT NULL,
  `student_fk` int(10) DEFAULT NULL,
  `semester` char(1) NOT NULL,
  `year` int(11) NOT NULL,
  `grade` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `lesson_fk_student_fk_semester_year` (`lesson_fk`,`student_fk`,`semester`,`year`),
  KEY `student_fk_enroll` (`student_fk`),
  CONSTRAINT `student_fk_enroll` FOREIGN KEY (`student_fk`) REFERENCES `student` (`id`) ON DELETE SET NULL ON UPDATE CASCADE,
  CONSTRAINT `lesson_fk_enroll` FOREIGN KEY (`lesson_fk`) REFERENCES `lesson` (`id`) ON DELETE SET NULL ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of enroll
-- ----------------------------

-- ----------------------------
-- Table structure for `lecturer`
-- ----------------------------
DROP TABLE IF EXISTS `lecturer`;
CREATE TABLE `lecturer` (
  `id` int(10) NOT NULL AUTO_INCREMENT,
  `name` varchar(50) NOT NULL,
  `surname` varchar(50) NOT NULL,
  `department_fk` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `department_fk_lecturer` (`department_fk`),
  CONSTRAINT `department_fk_lecturer` FOREIGN KEY (`department_fk`) REFERENCES `department` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=13 DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of lecturer
-- ----------------------------
INSERT INTO `lecturer` VALUES ('1', 'Nazlı ', 'İkizler', '1');
INSERT INTO `lecturer` VALUES ('3', 'Ali', 'Saatçi', '1');
INSERT INTO `lecturer` VALUES ('4', 'Ezgi', 'Demirbaş', '6');
INSERT INTO `lecturer` VALUES ('5', 'Mücahid', 'Çintaş', '5');
INSERT INTO `lecturer` VALUES ('6', 'Selman ', 'Bozkır', '1');
INSERT INTO `lecturer` VALUES ('7', 'Burcu', 'Can', '1');
INSERT INTO `lecturer` VALUES ('8', 'Sevil', 'Şen', '1');
INSERT INTO `lecturer` VALUES ('9', 'Rıza', 'Tekin', '3');
INSERT INTO `lecturer` VALUES ('10', 'Halil', 'Sezai', '6');
INSERT INTO `lecturer` VALUES ('11', 'Ferhat', 'Göçer', '8');

-- ----------------------------
-- Table structure for `lecturing`
-- ----------------------------
DROP TABLE IF EXISTS `lecturing`;
CREATE TABLE `lecturing` (
  `id` int(10) NOT NULL AUTO_INCREMENT,
  `lesson_fk` int(11) DEFAULT NULL,
  `lecturer_fk` int(10) DEFAULT NULL,
  `semester` char(1) NOT NULL,
  `year` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `lesson_fk_lecturer_fk_semester_year` (`lesson_fk`,`lecturer_fk`,`semester`,`year`),
  KEY `lecturer_fk_lecturing` (`lecturer_fk`),
  CONSTRAINT `lesson_fk_lecturing` FOREIGN KEY (`lesson_fk`) REFERENCES `lesson` (`id`) ON DELETE SET NULL ON UPDATE CASCADE,
  CONSTRAINT `lecturer_fk_lecturing` FOREIGN KEY (`lecturer_fk`) REFERENCES `lecturer` (`id`) ON DELETE SET NULL ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of lecturing
-- ----------------------------

-- ----------------------------
-- Table structure for `lesson`
-- ----------------------------
DROP TABLE IF EXISTS `lesson`;
CREATE TABLE `lesson` (
  `id` int(10) NOT NULL AUTO_INCREMENT,
  `name` text NOT NULL,
  `code` varchar(50) NOT NULL,
  `credit` int(11) NOT NULL,
  `department_fk` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `department_fk_lesson` (`department_fk`),
  CONSTRAINT `department_fk_lesson` FOREIGN KEY (`department_fk`) REFERENCES `department` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB AUTO_INCREMENT=21 DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of lesson
-- ----------------------------
INSERT INTO `lesson` VALUES ('1', 'Bilgisayar Mühendisliğine Giriş', 'Bil136', '3', '1');
INSERT INTO `lesson` VALUES ('2', 'Bilgisayar Programlama I', 'Bil131', '3', '1');
INSERT INTO `lesson` VALUES ('3', 'Bilgisayar Programlama II', 'Bil132', '3', '1');
INSERT INTO `lesson` VALUES ('4', 'Veri Yapıları', 'Bil233', '3', '1');
INSERT INTO `lesson` VALUES ('5', 'Kardiyoloji', 'Tıp342', '4', '8');
INSERT INTO `lesson` VALUES ('6', 'Nöroloji', 'Tıp232', '5', '8');
INSERT INTO `lesson` VALUES ('7', 'Ortopedi', 'Tıp', '4', '8');
INSERT INTO `lesson` VALUES ('8', 'Anatomi', 'Tıp456', '5', '8');
INSERT INTO `lesson` VALUES ('9', 'Gıda123', 'Gıd123', '3', '2');
INSERT INTO `lesson` VALUES ('10', 'Sentetik Katkılar', 'Gıd345', '1', '2');
INSERT INTO `lesson` VALUES ('11', 'Sinyal İşleme', 'Fiz144', '4', '3');
INSERT INTO `lesson` VALUES ('12', 'Ayrodinamik', 'Fiz355', '3', '3');
INSERT INTO `lesson` VALUES ('13', 'Organik Kimya', 'Kim123', '3', '4');
INSERT INTO `lesson` VALUES ('14', 'Ekoloji', 'Kim234', '2', '4');
INSERT INTO `lesson` VALUES ('15', 'Anatomi', 'Biy342', '4', '5');
INSERT INTO `lesson` VALUES ('16', 'Evrimsel Biyoloji', 'Biy212', '5', '5');
INSERT INTO `lesson` VALUES ('17', 'Mesleki Psikoloji', 'Psi123', '3', '6');
INSERT INTO `lesson` VALUES ('18', 'Hastalık Psikolojisi', 'Psi362', '3', '6');
INSERT INTO `lesson` VALUES ('19', 'Analiz I', 'Mat231', '5', '7');
INSERT INTO `lesson` VALUES ('20', 'Cebir', 'Mat345', '3', '7');

-- ----------------------------
-- Table structure for `student`
-- ----------------------------
DROP TABLE IF EXISTS `student`;
CREATE TABLE `student` (
  `id` int(10) NOT NULL AUTO_INCREMENT,
  `name` varchar(50) NOT NULL,
  `surname` varchar(50) NOT NULL,
  `number` varchar(50) NOT NULL,
  `department_fk` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `number` (`number`),
  KEY `department_fk` (`department_fk`),
  CONSTRAINT `department_fk` FOREIGN KEY (`department_fk`) REFERENCES `department` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB AUTO_INCREMENT=15 DEFAULT CHARSET=utf8;

-- ----------------------------
-- Records of student
-- ----------------------------
INSERT INTO `student` VALUES ('1', 'İlknur', 'Kabadayı', '20724821', '1');
INSERT INTO `student` VALUES ('2', 'Betül', 'Altındiş', '20724428', '1');
INSERT INTO `student` VALUES ('3', 'Tuğba', 'Filiz', '20812345', '6');
INSERT INTO `student` VALUES ('4', 'Rumeysa ', 'Demirbaş', '20624821', '8');
INSERT INTO `student` VALUES ('5', 'Ali', 'Veli', '20612345', '3');
INSERT INTO `student` VALUES ('7', 'İhsan', 'Çalışkan', '208', '1');
INSERT INTO `student` VALUES ('8', 'Celal', 'Akdağ', '2090', '5');
INSERT INTO `student` VALUES ('9', 'Ali', 'Rıza', '21', '6');
INSERT INTO `student` VALUES ('10', 'Özge', 'Eken', '234', '1');
INSERT INTO `student` VALUES ('11', 'Betül', 'Tekin', '2324', '1');
INSERT INTO `student` VALUES ('12', 'Ayşe', 'Yılmaz', '23535', '2');
INSERT INTO `student` VALUES ('13', 'Abdullah', 'Çintaş', '4547457', '4');
INSERT INTO `student` VALUES ('14', 'Hayrıye', 'Ayaz', '9795', '7');
