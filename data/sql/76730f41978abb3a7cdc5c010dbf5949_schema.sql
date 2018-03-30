# ************************************************************
# Sequel Pro SQL dump
# Version 4096
#
# http://www.sequelpro.com/
# http://code.google.com/p/sequel-pro/
#
# Host: 127.0.0.1 (MySQL 5.5.40-0ubuntu0.12.04.1)
# Database: foos
# Generation Time: 2014-11-21 04:47:02 +0000
# ************************************************************


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;


# Dump of table accounts
# ------------------------------------------------------------

DROP TABLE IF EXISTS `accounts`;

CREATE TABLE `accounts` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `email` varchar(255) NOT NULL DEFAULT '',
  `password` varchar(64) NOT NULL DEFAULT '',
  `created` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `email` (`email`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `accounts` WRITE;
/*!40000 ALTER TABLE `accounts` DISABLE KEYS */;

INSERT INTO `accounts` (`id`, `email`, `password`, `created`)
VALUES
	(51,'user@example.com','5f4dcc3b5aa765d61d8327deb882cf99','2014-02-19 15:31:53');

/*!40000 ALTER TABLE `accounts` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table games
# ------------------------------------------------------------

DROP TABLE IF EXISTS `games`;

CREATE TABLE `games` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `account_id` int(11) DEFAULT NULL,
  `side_1_score` int(11) DEFAULT NULL,
  `side_2_score` int(11) DEFAULT NULL,
  `created` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `user_id` (`account_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `games` WRITE;
/*!40000 ALTER TABLE `games` DISABLE KEYS */;

INSERT INTO `games` (`id`, `account_id`, `side_1_score`, `side_2_score`, `created`)
VALUES
	(1,51,10,5,'2014-02-19 15:32:29'),
	(2,51,10,5,'2014-02-19 15:32:51'),
	(3,51,10,5,'2014-02-19 15:33:08'),
	(4,51,10,5,'2014-02-19 15:33:29'),
	(5,51,10,5,'2014-02-19 15:33:47'),
	(6,51,10,5,'2014-02-19 15:33:59'),
	(7,51,10,5,'2014-11-21 04:46:00'),
	(8,51,10,5,'2014-11-21 04:46:11'),
	(9,51,10,5,'2014-11-21 04:46:20'),
	(10,51,10,5,'2014-11-21 04:46:31');

/*!40000 ALTER TABLE `games` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table games_players
# ------------------------------------------------------------

DROP TABLE IF EXISTS `games_players`;

CREATE TABLE `games_players` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `player_id` int(11) NOT NULL DEFAULT '0',
  `game_id` int(11) NOT NULL DEFAULT '0',
  `side` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `games_players` (`player_id`,`game_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `games_players` WRITE;
/*!40000 ALTER TABLE `games_players` DISABLE KEYS */;

INSERT INTO `games_players` (`id`, `player_id`, `game_id`, `side`)
VALUES
	(1,1,1,1),
	(2,2,1,1),
	(3,3,1,2),
	(4,4,1,2),
	(5,1,2,1),
	(6,3,2,1),
	(7,2,2,2),
	(8,4,2,2),
	(9,1,3,1),
	(10,4,3,1),
	(11,2,3,2),
	(12,3,3,2),
	(13,2,4,1),
	(14,3,4,1),
	(15,1,4,2),
	(16,4,4,2),
	(17,2,5,1),
	(18,4,5,1),
	(19,1,5,2),
	(20,3,5,2),
	(21,3,6,1),
	(22,4,6,1),
	(23,1,6,2),
	(24,2,6,2),
	(25,1,7,1),
	(26,2,7,2),
	(27,1,8,1),
	(28,3,8,2),
	(29,2,9,1),
	(30,4,9,2),
	(31,2,10,1),
	(32,4,10,2);

/*!40000 ALTER TABLE `games_players` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table pages
# ------------------------------------------------------------

DROP TABLE IF EXISTS `pages`;

CREATE TABLE `pages` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `slug` varchar(255) DEFAULT NULL,
  `title` varchar(255) DEFAULT NULL,
  `created` date DEFAULT NULL,
  `modified` date DEFAULT NULL,
  `body` text,
  `active` tinyint(4) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `pages` WRITE;
/*!40000 ALTER TABLE `pages` DISABLE KEYS */;

INSERT INTO `pages` (`id`, `slug`, `title`, `created`, `modified`, `body`, `active`)
VALUES
	(1,'about','About','2009-03-29','2009-03-29','<p>This is an application that I made in an evening to keep track of the games we played at work.</p>\n\n<h3>Version 2.0</h3>\n<p>&raquo; Conversion to CakePHP framework, replaced player graph.</p>\n\n<h3>Version 1.5.1</h3>\n<p>&raquo; Various bugfixes, formatting and layout fixes. Never launched, went straight to 2.0 instead.</p>\n\n<h3>Version 1.5</h3>\n<p>&raquo; Ranking algorithm installed.</p>\n\n<h3>Version 1.01</h3>\n<p>&raquo; Updated CSS and bug fixes. Games ordered by date played.</p>\n\n<h3>Version 1.0</h3>\n<p>&raquo; Initial Launch.</p>',1),
	(2,'contact','Contact','2009-03-29','2009-03-29','<p>To contact me, send an email to dave at zastica dot com. I\'ll get back to you as soon as I can.</p>',1);

/*!40000 ALTER TABLE `pages` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table players
# ------------------------------------------------------------

DROP TABLE IF EXISTS `players`;

CREATE TABLE `players` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `account_id` int(11) DEFAULT NULL,
  `name` varchar(32) DEFAULT NULL,
  `rank` int(11) unsigned DEFAULT '1000',
  `foos_rank` int(11) unsigned DEFAULT '1000',
  `foos_performance_rank` int(11) unsigned DEFAULT '1000',
  `elo_rank` int(11) unsigned DEFAULT '1000',
  `status` enum('active','inactive','retired') DEFAULT 'active',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `players` WRITE;
/*!40000 ALTER TABLE `players` DISABLE KEYS */;

INSERT INTO `players` (`id`, `account_id`, `name`, `rank`, `foos_rank`, `foos_performance_rank`, `elo_rank`, `status`)
VALUES
	(1,51,'Player 1',1033,1050,1026,1024,'active'),
	(2,51,'Player 2',1023,1039,1012,1018,'active'),
	(3,51,'Player 3',992,1005,984,987,'active'),
	(4,51,'Player 4',977,992,968,971,'active'),
	(5,51,'Unranked',1000,1000,1000,1000,'active'),
	(6,51,'Unranked',1000,1000,1000,1000,'active'),
	(7,51,'Retired',1000,1000,1000,1000,'retired');

/*!40000 ALTER TABLE `players` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table posts
# ------------------------------------------------------------

DROP TABLE IF EXISTS `posts`;

CREATE TABLE `posts` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `title` varchar(255) DEFAULT NULL,
  `subtitle` varchar(255) DEFAULT NULL,
  `created` datetime DEFAULT NULL,
  `body` text,
  `active` tinyint(4) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `posts` WRITE;
/*!40000 ALTER TABLE `posts` DISABLE KEYS */;

INSERT INTO `posts` (`id`, `title`, `subtitle`, `created`, `body`, `active`)
VALUES
	(1,'Score Tracker Launched','Version 1.0','2007-06-01 09:00:00','<p>I\'m announcing the launch of the Foosball League Score Tracker. It\'s super easy to use, just sign up, add your players, and begin to record games. Maybe if you\'re good enough, you can show up on the top players list. Easy as pie.</p>',1),
	(2,'Minor Updates','Version 1.01','2007-06-03 22:00:00','<p>Made some minor updates tonight, including a bugfix and some CSS updates. Games are now sorted by the date they were entered. Coming up soon: an updated rank algorithm.</p>',1),
	(3,'Updates: Rank Algorithm, CSS, and Stats','Version 1.5','2007-06-07 22:26:18','<p>I finished the first iteration of the ranking algorithm that this site uses. Rank is calculated based on how well you play in games and compared to how well you\'re expected to perform. The algorithm is subject to change at my whim, but it\'s as fair as I could make it. I\'d prefer to use an ELO or TrueSkill type algorithm, but it would take a lot more to develop. Sometime soon, perhaps.</p>',1),
	(4,'Announcing Version 2.0','','2009-03-29 00:00:00','<p>Well, it\'s been awhile since I did any development on foos.zastica. I recently decided to try out the CakePHP framework, and instead of creating an entirely new application, I decided to convert an existing one that I had already created. I chose this app because it was pretty straight forward, and didn\'t have a lot of odd features.</p>\n\n<p>About halfway through conversion, however, I learned that it wasn\'t as simple as I had thought. There are a lot of interesting database queries that need to be done to get the aggregate account and player data, and Cake isn\'t optimized to handle that. This left me doing a lot of direct queries on the database. Although I\'m sure that part of that is my lack of CakePHP knowledge as well.</p>\n\n<p>So, as of today, foos.zastica is now in v2.0. Most of the changes are backend type stuff, but there\'s a few front-end changes as well. I\'ve widened the site, changed around some styles, and changed the player graph to use the flot jquery library. This allows IE users to see the graph. It used to be in SVG, but some users weren\'t able to see the graph so it had to go.</p>\n\n<p>One of the bigger updates is the RSS syndication of updates. Whenever the site updates, you can get notified in your RSS reader. To subscribe, go to <a href=\"http://foos.zastica.com/posts/rss/\">/posts/rss/</a></p>\n\n<p>I\'ve also got some more changes up my sleeve, but I\'ll wait on announcing those until I\'ve got a better idea when they will be ready.</p>',1);

/*!40000 ALTER TABLE `posts` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table rank_track
# ------------------------------------------------------------

DROP TABLE IF EXISTS `rank_track`;

CREATE TABLE `rank_track` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `players_id` int(11) DEFAULT NULL,
  `games_id` int(11) DEFAULT NULL,
  `rank` int(11) unsigned DEFAULT '1000',
  `notes` varchar(255) NOT NULL DEFAULT '',
  `foos_rank` int(11) unsigned DEFAULT '1000',
  `foos_performance_rank` int(11) unsigned DEFAULT '1000',
  `elo_rank` int(11) unsigned DEFAULT '1000',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `rank_track` WRITE;
/*!40000 ALTER TABLE `rank_track` DISABLE KEYS */;

INSERT INTO `rank_track` (`id`, `players_id`, `games_id`, `rank`, `notes`, `foos_rank`, `foos_performance_rank`, `elo_rank`)
VALUES
	(1,1,1,1000,'Player 1, Player 2: 10; Player 3, Player 4: 5',1000,1000,1000),
	(2,2,1,1000,'Player 1, Player 2: 10; Player 3, Player 4: 5',1000,1000,1000),
	(3,3,1,1000,'Player 1, Player 2: 10; Player 3, Player 4: 5',1000,1000,1000),
	(4,4,1,1000,'Player 1, Player 2: 10; Player 3, Player 4: 5',1000,1000,1000),
	(5,1,2,1016,'Player 1, Player 3: 10; Player 2, Player 4: 5',1018,1015,1016),
	(6,3,2,986,'Player 1, Player 3: 10; Player 2, Player 4: 5',988,985,984),
	(7,2,2,1016,'Player 1, Player 3: 10; Player 2, Player 4: 5',1018,1015,1016),
	(8,4,2,986,'Player 1, Player 3: 10; Player 2, Player 4: 5',988,985,984),
	(9,1,3,1033,'Player 1, Player 4: 10; Player 2, Player 3: 5',1036,1030,1032),
	(10,4,3,971,'Player 1, Player 4: 10; Player 2, Player 3: 5',976,970,968),
	(11,2,3,1002,'Player 1, Player 4: 10; Player 2, Player 3: 5',1006,1000,1000),
	(12,3,3,1002,'Player 1, Player 4: 10; Player 2, Player 3: 5',1006,1000,1000),
	(13,2,4,988,'Player 2, Player 3: 10; Player 1, Player 4: 5',994,985,984),
	(14,3,4,988,'Player 2, Player 3: 10; Player 1, Player 4: 5',994,985,984),
	(15,1,4,1049,'Player 2, Player 3: 10; Player 1, Player 4: 5',1054,1045,1048),
	(16,4,4,988,'Player 2, Player 3: 10; Player 1, Player 4: 5',994,985,984),
	(17,2,5,1005,'Player 2, Player 4: 10; Player 1, Player 3: 5',1012,1000,1003),
	(18,4,5,972,'Player 2, Player 4: 10; Player 1, Player 3: 5',981,969,965),
	(19,1,5,1033,'Player 2, Player 4: 10; Player 1, Player 3: 5',1041,1029,1029),
	(20,3,5,1005,'Player 2, Player 4: 10; Player 1, Player 3: 5',1012,1000,1003),
	(21,3,6,989,'Player 3, Player 4: 10; Player 1, Player 2: 5',999,984,984),
	(22,4,6,989,'Player 3, Player 4: 10; Player 1, Player 2: 5',999,984,984),
	(23,1,6,1017,'Player 3, Player 4: 10; Player 1, Player 2: 5',1028,1013,1010),
	(24,2,6,1022,'Player 3, Player 4: 10; Player 1, Player 2: 5',1030,1015,1022),
	(25,1,7,1015,'Player 1: 10; Player 2: 5',1015,997,991),
	(26,2,7,1017,'Player 1: 10; Player 2: 5',1017,999,1003),
	(27,1,8,1018,'Player 1: 10; Player 3: 5',1033,1012,1008),
	(28,3,8,1017,'Player 1: 10; Player 3: 5',1017,999,1003),
	(29,2,9,991,'Player 2: 10; Player 4: 5',1004,983,986),
	(30,4,9,1017,'Player 2: 10; Player 4: 5',1017,999,1003),
	(31,2,10,1008,'Player 2: 10; Player 4: 5',1022,998,1003),
	(32,4,10,991,'Player 2: 10; Player 4: 5',1004,983,986);

/*!40000 ALTER TABLE `rank_track` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table seasons
# ------------------------------------------------------------

DROP TABLE IF EXISTS `seasons`;

CREATE TABLE `seasons` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `account_id` int(11) NOT NULL,
  `created` datetime DEFAULT NULL,
  `updated` datetime DEFAULT NULL,
  `archived` datetime DEFAULT NULL,
  `name` varchar(128) DEFAULT NULL,
  `status` enum('active','archived') DEFAULT NULL,
  `games_played` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `seasons` WRITE;
/*!40000 ALTER TABLE `seasons` DISABLE KEYS */;

INSERT INTO `seasons` (`id`, `account_id`, `created`, `updated`, `archived`, `name`, `status`, `games_played`)
VALUES
	(1,51,'2014-11-20 05:00:07','2014-11-20 05:00:07',NULL,'Season 1','active',4);

/*!40000 ALTER TABLE `seasons` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table seasons_games
# ------------------------------------------------------------

DROP TABLE IF EXISTS `seasons_games`;

CREATE TABLE `seasons_games` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `season_id` int(11) DEFAULT NULL,
  `game_id` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `seasons_games` WRITE;
/*!40000 ALTER TABLE `seasons_games` DISABLE KEYS */;

INSERT INTO `seasons_games` (`id`, `season_id`, `game_id`)
VALUES
	(1,1,7),
	(2,1,8),
	(3,1,9),
	(4,1,10);

/*!40000 ALTER TABLE `seasons_games` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table seasons_ranks
# ------------------------------------------------------------

DROP TABLE IF EXISTS `seasons_ranks`;

CREATE TABLE `seasons_ranks` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `season_id` int(11) DEFAULT NULL,
  `player_id` int(11) DEFAULT NULL,
  `rank` int(11) DEFAULT '1000',
  `foos_rank` int(11) DEFAULT '1000',
  `foos_performance_rank` int(11) DEFAULT '1000',
  `elo_rank` int(11) DEFAULT '1000',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `seasons_ranks` WRITE;
/*!40000 ALTER TABLE `seasons_ranks` DISABLE KEYS */;

INSERT INTO `seasons_ranks` (`id`, `season_id`, `player_id`, `rank`, `foos_rank`, `foos_performance_rank`, `elo_rank`)
VALUES
	(1,1,1,1032,1035,1029,1031),
	(2,1,2,1018,1023,1014,1016),
	(3,1,3,986,988,985,985),
	(4,1,4,971,975,969,968);

/*!40000 ALTER TABLE `seasons_ranks` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table users
# ------------------------------------------------------------

DROP TABLE IF EXISTS `users`;

CREATE TABLE `users` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `username` varchar(255) NOT NULL DEFAULT '',
  `password` varchar(64) DEFAULT NULL,
  `email` varchar(255) DEFAULT NULL,
  `created` datetime DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `users` WRITE;
/*!40000 ALTER TABLE `users` DISABLE KEYS */;

INSERT INTO `users` (`id`, `username`, `password`, `email`, `created`)
VALUES
	(1,'admin','5f4dcc3b5aa765d61d8327deb882cf99','admin@example.com','2014-03-11 20:16:47');

/*!40000 ALTER TABLE `users` ENABLE KEYS */;
UNLOCK TABLES;



/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;
/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
