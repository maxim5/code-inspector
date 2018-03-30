-- phpMyAdmin SQL Dump
-- version 3.5.2
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Erstellungszeit: 08. Sep 2012 um 11:31
-- Server Version: 5.5.25a
-- PHP-Version: 5.4.4

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";

--
-- Datenbank: `golden-boy`
--

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `module_golf_background`
--

CREATE TABLE IF NOT EXISTS `module_golf_background` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `path` varchar(355) COLLATE utf8_unicode_ci DEFAULT NULL,
  `status` int(11) NOT NULL DEFAULT '2',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=2 ;

--
-- Daten für Tabelle `module_golf_background`
--

INSERT INTO `module_golf_background` (`id`, `path`, `status`) VALUES
(1, '/upload/golf_app/background/1/corellia-screenshot-001.jpg', 1);

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `module_golf_column`
--

CREATE TABLE IF NOT EXISTS `module_golf_column` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `sort` varchar(12) COLLATE utf8_unicode_ci NOT NULL DEFAULT '1',
  `type_id` int(11) NOT NULL DEFAULT '1',
  `status` int(11) NOT NULL DEFAULT '1',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=7 ;

--
-- Daten für Tabelle `module_golf_column`
--

INSERT INTO `module_golf_column` (`id`, `sort`, `type_id`, `status`) VALUES
(5, '1', 2, 2),
(6, '2', 2, 2);

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `module_golf_column_type`
--

CREATE TABLE IF NOT EXISTS `module_golf_column_type` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=3 ;

--
-- Daten für Tabelle `module_golf_column_type`
--

INSERT INTO `module_golf_column_type` (`id`, `name`) VALUES
(1, 'einspaltig'),
(2, 'zweispaltig');

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `module_golf_logo`
--

CREATE TABLE IF NOT EXISTS `module_golf_logo` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `path` varchar(355) COLLATE utf8_unicode_ci DEFAULT NULL,
  `status` int(11) NOT NULL DEFAULT '2',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=3 ;

--
-- Daten für Tabelle `module_golf_logo`
--

INSERT INTO `module_golf_logo` (`id`, `path`, `status`) VALUES
(2, '/upload/golf_app/logo/2/corellia-konzeptgrafiken-004.jpg', 1);

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `module_golf_teaser`
--

CREATE TABLE IF NOT EXISTS `module_golf_teaser` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `column_id` int(11) NOT NULL,
  `type_id` int(11) NOT NULL,
  `content_id` int(11) DEFAULT NULL,
  `position` int(11) NOT NULL DEFAULT '1',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=13 ;

--
-- Daten für Tabelle `module_golf_teaser`
--

INSERT INTO `module_golf_teaser` (`id`, `column_id`, `type_id`, `content_id`, `position`) VALUES
(9, 5, 2, 13, 1),
(10, 5, 2, 15, 2),
(11, 6, 2, 17, 1),
(12, 6, 2, 19, 2);

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `module_golf_teaser_content`
--

CREATE TABLE IF NOT EXISTS `module_golf_teaser_content` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `teaser_id` int(11) NOT NULL,
  `gallery_id` int(11) DEFAULT NULL,
  `video` text COLLATE utf8_unicode_ci,
  `headline` varchar(555) COLLATE utf8_unicode_ci NOT NULL,
  `body` text COLLATE utf8_unicode_ci,
  `image` varchar(355) COLLATE utf8_unicode_ci DEFAULT NULL,
  `extern_url` varchar(355) COLLATE utf8_unicode_ci DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=20 ;

--
-- Daten für Tabelle `module_golf_teaser_content`
--

INSERT INTO `module_golf_teaser_content` (`id`, `teaser_id`, `gallery_id`, `video`, `headline`, `body`, `image`, `extern_url`) VALUES
(3, 2, 46, NULL, '', NULL, NULL, NULL),
(10, 9, 55, NULL, '', NULL, NULL, NULL),
(11, 9, 56, NULL, '', NULL, NULL, NULL),
(12, 9, 57, NULL, '', NULL, NULL, NULL),
(13, 9, 57, NULL, '', NULL, NULL, NULL),
(14, 10, 59, NULL, '', NULL, NULL, NULL),
(15, 10, 59, NULL, '', NULL, NULL, NULL),
(16, 11, 61, NULL, '', NULL, NULL, NULL),
(17, 11, 61, NULL, '', NULL, NULL, NULL),
(18, 12, 63, NULL, '', NULL, NULL, NULL),
(19, 12, 63, NULL, '', NULL, NULL, NULL);

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `module_golf_teaser_type`
--

CREATE TABLE IF NOT EXISTS `module_golf_teaser_type` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=5 ;

--
-- Daten für Tabelle `module_golf_teaser_type`
--

INSERT INTO `module_golf_teaser_type` (`id`, `name`) VALUES
(1, 'Video'),
(2, 'Gallerie'),
(3, 'Angebot'),
(4, 'Content');

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `module_storefinder`
--

CREATE TABLE IF NOT EXISTS `module_storefinder` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `title` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `subtitle` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `additional_names` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `alias` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `street` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `plz` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `place` varchar(355) COLLATE utf8_unicode_ci DEFAULT NULL,
  `lat` varchar(60) COLLATE utf8_unicode_ci DEFAULT NULL,
  `long` varchar(60) COLLATE utf8_unicode_ci DEFAULT NULL,
  `phone_code` varchar(10) COLLATE utf8_unicode_ci DEFAULT NULL,
  `phone` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `email` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `hours` varchar(355) COLLATE utf8_unicode_ci DEFAULT NULL,
  `image_path` varchar(355) COLLATE utf8_unicode_ci DEFAULT NULL,
  `country` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `create` datetime NOT NULL,
  `update` datetime NOT NULL,
  `import_date` datetime NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=30 ;

--
-- Daten für Tabelle `module_storefinder`
--

INSERT INTO `module_storefinder` (`id`, `title`, `subtitle`, `additional_names`, `alias`, `street`, `plz`, `place`, `lat`, `long`, `phone_code`, `phone`, `email`, `hours`, `image_path`, `country`, `create`, `update`, `import_date`) VALUES
(1, 'BRAX Store Hamburg', 'Alstertal-Einkaufszentrum', 'Alstertal-Einkaufszentrum', 'hamburg', 'Heegbarg 31', '22391', 'Hamburg', '53.65387', '10.09084', '040', '040/61199210', 'Hamburg.AEZ@brax.com', 'Öffnungszeiten: Mo. - Sa. 9:30 bis 20:00 Uhr. Wir freuen uns auf Ihren Besuch.', '/upload/store/1_brax-store-hamburg.jpg', 'Deutschland', '2003-04-20 08:00:00', '2003-04-20 08:00:00', '2012-08-15 11:53:32'),
(3, 'BRAX Store Berlin', 'ALEXA Shoppingcenter', 'im ALEXA Shoppingcenter', 'berlin', 'Gruner Straße 20', '10179', 'Berlin', '52.5197653', '13.414394', '', '030/24724812', 'Berlin.ASC@brax.com', 'Öffnungszeiten: Mo. - Sa. 10.00 - 21.00 Uhr. Wir freuen uns auf Ihren Besuch.', '/upload/store/3_brax-store-berlin.jpg', 'Deutschland', '2003-04-20 08:00:00', '2003-04-20 08:00:00', '2012-08-15 11:53:32'),
(4, 'BRAX Store Düren', '', 'StadtCenter', 'dueren', 'Kuhgasse 8', '52349', 'Düren', '50.80584', '6.47931', '', '02421/9205356', 'Dueren.Stadtcenter@brax.com', 'Öffnungszeiten: Mo. - Sa.   10.00 - 20.00 Uhr. Wir freuen uns auf Ihren Besuch.', '/upload/store/4_brax-store-dueren.jpg', 'Deutschland', '2006-08-20 08:00:00', '2006-08-20 08:00:00', '2012-08-15 11:53:32'),
(5, 'BRAX Store Lüdenscheid', '', 'Stern-Center', 'luedenscheid', 'Wilhelmstraße 33', '58511', 'Lüdenscheid', '51.2157298', '7.6316233', '', '02351/4324576', 'Luedenscheid.Sterncenter@brax.com', 'Öffungszeiten: Mo. - Fr.   10.00 - 20.00 Uhr und Sa. 09.30 - 20.00 Uhr. Wir freuen uns auf Ihren Besuch.', '/upload/store/5_brax-store-luedenscheid.jpg', 'Deutschland', '2006-08-20 08:00:00', '2006-08-20 08:00:00', '2012-08-15 11:53:32'),
(6, 'BRAX Store Arnsberg', '', '', 'arnsberg', 'Apothekerstr. 20', '59755', 'Arnsberg-Neheim', '51.4517', '7.96456', '', '02932/891126', 'Arnsberg.Neheim@brax.com', 'Öffnungszeiten: Mo. - Fr. 9.30 - 18.30 Uhr und Sa. 9.30 - 16.00 Uhr. Wir freuen uns auf Ihren Besuch.', '/upload/store/6_brax-store-arnsberg.jpg', 'Deutschland', '2006-08-20 08:00:00', '2006-08-20 08:00:00', '2012-08-15 11:53:32'),
(8, 'BRAX Store Bremen', '', 'im Einkaufszentrum Waterfront', 'bremen', 'AG-Weser-Straße 1', '28237', 'Bremen', '53.0292532', '8.8061439', '', '0421/6194427', 'Bremen.Waterfront@brax.com', 'Öffnungszeiten: Mo. - Sa. 10.00 - 20.00 Uhr. Wir freuen uns auf Ihren Besuch.', '/upload/store/8_brax-store-bremen.jpg', 'Deutschland', '2011-09-20 08:00:00', '2011-09-20 08:00:00', '2012-08-15 11:53:32'),
(9, 'BRAX Store Hannover', '', 'Ernst-August-Galerie', 'hannover', 'Ernst-August-Platz 2', '30159', 'Hannover', '52.3768459', '9.739157', '', '0511/ 169 20 44', 'Hannover.EAG@brax.com', 'Öffnungszeiten: 10:00 bis 20:00 Uhr. Wir freuen uns auf Ihren Besuch.', '/upload/store/9_brax-store-hannover.jpg', 'Deutschland', '2024-09-20 08:00:00', '2024-09-20 08:00:00', '2012-08-15 11:53:32'),
(10, 'BRAX Store Köln', '', 'Rhein-Center Köln-Weiden', 'koeln', 'Aachener Str. 1253', '50858', 'Köln', '50.937554', '6.835341', '', '02234/9333884', 'Koeln.Rheincenter@brax.com', 'Öffnungszeiten: Mo. - Sa. 10:00 - 20:00 Uhr. Wir freuen uns auf Ihren Besuch.', '/upload/store/10_brax-store-koeln.jpg', 'Deutschland', '2009-10-20 08:00:00', '2009-10-20 08:00:00', '2012-08-15 11:53:32'),
(11, 'BRAX Store Schweinfurt', '', 'Stadtgalerie Schweinfurt', 'schweinfurt', 'Schrammstr. 5', '97421', 'Schweinfurt', '50.041143', '10.2264681', '', '09721 / 7309775', 'Schweinfurt.Stadtgalerie@brax.com', 'Öffnungszeiten: Mo - Sa 9:30 - 20:00 Uhr. Wir freuen uns auf Ihren Besuch.', '/upload/store/11_bild-store-schweinfurt.jpg', 'Deutschland', '2008-02-20 09:00:00', '2008-02-20 09:00:00', '2012-08-15 11:53:32'),
(12, 'BRAX Store Viernheim', '', 'Rhein-Neckar-Zentrum', 'viernheim', 'Robert-Schumann-Str. 8a', '68519', 'Viernheim', '49.5280457', '8.5662823', '', '06204 / 6071603', 'Viernheim.RNZ@BRAX-Fashion.com', 'Öffnungszeiten: Mo - Sa 9:30 - 20:00 Uhr. Wir freuen uns auf Ihren Besuch.', '/upload/store/12_brax-store-viernheim.jpg', 'Deutschland', '2025-02-20 09:00:00', '2025-02-20 09:00:00', '2012-08-15 11:53:32'),
(13, 'BRAX Store Konstanz', '', 'Lago-Shopping Center', 'konstanz', 'Bodanstr. 1', '78462', 'Konstanz', '47.6575711', '9.1764994', '', '07531/2821055', 'Konstanz.LagoCenter@brax.com', 'Öffnungszeiten: Mo-Sa: 9:30 - 20:00 Uhr und Do: 9:30 - 22:00 Uhr. Wir freuen uns auf Ihren Besuch.', '/upload/store/13_brax-store-konstanz.jpg', 'Deutschland', '2021-04-20 09:00:00', '2021-04-20 09:00:00', '2012-08-15 11:53:32'),
(14, 'BRAX Store München-Pasing', '', 'Pasing-Arcaden', 'pasing', 'Pasinger Bahnhofsplatz 5', '81241', 'München', '48.1486622', '11.4619589', '089', '089/82000918', 'Muenchen.Pasing-Arcaden@brax.com', 'Mo. - Sa. 9:30 Uhr bis 20:00 Uhr.\nWir freuen uns auf Ihren Besuch.', '/upload/store/14_brax-store-muenchen-pasing.jpg', 'Deutschland', '2002-03-20 11:00:00', '2002-03-20 11:00:00', '2012-08-15 11:53:32'),
(15, 'BRAX Store Bocholt', '', '', 'bocholt', 'Nordstrasse 4', '46399', 'Bocholt', '51.83863', '6.61518', '', '', 'Bocholt.Store@Brax.com', 'Mo- Fr 10.00 - 18.30 Uhr und Samstag von 10.00 - 17.00 Uhr ', '/upload/store/15_brax-store-bocholt.jpg', 'Deutschland', '2019-05-20 11:00:00', '2019-05-20 11:00:00', '2012-08-15 11:53:32'),
(16, 'BRAX Store Koblenz', '', 'Löhr Center', 'koblenz', 'Hohenfelder Strasse 22', '56068', 'Koblenz', '50.35757', '7.59104', '', '', 'Koblenz.Loehr-Center@Brax.com', 'Mo - Sa. 9.30 - 20.00 Uhr', '/upload/store/16_brax-store-koblenz.jpg', 'Deutschland', '2019-05-20 11:00:00', '2019-05-20 11:00:00', '2012-08-15 11:53:32'),
(17, 'BRAX Store Erlangen', '', '', 'erlangen', 'Hauptstrasse 13', '91054', 'Erlangen', '49.59536', '11.00447', '', '09131/4000139', 'Erlangen.Store@Brax.com', 'Mo. - Fr.: 9:30 - 20:00 Uhr\nSa.: 9:30 Uhr - 19:00 Uhr ', '/upload/store/17_brax-store-erlangen.jpg', 'Deutschland', '2016-08-20 11:00:00', '2016-08-20 11:00:00', '2012-08-15 11:53:32'),
(18, 'BRAX Store Landshut', '', '', 'landshut', 'Altstadt 94-95', '84028', 'Landshut', '48.53741', '12.15195', '', '', 'Landshut.Store@Brax.com', 'Öffnungszeiten Mo-Sa 9:30 Uhr - 18:00 Uhr', '/upload/store/18_brax-store-landshut.jpg', 'Deutschland', '2016-11-20 11:00:00', '2016-11-20 11:00:00', '2012-08-15 11:53:32'),
(19, 'Coast Retail Oostende', '', 'ARW Retail', 'coastretailoostende', 'Kapellestraat 97', '8600', 'Oostende', '51.0279083', '2.8487223', '', '', '', '', '/upload/store/19_coast-retail-oostende.jpg', 'Belgien', '2013-02-20 12:00:00', '2013-02-20 12:00:00', '2012-08-15 11:53:32'),
(20, 'Coast Retail Nieuwpoort', '', 'ARW Retail', 'coastretailnieuwpoort', 'Albert I laan 114', '8620', 'Nieuwpoort', '51.1501992', '2.7210182', '', '', '', '', '/upload/store/20_coast-retail-nieuwpoort.jpg', 'Belgien', '2013-02-20 12:00:00', '2013-02-20 12:00:00', '2012-08-15 11:53:32'),
(21, 'BRAX Store Turnhout', '', 'ARW Retail', 'braxturnhout', 'Gasthuisstraat 74', '2300', 'Turnhout', '51.3224176', '4.9434798', '', '', '', '', '/upload/store/21_brax-store-turnhout.jpg', 'Belgien', '2013-02-20 12:00:00', '2013-02-20 12:00:00', '2012-08-15 11:53:32'),
(22, 'Brax Gent', '', 'ARW Retail', 'braxgent', 'Langemunt 53', '9000', 'Gent', '51.0569575', '3.723934', '', '', '', '', '/upload/store/22_brax-gent.jpg', 'Belgien', '2013-02-20 12:00:00', '2013-02-20 12:00:00', '2012-08-15 11:53:32'),
(23, 'BRAX Store Antwerpen', '', 'BRAX Retail', 'braxantwerpen', 'Schoenmarkt 4', '2000', 'Antwerpen', '51.2183058', '4.402947', '', '', '', '', '/upload/store/23_brax-store-antwerpen.jpg', 'Belgien', '2013-02-20 12:00:00', '2013-02-20 12:00:00', '2012-08-15 11:53:32'),
(24, 'BRAX Roeselare', '', 'ARW Retail', 'roeselare', 'Ooststraat 77 - 81', '8800', 'Roeselare', '50.9471149', '3.1279178', '', '', '', '', '/upload/store/24_brax-roeselare.jpg', 'Belgien', '2020-02-20 12:00:00', '2020-02-20 12:00:00', '2012-08-15 11:53:32'),
(25, 'BRAX Store Leuven', '', 'ARW Retail', 'leuven', 'Diestsestraat 125', '3000', 'Leuven', '50.8807569', '4.7074173', '', '', '', 'Geplante Eröffnung 23.02.2012', '/upload/store/25_brax-store-leuven.jpg', 'Belgien', '2020-02-20 12:00:00', '2020-02-20 12:00:00', '2012-08-15 11:53:32'),
(26, 'BRAX Koksijde', '', 'ARW Retail', 'koksijde', 'Zeelaan 283 - 285', '8670', 'Koksijde', '51.1179525', '2.6361874', '', '', '', 'Geplante Eröffnung 15.03.2012', '/upload/store/26_brax-koksijde.jpg', 'Belgien', '2020-02-20 12:00:00', '2020-02-20 12:00:00', '2012-08-15 11:53:32'),
(27, 'BRAX Store Hildesheim', '', 'Arneken-Galerie', 'hildesheim', 'Arnekenstr. 18', '31134', 'Hildesheim', '52.154356', '9.9497725', '', '05121/6972191', 'Hildesheim.Store@Brax.com', 'Öffnungszeiten: Mo. - Sa. 09.30 - 20.00 Uhr. Wir freuen uns auf Ihren Besuch.\n', '/upload/store/27_brax-store-hildesheim.jpg', 'Deutschland', '2020-02-20 12:00:00', '2020-02-20 12:00:00', '2012-08-15 11:53:32'),
(28, 'BRAX Store Bonn', '', '', 'bonn', 'Remigiusstraße 9', '53111', 'Bonn', '50.7340583', '7.1004976', '', '', 'Bonn.Store@brax.com', 'Mo. - Sa. 10:00 Uhr bis 20:00 Uhr.\nWir freuen uns auf Ihren Besuch.', '/upload/store/28_brax-store-bonn.jpg', 'Deutschland', '2027-04-20 12:00:00', '2027-04-20 12:00:00', '2012-08-15 11:53:32'),
(29, 'BRAX Store Bochum', '', 'Bochum Kortum-Karree', 'bochum', 'Huestr. 17-25', '44787', 'Bochum', '51.4796269', '7.2186696', '', '0234/68727687', 'Bochum.Store@Brax.com', '', '/upload/store/29_brax-store-bochum.jpg', 'Deutschland', '2004-06-20 12:00:00', '2004-06-20 12:00:00', '2012-08-15 11:53:32');

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `module_youtube_banner`
--

CREATE TABLE IF NOT EXISTS `module_youtube_banner` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `sort` int(11) NOT NULL,
  `status` int(11) NOT NULL DEFAULT '2',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=6 ;

--
-- Daten für Tabelle `module_youtube_banner`
--

INSERT INTO `module_youtube_banner` (`id`, `sort`, `status`) VALUES
(2, 3, 1),
(4, 2, 2),
(5, 4, 1);

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `module_youtube_banner_content`
--

CREATE TABLE IF NOT EXISTS `module_youtube_banner_content` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `banner_id` int(11) NOT NULL,
  `headline` text COLLATE utf8_unicode_ci NOT NULL,
  `url` varchar(355) COLLATE utf8_unicode_ci NOT NULL,
  `linktext` varchar(255) COLLATE utf8_unicode_ci NOT NULL DEFAULT 'jetzt entdecken',
  `image_path` varchar(355) COLLATE utf8_unicode_ci DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=6 ;

--
-- Daten für Tabelle `module_youtube_banner_content`
--

INSERT INTO `module_youtube_banner_content` (`id`, `banner_id`, `headline`, `url`, `linktext`, `image_path`) VALUES
(2, 2, 'AKTUELLE KAMPAGNEN OUTFITS IM SHOP', 'http://brax.de', 'jetzt entdecken', '/upload/youtube_app/banner/2/youtube_banner.png'),
(4, 4, 'AKTUELLE KAMPAGNEN OUTFITS IM SHOP', 'http://brax.de', 'jetzt entdecken', '/upload/youtube_app/banner/4/demo-aktion.png'),
(5, 5, 'AKTUELLE KAMPAGNEN OUTFITS IM SHOP', 'http://brax.de', 'jetzt entdecken', '/upload/youtube_app/banner/5/youtube_banner.png');

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `module_youtube_banner_status`
--

CREATE TABLE IF NOT EXISTS `module_youtube_banner_status` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(25) COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=3 ;

--
-- Daten für Tabelle `module_youtube_banner_status`
--

INSERT INTO `module_youtube_banner_status` (`id`, `name`) VALUES
(1, 'draft'),
(2, 'publish');

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `module_youtube_playlists`
--

CREATE TABLE IF NOT EXISTS `module_youtube_playlists` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `playlistId` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `youtubePlaylistId` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  `title` text COLLATE utf8_unicode_ci NOT NULL,
  `url` varchar(355) COLLATE utf8_unicode_ci NOT NULL,
  `isActive` int(11) NOT NULL DEFAULT '1',
  `isCurrent` int(11) NOT NULL DEFAULT '2',
  `sort` int(11) NOT NULL DEFAULT '1',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=22 ;

--
-- Daten für Tabelle `module_youtube_playlists`
--

INSERT INTO `module_youtube_playlists` (`id`, `playlistId`, `youtubePlaylistId`, `title`, `url`, `isActive`, `isCurrent`, `sort`) VALUES
(1, '4FC2BD6346A4E64A', '4FC2BD6346A4E64A', 'BOSS NUIT Pour Femme', 'https://gdata.youtube.com/feeds/api/playlists/4FC2BD6346A4E64A', 1, 0, 2),
(2, '177AE0A5A1461E3F', '177AE0A5A1461E3F', 'HUGO Fashion Show Spring/Summer 2013', 'https://gdata.youtube.com/feeds/api/playlists/177AE0A5A1461E3F', 1, 0, 3),
(3, '03BCCFD238074E58', '03BCCFD238074E58', 'BOSS Black Fashion Show Beijing', 'https://gdata.youtube.com/feeds/api/playlists/03BCCFD238074E58', 2, 0, 4),
(4, '3A844743BBAC77D8', '3A844743BBAC77D8', 'BOSS.BOTTLED.', 'https://gdata.youtube.com/feeds/api/playlists/3A844743BBAC77D8', 1, 0, 1),
(5, '55B70C8B4C0BFA42', '55B70C8B4C0BFA42', 'HUGO Fashion Show Fall/Winter 2012', 'https://gdata.youtube.com/feeds/api/playlists/55B70C8B4C0BFA42', 2, 0, 5),
(6, '934ED07048392B3E', '934ED07048392B3E', 'HUGO BOSS TALK', 'https://gdata.youtube.com/feeds/api/playlists/934ED07048392B3E', 2, 0, 6),
(7, '899084CDD7AF62E8', '899084CDD7AF62E8', 'BOSS Green', 'https://gdata.youtube.com/feeds/api/playlists/899084CDD7AF62E8', 2, 0, 7),
(8, 'D5512C748EBF216C', 'D5512C748EBF216C', 'BOSS Kidswear', 'https://gdata.youtube.com/feeds/api/playlists/D5512C748EBF216C', 2, 0, 8),
(9, 'C3DB65B2416FFF99', 'C3DB65B2416FFF99', 'HUGO Super Black', 'https://gdata.youtube.com/feeds/api/playlists/C3DB65B2416FFF99', 2, 0, 9),
(10, 'BE0F4AA4841693DF', 'BE0F4AA4841693DF', 'HUGO Tracks artists', 'https://gdata.youtube.com/feeds/api/playlists/BE0F4AA4841693DF', 2, 0, 10),
(11, '86B9D839CE6700D5', '86B9D839CE6700D5', 'HUGO Fashion Show Spring/Summer 2012', 'https://gdata.youtube.com/feeds/api/playlists/86B9D839CE6700D5', 2, 0, 11),
(12, '15025C2BFBD05C1E', '15025C2BFBD05C1E', 'HUGO Just Different', 'https://gdata.youtube.com/feeds/api/playlists/15025C2BFBD05C1E', 2, 0, 12),
(13, '076656B94655921F', '076656B94655921F', 'HUGO BOSS & McLAREN', 'https://gdata.youtube.com/feeds/api/playlists/076656B94655921F', 2, 0, 13),
(14, 'BA208F50A25A7104', 'BA208F50A25A7104', 'BOSS Selection', 'https://gdata.youtube.com/feeds/api/playlists/BA208F50A25A7104', 2, 0, 14),
(15, '9D2883820489AD03', '9D2883820489AD03', 'Trailer', 'https://gdata.youtube.com/feeds/api/playlists/9D2883820489AD03', 2, 0, 15),
(16, 'BED6E8B99A2D7104', 'BED6E8B99A2D7104', 'HUGO BOSS TV Commercials', 'https://gdata.youtube.com/feeds/api/playlists/BED6E8B99A2D7104', 2, 0, 16),
(17, '005C5B93D6166023', '005C5B93D6166023', 'BOSS Orange Design Contest', 'https://gdata.youtube.com/feeds/api/playlists/005C5B93D6166023', 2, 0, 17),
(18, 'BA67102E9487ADD2', 'BA67102E9487ADD2', 'HUGO BOSS Sponsoring', 'https://gdata.youtube.com/feeds/api/playlists/BA67102E9487ADD2', 2, 0, 18),
(19, 'F474A760A21891D3', 'F474A760A21891D3', 'HUGO', 'https://gdata.youtube.com/feeds/api/playlists/F474A760A21891D3', 2, 0, 19),
(20, '3CF8D56B3B235820', '3CF8D56B3B235820', 'BOSS Black', 'https://gdata.youtube.com/feeds/api/playlists/3CF8D56B3B235820', 2, 0, 21),
(21, '289C0505A9C86510', '289C0505A9C86510', 'BOSS Orange', 'https://gdata.youtube.com/feeds/api/playlists/289C0505A9C86510', 2, 0, 20);

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `module_youtube_playlists_videos`
--

CREATE TABLE IF NOT EXISTS `module_youtube_playlists_videos` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `playlistId` int(11) NOT NULL,
  `videoId` varchar(355) COLLATE utf8_unicode_ci NOT NULL,
  `thumbnail` varchar(355) COLLATE utf8_unicode_ci NOT NULL,
  `title` varchar(355) COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=566 ;

--
-- Daten für Tabelle `module_youtube_playlists_videos`
--

INSERT INTO `module_youtube_playlists_videos` (`id`, `playlistId`, `videoId`, `thumbnail`, `title`) VALUES
(14, 10, 'FLVXAV8lti8', 'http://i.ytimg.com/vi/FLVXAV8lti8/mqdefault.jpg', 'HUGO Tracks presents - Artist Nina Kraviz at the HUGO Fashion Show Spring/Summer 2013'),
(15, 10, 'u6gxGbHIaEE', 'http://i.ytimg.com/vi/u6gxGbHIaEE/mqdefault.jpg', 'HUGO Tracks Rocks Berlin with Nina Kraviz & Kasper Bjørke'),
(25, 10, 'G68e7jx0VPQ', 'http://i.ytimg.com/vi/G68e7jx0VPQ/mqdefault.jpg', 'HUGO Artists Shooting in Paris with Nina Kraviz and Kasper Bjørke'),
(55, 4, '4DZdPIkJP8w', 'http://i.ytimg.com/vi/4DZdPIkJP8w/mqdefault.jpg', 'BOSS Bottled. Sport. featuring Jenson Button'),
(59, 4, 'PsAuwKrfgos', 'http://i.ytimg.com/vi/PsAuwKrfgos/mqdefault.jpg', 'BOSS BOTTLED. TV Spot feat. Ryan Reynolds (presented by HUGO BOSS TV)'),
(125, 19, 'Rq2Vr1XKaMw', 'http://i.ytimg.com/vi/Rq2Vr1XKaMw/mqdefault.jpg', 'HUGO Man - HUGO Just Different Fragrance Commercial featuring Jared Leto'),
(126, 19, 'wm--_ExcrVo', 'http://i.ytimg.com/vi/wm--_ExcrVo/mqdefault.jpg', 'HUGO Just Different Commercial Making of with Jared Leto'),
(127, 19, 'LXb7cHxn8gU', 'http://i.ytimg.com/vi/LXb7cHxn8gU/mqdefault.jpg', 'Jared Leto for HUGO Just Different - Interview'),
(133, 20, '820bMCyOXoY', 'http://i.ytimg.com/vi/820bMCyOXoY/mqdefault.jpg', 'Button and Hamilton answer Facebook users questions (presented by HUGO BOSS TV)'),
(134, 20, 'v05uMAfUw-4', 'http://i.ytimg.com/vi/v05uMAfUw-4/mqdefault.jpg', 'Behind the scenes at the Jenson Button and Lewis Hamilton Shooting (presented by HUGO BOSS TV)'),
(137, 21, '88HATBcODWk', 'http://i.ytimg.com/vi/88HATBcODWk/mqdefault.jpg', 'Orlando Bloom For BOSS Orange The New Man Fragrance (presented by HUGO BOSS TV)'),
(138, 21, 'h9L_qgNO-LY', 'http://i.ytimg.com/vi/h9L_qgNO-LY/mqdefault.jpg', 'Sienna Miller For BOSS Orange The New Fragrance (presented by HUGO BOSS TV)'),
(151, 20, 'cdqO67Uq2Rs', 'http://i.ytimg.com/vi/cdqO67Uq2Rs/mqdefault.jpg', 'Behind The Scenes at the FC Bayern Shooting (presented by HUGO BOSS TV)'),
(382, 1, 'J8APEUXqhtg', 'http://i.ytimg.com/vi/J8APEUXqhtg/mqdefault.jpg', 'A ''perfectly put together'' moment with Gwyneth Paltrow - Wardrobe essentials'),
(383, 1, 'DQoY-UBfCRw', 'http://i.ytimg.com/vi/DQoY-UBfCRw/mqdefault.jpg', 'A ''perfectly put together'' moment with Gwyneth Paltrow - Evenings around the World'),
(384, 1, 'nlEhH1zEucM', 'http://i.ytimg.com/vi/nlEhH1zEucM/mqdefault.jpg', 'A ''perfectly put together'' moment with Elizabeth Saltzman - Little black dress'),
(385, 1, '--Lk4VfYp4I', 'http://i.ytimg.com/vi/--Lk4VfYp4I/mqdefault.jpg', 'A ''perfectly put together'' moment with Eyan Allen - Day or night'),
(386, 1, '_sSX5OTSiSM', 'http://i.ytimg.com/vi/_sSX5OTSiSM/mqdefault.jpg', 'A ''perfectly put together'' moment with Gwyneth Paltrow - Thoughts on fragrance'),
(387, 1, 'h6A4yFYINko', 'http://i.ytimg.com/vi/h6A4yFYINko/mqdefault.jpg', 'A ''perfectly put together'' moment with Elizabeth Saltzman - Styling tips'),
(388, 1, 'jUq6L3CF2Ls', 'http://i.ytimg.com/vi/jUq6L3CF2Ls/mqdefault.jpg', 'A ''perfectly put together'' moment with Gwyneth Paltrow -Details make the outfit'),
(389, 1, 'tN50smPo9Ms', 'http://i.ytimg.com/vi/tN50smPo9Ms/mqdefault.jpg', 'A ''perfectly put together'' moment with Eyan Allen - Reinventing the wheel'),
(390, 1, 'LjfqJhilfj0', 'http://i.ytimg.com/vi/LjfqJhilfj0/mqdefault.jpg', 'A ''perfectly put together'' moment with Eyan Allen - Fashion and Fragrance'),
(391, 1, 'FBO3KlNSdfI', 'http://i.ytimg.com/vi/FBO3KlNSdfI/mqdefault.jpg', 'A ''perfectly put together'' moment with Gwyneth Paltrow - The perfect playlist'),
(392, 2, 'L8q24XHd3iU', 'http://i.ytimg.com/vi/L8q24XHd3iU/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2013 Berlin'),
(393, 2, 'cZJZQ1UkzTg', 'http://i.ytimg.com/vi/cZJZQ1UkzTg/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2013 - Interview with Poppy Delevigne'),
(394, 2, 'xW8J2RQ_46o', 'http://i.ytimg.com/vi/xW8J2RQ_46o/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2013 - Interview with China Chow'),
(395, 2, 'R43_1PEV9RA', 'http://i.ytimg.com/vi/R43_1PEV9RA/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2013 - Interview with Model Kasia Struss'),
(396, 2, 'IafwejweHAw', 'http://i.ytimg.com/vi/IafwejweHAw/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2013 - Interview with Model Bastiaan van Gaalen'),
(397, 2, 'HAB4SEgd84U', 'http://i.ytimg.com/vi/HAB4SEgd84U/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2013 - Interview with Model Adrien Sahores'),
(398, 2, 'MYExp2gTFj4', 'http://i.ytimg.com/vi/MYExp2gTFj4/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2013 - Interview with Model Franziska Müller'),
(399, 2, 'IyAH_aeARBs', 'http://i.ytimg.com/vi/IyAH_aeARBs/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2013 - Interview with Anthony Delon'),
(400, 2, 'DNxrV9dnnmU', 'http://i.ytimg.com/vi/DNxrV9dnnmU/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2013 - Interview with David Coulthard'),
(401, 2, 'L8j_O-nJTec', 'http://i.ytimg.com/vi/L8j_O-nJTec/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2013 - Interview with Kate Bosworth'),
(402, 2, 'apUSglw2Luo', 'http://i.ytimg.com/vi/apUSglw2Luo/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2013 - Interview with Nina Kraviz'),
(403, 2, 'e2TTHeQvcC4', 'http://i.ytimg.com/vi/e2TTHeQvcC4/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2013 - Interview with Matthew Goode'),
(404, 3, 'N7LWUrf28K4', 'http://i.ytimg.com/vi/N7LWUrf28K4/mqdefault.jpg', 'BOSS Black Fashion Show Beijing Fall/Winter 2012 3D HD'),
(405, 3, 'HjswjVW_71w', 'http://i.ytimg.com/vi/HjswjVW_71w/mqdefault.jpg', 'BOSS Black Fashion Show Beijing Fall/Winter 2012 Fitting Models'),
(406, 3, 'HGXLqHBe_Xg', 'http://i.ytimg.com/vi/HGXLqHBe_Xg/mqdefault.jpg', 'BOSS Black Fashion Show Fall/Winter 2012 Behind the Scenes 3D'),
(407, 3, 'cMI3BrrCFso', 'http://i.ytimg.com/vi/cMI3BrrCFso/mqdefault.jpg', 'BOSS Black Fashion Show Beijing Fall/Winter 2012 - Interview Chow Yun-Fat 3D'),
(408, 3, '258ZXOVPXhU', 'http://i.ytimg.com/vi/258ZXOVPXhU/mqdefault.jpg', 'BOSS Black Fashion Show Beijing Fall/Winter 2012 - Interview Tilda Swinton 3D'),
(409, 3, '7DM5GGj65Uw', 'http://i.ytimg.com/vi/7DM5GGj65Uw/mqdefault.jpg', 'BOSS Black Fashion Show Beijing Fall/Winter 2012 - Interview Ryan Philippe 3D'),
(410, 3, 'Yy_GdY9Y_j4', 'http://i.ytimg.com/vi/Yy_GdY9Y_j4/mqdefault.jpg', 'BOSS Black Fashion Show Beijing Fall/Winter 2012 - Interview Georgia May Jagger 3D'),
(411, 3, 'RDm2wK-7dDU', 'http://i.ytimg.com/vi/RDm2wK-7dDU/mqdefault.jpg', 'BOSS Black Fashion Show Beijing Fall/Winter 2012 - Interview Matthew Goode 3D'),
(412, 3, 'WqDTor1VwpE', 'http://i.ytimg.com/vi/WqDTor1VwpE/mqdefault.jpg', 'The Way to Beijing Part 13 "Flashback" 3D'),
(413, 3, 'RhtD2ABNvYE', 'http://i.ytimg.com/vi/RhtD2ABNvYE/mqdefault.jpg', 'The Way to Beijing Part 12 "A Beijing Minute" 3D'),
(414, 3, '6c2y4Ip7A9k', 'http://i.ytimg.com/vi/6c2y4Ip7A9k/mqdefault.jpg', 'The Way to Beijing Part 11 "All eyes on Beijing" 3D'),
(415, 3, 'hIU9cygEjbM', 'http://i.ytimg.com/vi/hIU9cygEjbM/mqdefault.jpg', 'The Way to Beijing Part 10 "Fitting Fun" 3D'),
(416, 3, 'jxCfvLvvS4o', 'http://i.ytimg.com/vi/jxCfvLvvS4o/mqdefault.jpg', 'The Way to Beijing Part 9 "Man in Black" 3D'),
(417, 3, 'aK9shQP_CDo', 'http://i.ytimg.com/vi/aK9shQP_CDo/mqdefault.jpg', 'The Way to Beijing Part 8 "Model Talk" 3D'),
(418, 3, 'BVUSQSkyHz8', 'http://i.ytimg.com/vi/BVUSQSkyHz8/mqdefault.jpg', 'The Way to Beijing Part 7 "Hi, Mister Charming" 3D'),
(419, 3, 'd_Ap_v-hNkY', 'http://i.ytimg.com/vi/d_Ap_v-hNkY/mqdefault.jpg', 'The Way to Beijing Part 6 "Personal Delivery" 3D'),
(420, 3, 'miHyR4Q14S0', 'http://i.ytimg.com/vi/miHyR4Q14S0/mqdefault.jpg', 'The Way to Beijing Part 5 "Showroom Heaven" 3D'),
(421, 3, 'bVQqX5a88Zk', 'http://i.ytimg.com/vi/bVQqX5a88Zk/mqdefault.jpg', 'The Way to Beijing Part 4 "Designer Secrets" 3D'),
(422, 3, 'rOiyLjDrMXc', 'http://i.ytimg.com/vi/rOiyLjDrMXc/mqdefault.jpg', 'The Way to Beijing Part 3 "A New Dimension" 3D'),
(423, 3, '1o0z0kyxIrY', 'http://i.ytimg.com/vi/1o0z0kyxIrY/mqdefault.jpg', 'The Way to Beijing Part 2 "Why Beijing?" 3D'),
(424, 3, 'NP4BvJ6BZ_c', 'http://i.ytimg.com/vi/NP4BvJ6BZ_c/mqdefault.jpg', 'The Way to Beijing Part 1 "News on the Way" 3D'),
(425, 3, 'vWfsSD_xD-U', 'http://i.ytimg.com/vi/vWfsSD_xD-U/mqdefault.jpg', 'BOSS Black Fashion Show Beijing 2012 Teaser'),
(426, 4, 'f_WEdni7XWg', 'http://i.ytimg.com/vi/f_WEdni7XWg/mqdefault.jpg', 'HUGO BOSS Challenges: The Keel Walk with Alex Thompson'),
(427, 4, 'pr5MRRI-3_g', 'http://i.ytimg.com/vi/pr5MRRI-3_g/mqdefault.jpg', 'HUGO BOSS Challenges: Mario Gomez vs Martin Kaymer'),
(428, 4, 'pVnjiVKH3wg', 'http://i.ytimg.com/vi/pVnjiVKH3wg/mqdefault.jpg', 'BOSS Fragrances Campaign "Success Beyond the Game" featuring Alessandro Matri'),
(429, 4, 'Ud0VRHtbNXI', 'http://i.ytimg.com/vi/Ud0VRHtbNXI/mqdefault.jpg', 'BOSS Fragrances Campaign "Success Beyond the Game" featuring Gaël Clichy'),
(430, 4, '4BI8kiOr0CI', 'http://i.ytimg.com/vi/4BI8kiOr0CI/mqdefault.jpg', 'BOSS Fragrances Campaign "Success Beyond the Game" featuring Xabi Alonso'),
(431, 4, 'e6V5Y5EfYno', 'http://i.ytimg.com/vi/e6V5Y5EfYno/mqdefault.jpg', 'BOSS Fragrances Kampagne "Erfolg über das Spiel hinaus" mit Mario Gomez'),
(432, 4, '3Gd4MHOvFc8', 'http://i.ytimg.com/vi/3Gd4MHOvFc8/mqdefault.jpg', 'BOSS Fragrances Campaign "Success Beyond the Game" featuring Jakub B?aszczykowski'),
(433, 4, 'pRMQubW5dp8', 'http://i.ytimg.com/vi/pRMQubW5dp8/mqdefault.jpg', 'BOSS Bottled. Sport. Campaign Shooting featuring Jenson Button'),
(434, 4, 'TaC1qujHVcQ', 'http://i.ytimg.com/vi/TaC1qujHVcQ/mqdefault.jpg', 'BOSS Bottled. Sport. featuring Jenson Button - Making-of TV Commercial'),
(435, 4, 'DRcZCO9C95w', 'http://i.ytimg.com/vi/DRcZCO9C95w/mqdefault.jpg', 'BOSS Bottled. Sport. featuring Jenson Button - Mental Preparation'),
(436, 5, '9wdfMlG91uU', 'http://i.ytimg.com/vi/9wdfMlG91uU/mqdefault.jpg', 'HUGO Fashion Show Fall/Winter 2012'),
(437, 5, '8Ok5OsqA7L4', 'http://i.ytimg.com/vi/8Ok5OsqA7L4/mqdefault.jpg', 'HUGO Fashion Show Fall/Winter 2012 - Red Carpet Interview Jared Leto'),
(438, 5, 'iEYWAVto9Fk', 'http://i.ytimg.com/vi/iEYWAVto9Fk/mqdefault.jpg', 'HUGO Fashion Show Fall/Winter 2012 - Red Carpet Interview Jenson Button'),
(439, 5, 'MF_-VX6H7MA', 'http://i.ytimg.com/vi/MF_-VX6H7MA/mqdefault.jpg', 'HUGO Fashion Show Fall/Winter 2012 - Red Carpet Interview Sky Ferreira'),
(440, 5, 'HzKbJJHIWHo', 'http://i.ytimg.com/vi/HzKbJJHIWHo/mqdefault.jpg', 'HUGO Fashion Show Fall/Winter 2012 - Red Carpet Interview Franziska Knuppe'),
(441, 5, 'wDsgx-FSrr4', 'http://i.ytimg.com/vi/wDsgx-FSrr4/mqdefault.jpg', 'HUGO Fashion Show Fall/Winter 2012 - Red Carpet Interview Ursula Karven'),
(442, 5, 'xqQhW35lzDU', 'http://i.ytimg.com/vi/xqQhW35lzDU/mqdefault.jpg', 'HUGO Fashion Show Fall/Winter 2012 - Cosma Shiva Hagen Red Carpet Interview'),
(443, 5, 'zheknIDphYQ', 'http://i.ytimg.com/vi/zheknIDphYQ/mqdefault.jpg', 'HUGO Fashion Show Fall/Winter 2012 - Julianne Moore Red Carpet Interview'),
(444, 5, 'au9ew7i08aQ', 'http://i.ytimg.com/vi/au9ew7i08aQ/mqdefault.jpg', 'HUGO Fashion Show Fall/Winter 2012 - Matthias Schweighöfer Red Carpet Interview'),
(445, 5, 'vSxbhBY_Jzs', 'http://i.ytimg.com/vi/vSxbhBY_Jzs/mqdefault.jpg', 'HUGO Fashion Show Fall/Winter 2012 - Nora von Waldstätten Red Carpet Interview'),
(446, 5, 'j5wXLjW9nYI', 'http://i.ytimg.com/vi/j5wXLjW9nYI/mqdefault.jpg', 'HUGO Fashion Show Fall/Winter 2012 - Red Carpet Interview Palina Rojinski'),
(447, 5, 'r6qxBzJPhbw', 'http://i.ytimg.com/vi/r6qxBzJPhbw/mqdefault.jpg', 'HUGO Fashion Show Fall/Winter 2012 - Red Carpet Interview Pixi Geldof'),
(448, 5, 'plkusOw1aD4', 'http://i.ytimg.com/vi/plkusOw1aD4/mqdefault.jpg', 'HUGO Fashion Show Fall/Winter 2012 - Model Interview Karlie Kloss'),
(449, 5, 'fIxoMtzY9es', 'http://i.ytimg.com/vi/fIxoMtzY9es/mqdefault.jpg', 'HUGO Fashion Show Fall/Winter 2012 - Model Interview William Eustace'),
(450, 5, '2czOlU0sDoQ', 'http://i.ytimg.com/vi/2czOlU0sDoQ/mqdefault.jpg', 'HUGO Fashion Show Fall/Winter 2012 - Interview with Choreographer Carlo Castro'),
(451, 5, 'YNBf3s9xSuQ', 'http://i.ytimg.com/vi/YNBf3s9xSuQ/mqdefault.jpg', 'HUGO Fashion Show Fall/Winter 2012 - Model Interview Mathias Lauridsen'),
(452, 5, 'GeP07WdQSF8', 'http://i.ytimg.com/vi/GeP07WdQSF8/mqdefault.jpg', 'HUGO Fashion Show Fall/Winter 2012 - Model Interview Josefine Nielsen'),
(453, 5, 'G4ZB-MBFKRs', 'http://i.ytimg.com/vi/G4ZB-MBFKRs/mqdefault.jpg', 'HUGO Fashion Show Fall/Winter 2012 - Model Interview Oliver Greb'),
(454, 5, '0hvnv-k2fUU', 'http://i.ytimg.com/vi/0hvnv-k2fUU/mqdefault.jpg', 'HUGO presents Berlins Hot Locations on Foursquare'),
(455, 5, 'RJDTRZArLWQ', 'http://i.ytimg.com/vi/RJDTRZArLWQ/mqdefault.jpg', 'HUGO Fashion Show Fall/Winter 2012 Teaser'),
(456, 5, 'IJ8diJs3F6A', 'http://i.ytimg.com/vi/IJ8diJs3F6A/mqdefault.jpg', 'HUGO BOSS Fashion Show Fall/Winter 2012 - Showtime with Bryanboy'),
(457, 5, 'Fz81BKDlir4', 'http://i.ytimg.com/vi/Fz81BKDlir4/mqdefault.jpg', 'HUGO Fashion Show Fall/Winter 2012 - Bart de Backer Livestream Invitation'),
(458, 5, 'LjINm-vzBCk', 'http://i.ytimg.com/vi/LjINm-vzBCk/mqdefault.jpg', 'Josefine Nielsen at the HUGO Fashion Show'),
(459, 6, 'Ewh5f738N38', 'http://i.ytimg.com/vi/Ewh5f738N38/mqdefault.jpg', 'HUGO BOSS TALK EDITION TWO - Melissa Drier and Bryanboy'),
(460, 6, 'IsI_YjMlV9Q', 'http://i.ytimg.com/vi/IsI_YjMlV9Q/mqdefault.jpg', 'HUGO BOSS TALK EDITION TWO - Making of with Melissa Drier and Bryanboy'),
(461, 6, '818_zqbhe9Q', 'http://i.ytimg.com/vi/818_zqbhe9Q/mqdefault.jpg', 'HUGO BOSS TALK EDITION TWO - Melissa Drier and Bryanboy Trailer'),
(462, 6, '0ycvFDK_-ys', 'http://i.ytimg.com/vi/0ycvFDK_-ys/mqdefault.jpg', 'HUGO BOSS TALK Edition One'),
(463, 6, 'UtVcTiwZCIU', 'http://i.ytimg.com/vi/UtVcTiwZCIU/mqdefault.jpg', 'HUGO BOSS TALK Edition One Teaser'),
(464, 6, 's2gR3VP6zSU', 'http://i.ytimg.com/vi/s2gR3VP6zSU/mqdefault.jpg', 'HUGO BOSS TALK Edition One Making-of'),
(465, 7, 'hLPEgk3XAio', 'http://i.ytimg.com/vi/hLPEgk3XAio/mqdefault.jpg', 'Golf Lesson with Martin Kaymer: How to play Chip and Run'),
(466, 7, 'yMQzI-zkM4M', 'http://i.ytimg.com/vi/yMQzI-zkM4M/mqdefault.jpg', 'Golf Lesson with Martin Kaymer: How to hit a Fade'),
(467, 7, 'BXj0OabDoeA', 'http://i.ytimg.com/vi/BXj0OabDoeA/mqdefault.jpg', 'Golf Lesson with Martin Kaymer: How to hit a Lob Shot'),
(468, 7, 'LEgwqrnUdUk', 'http://i.ytimg.com/vi/LEgwqrnUdUk/mqdefault.jpg', 'Golf Lesson with Nick Watney: How to hit a basic Bunker Shot'),
(469, 7, '8XaiopZTTyE', 'http://i.ytimg.com/vi/8XaiopZTTyE/mqdefault.jpg', 'Golf Lesson with Nick Watney: How to put like Nick Watney'),
(470, 7, 'jQz4DvmK584', 'http://i.ytimg.com/vi/jQz4DvmK584/mqdefault.jpg', 'Golf Lesson with Nick Watney: Teeing'),
(471, 7, 'qHgN5DjvJwY', 'http://i.ytimg.com/vi/qHgN5DjvJwY/mqdefault.jpg', 'HUGO BOSS - Crazy Golf Tricks by Martin Kaymer'),
(472, 7, '8lqP50sjcy0', 'http://i.ytimg.com/vi/8lqP50sjcy0/mqdefault.jpg', 'BOSS Green Spring 2012 Campaign Shooting with Martin Kaymer'),
(473, 8, 'VO3N2LT0mj8', 'http://i.ytimg.com/vi/VO3N2LT0mj8/mqdefault.jpg', 'BOSS Kidswear - Behind the Scenes Shooting'),
(474, 8, 'fpGGn3y57kA', 'http://i.ytimg.com/vi/fpGGn3y57kA/mqdefault.jpg', 'BOSS Kidswear - Christian Anwander Shooting Interview'),
(475, 9, 'L5k1T_Lrtjk', 'http://i.ytimg.com/vi/L5k1T_Lrtjk/mqdefault.jpg', 'HUGO Super Black featuring James Rousseau'),
(476, 9, 'j9SFCXdh7pQ', 'http://i.ytimg.com/vi/j9SFCXdh7pQ/mqdefault.jpg', 'The new HUGO Super Black suit - Video Making-of'),
(477, 9, 'HNJ20jEbEts', 'http://i.ytimg.com/vi/HNJ20jEbEts/mqdefault.jpg', 'Behind the scenes with James Rousseau at a mysterious HUGO shoot'),
(478, 9, 'Oqh8w6fwdkk', 'http://i.ytimg.com/vi/Oqh8w6fwdkk/mqdefault.jpg', 'HUGOBOSS Super Black Shop Window Installation'),
(479, 10, 'hrzptQJIoJM', 'http://i.ytimg.com/vi/hrzptQJIoJM/mqdefault.jpg', 'HUGO Tracks featuring Nina Kraviz - @ the New Order exhibition in Berlin'),
(480, 10, 'WfPvEzNeAlw', 'http://i.ytimg.com/vi/WfPvEzNeAlw/mqdefault.jpg', 'HUGO Artist Kasper Bjørke visiting Katz Orange restaurant in Berlin'),
(481, 10, 'jVie2FZighE', 'http://i.ytimg.com/vi/jVie2FZighE/mqdefault.jpg', 'Nina Kraviz for HUGO Tracks Introduction'),
(482, 10, '7J5YLGNhrkY', 'http://i.ytimg.com/vi/7J5YLGNhrkY/mqdefault.jpg', 'Kasper Bjørke for HUGO Tracks Introduction'),
(483, 10, 'CHecpYC6w1c', 'http://i.ytimg.com/vi/CHecpYC6w1c/mqdefault.jpg', 'Kasper Bjørke "Deep is the Breath" (with Jacob Bellens & Emma Acs) Official Music Video'),
(484, 10, 'tlhXPE1eARg', 'http://i.ytimg.com/vi/tlhXPE1eARg/mqdefault.jpg', 'HUGO Tracks featuring Kasper Bjørke - Visiting "The Echo Vamper"'),
(485, 11, '4NivaG5L95I', 'http://i.ytimg.com/vi/4NivaG5L95I/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2012'),
(486, 11, 'umwUz-JsVWc', 'http://i.ytimg.com/vi/umwUz-JsVWc/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2012 - Behind the Scenes'),
(487, 11, 'SEeisDduf-0', 'http://i.ytimg.com/vi/SEeisDduf-0/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2012 - Interview Hilary Swank'),
(488, 11, 'ycZ9b2w6eY8', 'http://i.ytimg.com/vi/ycZ9b2w6eY8/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2012 - Interview Ryan Kwanten'),
(489, 11, 'LZXfNDzj6H8', 'http://i.ytimg.com/vi/LZXfNDzj6H8/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2012 - Interview Max Irons'),
(490, 11, 'tnndSWmECIs', 'http://i.ytimg.com/vi/tnndSWmECIs/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2012 - Interview Eyan Allen'),
(491, 11, 'DDRBIYeBgjE', 'http://i.ytimg.com/vi/DDRBIYeBgjE/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2012 - Interview David Coulthard'),
(492, 11, 'kurxyPXcdfQ', 'http://i.ytimg.com/vi/kurxyPXcdfQ/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2012 - Interview Colton Haynes'),
(493, 11, '2B4E4CLcCRo', 'http://i.ytimg.com/vi/2B4E4CLcCRo/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2012 - Interview Martin Kaymer'),
(494, 11, '61beAfU2iEU', 'http://i.ytimg.com/vi/61beAfU2iEU/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2012 - Interview Georgia and Elizabeth Jagger'),
(495, 11, 'XYLqT83KiH4', 'http://i.ytimg.com/vi/XYLqT83KiH4/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2012 - Interview Jourdan Dunn'),
(496, 11, 'spA-rYewyQ8', 'http://i.ytimg.com/vi/spA-rYewyQ8/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2012 - Interview Pep Gay'),
(497, 11, 'RJ8HVImOvI4', 'http://i.ytimg.com/vi/RJ8HVImOvI4/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2012 - Interview Franziska Knuppe'),
(498, 11, '2MyufC_6Kjk', 'http://i.ytimg.com/vi/2MyufC_6Kjk/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2012 - Interview Patrick Nuo'),
(499, 11, '3STTD_bvbpE', 'http://i.ytimg.com/vi/3STTD_bvbpE/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2012 - Interview Little Dragon'),
(500, 11, 'dgQoMrht30I', 'http://i.ytimg.com/vi/dgQoMrht30I/mqdefault.jpg', 'HUGO Fashion Show Spring/Summer 2012 - Interview Claus-Dietrich Lahrs'),
(501, 13, 'e1jt1_YRy8o', 'http://i.ytimg.com/vi/e1jt1_YRy8o/mqdefault.jpg', 'HUGO BOSS & McLaren - 30 years Anniversary 3D Live Projection'),
(502, 13, 'Kf6tPH4Wu54', 'http://i.ytimg.com/vi/Kf6tPH4Wu54/mqdefault.jpg', 'HUGO BOSS & McLaren - 30 years Anniversary 3D Animation'),
(503, 13, 'x5sinojw6R8', 'http://i.ytimg.com/vi/x5sinojw6R8/mqdefault.jpg', 'HUGO BOSS TV - Celebrating 30-year partnership with McLaren'),
(504, 13, '_kwuP40nEWg', 'http://i.ytimg.com/vi/_kwuP40nEWg/mqdefault.jpg', 'McLaren Special: Photo shoot (presented by HUGO BOSS TV)'),
(505, 13, 'Pxl8UiCvRs4', 'http://i.ytimg.com/vi/Pxl8UiCvRs4/mqdefault.jpg', 'McLaren Special: Dress Me For The Finale (presented by HUGO BOSS TV)'),
(506, 14, 'K_HLdH7qpBM', 'http://i.ytimg.com/vi/K_HLdH7qpBM/mqdefault.jpg', 'BOSS Selection Campaign Spring/Summer 2012'),
(507, 15, 'wxyNn52cCs0', 'http://i.ytimg.com/vi/wxyNn52cCs0/mqdefault.jpg', 'Wir sind die Nacht Trailer (presented by HUGO BOSS TV)'),
(508, 16, 'iINPaNpDHA8', 'http://i.ytimg.com/vi/iINPaNpDHA8/mqdefault.jpg', 'BOSS BOTTLED. NIGHT. TV Spot (presented by HUGO BOSS TV)'),
(509, 16, '3Tv-FFMiLgc', 'http://i.ytimg.com/vi/3Tv-FFMiLgc/mqdefault.jpg', 'BOSS BOTTLED für die Männer des Jahres, GQ Magazin 2010 (presented by HUGO BOSS TV)'),
(510, 17, 'iVVNgDb84yk', 'http://i.ytimg.com/vi/iVVNgDb84yk/mqdefault.jpg', 'BOSS Orange Design Contest Winner at HUGO BOSS Headquarters (presented by HUGO BOSS TV)'),
(511, 17, 'RFYxEkNmBJI', 'http://i.ytimg.com/vi/RFYxEkNmBJI/mqdefault.jpg', 'BOSS Orange Design Contest - The Winner (presented by HUGO BOSS TV)'),
(512, 18, '7rRt41vBcwc', 'http://i.ytimg.com/vi/7rRt41vBcwc/mqdefault.jpg', 'Mario Gomez vs Martin Kaymer'),
(513, 18, 'CL1oBFV81ME', 'http://i.ytimg.com/vi/CL1oBFV81ME/mqdefault.jpg', 'Mario Gomez vs Martin Kaymer - Making of'),
(514, 18, '12HSe-rPrmw', 'http://i.ytimg.com/vi/12HSe-rPrmw/mqdefault.jpg', 'Mario Gomez vs Martin Kaymer - Coming Soon'),
(515, 18, 'z7BI72Ub2jQ', 'http://i.ytimg.com/vi/z7BI72Ub2jQ/mqdefault.jpg', 'HUGO BOSS Berlinale Film Festival 2012 Party Event'),
(516, 18, '0hkoE4VlwsU', 'http://i.ytimg.com/vi/0hkoE4VlwsU/mqdefault.jpg', 'HUGO BOSS Berlinale Film Festival 2012 Trailer'),
(517, 18, 'W22tPnP6tU0', 'http://i.ytimg.com/vi/W22tPnP6tU0/mqdefault.jpg', 'HUGO BOSS Samsung Galaxy Ace'),
(518, 18, 'CrR2Gf_MdKI', 'http://i.ytimg.com/vi/CrR2Gf_MdKI/mqdefault.jpg', 'HUGO BOSS presents Boss TV Series debut on STARZ'),
(519, 18, 'fLwfZcPdrWc', 'http://i.ytimg.com/vi/fLwfZcPdrWc/mqdefault.jpg', 'HUGO BOSS - La Biennale Di Venezia 2011'),
(520, 18, 'Q1rdCc8Y9RI', 'http://i.ytimg.com/vi/Q1rdCc8Y9RI/mqdefault.jpg', 'HUGO BOSS Boat Launch In London (presented by HUGO BOSS TV)'),
(521, 18, 'byrEcehrZpk', 'http://i.ytimg.com/vi/byrEcehrZpk/mqdefault.jpg', 'La Biennale di Venezia 2009 (presented by HUGO BOSS TV)'),
(522, 19, 'RSNCr0zkJLE', 'http://i.ytimg.com/vi/RSNCr0zkJLE/mqdefault.jpg', 'HUGO BOSS TV presents the HUGO Sequined Jacket'),
(523, 19, 'JwshGjc1RZI', 'http://i.ytimg.com/vi/JwshGjc1RZI/mqdefault.jpg', 'Kekilli & Fitz Interview, HUGO Fashion Show Fall/Winter 2011 (presented by HUGO BOSS TV)'),
(524, 19, 'omi70JpsXW4', 'http://i.ytimg.com/vi/omi70JpsXW4/mqdefault.jpg', 'Lewis Hamilton Interview, HUGO Fashion Show Fall/Winter 2011 (presented by HUGO BOSS TV)'),
(525, 19, '9sUYYxIDDOM', 'http://i.ytimg.com/vi/9sUYYxIDDOM/mqdefault.jpg', 'Sara Nuru Interview, HUGO Fashion Show Fall/Winter 2011 (presented by HUGO BOSS TV)'),
(526, 19, 'Jb_DCdtvclo', 'http://i.ytimg.com/vi/Jb_DCdtvclo/mqdefault.jpg', 'Gercke, Padberg & Knuppe Interview, HUGO Fashion Show Fall/Winter 2011 (presented by HUGO BOSS TV)'),
(527, 19, 'SQVzbuoixFM', 'http://i.ytimg.com/vi/SQVzbuoixFM/mqdefault.jpg', 'Eyan Allen Interview, HUGO Fashion Show Fall/Winter 2011 (presented by HUGO BOSS TV)'),
(528, 19, 'QrvnlKrdMv0', 'http://i.ytimg.com/vi/QrvnlKrdMv0/mqdefault.jpg', 'Colton Haynes Interview, HUGO Fashion Show Fall/Winter 2011 (presented by HUGO BOSS TV)'),
(529, 19, 'apOqSscJGnA', 'http://i.ytimg.com/vi/apOqSscJGnA/mqdefault.jpg', 'Franziska van Almsick Interview, HUGO Fashion Show Fall/Winter 2011 (presented by HUGO BOSS TV)'),
(530, 19, 'lisJ4bJAMsI', 'http://i.ytimg.com/vi/lisJ4bJAMsI/mqdefault.jpg', 'Claus-Dietrich Lahrs Interview, HUGO Fashion Show Fall/Winter 2011 (presented by HUGO BOSS TV)'),
(531, 19, 'hRLTUMt96yk', 'http://i.ytimg.com/vi/hRLTUMt96yk/mqdefault.jpg', 'Chloë Sevigny Interview, HUGO Fashion Show Fall/Winter 2011 (presented by HUGO BOSS TV)'),
(532, 19, 'dmexOXk5_UM', 'http://i.ytimg.com/vi/dmexOXk5_UM/mqdefault.jpg', 'Tilda Swinton Interview, HUGO Fashion Show Fall/Winter 2011 (presented by HUGO BOSS TV)'),
(533, 19, 'V0p1gFf4rMA', 'http://i.ytimg.com/vi/V0p1gFf4rMA/mqdefault.jpg', 'Hayden Christensen Interview, HUGO Fashion Show Fall/Winter 2011 (presented by HUGO BOSS TV)'),
(534, 19, 'wSA8AKwdpPg', 'http://i.ytimg.com/vi/wSA8AKwdpPg/mqdefault.jpg', 'Matthias Schweighöfer Interview, HUGO Fashion Show Fall/Winter 2011 (presented by HUGO BOSS TV)'),
(535, 19, 'QTCIH4QVXgs', 'http://i.ytimg.com/vi/QTCIH4QVXgs/mqdefault.jpg', 'Backstage Report HUGO Fashion Show (presented by HUGO BOSS TV)'),
(536, 19, 'wDCzYrBrn88', 'http://i.ytimg.com/vi/wDCzYrBrn88/mqdefault.jpg', 'HUGO Tracks: One Day With Motor City Drum Ensemble (presented by HUGO BOSS TV)'),
(537, 19, 'GVNtuTp8gWk', 'http://i.ytimg.com/vi/GVNtuTp8gWk/mqdefault.jpg', 'HUGO Tracks Meets GOMMA Super Show (presented by HUGO BOSS TV)'),
(538, 19, 'v3IP5iSeo1Y', 'http://i.ytimg.com/vi/v3IP5iSeo1Y/mqdefault.jpg', 'HUGO Fashion Show Fall/Winter 2011 (presented by HUGO BOSS TV)'),
(539, 20, 'bxmHfpZtIGg', 'http://i.ytimg.com/vi/bxmHfpZtIGg/mqdefault.jpg', 'BOSS Black Fall/Winter 2012 Campaign Shoot behind the scenes 3D'),
(540, 20, '9WR_K6gT_Bo', 'http://i.ytimg.com/vi/9WR_K6gT_Bo/mqdefault.jpg', 'Elle Wardrobe presents BOSS Black'),
(541, 20, 'f5_Ax-yUjCM', 'http://i.ytimg.com/vi/f5_Ax-yUjCM/mqdefault.jpg', 'Neiman Marcus Event with BOSS Black and Men''s Health - Interview with Tom Glavine'),
(542, 20, 'bhDxySRjQoY', 'http://i.ytimg.com/vi/bhDxySRjQoY/mqdefault.jpg', 'Neiman Marcus Event with BOSS Black and Men''s Health - Interview with Desmond Howard'),
(543, 20, 'UfdFSWM_o44', 'http://i.ytimg.com/vi/UfdFSWM_o44/mqdefault.jpg', 'Neiman Marcus Event with BOSS Black and Men''s Health - Interview with Miles Austin'),
(544, 20, 'GxSFwUgtHCc', 'http://i.ytimg.com/vi/GxSFwUgtHCc/mqdefault.jpg', 'Neiman Marcus Event with BOSS Black and Men''s Health - Interview with Matt Birk'),
(545, 20, 'ZoQqnxKypRs', 'http://i.ytimg.com/vi/ZoQqnxKypRs/mqdefault.jpg', 'Der HUGO BOSS Stilberater präsentiert von GQ - Hemd und Krawatten Guide'),
(546, 20, 'OEmWcUyv0Ac', 'http://i.ytimg.com/vi/OEmWcUyv0Ac/mqdefault.jpg', 'The HUGO BOSS Style Consultant presented by GQ - Shirt and Tie Guide'),
(547, 20, 'G704WKEgZdI', 'http://i.ytimg.com/vi/G704WKEgZdI/mqdefault.jpg', 'Der HUGO BOSS Stilberater präsentiert von GQ - Der richtige Anzug'),
(548, 20, 'OU5IFkRTB-M', 'http://i.ytimg.com/vi/OU5IFkRTB-M/mqdefault.jpg', 'The HUGO BOSS Style Consultant presented by GQ - The right Suit'),
(549, 20, '_NhpRixG1EI', 'http://i.ytimg.com/vi/_NhpRixG1EI/mqdefault.jpg', 'BOSS Black Men Campaign Spring/Summer 2012 Shooting'),
(550, 20, 'Dh9Z0e6P17I', 'http://i.ytimg.com/vi/Dh9Z0e6P17I/mqdefault.jpg', 'BOSS Black Women Campaign Spring/Summer 2012 Shooting'),
(551, 20, 'c8A87FFvXxI', 'http://i.ytimg.com/vi/c8A87FFvXxI/mqdefault.jpg', 'HUGO BOSS TV presents Fall Looks from the BOSS Black Collection'),
(552, 20, 'rPXe5nrfahk', 'http://i.ytimg.com/vi/rPXe5nrfahk/mqdefault.jpg', 'HUGO BOSS TV - BOSS Black Herbst/Winter Kollektion Looks'),
(553, 20, 'rteKxx3vLBs', 'http://i.ytimg.com/vi/rteKxx3vLBs/mqdefault.jpg', 'Lewis Hamilton Fotoshooting for BOSS Black (presented by HUGO BOSS TV)'),
(554, 20, 'RJDQgPJWo-E', 'http://i.ytimg.com/vi/RJDQgPJWo-E/mqdefault.jpg', 'Neiman Marcus Event with BOSS Black and Men''s Health - Interview with Reggie Bush'),
(555, 21, 'hgnJgnK18PA', 'http://i.ytimg.com/vi/hgnJgnK18PA/mqdefault.jpg', 'BOSS Orange Spring/Summer 2012 - Styleguide Modern Casual'),
(556, 21, '2j9CMRTBfvE', 'http://i.ytimg.com/vi/2j9CMRTBfvE/mqdefault.jpg', 'BOSS Orange Styleguide - Urban Chic'),
(557, 21, 'ZKAA74AxMWc', 'http://i.ytimg.com/vi/ZKAA74AxMWc/mqdefault.jpg', 'BOSS Orange Styleguide - Rough and Unpolished'),
(558, 21, 'KE10cL400AA', 'http://i.ytimg.com/vi/KE10cL400AA/mqdefault.jpg', 'BOSS Orange Spring 2012 Campaign Shooting - On the Beach'),
(559, 21, 'FtDfxcRiL8w', 'http://i.ytimg.com/vi/FtDfxcRiL8w/mqdefault.jpg', 'BOSS Orange Spring 2012 Campaign Shooting - Statue of Rio de Janeiro'),
(560, 21, 'fpyC9jvTUJg', 'http://i.ytimg.com/vi/fpyC9jvTUJg/mqdefault.jpg', 'BOSS Orange Spring 2012 Campaign Shooting - City of Rio'),
(561, 21, 'oYJbJqFhsmc', 'http://i.ytimg.com/vi/oYJbJqFhsmc/mqdefault.jpg', 'BOSS Orange supports Unicef - Sienna Miller and Orlando Bloom Trailer'),
(562, 21, '-va7BSXqPCo', 'http://i.ytimg.com/vi/-va7BSXqPCo/mqdefault.jpg', 'BOSS Orange supports Unicef - Interview with Sienna Miller'),
(563, 21, 'Ijyf6saBIhs', 'http://i.ytimg.com/vi/Ijyf6saBIhs/mqdefault.jpg', 'BOSS Orange supports Unicef - Interview with Orlando Bloom'),
(564, 21, 'l131ZlQCdNM', 'http://i.ytimg.com/vi/l131ZlQCdNM/mqdefault.jpg', 'BOSS Orange Campaign Shooting Fall Winter 2011 Amsterdam'),
(565, 21, 'jjmjAZJz3JE', 'http://i.ytimg.com/vi/jjmjAZJz3JE/mqdefault.jpg', 'Sienna Miller For BOSS Orange The New Fragrance Teaser (presented by HUGO BOSS TV)');

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `wr_blog_comment`
--

CREATE TABLE IF NOT EXISTS `wr_blog_comment` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `content` text COLLATE utf8_unicode_ci NOT NULL,
  `status` int(11) NOT NULL,
  `create_time` int(11) DEFAULT NULL,
  `author` varchar(128) COLLATE utf8_unicode_ci DEFAULT NULL,
  `email` varchar(128) COLLATE utf8_unicode_ci DEFAULT NULL,
  `url` varchar(128) COLLATE utf8_unicode_ci DEFAULT NULL,
  `post_id` int(11) NOT NULL,
  `user_id` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `FK_comment_post` (`post_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=8 ;

--
-- Daten für Tabelle `wr_blog_comment`
--

INSERT INTO `wr_blog_comment` (`id`, `content`, `status`, `create_time`, `author`, `email`, `url`, `post_id`, `user_id`) VALUES
(7, 'Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec qu', 2, 1346275371, NULL, NULL, NULL, 23, 4);

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `wr_blog_gallery`
--

CREATE TABLE IF NOT EXISTS `wr_blog_gallery` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `order` int(11) NOT NULL,
  `title` varchar(128) COLLATE utf8_unicode_ci NOT NULL,
  `content` text COLLATE utf8_unicode_ci NOT NULL,
  `tags` text COLLATE utf8_unicode_ci,
  `status` int(11) NOT NULL,
  `create_time` int(11) DEFAULT NULL,
  `update_time` int(11) DEFAULT NULL,
  `author_id` int(11) NOT NULL,
  `gallery_id` int(11) DEFAULT NULL,
  `css_class` char(5) COLLATE utf8_unicode_ci NOT NULL DEFAULT 'col_1',
  `slug` varchar(300) COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK_post_author` (`author_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=24 ;

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `wr_blog_lookup`
--

CREATE TABLE IF NOT EXISTS `wr_blog_lookup` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(128) COLLATE utf8_unicode_ci NOT NULL,
  `code` int(11) NOT NULL,
  `type` varchar(128) COLLATE utf8_unicode_ci NOT NULL,
  `position` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=6 ;

--
-- Daten für Tabelle `wr_blog_lookup`
--

INSERT INTO `wr_blog_lookup` (`id`, `name`, `code`, `type`, `position`) VALUES
(1, 'Draft', 1, 'PostStatus', 1),
(2, 'Published', 2, 'PostStatus', 2),
(3, 'Archived', 3, 'PostStatus', 3),
(4, 'Pending Approval', 1, 'CommentStatus', 1),
(5, 'Approved', 2, 'CommentStatus', 2);

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `wr_blog_post`
--

CREATE TABLE IF NOT EXISTS `wr_blog_post` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `title` varchar(128) COLLATE utf8_unicode_ci NOT NULL,
  `content` text COLLATE utf8_unicode_ci NOT NULL,
  `tags` text COLLATE utf8_unicode_ci,
  `status` int(11) NOT NULL,
  `create_time` int(11) DEFAULT NULL,
  `update_time` int(11) DEFAULT NULL,
  `author_id` int(11) NOT NULL,
  `gallery_id` int(11) DEFAULT NULL,
  `css_class` char(5) COLLATE utf8_unicode_ci NOT NULL DEFAULT 'col_1',
  PRIMARY KEY (`id`),
  KEY `FK_post_author` (`author_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=24 ;

--
-- Daten für Tabelle `wr_blog_post`
--

INSERT INTO `wr_blog_post` (`id`, `title`, `content`, `tags`, `status`, `create_time`, `update_time`, `author_id`, `gallery_id`, `css_class`) VALUES
(23, 'Lorem ipsum dolor sit amet', 'Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae, justo. Nullam dictum felis eu pede mollis pretium. Integer tincidunt. Cras dapibus. Vivamus elementum semper nisi. Aenean vulputate eleifend tellus. Aenean leo ligula, porttitor eu, consequat vitae, eleifend ac, enim. Aliquam lorem ante, dapibus in, viverra quis, feugiat a, tellus. Phasellus viverra nulla ut metus varius laoreet. Quisque rutrum. Aenean imperdiet. Etiam ultricies nisi vel augue. Curabitur ullamcorper ultricies nisi. Nam eget dui. Etiam rhoncus. Maecenas tempus, tellus eget condimentum rhoncus, sem quam semper libero, sit amet adipiscing sem neque sed ipsum. Nam quam nunc, blandit vel, luctus pulvinar, hendrerit id, lorem. Maecenas nec odio et ante tincidunt tempus. Donec vitae sapien ut libero venenatis faucibus. Nullam quis ante. Etiam sit amet orci eget eros faucibus tincidunt. Duis leo. Sed fringilla mauris sit amet nibh. Donec sodales sagittis magna. Sed consequat, leo eget bibendum sodales, augue velit cursus nunc,', 'Lorem, ipsum, dolor, sit, amet', 2, 1346274948, 1346274948, 4, 65, 'col_1');

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `wr_blog_tag`
--

CREATE TABLE IF NOT EXISTS `wr_blog_tag` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(128) COLLATE utf8_unicode_ci NOT NULL,
  `frequency` int(11) DEFAULT '1',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=12 ;

--
-- Daten für Tabelle `wr_blog_tag`
--

INSERT INTO `wr_blog_tag` (`id`, `name`, `frequency`) VALUES
(7, 'Lorem', 1),
(8, 'ipsum', 1),
(9, 'dolor', 1),
(10, 'sit', 1),
(11, 'amet', 1);

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `wr_media_gallery`
--

CREATE TABLE IF NOT EXISTS `wr_media_gallery` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `versions_data` text COLLATE utf8_unicode_ci NOT NULL,
  `name` tinyint(1) NOT NULL DEFAULT '1',
  `description` tinyint(1) NOT NULL DEFAULT '1',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=66 ;

--
-- Daten für Tabelle `wr_media_gallery`
--

INSERT INTO `wr_media_gallery` (`id`, `versions_data`, `name`, `description`) VALUES
(20, 'a:2:{s:5:"small";a:1:{s:15:"centeredpreview";a:2:{i:0;i:340;i:1;i:434;}}s:6:"medium";a:1:{s:6:"resize";a:2:{i:0;i:800;i:1;N;}}}', 1, 1),
(22, 'a:2:{s:5:"small";a:1:{s:15:"centeredpreview";a:2:{i:0;i:340;i:1;i:434;}}s:6:"medium";a:1:{s:6:"resize";a:2:{i:0;i:800;i:1;N;}}}', 1, 1),
(23, '', 1, 1),
(28, '', 1, 1),
(29, '', 1, 1),
(30, '', 1, 1),
(33, 'a:2:{s:5:"small";a:1:{s:15:"centeredpreview";a:2:{i:0;i:340;i:1;i:434;}}s:6:"medium";a:1:{s:6:"resize";a:2:{i:0;i:800;i:1;N;}}}', 1, 1),
(35, 'a:2:{s:5:"small";a:1:{s:15:"centeredpreview";a:2:{i:0;i:340;i:1;i:434;}}s:6:"medium";a:1:{s:6:"resize";a:2:{i:0;i:800;i:1;N;}}}', 1, 1),
(37, 'a:2:{s:5:"small";a:1:{s:15:"centeredpreview";a:2:{i:0;i:340;i:1;i:434;}}s:6:"medium";a:1:{s:6:"resize";a:2:{i:0;i:800;i:1;N;}}}', 1, 1),
(39, 'a:2:{s:5:"small";a:1:{s:15:"centeredpreview";a:2:{i:0;i:340;i:1;i:434;}}s:6:"medium";a:1:{s:15:"centeredpreview";a:2:{i:0;i:700;i:1;i:600;}}}', 1, 1),
(41, 'a:2:{s:5:"small";a:1:{s:15:"centeredpreview";a:2:{i:0;i:340;i:1;i:434;}}s:6:"medium";a:1:{s:6:"resize";a:2:{i:0;i:800;i:1;i:800;}}}', 1, 1),
(42, 'a:2:{s:5:"small";a:1:{s:15:"centeredpreview";a:2:{i:0;i:340;i:1;i:434;}}s:6:"medium";a:1:{s:6:"resize";a:2:{i:0;i:450;i:1;N;}}}', 1, 1),
(43, 'a:2:{s:5:"small";a:1:{s:15:"centeredpreview";a:2:{i:0;i:340;i:1;i:434;}}s:6:"medium";a:1:{s:6:"resize";a:2:{i:0;i:450;i:1;N;}}}', 1, 1),
(44, '', 1, 1),
(47, 'a:2:{s:5:"small";a:1:{s:15:"centeredpreview";a:2:{i:0;i:340;i:1;i:434;}}s:6:"medium";a:1:{s:6:"resize";a:2:{i:0;i:450;i:1;N;}}}', 1, 1),
(54, 'a:2:{s:5:"small";a:1:{s:15:"centeredpreview";a:2:{i:0;i:340;i:1;i:434;}}s:6:"medium";a:1:{s:15:"centeredpreview";a:2:{i:0;i:800;i:1;i:600;}}}', 1, 1),
(55, 'a:2:{s:5:"small";a:1:{s:15:"centeredpreview";a:2:{i:0;i:340;i:1;i:434;}}s:6:"medium";a:1:{s:6:"resize";a:2:{i:0;i:450;i:1;N;}}}', 1, 1),
(56, 'a:2:{s:5:"small";a:1:{s:15:"centeredpreview";a:2:{i:0;i:340;i:1;i:434;}}s:6:"medium";a:1:{s:6:"resize";a:2:{i:0;i:450;i:1;N;}}}', 1, 1),
(57, 'a:2:{s:5:"small";a:1:{s:15:"centeredpreview";a:2:{i:0;i:340;i:1;i:434;}}s:6:"medium";a:1:{s:6:"resize";a:2:{i:0;i:450;i:1;N;}}}', 1, 1),
(58, 'a:2:{s:5:"small";a:1:{s:15:"centeredpreview";a:2:{i:0;i:340;i:1;i:434;}}s:6:"medium";a:1:{s:6:"resize";a:2:{i:0;i:450;i:1;N;}}}', 1, 1),
(59, 'a:2:{s:5:"small";a:1:{s:15:"centeredpreview";a:2:{i:0;i:340;i:1;i:434;}}s:6:"medium";a:1:{s:6:"resize";a:2:{i:0;i:450;i:1;N;}}}', 1, 1),
(60, 'a:2:{s:5:"small";a:1:{s:15:"centeredpreview";a:2:{i:0;i:340;i:1;i:434;}}s:6:"medium";a:1:{s:6:"resize";a:2:{i:0;i:450;i:1;N;}}}', 1, 1),
(61, 'a:2:{s:5:"small";a:1:{s:15:"centeredpreview";a:2:{i:0;i:340;i:1;i:434;}}s:6:"medium";a:1:{s:6:"resize";a:2:{i:0;i:450;i:1;N;}}}', 1, 1),
(62, 'a:2:{s:5:"small";a:1:{s:15:"centeredpreview";a:2:{i:0;i:340;i:1;i:434;}}s:6:"medium";a:1:{s:6:"resize";a:2:{i:0;i:450;i:1;N;}}}', 1, 1),
(63, 'a:2:{s:5:"small";a:1:{s:15:"centeredpreview";a:2:{i:0;i:340;i:1;i:434;}}s:6:"medium";a:1:{s:6:"resize";a:2:{i:0;i:450;i:1;N;}}}', 1, 1),
(64, 'a:2:{s:5:"small";a:1:{s:15:"centeredpreview";a:2:{i:0;i:340;i:1;i:434;}}s:6:"medium";a:1:{s:6:"resize";a:2:{i:0;i:450;i:1;N;}}}', 1, 1),
(65, 'a:2:{s:5:"small";a:1:{s:15:"centeredpreview";a:2:{i:0;i:98;i:1;i:98;}}s:6:"medium";a:1:{s:6:"resize";a:2:{i:0;i:800;i:1;N;}}}', 1, 1);

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `wr_media_gallery_photo`
--

CREATE TABLE IF NOT EXISTS `wr_media_gallery_photo` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `gallery_id` int(11) NOT NULL,
  `rank` int(11) NOT NULL DEFAULT '0',
  `name` varchar(512) COLLATE utf8_unicode_ci NOT NULL,
  `description` text COLLATE utf8_unicode_ci NOT NULL,
  `file_name` varchar(128) COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`id`),
  KEY `fk_gallery_photo_gallery1` (`gallery_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=286 ;

--
-- Daten für Tabelle `wr_media_gallery_photo`
--

INSERT INTO `wr_media_gallery_photo` (`id`, `gallery_id`, `rank`, `name`, `description`, `file_name`) VALUES
(193, 42, 193, '', '', 'GOLF 1 595.jpg'),
(194, 42, 194, '', '', 'GOLF 1 627.jpg'),
(195, 42, 195, 'http://www.cniska.net/yii-bootstrap/#bootModal', 'Gggg', 'GOLF 539.jpg'),
(196, 42, 196, '', '', 'GOLF 689.jpg'),
(197, 42, 197, '', '', 'GOLF 1 616.jpg'),
(198, 42, 198, '', '', 'GOLF 847.jpg'),
(199, 42, 199, '', '', 'GOLF 913.jpg'),
(200, 42, 200, '', '', 'GOLF 1127.jpg'),
(201, 42, 201, '', '', 'GOLF 1297.jpg'),
(202, 42, 202, '', '', 'GOLF 1354.jpg'),
(203, 42, 203, '', '', 'GOLF 1463.jpg'),
(204, 42, 204, '', '', 'GOLF 1628.jpg'),
(205, 42, 205, '', '', 'GOLF 1692.jpg'),
(206, 42, 206, '', '', 'GOLF 1977.jpg'),
(235, 55, 235, '', '', 'corellia-konzeptgrafiken-004.jpg'),
(236, 55, 236, '', '', 'corellia-screenshot-001.jpg'),
(237, 55, 237, '', '', 'corellia-konzeptgrafiken-003.jpg'),
(238, 55, 238, '', '', 'corellia-konzeptgrafiken-002.jpg'),
(239, 55, 239, '', '', 'corellia-konzeptgrafiken-001.jpg'),
(240, 55, 240, '', '', 'corellia-hintergrundbild.jpg'),
(241, 55, 241, '', '', 'corellia-screenshot-002.jpg'),
(242, 55, 242, '', '', 'corellia-screenshot-004.jpg'),
(243, 55, 243, '', '', 'corellia-screenshot-003.jpg'),
(244, 56, 244, '', '', 'corellia-konzeptgrafiken-004.jpg'),
(245, 56, 245, '', '', 'corellia-konzeptgrafiken-001.jpg'),
(246, 56, 246, '', '', 'corellia-screenshot-001.jpg'),
(247, 56, 247, '', '', 'corellia-hintergrundbild.jpg'),
(248, 56, 248, '', '', 'corellia-konzeptgrafiken-003.jpg'),
(249, 56, 249, '', '', 'corellia-konzeptgrafiken-002.jpg'),
(250, 56, 250, '', '', 'corellia-screenshot-002.jpg'),
(251, 56, 251, '', '', 'corellia-screenshot-003.jpg'),
(252, 56, 252, '', '', 'corellia-screenshot-004.jpg'),
(253, 57, 253, '', '', 'corellia-screenshot-001.jpg'),
(254, 57, 254, '', '', 'corellia-konzeptgrafiken-002.jpg'),
(255, 57, 255, '', '', 'corellia-konzeptgrafiken-004.jpg'),
(256, 57, 256, '', '', 'corellia-konzeptgrafiken-003.jpg'),
(257, 57, 257, '', '', 'corellia-hintergrundbild.jpg'),
(258, 57, 258, '', '', 'corellia-konzeptgrafiken-001.jpg'),
(259, 57, 259, '', '', 'corellia-screenshot-002.jpg'),
(260, 57, 260, '', '', 'corellia-screenshot-003.jpg'),
(261, 57, 261, '', '', 'corellia-screenshot-004.jpg'),
(262, 59, 262, '', '', 'coruscant-konzeptgrafiken-002.jpg'),
(263, 59, 263, '', '', 'coruscant-hintergrundbild-003.jpg'),
(264, 59, 264, '', '', 'coruscant-konzeptgrafiken-003.jpg'),
(265, 59, 265, '', '', 'coruscant-hintergrundbild-002.jpg'),
(266, 59, 266, '', '', 'coruscant-hintergrundbild-001.jpg'),
(267, 59, 267, '', '', 'coruscant-konzeptgrafiken-001.jpg'),
(268, 61, 268, '', '', 'alderaan-hintergrundbild.jpg'),
(269, 61, 269, '', '', 'alderaan-konzeptgrafiken-001.jpg'),
(270, 61, 270, '', '', 'alderaan-screenshot-002.jpg'),
(271, 61, 271, '', '', 'alderaan-konzeptgrafiken-003.jpg'),
(272, 61, 272, '', '', 'alderaan-konzeptgrafiken-004.jpg'),
(273, 61, 273, '', '', 'alderaan-konzeptgrafiken-002.jpg'),
(274, 61, 274, '', '', 'alderaan-screenshot-003.jpg'),
(275, 61, 275, '', '', 'alderaan-screenshot-004.jpg'),
(276, 63, 276, '', '', 'balmorra-konzeptgrafiken-002.jpg'),
(277, 63, 277, '', '', 'balmorra-konzeptgrafiken-004.jpg'),
(278, 63, 278, '', '', 'balmorra-konzeptgrafiken-001.jpg'),
(281, 63, 281, '', '', 'balmorra-konzeptgrafiken-003.jpg'),
(282, 63, 282, '', '', 'balmorra-screenshot-001.jpg'),
(283, 63, 283, '', '', 'balmorra-screenshot-002.jpg'),
(284, 63, 284, '', '', 'balmorra-screenshot-003.jpg'),
(285, 63, 285, '', '', 'balmorra-screenshot-004.jpg');

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `wr_tree`
--

CREATE TABLE IF NOT EXISTS `wr_tree` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(255) NOT NULL,
  `path` varchar(555) DEFAULT NULL,
  `icon_id` int(11) DEFAULT NULL,
  `lft` int(10) unsigned NOT NULL,
  `rgt` int(10) unsigned NOT NULL,
  `level` smallint(5) unsigned NOT NULL,
  `root` int(10) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `lft` (`lft`),
  KEY `rgt` (`rgt`),
  KEY `level` (`level`),
  KEY `icon_id` (`icon_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=82 ;

--
-- Daten für Tabelle `wr_tree`
--

INSERT INTO `wr_tree` (`id`, `name`, `path`, `icon_id`, `lft`, `rgt`, `level`, `root`) VALUES
(10, 'root', NULL, NULL, 0, 33, 0, 1),
(11, 'Root', '', 19, 1, 32, 1, 1),
(12, 'Menü', '', 43, 3, 10, 3, 1),
(15, 'User', '', 8, 11, 18, 3, 1),
(18, 'Tree Create', '/wr_tree/tree/create', 81, 4, 5, 4, 1),
(20, 'Tree Admin', '/wr_tree/tree/admin', 21, 6, 7, 4, 1),
(21, 'Tree ImportModule', '/wr_tree/tree/importModule', 25, 8, 9, 4, 1),
(22, 'User Create', '/wr_user/user/create', 81, 14, 15, 4, 1),
(23, 'User Status', '/wr_user/user/status', 138, 16, 17, 4, 1),
(24, 'User Admin', '/wr_user/user/admin', 21, 12, 13, 4, 1),
(45, 'Backend Menu', '', 103, 2, 31, 2, 1),
(75, 'Cache leeren', '/wr_system/main/clearCacheFiles', 20, 29, 30, 3, 1),
(76, 'Blog', '', 61, 19, 28, 3, 1),
(77, 'Kommentare', '/wr_blog/comment/index', 122, 26, 27, 4, 1),
(79, 'Post Create', '/wr_blog/post/create', 81, 20, 21, 4, 1),
(80, 'Post Index', '/wr_blog/post/index', 105, 22, 23, 4, 1),
(81, 'Post Admin', '/wr_blog/post/admin', 32, 24, 25, 4, 1);

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `wr_tree_icons`
--

CREATE TABLE IF NOT EXISTS `wr_tree_icons` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `html` text COLLATE utf8_unicode_ci NOT NULL,
  `name` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=141 ;

--
-- Daten für Tabelle `wr_tree_icons`
--

INSERT INTO `wr_tree_icons` (`id`, `html`, `name`) VALUES
(1, '<i class="icon-glass"></i>', 'icon-glass'),
(2, '<i class="icon-music"></i>', 'icon-music'),
(3, '<i class="icon-search"></i>', 'icon-search'),
(4, '<i class="icon-envelope"></i>', 'icon-envelope'),
(5, '<i class="icon-heart"></i>', 'icon-heart'),
(6, '<i class="icon-star"></i>', 'icon-star'),
(7, '<i class="icon-star-empty"></i>', 'icon-star-empty'),
(8, '<i class="icon-user"></i>', 'icon-user'),
(9, '<i class="icon-film"></i>', 'icon-film'),
(10, '<i class="icon-th-large"></i>', 'icon-th-large'),
(11, '<i class="icon-th"></i>', 'icon-th'),
(12, '<i class="icon-th-list"></i>', 'icon-th-list'),
(13, '<i class="icon-ok"></i>', 'icon-ok'),
(14, '<i class="icon-remove"></i>', 'icon-remove'),
(15, '<i class="icon-zoom-in"></i>', 'icon-zoom-in'),
(16, '<i class="icon-zoom-out"></i>', 'icon-zoom-out'),
(17, '<i class="icon-off"></i>', 'icon-off'),
(18, '<i class="icon-signal"></i>', 'icon-signal'),
(19, '<i class="icon-cog"></i>', 'icon-cog'),
(20, '<i class="icon-trash"></i>', 'icon-trash'),
(21, '<i class="icon-home"></i>', 'icon-home'),
(22, '<i class="icon-file"></i>', 'icon-file'),
(23, '<i class="icon-time"></i>', 'icon-time'),
(24, '<i class="icon-road"></i>', 'icon-road'),
(25, '<i class="icon-download-alt"></i>', 'icon-download-alt'),
(26, '<i class="icon-download"></i>', 'icon-download'),
(27, '<i class="icon-upload"></i>', 'icon-upload'),
(28, '<i class="icon-inbox"></i>', 'icon-inbox'),
(29, '<i class="icon-play-circle"></i>', 'icon-play-circle'),
(30, '<i class="icon-repeat"></i>', 'icon-repeat'),
(31, '<i class="icon-refresh"></i>', 'icon-refresh'),
(32, '<i class="icon-list-alt"></i>', 'icon-list-alt'),
(33, '<i class="icon-lock"></i>', 'icon-lock'),
(34, '<i class="icon-flag"></i>', 'icon-flag'),
(35, '<i class="icon-headphones"></i>', 'icon-headphones'),
(36, '<i class="icon-volume-off"></i>', 'icon-volume-off'),
(37, '<i class="icon-volume-down"></i>', 'icon-volume-down'),
(38, '<i class="icon-volume-up"></i>', 'icon-volume-up'),
(39, '<i class="icon-qrcode"></i>', 'icon-qrcode'),
(40, '<i class="icon-barcode"></i>', 'icon-barcode'),
(41, '<i class="icon-tag"></i>', 'icon-tag'),
(42, '<i class="icon-tags"></i>', 'icon-tags'),
(43, '<i class="icon-book"></i>', 'icon-book'),
(44, '<i class="icon-bookmark"></i>', 'icon-bookmark'),
(45, '<i class="icon-print"></i>', 'icon-print'),
(46, '<i class="icon-camera"></i>', 'icon-camera'),
(47, '<i class="icon-font"></i>', 'icon-font'),
(48, '<i class="icon-bold"></i>', 'icon-bold'),
(49, '<i class="icon-italic"></i>', 'icon-italic'),
(50, '<i class="icon-text-height"></i>', 'icon-text-height'),
(51, '<i class="icon-text-width"></i>', 'icon-text-width'),
(52, '<i class="icon-align-left"></i>', 'icon-align-left'),
(53, '<i class="icon-align-center"></i>', 'icon-align-center'),
(54, '<i class="icon-align-right"></i>', 'icon-align-right'),
(55, '<i class="icon-align-justify"></i>', 'icon-align-justify'),
(56, '<i class="icon-list"></i>', 'icon-list'),
(57, '<i class="icon-indent-left"></i>', 'icon-indent-left'),
(58, '<i class="icon-indent-right"></i>', 'icon-indent-right'),
(59, '<i class="icon-facetime-video"></i>', 'icon-facetime-video'),
(60, '<i class="icon-picture"></i>', 'icon-picture'),
(61, '<i class="icon-pencil"></i>', 'icon-pencil'),
(62, '<i class="icon-map-marker"></i>', 'icon-map-marker'),
(63, '<i class="icon-adjust"></i>', 'icon-adjust'),
(64, '<i class="icon-tint"></i>', 'icon-tint'),
(65, '<i class="icon-edit"></i>', 'icon-edit'),
(66, '<i class="icon-share"></i>', 'icon-share'),
(67, '<i class="icon-check"></i>', 'icon-check'),
(68, '<i class="icon-move"></i>', 'icon-move'),
(69, '<i class="icon-step-backward"></i>', 'icon-step-backward'),
(70, '<i class="icon-fast-backward"></i>', 'icon-fast-backward'),
(71, '<i class="icon-backward"></i>', 'icon-backward'),
(72, '<i class="icon-play"></i>', 'icon-play'),
(73, '<i class="icon-pause"></i>', 'icon-pause'),
(74, '<i class="icon-stop"></i>', 'icon-stop'),
(75, '<i class="icon-forward"></i>', 'icon-forward'),
(76, '<i class="icon-fast-forward"></i>', 'icon-fast-forward'),
(77, '<i class="icon-step-forward"></i>', 'icon-step-forward'),
(78, '<i class="icon-eject"></i>', 'icon-eject'),
(79, '<i class="icon-chevron-left"></i>', 'icon-chevron-left'),
(80, '<i class="icon-chevron-right"></i>', 'icon-chevron-right'),
(81, '<i class="icon-plus-sign"></i>', 'icon-plus-sign'),
(82, '<i class="icon-minus-sign"></i>', 'icon-minus-sign'),
(83, '<i class="icon-remove-sign"></i>', 'icon-remove-sign'),
(84, '<i class="icon-ok-sign"></i>', 'icon-ok-sign'),
(85, '<i class="icon-question-sign"></i>', 'icon-question-sign'),
(86, '<i class="icon-info-sign"></i>', 'icon-info-sign'),
(87, '<i class="icon-screenshot"></i>', 'icon-screenshot'),
(88, '<i class="icon-remove-circle"></i>', 'icon-remove-circle'),
(89, '<i class="icon-ok-circle"></i>', 'icon-ok-circle'),
(90, '<i class="icon-ban-circle"></i>', 'icon-ban-circle'),
(91, '<i class="icon-arrow-left"></i>', 'icon-arrow-left'),
(92, '<i class="icon-arrow-right"></i>', 'icon-arrow-right'),
(93, '<i class="icon-arrow-up"></i>', 'icon-arrow-up'),
(94, '<i class="icon-arrow-down"></i>', 'icon-arrow-down'),
(95, '<i class="icon-share-alt"></i>', 'icon-share-alt'),
(96, '<i class="icon-resize-full"></i>', 'icon-resize-full'),
(97, '<i class="icon-resize-small"></i>', 'icon-resize-small'),
(98, '<i class="icon-plus"></i>', 'icon-plus'),
(99, '<i class="icon-minus"></i>', 'icon-minus'),
(100, '<i class="icon-asterisk"></i>', 'icon-asterisk'),
(101, '<i class="icon-exclamation-sign"></i>', 'icon-exclamation-sign'),
(102, '<i class="icon-gift"></i>', 'icon-gift'),
(103, '<i class="icon-leaf"></i>', 'icon-leaf'),
(104, '<i class="icon-fire"></i>', 'icon-fire'),
(105, '<i class="icon-eye-open"></i>', 'icon-eye-open'),
(106, '<i class="icon-eye-close"></i>', 'icon-eye-close'),
(107, '<i class="icon-warning-sign"></i>', 'icon-warning-sign'),
(108, '<i class="icon-plane"></i>', 'icon-plane'),
(109, '<i class="icon-calendar"></i>', 'icon-calendar'),
(110, '<i class="icon-random"></i>', 'icon-random'),
(111, '<i class="icon-comment"></i>', 'icon-comment'),
(112, '<i class="icon-magnet"></i>', 'icon-magnet'),
(113, '<i class="icon-chevron-up"></i>', 'icon-chevron-up'),
(114, '<i class="icon-chevron-down"></i>', 'icon-chevron-down'),
(115, '<i class="icon-retweet"></i>', 'icon-retweet'),
(116, '<i class="icon-shopping-cart"></i>', 'icon-shopping-cart'),
(117, '<i class="icon-folder-close"></i>', 'icon-folder-close'),
(118, '<i class="icon-folder-open"></i>', 'icon-folder-open'),
(119, '<i class="icon-resize-vertical"></i>', 'icon-resize-vertical'),
(120, '<i class="icon-resize-horizontal"></i>', 'icon-resize-horizontal'),
(121, '<i class="icon-hdd"></i>', 'icon-hdd'),
(122, '<i class="icon-bullhorn"></i>', 'icon-bullhorn'),
(123, '<i class="icon-bell"></i>', 'icon-bell'),
(124, '<i class="icon-certificate"></i>', 'icon-certificate'),
(125, '<i class="icon-thumbs-up"></i>', 'icon-thumbs-up'),
(126, '<i class="icon-thumbs-down"></i>', 'icon-thumbs-down'),
(127, '<i class="icon-hand-right"></i>', 'icon-hand-right'),
(128, '<i class="icon-hand-left"></i>', 'icon-hand-left'),
(129, '<i class="icon-hand-up"></i>', 'icon-hand-up'),
(130, '<i class="icon-hand-down"></i>', 'icon-hand-down'),
(131, '<i class="icon-circle-arrow-right"></i>', 'icon-circle-arrow-right'),
(132, '<i class="icon-circle-arrow-left"></i>', 'icon-circle-arrow-left'),
(133, '<i class="icon-circle-arrow-up"></i>', 'icon-circle-arrow-up'),
(134, '<i class="icon-circle-arrow-down"></i>', 'icon-circle-arrow-down'),
(135, '<i class="icon-globe"></i>', 'icon-globe'),
(136, '<i class="icon-wrench"></i>', 'icon-wrench'),
(137, '<i class="icon-tasks"></i>', 'icon-tasks'),
(138, '<i class="icon-filter"></i>', 'icon-filter'),
(139, '<i class="icon-briefcase"></i>', 'icon-briefcase'),
(140, '<i class="icon-fullscreen"></i>', 'icon-fullscreen');

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `wr_user`
--

CREATE TABLE IF NOT EXISTS `wr_user` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `author_id` int(11) NOT NULL,
  `group_id` int(11) NOT NULL DEFAULT '1',
  `password_length` int(11) DEFAULT NULL,
  `username` varchar(128) NOT NULL,
  `password_hash` char(60) NOT NULL,
  `email` varchar(128) NOT NULL,
  `create` datetime NOT NULL,
  `update` datetime NOT NULL,
  `status` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `group_id` (`group_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=5 ;

--
-- Daten für Tabelle `wr_user`
--

INSERT INTO `wr_user` (`id`, `author_id`, `group_id`, `password_length`, `username`, `password_hash`, `email`, `create`, `update`, `status`) VALUES
(4, 0, 6, 0, 'admin', '$2a$10$.jJbB3.tIlMjOJoY4jWIQOC3Q18J55454aakUCgBuemi54VuMOK2m', 'pb@becklyn.com', '2012-03-16 00:17:38', '2012-08-13 18:18:32', 1);

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `wr_user_group`
--

CREATE TABLE IF NOT EXISTS `wr_user_group` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(128) NOT NULL,
  `level` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=7 ;

--
-- Daten für Tabelle `wr_user_group`
--

INSERT INTO `wr_user_group` (`id`, `name`, `level`) VALUES
(1, 'Guest', 0),
(2, 'User', 1),
(3, 'Reporter', 10),
(4, 'Manager', 20),
(5, 'Admin', 70),
(6, 'SuperAdmin', 100);

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `wr_user_profiles`
--

CREATE TABLE IF NOT EXISTS `wr_user_profiles` (
  `user_id` int(11) NOT NULL,
  `lastname` varchar(50) NOT NULL DEFAULT '',
  `firstname` varchar(50) NOT NULL DEFAULT '',
  `thumb` varchar(300) NOT NULL DEFAULT '',
  `birthday` date NOT NULL DEFAULT '0000-00-00',
  PRIMARY KEY (`user_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

--
-- Daten für Tabelle `wr_user_profiles`
--

INSERT INTO `wr_user_profiles` (`user_id`, `lastname`, `firstname`, `thumb`, `birthday`) VALUES
(4, 'Brewing', 'Pascal', '47', '1977-04-26');

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `wr_user_status`
--

CREATE TABLE IF NOT EXISTS `wr_user_status` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(255) COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`id`),
  KEY `name` (`name`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci AUTO_INCREMENT=3 ;

--
-- Daten für Tabelle `wr_user_status`
--

INSERT INTO `wr_user_status` (`id`, `name`) VALUES
(1, 'activated'),
(2, 'banned');

--
-- Constraints der exportierten Tabellen
--

--
-- Constraints der Tabelle `wr_user`
--
ALTER TABLE `wr_user`
  ADD CONSTRAINT `wr_user_ibfk_1` FOREIGN KEY (`group_id`) REFERENCES `wr_user_group` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION;
