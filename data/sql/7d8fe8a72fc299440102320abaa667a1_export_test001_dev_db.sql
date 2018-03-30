-- phpMyAdmin SQL Dump
-- version 2.10.0.2
-- http://www.phpmyadmin.net
-- 
-- Host: localhost
-- Generation Time: Jun 12, 2009 at 11:07 AM
-- Server version: 5.0.27
-- PHP Version: 4.3.11RC1-dev

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";

-- 
-- Database: `test001_development`
-- 

-- 
-- Dumping data for table `beats`
-- 

INSERT INTO `beats` (`id`, `name`, `content_text`, `position`, `lesson_id`, `created_at`, `updated_at`, `intro_text`, `header`, `footer`, `intro_audio_path`, `intro_video_path`, `content_video_path`, `content_audio_path`, `description`, `pretty_name`) VALUES 
(1, 'beat one ', 'this is the first beat in the first lesson with updated field', 1, 1, '2009-04-21 05:44:52', '2009-05-06 06:16:14', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
(3, 'beat two lesson one', 'this is the content field of beat two', 2, 1, '2009-04-21 05:46:13', '2009-04-21 05:46:13', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
(13, 'beat three', 'this is the content field of beat three', 3, 1, '2009-04-21 23:39:01', '2009-04-21 23:39:01', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
(14, 'beat four', 'this is the content field for beat four', 4, 1, '2009-04-21 23:44:16', '2009-04-21 23:44:16', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
(15, 'beat five', 'this is the content field of beat five and more and more and more and more', 5, 1, '2009-04-22 04:33:18', '2009-05-07 04:25:27', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
(16, 'beat six', 'this is the content field of beat six in lesson one', 6, 1, '2009-05-07 04:16:53', '2009-05-07 04:16:53', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);

-- 
-- Dumping data for table `courses`
-- 

INSERT INTO `courses` (`id`, `name`, `header`, `footer`, `content_text`, `created_at`, `updated_at`, `intro_text`, `intro_audio_path`, `intro_video_path`, `content_video_path`, `content_audio_path`, `description`, `pretty_name`) VALUES 
(1, 'rock basics', 'this is the header of the course rock basics', 'this is the footer of the course rock basics', 'this is the content area of the course rock basics. here goes the description of the course, what the students can learn, what they cannot and the whole bit about the course and so on... and on', '2009-04-26 23:37:43', '2009-05-14 02:12:08', 'course rock basics for beginners of drumming in the style of rock and roll', '/some/path/to/somewhere', '/some/path/to/somewhere', '/some/path/to/somewhere', '/some/path/to/somewhere', 'course rock basics for beginners of drumming in the style of rock and roll as the description field would suggest!', 'course rock basics for beginners of drumming in the style of rock and roll');

-- 
-- Dumping data for table `lessons`
-- 

INSERT INTO `lessons` (`id`, `name`, `content_text`, `header`, `footer`, `created_at`, `updated_at`, `term_id`, `intro_text`, `intro_audio_path`, `intro_video_path`, `content_video_path`, `content_audio_path`, `description`, `pretty_name`, `position`) VALUES 
(1, 'lesson one', 'this is the content field of lesson one and more and more', 'this is the header of lesson one', 'this is the footer of lesson one', '2009-04-21 02:15:37', '2009-05-07 02:32:37', 1, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
(2, 'lesson two', 'this is the content field of lesson two', 'this is the header of lesson two', 'this is the footer of lesson two', '2009-04-21 02:16:24', '2009-04-23 22:42:59', 1, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
(3, 'lesson three', 'this is the content field of lesson three', 'this is the header of lesson three', 'this is the footer of lesson three', '2009-04-21 02:16:34', '2009-04-23 22:43:21', 1, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
(4, 'lesson four', 'this is the content field of lesson four', 'this is the header of lesson four', 'this is the footer of lesson four', '2009-04-22 01:20:41', '2009-04-23 22:43:28', 1, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);

 


-- 
-- Dumping data for table `terms`
-- 

INSERT INTO `terms` (`id`, `name`, `header`, `footer`, `content_text`, `position`, `course_id`, `created_at`, `updated_at`, `intro_text`, `intro_audio_path`, `intro_video_path`, `content_video_path`, `content_audio_path`, `description`, `pretty_name`) VALUES 
(1, 'course rock basics term one ', 'this is the header of term one', 'this is the footer of term one', 'this is the content are of term one of the course rock basics. Here goes the description of the content of the term what it is all about and all that kind of stuff stuff stuff', 1, 1, '2009-04-26 23:19:02', '2009-05-14 04:42:23', '', '', '', '', '', '', 'course rock basics term one for beginners'),
(2, 'course rock basics term two', 'this is the header of term two', 'this is the footer of term two', 'this is the content are of term two', 2, 1, '2009-05-05 02:43:50', '2009-05-05 02:43:50', NULL, NULL, NULL, NULL, NULL, NULL, NULL);
