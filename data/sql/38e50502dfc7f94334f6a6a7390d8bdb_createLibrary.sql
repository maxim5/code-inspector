# Make sure tables don't exist before creating.

DROP TABLE IF EXISTS Returns;
DROP TABLE IF EXISTS Checkout;
DROP TABLE IF EXISTS HasAuthor;
DROP TABLE IF EXISTS HasSearchGenre;
DROP TABLE IF EXISTS HasPublisher;
DROP TABLE IF EXISTS Patron;
DROP TABLE IF EXISTS Librarian;
DROP TABLE IF EXISTS Book;
DROP TABLE IF EXISTS Category;
DROP TABLE IF EXISTS Publisher;
DROP TABLE IF EXISTS SearchGenre;
DROP TABLE IF EXISTS Author;
DROP TABLE IF EXISTS BookType;



# Create all the tables

CREATE TABLE IF NOT EXISTS BookType (
	typeName varchar(30),
	maxReservation int,
	overdueFee int,
	PRIMARY KEY (typeName)
);

CREATE TABLE IF NOT EXISTS Author (
	name varchar(20),
	PRIMARY KEY (name)
);

CREATE TABLE IF NOT EXISTS Publisher (
	name varchar(50),
	PRIMARY KEY (name)
);

CREATE TABLE IF NOT EXISTS SearchGenre (
	name varchar(20),
	PRIMARY KEY (name)
);

CREATE TABLE IF NOT EXISTS Patron (
	cardNumber int AUTO_INCREMENT,
	name varchar(20),
	phone int,
	address varchar(100), 
	unpaidFees int,
	PRIMARY KEY (cardNumber)
	/* CONSTRAINT VALID_CARDNUMBER
	CHECK (cardNumber >= 100000 AND cardNumber <= 999999) */
) AUTO_INCREMENT = 100000;


CREATE TABLE IF NOT EXISTS Librarian (
	idNumber int AUTO_INCREMENT, 
	name varchar(20), 
	address varchar(100),
	PRIMARY KEY (idNumber)
/* CONSTRAINT VALID_ID
	CHECK (idNumber >= 1000 AND idNumber <= 9999) */
) AUTO_INCREMENT = 1000;

CREATE TABLE IF NOT EXISTS Category (
	name varchar(20),
	idNumber int AUTO_INCREMENT,
	superCategoryId int,
	PRIMARY KEY (idNumber),
	FOREIGN KEY (superCategoryId) REFERENCES Category (idNumber)
);

CREATE TABLE IF NOT EXISTS Book (
	isbn bigint,
	title varchar(100),
	description varchar(255), 
	currentQuantity int, 
	totalQuantity int, 
	publisherYear int,
	idNumber int NOT NULL, 
	typeName varchar(30) NOT NULL,
	PRIMARY KEY (isbn),
	FOREIGN KEY (idNumber) REFERENCES Category(idNumber)
	ON DELETE CASCADE
	ON UPDATE CASCADE,
	FOREIGN KEY (typeName) REFERENCES BookType(typeName)
	ON DELETE CASCADE
	ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS HasSearchGenre (
	isbn bigint, 
	name varchar(20),
	PRIMARY KEY (isbn, name),
	FOREIGN KEY (isbn) REFERENCES Book(isbn)
	ON DELETE CASCADE,
	FOREIGN KEY (name) REFERENCES SearchGenre(name)
	ON DELETE CASCADE
	ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS HasAuthor (
	isbn bigint, 
	name varchar(20),
	PRIMARY KEY (isbn, name),
	FOREIGN KEY (isbn) REFERENCES Book(isbn)
	ON DELETE CASCADE
	ON UPDATE CASCADE,
	FOREIGN KEY (name) REFERENCES Author(name)
	ON DELETE CASCADE
	ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS HasPublisher (
	isbn bigint, 
	name varchar(50),
	PRIMARY KEY (isbn, name),
	FOREIGN KEY (isbn) REFERENCES Book(isbn)
	ON DELETE CASCADE
	ON UPDATE CASCADE,
	FOREIGN KEY (name) REFERENCES Publisher(name)
	ON DELETE CASCADE
	ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS CheckOut (
	isbn bigint, 
	start timestamp, 
	end timestamp, 
	cardNumber int, 
	idNumber int,
	PRIMARY KEY (isbn, start, end, cardNumber, idNumber),
	FOREIGN KEY (isbn) REFERENCES Book(isbn)
	ON DELETE CASCADE
	ON UPDATE CASCADE,
	FOREIGN KEY (cardNumber) REFERENCES Patron(cardNumber)
	ON DELETE CASCADE
	ON UPDATE CASCADE,
	FOREIGN KEY (idNumber) REFERENCES Librarian(idNumber)
	ON DELETE CASCADE
	ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS Returns (
	isbn bigint, 
	start timestamp, 
	end timestamp, 
	cardNumber int,
	returned timestamp, 
	checkoutId int,
	returnId int,
	PRIMARY KEY (isbn, start, end, cardNumber, checkoutId, returnId),
	FOREIGN KEY (isbn, start, end, cardNumber, checkoutId) REFERENCES CheckOut(isbn, start, end, cardNumber, idNumber)
	ON DELETE CASCADE
	ON UPDATE CASCADE,
	FOREIGN KEY (returnId) REFERENCES Librarian(idNumber)
	ON DELETE CASCADE
	ON UPDATE CASCADE
);

/* Assertions for relationships with total participation for both entities (NOTE: handled at application level)
CREATE ASSERTION AuthorParticipation
CHECK (( SELECT COUNT (DISTINCT (H.name)) FROM HasAuthor H ) - ( SELECT COUNT (DISTINCT (A.name)) FROM Author A ) = 0)

CREATE ASSERTION BookAuthParticipation
CHECK (( SELECT COUNT (DISTINCT (H.isbn)) FROM HasAuthor H ) - ( SELECT COUNT (DISTINCT (B.isbn)) FROM Book B ) = 0)

CREATE ASSERTION PublisherParticipation
CHECK (( SELECT COUNT (DISTINCT (H.name)) FROM HasPublisher H ) - ( SELECT COUNT (DISTINCT (P.name)) FROM Publisher P ) = 0)

CREATE ASSERTION BookPubParticipation
CHECK (( SELECT COUNT (DISTINCT (H.isbn)) FROM HasPublisher H ) - ( SELECT COUNT (DISTINCT (B.isbn)) FROM Book B ) = 0)
*/



# Populate tables

INSERT
INTO BookType(typeName, maxReservation, overdueFee)
VALUES ('Adult Fiction', '14', '1'),
('Adult Nonfiction', '7', '2'),
('Children Fiction', '21', '1'),
('Children Nonfiction', '21', '1'),
('Audiobooks', '14', '3'),
('Reference Books', '2', '3');

INSERT
INTO Author
VALUES ('J.K. Rowling'),
('J.R.R Tolkien'),
('Johannes Gehrke'),
('Raghu Ramakrishnan'),
('Michael Harrington'),
('Scott Freeman'),
('Joan C. Sharp'),
('Mary Shelley'),
('Mark Twain'),
('Jane Austen'),
('Agatha Christie'),
('George Orwell'),
('Dr. Seuss'),
('Gordon Ramsay'),
('Chris Hadfield'),
('Susan Cain'),
('Robert Munsch'),
('Neil Gaiman'),
('Randall Munroe'),
('Benjamin Graham'),
('Stephen King'),
('Eben Alexander M.D.'),
('Oxford Dictionaries');

INSERT
INTO Publisher
VALUES ('Scholastic'),
('Scholastic Paperbacks'),
('Arthur A. Levine Books'),
('Houghton Mifflin Harcourt'),
('Mcgraw Hill Ryerson Ltd'),
('Pearson Education Canada'),
('Penguin Classics'),
('Harper Collins Publishers'),
('Penguin UK'),
('Random House Books for Younger Readers'),
('Grand Central Life & Style'),
('Random House Canada'),
('Broadway Books'),
('Firefly Books'),
('Vertigo'),
('Harper Business'),
('Scribner'),
('Simon & Schuster'),
('Oxford University Press');

INSERT 
INTO SearchGenre
VALUES ('Fantasy'),
('Textbook'),
('Mystery'),
('Humor'),
('Science fiction'),
('Biography'),
('Essay'),
('Dictionary'),
('Lifestyle'),
('Dystopian'),
('Religion'),
('Picture Books'),
('Graphic Novels'),
('Visual Essay'),
('Thriller');

INSERT
INTO Patron(name, phone, address, unpaidFees)
VALUES ('Jack Smith', '5555555', '1234 Madeup St', '0'),
('Jane Lambda', '1234567', '9876 Weird Ave', '3'),
('Bill Chan', '1235354', '3141 Pi Rd', '10'),
('Person Name', '1129284', '1038 Random Rd', '0'),
('Norman Doorman', '3072846', '1203 Knob Street', '35'),
('Lucy Latesworth', '2342357', '22 Main Street', '35');


INSERT
INTO Librarian(name, address)
VALUES ('Strict Shusher', '5000 Quiet Ave'),
('Nice Nora', '2734 Kindness Dr'),
('Clumsy Chad', '3756 Knockover Rd'),
('Lily Late', '1984 Sorry St'),
('Peter Pan', '4729 Neverland Dr');

INSERT
INTO Category(name, superCategoryId)
VALUES ('Computer Science', null),
('Literature', null),
('Biology', null),
('Databases', '1'),
('Genetics', '3'),
('Canadian Literature', '2'),
('Cooking', null),
('Photography', null),
('Psychology', null),
('Entertainment', null),
('Trivia', '10'),
('Finances', null),
('Investing', '12'),
('Religion', null),
('Science & Religion', '14'),
('Dictionary', null);

INSERT
INTO Book(isbn, title, description, currentQuantity, totalQuantity, publisherYear, idNumber, typeName)
VALUES 
('9780590353427', 'Harry Potter and the Sorcerers Stone', 'Harry Potter goes to Hogwarts.', '1', '3', '1999',
	(SELECT idNumber FROM Category WHERE name='Literature'), (SELECT typeName FROM BookType WHERE typeName='Children Fiction')),
('9780439064873', 'Harry Potter and the Chamber of Secrets', 'Harry Potter fights a basilisk.', '2', '2', '2000',
	(SELECT idNumber FROM Category WHERE name='Literature'), (SELECT typeName FROM BookType WHERE typeName='Children Fiction')),
('9780545139700', 'Harry Potter and the Deathly Hallows', 'The final Harry Potter book.', '0', '2', '2009',
	(SELECT idNumber FROM Category WHERE name='Literature'), (SELECT typeName FROM BookType WHERE typeName='Children Fiction')),
('9780547928227', 'The Hobbit', 'In a hole in the ground there lived a hobbit.', '2', '5', '2012',
	(SELECT idNumber FROM Category WHERE name='Literature'), (SELECT typeName FROM BookType WHERE typeName='Children Fiction')),
('9780072465631', 'Database Management Systems', 'CPSC 304 Textbook', '0', '1', '2003',
	(SELECT idNumber FROM Category WHERE name='Databases'), (SELECT typeName FROM BookType WHERE typeName='Reference Books')),
('9780321834843', 'Biological Science', 'First Year Biology Textbook', '0', '2', '2012',
	(SELECT idNumber FROM Category WHERE name='Biology'), (SELECT typeName FROM BookType WHERE typeName='Reference Books')),
('9780141439471', 'Frankenstein', 'The classic about a monster creating life.', '3', '3', '2003', 
	(SELECT idNumber FROM Category WHERE name='Literature'), (SELECT typeName FROM BookType WHERE typeName='Adult Fiction')),
('9780141036144', '1984', 'Doublethink. Big Brother. Doublespeak.', '2', '2', '2008', 
	(SELECT idNumber FROM Category WHERE name='Literature'), (SELECT typeName FROM BookType WHERE typeName='Adult Fiction')),
('9780394800165', 'Green Eggs and Ham', '"I do not like green eggs and ham. I do not like them, Sam-I-Am."', '1', '3', '1960', 
	(SELECT idNumber FROM Category WHERE name='Literature'), (SELECT typeName FROM BookType WHERE typeName='Children Fiction')),
('9781455525256', 'Gordon Ramsays Home Cooking: Everything You Need to Know to Make Fabulous Food', 
	'From the chef known for being mean in his cooking shows.', '0', '1', '2013', 
	(SELECT idNumber FROM Category WHERE name='Cooking'), (SELECT typeName FROM BookType WHERE typeName='Adult Nonfiction')),
('9780345814944', 'You Are Here: Around the World in 92 Minutes',
	'A visual essay of Earth from the viewpoint of the International Space Station.', '1', '1', '2014', 
	(SELECT idNumber FROM Category WHERE name='Photography'), (SELECT typeName FROM BookType WHERE typeName='Adult Nonfiction')),
('9780307352156', 'Quiet: The Power of Introverts in a World That Cant Stop Talking',
	'Introversion isnt bad.', '1', '1', '2013', 
	(SELECT idNumber FROM Category WHERE name='Psychology'), (SELECT typeName FROM BookType WHERE typeName='Adult Nonfiction')),
('9780920668375', 'Love You Forever', 'The cover of the book has a kid messing up the bathroom.', '3', '4', '1995', 
	(SELECT idNumber FROM Category WHERE name='Literature'), (SELECT typeName FROM BookType WHERE typeName='Children Fiction')),
('9781401225759', 'The Sandman Vol. 1: Preludes & Nocturnes', 'A graphic novel epic.', '1', '2', '2010', 
	(SELECT idNumber FROM Category WHERE name='Literature'), (SELECT typeName FROM BookType WHERE typeName='Adult Fiction')),
('9780544272996', 'What If?: Serious Scientific Answers to Absurd Hypothetical Questions', 
	'From the creator of webcomic xkcd (supposedly popular).', '0', '1', '2014', 
	(SELECT idNumber FROM Category WHERE name='Trivia'), (SELECT typeName FROM BookType WHERE typeName='Adult Nonfiction')),
('9780060555665', 'The Intelligent Investor: The Definitive Book on Value Investing', 'Dont invest without knowing how.', '1', '2', '2003', 
	(SELECT idNumber FROM Category WHERE name='Investing'), (SELECT typeName FROM BookType WHERE typeName='Adult Nonfiction')),
('9781476754451', 'Mr. Mercedes', 'A Stephen King thriller.', '2', '3', '2014', 
	(SELECT idNumber FROM Category WHERE name='Literature'), (SELECT typeName FROM BookType WHERE typeName='Adult Fiction')),
('9781451695199', 'Proof of Heaven: A Neurosurgeons Journey into the Afterlife',
	'A doctor experiences a near death experience of his own.', '1', '1', '2012', 
	(SELECT idNumber FROM Category WHERE name='Science & Religion'), (SELECT typeName FROM BookType WHERE typeName='Adult Nonfiction')),
('9780199640942', 'Paperback Oxford English Dictionary', 'Just a dictionary.', '3', '3', '2012', 
	(SELECT idNumber FROM Category WHERE name='Dictionary'), (SELECT typeName FROM BookType WHERE typeName='Reference Books'));

INSERT
INTO HasSearchGenre(isbn, name)
VALUES 
((SELECT isbn FROM Book WHERE title='Harry Potter and the Sorcerers Stone'), (SELECT name FROM SearchGenre WHERE name='Fantasy')),
((SELECT isbn FROM Book WHERE title='Harry Potter and the Chamber of Secrets'), (SELECT name FROM SearchGenre WHERE name='Fantasy')),
((SELECT isbn FROM Book WHERE title='Harry Potter and the Deathly Hallows'), (SELECT name FROM SearchGenre WHERE name='Fantasy')),
((SELECT isbn FROM Book WHERE title='The Hobbit'), (SELECT name FROM SearchGenre WHERE name='Fantasy')),
((SELECT isbn FROM Book WHERE title='Database Management Systems'), (SELECT name FROM SearchGenre WHERE name='Textbook')),
((SELECT isbn FROM Book WHERE title='Biological Science'), (SELECT name FROM SearchGenre WHERE name='Textbook')),
((SELECT isbn FROM Book WHERE title='Frankenstein'), (SELECT name FROM SearchGenre WHERE name='Science Fiction')),
((SELECT isbn FROM Book WHERE title='1984'), (SELECT name FROM SearchGenre WHERE name='Dystopian')),
((SELECT isbn FROM Book WHERE title='Green Eggs and Ham'), (SELECT name FROM SearchGenre WHERE name='Humor')),
((SELECT isbn FROM Book WHERE title='Gordon Ramsays Home Cooking: Everything You Need to Know to Make Fabulous Food'), 
	(SELECT name FROM SearchGenre WHERE name='Lifestyle')),
((SELECT isbn FROM Book WHERE title='You Are Here: Around the World in 92 Minutes'), (SELECT name FROM SearchGenre WHERE name='Visual Essay')),
((SELECT isbn FROM Book WHERE title='Quiet: The Power of Introverts in a World That Cant Stop Talking'), 
	(SELECT name FROM SearchGenre WHERE name='Lifestyle')),
((SELECT isbn FROM Book WHERE title='Love You Forever'), (SELECT name FROM SearchGenre WHERE name='Picture Books')),
((SELECT isbn FROM Book WHERE title='The Sandman Vol. 1: Preludes & Nocturnes'), (SELECT name FROM SearchGenre WHERE name='Graphic Novels')),
((SELECT isbn FROM Book WHERE title='What If?: Serious Scientific Answers to Absurd Hypothetical Questions'), 
	(SELECT name FROM SearchGenre WHERE name='Humor')),
((SELECT isbn FROM Book WHERE title='The Intelligent Investor: The Definitive Book on Value Investing'), 
	(SELECT name FROM SearchGenre WHERE name='Lifestyle')),
((SELECT isbn FROM Book WHERE title='Mr. Mercedes'), (SELECT name FROM SearchGenre WHERE name='Thriller')),
((SELECT isbn FROM Book WHERE title='Proof of Heaven: A Neurosurgeons Journey into the Afterlife'), 
	(SELECT name FROM SearchGenre WHERE name='Religion')),
((SELECT isbn FROM Book WHERE title='Paperback Oxford English Dictionary'), (SELECT name FROM SearchGenre WHERE name='Dictionary'));

INSERT
INTO HasAuthor(isbn, name)
VALUES 
((SELECT isbn FROM Book WHERE title='Harry Potter and the Sorcerers Stone'), (SELECT name FROM Author WHERE name='J.K. Rowling')),
((SELECT isbn FROM Book WHERE title='Harry Potter and the Chamber of Secrets'), (SELECT name FROM Author WHERE name='J.K. Rowling')),
((SELECT isbn FROM Book WHERE title='Harry Potter and the Deathly Hallows'), (SELECT name FROM Author WHERE name='J.K. Rowling')),
((SELECT isbn FROM Book WHERE title='The Hobbit'), (SELECT name FROM Author WHERE name='J.R.R Tolkien')),
((SELECT isbn FROM Book WHERE title='Database Management Systems'), (SELECT name FROM Author WHERE name='Johannes Gehrke')),
((SELECT isbn FROM Book WHERE title='Database Management Systems'), (SELECT name FROM Author WHERE name='Raghu Ramakrishnan')),
((SELECT isbn FROM Book WHERE title='Biological Science'), (SELECT name FROM Author WHERE name='Scott Freeman')),
((SELECT isbn FROM Book WHERE title='Biological Science'), (SELECT name FROM Author WHERE name='Michael Harrington')),
((SELECT isbn FROM Book WHERE title='Biological Science'), (SELECT name FROM Author WHERE name='Joan C. Sharp')),
((SELECT isbn FROM Book WHERE title='Frankenstein'), (SELECT name FROM Author WHERE name='Mary Shelley')),
((SELECT isbn FROM Book WHERE title='1984'), (SELECT name FROM Author WHERE name='George Orwell')),
((SELECT isbn FROM Book WHERE title='Green Eggs and Ham'), (SELECT name FROM Author WHERE name='Dr. Seuss')),
((SELECT isbn FROM Book WHERE title='Gordon Ramsays Home Cooking: Everything You Need to Know to Make Fabulous Food'), 
	(SELECT name FROM Author WHERE name='Gordon Ramsay')),
((SELECT isbn FROM Book WHERE title='You Are Here: Around the World in 92 Minutes'), (SELECT name FROM Author WHERE name='Chris Hadfield')),
((SELECT isbn FROM Book WHERE title='Quiet: The Power of Introverts in a World That Cant Stop Talking'), 
	(SELECT name FROM Author WHERE name='Susan Cain')),
((SELECT isbn FROM Book WHERE title='Love You Forever'), (SELECT name FROM Author WHERE name='Robert Munsch')),
((SELECT isbn FROM Book WHERE title='The Sandman Vol. 1: Preludes & Nocturnes'), (SELECT name FROM Author WHERE name='Neil Gaiman')),
((SELECT isbn FROM Book WHERE title='What If?: Serious Scientific Answers to Absurd Hypothetical Questions'), 
	(SELECT name FROM Author WHERE name='Randall Munroe')),
((SELECT isbn FROM Book WHERE title='The Intelligent Investor: The Definitive Book on Value Investing'), 
	(SELECT name FROM Author WHERE name='Benjamin Graham')),
((SELECT isbn FROM Book WHERE title='Mr. Mercedes'), (SELECT name FROM Author WHERE name='Stephen King')),
((SELECT isbn FROM Book WHERE title='Proof of Heaven: A Neurosurgeons Journey into the Afterlife'), 
	(SELECT name FROM Author WHERE name='Eben Alexander M.D.')),
((SELECT isbn FROM Book WHERE title='Paperback Oxford English Dictionary'), (SELECT name FROM Author WHERE name='Oxford Dictionaries'));


INSERT
INTO HasPublisher(isbn, name)
VALUES 
((SELECT isbn FROM Book WHERE title='Harry Potter and the Sorcerers Stone'), (SELECT name FROM Publisher WHERE name='Scholastic')),
((SELECT isbn FROM Book WHERE title='Harry Potter and the Chamber of Secrets'), (SELECT name FROM Publisher WHERE name='Scholastic Paperbacks')),
((SELECT isbn FROM Book WHERE title='Harry Potter and the Deathly Hallows'), (SELECT name FROM Publisher WHERE name='Arthur A. Levine Books')),
((SELECT isbn FROM Book WHERE title='The Hobbit'), (SELECT name FROM Publisher WHERE name='Houghton Mifflin Harcourt')),
((SELECT isbn FROM Book WHERE title='Database Management Systems'), (SELECT name FROM Publisher WHERE name='Mcgraw Hill Ryerson Ltd')),
((SELECT isbn FROM Book WHERE title='Biological Science'), (SELECT name FROM Publisher WHERE name='Pearson Education Canada')),
((SELECT isbn FROM Book WHERE title='Frankenstein'), (SELECT name FROM Publisher WHERE name='Penguin Classics')),
((SELECT isbn FROM Book WHERE title='Frankenstein'), (SELECT name FROM Publisher WHERE name='Penguin UK')),
((SELECT isbn FROM Book WHERE title='1984'), (SELECT name FROM Publisher WHERE name='Penguin UK')),
((SELECT isbn FROM Book WHERE title='Green Eggs and Ham'), (SELECT name FROM Publisher WHERE name='Random House Books for Younger Readers')),
((SELECT isbn FROM Book WHERE title='Gordon Ramsays Home Cooking: Everything You Need to Know to Make Fabulous Food'), 
	(SELECT name FROM Publisher WHERE name='Grand Central Life & Style')),
((SELECT isbn FROM Book WHERE title='You Are Here: Around the World in 92 Minutes'), (SELECT name FROM Publisher WHERE name='Random House Canada')),
((SELECT isbn FROM Book WHERE title='Quiet: The Power of Introverts in a World That Cant Stop Talking'), 
	(SELECT name FROM Publisher WHERE name='Broadway Books')),
((SELECT isbn FROM Book WHERE title='Love You Forever'), (SELECT name FROM Publisher WHERE name='Firefly Books')),
((SELECT isbn FROM Book WHERE title='The Sandman Vol. 1: Preludes & Nocturnes'), (SELECT name FROM Publisher WHERE name='Vertigo')),
((SELECT isbn FROM Book WHERE title='What If?: Serious Scientific Answers to Absurd Hypothetical Questions'), 
	(SELECT name FROM Publisher WHERE name='Houghton Mifflin Harcourt')),
((SELECT isbn FROM Book WHERE title='The Intelligent Investor: The Definitive Book on Value Investing'), 
	(SELECT name FROM Publisher WHERE name='Harper Business')),
((SELECT isbn FROM Book WHERE title='Mr. Mercedes'), (SELECT name FROM Publisher WHERE name='Scribner')),
((SELECT isbn FROM Book WHERE title='Proof of Heaven: A Neurosurgeons Journey into the Afterlife'), 
	(SELECT name FROM Publisher WHERE name='Simon & Schuster')),
((SELECT isbn FROM Book WHERE title='Paperback Oxford English Dictionary'), (SELECT name FROM Publisher WHERE name='Oxford University Press'));


INSERT
INTO Checkout(isbn, start, end, cardNumber, idNumber)
VALUES
((SELECT isbn FROM Book WHERE title='Harry Potter and the Chamber of Secrets'), '2014-5-3 10:31:10.75', '2014-5-24 10:31:10.75',
	(SELECT cardNumber FROM Patron WHERE name='Jack Smith'), (SELECT idNumber FROM Librarian WHERE name='Nice Nora')),
((SELECT isbn FROM Book WHERE title='Biological Science'), '2014-4-30 12:45:26.36', '2014-5-6 12:45:26.36',
	(SELECT cardNumber FROM Patron WHERE name='Bill Chan'), (SELECT idNumber FROM Librarian WHERE name='Strict Shusher')),
((SELECT isbn FROM Book WHERE title='Database Management Systems'), '2014-4-15 08:21:14.28', '2014-4-22 08:21:14.28',
	(SELECT cardNumber FROM Patron WHERE name='Jack Smith'), (SELECT idNumber FROM Librarian WHERE name='Nice Nora')),
((SELECT isbn FROM Book WHERE title='The Hobbit'), '2014-5-3 12:31:11.75', '2014-5-24 12:31:11.75',
	(SELECT cardNumber FROM Patron WHERE name='Person Name'), (SELECT idNumber FROM Librarian WHERE name='Clumsy Chad')),
((SELECT isbn FROM Book WHERE title='The Hobbit'), '2014-4-5 09:21:14.28', '2014-4-26 09:21:14.28',
	(SELECT cardNumber FROM Patron WHERE name='Norman Doorman'), (SELECT idNumber FROM Librarian WHERE name='Lily Late')),
((SELECT isbn FROM Book WHERE title='Biological Science'), '2014-5-30 10:47:50.13', '2014-6-6 10:47:50.13',
	(SELECT cardNumber FROM Patron WHERE name='Jane Lambda'), (SELECT idNumber FROM Librarian WHERE name='Peter Pan')),
((SELECT isbn FROM Book WHERE title='Biological Science'), '2014-4-23 10:16:50.13', '2014-5-6 10:47:50.13',
	(SELECT cardNumber FROM Patron WHERE name='Lucy Latesworth'), (SELECT idNumber FROM Librarian WHERE name='Peter Pan')),
((SELECT isbn FROM Book WHERE title='Harry Potter and the Chamber of Secrets'), '2014-4-20 1:16:50.13', '2014-4-26 12:47:50.13',
	(SELECT cardNumber FROM Patron WHERE name='Lucy Latesworth'), (SELECT idNumber FROM Librarian WHERE name='Peter Pan')),
((SELECT isbn FROM Book WHERE title='Biological Science'), '2014-5-15 1:16:50.13', '2014-5-22 12:47:50.13',
	(SELECT cardNumber FROM Patron WHERE name='Lucy Latesworth'), (SELECT idNumber FROM Librarian WHERE name='Peter Pan')),
((SELECT isbn FROM Book WHERE title='1984'), '2014-06-15 10:44:45', '2014-06-29 10:44:45',
	(SELECT cardNumber FROM Patron WHERE name='Jane Lambda'), (SELECT idNumber FROM Librarian WHERE name='Strict Shusher')),
((SELECT isbn FROM Book WHERE title='Green Eggs and Ham'), '2014-06-15 11:24:45', '2014-06-29 11:24:45',
	(SELECT cardNumber FROM Patron WHERE name='Norman Doorman'), (SELECT idNumber FROM Librarian WHERE name='Strict Shusher')),
((SELECT isbn FROM Book WHERE title='You Are Here: Around the World in 92 Minutes'), '2014-06-11 2:24:45', '2014-06-18 2:24:45',
	(SELECT cardNumber FROM Patron WHERE name='Jack Smith'), (SELECT idNumber FROM Librarian WHERE name='Peter Pan'));


INSERT 
INTO Returns (isbn, start, end, cardNumber, checkoutId, returned, returnID)
SELECT isbn, start, end, cardNumber, idNumber, '2014-5-20 12:28:13.34', 
(SELECT idNumber FROM Librarian WHERE name='Strict Shusher') FROM Checkout where start = '2014-05-03 10:31:11';

INSERT 
INTO Returns (isbn, start, end, cardNumber, checkoutId, returned, returnID)
SELECT isbn, start, end, cardNumber, idNumber, '2014-5-6 12:44:13.34', 
(SELECT idNumber FROM Librarian WHERE name='Nice Nora') FROM Checkout where start = '2014-04-30 12:45:26';

INSERT 
INTO Returns (isbn, start, end, cardNumber, checkoutId, returned, returnID)
SELECT isbn, start, end, cardNumber, idNumber, '2014-4-20 09:12:54.23', 
(SELECT idNumber FROM Librarian WHERE name='Clumsy Chad') FROM Checkout where start = '2014-04-15 08:21:14';

INSERT 
INTO Returns (isbn, start, end, cardNumber, checkoutId, returned, returnID)
SELECT isbn, start, end, cardNumber, idNumber, '2014-5-9 11:56:43.27', 
(SELECT idNumber FROM Librarian WHERE name='Peter Pan') FROM Checkout where start = '2014-05-03 12:31:12';

INSERT 
INTO Returns (isbn, start, end, cardNumber, checkoutId, returned, returnID)
SELECT isbn, start, end, cardNumber, idNumber, '2014-4-20 12:22:12.23', 
(SELECT idNumber FROM Librarian WHERE name='Peter Pan') FROM Checkout where start = '2014-04-05 09:21:14';

INSERT 
INTO Returns (isbn, start, end, cardNumber, checkoutId, returned, returnID)
SELECT isbn, start, end, cardNumber, idNumber, '2014-5-6 12:34:13.34', 
(SELECT idNumber FROM Librarian WHERE name='Peter Pan') FROM Checkout where start = '2014-4-20 1:16:50';

INSERT 
INTO Returns (isbn, start, end, cardNumber, checkoutId, returned, returnID)
SELECT isbn, start, end, cardNumber, idNumber, '2014-5-26 12:34:13.34', 
(SELECT idNumber FROM Librarian WHERE name='Clumsy Chad') FROM Checkout where start = '2014-4-23 10:16:50.13';
