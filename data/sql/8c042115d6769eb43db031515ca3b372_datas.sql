
DROP TABLE Algorithm;
CREATE TABLE Algorithm (
	IDAlgo SERIAL NOT NULL,
	Name VARCHAR,
	Domain_ VARCHAR 	CONSTRAINT check_domain
				CHECK (Domain_ IN ('NUM','GRA', 'DON', 'COD', 'GEO')),
     	Type_ VARCHAR 		CONSTRAINT check_type
				CHECK (Type_ IN ('ITR','REC')),
	Level_ INT		CONSTRAINT check_level
				CHECK (Level_ BETWEEN 0 AND 3),
	Description VARCHAR,
	CodeC VARCHAR,
	FunctionHeader VARCHAR,
	AlgoProposition TEXT,
	News BOOL,
	CONSTRAINT p_Algorithm PRIMARY KEY (IDAlgo)
);

DROP TABLE Teacher;
CREATE TABLE Teacher (
	Login VARCHAR NOT NULL,
	Name VARCHAR,
	CONSTRAINT p_Teacher PRIMARY KEY (Login)
);

DROP TABLE Student;
CREATE TABLE Student (
	Login VARCHAR NOT NULL,
	Name VARCHAR,
	CONSTRAINT p_Student PRIMARY KEY (Login)
);

DROP TABLE Step;
CREATE TABLE Step (
	Step VARCHAR NOT NULL	CONSTRAINT check_step
				CHECK (Step IN ('L1','L2', 'L3', 'M1', 'M2')),
	CONSTRAINT p_Step PRIMARY KEY (Step)
);

DROP TABLE StudiesIn;
CREATE TABLE StudiesIn (
	Student VARCHAR NOT NULL,
	Step VARCHAR NOT NULL	CONSTRAINT check_step
				CHECK (Step IN ('L1','L2', 'L3', 'M1', 'M2')),
	Graduation INT,
	CONSTRAINT p_StudiesIn PRIMARY KEY (Step, Student),
	CONSTRAINT f_sd FOREIGN KEY (Student) REFERENCES Student(Login),
	CONSTRAINT f_sp FOREIGN KEY (Step) REFERENCES Step(Step)
);

DROP TABLE IsRelatedToProgram;
CREATE TABLE IsRelatedToProgram (
	Algorithm INT NOT NULL,
	Step VARCHAR NOT NULL	CONSTRAINT check_step
				CHECK (Step IN ('L1','L2', 'L3', 'M1', 'M2')),
-- m,a,d stands for mandatory, advised, dummy
	Set_ VARCHAR NOT NULL	CONSTRAINT check_set
				CHECK (Set_ IN ('m','a', 'd')),
	CONSTRAINT p_irtp PRIMARY KEY (Algorithm, Step),
	CONSTRAINT f_sp FOREIGN KEY (Step) REFERENCES Step(Step),
	CONSTRAINT f_a FOREIGN KEY (Algorithm) REFERENCES Algorithm(IDAlgo)
);

DROP TABLE StudentKnowledge;
CREATE TABLE StudentKnowledge (
	Student VARCHAR NOT NULL,
	Algorithm INT NOT NULL,
	isLearnt VARCHAR NOT NULL	CONSTRAINT check_islearnt
				CHECK (islearnt IN ('l', 't')),
-- l and t stands for "learnt" and "to learn"
	CONSTRAINT p_sk PRIMARY KEY (Algorithm, Student),
	CONSTRAINT f_sd FOREIGN KEY (Student) REFERENCES Student(login),
	CONSTRAINT f_a FOREIGN KEY (Algorithm) REFERENCES Algorithm(IDAlgo)
);

DROP TRIGGER tc1;
CREATE TRIGGER tc1 