SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL';

CREATE SCHEMA IF NOT EXISTS `icsil1` DEFAULT CHARACTER SET utf8 ;
USE `icsil1` ;

-- -----------------------------------------------------
-- Table `icsil1`.`ExecutedBy`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `icsil1`.`ExecutedBy` (
  `ID` INT NOT NULL AUTO_INCREMENT ,
  `Name` TEXT NOT NULL ,
  PRIMARY KEY (`ID`) ,
  UNIQUE INDEX `idExecutedBy_UNIQUE` (`ID` ASC) ,
  UNIQUE INDEX `ExecutedBycol_UNIQUE` (`Name` ASC) )
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `icsil1`.`ActivityGroup`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `icsil1`.`ActivityGroup` (
  `ID` INT NOT NULL AUTO_INCREMENT ,
  `Name` TEXT NOT NULL ,
  `Description` TEXT NOT NULL ,
  `report` TINYINT(1)  NOT NULL ,
  PRIMARY KEY (`ID`) ,
  UNIQUE INDEX `idActivityGroup_UNIQUE` (`ID` ASC) ,
  UNIQUE INDEX `ActivityGroupcol_UNIQUE` (`Name` ASC) ,
  UNIQUE INDEX `ActivityGroupcol_UNIQUE` (`Description` ASC) ,
  UNIQUE INDEX `ActivityGroupcol1_UNIQUE` (`report` ASC) )
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `icsil1`.`For`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `icsil1`.`For` (
  `ID` INT NOT NULL AUTO_INCREMENT ,
  `Name` TEXT NOT NULL ,
  `Description` TEXT NOT NULL ,
  `report` TINYINT(1)  NOT NULL ,
  PRIMARY KEY (`ID`) ,
  UNIQUE INDEX `idFor_UNIQUE` (`ID` ASC) ,
  UNIQUE INDEX `Forcol_UNIQUE` (`Name` ASC) ,
  UNIQUE INDEX `Forcol1_UNIQUE` (`Description` ASC) ,
  UNIQUE INDEX `Forcol2_UNIQUE` (`report` ASC) )
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `icsil1`.`WorkOnObject`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `icsil1`.`WorkOnObject` (
  `ID` INT NOT NULL AUTO_INCREMENT ,
  `Name` TEXT NOT NULL ,
  `Description` TEXT NOT NULL ,
  `report` TINYINT(1)  NOT NULL ,
  PRIMARY KEY (`ID`) ,
  UNIQUE INDEX `idWorkOnObject_UNIQUE` (`ID` ASC) ,
  UNIQUE INDEX `WorkOnObjectcol_UNIQUE` (`Name` ASC) ,
  UNIQUE INDEX `WorkOnObjectcol1_UNIQUE` (`Description` ASC) ,
  UNIQUE INDEX `WorkOnObjectcol2_UNIQUE` (`report` ASC) )
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `icsil1`.`Task`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `icsil1`.`Task` (
  `ID` INT NOT NULL AUTO_INCREMENT ,
  `Name` TEXT NOT NULL ,
  `Description` TEXT NOT NULL ,
  PRIMARY KEY (`ID`) ,
  UNIQUE INDEX `idTask_UNIQUE` (`ID` ASC) ,
  UNIQUE INDEX `Taskcol_UNIQUE` (`Name` ASC) ,
  UNIQUE INDEX `Taskcol1_UNIQUE` (`Description` ASC) )
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `icsil1`.`ServiceType`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `icsil1`.`ServiceType` (
  `ID` INT NOT NULL AUTO_INCREMENT ,
  `Name` TEXT NOT NULL ,
  `Description` TEXT NOT NULL ,
  PRIMARY KEY (`ID`) ,
  UNIQUE INDEX `idServiceType_UNIQUE` (`ID` ASC) ,
  UNIQUE INDEX `Name_UNIQUE` (`Name` ASC) ,
  UNIQUE INDEX `ServiceTypecol_UNIQUE` (`Description` ASC) )
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `icsil1`.`ServiceTypeCategory`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `icsil1`.`ServiceTypeCategory` (
  `ID` INT NOT NULL AUTO_INCREMENT ,
  `Name` TEXT NOT NULL ,
  `Description` TEXT NOT NULL ,
  `Type` INT NOT NULL ,
  PRIMARY KEY (`ID`) ,
  UNIQUE INDEX `idServiceTypeCategory_UNIQUE` (`ID` ASC) ,
  UNIQUE INDEX `Name_UNIQUE` (`Name` ASC) ,
  UNIQUE INDEX `ServiceTypeCategorycol_UNIQUE` (`Description` ASC) ,
  INDEX `fk_ServiceTypeCategory_ServiceType` (`Type` ASC) ,
  CONSTRAINT `fk_ServiceTypeCategory_ServiceType`
    FOREIGN KEY (`Type` )
    REFERENCES `icsil1`.`ServiceType` (`ID` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `icsil1`.`Service`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `icsil1`.`Service` (
  `ID` INT NOT NULL AUTO_INCREMENT ,
  `Name` TEXT NOT NULL ,
  `Description` TEXT NOT NULL ,
  `Category` INT NOT NULL ,
  PRIMARY KEY (`ID`) ,
  UNIQUE INDEX `idService_UNIQUE` (`ID` ASC) ,
  UNIQUE INDEX `Servicecol_UNIQUE` (`Name` ASC) ,
  UNIQUE INDEX `Servicecol1_UNIQUE` (`Description` ASC) ,
  INDEX `fk_Service_ServiceTypeCategory1` (`Category` ASC) ,
  CONSTRAINT `fk_Service_ServiceTypeCategory1`
    FOREIGN KEY (`Category` )
    REFERENCES `icsil1`.`ServiceTypeCategory` (`ID` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `icsil1`.`Day`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `icsil1`.`Day` (
  `ID` INT NOT NULL AUTO_INCREMENT ,
  `Day` TEXT NOT NULL ,
  PRIMARY KEY (`ID`) ,
  UNIQUE INDEX `idDay_UNIQUE` (`ID` ASC) ,
  UNIQUE INDEX `Day_UNIQUE` (`Day` ASC) )
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `icsil1`.`ActivityInfo`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `icsil1`.`ActivityInfo` (
  `ID` INT NOT NULL AUTO_INCREMENT ,
  `Name` TEXT NOT NULL ,
  `Description` TEXT NOT NULL ,
  `Issue` TEXT NULL ,
  PRIMARY KEY (`ID`) ,
  UNIQUE INDEX `ID_UNIQUE` (`ID` ASC) )
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `icsil1`.`Activity`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `icsil1`.`Activity` (
  `ID` INT NOT NULL AUTO_INCREMENT ,
  `ExecutedBy` INT NOT NULL ,
  `Week` INT NOT NULL ,
  `Year` INT NOT NULL ,
  `Duration` INT NOT NULL ,
  `ActivityGroup` INT NOT NULL ,
  `WorkOnObject` INT NOT NULL ,
  `Task` INT NOT NULL ,
  `Service` INT NOT NULL ,
  `For` INT NOT NULL ,
  `Day` INT NOT NULL ,
  `Checked` TINYINT(1)  NOT NULL ,
  `Info` INT NOT NULL ,
  PRIMARY KEY (`ID`) ,
  UNIQUE INDEX `idActivity_UNIQUE` (`ID` ASC) ,
  INDEX `fk_Activity_ExecutedBy1` (`ExecutedBy` ASC) ,
  INDEX `fk_Activity_ActivityGroup1` (`ActivityGroup` ASC) ,
  INDEX `fk_Activity_WorkOnObject1` (`WorkOnObject` ASC) ,
  INDEX `fk_Activity_Task1` (`Task` ASC) ,
  INDEX `fk_Activity_Service1` (`Service` ASC) ,
  INDEX `fk_Activity_For1` (`For` ASC) ,
  INDEX `fk_Activity_Day1` (`Day` ASC) ,
  INDEX `fk_Activity_ActivityInfo1` (`Info` ASC) ,
  CONSTRAINT `fk_Activity_ExecutedBy1`
    FOREIGN KEY (`ExecutedBy` )
    REFERENCES `icsil1`.`ExecutedBy` (`ID` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Activity_ActivityGroup1`
    FOREIGN KEY (`ActivityGroup` )
    REFERENCES `icsil1`.`ActivityGroup` (`ID` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Activity_WorkOnObject1`
    FOREIGN KEY (`WorkOnObject` )
    REFERENCES `icsil1`.`WorkOnObject` (`ID` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Activity_Task1`
    FOREIGN KEY (`Task` )
    REFERENCES `icsil1`.`Task` (`ID` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Activity_Service1`
    FOREIGN KEY (`Service` )
    REFERENCES `icsil1`.`Service` (`ID` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Activity_For1`
    FOREIGN KEY (`For` )
    REFERENCES `icsil1`.`For` (`ID` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Activity_Day1`
    FOREIGN KEY (`Day` )
    REFERENCES `icsil1`.`Day` (`ID` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Activity_ActivityInfo1`
    FOREIGN KEY (`Info` )
    REFERENCES `icsil1`.`ActivityInfo` (`ID` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `icsil1`.`Week`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `icsil1`.`Week` (
  `ID` INT NOT NULL AUTO_INCREMENT ,
  `Week` INT NOT NULL ,
  `Year` INT NOT NULL ,
  `HourToWork` INT NOT NULL ,
  PRIMARY KEY (`ID`) ,
  UNIQUE INDEX `idtable1_UNIQUE` (`ID` ASC) )
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `icsil1`.`Horaire`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `icsil1`.`Horaire` (
  `ID` INT NOT NULL AUTO_INCREMENT ,
  `ExecutedBy` INT NOT NULL ,
  `Year` INT NOT NULL ,
  `Week` INT NOT NULL ,
  `Day` INT NOT NULL ,
  `Hour` INT NOT NULL ,
  PRIMARY KEY (`ID`) ,
  UNIQUE INDEX `idLog_UNIQUE` (`ID` ASC) ,
  INDEX `fk_Log_ExecutedBy1` (`ExecutedBy` ASC) ,
  INDEX `fk_Log_Week1` (`Week` ASC) ,
  INDEX `fk_Log_Day1` (`Day` ASC) ,
  CONSTRAINT `fk_Log_ExecutedBy1`
    FOREIGN KEY (`ExecutedBy` )
    REFERENCES `icsil1`.`ExecutedBy` (`ID` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Log_Week1`
    FOREIGN KEY (`Week` )
    REFERENCES `icsil1`.`Week` (`ID` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Log_Day1`
    FOREIGN KEY (`Day` )
    REFERENCES `icsil1`.`Day` (`ID` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `icsil1`.`TypeAbsence`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `icsil1`.`TypeAbsence` (
  `ID` INT NOT NULL AUTO_INCREMENT ,
  `Motif` TEXT NOT NULL ,
  PRIMARY KEY (`ID`) ,
  UNIQUE INDEX `idtable1_UNIQUE` (`ID` ASC) ,
  UNIQUE INDEX `Motif_UNIQUE` (`Motif` ASC) )
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `icsil1`.`Absence`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `icsil1`.`Absence` (
  `ID` INT NOT NULL AUTO_INCREMENT ,
  `ExecutedBy` INT NOT NULL ,
  `Week` INT NOT NULL ,
  `TypeAbsence` INT NOT NULL ,
  `Day` INT NOT NULL ,
  `Hour` INT NOT NULL ,
  PRIMARY KEY (`ID`) ,
  UNIQUE INDEX `idtable1_UNIQUE` (`ID` ASC) ,
  INDEX `fk_Absence_ExecutedBy1` (`ExecutedBy` ASC) ,
  INDEX `fk_Absence_Week1` (`Week` ASC) ,
  INDEX `fk_Absence_TypeAbsence1` (`TypeAbsence` ASC) ,
  INDEX `fk_Absence_Day1` (`Day` ASC) ,
  CONSTRAINT `fk_Absence_ExecutedBy1`
    FOREIGN KEY (`ExecutedBy` )
    REFERENCES `icsil1`.`ExecutedBy` (`ID` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Absence_Week1`
    FOREIGN KEY (`Week` )
    REFERENCES `icsil1`.`Week` (`ID` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Absence_TypeAbsence1`
    FOREIGN KEY (`TypeAbsence` )
    REFERENCES `icsil1`.`TypeAbsence` (`ID` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Absence_Day1`
    FOREIGN KEY (`Day` )
    REFERENCES `icsil1`.`Day` (`ID` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `icsil1`.`Vacances`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `icsil1`.`Vacances` (
  `ID` INT NOT NULL AUTO_INCREMENT ,
  `ExecutedBy` INT NOT NULL ,
  `Date` TEXT NOT NULL ,
  `Heures` INT NOT NULL ,
  `JoursVacances` INT NOT NULL ,
  PRIMARY KEY (`ID`) ,
  UNIQUE INDEX `idVacances_UNIQUE` (`ID` ASC) ,
  INDEX `fk_Vacances_ExecutedBy1` (`ExecutedBy` ASC) ,
  CONSTRAINT `fk_Vacances_ExecutedBy1`
    FOREIGN KEY (`ExecutedBy` )
    REFERENCES `icsil1`.`ExecutedBy` (`ID` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;



SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
