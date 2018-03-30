-- Foreign key

ALTER TABLE `chantier`.`commandes` ADD CONSTRAINT `FK_commandes_1` FOREIGN KEY `FK_commandes_1` (`Client_id`)
    REFERENCES `clients` (`Client_id`)
    ON DELETE RESTRICT
    ON UPDATE RESTRICT
, ROW_FORMAT = DYNAMIC;

ALTER TABLE `chantier`.`historique_somme` ADD CONSTRAINT `FK_historique_somme_1` FOREIGN KEY `FK_historique_somme_1` (`St_id`)
    REFERENCES `sous_traitants` (`St_id`)
    ON DELETE RESTRICT
    ON UPDATE RESTRICT,
 ADD CONSTRAINT `FK_historique_somme_2` FOREIGN KEY `FK_historique_somme_2` (`Command_id`)
    REFERENCES `commandes` (`Command_id`)
    ON DELETE RESTRICT
    ON UPDATE RESTRICT
, ROW_FORMAT = DYNAMIC;

ALTER TABLE `chantier`.`historique_heures` ADD CONSTRAINT `FK_historique_heures_1` FOREIGN KEY `FK_historique_heures_1` (`Command_id`)
    REFERENCES `commandes` (`Command_id`)
    ON DELETE RESTRICT
    ON UPDATE RESTRICT;

ALTER TABLE `chantier`.`historique_heures` 
 ADD CONSTRAINT `FK_historique_heures_2` FOREIGN KEY `FK_historique_heures_2` (`Inter_id`)
    REFERENCES `intervenants` (`Inter_id`)
    ON DELETE RESTRICT
    ON UPDATE RESTRICT
, ROW_FORMAT = DYNAMIC;

-- Primary Keys

ALTER TABLE `chantier`.`historique_heures` ADD COLUMN `historique_heures_id` MEDIUMINT(8) UNSIGNED NOT NULL AUTO_INCREMENT AFTER `Historique_date`,
 ADD PRIMARY KEY (`historique_heures_id`)
, ROW_FORMAT = DYNAMIC;

ALTER TABLE `chantier`.`historique_somme` ADD COLUMN `historique_somme_id` MEDIUMINT(8) UNSIGNED NOT NULL AUTO_INCREMENT AFTER `Historique_date`,
 ADD PRIMARY KEY (`historique_somme_id`)
, ROW_FORMAT = DYNAMIC;


ALTER TABLE `chantier`.`coefficient` ADD COLUMN `coefficient_id` MEDIUMINT(8) UNSIGNED NOT NULL AUTO_INCREMENT AFTER `Inter_coef`,
 ADD PRIMARY KEY (`coefficient_id`);

ALTER TABLE `chantier`.`commandes` MODIFY COLUMN `Command_libelle` VARCHAR(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL;

ALTER TABLE `chantier`.`intervenants` MODIFY COLUMN `inter_firstname` VARCHAR(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL;

ALTER TABLE `chantier`.`sous_traitants` MODIFY COLUMN `St_old` BOOLEAN NOT NULL DEFAULT 0;

ALTER TABLE `chantier`.`intervenants` MODIFY COLUMN `Inter_old` BOOLEAN NOT NULL DEFAULT 0;

ALTER TABLE `chantier`.`clients` MODIFY COLUMN `client_old` BOOLEAN NOT NULL DEFAULT 0;

ALTER TABLE `chantier`.`commandes` MODIFY COLUMN `finalise` BOOLEAN NOT NULL DEFAULT 0;

