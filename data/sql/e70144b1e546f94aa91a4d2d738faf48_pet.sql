SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL';


-- -----------------------------------------------------
-- Table `download_formats`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `download_formats` ;

CREATE  TABLE IF NOT EXISTS `download_formats` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `name` VARCHAR(100) NOT NULL ,
  `mimetype` VARCHAR(50) NOT NULL ,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `id_UNIQUE` (`id` ASC) )
ENGINE = InnoDB
AUTO_INCREMENT = 3
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_general_ci;


-- -----------------------------------------------------
-- Table `product_types`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `product_types` ;

CREATE  TABLE IF NOT EXISTS `product_types` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `name` VARCHAR(255) NOT NULL ,
  `type` VARCHAR(50) NOT NULL ,
  `plural_name` VARCHAR(255) NOT NULL ,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `id` (`id` ASC) )
ENGINE = InnoDB
AUTO_INCREMENT = 5
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_general_ci;


-- -----------------------------------------------------
-- Table `products`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `products` ;

CREATE  TABLE IF NOT EXISTS `products` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `product_type_id` INT(11) NOT NULL ,
  `sku` VARCHAR(30) NOT NULL ,
  `cost` DECIMAL(5,2) NOT NULL DEFAULT 0 ,
  `image` VARCHAR(100) NULL DEFAULT NULL ,
  `active` INT(1) NOT NULL DEFAULT 1 ,
  `max_qty` INT(2) NULL ,
  `is_giftable` INT(1) NOT NULL DEFAULT 0 ,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `id` (`id` ASC) ,
  UNIQUE INDEX `sku` (`sku` ASC) ,
  INDEX `product_type_id` (`product_type_id` ASC) ,
  CONSTRAINT `products_ibfk_1`
    FOREIGN KEY (`product_type_id` )
    REFERENCES `product_types` (`id` ))
ENGINE = InnoDB
AUTO_INCREMENT = 102
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_general_ci;


-- -----------------------------------------------------
-- Table `downloads`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `downloads` ;

CREATE  TABLE IF NOT EXISTS `downloads` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `product_id` INT(11) NOT NULL ,
  `download_format_id` INT(11) NOT NULL ,
  `name` VARCHAR(200) NOT NULL ,
  `description` TEXT NULL DEFAULT NULL ,
  `date` DATE NOT NULL ,
  `path` VARCHAR(250) NOT NULL ,
  `size` VARCHAR(50) NOT NULL ,
  `thumb` VARCHAR(100) NULL DEFAULT NULL ,
  `subscriber_only` TINYINT(1) NOT NULL ,
  PRIMARY KEY (`id`) ,
  INDEX `downloads_ibfk_1` (`download_format_id` ASC) ,
  UNIQUE INDEX `product_id_UNIQUE` (`product_id` ASC) ,
  INDEX `downloads_ibfk_2` (`product_id` ASC) ,
  CONSTRAINT `downloads_ibfk_1`
    FOREIGN KEY (`download_format_id` )
    REFERENCES `download_formats` (`id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `downloads_ibfk_2`
    FOREIGN KEY (`product_id` )
    REFERENCES `products` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB
AUTO_INCREMENT = 54
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_general_ci;


-- -----------------------------------------------------
-- Table `shipping_zones`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `shipping_zones` ;

CREATE  TABLE IF NOT EXISTS `shipping_zones` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `usa` DECIMAL(5,2) NOT NULL DEFAULT '0.00' ,
  `can` DECIMAL(5,2) NOT NULL DEFAULT '0.00' ,
  `intl` DECIMAL(5,2) NOT NULL DEFAULT '0.00' ,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `id` (`id` ASC) )
ENGINE = InnoDB
AUTO_INCREMENT = 4
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_general_ci;


-- -----------------------------------------------------
-- Table `subscription_zones`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `subscription_zones` ;

CREATE  TABLE IF NOT EXISTS `subscription_zones` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `name` VARCHAR(100) NOT NULL ,
  `zone` VARCHAR(10) NOT NULL ,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `id` (`id` ASC) )
ENGINE = InnoDB
AUTO_INCREMENT = 4
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_general_ci;


-- -----------------------------------------------------
-- Table `promos`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `promos` ;

CREATE  TABLE IF NOT EXISTS `promos` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `code` VARCHAR(20) NOT NULL ,
  `expiration` DATE NOT NULL ,
  `description` VARCHAR(200) NULL DEFAULT NULL ,
  `public_description` LONGTEXT NULL DEFAULT NULL ,
  `receipt_description` LONGTEXT NULL DEFAULT NULL ,
  `banner` VARCHAR(100) NULL ,
  `discount` DECIMAL(5,2) NOT NULL DEFAULT 0 ,
  `extra_days` INT(11) NOT NULL ,
  `uses` INT(11) NOT NULL DEFAULT 0 ,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `code_UNIQUE` (`code` ASC) )
ENGINE = InnoDB
AUTO_INCREMENT = 119
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_general_ci;


-- -----------------------------------------------------
-- Table `users`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `users` ;

CREATE  TABLE IF NOT EXISTS `users` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `username` VARCHAR(30) NULL ,
  `first_name` VARCHAR(50) NOT NULL ,
  `last_name` VARCHAR(50) NOT NULL ,
  `email` VARCHAR(75) NULL DEFAULT NULL ,
  `password` VARCHAR(128) NULL ,
  `is_staff` TINYINT(4) NOT NULL DEFAULT '0' ,
  `is_active` TINYINT(4) NOT NULL DEFAULT '1' ,
  `is_superuser` TINYINT(4) NOT NULL DEFAULT '0' ,
  `last_login` DATETIME NULL ,
  `date_joined` DATETIME NOT NULL ,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `id_UNIQUE` (`id` ASC) ,
  UNIQUE INDEX `username_UNIQUE` (`username` ASC) ,
  UNIQUE INDEX `email` (`email` ASC) ,
  INDEX `index3` (`username` ASC) ,
  INDEX `index4` (`email` ASC) )
ENGINE = InnoDB
AUTO_INCREMENT = 87701000
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_general_ci;


-- -----------------------------------------------------
-- Table `orders`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `orders` ;

CREATE  TABLE IF NOT EXISTS `orders` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `user_id` INT(11) NULL DEFAULT NULL ,
  `promo_id` INT(11) NULL DEFAULT NULL ,
  `date_created` DATETIME NOT NULL ,
  `date_updated` DATETIME NOT NULL ,
  `email` VARCHAR(75) NOT NULL ,
  `billing_first_name` VARCHAR(100) NOT NULL ,
  `billing_last_name` VARCHAR(100) NOT NULL ,
  `billing_address` VARCHAR(100) NOT NULL ,
  `billing_address_2` VARCHAR(100) NOT NULL ,
  `billing_company` VARCHAR(100) NOT NULL ,
  `billing_city` VARCHAR(100) NOT NULL ,
  `billing_country` VARCHAR(100) NOT NULL ,
  `billing_state` VARCHAR(100) NOT NULL ,
  `billing_postal_code` VARCHAR(100) NOT NULL ,
  `billing_phone` VARCHAR(100) NOT NULL ,
  `shipping_first_name` VARCHAR(100) NOT NULL ,
  `shipping_last_name` VARCHAR(100) NOT NULL ,
  `shipping_address` VARCHAR(100) NOT NULL COMMENT '	' ,
  `shipping_address_2` VARCHAR(100) NOT NULL ,
  `shipping_company` VARCHAR(100) NOT NULL ,
  `shipping_city` VARCHAR(100) NOT NULL ,
  `shipping_state` VARCHAR(100) NOT NULL ,
  `shipping_postal_code` VARCHAR(100) NOT NULL ,
  `shipping_country` VARCHAR(100) NOT NULL ,
  `shipping_phone` VARCHAR(50) NOT NULL ,
  `shipping` DECIMAL(5,2) NOT NULL DEFAULT '0.00' ,
  `discount` DECIMAL(5,2) NOT NULL DEFAULT 0 ,
  `total` DECIMAL(5,2) NOT NULL DEFAULT '0.00' ,
  `phone_order` TINYINT(1) NOT NULL ,
  `active` INT(1) NOT NULL DEFAULT '1' ,
  `email_sent` INT(1) NOT NULL DEFAULT 0 ,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `id` (`id` ASC) ,
  INDEX `promo_id` (`promo_id` ASC) ,
  INDEX `orders_ibfk_2` (`promo_id` ASC) ,
  INDEX `orders_ibfk_3` (`user_id` ASC) ,
  INDEX `email_sent` (`email_sent` ASC) ,
  INDEX `date_created` (`date_created` ASC) ,
  INDEX `email` (`email` ASC) ,
  INDEX `billing_first_name` (`billing_first_name` ASC) ,
  INDEX `billing_last_name` (`billing_last_name` ASC) ,
  CONSTRAINT `orders_ibfk_2`
    FOREIGN KEY (`promo_id` )
    REFERENCES `promos` (`id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `orders_ibfk_3`
    FOREIGN KEY (`user_id` )
    REFERENCES `users` (`id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB
AUTO_INCREMENT = 1128415
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_general_ci;


-- -----------------------------------------------------
-- Table `order_products`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `order_products` ;

CREATE  TABLE IF NOT EXISTS `order_products` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `order_id` INT(11) NOT NULL ,
  `product_id` INT(11) NOT NULL ,
  `qty` INT(3) NOT NULL DEFAULT '0' ,
  `cost` DECIMAL(5,2) NOT NULL DEFAULT '0.00' ,
  `discount` DECIMAL(5,2) NOT NULL DEFAULT '0.00' ,
  `is_gift` VARCHAR(100) NOT NULL DEFAULT 0 ,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `id` (`id` ASC) ,
  INDEX `order_id` (`order_id` ASC) ,
  INDEX `product_id` (`product_id` ASC) ,
  CONSTRAINT `order_products_ibfk_1`
    FOREIGN KEY (`order_id` )
    REFERENCES `orders` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `order_products_ibfk_2`
    FOREIGN KEY (`product_id` )
    REFERENCES `products` (`id` )
    ON DELETE RESTRICT
    ON UPDATE RESTRICT)
ENGINE = InnoDB
AUTO_INCREMENT = 131071
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_general_ci;


-- -----------------------------------------------------
-- Table `subscriptions`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `subscriptions` ;

CREATE  TABLE IF NOT EXISTS `subscriptions` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `product_id` INT(11) NOT NULL ,
  `zone_id` INT(11) NOT NULL ,
  `name` VARCHAR(100) NOT NULL ,
  `description` TEXT NULL DEFAULT NULL ,
  `term_months` INT(11) NOT NULL DEFAULT '1' ,
  `is_renewal` INT(1) NOT NULL DEFAULT '0' ,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `id` (`id` ASC) ,
  INDEX `zone_id` (`zone_id` ASC) ,
  UNIQUE INDEX `product_id_UNIQUE` (`product_id` ASC) ,
  INDEX `subscription_products_ibfk_1` (`product_id` ASC) ,
  CONSTRAINT `subscription_products_ibfk_3`
    FOREIGN KEY (`zone_id` )
    REFERENCES `subscription_zones` (`id` ),
  CONSTRAINT `subscription_products_ibfk_1`
    FOREIGN KEY (`product_id` )
    REFERENCES `products` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB
AUTO_INCREMENT = 16
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_general_ci;


-- -----------------------------------------------------
-- Table `courses`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `courses` ;

CREATE  TABLE IF NOT EXISTS `courses` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `product_id` INT(11) NOT NULL ,
  `name` VARCHAR(100) NOT NULL ,
  `description` TEXT NULL DEFAULT NULL ,
  `slug` VARCHAR(200) NOT NULL ,
  `active` TINYINT(4) NOT NULL DEFAULT '0' ,
  `free` TINYINT(4) NOT NULL DEFAULT '0' ,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `id` (`id` ASC) ,
  UNIQUE INDEX `product_id_UNIQUE` (`product_id` ASC) ,
  INDEX `courses_ibfk_1` (`product_id` ASC) ,
  CONSTRAINT `courses_ibfk_1`
    FOREIGN KEY (`product_id` )
    REFERENCES `products` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB
AUTO_INCREMENT = 3
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_general_ci;


-- -----------------------------------------------------
-- Table `physical_products`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `physical_products` ;

CREATE  TABLE IF NOT EXISTS `physical_products` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `product_id` INT(11) NOT NULL ,
  `shipping_zone_id` INT(11) NOT NULL ,
  `name` VARCHAR(100) NOT NULL ,
  `description` TEXT NULL DEFAULT NULL ,
  `sequence` INT(5) NOT NULL DEFAULT 0 ,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `id` (`id` ASC) ,
  UNIQUE INDEX `product_id_UNIQUE` (`product_id` ASC) ,
  INDEX `product_id` (`product_id` ASC) ,
  INDEX `shipping_id` (`shipping_zone_id` ASC) ,
  CONSTRAINT `physical_products_ibfk_1`
    FOREIGN KEY (`product_id` )
    REFERENCES `products` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `physical_products_ibfk_2`
    FOREIGN KEY (`shipping_zone_id` )
    REFERENCES `shipping_zones` (`id` ))
ENGINE = InnoDB
AUTO_INCREMENT = 16
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_general_ci;


-- -----------------------------------------------------
-- Table `promo_products`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `promo_products` ;

CREATE  TABLE IF NOT EXISTS `promo_products` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `promo_id` INT(11) NOT NULL ,
  `product_id` INT(11) NOT NULL ,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `id_UNIQUE` (`id` ASC) ,
  INDEX `promo_id` (`promo_id` ASC) ,
  INDEX `product_id` (`product_id` ASC) ,
  INDEX `promo_id_ibfk_1` (`promo_id` ASC) ,
  INDEX `product_id_ibfk_1` (`product_id` ASC) ,
  CONSTRAINT `promo_id_ibfk_1`
    FOREIGN KEY (`promo_id` )
    REFERENCES `promos` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `product_id_ibfk_1`
    FOREIGN KEY (`product_id` )
    REFERENCES `products` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB
AUTO_INCREMENT = 1024
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_general_ci;


-- -----------------------------------------------------
-- Table `payment_types`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `payment_types` ;

CREATE  TABLE IF NOT EXISTS `payment_types` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `name` VARCHAR(50) NOT NULL ,
  `table_name` VARCHAR(30) NOT NULL ,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `id_UNIQUE` (`id` ASC) )
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_general_ci;


-- -----------------------------------------------------
-- Table `order_payments`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `order_payments` ;

CREATE  TABLE IF NOT EXISTS `order_payments` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `order_id` INT(11) NOT NULL ,
  `payment_type_id` INT(11) NOT NULL ,
  `amount` DECIMAL(5,2) NOT NULL DEFAULT '0.00' ,
  `date` DATETIME NULL DEFAULT NULL ,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `id_UNIQUE` (`id` ASC) ,
  INDEX `order_id` (`order_id` ASC) ,
  INDEX `order_payments_ibfk_1` (`order_id` ASC) ,
  INDEX `order_payments_ibfk_2` (`payment_type_id` ASC) ,
  CONSTRAINT `order_payments_ibfk_1`
    FOREIGN KEY (`order_id` )
    REFERENCES `orders` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `order_payments_ibfk_2`
    FOREIGN KEY (`payment_type_id` )
    REFERENCES `payment_types` (`id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_general_ci;


-- -----------------------------------------------------
-- Table `order_payments_payflow`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `order_payments_payflow` ;

CREATE  TABLE IF NOT EXISTS `order_payments_payflow` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `order_payment_id` INT(11) NOT NULL ,
  `cc_number` VARCHAR(16) NULL ,
  `cc_expiration_month` INT(2) NULL ,
  `cc_expiration_year` INT(2) NULL ,
  `pnref` VARCHAR(12) NOT NULL ,
  `ppref` VARCHAR(17) NOT NULL ,
  `correlationid` VARCHAR(13) NOT NULL ,
  `cvv2match` VARCHAR(1) NULL ,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `id_UNIQUE` (`id` ASC) ,
  INDEX `order_payments_payflow_ibfk_1` (`order_payment_id` ASC) ,
  UNIQUE INDEX `order_payment_id_UNIQUE` (`order_payment_id` ASC) ,
  CONSTRAINT `order_payments_payflow_ibfk_1`
    FOREIGN KEY (`order_payment_id` )
    REFERENCES `order_payments` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_general_ci;


-- -----------------------------------------------------
-- Table `order_payments_paypal`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `order_payments_paypal` ;

CREATE  TABLE IF NOT EXISTS `order_payments_paypal` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `order_payment_id` INT(11) NOT NULL ,
  `pnref` VARCHAR(20) NULL ,
  `correlationid` VARCHAR(20) NOT NULL ,
  `baid` VARCHAR(20) NULL ,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `id_UNIQUE` (`id` ASC) ,
  INDEX `order_payments_paypal_ibfk_2` (`order_payment_id` ASC) ,
  UNIQUE INDEX `order_payment_id_UNIQUE` (`order_payment_id` ASC) ,
  CONSTRAINT `order_payments_paypal_ibfk_2`
    FOREIGN KEY (`order_payment_id` )
    REFERENCES `order_payments` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_general_ci;


-- -----------------------------------------------------
-- Table `user_profiles`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `user_profiles` ;

CREATE  TABLE IF NOT EXISTS `user_profiles` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `user_id` INT(11) NOT NULL ,
  `billing_address` VARCHAR(128) NOT NULL ,
  `billing_address_2` VARCHAR(50) NOT NULL ,
  `billing_company` VARCHAR(100) NOT NULL ,
  `billing_city` VARCHAR(50) NOT NULL ,
  `billing_state` VARCHAR(30) NOT NULL ,
  `billing_postal_code` VARCHAR(30) NOT NULL ,
  `billing_country` VARCHAR(50) NOT NULL ,
  `billing_phone` VARCHAR(50) NOT NULL ,
  `shipping_first_name` VARCHAR(50) NOT NULL COMMENT '		' ,
  `shipping_last_name` VARCHAR(50) NOT NULL ,
  `shipping_address` VARCHAR(128) NOT NULL ,
  `shipping_address_2` VARCHAR(50) NOT NULL ,
  `shipping_company` VARCHAR(100) NOT NULL ,
  `shipping_city` VARCHAR(50) NOT NULL ,
  `shipping_state` VARCHAR(30) NOT NULL ,
  `shipping_postal_code` VARCHAR(30) NOT NULL ,
  `shipping_country` VARCHAR(50) NOT NULL ,
  `shipping_phone` VARCHAR(50) NOT NULL ,
  `marketing` VARCHAR(100) NULL ,
  `occupation` VARCHAR(100) NULL ,
  `opt_in` TINYINT(1) NOT NULL DEFAULT '0' ,
  `opt_in_partner` TINYINT(1) NOT NULL DEFAULT '0' ,
  `opt_in_subscriber` TINYINT(1) NOT NULL DEFAULT '0' ,
  `comp` TINYINT(1) NOT NULL DEFAULT '0' ,
  `version` VARCHAR(100) NULL ,
  `platform` VARCHAR(100) NULL ,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `user_id_UNIQUE` (`user_id` ASC) ,
  UNIQUE INDEX `id_UNIQUE` (`id` ASC) ,
  INDEX `user_profiles_ibfk_1` (`user_id` ASC) ,
  INDEX `shipping_country` (`shipping_country` ASC) ,
  CONSTRAINT `user_profiles_ibfk_1`
    FOREIGN KEY (`user_id` )
    REFERENCES `users` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB
AUTO_INCREMENT = 87701000
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_general_ci;


-- -----------------------------------------------------
-- Table `order_payments_check`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `order_payments_check` ;

CREATE  TABLE IF NOT EXISTS `order_payments_check` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `order_payment_id` INT NOT NULL ,
  `check_number` VARCHAR(50) NOT NULL ,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `id_UNIQUE` (`id` ASC) ,
  UNIQUE INDEX `order_payment_id_UNIQUE` (`order_payment_id` ASC) ,
  INDEX `order_payments_check_ibfk_1` (`order_payment_id` ASC) ,
  CONSTRAINT `order_payments_check_ibfk_1`
    FOREIGN KEY (`order_payment_id` )
    REFERENCES `order_payments` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `user_password_tokens`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `user_password_tokens` ;

CREATE  TABLE IF NOT EXISTS `user_password_tokens` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `user_id` INT NOT NULL ,
  `token` VARCHAR(100) NOT NULL ,
  `timestamp` DATETIME NOT NULL ,
  `attempts` TINYINT NOT NULL DEFAULT 1 ,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `id_UNIQUE` (`id` ASC) ,
  INDEX `user_password_resets_fk_1` (`user_id` ASC) ,
  UNIQUE INDEX `token_UNIQUE` (`token` ASC) ,
  CONSTRAINT `user_password_resets_fk_1`
    FOREIGN KEY (`user_id` )
    REFERENCES `users` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `digital_subscriptions`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `digital_subscriptions` ;

CREATE  TABLE IF NOT EXISTS `digital_subscriptions` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `product_id` INT(11) NOT NULL ,
  `name` VARCHAR(100) NOT NULL ,
  `description` TEXT NULL DEFAULT NULL ,
  `is_renewal` INT(1) NOT NULL DEFAULT '0' ,
  `is_recurring` TINYINT(1) NOT NULL DEFAULT 0 ,
  `term_months` INT(4) NULL COMMENT '					' ,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `id` (`id` ASC) ,
  UNIQUE INDEX `product_id_UNIQUE` (`product_id` ASC) ,
  INDEX `digital_subscriptions_ibfk_1` (`product_id` ASC) ,
  CONSTRAINT `digital_subscriptions_ibfk_1`
    FOREIGN KEY (`product_id` )
    REFERENCES `products` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB
AUTO_INCREMENT = 16
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_general_ci;


-- -----------------------------------------------------
-- Table `order_product_subscriptions`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `order_product_subscriptions` ;

CREATE  TABLE IF NOT EXISTS `order_product_subscriptions` (
  `id` INT(11) NOT NULL AUTO_INCREMENT ,
  `user_id` INT NOT NULL ,
  `order_product_id` INT(11) NULL ,
  `expiration` DATE NOT NULL ,
  `digital_only` INT(1) NOT NULL DEFAULT 0 ,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `id_UNIQUE` (`id` ASC) ,
  INDEX `order_product_subscriptions_ibfk_1` (`user_id` ASC) ,
  INDEX `order_product_subscriptions_ibfk_2` (`order_product_id` ASC) ,
  INDEX `expiration` (`expiration` ASC) ,
  INDEX `user_id` (`user_id` ASC) ,
  CONSTRAINT `order_product_subscriptions_ibfk_1`
    FOREIGN KEY (`user_id` )
    REFERENCES `users` (`id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `order_product_subscriptions_ibfk_2`
    FOREIGN KEY (`order_product_id` )
    REFERENCES `order_products` (`id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB
AUTO_INCREMENT = 98303
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_general_ci;


-- -----------------------------------------------------
-- Table `order_product_gifts`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `order_product_gifts` ;

CREATE  TABLE IF NOT EXISTS `order_product_gifts` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `order_product_id` INT NOT NULL ,
  `token` VARCHAR(100) NOT NULL ,
  `redeem_date` DATETIME NULL ,
  `redeemer_order_product_id` INT NULL ,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `id_UNIQUE` (`id` ASC) ,
  INDEX `order_product_gifts_fk_1` (`order_product_id` ASC) ,
  INDEX `order_product_gifts_fk_2` (`redeemer_order_product_id` ASC) ,
  INDEX `token` (`token` ASC) ,
  CONSTRAINT `order_product_gifts_fk_1`
    FOREIGN KEY (`order_product_id` )
    REFERENCES `order_products` (`id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `order_product_gifts_fk_2`
    FOREIGN KEY (`redeemer_order_product_id` )
    REFERENCES `order_products` (`id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `user_notes`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `user_notes` ;

CREATE  TABLE IF NOT EXISTS `user_notes` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `user_id` INT NOT NULL ,
  `rep_user_id` INT NOT NULL ,
  `note` VARCHAR(255) NOT NULL ,
  `date_created` DATETIME NOT NULL ,
  PRIMARY KEY (`id`) ,
  INDEX `user_notes_fk_2` (`user_id` ASC) ,
  INDEX `user_notes_fk_3` (`rep_user_id` ASC) ,
  UNIQUE INDEX `id_UNIQUE` (`id` ASC) ,
  INDEX `date_created` (`date_created` ASC) ,
  CONSTRAINT `user_notes_fk_2`
    FOREIGN KEY (`user_id` )
    REFERENCES `users` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `user_notes_fk_3`
    FOREIGN KEY (`rep_user_id` )
    REFERENCES `users` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Placeholder table for view `view_products`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `view_products` (`id` INT, `product_type_id` INT, `sku` INT, `cost` INT, `image` INT, `active` INT, `max_qty` INT, `is_giftable` INT, `name` INT, `product_type` INT);

-- -----------------------------------------------------
-- View `view_products`
-- -----------------------------------------------------
DROP VIEW IF EXISTS `view_products` ;
DROP TABLE IF EXISTS `view_products`;
CREATE  OR REPLACE VIEW `view_products` AS
select p.*,
(case p.product_type_id
    when 1 then d.name
    when 2 then pp.name
    when 3 then c.name
    when 4 then s.name
    when 5 then ds.name end) as name,
pt.name as product_type
from products p
left join product_types pt
on p.product_type_id = pt.id
left join downloads d
on p.id = d.product_id
left join digital_subscriptions ds
on p.id = ds.product_id
left join subscriptions s
on p.id = s.product_id
left join physical_products pp
on p.id = pp.product_id
left join courses c
on p.id = c.product_id;


SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
