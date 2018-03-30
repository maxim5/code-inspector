SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL';

CREATE SCHEMA IF NOT EXISTS `Hotel_V1` DEFAULT CHARACTER SET latin1 COLLATE latin1_swedish_ci ;
USE `Hotel_V1` ;

-- -----------------------------------------------------
-- Table `Hotel_V1`.`pagamentos`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`pagamentos` (
  `pagamento_id` INT NOT NULL ,
  `pagamento_nome` VARCHAR(45) NULL ,
  PRIMARY KEY (`pagamento_id`) )
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`financeiro`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`financeiro` (
  `financeiro_id` INT NOT NULL ,
  `financeiro_valor` VARCHAR(45) NOT NULL ,
  `pagamento_pagamento_id` INT NOT NULL ,
  PRIMARY KEY (`financeiro_id`) ,
  INDEX `fk_financeiro_pagamento1` (`pagamento_pagamento_id` ASC) ,
  CONSTRAINT `fk_financeiro_pagamento1`
    FOREIGN KEY (`pagamento_pagamento_id` )
    REFERENCES `Hotel_V1`.`pagamentos` (`pagamento_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`reservas`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`reservas` (
  `reserva_id` INT NOT NULL ,
  `financeiro` INT NOT NULL ,
  `reserva_check_in` TINYINT(1) NOT NULL ,
  `reserva_finalizado` TINYINT(1) NOT NULL DEFAULT FALSE ,
  PRIMARY KEY (`reserva_id`) ,
  INDEX `fk_reserva_financeiro1` (`financeiro` ASC) ,
  CONSTRAINT `fk_reserva_financeiro1`
    FOREIGN KEY (`financeiro` )
    REFERENCES `Hotel_V1`.`financeiro` (`financeiro_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`niveis`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`niveis` (
  `nivel_id` INT NOT NULL ,
  `nivel_nome` VARCHAR(45) NOT NULL ,
  PRIMARY KEY (`nivel_id`) )
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`usuarios`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`usuarios` (
  `usuario_id` INT NOT NULL ,
  `usuario_nome` VARCHAR(45) NOT NULL ,
  `usuario_email` VARCHAR(45) NOT NULL ,
  `usuario_tipo` VARCHAR(45) NOT NULL ,
  `usuario_cpf` INT NULL ,
  `usuario_cnpj` INT NULL ,
  `usuario_email` VARCHAR(100) NOT NULL ,
  `usuario_senha` VARCHAR(45) NOT NULL ,
  `niveis` INT NOT NULL ,
  `reserva` INT NOT NULL ,
  INDEX `fk_usuario_reserva1` (`reserva` ASC) ,
  UNIQUE INDEX `usuario_email_UNIQUE` (`usuario_email` ASC) ,
  UNIQUE INDEX `idusuario_UNIQUE` (`usuario_id` ASC) ,
  PRIMARY KEY (`usuario_id`) ,
  INDEX `fk_usuario_niveis1` (`niveis` ASC) ,
  CONSTRAINT `fk_usuario_reserva1`
    FOREIGN KEY (`reserva` )
    REFERENCES `Hotel_V1`.`reservas` (`reserva_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_usuario_niveis1`
    FOREIGN KEY (`niveis` )
    REFERENCES `Hotel_V1`.`niveis` (`nivel_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`chat`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`chat` (
  `chat_id` INT NOT NULL ,
  `chat_conversa` VARCHAR(800) NOT NULL ,
  PRIMARY KEY (`chat_id`) )
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`usuarioXchat`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`usuarioXchat` (
  `usuario` INT NOT NULL ,
  `chat` INT NOT NULL ,
  `usuario_chat_id` VARCHAR(45) NOT NULL ,
  INDEX `fk_usuarioXchat_usuario2` (`usuario` ASC) ,
  INDEX `fk_usuarioXchat_chat2` (`chat` ASC) ,
  PRIMARY KEY (`usuario_chat_id`) ,
  CONSTRAINT `fk_usuarioXchat_usuario2`
    FOREIGN KEY (`usuario` )
    REFERENCES `Hotel_V1`.`usuarios` (`usuario_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_usuarioXchat_chat2`
    FOREIGN KEY (`chat` )
    REFERENCES `Hotel_V1`.`chat` (`chat_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`paises`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`paises` (
  `pais_id` INT NOT NULL ,
  `pais_nome` VARCHAR(45) NOT NULL ,
  PRIMARY KEY (`pais_id`) )
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`estados`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`estados` (
  `estado_id` INT NOT NULL ,
  `estado_UF` VARCHAR(45) NOT NULL ,
  `pais` INT NOT NULL ,
  PRIMARY KEY (`estado_id`) ,
  INDEX `fk_estado_pais1` (`pais` ASC) ,
  CONSTRAINT `fk_estado_pais1`
    FOREIGN KEY (`pais` )
    REFERENCES `Hotel_V1`.`paises` (`pais_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`cidades`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`cidades` (
  `cidade_id` INT NOT NULL ,
  `cidade_nome` VARCHAR(45) NOT NULL ,
  `estado` INT NOT NULL ,
  PRIMARY KEY (`cidade_id`) ,
  INDEX `fk_cidade_estado1` (`estado` ASC) ,
  CONSTRAINT `fk_cidade_estado1`
    FOREIGN KEY (`estado` )
    REFERENCES `Hotel_V1`.`estados` (`estado_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`logradouros`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`logradouros` (
  `logradouro_id` INT NOT NULL ,
  `logradouro_nome` VARCHAR(45) NOT NULL ,
  `logradouro_numero` VARCHAR(45) NOT NULL ,
  PRIMARY KEY (`logradouro_id`) )
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`cep`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`cep` (
  `cep_id` INT NOT NULL ,
  `cidade` INT NOT NULL ,
  `logradouro` INT NOT NULL ,
  PRIMARY KEY (`cep_id`) ,
  INDEX `fk_cep_cidade1` (`cidade` ASC) ,
  INDEX `fk_cep_logradouro1` (`logradouro` ASC) ,
  CONSTRAINT `fk_cep_cidade1`
    FOREIGN KEY (`cidade` )
    REFERENCES `Hotel_V1`.`cidades` (`cidade_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_cep_logradouro1`
    FOREIGN KEY (`logradouro` )
    REFERENCES `Hotel_V1`.`logradouros` (`logradouro_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`hoteis`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`hoteis` (
  `hotel_id` INT NOT NULL ,
  `hotel_nome` VARCHAR(45) NOT NULL ,
  `hotel_cnpj` INT NOT NULL ,
  `hotel_inscricao_estadual` INT NOT NULL ,
  `hotel_email` VARCHAR(100) NOT NULL ,
  `hotel_observacoes` VARCHAR(800) NULL ,
  `hotel_gerente` VARCHAR(70) NULL ,
  PRIMARY KEY (`hotel_id`) ,
  UNIQUE INDEX `hotel_cnpj_UNIQUE` (`hotel_cnpj` ASC) ,
  UNIQUE INDEX `hotel_nome_UNIQUE` (`hotel_nome` ASC) ,
  UNIQUE INDEX `hotel_inscricao_estadual_UNIQUE` (`hotel_inscricao_estadual` ASC) )
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`telefones`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`telefones` (
  `telefone_id` INT NOT NULL ,
  `hotel` INT NULL ,
  `usuario` INT NULL ,
  `telefone_numero` INT NOT NULL ,
  PRIMARY KEY (`telefone_id`) ,
  INDEX `fk_telefone_hotel1` (`hotel` ASC) ,
  INDEX `fk_telefone_usuario1` (`usuario` ASC) ,
  CONSTRAINT `fk_telefone_hotel1`
    FOREIGN KEY (`hotel` )
    REFERENCES `Hotel_V1`.`hoteis` (`hotel_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_telefone_usuario1`
    FOREIGN KEY (`usuario` )
    REFERENCES `Hotel_V1`.`usuarios` (`usuario_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`ambientes`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`ambientes` (
  `ambiente_id` INT NOT NULL ,
  `hotel` INT NOT NULL ,
  `ambiente_nome` VARCHAR(75) NOT NULL ,
  `ambiente_observacao` VARCHAR(800) NOT NULL ,
  `ambiente_valor` VARCHAR(45) NOT NULL ,
  `ambiente_data_inicial` DATE NULL ,
  `ambiente_data_final` DATE NULL ,
  `ambiente_reservado` TINYINT(1) NOT NULL DEFAULT FALSE ,
  PRIMARY KEY (`ambiente_id`) ,
  INDEX `fk_ambiente_hotel1` (`hotel` ASC) ,
  UNIQUE INDEX `ambiente_nome_UNIQUE` (`ambiente_nome` ASC) ,
  CONSTRAINT `fk_ambiente_hotel1`
    FOREIGN KEY (`hotel` )
    REFERENCES `Hotel_V1`.`hoteis` (`hotel_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`servicos`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`servicos` (
  `servico_id` INT NOT NULL ,
  `hotel` INT NOT NULL ,
  `servico_nome` VARCHAR(45) NOT NULL ,
  `servico_observacao` VARCHAR(800) NULL ,
  `servico_valor` DECIMAL(10,0) NOT NULL ,
  `servico_data_pedido` VARCHAR(45) NULL ,
  PRIMARY KEY (`servico_id`) ,
  INDEX `fk_servico_hotel1` (`hotel` ASC) ,
  CONSTRAINT `fk_servico_hotel1`
    FOREIGN KEY (`hotel` )
    REFERENCES `Hotel_V1`.`hoteis` (`hotel_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`camas`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`camas` (
  `camas_id` INT NOT NULL ,
  `cama_nome` VARCHAR(75) NOT NULL ,
  PRIMARY KEY (`camas_id`) )
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`tipos_quartos`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`tipos_quartos` (
  `tipos_quartos_id` INT NOT NULL ,
  `quartos_descricao` VARCHAR(75) NOT NULL ,
  PRIMARY KEY (`tipos_quartos_id`) )
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`quartos`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`quartos` (
  `quartos_id` INT NOT NULL ,
  `hotel` INT NOT NULL ,
  `quarto_numero` INT NOT NULL ,
  `quarto_descricao` VARCHAR(800) NULL ,
  `quarto_valor` DECIMAL(10,0) NOT NULL ,
  `tipos_quartos` INT NOT NULL ,
  `quarto_data_inicial` DATE NULL ,
  `quarto_data_final` DATE NULL ,
  `quarto_reservado` TINYINT(1) NOT NULL DEFAULT FALSE ,
  `quarto_data_reserva` DATE NOT NULL ,
  PRIMARY KEY (`quartos_id`) ,
  INDEX `fk_quartos_hotel1` (`hotel` ASC) ,
  UNIQUE INDEX `quartos_numero_UNIQUE` (`quarto_numero` ASC) ,
  INDEX `fk_quartos_tipos_quartos1` (`tipos_quartos` ASC) ,
  CONSTRAINT `fk_quartos_hotel1`
    FOREIGN KEY (`hotel` )
    REFERENCES `Hotel_V1`.`hoteis` (`hotel_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_quartos_tipos_quartos1`
    FOREIGN KEY (`tipos_quartos` )
    REFERENCES `Hotel_V1`.`tipos_quartos` (`tipos_quartos_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`cardapios`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`cardapios` (
  `cardapio_id` INT NOT NULL ,
  `hotel` INT NOT NULL ,
  `cardapio_nome` VARCHAR(75) NOT NULL ,
  `cardapio_tempo` TIME NOT NULL ,
  `cardapio_observacao` VARCHAR(800) NULL ,
  `cardapio_valor_calorico` INT NULL ,
  `cardapio_valor` DECIMAL(10,0) NOT NULL ,
  `cardapio_data_pedido` DATE NOT NULL ,
  PRIMARY KEY (`cardapio_id`) ,
  INDEX `fk_cardapio_hotel1` (`hotel` ASC) ,
  UNIQUE INDEX `cardapio_nome_UNIQUE` (`cardapio_nome` ASC) ,
  CONSTRAINT `fk_cardapio_hotel1`
    FOREIGN KEY (`hotel` )
    REFERENCES `Hotel_V1`.`hoteis` (`hotel_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`pacotes`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`pacotes` (
  `pacote_id` INT NOT NULL ,
  `quartos` INT NOT NULL ,
  `cardapio` INT NOT NULL ,
  `ambiente` INT NOT NULL ,
  `servico` INT NOT NULL ,
  `pacote_nome` VARCHAR(45) NOT NULL ,
  `pacote_data_inicial` DATE NOT NULL ,
  `pacote_data_final` DATE NOT NULL ,
  `pacote_pessoas` INT NOT NULL ,
  `pacote_desc` INT NOT NULL ,
  PRIMARY KEY (`pacote_id`) ,
  INDEX `fk_pacote_quartos1` (`quartos` ASC) ,
  INDEX `fk_pacote_cardapio1` (`cardapio` ASC) ,
  INDEX `fk_pacote_ambiente1` (`ambiente` ASC) ,
  INDEX `fk_pacote_servico1` (`servico` ASC) ,
  UNIQUE INDEX `pacote_nome_UNIQUE` (`pacote_nome` ASC) ,
  CONSTRAINT `fk_pacote_quartos1`
    FOREIGN KEY (`quartos` )
    REFERENCES `Hotel_V1`.`quartos` (`quartos_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_pacote_cardapio1`
    FOREIGN KEY (`cardapio` )
    REFERENCES `Hotel_V1`.`cardapios` (`cardapio_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_pacote_ambiente1`
    FOREIGN KEY (`ambiente` )
    REFERENCES `Hotel_V1`.`ambientes` (`ambiente_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_pacote_servico1`
    FOREIGN KEY (`servico` )
    REFERENCES `Hotel_V1`.`servicos` (`servico_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`itens_reserva`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`itens_reserva` (
  `reserva_id` INT NOT NULL ,
  `quartos` INT NOT NULL ,
  `cardapio` INT NOT NULL ,
  `ambiente` INT NOT NULL ,
  `servico` INT NOT NULL ,
  `reserva` INT NOT NULL ,
  PRIMARY KEY (`reserva_id`) ,
  INDEX `fk_reserva_quartos1` (`quartos` ASC) ,
  INDEX `fk_reserva_cardapio1` (`cardapio` ASC) ,
  INDEX `fk_reserva_ambiente1` (`ambiente` ASC) ,
  INDEX `fk_reserva_servico1` (`servico` ASC) ,
  INDEX `fk_itens_reserva_reserva1` (`reserva` ASC) ,
  CONSTRAINT `fk_reserva_quartos1`
    FOREIGN KEY (`quartos` )
    REFERENCES `Hotel_V1`.`quartos` (`quartos_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_reserva_cardapio1`
    FOREIGN KEY (`cardapio` )
    REFERENCES `Hotel_V1`.`cardapios` (`cardapio_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_reserva_ambiente1`
    FOREIGN KEY (`ambiente` )
    REFERENCES `Hotel_V1`.`ambientes` (`ambiente_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_reserva_servico1`
    FOREIGN KEY (`servico` )
    REFERENCES `Hotel_V1`.`servicos` (`servico_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_itens_reserva_reserva1`
    FOREIGN KEY (`reserva` )
    REFERENCES `Hotel_V1`.`reservas` (`reserva_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`logradouroXusuario`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`logradouroXusuario` (
  `logradouro_usuario_id` INT NOT NULL ,
  `cep` INT NOT NULL ,
  `usuario` INT NOT NULL ,
  `hotel` INT NOT NULL ,
  PRIMARY KEY (`logradouro_usuario_id`) ,
  INDEX `fk_logradouro_usuario_cep1` (`cep` ASC) ,
  INDEX `fk_logradouro_usuario_usuario1` (`usuario` ASC) ,
  INDEX `fk_logradouro_usuario_hotel1` (`hotel` ASC) ,
  CONSTRAINT `fk_logradouro_usuario_cep1`
    FOREIGN KEY (`cep` )
    REFERENCES `Hotel_V1`.`cep` (`cep_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_logradouro_usuario_usuario1`
    FOREIGN KEY (`usuario` )
    REFERENCES `Hotel_V1`.`usuarios` (`usuario_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_logradouro_usuario_hotel1`
    FOREIGN KEY (`hotel` )
    REFERENCES `Hotel_V1`.`hoteis` (`hotel_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`pacoteXusuario`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`pacoteXusuario` (
  `usuario` INT NOT NULL ,
  `pacote` INT NOT NULL ,
  `pacoteXusuario_id` VARCHAR(45) NOT NULL ,
  INDEX `fk_pacoteXusuario_usuario1` (`usuario` ASC) ,
  INDEX `fk_pacoteXusuario_pacote1` (`pacote` ASC) ,
  PRIMARY KEY (`pacoteXusuario_id`) ,
  CONSTRAINT `fk_pacoteXusuario_usuario1`
    FOREIGN KEY (`usuario` )
    REFERENCES `Hotel_V1`.`usuarios` (`usuario_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_pacoteXusuario_pacote1`
    FOREIGN KEY (`pacote` )
    REFERENCES `Hotel_V1`.`pacotes` (`pacote_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`ramais`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`ramais` (
  `ramal_id` INT NOT NULL ,
  `quartos_idquartos` INT NULL ,
  `ambiente_idambiente` INT NULL ,
  `ramal_numero` INT NOT NULL ,
  PRIMARY KEY (`ramal_id`) ,
  INDEX `fk_ramal_quartos1` (`quartos_idquartos` ASC) ,
  INDEX `fk_ramal_ambiente1` (`ambiente_idambiente` ASC) ,
  CONSTRAINT `fk_ramal_quartos1`
    FOREIGN KEY (`quartos_idquartos` )
    REFERENCES `Hotel_V1`.`quartos` (`quartos_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_ramal_ambiente1`
    FOREIGN KEY (`ambiente_idambiente` )
    REFERENCES `Hotel_V1`.`ambientes` (`ambiente_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Hotel_V1`.`quartoXcama`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `Hotel_V1`.`quartoXcama` (
  `camas` INT NOT NULL ,
  `quartos` INT NOT NULL ,
  `usuario_chat_id` VARCHAR(45) NOT NULL ,
  INDEX `fk_quartoXcama_camas1` (`camas` ASC) ,
  INDEX `fk_quartoXcama_quartos1` (`quartos` ASC) ,
  PRIMARY KEY (`usuario_chat_id`) ,
  CONSTRAINT `fk_quartoXcama_camas1`
    FOREIGN KEY (`camas` )
    REFERENCES `Hotel_V1`.`camas` (`camas_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_quartoXcama_quartos1`
    FOREIGN KEY (`quartos` )
    REFERENCES `Hotel_V1`.`quartos` (`quartos_id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;



SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
