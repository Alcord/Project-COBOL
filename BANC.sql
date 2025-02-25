CREATE DATABASE  IF NOT EXISTS `banco` /*!40100 DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci */ /*!80016 DEFAULT ENCRYPTION='N' */;
USE `banco`;
-- MySQL dump 10.13  Distrib 8.0.40, for Win64 (x86_64)
--
-- Host: localhost    Database: banco
-- ------------------------------------------------------
-- Server version	8.0.40

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!50503 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `clientes`
--

DROP TABLE IF EXISTS `clientes`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `clientes` (
  `ID_CLIENTE` int NOT NULL,
  `TIPO_DOC` char(3) DEFAULT NULL,
  `DOC_CLIENTE` varchar(12) DEFAULT NULL,
  `FECHA_ALTA` date DEFAULT NULL,
  `NOMBRE_CLIENTE` varchar(25) DEFAULT NULL,
  `APELLIDOS_CLIENTE` varchar(25) DEFAULT NULL,
  `DIRECCION_CLIENTE` varchar(45) DEFAULT NULL,
  `TELEF_CLIENTE` char(12) DEFAULT NULL,
  `EMAIL_CLIENTE` varchar(40) DEFAULT NULL,
  `TARJETA` tinyint(1) DEFAULT NULL,
  `CREDITO` tinyint(1) DEFAULT NULL,
  `HIPOTECA` tinyint(1) DEFAULT NULL,
  `CTA_ACTIVA` tinyint(1) DEFAULT NULL,
  `FECHA_CIERRE` date DEFAULT NULL,
  `SALDO_CLIENTE` decimal(12,2) DEFAULT NULL,
  PRIMARY KEY (`ID_CLIENTE`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `clientes`
--

LOCK TABLES `clientes` WRITE;
/*!40000 ALTER TABLE `clientes` DISABLE KEYS */;
INSERT INTO `clientes` VALUES (1,'DNI','12345678','2023-01-01','Juan','Perez','Calle Falsa 123','5551234','juan.perez@email.com',1,0,0,1,NULL,1000.50),(2,'DNI','87654321','2022-06-15','Maria','Gomez','Av. Siempre Viva 742','5555678','maria.gomez@email.com',1,1,1,1,NULL,2500.00),(3,'PAS','99887766','2021-12-01','Carlos','Lopez','Calle Principal 55','5559876','carlos.lopez@email.com',0,1,0,1,NULL,1500.75),(4,'DNI','11223344','2023-02-10','Ana','Martinez','Calle Las Flores 10','5552234','ana.martinez@email.com',1,1,1,1,NULL,3000.00),(5,'DNI','33445566','2020-10-20','Luis','Garcia','Calle El Sol 88','5553456','luis.garcia@email.com',0,0,1,1,NULL,1800.00),(6,'PAS','55667788','2021-05-18','Laura','Fernandez','Av. Central 55','5557890','laura.fernandez@email.com',1,1,0,1,NULL,2300.40),(7,'DNI','77889900','2022-03-25','Pedro','Rodriguez','Calle Luna 22','5556543','pedro.rodriguez@email.com',1,0,1,1,NULL,1200.00),(8,'PAS','88990011','2021-11-05','Sofia','Ruiz','Av. Libertad 77','5552314','sofia.ruiz@email.com',0,0,0,1,NULL,900.00),(9,'DNI','99001122','2023-04-07','Diego','Moreno','Calle La Paz 15','5559874','diego.moreno@email.com',1,1,1,1,NULL,3200.75),(10,'DNI','10111213','2022-01-01','Paula','Jimenez','Calle Olivo 30','5554467','paula.jimenez@email.com',1,0,0,1,NULL,4000.00),(11,'DNI','11121314','2022-08-15','David','Herrera','Calle Sauce 60','5556789','david.herrera@email.com',0,1,1,1,NULL,2750.25),(12,'PAS','12131415','2020-09-01','Carmen','Romero','Calle Del Mar 40','5552235','carmen.romero@email.com',1,1,0,1,NULL,3100.50),(13,'DNI','13141516','2023-06-12','Andres','Navarro','Calle Horizonte 5','5555467','andres.navarro@email.com',1,0,1,1,NULL,2000.00),(14,'PAS','14151617','2021-04-04','Susana','Guzman','Calle Victoria 33','5556654','susana.guzman@email.com',0,1,1,1,NULL,1900.75),(15,'DNI','15161718','2023-03-01','Raul','Alonso','Calle Libertador 22','5553322','raul.alonso@email.com',1,0,0,1,NULL,1450.00),(16,'DNI','16171819','2020-12-24','Lucia','Iglesias','Calle America 19','5557889','lucia.iglesias@email.com',0,0,1,1,NULL,2500.00),(17,'PAS','17181920','2021-07-14','Manuel','Ortiz','Av. Italia 77','5551190','manuel.ortiz@email.com',1,1,0,1,NULL,2900.00),(18,'DNI','18192021','2023-01-25','Isabel','Vega','Calle Estrella 50','5553311','isabel.vega@email.com',0,1,1,1,NULL,1700.00),(19,'DNI','19202122','2022-10-31','Fernando','Campos','Av. Argentina 99','5554433','fernando.campos@email.com',1,1,1,1,NULL,2100.00),(20,'PAS','20212223','2023-05-18','Carolina','Ramirez','Calle Bolivia 88','5552298','carolina.ramirez@email.com',1,0,0,1,NULL,3100.50);
/*!40000 ALTER TABLE `clientes` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `cod_movimientos`
--

DROP TABLE IF EXISTS `cod_movimientos`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `cod_movimientos` (
  `ID_MOV` int NOT NULL,
  `DESCRIP_MOV` varchar(35) DEFAULT NULL,
  `SIGNO_MOV` char(1) DEFAULT NULL,
  PRIMARY KEY (`ID_MOV`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `cod_movimientos`
--

LOCK TABLES `cod_movimientos` WRITE;
/*!40000 ALTER TABLE `cod_movimientos` DISABLE KEYS */;
INSERT INTO `cod_movimientos` VALUES (1,'Transferencia bancaria con signo +','+'),(2,'Deposito en efectivo con signo +','+'),(7,'Comisiones bancarias con signo -','-'),(8,'Apertura de plazo fijo con signo -','-'),(10,'Pago de hipoteca con signo -','-'),(11,'Intereses de hipoteca con signo +','+'),(12,'Credito aprobado con signo +','+'),(13,'Devolucion de credito con signo -','-'),(14,'Cancelacion de deuda con signo +','+'),(15,'Mantenimiento de cuenta con signo -','-');
/*!40000 ALTER TABLE `cod_movimientos` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `ctactes`
--

DROP TABLE IF EXISTS `ctactes`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `ctactes` (
  `ID_CTACTES` int NOT NULL AUTO_INCREMENT,
  `ID_CLIENTE` int DEFAULT NULL,
  `COD_ULT_MOV` int DEFAULT NULL,
  `FECHA_ULT_MOV` date DEFAULT NULL,
  `IMPORTE_MOV` decimal(12,2) DEFAULT NULL,
  `SALDO_ACTUAL` decimal(12,2) DEFAULT NULL,
  PRIMARY KEY (`ID_CTACTES`),
  KEY `ID_CLIENTE` (`ID_CLIENTE`),
  CONSTRAINT `ctactes_ibfk_1` FOREIGN KEY (`ID_CLIENTE`) REFERENCES `clientes` (`ID_CLIENTE`)
) ENGINE=InnoDB AUTO_INCREMENT=21 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `ctactes`
--

LOCK TABLES `ctactes` WRITE;
/*!40000 ALTER TABLE `ctactes` DISABLE KEYS */;
INSERT INTO `ctactes` VALUES (1,1,1,'2023-01-15',500.00,1500.50),(2,2,2,'2023-02-01',-200.00,2300.00),(3,3,3,'2023-03-10',300.75,1800.50),(4,4,4,'2023-04-05',-150.00,2850.00),(5,5,5,'2023-05-01',400.00,2200.00),(6,6,6,'2023-06-15',-500.00,1800.40),(7,7,7,'2023-07-01',200.00,1400.00),(8,8,8,'2023-08-01',-100.00,800.00),(9,9,9,'2023-09-01',600.75,3800.75),(10,10,10,'2023-10-10',-250.00,3750.00),(11,11,11,'2023-11-01',350.00,3100.25),(12,12,12,'2023-12-01',-450.00,2650.50),(13,13,13,'2023-12-10',400.00,2400.00),(14,14,14,'2023-12-15',-300.00,1600.75),(15,15,15,'2023-12-20',150.00,1600.00),(16,16,16,'2023-12-25',-100.00,2400.00),(17,17,17,'2023-12-30',250.00,3150.00),(18,18,18,'2024-01-01',-350.00,1350.00),(19,19,19,'2024-01-05',400.00,2500.00),(20,20,20,'2024-01-10',-450.00,2650.50);
/*!40000 ALTER TABLE `ctactes` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `hipotecas`
--

DROP TABLE IF EXISTS `hipotecas`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `hipotecas` (
  `ID_HIPOTECA` int NOT NULL AUTO_INCREMENT,
  `ID_CLIENTE` int NOT NULL,
  `FECHA_INICIO` date NOT NULL,
  `MONTO_ORIGINAL` decimal(12,2) NOT NULL,
  `TASA_INTERES` decimal(5,2) NOT NULL,
  `SALDO_ACTUAL` decimal(12,2) NOT NULL,
  `FECHA_VENCIMIENTO` date NOT NULL,
  `ESTADO` varchar(20) NOT NULL,
  PRIMARY KEY (`ID_HIPOTECA`),
  KEY `ID_CLIENTE` (`ID_CLIENTE`),
  CONSTRAINT `hipotecas_ibfk_1` FOREIGN KEY (`ID_CLIENTE`) REFERENCES `clientes` (`ID_CLIENTE`)
) ENGINE=InnoDB AUTO_INCREMENT=11 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `hipotecas`
--

LOCK TABLES `hipotecas` WRITE;
/*!40000 ALTER TABLE `hipotecas` DISABLE KEYS */;
INSERT INTO `hipotecas` VALUES (1,1,'2023-01-01',250000.00,3.50,240000.00,'2033-01-01','Activa'),(2,2,'2023-02-01',200000.00,3.80,195000.00,'2033-02-01','Activa'),(3,3,'2023-03-01',150000.00,4.00,140000.00,'2033-03-01','Morosa'),(4,4,'2023-04-01',100000.00,4.20,90000.00,'2033-04-01','Activa'),(5,5,'2023-05-01',300000.00,3.90,290000.00,'2033-05-01','Activa'),(6,6,'2023-06-01',180000.00,4.10,175000.00,'2033-06-01','Activa'),(7,7,'2023-07-01',220000.00,4.30,215000.00,'2033-07-01','Morosa'),(8,8,'2023-08-01',280000.00,3.60,275000.00,'2033-08-01','Activa'),(9,9,'2023-09-01',320000.00,3.70,310000.00,'2033-09-01','Activa'),(10,10,'2023-10-01',150000.00,4.50,145000.00,'2033-10-01','Activa');
/*!40000 ALTER TABLE `hipotecas` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `indicadores`
--

DROP TABLE IF EXISTS `indicadores`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `indicadores` (
  `ID_INDICADOR` int NOT NULL,
  `DESCRIPCION` varchar(35) DEFAULT NULL,
  `TASA_INTERES` decimal(5,2) DEFAULT NULL,
  PRIMARY KEY (`ID_INDICADOR`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `indicadores`
--

LOCK TABLES `indicadores` WRITE;
/*!40000 ALTER TABLE `indicadores` DISABLE KEYS */;
INSERT INTO `indicadores` VALUES (3,'Tasa de inflacion promedio anual',3.20),(8,'Euribor mensual',0.30),(9,'Tasa de mora para hipotecas',2.50);
/*!40000 ALTER TABLE `indicadores` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `tarjetas`
--

DROP TABLE IF EXISTS `tarjetas`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `tarjetas` (
  `ID_CLIENTE` int NOT NULL,
  `NRO_TARJETA` char(16) NOT NULL,
  `FECHA_VENCIMIENTO` date DEFAULT NULL,
  `LIMITE_TARJETA` decimal(12,2) DEFAULT NULL,
  `ACUM_MES` decimal(12,2) DEFAULT NULL,
  `LIQUIDACION_MES` decimal(12,2) DEFAULT NULL,
  PRIMARY KEY (`ID_CLIENTE`,`NRO_TARJETA`),
  CONSTRAINT `tarjetas_ibfk_1` FOREIGN KEY (`ID_CLIENTE`) REFERENCES `clientes` (`ID_CLIENTE`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `tarjetas`
--

LOCK TABLES `tarjetas` WRITE;
/*!40000 ALTER TABLE `tarjetas` DISABLE KEYS */;
INSERT INTO `tarjetas` VALUES (1,'1234567812345678','2026-12-31',10000.00,500.00,9500.00),(2,'8765432187654321','2025-11-30',15000.00,1200.00,13800.00),(3,'9988776699887766','2027-01-31',8000.00,700.00,7300.00),(4,'1122334411223344','2026-06-30',20000.00,3000.00,17000.00),(5,'3344556633445566','2028-03-31',12000.00,1800.00,10200.00),(6,'5566778855667788','2025-07-31',10000.00,500.00,9500.00),(7,'7788990077889900','2026-10-31',15000.00,2000.00,13000.00),(8,'8899001188990011','2027-12-31',18000.00,900.00,17100.00),(9,'9900112299001122','2028-08-31',25000.00,6000.00,19000.00),(10,'1011121310111213','2026-05-31',30000.00,5000.00,25000.00),(11,'1112131411121314','2025-09-30',15000.00,2750.00,12250.00),(12,'1213141512131415','2028-04-30',12000.00,3100.00,8900.00),(13,'1314151613141516','2027-11-30',8000.00,2000.00,6000.00),(14,'1415161714151617','2026-02-28',16000.00,1900.00,14100.00),(15,'1516171815161718','2028-07-31',11000.00,1450.00,9550.00),(16,'1617181916171819','2027-09-30',15000.00,2500.00,12500.00),(17,'1718192017181920','2028-10-31',17000.00,2900.00,14100.00),(18,'1819202118192021','2025-03-31',12000.00,1700.00,10300.00),(19,'1920212219202122','2027-01-31',13000.00,2100.00,10900.00),(20,'2021222320212223','2026-11-30',14000.00,3100.00,10900.00);
/*!40000 ALTER TABLE `tarjetas` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2025-01-17 16:39:53
