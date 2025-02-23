CREATE TABLE banco.movimientos_ctactes (
  ID_MOV_CTA int ,
  ID_CTACTES int ,
  ID_CLIENTE int ,
  FECHA_MOV date ,
  TIP_MOV char(1),
  IMPORTE_MOV decimal(12,2) DEFAULT NULL,
  SALDO_ACTUAL decimal(12,2) DEFAULT NULL
) 