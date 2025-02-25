       IDENTIFICATION DIVISION.
       PROGRAM-ID. TARJCRE001.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE-DETAIL ASSIGN TO "DEUDA_DETAIL.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE-DETAIL.
       01  RPT-DETALLE-CLI   PIC X(1024).


       WORKING-STORAGE SECTION.
      **********************************************************************
      *******                EMBEDDED SQL VARIABLES                  *******
       01 SQLCA.
           05 SQLSTATE PIC X(5).
              88  SQL-SUCCESS           VALUE '00000'.
              88  SQL-RIGHT-TRUNC       VALUE '01004'.
              88  SQL-NODATA            VALUE '02000'.
              88  SQL-DUPLICATE         VALUE '23000' THRU '23999'.
              88  SQL-MULTIPLE-ROWS     VALUE '21000'.
              88  SQL-NULL-NO-IND       VALUE '22002'.
              88  SQL-INVALID-CURSOR-STATE VALUE '24000'.
           05 FILLER   PIC X.
           05 SQLVERSN PIC 99 VALUE 03.
           05 SQLCODE  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQLERRM.
               49 SQLERRML PIC S9(4) COMP-5 VALUE ZERO.
               49 SQLERRMC PIC X(486).
           05 SQLERRD OCCURS 6 TIMES PIC S9(9) COMP-5 VALUE ZERO.
           05 FILLER   PIC X(4).
           05 SQL-HCONN USAGE POINTER VALUE NULL.
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 8.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 8 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 8 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 8 TIMES.
           05 SQL-PREC   PIC X OCCURS 8 TIMES.
      **********************************************************************
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 111.
           05 SQL-STMT   PIC X(111) VALUE 'SELECT CASE WHEN EXISTS (SELE
      -    'CT 1 FROM BANCO.TARJETAS WHERE NRO_TARJETA =TRIM(?) LIMIT 1)
      -    ' THEN ''S'' ELSE ''N'' END'.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 66.
           05 SQL-STMT   PIC X(66) VALUE 'SELECT ID_CLIENTE FROM CLIENTE
      -    'S WHERE DOC_CLIENTE =TRIM(?) LIMIT 1'.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 16.
           05 SQL-STMT   PIC X(16) VALUE 'SELECT CURDATE()'.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 73.
           05 SQL-STMT   PIC X(73) VALUE 'SELECT COALESCE(MAX(ID_MOVIMIE
      -    'NTO),0) + 1 FROM BANCO.MOVIMIENTOS_TARJETAS'.
      **********************************************************************
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 6.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 123.
           05 SQL-STMT   PIC X(123) VALUE 'INSERT INTO BANCO.MOVIMIENTOS
      -    '_TARJETAS (ID_MOVIMIENTO,ID_CLIENTE,NRO_TARJETA,FECHA_MOV,TI
      -    'PO_MOV,MONTO) VALUES (?,?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-5.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 193.
           05 SQL-STMT   PIC X(193) VALUE 'SELECT ID_CLIENTE,NRO_TARJETA
      -    ',LIMITE_TARJETA - ACUM_MES SALDO_ACTUAL FROM BANCO.TARJETAS 
      -    'WHERE ID_CLIENTE = (SELECT ID_CLIENTE FROM BANCO.CLIENTES WH
      -    'ERE CTA_ACTIVA = 1 AND DOC_CLIENTE =TRIM(?))'.
           05 SQL-CNAME  PIC X(12) VALUE 'CUR_TARJ-ACT'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-6.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 70.
           05 SQL-STMT   PIC X(70) VALUE 'UPDATE TARJETAS SET ACUM_MES =
      -    ' ACUM_MES + ? WHERE NRO_TARJETA =TRIM(?)'.
      **********************************************************************
       01 SQL-STMT-7.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 70.
           05 SQL-STMT   PIC X(70) VALUE 'UPDATE TARJETAS SET ACUM_MES =
      -    ' ACUM_MES - ? WHERE NRO_TARJETA =TRIM(?)'.
      **********************************************************************
       01 SQL-STMT-8.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 153.
           05 SQL-STMT   PIC X(153) VALUE 'UPDATE TARJETAS SET LIQUIDACI
      -    'ON_MES = CASE WHEN ACUM_MES < 0 THEN LIMITE_TARJETA + ACUM_M
      -    'ES ELSE LIMITE_TARJETA - ACUM_MES END WHERE NRO_TARJETA =TRI
      -    'M(?)'.
      **********************************************************************
       01 SQL-STMT-9.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 171.
           05 SQL-STMT   PIC X(171) VALUE 'SELECT CASE WHEN EXISTS (SELE
      -    'CT 1 FROM BANCO.TARJETAS WHERE ID_CLIENTE = (SELECT ID_CLIEN
      -    'TE FROM BANCO.CLIENTES WHERE DOC_CLIENTE = TRIM(?) LIMIT 1))
      -    ' THEN ''S'' ELSE ''N'' END'.
      **********************************************************************
       01 SQL-STMT-10.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 39.
           05 SQL-STMT   PIC X(39) VALUE 'SELECT GenerarNumeroTarjeta() 
      -    'FROM DUAL'.
      **********************************************************************
       01 SQL-STMT-11.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 72.
           05 SQL-STMT   PIC X(72) VALUE 'SELECT ID_CLIENTE FROM BANCO.C
      -    'LIENTES WHERE DOC_CLIENTE =TRIM(?) LIMIT 1'.
      **********************************************************************
       01 SQL-STMT-12.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 4.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 104.
           05 SQL-STMT   PIC X(104) VALUE 'INSERT INTO BANCO.TARJETAS VA
      -    'LUES(TRIM(?),TRIM(?),(SELECT CURDATE() + INTERVAL 10 YEAR FR
      -    'OM DUAL),?,0,?)'.
      **********************************************************************
       01 SQL-STMT-13.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 329.
           05 SQL-STMT   PIC X(329) VALUE 'SELECT ID_CLIENTE,NRO_TARJETA
      -    ',FECHA_MOV,CASE WHEN TIPO_MOV = ''P'' THEN ''PAGO'' ELSE ''C
      -    'ONSUMO'' END TIPO_MOV,CASE WHEN TIPO_MOV = ''P'' THEN MONTO 
      -    '* (- 1) ELSE MONTO END MONTO FROM BANCO.MOVIMIENTOS_TARJETAS
      -    ' WHERE ID_CLIENTE = (SELECT ID_CLIENTE FROM BANCO.CLIENTES W
      -    'HERE CTA_ACTIVA = 1 AND DOC_CLIENTE =TRIM(?)) ORDER BY FECHA
      -    '_MOV ASC'.
           05 SQL-CNAME  PIC X(16) VALUE 'CURSOR_DEUDA-DET'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-14.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 697.
           05 SQL-STMT   PIC X(697) VALUE 'SELECT D.ID_CLIENTE,D.NRO_TAR
      -    'JETA,D.FECHA_VENCIMIENTO,D.LIMITE_TARJETA,IFNULL(CONSUMO.MON
      -    'TO_TOTAL,0) AS CONSUMO,IFNULL(PAGO.MONTO_TOTAL,0) AS PAGO,IF
      -    'NULL(CONSUMO.MONTO_TOTAL,0) - IFNULL(PAGO.MONTO_TOTAL,0) AS 
      -    'DEUDA,D.LIQUIDACION_MES FROM TARJETAS D LEFT JOIN (SELECT ID
      -    '_CLIENTE,SUM(MONTO) AS MONTO_TOTAL FROM BANCO.MOVIMIENTOS_TA
      -    'RJETAS WHERE TIPO_MOV = ''C'' GROUP BY ID_CLIENTE) CONSUMO O
      -    'N D.ID_CLIENTE = CONSUMO.ID_CLIENTE LEFT JOIN (SELECT ID_CLI
      -    'ENTE,SUM(MONTO) AS MONTO_TOTAL FROM BANCO.MOVIMIENTOS_TARJET
      -    'AS WHERE TIPO_MOV = ''P'' GROUP BY ID_CLIENTE) PAGO ON D.ID_
      -    'CLIENTE = PAGO.ID_CLIENTE WHERE D.ID_CLIENTE = (SELECT ID_CL
      -    'IENTE FROM BANCO.CLIENTES WHERE CTA_ACTIVA = 1 AND DOC_CLIEN
      -    'TE =TRIM(?))'.
           05 SQL-CNAME  PIC X(12) VALUE 'CURSOR_DEUDA'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-15.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 41.
           05 SQL-STMT   PIC X(41) VALUE 'SELECT InsertarMovimiento(?,?)
      -    ' FROM DUAL;'.
      **********************************************************************
       01 SQL-STMT-16.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 99.
           05 SQL-STMT   PIC X(99) VALUE 'SELECT IFNULL((SELECT ''S'' FR
      -    'OM CLIENTES WHERE DOC_CLIENTE =TRIM(?) AND CTA_ACTIVA = 1 LI
      -    'MIT 1),''N'')'.
      **********************************************************************
       01 SQL-STMT-17.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 17.
           05 SQL-STMT   PIC X(17) VALUE 'SELECT DATABASE()'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0002  PIC S9(5) COMP-3.
           05 SQL-VAR-0003  PIC S9(5) COMP-3.
           05 SQL-VAR-0004  PIC S9(9)V9(2) COMP-3.
           05 SQL-VAR-0006  PIC S9(9)V9(2) COMP-3.
           05 SQL-VAR-0007  PIC S9(3) COMP-3.
           05 SQL-VAR-0008  PIC S9(11) COMP-3.
           05 SQL-VAR-0009  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0013  PIC S9(11) COMP-3.
           05 SQL-VAR-0014  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0015  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0016  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0017  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0018  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0019  PIC S9(11) COMP-3.
           05 SQL-VAR-0020  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0021  PIC S9(11) COMP-3.
           05 SQL-VAR-0022  PIC S9(13)V9(2) COMP-3.
      *******       END OF PRECOMPILER-GENERATED VARIABLES           *******
      **********************************************************************
           COPY "BD001".

      *    EXEC SQL
      *        BEGIN DECLARE SECTION
      *    END-EXEC

       01  DB-DATOS-CLI.
           10  WS-NOMBRE           PIC X(15).
           10  WS-APELLIDOS        PIC X(15).
           10  WS-FECHA-ULT-MOV    PIC X(10).
           10  WS-SALDO-ACTUAL     PIC 9(8)V99.

       01  DB-DATOS-TARJETA.
           05  WT-ID-CLIENTE                  PIC 9(05).
           05  WTT-ID-CLIENTE                 PIC 9(05).
           05  WT-DOC-CLI                     PIC X(12).
           05  WT-NUMERO-TARJ                 PIC X(16).
           05  WT-LIMITE-TARJ                 PIC S9(9)V99.
           05  WT-EXISTE-TARJ                 PIC X(01).
       01  DB-VARS.
           05  BUFFER                  PIC X(1024).
           05  ST-COUNT                PIC 9(6).
           05  CTACTE.
               10  WT-MONTO                   PIC S9(8)V99.
               10  WS-EXISTE-CLIENTE          PIC X(01).
               10  WS-NEWID-CTACTE            PIC 999.

       01  DB-DATOS-CONSUMO.
           05  WT-ID-MOVIMIENTO              PIC 9(10).
           05  WT-NRO-TARJETA                PIC X(16).
           05  WT-FECHA-MOVIMIENTO           PIC X(10).
           05  WT-TIPO-MOVIMIENTO            PIC X(1).
           05  WT-MONTO-TARJ                 PIC 9(12)V99.

       01  DB-EXTRACTO-DETALLE.
      *     05  WE-ID-CLIENTE                  PIC 9(10).
           05  WE-DOC-CLIENTE                 PIC 9(10).
           05  WE-FECHA-MOVIMIENTO            PIC X(10).
           05  WE-TIPO-MOVIMIENTO             PIC X(15).
           05  WE-IMPORTE                     PIC S9(9)V99.
           05  WE-SALDO                       PIC S9(9)V99.

       01  DB-DEUDA-DETALLE.
           05  WE-ID-CLIENTE                  PIC 9(10).
           05  WE-NRO-TARJETA                 PIC X(16).
           05  WE-FECHA-VENCIMIENTO           PIC X(10).
           05  WE-LIMITE-TARJETA              PIC 9(12)V99.
           05  WE-CONSUMO                     PIC S9(12)V99.
           05  WE-PAGO                        PIC S9(12)V99.
           05  WE-DEUDA                       PIC S9(12)V99.
           05  WE-CREDITO-DISPONIBLE          PIC S9(12)V99.



       01  DB-TARJETA-ACTIVA.
           03 WX-TARJETA-ACTIVA.
              05 WX-ID-CLIENTE                 PIC 9(10).
              05 WX-NRO-TARJETA                PIC X(16).
              05 WX-SALDO-ACTUAL               PIC 9(12)V99.

       01  DB-DEUDA-DET-CON-PAGO.
           05  WP-ID-CLIENTE                 PIC 9(10).
           05  WP-NRO-TARJETA                PIC X(16).
           05  WP-FECHA-MOVIMIENTO           PIC X(10).
           05  WP-TIPO-MOVIMIENTO            PIC X(15).
           05  WP-MONTO                      PIC S9(12)V99.


      *    EXEC SQL
      *        END DECLARE SECTION
      *    END-EXEC

       01  WS-HEADER-P0  PIC X(80) VALUE ALL "-".

       01  WS-HEADER-P1  PIC X(80) VALUE
           "NOMBRE        APELLIDOS       FECHA_ULT_MOV   SALDO_ACTUAL".

       01  WS-HEADER-P2  PIC X(80) VALUE
           "*--------------------------------------------------------*".

       01  WS-CABECERA-REP-MOV.
           05 FILLER                 PIC X(1)  VALUE X"0A". *> Salto de
           05 FILLER                 PIC X(95) VALUE ALL "-".
           05 FILLER                 PIC X(1)  VALUE X"0A". *> Salto de
           05 FILLER                 PIC X(54) VALUE
               "*********************   REPORTE DE MOVIMIENTOS DE TARJ".
           05 FILLER                 PIC X(40) VALUE
               "ETAS   *********************************".
           05 FILLER                 PIC X(1)  VALUE X"0A". *> Salto de
           05 FILLER                 PIC X(95) VALUE ALL "-".
           05 FILLER                 PIC X(1)  VALUE X"0A". *> Salto de
           05 FILLER                 PIC X(54) VALUE
               "ID_CLIENTE| NRO_TARJETA    | VENCIMIENTO | LIMITE |  C".
           05 FILLER                 PIC X(40) VALUE
               "ONSUMO  | PAGO  |    DEUDA  | DISPONIBLE".
           05 FILLER                 PIC X(1)  VALUE X"0A". *> Salto de

       01  WS-DETALLE-PAGO-CONSUMO.
           05 FILLER                 PIC X(1)  VALUE X"0A". *> Salto de
           05 FILLER                 PIC X(95) VALUE ALL "-".
           05 FILLER                 PIC X(1)  VALUE X"0A". *> Salto de
           05 FILLER                 PIC X(54) VALUE
               "************************   DETALLE DE CONSUMOS Y PAGOS".
           05 FILLER                 PIC X(40) VALUE
               "     ***********************************".
           05 FILLER                 PIC X(1)  VALUE X"0A". *> Salto de
           05 FILLER                 PIC X(95) VALUE ALL "-".
           05 FILLER                 PIC X(1)  VALUE X"0A". *> Salto de
           05 FILLER                 PIC X(54) VALUE
               "      ID_CLIENTE| NRO_TARJETA    |   FECHA  |  MOVIMIE".
           05 FILLER                 PIC X(40) VALUE
               "NTO    |   MONTO                        ".
           05 FILLER                 PIC X(1)  VALUE X"0A". *> Salto de



       01  WS-CABECERA-TARJ-ACT.

           03 FILLER-BREAK   PIC X(1)  VALUE X"0A".  *> Salto de línea
           03 FILLER-1       PIC X(42) VALUE ALL "*".
           03 FILLER-BREAK-1 PIC X(1)  VALUE X"0A".  *> Salto de línea
           03 FILLER-2       PIC X(42) VALUE
              "ID_CLIENTE     NRO_TARJETA          SALDO ".
           03 FILLER-BREAK-2 PIC X(1)  VALUE X"0A".  *> Salto de línea

       01  PRTEC                 PIC X(120).

       01  WS-DATOS-TARJETA.
           05  WS-ID-CLIENTE                  PIC 9(05).
           05  WS-DOC-CLI                     PIC X(12).
           05  WS-NUMERO-TARJ                 PIC X(16).
           05  WS-LIMITE-TARJ                 PIC S9(9)V99.
           05  WS-EXISTE-TARJ                 PIC X(01).

       01  WS-DATOS-CONSUMO.
           05  WS-ID-MOVIMIENTO              PIC 9(10).
           05  WS-NRO-TARJETA                PIC X(16).
           05  WS-FECHA-MOVIMIENTO           PIC X(10).
           05  WS-TIPO-MOVIMIENTO            PIC X(1).
           05  WS-MONTO-TARJ                 PIC S9(12)V99.

       01  WS-TARJETA-DETAIL.
           03 REG-DETAIL OCCURS 100 TIMES.
              05  RPT-ID-CLIENTE                  PIC 9(10).
              05  RPT-NRO-TARJETA                 PIC X(16).
              05  RPT-FECHA-VENCIMIENTO           PIC X(10).
              05  RPT-LIMITE-TARJETA              PIC ZZZZZ9.99-.
              05  RPT-CONSUMO                     PIC ZZZZZ9.99-.
              05  RPT-PAGO                        PIC ZZZZZ9.99-.
              05  RPT-DEUDA                       PIC ZZZZZ9.99-.
              05  RPT-CREDITO-DISPONIBLE          PIC ZZZZZ9.99-.


       01  WS-PAGO-CONSUMO-DETAIL.
           03 WS-REG-DETAIL OCCURS 100 TIMES.
              05  RP-WP-ID-CLIENTE                 PIC 9(10).
              05  RP-WP-NRO-TARJETA                PIC X(16).
              05  RP-WP-FECHA-MOVIMIENTO           PIC X(10).
              05  RP-WP-TIPO-MOVIMIENTO            PIC X(15).
              05  RP-WP-MONTO                      PIC ZZZZZ9.99-.



       01  WT-PREC-TARJ-ACTIVA.
           05 WT-PRE-TARJ-ACTIVA       OCCURS 10 TIMES.
              08 WX-PRC-ID-CLIENTE             PIC 9(10).
              08 WX-ESPACIO-1                  PIC X(01).
              08 WX-PRC-NRO-TARJETA            PIC X(16).
              08 WX-ESPACIO-2                  PIC X(01).
              08 WX-PRC-SALDO-ACTUAL           PIC ZZZZZZZZZ.ZZ.

       01  RXT-FECHA-MOVIMIENTO           PIC X(10).
       01  FIN-DATOS                      PIC X VALUE 'N'.
       01  WS-WORK-AREAS.
           05  FILE-CHECK-KEY          PIC X(2).
           05  ERR-MSG                 PIC X(128).
           05  ERR-CODE                PIC X(2).
       01  OPCION                      PIC 9.
       01  WS-SALDO                    PIC ZZZZZZ9.99.
       01  WS-MONTO                    PIC S9(8)V99.

       01  WX-TIPO-MOVIMIENTO          PIC 9.
       01  WS-CONT                     PIC 999.
       01  WS-CONTX                    PIC 999.
       01  WS-DETX                     PIC 999.
       01  WS-DET                      PIC 999.
       01  WS-INTENTOS                 PIC 99.
       01  WS-INTENTOS-P               PIC ZZ.
       01  WS-MENSAJE                  PIC X(25).
       01  WS-RESU-INT                 PIC ZZ.

       01  WS-ACUM-MES-FT              PIC ZZZZZ9.99-.
       01  WS-LIQUIDACION-MES-FT       PIC ZZZZZ9.99-.
       01  WS-DEUDA-TOTAL-FT           PIC ZZZZZ9.99-.
       01  WS-CREDITO-DISPONIBLE-FT    PIC ZZZZZ9.99-.
       01  WS-LIMITE-TARJETA-FT        PIC ZZZZZ9.99-.
       01  WS-DEUDA-TOTAL              PIC 9(12)V99.
       01  WS-CREDITO-DISPONIBLE       PIC 9(12)V99.
       01  WS-VALIDO                   PIC X(01).

       LINKAGE SECTION.
       01 LK-USER-ID PIC 9(1).  *> Recibirá un ID de usuario

       PROCEDURE DIVISION USING LK-USER-ID.
       MAIN-PROGRAM.
           PERFORM 0100-INICIO.
           PERFORM 100-MENU.

       100-MENU.
           DISPLAY "===================================="
           DISPLAY "    SISTEMA DE TARJ. CREDITO        "
           DISPLAY "===================================="
           DISPLAY "1 - Registrar nuevos contratos".
           DISPLAY "2 - Registrar pagos y consumos"
           DISPLAY "3 - Generar reportes de deuda mensual"
           DISPLAY "4 - Salir"
           DISPLAY "Seleccione una opción: "
           ACCEPT OPCION

           EVALUATE OPCION
               WHEN 1
                   PERFORM 100-REGISTRAR-NUEVO-CONTRATO
               WHEN 2
                   PERFORM 200-REGISTRAR-PAGOS-CONSUMO
               WHEN 3
                   PERFORM 300-GENERAR-REPORTE-DEUDA
               WHEN 4
                   PERFORM 0300-FIN
               WHEN OTHER
                   DISPLAY "Opción inválida, intente nuevamente."
           END-EVALUATE.

       300-GENERAR-REPORTE-DEUDA.
           PERFORM 310-APERTURA-ARCHIVO.
           PERFORM 100-CONSULTA-CLIENTE.
           PERFORM 320-EXTRAE-DEUDA-CURSOR.
           PERFORM 320-EXTRAE-DET-DEUDA-CURSOR.
           PERFORM 350-IMPRIME-REPORTE.
           PERFORM 100-MENU.

       350-IMPRIME-REPORTE.

           MOVE SPACES TO RPT-DETALLE-CLI
           MOVE WS-CABECERA-REP-MOV TO RPT-DETALLE-CLI
           WRITE RPT-DETALLE-CLI
           DISPLAY WS-CABECERA-REP-MOV
           MOVE SPACES TO RPT-DETALLE-CLI
           PERFORM VARYING WS-CONTX FROM 1 BY 1 UNTIL
                                                    WS-CONTX > WS-CONT

            STRING RPT-ID-CLIENTE         (WS-CONTX) " "
                   RPT-NRO-TARJETA        (WS-CONTX) " "
                   RPT-FECHA-VENCIMIENTO  (WS-CONTX) " "
                   RPT-LIMITE-TARJETA     (WS-CONTX) " "
                   RPT-CONSUMO            (WS-CONTX) " "
                   RPT-PAGO               (WS-CONTX) " "
                   RPT-DEUDA              (WS-CONTX) " "
                   RPT-CREDITO-DISPONIBLE (WS-CONTX)
                  INTO RPT-DETALLE-CLI
              DISPLAY RPT-DETALLE-CLI(1:100)
              WRITE RPT-DETALLE-CLI
           END-PERFORM.

           MOVE SPACES TO RPT-DETALLE-CLI
           MOVE WS-DETALLE-PAGO-CONSUMO TO RPT-DETALLE-CLI
           WRITE RPT-DETALLE-CLI
           DISPLAY WS-DETALLE-PAGO-CONSUMO
           MOVE SPACES TO RPT-DETALLE-CLI
           PERFORM VARYING WS-DETX FROM 1 BY 1 UNTIL
                                                    WS-DETX > WS-DET

            STRING "      "
                   RP-WP-ID-CLIENTE         (WS-DETX) " "
                   RP-WP-NRO-TARJETA        (WS-DETX) " "
                   RP-WP-FECHA-MOVIMIENTO   (WS-DETX) " "
                   RP-WP-TIPO-MOVIMIENTO    (WS-DETX) " "
                   RP-WP-MONTO              (WS-DETX)
                  INTO RPT-DETALLE-CLI
              DISPLAY RPT-DETALLE-CLI(1:100)
              WRITE RPT-DETALLE-CLI
           END-PERFORM.
           CLOSE RPT-FILE-DETAIL.

       200-VERIFICA-NRO-TARJETA.

           IF WS-INTENTOS > 3 THEN
             DISPLAY "Número de intentos superado. "
             DISPLAY "Ingrese otro documento."
             PERFORM 200-REGISTRAR-PAGOS-CONSUMO
           END-IF.

           DISPLAY "==============================================="
           DISPLAY "  INGRESE EL NÚMERO DE TARJETA CORRECTAMENTE  "

           IF WS-INTENTOS > 0 THEN
              STRING"   (Número de intentos " WS-INTENTOS-P " ) " INTO
              WS-MENSAJE
              DISPLAY WS-MENSAJE
           ELSE
              DISPLAY "   (Tiene hsta 3 intentos) "
           END-IF

           DISPLAY "==============================================="
           DISPLAY "  Ingrese el Número de Tarjeta: ".
           ACCEPT WS-NRO-TARJETA.
           MOVE WS-NRO-TARJETA TO WT-NRO-TARJETA.

      *    EXEC SQL
      *     SELECT CASE
      *              WHEN EXISTS (SELECT 1
      *                             FROM BANCO.TARJETAS
      *                         WHERE NRO_TARJETA =TRIM(:WT-NRO-TARJETA)
      *                                  LIMIT 1)
      *              THEN 'S'
      *              ELSE 'N'
      *            END
      *     INTO :WT-EXISTE-TARJ
      *     END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WT-EXISTE-TARJ
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 1 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WT-NRO-TARJETA
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 16 TO SQL-LEN(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA
                    .

           MOVE WT-EXISTE-TARJ TO WS-EXISTE-TARJ.
           IF WS-EXISTE-TARJ = 'S' THEN
              DISPLAY "Tarjeta encontrada. Continuando..."
              PERFORM 210-PEDIR-DATOS-MOVIMIENTO
           ELSE
            DISPLAY "Número de tarjeta incorrecto. Intente nuevamente."

               ADD 1 TO WS-INTENTOS
               MOVE WS-INTENTOS TO WS-INTENTOS-P
               PERFORM 200-VERIFICA-NRO-TARJETA
           END-IF.

       210-PEDIR-DATOS-MOVIMIENTO.

           DISPLAY "Ingrese Tipo de Movimient(P = Pago, C = Consumo): ".
           ACCEPT WS-TIPO-MOVIMIENTO

           IF WS-TIPO-MOVIMIENTO NOT = 'P' AND
                                     WS-TIPO-MOVIMIENTO NOT = 'C' THEN
               DISPLAY "Tipo de movimien inválido. Debe ser 'P' o 'C'."
               PERFORM 210-PEDIR-DATOS-MOVIMIENTO
           END-IF

           MOVE 'N' TO WS-VALIDO
           PERFORM UNTIL WS-VALIDO = 'S'
               DISPLAY "Ingrese el Monto: "
               ACCEPT WS-MONTO-TARJ
               IF WS-MONTO-TARJ < 0 THEN
                  DISPLAY "Error: El monto no puede ser negativo."
                  MOVE 'N' TO WS-VALIDO
               ELSE
                  MOVE 'S' TO WS-VALIDO
               END-IF
           END-PERFORM

           MOVE WS-DOC-CLI TO WT-DOC-CLI

           *> Obtener el ID del Cliente
      *    EXEC SQL
      *      SELECT ID_CLIENTE
      *       INTO :WT-ID-CLIENTE
      *       FROM CLIENTES WHERE
      *            DOC_CLIENTE =TRIM(:WT-DOC-CLI) LIMIT 1
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 3 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WT-DOC-CLI
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 12 TO SQL-LEN(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA
           MOVE SQL-VAR-0002 TO WT-ID-CLIENTE
                   .

           *> Obtener la fecha actual para el movimiento
      *    EXEC SQL
      *        SELECT CURDATE()
      *        INTO :WT-FECHA-MOVIMIENTO
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WT-FECHA-MOVIMIENTO
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 10 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
                   .
           display "fecha: "WT-FECHA-MOVIMIENTO

           *> Obtener el siguiente ID_MOVIMIENTO
      *    EXEC SQL
      *        SELECT COALESCE(MAX(ID_MOVIMIENTO), 0) + 1
      *        INTO :WT-ID-MOVIMIENTO
      *        FROM BANCO.MOVIMIENTOS_TARJETAS
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-3 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0008
               MOVE '3' TO SQL-TYPE(1)
               MOVE 6 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-3
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-3
                               SQLCA
           MOVE SQL-VAR-0008 TO WT-ID-MOVIMIENTO
                   .

       220-INSERTA-PAGO-CONSUMO.

           MOVE WS-TIPO-MOVIMIENTO  TO WT-TIPO-MOVIMIENTO
           MOVE WS-MONTO-TARJ       TO WT-MONTO-TARJ
           DISPLAY WT-ID-MOVIMIENTO
           DISPLAY WT-ID-CLIENTE
           DISPLAY WT-NRO-TARJETA
           DISPLAY WT-FECHA-MOVIMIENTO
           DISPLAY WT-TIPO-MOVIMIENTO
           DISPLAY WT-MONTO-TARJ
           *> Inserta el movimiento en la base de datos
      *    EXEC SQL
      *        INSERT INTO BANCO.MOVIMIENTOS_TARJETAS
      *               (ID_MOVIMIENTO, ID_CLIENTE,
      *                NRO_TARJETA, FECHA_MOV, TIPO_MOV, MONTO)
      *        VALUES (:WT-ID-MOVIMIENTO, :WT-ID-CLIENTE,
      *                :WT-NRO-TARJETA, :WT-FECHA-MOVIMIENTO,
      *                :WT-TIPO-MOVIMIENTO, :WT-MONTO-TARJ)
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-4 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0008
               MOVE '3' TO SQL-TYPE(1)
               MOVE 6 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(2)
               MOVE 3 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 WT-NRO-TARJETA
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 16 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 WT-FECHA-MOVIMIENTO
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 10 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 WT-TIPO-MOVIMIENTO
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 1 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0009
               MOVE '3' TO SQL-TYPE(6)
               MOVE 8 TO SQL-LEN(6)
               MOVE X'02' TO SQL-PREC(6)
               MOVE 6 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-4
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WT-ID-MOVIMIENTO
             TO SQL-VAR-0008
           MOVE WT-ID-CLIENTE
             TO SQL-VAR-0002
           MOVE WT-MONTO-TARJ
             TO SQL-VAR-0009
           CALL 'OCSQLEXE' USING SQL-STMT-4
                               SQLCA
      *    EXEC SQL
      *      COMMIT
      *    END-EXEC
           CALL 'OCSQLCMT' USING SQLCA END-CALL
           IF SQLCODE = 0 THEN
              DISPLAY "Movimiento registrado exitosamente...!"
           ELSE
              DISPLAY "Error en Inserta Movimiento....!"
           END-IF.

       200-REGISTRAR-PAGOS-CONSUMO.

           MOVE ZEROES TO WS-INTENTOS WS-INTENTOS-P WS-RESU-INT
           PERFORM 100-CONSULTA-CLIENTE
           PERFORM 110-MOSTRAR-TARJETA-ACTIVA.
           PERFORM 200-VERIFICA-NRO-TARJETA
           PERFORM 220-INSERTA-PAGO-CONSUMO.
           PERFORM 230-ACTUALIZA-PAGO-CONSUMO.
           PERFORM 100-MENU.

       110-MOSTRAR-TARJETA-ACTIVA.
           PERFORM 250-EXTRAE-TARJ-ACTIVA.

       250-EXTRAE-TARJ-ACTIVA.
           INITIALIZE DB-TARJETA-ACTIVA WT-PREC-TARJ-ACTIVA PRTEC
           MOVE ZEROES TO WS-CONT
           MOVE WS-DOC-CLI TO WT-DOC-CLI

      *    EXEC SQL
      *        DECLARE CUR_TARJ-ACT CURSOR FOR

      *                SELECT ID_CLIENTE,
      *                       NRO_TARJETA,
      *                       LIMITE_TARJETA - ACUM_MES SALDO_ACTUAL
      *                 FROM BANCO.TARJETAS
      *                WHERE ID_CLIENTE = (SELECT ID_CLIENTE
      *                                      FROM BANCO.CLIENTES
      *                                      WHERE CTA_ACTIVA = 1
      *                                        AND DOC_CLIENTE
      *                                             =TRIM(:WT-DOC-CLI))
      *    END-EXEC.
                   .
      *    EXEC SQL
      *        OPEN CUR_TARJ-ACT
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-5 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WT-DOC-CLI
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 12 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-5
                                   SQLCA
           END-IF
           CALL 'OCSQLOCU' USING SQL-STMT-5
                               SQLCA
           END-CALL
                   .
           PERFORM UNTIL SQLCODE = 100
      *        EXEC SQL
      *          FETCH CUR_TARJ-ACT
      *          INTO
      *            :WX-ID-CLIENTE,
      *            :WX-NRO-TARJETA,
      *            :WX-SALDO-ACTUAL
      *        END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0019
           MOVE '3' TO SQL-TYPE(1)
           MOVE 6 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             WX-NRO-TARJETA
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 16 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             SQL-VAR-0020
           MOVE '3' TO SQL-TYPE(3)
           MOVE 8 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
           MOVE 3 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-5
                               SQLCA
           MOVE SQL-VAR-0019 TO WX-ID-CLIENTE
           MOVE SQL-VAR-0020 TO WX-SALDO-ACTUAL

            IF SQLCODE NOT = 100 THEN
               *> Formatear línea del reporte
               ADD 1 TO WS-CONT
               MOVE WX-ID-CLIENTE      TO WX-PRC-ID-CLIENTE   (WS-CONT)
               MOVE WX-NRO-TARJETA     TO WX-PRC-NRO-TARJETA  (WS-CONT)
               MOVE WX-SALDO-ACTUAL    TO WX-PRC-SALDO-ACTUAL (WS-CONT)
           ELSE
               DISPLAY ' NO TIENE MAS FILAS LA TABLA DE DB'
           END-IF
           END-PERFORM.
      *    EXEC SQL CLOSE CUR_TARJ-ACT END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-5
                               SQLCA
                                               .
           DISPLAY WS-CABECERA-TARJ-ACT
           PERFORM VARYING WS-CONTX FROM 1 BY 1 UNTIL WS-CONTX > WS-CONT

              STRING WX-PRC-ID-CLIENTE  (WS-CONTX) "  "
                     WX-PRC-NRO-TARJETA (WS-CONTX) "  "
                     WX-PRC-SALDO-ACTUAL(WS-CONT)
               INTO PRTEC
             DISPLAY PRTEC
           END-PERFORM.

       230-ACTUALIZA-PAGO-CONSUMO.

           IF WT-TIPO-MOVIMIENTO = 'C' THEN
      *-- Actualizar el saldo del mes para tipo consumo
      *       EXEC SQL
      *        UPDATE TARJETAS
      *           SET ACUM_MES = ACUM_MES + :WT-MONTO-TARJ
      *         WHERE NRO_TARJETA =TRIM(:WT-NRO-TARJETA)
      *       END-EXEC
           IF SQL-PREP OF SQL-STMT-6 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0009
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WT-NRO-TARJETA
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 16 TO SQL-LEN(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-6
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WT-MONTO-TARJ
             TO SQL-VAR-0009
           CALL 'OCSQLEXE' USING SQL-STMT-6
                               SQLCA
           END-IF
           PERFORM 010-COMMIT
           IF WT-TIPO-MOVIMIENTO = 'P' THEN
      *-- Actualizar el saldo del mes para tipo pago
      *       EXEC SQL
      *        UPDATE TARJETAS
      *           SET ACUM_MES = ACUM_MES - :WT-MONTO-TARJ
      *         WHERE NRO_TARJETA =TRIM(:WT-NRO-TARJETA)
      *       END-EXEC
           IF SQL-PREP OF SQL-STMT-7 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0009
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WT-NRO-TARJETA
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 16 TO SQL-LEN(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-7
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WT-MONTO-TARJ
             TO SQL-VAR-0009
           CALL 'OCSQLEXE' USING SQL-STMT-7
                               SQLCA
           END-IF
           PERFORM 010-COMMIT

      *-- Actualizar la liquidación del mes
      *    EXEC SQL

      *    UPDATE TARJETAS
      *    SET LIQUIDACION_MES =
      *        CASE
      *            WHEN ACUM_MES < 0 THEN LIMITE_TARJETA + ACUM_MES
      *            ELSE LIMITE_TARJETA - ACUM_MES
      *        END
      *    WHERE NRO_TARJETA =TRIM(:WT-NRO-TARJETA)

      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-8 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WT-NRO-TARJETA
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 16 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-8
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-8
                               SQLCA
           PERFORM 010-COMMIT.

       010-COMMIT.
      *    EXEC SQL
      *       COMMIT
      *    END-EXEC.
           CALL 'OCSQLCMT' USING SQLCA END-CALL
                   .

       100-CONSULTA-CLIENTE.
           DISPLAY "Ingrese (-1) para salir"
           DISPLAY "Ingrese Documento Cliente: "
           ACCEPT WS-DOC-CLI

           PERFORM 100-EXISTE-CLIENTE

           IF WS-EXISTE-CLIENTE = 'N' THEN
              DISPLAY "Ingrese (-1) para salir"
              DISPLAY "Doc. No Existe: " WS-DOC-CLI
              DISPLAY "Favor, Ingresar Nuevamente: "
              IF WS-DOC-CLI = "-1" THEN
                 PERFORM 0300-FIN
              END-IF
              PERFORM 100-CONSULTA-CLIENTE
           END-IF.

       110-VERIFICA-TARJETA.
           MOVE WS-DOC-CLI TO WT-DOC-CLI.
      *    EXEC SQL
      *    SELECT CASE
      *             WHEN EXISTS (SELECT 1 FROM BANCO.TARJETAS
      *                          WHERE ID_CLIENTE =
      *                                (SELECT ID_CLIENTE
      *                                 FROM BANCO.CLIENTES
      *                       WHERE DOC_CLIENTE = TRIM(:WT-DOC-CLI)
      *                                 LIMIT 1))
      *             THEN 'S'
      *             ELSE 'N'
      *           END
      *    INTO :WT-EXISTE-TARJ
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-9 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WT-EXISTE-TARJ
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 1 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WT-DOC-CLI
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 12 TO SQL-LEN(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-9
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-9
                               SQLCA
                   .
           MOVE WT-EXISTE-TARJ TO WS-EXISTE-TARJ.
       120-OBTENER-NRO-TARJETA.

      *     CALL "GENTARJ001" USING WS-NUMERO-TARJ.

      *    EXEC SQL
      *    SELECT GenerarNumeroTarjeta()
      *      INTO :WT-NUMERO-TARJ
      *    FROM DUAL
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-10 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WT-NUMERO-TARJ
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 16 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-10
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-10
                               SQLCA
                   .

       130-INSERTA-TARJETA.

           MOVE WS-DOC-CLI TO WT-DOC-CLI
      *    EXEC SQL
      *       SELECT ID_CLIENTE
      *         INTO :WT-ID-CLIENTE
      *         FROM BANCO.CLIENTES
      *        WHERE DOC_CLIENTE =TRIM(:WT-DOC-CLI)
      *      LIMIT 1
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-11 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 3 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WT-DOC-CLI
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 12 TO SQL-LEN(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-11
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-11
                               SQLCA
           MOVE SQL-VAR-0002 TO WT-ID-CLIENTE
                   .

            MOVE WT-ID-CLIENTE  TO WTT-ID-CLIENTE.
            MOVE WS-LIMITE-TARJ TO WT-LIMITE-TARJ.

      *    EXEC SQL
      *      INSERT INTO BANCO.TARJETAS
      *         VALUES(TRIM(:WTT-ID-CLIENTE),
      *                TRIM(:WT-NUMERO-TARJ),
      *                (SELECT CURDATE() + INTERVAL 10 YEAR FROM DUAL),
      *                :WT-LIMITE-TARJ,
      *                0,
      *                :WT-LIMITE-TARJ)
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-12 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(1)
               MOVE 3 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WT-NUMERO-TARJ
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 16 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(3)
               MOVE 6 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(4)
               MOVE 6 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
               MOVE 4 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-12
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WTT-ID-CLIENTE
             TO SQL-VAR-0003
           MOVE WT-LIMITE-TARJ
             TO SQL-VAR-0004
           MOVE WT-LIMITE-TARJ
             TO SQL-VAR-0004
           CALL 'OCSQLEXE' USING SQL-STMT-12
                               SQLCA
      *    EXEC SQL
      *        COMMIT
      *    END-EXEC.
           CALL 'OCSQLCMT' USING SQLCA END-CALL
                   .
            IF SQLCODE = 0 THEN
                 DISPLAY "Tarjeta creada.! NRO_TARJ:"WT-NUMERO-TARJ
            ELSE
                DISPLAY "Error en creacion de tarjeta....!"
            END-IF.

       140-VALIDAR-LIMITE-TARJ.

           PERFORM UNTIL WS-LIMITE-TARJ > 5000
              DISPLAY "Ingrese (-1) para salir"
              DISPLAY "Ingrese el Limite (Debe ser mayor a 5000): "
              ACCEPT WS-LIMITE-TARJ
              IF WS-LIMITE-TARJ = -1 THEN
                 PERFORM 0300-FIN
              END-IF
           END-PERFORM
           DISPLAY "Límite de tarjeta aceptado: " WS-LIMITE-TARJ.

       100-REGISTRAR-NUEVO-CONTRATO.

           PERFORM 100-CONSULTA-CLIENTE
           PERFORM 110-VERIFICA-TARJETA

           IF WS-EXISTE-TARJ = 'N' THEN
              PERFORM 140-VALIDAR-LIMITE-TARJ
              PERFORM 120-OBTENER-NRO-TARJETA
              PERFORM 130-INSERTA-TARJETA
           ELSE
               DISPLAY "Tarjeta Credito ya Existe...!"
           END-IF.
           PERFORM 100-MENU.

       320-EXTRAE-DET-DEUDA-CURSOR.

           MOVE ZEROES     TO WS-DET WS-DETX
           MOVE WS-DOC-CLI TO WT-DOC-CLI

           INITIALIZE DB-DEUDA-DET-CON-PAGO
      *    EXEC SQL
      *        DECLARE CURSOR_DEUDA-DET CURSOR FOR

      *     SELECT
      *         ID_CLIENTE,
      *         NRO_TARJETA,
      *         FECHA_MOV,
      *         CASE
      *             WHEN TIPO_MOV = 'P' THEN 'PAGO'
      *             ELSE 'CONSUMO'
      *         END TIPO_MOV,
      *         CASE
      *             WHEN TIPO_MOV = 'P' THEN MONTO * (- 1)
      *             ELSE MONTO
      *         END MONTO
      *     FROM
      *         BANCO.MOVIMIENTOS_TARJETAS
      *     WHERE
      *         ID_CLIENTE = (SELECT
      *                 ID_CLIENTE
      *             FROM
      *                 BANCO.CLIENTES
      *             WHERE CTA_ACTIVA = 1
      *               AND DOC_CLIENTE =TRIM(:WT-DOC-CLI))
      *     ORDER BY FECHA_MOV ASC


      *    END-EXEC.
                   .
      *    EXEC SQL
      *        OPEN CURSOR_DEUDA-DET
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-13 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WT-DOC-CLI
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 12 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-13
                                   SQLCA
           END-IF
           CALL 'OCSQLOCU' USING SQL-STMT-13
                               SQLCA
           END-CALL
                   .
           DISPLAY "SQLCODE "SQLCODE
           PERFORM UNTIL SQLCODE = 100
      *        EXEC SQL
      *          FETCH CURSOR_DEUDA-DET
      *          INTO
      *            :WP-ID-CLIENTE,
      *            :WP-NRO-TARJETA,
      *            :WP-FECHA-MOVIMIENTO,
      *            :WP-TIPO-MOVIMIENTO,
      *            :WP-MONTO
      *        END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0021
           MOVE '3' TO SQL-TYPE(1)
           MOVE 6 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             WP-NRO-TARJETA
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 16 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             WP-FECHA-MOVIMIENTO
           MOVE 'X' TO SQL-TYPE(3)
           MOVE 10 TO SQL-LEN(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             WP-TIPO-MOVIMIENTO
           MOVE 'X' TO SQL-TYPE(4)
           MOVE 15 TO SQL-LEN(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             SQL-VAR-0022
           MOVE '3' TO SQL-TYPE(5)
           MOVE 8 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
           MOVE 5 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-13
                               SQLCA
           MOVE SQL-VAR-0021 TO WP-ID-CLIENTE
           MOVE SQL-VAR-0022 TO WP-MONTO
            IF SQLCODE NOT = 100 THEN
               *> Formatear línea del reporte

               ADD 1 TO WS-DET

               MOVE WP-ID-CLIENTE        TO RP-WP-ID-CLIENTE    (WS-DET)
               MOVE WP-NRO-TARJETA       TO RP-WP-NRO-TARJETA   (WS-DET)
               MOVE WP-FECHA-MOVIMIENTO
                                         TO RP-WP-FECHA-MOVIMIENTO
                                                                (WS-DET)
               MOVE WP-TIPO-MOVIMIENTO   TO RP-WP-TIPO-MOVIMIENTO
                                                                (WS-DET)
               MOVE WP-MONTO             TO RP-WP-MONTO         (WS-DET)


           ELSE
               DISPLAY ' NO TIENE MAS FILAS LA TABLA DE DB'
           END-IF
           END-PERFORM.
      *    EXEC SQL CLOSE CURSOR_DEUDA-DET END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-13
                               SQLCA
                                                   .


       320-EXTRAE-DEUDA-CURSOR.
           MOVE ZEROES     TO WS-CONT WS-CONTX
           MOVE WS-DOC-CLI TO WT-DOC-CLI

           INITIALIZE DB-DEUDA-DETALLE
      *    EXEC SQL
      *        DECLARE CURSOR_DEUDA CURSOR FOR

      *           SELECT
      *               D.ID_CLIENTE,
      *               D.NRO_TARJETA,
      *               D.FECHA_VENCIMIENTO,
      *               D.LIMITE_TARJETA,
      *               IFNULL(CONSUMO.MONTO_TOTAL, 0) AS CONSUMO,
      *               IFNULL(PAGO.MONTO_TOTAL, 0) AS PAGO,
      *               IFNULL(CONSUMO.MONTO_TOTAL, 0) -
      *                          IFNULL(PAGO.MONTO_TOTAL, 0) AS DEUDA,
      *               D.LIQUIDACION_MES
      *           FROM TARJETAS D
      *           LEFT JOIN (
      *               SELECT ID_CLIENTE, SUM(MONTO) AS MONTO_TOTAL
      *               FROM BANCO.MOVIMIENTOS_TARJETAS
      *               WHERE TIPO_MOV = 'C'
      *               GROUP BY ID_CLIENTE
      *           ) CONSUMO ON D.ID_CLIENTE = CONSUMO.ID_CLIENTE
      *           LEFT JOIN (
      *               SELECT ID_CLIENTE, SUM(MONTO) AS MONTO_TOTAL
      *               FROM BANCO.MOVIMIENTOS_TARJETAS
      *               WHERE TIPO_MOV = 'P'
      *               GROUP BY ID_CLIENTE
      *           ) PAGO ON D.ID_CLIENTE = PAGO.ID_CLIENTE
      *           WHERE D.ID_CLIENTE = (
      *               SELECT ID_CLIENTE
      *               FROM BANCO.CLIENTES
      *               WHERE CTA_ACTIVA = 1
      *               AND DOC_CLIENTE =TRIM(:WT-DOC-CLI)
      *           )

      *    END-EXEC.
                   .
      *    EXEC SQL
      *        OPEN CURSOR_DEUDA
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-14 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WT-DOC-CLI
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 12 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-14
                                   SQLCA
           END-IF
           CALL 'OCSQLOCU' USING SQL-STMT-14
                               SQLCA
           END-CALL
                   .
           DISPLAY "SQLCODE "SQLCODE
           PERFORM UNTIL SQLCODE = 100
      *        EXEC SQL
      *          FETCH CURSOR_DEUDA
      *          INTO
      *            :WE-ID-CLIENTE,
      *            :WE-NRO-TARJETA,
      *            :WE-FECHA-VENCIMIENTO,
      *            :WE-LIMITE-TARJETA,
      *            :WE-CONSUMO,
      *            :WE-PAGO,
      *            :WE-DEUDA,
      *            :WE-CREDITO-DISPONIBLE
      *        END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0013
           MOVE '3' TO SQL-TYPE(1)
           MOVE 6 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             WE-NRO-TARJETA
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 16 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             WE-FECHA-VENCIMIENTO
           MOVE 'X' TO SQL-TYPE(3)
           MOVE 10 TO SQL-LEN(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0014
           MOVE '3' TO SQL-TYPE(4)
           MOVE 8 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             SQL-VAR-0015
           MOVE '3' TO SQL-TYPE(5)
           MOVE 8 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
           SET SQL-ADDR(6) TO ADDRESS OF
             SQL-VAR-0016
           MOVE '3' TO SQL-TYPE(6)
           MOVE 8 TO SQL-LEN(6)
               MOVE X'02' TO SQL-PREC(6)
           SET SQL-ADDR(7) TO ADDRESS OF
             SQL-VAR-0017
           MOVE '3' TO SQL-TYPE(7)
           MOVE 8 TO SQL-LEN(7)
               MOVE X'02' TO SQL-PREC(7)
           SET SQL-ADDR(8) TO ADDRESS OF
             SQL-VAR-0018
           MOVE '3' TO SQL-TYPE(8)
           MOVE 8 TO SQL-LEN(8)
               MOVE X'02' TO SQL-PREC(8)
           MOVE 8 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-14
                               SQLCA
           MOVE SQL-VAR-0013 TO WE-ID-CLIENTE
           MOVE SQL-VAR-0014 TO WE-LIMITE-TARJETA
           MOVE SQL-VAR-0015 TO WE-CONSUMO
           MOVE SQL-VAR-0016 TO WE-PAGO
           MOVE SQL-VAR-0017 TO WE-DEUDA
           MOVE SQL-VAR-0018 TO WE-CREDITO-DISPONIBLE
            DISPLAY "SQLCODE 1111 "SQLCODE
            IF SQLCODE NOT = 100 THEN
               *> Formatear línea del reporte

               ADD 1 TO WS-CONT

               MOVE WE-ID-CLIENTE        TO RPT-ID-CLIENTE     (WS-CONT)
               MOVE WE-NRO-TARJETA       TO RPT-NRO-TARJETA    (WS-CONT)
               MOVE WE-FECHA-VENCIMIENTO
                                         TO RPT-FECHA-VENCIMIENTO
                                                               (WS-CONT)
               MOVE WE-LIMITE-TARJETA    TO RPT-LIMITE-TARJETA (WS-CONT)
               MOVE WE-CONSUMO           TO RPT-CONSUMO        (WS-CONT)
               MOVE WE-PAGO              TO RPT-PAGO           (WS-CONT)
               MOVE WE-DEUDA             TO RPT-DEUDA          (WS-CONT)
               MOVE WE-CREDITO-DISPONIBLE
                                         TO RPT-CREDITO-DISPONIBLE
                                                               (WS-CONT)

           ELSE
               DISPLAY ' NO TIENE MAS FILAS LA TABLA DE DB'
           END-IF
           END-PERFORM.
      *    EXEC SQL CLOSE CURSOR_DEUDA END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-14
                               SQLCA
                                               .

       310-APERTURA-ARCHIVO.
           OPEN OUTPUT RPT-FILE-DETAIL
           INITIALIZE WS-DOC-CLI.

       100-INSERTA-MOVIMIENTO.
           MOVE WS-MONTO TO WT-MONTO.
           MOVE WS-DOC-CLI TO WT-DOC-CLI.
      *    EXEC SQL
      *        SELECT InsertarMovimiento(:WT-DOC-CLI, :WT-MONTO)
      *          INTO :WS-NEWID-CTACTE
      *          FROM DUAL;
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-15 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0007
               MOVE '3' TO SQL-TYPE(1)
               MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WT-DOC-CLI
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 12 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(3)
               MOVE 6 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               MOVE 3 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-15
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WT-MONTO TO SQL-VAR-0006
           CALL 'OCSQLEXE' USING SQL-STMT-15
                               SQLCA
           MOVE SQL-VAR-0007 TO WS-NEWID-CTACTE
      *    EXEC SQL
      *        COMMIT
      *    END-EXEC.
           CALL 'OCSQLCMT' USING SQLCA END-CALL
                   .

       100-EXISTE-CLIENTE.
           MOVE WS-DOC-CLI TO WT-DOC-CLI.
      *    EXEC SQL
      *    SELECT
      *     IFNULL((SELECT 'S' FROM CLIENTES WHERE
      *                   DOC_CLIENTE =TRIM(:WT-DOC-CLI)
      *                   AND CTA_ACTIVA = 1
      *                   LIMIT 1), 'N')
      *       INTO :WS-EXISTE-CLIENTE
      *       FROM DUAL
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-16 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WS-EXISTE-CLIENTE
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 1 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WT-DOC-CLI
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 12 TO SQL-LEN(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-16
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-16
                               SQLCA
                   .

       0100-INICIO.
      *-----------------------------------------------------------------
      * CONNECT TO THE DATABASE
      * also possible with DSN: 'youruser/yourpasswd@yourODBC_DSN'
      *-----------------------------------------------------------------
           STRING  'DRIVER={MySQL ODBC 8.0 ANSI Driver};'
                   'SERVER=',DB-HOST,';'
                   'PORT=',DB-PORT,';'
                   'DATABASE=',DB-NAME,';'
                   'USER=',DB-USER,';'
                   'PASSWORD=',DB-PASSWORD,';'
      * example for DB specific ODBC parameter:
      * no compressed MySQL connection (would be the DEFAULT anyway)
                   'COMRESSED_PROTO=0;'
                   INTO BUFFER.
      *    EXEC SQL
      *        CONNECT TO :BUFFER
      *    END-EXEC
           MOVE 1024 TO SQL-LEN(1)
           CALL 'OCSQL'    USING BUFFER
                               SQL-LEN(1)
                               SQLCA
           END-CALL


      *    EXEC SQL
      *       SELECT DATABASE() INTO :BUFFER
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-17 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 BUFFER
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 1024 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-17
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-17
                               SQLCA

           DISPLAY 'BASE DE DATOS ACTUAL: ' BUFFER
           PERFORM SQLSTATE-CHECK.
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
      * REVISA SQLSTATE E IMPRIME ERRORES SI EXISTEN
      *-----------------------------------------------------------------
       SQLSTATE-CHECK.
           IF SQLCODE < 0
               DISPLAY 'SQLSTATE='  SQLSTATE,
                       ', SQLCODE=' SQLCODE
               IF SQLERRML > 0
                   DISPLAY 'SQL Error message:' SQLERRMC(1:SQLERRML)
               END-IF
               MOVE SQLCODE TO RETURN-CODE
      *         PERFORM 0400-MENU
      *         STOP RUN
           ELSE
               IF SQLCODE > 0 AND NOT = 100
                   DISPLAY 'SQLSTATE='  SQLSTATE,
                           ', SQLCODE=' SQLCODE
                   IF SQLERRML > 0
                       DISPLAY 'SQL Warning message:'
                               SQLERRMC(1:SQLERRML)
                   END-IF
               END-IF.
       0300-FIN.
           DISPLAY "Regresando a Menu Principal..."
           EXIT PROGRAM.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  BUFFER                   IN USE CHAR(1024)
      *  CTACTE               NOT IN USE
      *  CURSOR_DEUDA             IN USE CURSOR
      *  CURSOR_DEUDA-DET         IN USE CURSOR
      *  CUR_TARJ-ACT             IN USE CURSOR
      *  DB-DATOS-CLI         NOT IN USE
      *  DB-DATOS-CLI.WS-APELLIDOS NOT IN USE
      *  DB-DATOS-CLI.WS-FECHA-ULT-MOV NOT IN USE
      *  DB-DATOS-CLI.WS-NOMBRE NOT IN USE
      *  DB-DATOS-CLI.WS-SALDO-ACTUAL NOT IN USE
      *  DB-DATOS-CONSUMO     NOT IN USE
      *  DB-DATOS-CONSUMO.WT-FECHA-MOVIMIENTO NOT IN USE
      *  DB-DATOS-CONSUMO.WT-ID-MOVIMIENTO NOT IN USE
      *  DB-DATOS-CONSUMO.WT-MONTO-TARJ NOT IN USE
      *  DB-DATOS-CONSUMO.WT-NRO-TARJETA NOT IN USE
      *  DB-DATOS-CONSUMO.WT-TIPO-MOVIMIENTO NOT IN USE
      *  DB-DATOS-TARJETA     NOT IN USE
      *  DB-DATOS-TARJETA.WT-DOC-CLI NOT IN USE
      *  DB-DATOS-TARJETA.WT-EXISTE-TARJ NOT IN USE
      *  DB-DATOS-TARJETA.WT-ID-CLIENTE NOT IN USE
      *  DB-DATOS-TARJETA.WT-LIMITE-TARJ NOT IN USE
      *  DB-DATOS-TARJETA.WT-NUMERO-TARJ NOT IN USE
      *  DB-DATOS-TARJETA.WTT-ID-CLIENTE NOT IN USE
      *  DB-DEUDA-DET-CON-PAGO NOT IN USE
      *  DB-DEUDA-DET-CON-PAGO.WP-FECHA-MOVIMIENTO NOT IN USE
      *  DB-DEUDA-DET-CON-PAGO.WP-ID-CLIENTE NOT IN USE
      *  DB-DEUDA-DET-CON-PAGO.WP-MONTO NOT IN USE
      *  DB-DEUDA-DET-CON-PAGO.WP-NRO-TARJETA NOT IN USE
      *  DB-DEUDA-DET-CON-PAGO.WP-TIPO-MOVIMIENTO NOT IN USE
      *  DB-DEUDA-DETALLE     NOT IN USE
      *  DB-DEUDA-DETALLE.WE-CONSUMO NOT IN USE
      *  DB-DEUDA-DETALLE.WE-CREDITO-DISPONIBLE NOT IN USE
      *  DB-DEUDA-DETALLE.WE-DEUDA NOT IN USE
      *  DB-DEUDA-DETALLE.WE-FECHA-VENCIMIENTO NOT IN USE
      *  DB-DEUDA-DETALLE.WE-ID-CLIENTE NOT IN USE
      *  DB-DEUDA-DETALLE.WE-LIMITE-TARJETA NOT IN USE
      *  DB-DEUDA-DETALLE.WE-NRO-TARJETA NOT IN USE
      *  DB-DEUDA-DETALLE.WE-PAGO NOT IN USE
      *  DB-EXTRACTO-DETALLE  NOT IN USE
      *  DB-EXTRACTO-DETALLE.WE-DOC-CLIENTE NOT IN USE
      *  DB-EXTRACTO-DETALLE.WE-FECHA-MOVIMIENTO NOT IN USE
      *  DB-EXTRACTO-DETALLE.WE-IMPORTE NOT IN USE
      *  DB-EXTRACTO-DETALLE.WE-SALDO NOT IN USE
      *  DB-EXTRACTO-DETALLE.WE-TIPO-MOVIMIENTO NOT IN USE
      *  DB-TARJETA-ACTIVA    NOT IN USE
      *  DB-TARJETA-ACTIVA.WX-ID-CLIENTE NOT IN USE
      *  DB-TARJETA-ACTIVA.WX-NRO-TARJETA NOT IN USE
      *  DB-TARJETA-ACTIVA.WX-SALDO-ACTUAL NOT IN USE
      *  DB-TARJETA-ACTIVA.WX-TARJETA-ACTIVA NOT IN USE
      *  DB-VARS              NOT IN USE
      *  DB-VARS.BUFFER       NOT IN USE
      *  DB-VARS.CTACTE       NOT IN USE
      *  DB-VARS.ST-COUNT     NOT IN USE
      *  DB-VARS.WS-EXISTE-CLIENTE NOT IN USE
      *  DB-VARS.WS-NEWID-CTACTE NOT IN USE
      *  DB-VARS.WT-MONTO     NOT IN USE
      *  ST-COUNT             NOT IN USE
      *  WE-CONSUMO               IN USE THROUGH TEMP VAR SQL-VAR-0015 DECIMAL(15,2)
      *  WE-CREDITO-DISPONIBLE     IN USE THROUGH TEMP VAR SQL-VAR-0018 DECIMAL(15,2)
      *  WE-DEUDA                 IN USE THROUGH TEMP VAR SQL-VAR-0017 DECIMAL(15,2)
      *  WE-DOC-CLIENTE       NOT IN USE
      *  WE-FECHA-MOVIMIENTO  NOT IN USE
      *  WE-FECHA-VENCIMIENTO     IN USE CHAR(10)
      *  WE-ID-CLIENTE            IN USE THROUGH TEMP VAR SQL-VAR-0013 DECIMAL(11,0)
      *  WE-IMPORTE           NOT IN USE
      *  WE-LIMITE-TARJETA        IN USE THROUGH TEMP VAR SQL-VAR-0014 DECIMAL(15,2)
      *  WE-NRO-TARJETA           IN USE CHAR(16)
      *  WE-PAGO                  IN USE THROUGH TEMP VAR SQL-VAR-0016 DECIMAL(15,2)
      *  WE-SALDO             NOT IN USE
      *  WE-TIPO-MOVIMIENTO   NOT IN USE
      *  WP-FECHA-MOVIMIENTO      IN USE CHAR(10)
      *  WP-ID-CLIENTE            IN USE THROUGH TEMP VAR SQL-VAR-0021 DECIMAL(11,0)
      *  WP-MONTO                 IN USE THROUGH TEMP VAR SQL-VAR-0022 DECIMAL(15,2)
      *  WP-NRO-TARJETA           IN USE CHAR(16)
      *  WP-TIPO-MOVIMIENTO       IN USE CHAR(15)
      *  WS-APELLIDOS         NOT IN USE
      *  WS-EXISTE-CLIENTE        IN USE CHAR(1)
      *  WS-FECHA-ULT-MOV     NOT IN USE
      *  WS-NEWID-CTACTE          IN USE THROUGH TEMP VAR SQL-VAR-0007 DECIMAL(3,0)
      *  WS-NOMBRE            NOT IN USE
      *  WS-SALDO-ACTUAL      NOT IN USE
      *  WT-DOC-CLI               IN USE CHAR(12)
      *  WT-EXISTE-TARJ           IN USE CHAR(1)
      *  WT-FECHA-MOVIMIENTO      IN USE CHAR(10)
      *  WT-ID-CLIENTE            IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(5,0)
      *  WT-ID-MOVIMIENTO         IN USE THROUGH TEMP VAR SQL-VAR-0008 DECIMAL(11,0)
      *  WT-LIMITE-TARJ           IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(11,2)
      *  WT-MONTO                 IN USE THROUGH TEMP VAR SQL-VAR-0006 DECIMAL(11,2)
      *  WT-MONTO-TARJ            IN USE THROUGH TEMP VAR SQL-VAR-0009 DECIMAL(15,2)
      *  WT-NRO-TARJETA           IN USE CHAR(16)
      *  WT-NUMERO-TARJ           IN USE CHAR(16)
      *  WT-TIPO-MOVIMIENTO       IN USE CHAR(1)
      *  WTT-ID-CLIENTE           IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(5,0)
      *  WX-ID-CLIENTE            IN USE THROUGH TEMP VAR SQL-VAR-0019 DECIMAL(11,0)
      *  WX-NRO-TARJETA           IN USE CHAR(16)
      *  WX-SALDO-ACTUAL          IN USE THROUGH TEMP VAR SQL-VAR-0020 DECIMAL(15,2)
      *  WX-TARJETA-ACTIVA    NOT IN USE
      **********************************************************************
