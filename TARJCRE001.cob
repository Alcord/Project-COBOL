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
       01  RPT-DETALLE-CLI.
           05  RPT-NOMBRE           PIC X(15).  *> Nombre del cliente
           05  RPT-FILLER1          PIC X(02).
           05  RPT-APELLIDOS        PIC X(15).  *> Apellidos del cliente
           05  RPT-FILLER2          PIC X(02).
           05  RPT-FECHA-ULT-MOV    PIC X(10).  *> Fecha del último movi
           05  RPT-FILLER3          PIC X(02).
           05  RPT-SALDO-ACTUAL    PIC ZZZZZ9.99-.  *> Saldo actual


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
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 6.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 6 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 6 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 6 TIMES.
           05 SQL-PREC   PIC X OCCURS 6 TIMES.
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
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 70.
           05 SQL-STMT   PIC X(70) VALUE 'UPDATE TARJETAS SET ACUM_MES =
      -    ' ACUM_MES + ? WHERE NRO_TARJETA =TRIM(?)'.
      **********************************************************************
       01 SQL-STMT-6.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 85.
           05 SQL-STMT   PIC X(85) VALUE 'UPDATE TARJETAS SET LIQUIDACIO
      -    'N_MES = LIQUIDACION_MES - ? WHERE NRO_TARJETA =TRIM(?);'.
      **********************************************************************
       01 SQL-STMT-7.
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
       01 SQL-STMT-8.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 39.
           05 SQL-STMT   PIC X(39) VALUE 'SELECT GenerarNumeroTarjeta() 
      -    'FROM DUAL'.
      **********************************************************************
       01 SQL-STMT-9.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 72.
           05 SQL-STMT   PIC X(72) VALUE 'SELECT ID_CLIENTE FROM BANCO.C
      -    'LIENTES WHERE DOC_CLIENTE =TRIM(?) LIMIT 1'.
      **********************************************************************
       01 SQL-STMT-10.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 4.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 104.
           05 SQL-STMT   PIC X(104) VALUE 'INSERT INTO BANCO.TARJETAS VA
      -    'LUES(TRIM(?),TRIM(?),(SELECT CURDATE() + INTERVAL 10 YEAR FR
      -    'OM DUAL),?,0,?)'.
      **********************************************************************
       01 SQL-STMT-11.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 101.
           05 SQL-STMT   PIC X(101) VALUE 'SELECT ID_CLIENTE,NRO_TARJETA
      -    ',FECHA_VENCIMIENTO,LIMITE_TARJETA,ACUM_MES,LIQUIDACION_MES F
      -    'ROM TARJETAS'.
           05 SQL-CNAME  PIC X(12) VALUE 'CURSOR_DEUDA'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-12.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 41.
           05 SQL-STMT   PIC X(41) VALUE 'SELECT InsertarMovimiento(?,?)
      -    ' FROM DUAL;'.
      **********************************************************************
       01 SQL-STMT-13.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 80.
           05 SQL-STMT   PIC X(80) VALUE 'SELECT IFNULL((SELECT ''S'' FR
      -    'OM CLIENTES WHERE DOC_CLIENTE =TRIM(?) LIMIT 1),''N'')'.
      **********************************************************************
       01 SQL-STMT-14.
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
      *******       END OF PRECOMPILER-GENERATED VARIABLES           *******
      **********************************************************************
       01   DB-CONN.
           05  DB-USER                 PIC X(20) VALUE 'root'.
           05  DB-PASSWORD             PIC X(20) VALUE 'root'.
           05  DB-NAME                 PIC X(20) VALUE 'banco'.
           05  DB-HOST                 PIC X(20) VALUE 'localhost'.
           05  DB-PORT                 PIC 9(5)  VALUE 3306.


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
           05  WE-ACUM-MES                    PIC 9(12)V99.
           05  WE-LIQUIDACION-MES             PIC 9(12)V99.

      *    EXEC SQL
      *        END DECLARE SECTION
      *    END-EXEC

       01  WS-HEADER-P0  PIC X(80) VALUE
           "*--------------------------------------------------------*".

       01  WS-HEADER-P1  PIC X(80) VALUE
           "NOMBRE        APELLIDOS       FECHA_ULT_MOV   SALDO_ACTUAL".

       01  WS-HEADER-P2  PIC X(80) VALUE
           "*--------------------------------------------------------*".

       01  WS-HEADER             PIC X(59) VALUE
           "ID_CLIENTE | NRO_TARJETA      | VENCIMIENTO | LIMITE | CON".
       01  WS-HEADER-1           PIC X(59) VALUE
           "SUMO | PAGO | DEUDA | DISPONIBLE".

       01  WS-HEADER2             PIC X(80) VALUE
           "************REPORTE DE MOVIMIENTOS DE TARJETAS**********".
       01  WS-HEADER3             PIC X(80) VALUE
           "********************************************************".
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
           05  WS-MONTO-TARJ                 PIC 9(12)V99.

       01  WS-TARJETA-DETAIL.
           03 REG-DETAIL OCCURS 100 TIMES.
              05  RPT-ID-CLIENTE                  PIC 9(10).
              05  RPT-NRO-TARJETA                 PIC X(16).
              05  RPT-FECHA-VENCIMIENTO           PIC X(10).
              05  RPT-LIMITE-TARJETA              PIC 9(12)V99.
              05  RPT-ACUM-MES                    PIC 9(12)V99.
              05  RPT-LIQUIDACION-MES             PIC 9(12)V99.
              05  RPT-DEUDA-TOTAL                 PIC 9(12)V99.
              05  RPT-CREDITO-DISPONIBLE          PIC 9(12)V99.


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
       01  WS-INTENTOS                 PIC 99.

       01  WS-ACUM-MES-FT              PIC ZZZZZZZZZZ9.99-.
       01  WS-LIQUIDACION-MES-FT       PIC ZZZZZZZZZZ9.99-.
       01  WS-DEUDA-TOTAL-FT           PIC ZZZZZZZZZZ9.99-.
       01  WS-CREDITO-DISPONIBLE-FT    PIC ZZZZZZZZZZ9.99-.
       01  WS-DEUDA-TOTAL              PIC 9(12)V99.
       01  WS-CREDITO-DISPONIBLE       PIC 9(12)V99.



       PROCEDURE DIVISION.
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
                   STOP RUN
               WHEN OTHER
                   DISPLAY "Opción inválida, intente nuevamente."
           END-EVALUATE.
       300-GENERAR-REPORTE-DEUDA.
           PERFORM 310-INICIO.
           PERFORM 320-EXTRAE-DEUDA-CURSOR.
           PERFORM 100-MENU.

       200-VERIFICA-NRO-TARJETA.

           IF WS-INTENTOS >= 3 THEN
             DISPLAY "Núm de intentos superado. Ingrese otro documento."
             PERFORM 200-REGISTRAR-PAGOS-CONSUMO
           END-IF.

           DISPLAY "Ingrese el Número de Tarjeta: ".
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

           DISPLAY "Ingrese el Monto: "
           ACCEPT WS-MONTO-TARJ

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

           MOVE ZEROES TO WS-INTENTOS
           PERFORM 100-CONSULTA-CLIENTE
           PERFORM 200-VERIFICA-NRO-TARJETA
           PERFORM 220-INSERTA-PAGO-CONSUMO.
           PERFORM 230-ACTUALIZA-PAGO-CONSUMO.
           PERFORM 100-MENU.

       230-ACTUALIZA-PAGO-CONSUMO.

           IF WT-TIPO-MOVIMIENTO = 'C' THEN
      *-- Actualizar el saldo del mes
      *       EXEC SQL
      *        UPDATE TARJETAS
      *           SET ACUM_MES = ACUM_MES + :WT-MONTO-TARJ
      *         WHERE NRO_TARJETA =TRIM(:WT-NRO-TARJETA)
      *       END-EXEC
           IF SQL-PREP OF SQL-STMT-5 = 'N'
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
                                   SQL-STMT-5
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WT-MONTO-TARJ
             TO SQL-VAR-0009
           CALL 'OCSQLEXE' USING SQL-STMT-5
                               SQLCA
           ELSE
      *-- Actualizar la liquidación
      *        EXEC SQL
      *        UPDATE TARJETAS
      *           SET LIQUIDACION_MES = LIQUIDACION_MES - :WT-MONTO-TARJ
      *         WHERE NRO_TARJETA =TRIM(:WT-NRO-TARJETA);
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
      *    EXEC SQL
      *       COMMIT
      *    END-EXEC.
           CALL 'OCSQLCMT' USING SQLCA END-CALL
                   .

       100-CONSULTA-CLIENTE.

           DISPLAY "Ingrese Documento Cliente: ".
           ACCEPT WS-DOC-CLI

           IF WS-DOC-CLI < 0 THEN
              PERFORM 100-CONSULTA-CLIENTE
           END-IF

           PERFORM 100-EXISTE-CLIENTE

           IF WS-EXISTE-CLIENTE = 'N' THEN
              DISPLAY "Doc. No Existe: " WS-DOC-CLI
              DISPLAY "Favor, Ingresar Nuevamente: "
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
           IF SQL-PREP OF SQL-STMT-7 = 'N'
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
                                   SQL-STMT-7
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-7
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
           IF SQL-PREP OF SQL-STMT-8 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WT-NUMERO-TARJ
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
           IF SQL-PREP OF SQL-STMT-9 = 'N'
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
                                   SQL-STMT-9
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-9
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
           IF SQL-PREP OF SQL-STMT-10 = 'N'
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
                                   SQL-STMT-10
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WTT-ID-CLIENTE
             TO SQL-VAR-0003
           MOVE WT-LIMITE-TARJ
             TO SQL-VAR-0004
           MOVE WT-LIMITE-TARJ
             TO SQL-VAR-0004
           CALL 'OCSQLEXE' USING SQL-STMT-10
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
              DISPLAY "Ingrese el Limite (Debe ser mayor a 1000): "
              ACCEPT WS-LIMITE-TARJ
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

       320-EXTRAE-DEUDA-CURSOR.
      *    EXEC SQL
      *        DECLARE CURSOR_DEUDA CURSOR FOR
      *        SELECT ID_CLIENTE, NRO_TARJETA,
      *               FECHA_VENCIMIENTO,
      *               LIMITE_TARJETA,
      *               ACUM_MES,
      *               LIQUIDACION_MES
      *         FROM TARJETAS
      *    END-EXEC.
                   .
      *    EXEC SQL
      *        OPEN CURSOR_DEUDA
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-11 = 'N'
               MOVE 0 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-11
                                   SQLCA
           END-IF
           CALL 'OCSQLOCU' USING SQL-STMT-11
                               SQLCA
           END-CALL
                   .
           DISPLAY WS-HEADER.
           PERFORM UNTIL SQLCODE = 100
      *        EXEC SQL
      *          FETCH CURSOR_DEUDA
      *          INTO
      *            :WE-ID-CLIENTE,
      *            :WE-NRO-TARJETA,
      *            :WE-FECHA-VENCIMIENTO,
      *            :WE-LIMITE-TARJETA,
      *            :WE-ACUM-MES,
      *            :WE-LIQUIDACION-MES
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
           MOVE 6 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-11
                               SQLCA
           MOVE SQL-VAR-0013 TO WE-ID-CLIENTE
           MOVE SQL-VAR-0014 TO WE-LIMITE-TARJETA
           MOVE SQL-VAR-0015 TO WE-ACUM-MES
           MOVE SQL-VAR-0016 TO WE-LIQUIDACION-MES

            IF SQLCODE NOT = 100 THEN
               *> Formatear línea del reporte

               ADD 1 TO WS-CONT
              COMPUTE WS-DEUDA-TOTAL = WE-ACUM-MES - WE-LIQUIDACION-MES
              COMPUTE WS-CREDITO-DISPONIBLE =
                                      WE-LIMITE-TARJETA - WS-DEUDA-TOTAL

               MOVE WE-ID-CLIENTE        TO RPT-ID-CLIENTE(WS-CONT)
               MOVE WE-NRO-TARJETA       TO RPT-NRO-TARJETA(WS-CONT)
               MOVE WE-FECHA-VENCIMIENTO
                                       TO RPT-FECHA-VENCIMIENTO(WS-CONT)
               MOVE WE-LIMITE-TARJETA    TO RPT-LIMITE-TARJETA(WS-CONT)
               MOVE WE-ACUM-MES          TO RPT-ACUM-MES(WS-CONT)
               MOVE WE-LIQUIDACION-MES   TO RPT-LIQUIDACION-MES(WS-CONT)
               MOVE WS-DEUDA-TOTAL       TO RPT-DEUDA-TOTAL(WS-CONT)
               MOVE WS-CREDITO-DISPONIBLE
                                      TO RPT-CREDITO-DISPONIBLE(WS-CONT)
           ELSE
               DISPLAY ' NO TIENE MAS FILAS LA TABLA DE DB'
           END-IF
           END-PERFORM.
      *    EXEC SQL CLOSE CURSOR_DEUDA END-EXEC
           CALL 'OCSQLCCU' USING SQL-STMT-11
                               SQLCA
           DISPLAY"                                                  "
           DISPLAY"                                                  "
           DISPLAY WS-HEADER2
           STRING WS-HEADER WS-HEADER-1 INTO PRTEC
           DISPLAY PRTEC
           WRITE RPT-DETALLE-CLI FROM WS-HEADER2
           WRITE RPT-DETALLE-CLI FROM PRTEC
      *     MOVE SPACES TO PRTEC
           PERFORM VARYING WS-CONTX FROM 1 BY 1 UNTIL WS-CONTX > WS-CONT
           MOVE RPT-ACUM-MES(WS-CONTX) TO WS-ACUM-MES-FT
           MOVE RPT-LIQUIDACION-MES(WS-CONTX)
                                       TO WS-LIQUIDACION-MES-FT
           MOVE RPT-DEUDA-TOTAL(WS-CONTX)
                                       TO WS-DEUDA-TOTAL-FT
           MOVE RPT-CREDITO-DISPONIBLE(WS-CONTX)
                                       TO WS-CREDITO-DISPONIBLE-FT

            STRING RPT-ID-CLIENTE(WS-CONTX)        " "
                   RPT-NRO-TARJETA(WS-CONTX)    " "
                   RPT-FECHA-VENCIMIENTO(WS-CONTX) " "
                   RPT-LIMITE-TARJETA(WS-CONTX)    " "
                   WS-ACUM-MES-FT                  " "
                   WS-LIQUIDACION-MES-FT           " "
                   WS-DEUDA-TOTAL-FT               " "
                   WS-CREDITO-DISPONIBLE-FT
                  INTO PRTEC
              DISPLAY PRTEC
           WRITE RPT-DETALLE-CLI FROM PRTEC
           END-PERFORM
              WRITE RPT-DETALLE-CLI FROM WS-HEADER3
              DISPLAY WS-HEADER3
           CLOSE RPT-FILE-DETAIL.

       310-INICIO.
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
           IF SQL-PREP OF SQL-STMT-12 = 'N'
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
                                   SQL-STMT-12
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WT-MONTO TO SQL-VAR-0006
           CALL 'OCSQLEXE' USING SQL-STMT-12
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
      *                   DOC_CLIENTE =TRIM(:WT-DOC-CLI) LIMIT 1), 'N')
      *       INTO :WS-EXISTE-CLIENTE
      *       FROM DUAL
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-13 = 'N'
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
                                   SQL-STMT-13
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-13
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
           IF SQL-PREP OF SQL-STMT-14 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 BUFFER
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 1024 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-14
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-14
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
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  BUFFER                   IN USE CHAR(1024)
      *  CTACTE               NOT IN USE
      *  CURSOR_DEUDA             IN USE CURSOR
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
      *  DB-DEUDA-DETALLE     NOT IN USE
      *  DB-DEUDA-DETALLE.WE-ACUM-MES NOT IN USE
      *  DB-DEUDA-DETALLE.WE-FECHA-VENCIMIENTO NOT IN USE
      *  DB-DEUDA-DETALLE.WE-ID-CLIENTE NOT IN USE
      *  DB-DEUDA-DETALLE.WE-LIMITE-TARJETA NOT IN USE
      *  DB-DEUDA-DETALLE.WE-LIQUIDACION-MES NOT IN USE
      *  DB-DEUDA-DETALLE.WE-NRO-TARJETA NOT IN USE
      *  DB-EXTRACTO-DETALLE  NOT IN USE
      *  DB-EXTRACTO-DETALLE.WE-DOC-CLIENTE NOT IN USE
      *  DB-EXTRACTO-DETALLE.WE-FECHA-MOVIMIENTO NOT IN USE
      *  DB-EXTRACTO-DETALLE.WE-IMPORTE NOT IN USE
      *  DB-EXTRACTO-DETALLE.WE-SALDO NOT IN USE
      *  DB-EXTRACTO-DETALLE.WE-TIPO-MOVIMIENTO NOT IN USE
      *  DB-VARS              NOT IN USE
      *  DB-VARS.BUFFER       NOT IN USE
      *  DB-VARS.CTACTE       NOT IN USE
      *  DB-VARS.ST-COUNT     NOT IN USE
      *  DB-VARS.WS-EXISTE-CLIENTE NOT IN USE
      *  DB-VARS.WS-NEWID-CTACTE NOT IN USE
      *  DB-VARS.WT-MONTO     NOT IN USE
      *  ST-COUNT             NOT IN USE
      *  WE-ACUM-MES              IN USE THROUGH TEMP VAR SQL-VAR-0015 DECIMAL(15,2)
      *  WE-DOC-CLIENTE       NOT IN USE
      *  WE-FECHA-MOVIMIENTO  NOT IN USE
      *  WE-FECHA-VENCIMIENTO     IN USE CHAR(10)
      *  WE-ID-CLIENTE            IN USE THROUGH TEMP VAR SQL-VAR-0013 DECIMAL(11,0)
      *  WE-IMPORTE           NOT IN USE
      *  WE-LIMITE-TARJETA        IN USE THROUGH TEMP VAR SQL-VAR-0014 DECIMAL(15,2)
      *  WE-LIQUIDACION-MES       IN USE THROUGH TEMP VAR SQL-VAR-0016 DECIMAL(15,2)
      *  WE-NRO-TARJETA           IN USE CHAR(16)
      *  WE-SALDO             NOT IN USE
      *  WE-TIPO-MOVIMIENTO   NOT IN USE
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
      **********************************************************************
