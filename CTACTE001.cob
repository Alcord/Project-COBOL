       IDENTIFICATION DIVISION.
       PROGRAM-ID. CTACTE001.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE-DETAIL ASSIGN TO "EXTRACTO_DETAIL.DAT"
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
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 481.
           05 SQL-STMT   PIC X(481) VALUE 'SELECT A.NOMBRE_CLIENTE,A.APE
      -    'LLIDOS_CLIENTE,B.FECHA_ULT_MOV,B.SALDO_ACTUAL FROM (SELECT I
      -    'D_CLIENTE,DOC_CLIENTE,NOMBRE_CLIENTE,APELLIDOS_CLIENTE FROM 
      -    'BANCO.CLIENTES WHERE DOC_CLIENTE = TRIM(?) LIMIT 1) A,(SELEC
      -    'T MAX(ID_CLIENTE) ID_CLIENTE,MAX(COD_ULT_MOV) COD_ULT_MOV,MA
      -    'X(FECHA_ULT_MOV) FECHA_ULT_MOV,MAX(SALDO_ACTUAL) SALDO_ACTUA
      -    'L FROM BANCO.CTACTES WHERE ID_CLIENTE = (SELECT ID_CLIENTE F
      -    'ROM BANCO.CLIENTES WHERE DOC_CLIENTE = TRIM(?) LIMIT 1)) B W
      -    'HERE A.ID_CLIENTE = B.ID_CLIENTE'.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 249.
           05 SQL-STMT   PIC X(249) VALUE 'SELECT FECHA_ULT_MOV,CASE WHE
      -    'N IMPORTE_MOV > 0 THEN ''DEPOSITO'' ELSE ''EXTRACCION'' END 
      -    'AS TIPO_MOV,IMPORTE_MOV,SALDO_ACTUAL FROM BANCO.CTACTES WHER
      -    'E ID_CLIENTE = (SELECT ID_CLIENTE FROM BANCO.CLIENTES WHERE 
      -    'DOC_CLIENTE =TRIM(?)) ORDER BY FECHA_ULT_MOV'.
           05 SQL-CNAME  PIC X(7) VALUE 'CUR_ALL'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 41.
           05 SQL-STMT   PIC X(41) VALUE 'SELECT InsertarMovimiento(?,?)
      -    ' FROM DUAL;'.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 80.
           05 SQL-STMT   PIC X(80) VALUE 'SELECT IFNULL((SELECT ''S'' FR
      -    'OM CLIENTES WHERE DOC_CLIENTE =TRIM(?) LIMIT 1),''N'')'.
      **********************************************************************
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 17.
           05 SQL-STMT   PIC X(17) VALUE 'SELECT DATABASE()'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9)V9(2) COMP-3.
           05 SQL-VAR-0003  PIC S9(9)V9(2) COMP-3.
           05 SQL-VAR-0004  PIC S9(3) COMP-3.
           05 SQL-VAR-0007  PIC S9(9)V9(2) COMP-3.
           05 SQL-VAR-0008  PIC S9(9)V9(2) COMP-3.
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

       01  DB-VARS.
           05  BUFFER                  PIC X(1024).
           05  ST-COUNT                PIC 9(6).
           05  CTACTE.
               10  WT-DOC-CLI                 PIC X(12).
               10  WT-MONTO                   PIC S9(8)V99.
               10  WS-EXISTE-CLIENTE          PIC X(01).
               10  WS-NEWID-CTACTE            PIC 999.

       01  DB-EXTRACTO-DETALLE.
           05  WE-ID-CLIENTE                  PIC 9(10).
           05  WE-DOC-CLIENTE                 PIC 9(10).
           05  WE-FECHA-MOVIMIENTO            PIC X(10).
           05  WE-TIPO-MOVIMIENTO             PIC X(15).
           05  WE-IMPORTE                     PIC S9(9)V99.
           05  WE-SALDO                       PIC S9(9)V99.

      *    EXEC SQL
      *        END DECLARE SECTION
      *    END-EXEC

       01  WS-HEADER-P0  PIC X(80) VALUE
           "*--------------------------------------------------------*".

       01  WS-HEADER-P1  PIC X(80) VALUE
           "NOMBRE        APELLIDOS       FECHA_ULT_MOV   SALDO_ACTUAL".

       01  WS-HEADER-P2  PIC X(80) VALUE
           "*--------------------------------------------------------*".

       01  WS-HEADER             PIC X(80) VALUE
           "FECHA       TIPO MOVIMIENTO   IMPORTE     SALDO ACTUAL".
       01  WS-HEADER2             PIC X(80) VALUE
           "************GENERACION DE EXTRACTO DETALLADO**********".
       01  WS-HEADER3             PIC X(80) VALUE
           "******************************************************".
       01  PRTEC                 PIC X(100).

       01  WS-EXTRACTO-DETAIL.
           03 REG-DETAIL OCCURS 100 TIMES.
               05  EXT-FECHA-MOVIMIENTO   PIC X(10).
               05  EXT-TIPO-MOVIMIENTO    PIC X(15).
               05  EXT-IMPORTE            PIC S9(9)V99.
               05  EXT-SALDO              PIC S9(9)V99.

       01  RXT-FECHA-MOVIMIENTO           PIC X(10).
       01  FIN-DATOS             PIC X VALUE 'N'.
       01  WS-WORK-AREAS.
           05  FILE-CHECK-KEY          PIC X(2).
           05  ERR-MSG                 PIC X(128).
           05  ERR-CODE                PIC X(2).
       01  WS-OPCION                   PIC 9.
       01  WS-SALDO                    PIC ZZZZZZ9.99.
       01  WS-MONTO                    PIC S9(8)V99.
       01  WS-DOC-CLI                  PIC X(12).
       01  WS-TIPO-MOVIMIENTO          PIC X(01).
       01  WX-TIPO-MOVIMIENTO          PIC 9.
       01  WS-IMPORTE-FORM             PIC ZZZZZZ9.99-.
       01  WS-SALDO-FORM               PIC ZZZZZZ9.99-.
       01  WS-CONT                     PIC 999.
       01  WS-CONTX                    PIC 999.

       LINKAGE SECTION.
       01 LK-USER-ID PIC 9(1).  *> Recibirá un ID de usuario

       PROCEDURE DIVISION USING LK-USER-ID.
       MAIN-PROGRAM.
           PERFORM 0100-INICIO.
           PERFORM 100-MENU.

       100-MENU.
           DISPLAY "===================================="
           DISPLAY "       SISTEMA DE CTA. CTE.        "
           DISPLAY "===================================="
           DISPLAY "1 - Registrar Movimiento".
           DISPLAY "2 - Consultar Saldo"
           DISPLAY "3 - Generar Extracto"
           DISPLAY "4 - Salir"
           DISPLAY "Seleccione una opción: "
           ACCEPT WS-OPCION
           EVALUATE WS-OPCION
               WHEN 1
                   PERFORM 100-REGISTRAR-MOVIMIENTO
               WHEN 2
                   PERFORM 200-CONSULTAR-SALDO
                   DISPLAY "Saldo Actual: " WS-SALDO
               WHEN 3
                   PERFORM 300-GENERAR-EXTRACTO
               WHEN 4
                   EXIT PROGRAM
               WHEN OTHER
                   DISPLAY "Opción inválida, intente nuevamente."
           END-EVALUATE.

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

       100-REGISTRAR-MOVIMIENTO.

           PERFORM 100-CONSULTA-CLIENTE
           DISPLAY "Ingrese tipo de movim (D=Depósito, E=Extracción): ".
           ACCEPT WS-TIPO-MOVIMIENTO.

           DISPLAY "Ingrese el monto: ".
           ACCEPT WS-MONTO.
           IF WS-TIPO-MOVIMIENTO = 'D' THEN
              MOVE 1 TO WX-TIPO-MOVIMIENTO
              COMPUTE WS-MONTO = WS-MONTO * (1)
           ELSE
              MOVE 2 TO WX-TIPO-MOVIMIENTO
              COMPUTE WS-MONTO = WS-MONTO * (-1)
           END-IF

           PERFORM 100-INSERTA-MOVIMIENTO
           DISPLAY "NUEVO MOVIMIENTO " WS-NEWID-CTACTE
           IF WS-NEWID-CTACTE > 0 THEN
               DISPLAY "Movimiento registrado correctamente."
           ELSE
               DISPLAY "Movimiento no registrado correctamente."
           END-IF.
           PERFORM 100-MENU.

       200-CONSULTAR-SALDO.

           INITIALIZE WS-DOC-CLI
           PERFORM 100-CONSULTA-CLIENTE
           PERFORM 200-EXTRAE-INFO-SALDO
           DISPLAY WS-HEADER-P0
           DISPLAY WS-HEADER-P1
           DISPLAY WS-HEADER-P2
           DISPLAY RPT-DETALLE-CLI.

       200-EXTRAE-INFO-SALDO.
           MOVE WS-DOC-CLI TO WT-DOC-CLI.
      *    EXEC SQL

      *    SELECT A.NOMBRE_CLIENTE, A.APELLIDOS_CLIENTE,
      *           B.FECHA_ULT_MOV, B.SALDO_ACTUAL
      *    INTO :WS-NOMBRE, :WS-APELLIDOS,
      *         :WS-FECHA-ULT-MOV, :WS-SALDO-ACTUAL
      *    FROM (SELECT ID_CLIENTE, DOC_CLIENTE, NOMBRE_CLIENTE,
      *                 APELLIDOS_CLIENTE
      *            FROM BANCO.CLIENTES
      *           WHERE DOC_CLIENTE = TRIM(:WT-DOC-CLI) LIMIT 1) A,
      *         (SELECT MAX(ID_CLIENTE) ID_CLIENTE,
      *                 MAX(COD_ULT_MOV) COD_ULT_MOV,
      *                 MAX(FECHA_ULT_MOV) FECHA_ULT_MOV,
      *                 MAX(SALDO_ACTUAL) SALDO_ACTUAL
      *            FROM BANCO.CTACTES
      *           WHERE ID_CLIENTE = (SELECT ID_CLIENTE
      *                                FROM BANCO.CLIENTES
      *                               WHERE DOC_CLIENTE =
      *                               TRIM(:WT-DOC-CLI) LIMIT 1)) B
      *         WHERE A.ID_CLIENTE = B.ID_CLIENTE
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WS-NOMBRE
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 15 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WS-APELLIDOS
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 15 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 WS-FECHA-ULT-MOV
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 10 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(4)
               MOVE 6 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 WT-DOC-CLI
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 12 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 WT-DOC-CLI
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 12 TO SQL-LEN(6)
               MOVE 6 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA
           MOVE SQL-VAR-0001 TO WS-SALDO-ACTUAL
                   .

           MOVE WS-NOMBRE          TO RPT-NOMBRE
           MOVE WS-APELLIDOS       TO RPT-APELLIDOS
           MOVE WS-FECHA-ULT-MOV   TO RPT-FECHA-ULT-MOV
           MOVE WS-SALDO-ACTUAL    TO RPT-SALDO-ACTUAL
           MOVE RPT-SALDO-ACTUAL   TO WS-SALDO.

       320-EXTRAE-EXTRATO-CURSOR.
           MOVE WS-DOC-CLI TO WT-DOC-CLI
      *    EXEC SQL
      *        DECLARE CUR_ALL CURSOR FOR
      *    SELECT FECHA_ULT_MOV,
      *           CASE
      *               WHEN IMPORTE_MOV > 0 THEN 'DEPOSITO'
      *               ELSE 'EXTRACCION'
      *           END AS TIPO_MOV,
      *           IMPORTE_MOV, SALDO_ACTUAL
      *    FROM BANCO.CTACTES
      *    WHERE ID_CLIENTE = (SELECT ID_CLIENTE
      *                           FROM BANCO.CLIENTES
      *                          WHERE DOC_CLIENTE =TRIM(:WT-DOC-CLI))
      *    ORDER BY FECHA_ULT_MOV
      *    END-EXEC.
                   .
      *    EXEC SQL
      *        OPEN CUR_ALL
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WT-DOC-CLI
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 12 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
           END-IF
           CALL 'OCSQLOCU' USING SQL-STMT-1
                               SQLCA
           END-CALL
                   .
           DISPLAY WS-HEADER.
           PERFORM UNTIL SQLCODE = 100
      *        EXEC SQL
      *          FETCH CUR_ALL
      *          INTO
      *            :WE-FECHA-MOVIMIENTO,
      *            :WE-TIPO-MOVIMIENTO,
      *            :WE-IMPORTE,
      *            :WE-SALDO
      *        END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             WE-FECHA-MOVIMIENTO
           MOVE 'X' TO SQL-TYPE(1)
           MOVE 10 TO SQL-LEN(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             WE-TIPO-MOVIMIENTO
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 15 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             SQL-VAR-0007
           MOVE '3' TO SQL-TYPE(3)
           MOVE 6 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0008
           MOVE '3' TO SQL-TYPE(4)
           MOVE 6 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
           MOVE 4 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-1
                               SQLCA
           MOVE SQL-VAR-0007 TO WE-IMPORTE
           MOVE SQL-VAR-0008 TO WE-SALDO

            IF SQLCODE NOT = 100 THEN
               *> Formatear línea del reporte
               ADD 1 TO WS-CONT
               MOVE WE-FECHA-MOVIMIENTO TO EXT-FECHA-MOVIMIENTO(WS-CONT)
               MOVE WE-TIPO-MOVIMIENTO TO EXT-TIPO-MOVIMIENTO(WS-CONT)
               MOVE WE-IMPORTE         TO EXT-IMPORTE(WS-CONT)
               MOVE WE-SALDO           TO EXT-SALDO(WS-CONT)
               MOVE 'S' TO FIN-DATOS
           ELSE
               DISPLAY ' NO TIENE MAS FILAS LA TABLA DE DB'
           END-IF
           END-PERFORM.
      *    EXEC SQL CLOSE CUR_ALL END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-1
                               SQLCA
                                          .
           DISPLAY"                                                  "
           DISPLAY"                                                  "
           DISPLAY WS-HEADER2
           DISPLAY WS-HEADER
           WRITE RPT-DETALLE-CLI FROM WS-HEADER2
           WRITE RPT-DETALLE-CLI FROM WS-HEADER
           PERFORM VARYING WS-CONTX FROM 1 BY 1 UNTIL WS-CONTX > WS-CONT

             MOVE EXT-IMPORTE(WS-CONTX) TO WS-IMPORTE-FORM
             MOVE EXT-SALDO(WS-CONTX)   TO WS-SALDO-FORM
              STRING EXT-FECHA-MOVIMIENTO(WS-CONTX) "  "
                     EXT-TIPO-MOVIMIENTO(WS-CONTX)  "  "
                     WS-IMPORTE-FORM                "  "
                     WS-SALDO-FORM
               INTO PRTEC
             DISPLAY PRTEC
             WRITE RPT-DETALLE-CLI FROM PRTEC
           END-PERFORM.
             WRITE RPT-DETALLE-CLI FROM WS-HEADER3
             DISPLAY WS-HEADER3
             CLOSE RPT-FILE-DETAIL.

       310-INICIO.
           OPEN OUTPUT RPT-FILE-DETAIL
           INITIALIZE WS-DOC-CLI.

       300-GENERAR-EXTRACTO.

           PERFORM 310-INICIO.
           PERFORM 100-CONSULTA-CLIENTE.
           PERFORM 320-EXTRAE-EXTRATO-CURSOR.
           PERFORM 100-MENU.

       100-INSERTA-MOVIMIENTO.
           MOVE WS-MONTO TO WT-MONTO.
           MOVE WS-DOC-CLI TO WT-DOC-CLI.
      *    EXEC SQL
      *        SELECT InsertarMovimiento(:WT-DOC-CLI, :WT-MONTO)
      *          INTO :WS-NEWID-CTACTE
      *          FROM DUAL;
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(1)
               MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WT-DOC-CLI
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 12 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(3)
               MOVE 6 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               MOVE 3 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WT-MONTO TO SQL-VAR-0003
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
           MOVE SQL-VAR-0004 TO WS-NEWID-CTACTE
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
           IF SQL-PREP OF SQL-STMT-3 = 'N'
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
                                   SQL-STMT-3
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-3
                               SQLCA
                   .

       GENERAR-EXTRACTO.

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
           IF SQL-PREP OF SQL-STMT-4 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 BUFFER
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 1024 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-4
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-4
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
      *  CUR_ALL                  IN USE CURSOR
      *  DB-DATOS-CLI         NOT IN USE
      *  DB-DATOS-CLI.WS-APELLIDOS NOT IN USE
      *  DB-DATOS-CLI.WS-FECHA-ULT-MOV NOT IN USE
      *  DB-DATOS-CLI.WS-NOMBRE NOT IN USE
      *  DB-DATOS-CLI.WS-SALDO-ACTUAL NOT IN USE
      *  DB-EXTRACTO-DETALLE  NOT IN USE
      *  DB-EXTRACTO-DETALLE.WE-DOC-CLIENTE NOT IN USE
      *  DB-EXTRACTO-DETALLE.WE-FECHA-MOVIMIENTO NOT IN USE
      *  DB-EXTRACTO-DETALLE.WE-ID-CLIENTE NOT IN USE
      *  DB-EXTRACTO-DETALLE.WE-IMPORTE NOT IN USE
      *  DB-EXTRACTO-DETALLE.WE-SALDO NOT IN USE
      *  DB-EXTRACTO-DETALLE.WE-TIPO-MOVIMIENTO NOT IN USE
      *  DB-VARS              NOT IN USE
      *  DB-VARS.BUFFER       NOT IN USE
      *  DB-VARS.CTACTE       NOT IN USE
      *  DB-VARS.ST-COUNT     NOT IN USE
      *  DB-VARS.WS-EXISTE-CLIENTE NOT IN USE
      *  DB-VARS.WS-NEWID-CTACTE NOT IN USE
      *  DB-VARS.WT-DOC-CLI   NOT IN USE
      *  DB-VARS.WT-MONTO     NOT IN USE
      *  ST-COUNT             NOT IN USE
      *  WE-DOC-CLIENTE       NOT IN USE
      *  WE-FECHA-MOVIMIENTO      IN USE CHAR(10)
      *  WE-ID-CLIENTE        NOT IN USE
      *  WE-IMPORTE               IN USE THROUGH TEMP VAR SQL-VAR-0007 DECIMAL(11,2)
      *  WE-SALDO                 IN USE THROUGH TEMP VAR SQL-VAR-0008 DECIMAL(11,2)
      *  WE-TIPO-MOVIMIENTO       IN USE CHAR(15)
      *  WS-APELLIDOS             IN USE CHAR(15)
      *  WS-EXISTE-CLIENTE        IN USE CHAR(1)
      *  WS-FECHA-ULT-MOV         IN USE CHAR(10)
      *  WS-NEWID-CTACTE          IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(3,0)
      *  WS-NOMBRE                IN USE CHAR(15)
      *  WS-SALDO-ACTUAL          IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(11,2)
      *  WT-DOC-CLI               IN USE CHAR(12)
      *  WT-MONTO                 IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(11,2)
      **********************************************************************
