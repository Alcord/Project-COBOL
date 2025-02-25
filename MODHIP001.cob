       IDENTIFICATION DIVISION.
       PROGRAM-ID. MODHIP001.
       DATA DIVISION.
       FILE SECTION.
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
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 9.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 9 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 9 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 9 TIMES.
           05 SQL-PREC   PIC X OCCURS 9 TIMES.
      **********************************************************************
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 17.
           05 SQL-STMT   PIC X(17) VALUE 'SELECT DATABASE()'.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 6.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 112.
           05 SQL-STMT   PIC X(112) VALUE 'INSERT INTO cuotas_hipoteca (
      -    'ID_CLIENTE,ID_HIPOTECA,N_CUOTA,FECHA,ESTADO,MONTO_CUOTA) VAL
      -    'UES (?,?,?,?,TRIM(?),?)'.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 169.
           05 SQL-STMT   PIC X(169) VALUE 'SELECT C.ID_CLIENTE,C.DOC_CLI
      -    'ENTE,C.NOMBRE_CLIENTE,C.APELLIDOS_CLIENTE,C.HIPOTECA,C.CTA_A
      -    'CTIVA,C.FECHA_CIERRE FROM banco.clientes C WHERE C.DOC_CLIEN
      -    'TE = TRIM(?) LIMIT 1'.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 48.
           05 SQL-STMT   PIC X(48) VALUE 'SELECT MAX(H.ID_HIPOTECA) FROM
      -    ' banco.hipotecas H'.
      **********************************************************************
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 52.
           05 SQL-STMT   PIC X(52) VALUE 'UPDATE CLIENTES SET HIPOTECA =
      -    ' 1 WHERE ID_CLIENTE =?'.
      **********************************************************************
       01 SQL-STMT-5.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 8.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 162.
           05 SQL-STMT   PIC X(162) VALUE 'INSERT INTO banco.hipotecas (
      -    'ID_HIPOTECA,ID_CLIENTE,FECHA_INICIO,MONTO_ORIGINAL,TASA_INTE
      -    'RES,SALDO_ACTUAL,FECHA_VENCIMIENTO,ESTADO) VALUES (?,?,?,?,?
      -    ',?,?,TRIM(?))'.
      **********************************************************************
       01 SQL-STMT-6.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 124.
           05 SQL-STMT   PIC X(124) VALUE 'SELECT N_CUOTA,FECHA,ESTADO,M
      -    'ONTO_CUOTA FROM banco.cuotas_hipoteca WHERE ID_HIPOTECA = ? 
      -    'AND ID_CLIENTE = ? ORDER BY N_CUOTA'.
           05 SQL-CNAME  PIC X(8) VALUE 'C_CUOTAS'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-7.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 151.
           05 SQL-STMT   PIC X(151) VALUE 'SELECT ID_HIPOTECA,ID_CLIENTE
      -    ',FECHA_INICIO,MONTO_ORIGINAL,TASA_INTERES,SALDO_ACTUAL,FECHA
      -    '_VENCIMIENTO,ESTADO FROM banco.hipotecas WHERE ID_HIPOTECA =
      -    ' ?'.
      **********************************************************************
       01 SQL-STMT-8.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 90.
           05 SQL-STMT   PIC X(90) VALUE 'SELECT MAX(H.ID_HIPOTECA) FROM
      -    ' banco.hipotecas H WHERE ID_CLIENTE = ? AND ESTADO = TRIM(?)
      -    ''.
      **********************************************************************
       01 SQL-STMT-9.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 136.
           05 SQL-STMT   PIC X(136) VALUE 'SELECT N_CUOTA,MONTO_CUOTA FR
      -    'OM banco.cuotas_hipoteca WHERE ID_HIPOTECA = ? AND ID_CLIENT
      -    'E = ? AND ESTADO = ''Pendiente'' ORDER BY N_CUOTA'.
           05 SQL-CNAME  PIC X(12) VALUE 'C_CUOTAS_PAY'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-10.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 3.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 107.
           05 SQL-STMT   PIC X(107) VALUE 'UPDATE banco.cuotas_hipoteca 
      -    'SET ESTADO = ''Pagada'' WHERE ID_HIPOTECA = ? AND ID_CLIENTE
      -    ' = ? AND N_CUOTA = ?'.
      **********************************************************************
       01 SQL-STMT-11.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 3.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 99.
           05 SQL-STMT   PIC X(99) VALUE 'UPDATE banco.hipotecas SET SAL
      -    'DO_ACTUAL = SALDO_ACTUAL - ? WHERE ID_HIPOTECA = ? AND ID_CL
      -    'IENTE = ?'.
      **********************************************************************
       01 SQL-STMT-12.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 91.
           05 SQL-STMT   PIC X(91) VALUE 'UPDATE banco.cuotas_hipoteca S
      -    'ET ESTADO = ''Pagada'' WHERE ID_HIPOTECA = ? AND ID_CLIENTE 
      -    '= ?'.
      **********************************************************************
       01 SQL-STMT-13.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 102.
           05 SQL-STMT   PIC X(102) VALUE 'UPDATE banco.hipotecas SET SA
      -    'LDO_ACTUAL = 0,ESTADO = ''Pagada'' WHERE ID_HIPOTECA = ? AND
      -    ' ID_CLIENTE = ?'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0002  PIC S9(3) COMP-3.
           05 SQL-VAR-0003  PIC S9(1) COMP-3.
           05 SQL-VAR-0004  PIC S9(1) COMP-3.
           05 SQL-VAR-0005  PIC S9(7) COMP-3.
           05 SQL-VAR-0006  PIC S9(3) COMP-3.
           05 SQL-VAR-0007  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0008  PIC S9(3)V9(2) COMP-3.
           05 SQL-VAR-0009  PIC S9(3) COMP-3.
           05 SQL-VAR-0010  PIC S9(3) COMP-3.
           05 SQL-VAR-0011  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0012  PIC S9(3)V9(2) COMP-3.
           05 SQL-VAR-0013  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0014  PIC S9(3) COMP-3.
           05 SQL-VAR-0015  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0016  PIC S9(13)V9(2) COMP-3.
      *******       END OF PRECOMPILER-GENERATED VARIABLES           *******
      **********************************************************************

       COPY "BD001".
       COPY "CONF0223".

      *EXEC SQL
      *    BEGIN DECLARE SECTION
      *END-EXEC.

       01  SEARCH-APELLI        PIC X(10).

       01  DB-VARS.
           05  DB-DOCUMENT      PIC X(12).
           05  BUFFER           PIC X(1024).
           05  ST-COUNT         PIC 9(6).
           05  CLIENT.
               10  ID-CLIENTE       PIC 9(3).
               10  DOCUMENT         PIC X(12).
               10  NOMBRE           PIC X(25).
               10  APELLIDO         PIC X(25).
               10  HIPOTECA         PIC 9(01).
               10  ACTIVA           PIC 9(01).
               10  FECHA-CIERRE     PIC X(10).

       01  DB-EXTRA-INFO.
           05  MAX-N-HIP        PIC 9(6).
           05  DB-N-CUOTA       PIC 9(3).
           05  DB-FECHA         PIC X(10).
           05  DB-STATUS        PIC X(10).
           05  DB-PMT           PIC 9(12)V99.
           05  DB-INTERES       PIC 9(3)V99.

       01  DB-HIPOTECA-RESUMEN.
           05  DB-ID-HIPOTECA       PIC 9(3).
           05  DB-ID-CLIENTE        PIC 9(3).
           05  DB-FECHA-INICIO      PIC X(10).
           05  DB-MONTO-ORIGINAL    PIC 9(12)V99.
           05  DB-TASA-INTERES      PIC 9(3)V99.
           05  DB-SALDO-ACTUAL      PIC 9(12)V99.
           05  DB-FECHA-VENCIMIENTO PIC X(10).
           05  DB-ESTADO            PIC X(10).

       01  DB-CUOTA-DET.
           05  DB-N-CUOTA-DET       PIC 9(3).
           05  DB-FECHA-CUOTA       PIC X(10).
           05  DB-ESTADO-CUOTA      PIC X(20).
           05  DB-MONTO-CUOTA       PIC 9(12)V99.

       01  DB-ESTADO-HIP        PIC X(10) VALUE "Activa".
       01  DB-SUM-PAGO          PIC 9(12)V99 VALUE 0.

      *EXEC SQL
      *    END DECLARE SECTION
      *END-EXEC.


       01  WS-MAINMENU.
           05 WS-LINE PIC X(30) VALUE ALL "-".
           05 WS-TITLE.
               10 WS-LEFT-WALL PIC X(01) VALUE "|".
               10 WS-TXT-TITLE PIC X(30) VALUE SPACES.
               10 WS-RGTH-WALL PIC X(01) VALUE "|".
           05 WS-SUBTITLE.
             10 WS-SPACE PIC X(05) VALUE SPACES.
             10 WS-TXT-SUBTITLE PIC X(25) VALUE SPACES.
           05 WS-MENU.
            10 WS-TXTMN1 PIC X(30) VALUE "1. Registrar Hipoteca".
            10 WS-TXTMN2 PIC X(30) VALUE "2. Consultar Deudas".
            10 WS-TXTMN3 PIC X(30) VALUE "3. Pagar Deudas".
            10 WS-TXTMN4 PIC X(30) VALUE "4. Salir".

       01  WS-MENU-PAGO.
           05 WS-PG-OPTION1 PIC X(30) VALUE "1. Pagar 1 o más cuotas".
           05 WS-PG-OPTION2 PIC X(30) VALUE "2. Pagar totalidad".
           05 WS-PG-OPTION3 PIC X(30) VALUE "3. Salir".

       01  WS-OPTION PIC 9 VALUE 0.

       01  WS-INPUT-INFORMATION.
           05 WS-MONTO     PIC 9(12)V99.
           05 WS-GARANTIA  PIC 9(12)V99.
           05 WS-CUOTAS    PIC 9(3).
           05 WS-INTERES   PIC 9(3)V99.
           05 WS-DOCUMENT  PIC X(12).

       01  WS-CALC-FECHA.
           05  WS-MES-ALFA         PIC X(2).
           05  WS-YEAR-ALFA        PIC X(4).
           05  WS-MES   PIC 99.
           05  WS-YEAR  PIC 9(4).

       01  WS-FECHA         PIC X(10) VALUE "2025-01-01".


       01  WS-CALCULOS.
           05  WS-N             PIC 9(3).  *> Contador de cuota
           05  WS-I             PIC 9(3)V99.     *> Tasa de interés mens
           05  WS-GRTMIN        PIC 9(12)V99.   *> Valor min inmueble
           05  WS-PMT           PIC 9(12)V99.    *> Valor de la cuota
           05  WS-CT-PAY        PIC 9(3).   *> Numero de cuotas a pagar
           05  WS-SUM-PAGO          PIC 9(12)V99 VALUE 0.   *> Acumulado
           05  WS-COUNT             PIC 9(3) VALUE 0.        *> Contador



       LINKAGE SECTION.
       01  LK-OPTION     PIC 9.

       PROCEDURE DIVISION USING LK-OPTION.
       PERFORM 0100-INICIO THRU 0210-HIPOTECAS.
       STOP RUN.

       0100-INICIO.

       0110-BD.
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
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 BUFFER
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 1024 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA

           DISPLAY 'BASE DE DATOS ACTUAL: ' BUFFER.

           PERFORM 0291-SQLSTATE-CHECK.
      *-----------------------------------------------------------------
       0110-END.


       0111-CABECERA-MODULO.
           DISPLAY "===================================="
           DISPLAY "       SISTEMA DE HIPOTECAS          "
           DISPLAY "====================================".
       0111-END.

       0100-END.

       0200-PROCEDIMIENTO.

       0210-HIPOTECAS.

       INITIALIZE WS-OPTION
       MOVE "Menu Hipotecas" TO WS-TXT-TITLE(05:15)
       MOVE ":Opciones disponibles:" TO WS-TXT-SUBTITLE
       PERFORM 0221-PRINT-MENU
       ACCEPT WS-OPTION

       EVALUATE WS-OPTION

           WHEN 1
               DISPLAY "Registrar nueva hipoteca"
               PERFORM 0230-NEW-HIPOTECA
               PERFORM 0210-HIPOTECAS

           WHEN 2
               DISPLAY "CONSULTAR HIPOTECA"
               PERFORM 0240-CONSULT-DEUDA
               PERFORM 0210-HIPOTECAS

           WHEN 3
               DISPLAY "Realizar pago"
               PERFORM 0250-REALIZA-PAGO
               PERFORM 0210-HIPOTECAS

           WHEN 4
               PERFORM 0300-FIN

           WHEN OTHER
               DISPLAY "(" WS-OPTION ") - " "Opcion invalida."
               PERFORM 0210-HIPOTECAS

           DISPLAY "Salida incorrecta".


       0230-NEW-HIPOTECA.
           DISPLAY "***************************************************"
           DISPLAY "Registrando Nueva hipoteca....."
           DISPLAY "Ingrese -1 para salir"
           DISPLAY "***************************************************"

           DISPLAY "Inserte el documento del cliente"
           ACCEPT WS-DOCUMENT

           IF WS-DOCUMENT = "-1"
               DISPLAY "Regresando a Menu Hipotecas..."
               EXIT PARAGRAPH
           END-IF

           PERFORM 0232-SEARCH-CLIENT

           IF SQLCODE NOT = 0
               DISPLAY "Cliente no encontrado."
               PERFORM 0230-NEW-HIPOTECA
           END-IF

           IF ACTIVA NOT = 1
               DISPLAY "El usuario esta dado de baja."
               PERFORM 0230-NEW-HIPOTECA
           END-IF

           IF HIPOTECA = 1
               DISPLAY "El usurio ya tiene una hipoteca activa."
               PERFORM 0230-NEW-HIPOTECA
           END-IF

      *>   Valores para cuotas
           DISPLAY "***************************************************"
           DISPLAY "Datos para el prestamo"
           DISPLAY "Para cancelar inserte un numero negativo"
           DISPLAY "***************************************************"

           DISPLAY "Ingrese el monto de la hipoteca"
           ACCEPT WS-MONTO

           IF WS-MONTO < 0
               DISPLAY "Regresando a Menu Hipotecas..."
               EXIT PARAGRAPH
           END-IF

           PERFORM 0234-GARANTIA-VAL

           DISPLAY "Ingrese el numero de cuotas"
           ACCEPT WS-CUOTAS

           IF WS-CUOTAS < 0
               DISPLAY "Regresando a Menu Hipotecas..."
               EXIT PARAGRAPH
           END-IF

           DISPLAY "Ingrese el interes para esta hipoteca"
           ACCEPT WS-INTERES

           IF WS-INTERES < 0
               DISPLAY "Regresando a Menu Hipotecas..."
               EXIT PARAGRAPH
           END-IF

           MOVE  WS-MONTO  TO DB-MONTO-ORIGINAL
           MOVE  WS-INTERES TO DB-INTERES


      *>   Fecha actual
           MOVE FUNCTION CURRENT-DATE (1:4) TO DB-FECHA(1:4)
           MOVE "-"                      TO DB-FECHA(5:1)
           MOVE FUNCTION CURRENT-DATE (5:2) TO DB-FECHA(6:2)
           MOVE "-"                      TO DB-FECHA(8:1)
           MOVE FUNCTION CURRENT-DATE (7:2) TO DB-FECHA(9:2)

      *>      DISPLAY "Fecha para la hipoteca: " DB-FECHA

           PERFORM 0235-NUM-HIPOTECA

      *>      DISPLAY  MAX-N-HIP

           PERFORM 0231-CREAR-NUEVA-HIPOTECA

           PERFORM 0236-ADD-HIP-CLITE

           PERFORM 0237-REG-NEW-HIP.

       0230-END.

       0231-CREAR-NUEVA-HIPOTECA.
           INITIALIZE WS-CALCULOS
           MOVE "Pendiente" TO DB-STATUS

           PERFORM 0233-ACTUALIZA-FECHA

           MOVE DB-FECHA TO DB-FECHA-INICIO

      * Calcular la tasa de interés mensual y el valor de la cuota.
           IF WS-INTERES > 0
               COMPUTE WS-I = WS-INTERES / 100 / 12
               *> La fórmula de cuota (PMT) se puede expresar como:
               *>   PMT = MONTO * WS-I / (1 - (1 + WS-I) ** (-WS-CUOTAS)
               COMPUTE WS-PMT = WS-MONTO * WS-I
               *> Se debe dividir por (1 - (1 + WS-I) ** (-WS-CUOTAS))
              COMPUTE WS-PMT = WS-PMT / (1 - (1 + WS-I) ** (-WS-CUOTAS))
           ELSE
               COMPUTE WS-PMT = WS-MONTO / WS-CUOTAS
           END-IF.

           MOVE WS-PMT TO DB-PMT

           DISPLAY "Generando cuotas de la hipoteca..."

           PERFORM VARYING WS-N FROM 1 BY 1 UNTIL WS-N > WS-CUOTAS

               MOVE WS-N TO DB-N-CUOTA

      *        EXEC SQL
      *            INSERT INTO cuotas_hipoteca
      *              (ID_CLIENTE,
      *              ID_HIPOTECA,
      *              N_CUOTA,
      *              FECHA,
      *              ESTADO,
      *              MONTO_CUOTA)
      *            VALUES
      *              (:ID-CLIENTE,
      *              :MAX-N-HIP,
      *              :DB-N-CUOTA,
      *              :DB-FECHA,
      *              TRIM(:DB-STATUS),
      *              :DB-PMT)
      *        END-EXEC
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(2)
               MOVE 4 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(3)
               MOVE 2 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 DB-FECHA
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 10 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 DB-STATUS
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 10 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0007
               MOVE '3' TO SQL-TYPE(6)
               MOVE 8 TO SQL-LEN(6)
               MOVE X'02' TO SQL-PREC(6)
               MOVE 6 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE ID-CLIENTE
             TO SQL-VAR-0002
           MOVE MAX-N-HIP
             TO SQL-VAR-0005
           MOVE DB-N-CUOTA
             TO SQL-VAR-0006
           MOVE DB-PMT
             TO SQL-VAR-0007
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA

               DISPLAY "Insertada cuota número: " DB-N-CUOTA
               DISPLAY " con fecha: " WS-FECHA

      * Actualizar WS-FECHA al primer día del siguiente mes.
               PERFORM 0233-ACTUALIZA-FECHA

           END-PERFORM

           PERFORM 0291-COMMIT.
       0231-END.

       0232-SEARCH-CLIENT.
           MOVE WS-DOCUMENT TO DB-DOCUMENT

      *    EXEC SQL
      *        SELECT  C.ID_CLIENTE,
      *                C.DOC_CLIENTE,
      *                C.NOMBRE_CLIENTE,
      *                C.APELLIDOS_CLIENTE,
      *                C.HIPOTECA,
      *                C.CTA_ACTIVA,
      *                C.FECHA_CIERRE

      *            INTO    :ID-CLIENTE,
      *                :DOCUMENT,
      *                :NOMBRE,
      *                :APELLIDO,
      *                :HIPOTECA,
      *                :ACTIVA,
      *                :FECHA-CIERRE

      *            FROM banco.clientes C
      *        WHERE C.DOC_CLIENTE = TRIM(:DB-DOCUMENT)
      *        LIMIT 1
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 DOCUMENT
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 12 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 NOMBRE
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 25 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 APELLIDO
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 25 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(5)
               MOVE 1 TO SQL-LEN(5)
               MOVE X'00' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(6)
               MOVE 1 TO SQL-LEN(6)
               MOVE X'00' TO SQL-PREC(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 FECHA-CIERRE
               MOVE 'X' TO SQL-TYPE(7)
               MOVE 10 TO SQL-LEN(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 DB-DOCUMENT
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 12 TO SQL-LEN(8)
               MOVE 8 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
           MOVE SQL-VAR-0002 TO ID-CLIENTE
           MOVE SQL-VAR-0003 TO HIPOTECA
           MOVE SQL-VAR-0004 TO ACTIVA

           PERFORM 0291-SQLSTATE-CHECK.
       0232-END.

       0233-ACTUALIZA-FECHA.
           INITIALIZE WS-CALC-FECHA
      *> Formatear la fecha actual para obtener el primer día del mes si
           MOVE DB-FECHA(1:4)       TO WS-FECHA(1:4)
           MOVE DB-FECHA(1:4)       TO WS-YEAR-ALFA
           MOVE '-'                   TO WS-FECHA(5:1)
           MOVE DB-FECHA(6:2)       TO WS-FECHA(6:2)
           MOVE DB-FECHA(6:2)       TO WS-MES-ALFA
           MOVE '-'                   TO WS-FECHA(8:1)
           MOVE "01"                  TO WS-FECHA(9:2)

           MOVE WS-YEAR-ALFA         TO WS-YEAR
           MOVE WS-MES-ALFA          TO WS-MES

           ADD 1 TO WS-MES

           IF WS-MES > 12
              MOVE 1 TO WS-MES
              ADD 1 TO WS-YEAR
           END-IF

           MOVE WS-YEAR              TO WS-FECHA(1:4)
           MOVE WS-MES               TO WS-FECHA(6:2)
           DISPLAY "Fecha formateada: " WS-FECHA
           MOVE WS-FECHA TO DB-FECHA.
       0233-END.

       0234-GARANTIA-VAL.

           DISPLAY "Inserte el valor aprox. de la garantia"
           ACCEPT WS-GARANTIA

           IF WS-GARANTIA < 0
               DISPLAY "Regresando a Menu Hipotecas..."
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-GRTMIN = WS-MONTO * (PORC-MIN-GRNTIA / 100)

           PERFORM UNTIL WS-GARANTIA > WS-GRTMIN
               PERFORM 0234-GARANTIA-VAL
           END-PERFORM.

       0234-END.

       0235-NUM-HIPOTECA.
      *    EXEC SQL
      *        SELECT MAX(H.ID_HIPOTECA)

      *        INTO    :MAX-N-HIP

      *        FROM banco.hipotecas H

      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-3 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(1)
               MOVE 4 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-3
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-3
                               SQLCA
           MOVE SQL-VAR-0005 TO MAX-N-HIP

           IF MAX-N-HIP = 0
               MOVE 1 TO MAX-N-HIP
           ELSE
               ADD 1 TO MAX-N-HIP
           END-IF.
       0235-END.

       0236-ADD-HIP-CLITE.

      *    EXEC SQL
      *        UPDATE CLIENTES
      *        SET  HIPOTECA = 1
      *        WHERE ID_CLIENTE =: ID-CLIENTE
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-4 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-4
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE ID-CLIENTE
             TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-4
                               SQLCA

           PERFORM 0291-COMMIT.

      *>      DISPLAY "Se actualizo hip = Si".
       0236-END.

       0237-REG-NEW-HIP.

      *    EXEC SQL
      *        INSERT INTO banco.hipotecas
      *          (ID_HIPOTECA,
      *           ID_CLIENTE,
      *           FECHA_INICIO,
      *           MONTO_ORIGINAL,
      *           TASA_INTERES,
      *           SALDO_ACTUAL,
      *           FECHA_VENCIMIENTO,
      *           ESTADO)
      *        VALUES
      *          (:MAX-N-HIP,
      *           :ID-CLIENTE,
      *           :DB-FECHA-INICIO,
      *           :DB-MONTO-ORIGINAL,
      *           :DB-INTERES,
      *           :DB-MONTO-ORIGINAL,
      *           :DB-FECHA,
      *           TRIM(:DB-ESTADO-HIP))
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-5 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(1)
               MOVE 4 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(2)
               MOVE 2 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 DB-FECHA-INICIO
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 10 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0011
               MOVE '3' TO SQL-TYPE(4)
               MOVE 8 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0008
               MOVE '3' TO SQL-TYPE(5)
               MOVE 3 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0011
               MOVE '3' TO SQL-TYPE(6)
               MOVE 8 TO SQL-LEN(6)
               MOVE X'02' TO SQL-PREC(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 DB-FECHA
               MOVE 'X' TO SQL-TYPE(7)
               MOVE 10 TO SQL-LEN(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 DB-ESTADO-HIP
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 10 TO SQL-LEN(8)
               MOVE 8 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-5
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE MAX-N-HIP
             TO SQL-VAR-0005
           MOVE ID-CLIENTE
             TO SQL-VAR-0002
           MOVE DB-MONTO-ORIGINAL
             TO SQL-VAR-0011
           MOVE DB-INTERES
             TO SQL-VAR-0008
           MOVE DB-MONTO-ORIGINAL
             TO SQL-VAR-0011
           CALL 'OCSQLEXE' USING SQL-STMT-5
                               SQLCA

           PERFORM 0291-COMMIT

           DISPLAY "Se reg nueva Hipoteca".


       0237-END.

       0240-CONSULT-DEUDA.
           DISPLAY "Consultar Hipoteca..."
           DISPLAY "Ingrese -1 para salir"

           DISPLAY "Inserte el documento del cliente"
           ACCEPT WS-DOCUMENT

           IF WS-DOCUMENT = "-1"
               DISPLAY "Regresando a Menu Hipotecas..."
               EXIT PARAGRAPH
           END-IF

           PERFORM 0232-SEARCH-CLIENT

           IF SQLCODE NOT = 0
               DISPLAY "Cliente no encontrado."
               PERFORM 0240-CONSULT-DEUDA
               EXIT PARAGRAPH
           END-IF

           IF HIPOTECA = 0
               DISPLAY "El usurio no tiene una hipoteca activa."
               PERFORM 0240-CONSULT-DEUDA
               EXIT PARAGRAPH
           END-IF

           PERFORM 0243-HIPOTECA-RECIENTE

           PERFORM 0241-RESUMEN-HIPOTECA.

       0240-END.

       0241-RESUMEN-HIPOTECA.
      *> Obtener el resumen de la hipoteca (para el valor máximo de ID_H
           PERFORM 0242-CONSULTAR-HIPOTECA

      *> Mostrar el resumen de la hipoteca
           PERFORM 0244-RESUMEN-HIPOT

      *    EXEC SQL
      *     OPEN C_CUOTAS
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-6 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0009
               MOVE '3' TO SQL-TYPE(1)
               MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0010
               MOVE '3' TO SQL-TYPE(2)
               MOVE 2 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-6
                                   SQLCA
           END-IF
           MOVE DB-ID-HIPOTECA TO SQL-VAR-0009
           MOVE DB-ID-CLIENTE TO SQL-VAR-0010
           CALL 'OCSQLOCU' USING SQL-STMT-6
                               SQLCA
           END-CALL

           DISPLAY "Detalle de cuotas:"
               *> Imprimir cabecera del detalle de cuotas
           DISPLAY "---------------------------------------------------"
           DISPLAY "CUOTA    FECHA       ESTADO      MONTO"
           DISPLAY "---------------------------------------------------"

      *    EXEC SQL
      *    DECLARE C_CUOTAS CURSOR FOR
      *        SELECT N_CUOTA,
      *             FECHA,
      *             ESTADO,
      *             MONTO_CUOTA
      *        FROM banco.cuotas_hipoteca
      *        WHERE ID_HIPOTECA = :DB-ID-HIPOTECA
      *        AND ID_CLIENTE  = :DB-ID-CLIENTE
      *        ORDER BY N_CUOTA
      *    END-EXEC

           PERFORM UNTIL SQLCODE = 100
      *        EXEC SQL
      *          FETCH C_CUOTAS INTO :DB-N-CUOTA-DET,
      *                               :DB-FECHA-CUOTA,
      *                               :DB-ESTADO-CUOTA,
      *                               :DB-MONTO-CUOTA
      *        END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0014
           MOVE '3' TO SQL-TYPE(1)
           MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             DB-FECHA-CUOTA
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 10 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             DB-ESTADO-CUOTA
           MOVE 'X' TO SQL-TYPE(3)
           MOVE 20 TO SQL-LEN(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0015
           MOVE '3' TO SQL-TYPE(4)
           MOVE 8 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
           MOVE 4 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-6
                               SQLCA
           MOVE SQL-VAR-0014 TO DB-N-CUOTA-DET
           MOVE SQL-VAR-0015 TO DB-MONTO-CUOTA

               IF SQLCODE = 0
                 DISPLAY DB-N-CUOTA-DET "    "
                         DB-FECHA-CUOTA "    "
                         DB-ESTADO-CUOTA " "
                         DB-MONTO-CUOTA
               END-IF
           END-PERFORM

      *    EXEC SQL
      *    CLOSE C_CUOTAS
      *    END-EXEC
           CALL 'OCSQLCCU' USING SQL-STMT-6
                               SQLCA

           PERFORM 0210-HIPOTECAS.

       0242-CONSULTAR-HIPOTECA.
      *    EXEC SQL
      *        SELECT ID_HIPOTECA,
      *             ID_CLIENTE,
      *             FECHA_INICIO,
      *             MONTO_ORIGINAL,
      *             TASA_INTERES,
      *             SALDO_ACTUAL,
      *             FECHA_VENCIMIENTO,
      *             ESTADO
      *        INTO     :DB-ID-HIPOTECA,
      *                 :DB-ID-CLIENTE,
      *                 :DB-FECHA-INICIO,
      *                 :DB-MONTO-ORIGINAL,
      *                 :DB-TASA-INTERES,
      *                 :DB-SALDO-ACTUAL,
      *                 :DB-FECHA-VENCIMIENTO,
      *                 :DB-ESTADO
      *        FROM banco.hipotecas
      *            WHERE ID_HIPOTECA = :MAX-N-HIP
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-7 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0009
               MOVE '3' TO SQL-TYPE(1)
               MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0010
               MOVE '3' TO SQL-TYPE(2)
               MOVE 2 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 DB-FECHA-INICIO
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 10 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0011
               MOVE '3' TO SQL-TYPE(4)
               MOVE 8 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0012
               MOVE '3' TO SQL-TYPE(5)
               MOVE 3 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0013
               MOVE '3' TO SQL-TYPE(6)
               MOVE 8 TO SQL-LEN(6)
               MOVE X'02' TO SQL-PREC(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 DB-FECHA-VENCIMIENTO
               MOVE 'X' TO SQL-TYPE(7)
               MOVE 10 TO SQL-LEN(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 DB-ESTADO
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 10 TO SQL-LEN(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(9)
               MOVE 4 TO SQL-LEN(9)
               MOVE X'00' TO SQL-PREC(9)
               MOVE 9 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-7
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE MAX-N-HIP TO SQL-VAR-0005
           CALL 'OCSQLEXE' USING SQL-STMT-7
                               SQLCA
           MOVE SQL-VAR-0009 TO DB-ID-HIPOTECA
           MOVE SQL-VAR-0010 TO DB-ID-CLIENTE
           MOVE SQL-VAR-0011 TO DB-MONTO-ORIGINAL
           MOVE SQL-VAR-0012 TO DB-TASA-INTERES
           MOVE SQL-VAR-0013 TO DB-SALDO-ACTUAL

           PERFORM 0291-SQLSTATE-CHECK.

       0242-END.


       0243-HIPOTECA-RECIENTE.

      *    EXEC SQL
      *            SELECT MAX(H.ID_HIPOTECA)

      *            INTO    :MAX-N-HIP

      *            FROM banco.hipotecas H

      *            WHERE ID_CLIENTE = :ID-CLIENTE

      *            AND ESTADO = TRIM(:DB-ESTADO-HIP)
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-8 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(1)
               MOVE 4 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(2)
               MOVE 2 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 DB-ESTADO-HIP
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 10 TO SQL-LEN(3)
               MOVE 3 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-8
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE ID-CLIENTE TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-8
                               SQLCA
           MOVE SQL-VAR-0005 TO MAX-N-HIP

           PERFORM 0291-SQLSTATE-CHECK.

       0243-END.

       0244-RESUMEN-HIPOT.
           DISPLAY "------------------------------------"
           DISPLAY "       RESUMEN DE LA HIPOTECA       "
           DISPLAY "------------------------------------"
           DISPLAY "ID HIPOTECA       : " DB-ID-HIPOTECA
           DISPLAY "ID CLIENTE        : " DB-ID-CLIENTE
           DISPLAY "FECHA INICIO      : " DB-FECHA-INICIO
           DISPLAY "MONTO ORIGINAL    : " DB-MONTO-ORIGINAL
           DISPLAY "TASA DE INTERES   : " DB-TASA-INTERES
           DISPLAY "SALDO ACTUAL      : " DB-SALDO-ACTUAL
           DISPLAY "FECHA VENCIMIENTO : " DB-FECHA-VENCIMIENTO
           DISPLAY "ESTADO            : " DB-ESTADO
           DISPLAY "------------------------------------".
       0244-END.


       0250-REALIZA-PAGO.
           INITIALIZE WS-OPTION
           DISPLAY "realizando pago ............".
           DISPLAY "***************************************************"
           DISPLAY "Pagos de prestamos"
           DISPLAY "Para cancelar inserte un numero negativo"
           DISPLAY "***************************************************"

           DISPLAY "Inserte el documento del cliente"
           ACCEPT WS-DOCUMENT

           IF WS-DOCUMENT = "-1"
               DISPLAY "Regresando a Menu Hipotecas..."
               EXIT PARAGRAPH
           END-IF

           PERFORM 0232-SEARCH-CLIENT

           IF SQLCODE NOT = 0
               DISPLAY "Cliente no encontrado."
               PERFORM 0250-REALIZA-PAGO
           END-IF

           IF HIPOTECA = 0
               DISPLAY "El usurio no tiene una hipoteca activa."
               PERFORM 0250-REALIZA-PAGO
           END-IF

           PERFORM 0243-HIPOTECA-RECIENTE

           PERFORM 0242-CONSULTAR-HIPOTECA

           PERFORM 0244-RESUMEN-HIPOT


           PERFORM UNTIL WS-OPTION = 1 OR WS-OPTION = 2
               OR WS-OPTION = 3

               DISPLAY ".................................."
               DISPLAY WS-PG-OPTION1
               DISPLAY WS-PG-OPTION2
               DISPLAY WS-PG-OPTION3
               DISPLAY ".................................."
               DISPLAY "Que desea Realizar?"
               ACCEPT WS-OPTION

           END-PERFORM


           EVALUATE WS-OPTION
               WHEN 1
                   DISPLAY "Pagar cuotas"
                   PERFORM 0251-PAGAR-CUOTAS
                   EXIT PARAGRAPH

               WHEN 2
                   DISPLAY "Pagar en su totalidad"
                   PERFORM 0253-CANCELAR-PRESTAMO
                   EXIT PARAGRAPH

               WHEN 3
                   DISPLAY "Volviendo..."
                   EXIT PARAGRAPH

           END-EVALUATE.


       0250-END.


       0251-PAGAR-CUOTAS.
           INITIALIZE WS-COUNT WS-SUM-PAGO

           DISPLAY "------------------------------------"
           DISPLAY "Cuantas cuotas quiere pagar?"
                      ACCEPT WS-CT-PAY
           DISPLAY "------------------------------------"
           DISPLAY "Iniciando proceso de pago de cuotas..."
           DISPLAY "Se pagarán " WS-CT-PAY " cuota(s)"

      *> Declarar un cursor exclusivo para el pago de cuotas (con 2 colu
      *    EXEC SQL
      *     DECLARE C_CUOTAS_PAY CURSOR FOR
      *     SELECT N_CUOTA, MONTO_CUOTA
      *     FROM banco.cuotas_hipoteca
      *      WHERE ID_HIPOTECA = :DB-ID-HIPOTECA
      *       AND ID_CLIENTE  = :ID-CLIENTE
      *       AND ESTADO = 'Pendiente'
      *       ORDER BY N_CUOTA
      *      END-EXEC

      *     EXEC SQL
      *       OPEN C_CUOTAS_PAY
      *     END-EXEC
           IF SQL-PREP OF SQL-STMT-9 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0009
               MOVE '3' TO SQL-TYPE(1)
               MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(2)
               MOVE 2 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-9
                                   SQLCA
           END-IF
           MOVE DB-ID-HIPOTECA TO SQL-VAR-0009
           MOVE ID-CLIENTE TO SQL-VAR-0002
           CALL 'OCSQLOCU' USING SQL-STMT-9
                               SQLCA
           END-CALL

           PERFORM UNTIL WS-COUNT >= WS-CT-PAY OR SQLCODE = 100
      *       EXEC SQL
      *         FETCH C_CUOTAS_PAY INTO :DB-N-CUOTA-DET, :DB-MONTO-CUOTA
      *       END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0014
           MOVE '3' TO SQL-TYPE(1)
           MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             SQL-VAR-0015
           MOVE '3' TO SQL-TYPE(2)
           MOVE 8 TO SQL-LEN(2)
               MOVE X'02' TO SQL-PREC(2)
           MOVE 2 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-9
                               SQLCA
           MOVE SQL-VAR-0014 TO DB-N-CUOTA-DET
           MOVE SQL-VAR-0015 TO DB-MONTO-CUOTA


            IF SQLCODE = 0
               ADD 1 TO WS-COUNT
               ADD DB-MONTO-CUOTA TO WS-SUM-PAGO

                         *> Actualizar la cuota a "Pagada"
      *        EXEC SQL
      *        UPDATE banco.cuotas_hipoteca
      *        SET ESTADO = 'Pagada'
      *        WHERE ID_HIPOTECA = :DB-ID-HIPOTECA
      *        AND ID_CLIENTE  = :ID-CLIENTE
      *        AND N_CUOTA     = :DB-N-CUOTA-DET
      *        END-EXEC
           IF SQL-PREP OF SQL-STMT-10 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0009
               MOVE '3' TO SQL-TYPE(1)
               MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(2)
               MOVE 2 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0014
               MOVE '3' TO SQL-TYPE(3)
               MOVE 2 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
               MOVE 3 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-10
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-ID-HIPOTECA
             TO SQL-VAR-0009
           MOVE ID-CLIENTE
             TO SQL-VAR-0002
           MOVE DB-N-CUOTA-DET
             TO SQL-VAR-0014
           CALL 'OCSQLEXE' USING SQL-STMT-10
                               SQLCA
            END-IF
           END-PERFORM

      *    EXEC SQL
      *     CLOSE C_CUOTAS_PAY
      *    END-EXEC
           CALL 'OCSQLCCU' USING SQL-STMT-9
                               SQLCA

           MOVE WS-SUM-PAGO TO DB-SUM-PAGO
      *> Actualizar la hipoteca, restando el total pagado del saldo actu

      *    EXEC SQL
      *       UPDATE banco.hipotecas
      *       SET SALDO_ACTUAL = SALDO_ACTUAL - :DB-SUM-PAGO
      *       WHERE ID_HIPOTECA = :DB-ID-HIPOTECA
      *       AND ID_CLIENTE  = :ID-CLIENTE
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-11 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0016
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0009
               MOVE '3' TO SQL-TYPE(2)
               MOVE 2 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(3)
               MOVE 2 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
               MOVE 3 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-11
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-SUM-PAGO
             TO SQL-VAR-0016
           MOVE DB-ID-HIPOTECA
             TO SQL-VAR-0009
           MOVE ID-CLIENTE
             TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-11
                               SQLCA

           PERFORM 0291-COMMIT.

           DISPLAY "Pago realizado: " WS-COUNT " cuota(s) pagadas"
           DISPLAY "Por un total de " WS-SUM-PAGO
           DISPLAY "------------------------------------".

       0252-END.

       0253-CANCELAR-PRESTAMO.
            DISPLAY "Cancelando todo el préstamo..."

       *> Actualizar todas las cuotas de la hipoteca a "Pagada"
      *    EXEC SQL
      *      UPDATE banco.cuotas_hipoteca
      *      SET ESTADO = 'Pagada'
      *      WHERE ID_HIPOTECA = :DB-ID-HIPOTECA
      *      AND ID_CLIENTE = :ID-CLIENTE
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-12 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0009
               MOVE '3' TO SQL-TYPE(1)
               MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(2)
               MOVE 2 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-12
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-ID-HIPOTECA
             TO SQL-VAR-0009
           MOVE ID-CLIENTE
             TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-12
                               SQLCA

      *> Actualizar la hipoteca: SALDO_ACTUAL = 0 y ESTADO = 'Pagada'
      *    EXEC SQL
      *    UPDATE banco.hipotecas
      *    SET SALDO_ACTUAL = 0,
      *     ESTADO = 'Pagada'
      *     WHERE ID_HIPOTECA = :DB-ID-HIPOTECA
      *     AND ID_CLIENTE = :ID-CLIENTE
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-13 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0009
               MOVE '3' TO SQL-TYPE(1)
               MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(2)
               MOVE 2 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-13
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-ID-HIPOTECA
             TO SQL-VAR-0009
           MOVE ID-CLIENTE
             TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-13
                               SQLCA

           DISPLAY "Préstamo cancelado exitosamente.".

       0253-END.

       0221-PRINT-MENU.
           DISPLAY "+" WS-LINE "+".
           DISPLAY WS-TITLE.
           DISPLAY "+" WS-LINE "+".
           DISPLAY WS-LEFT-WALL WS-SPACE WS-TXT-SUBTITLE WS-RGTH-WALL.
           DISPLAY WS-LEFT-WALL WS-TXTMN1 WS-RGTH-WALL.
           DISPLAY WS-LEFT-WALL WS-TXTMN2 WS-RGTH-WALL.
           DISPLAY WS-LEFT-WALL WS-TXTMN3 WS-RGTH-WALL.
           DISPLAY WS-LEFT-WALL WS-TXTMN4 WS-RGTH-WALL.
           DISPLAY "+" WS-LINE "+"
           DISPLAY "Seleccione una opcion:".
       0221-END.

       0200-END.

       0291-COMMIT.
      *    EXEC SQL
      *        COMMIT
      *    END-EXEC
           CALL 'OCSQLCMT' USING SQLCA END-CALL
           PERFORM 0291-SQLSTATE-CHECK.
       0291-END.

       0291-SQLSTATE-CHECK.
           IF SQLCODE < 0
               DISPLAY 'SQLSTATE='  SQLSTATE,
                       ', SQLCODE=' SQLCODE
               IF SQLERRML > 0
                   DISPLAY 'SQL Error message:' SQLERRMC(1:SQLERRML)
               END-IF
               MOVE SQLCODE TO RETURN-CODE
           ELSE
               IF SQLCODE > 0 AND SQLCODE NOT = 100
                   DISPLAY 'SQLSTATE='  SQLSTATE,
                           ', SQLCODE=' SQLCODE
                   IF SQLERRML > 0
                       DISPLAY 'SQL Warning message:'
                               SQLERRMC(1:SQLERRML)
                   END-IF
               END-IF.
       0291-END.

       0200-END.

       0300-FIN.
           DISPLAY "Regresando a Menu Principal..."
           PERFORM 0301-DESCONECTAR-BD
           EXIT PROGRAM.
       0300-END.

       0301-DESCONECTAR-BD.
      *    EXEC SQL
      *          DISCONNECT ALL
      *    END-EXEC
           CALL 'OCSQLDIS' USING SQLCA END-CALL
           DISPLAY "CONEXION FINALIZADA.".
       0301-END.

       END PROGRAM MODHIP001.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  ACTIVA                   IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(1,0)
      *  APELLIDO                 IN USE CHAR(25)
      *  BUFFER                   IN USE CHAR(1024)
      *  CLIENT               NOT IN USE
      *  C_CUOTAS                 IN USE CURSOR
      *  C_CUOTAS_PAY             IN USE CURSOR
      *  DB-CUOTA-DET         NOT IN USE
      *  DB-CUOTA-DET.DB-ESTADO-CUOTA NOT IN USE
      *  DB-CUOTA-DET.DB-FECHA-CUOTA NOT IN USE
      *  DB-CUOTA-DET.DB-MONTO-CUOTA NOT IN USE
      *  DB-CUOTA-DET.DB-N-CUOTA-DET NOT IN USE
      *  DB-DOCUMENT              IN USE CHAR(12)
      *  DB-ESTADO                IN USE CHAR(10)
      *  DB-ESTADO-CUOTA          IN USE CHAR(20)
      *  DB-ESTADO-HIP            IN USE CHAR(10)
      *  DB-EXTRA-INFO        NOT IN USE
      *  DB-EXTRA-INFO.DB-FECHA NOT IN USE
      *  DB-EXTRA-INFO.DB-INTERES NOT IN USE
      *  DB-EXTRA-INFO.DB-N-CUOTA NOT IN USE
      *  DB-EXTRA-INFO.DB-PMT NOT IN USE
      *  DB-EXTRA-INFO.DB-STATUS NOT IN USE
      *  DB-EXTRA-INFO.MAX-N-HIP NOT IN USE
      *  DB-FECHA                 IN USE CHAR(10)
      *  DB-FECHA-CUOTA           IN USE CHAR(10)
      *  DB-FECHA-INICIO          IN USE CHAR(10)
      *  DB-FECHA-VENCIMIENTO     IN USE CHAR(10)
      *  DB-HIPOTECA-RESUMEN  NOT IN USE
      *  DB-HIPOTECA-RESUMEN.DB-ESTADO NOT IN USE
      *  DB-HIPOTECA-RESUMEN.DB-FECHA-INICIO NOT IN USE
      *  DB-HIPOTECA-RESUMEN.DB-FECHA-VENCIMIENTO NOT IN USE
      *  DB-HIPOTECA-RESUMEN.DB-ID-CLIENTE NOT IN USE
      *  DB-HIPOTECA-RESUMEN.DB-ID-HIPOTECA NOT IN USE
      *  DB-HIPOTECA-RESUMEN.DB-MONTO-ORIGINAL NOT IN USE
      *  DB-HIPOTECA-RESUMEN.DB-SALDO-ACTUAL NOT IN USE
      *  DB-HIPOTECA-RESUMEN.DB-TASA-INTERES NOT IN USE
      *  DB-ID-CLIENTE            IN USE THROUGH TEMP VAR SQL-VAR-0010 DECIMAL(3,0)
      *  DB-ID-HIPOTECA           IN USE THROUGH TEMP VAR SQL-VAR-0009 DECIMAL(3,0)
      *  DB-INTERES               IN USE THROUGH TEMP VAR SQL-VAR-0008 DECIMAL(5,2)
      *  DB-MONTO-CUOTA           IN USE THROUGH TEMP VAR SQL-VAR-0015 DECIMAL(15,2)
      *  DB-MONTO-ORIGINAL        IN USE THROUGH TEMP VAR SQL-VAR-0011 DECIMAL(15,2)
      *  DB-N-CUOTA               IN USE THROUGH TEMP VAR SQL-VAR-0006 DECIMAL(3,0)
      *  DB-N-CUOTA-DET           IN USE THROUGH TEMP VAR SQL-VAR-0014 DECIMAL(3,0)
      *  DB-PMT                   IN USE THROUGH TEMP VAR SQL-VAR-0007 DECIMAL(15,2)
      *  DB-SALDO-ACTUAL          IN USE THROUGH TEMP VAR SQL-VAR-0013 DECIMAL(15,2)
      *  DB-STATUS                IN USE CHAR(10)
      *  DB-SUM-PAGO              IN USE THROUGH TEMP VAR SQL-VAR-0016 DECIMAL(15,2)
      *  DB-TASA-INTERES          IN USE THROUGH TEMP VAR SQL-VAR-0012 DECIMAL(5,2)
      *  DB-VARS              NOT IN USE
      *  DB-VARS.ACTIVA       NOT IN USE
      *  DB-VARS.APELLIDO     NOT IN USE
      *  DB-VARS.BUFFER       NOT IN USE
      *  DB-VARS.CLIENT       NOT IN USE
      *  DB-VARS.DB-DOCUMENT  NOT IN USE
      *  DB-VARS.DOCUMENT     NOT IN USE
      *  DB-VARS.FECHA-CIERRE NOT IN USE
      *  DB-VARS.HIPOTECA     NOT IN USE
      *  DB-VARS.ID-CLIENTE   NOT IN USE
      *  DB-VARS.NOMBRE       NOT IN USE
      *  DB-VARS.ST-COUNT     NOT IN USE
      *  DOCUMENT                 IN USE CHAR(12)
      *  FECHA-CIERRE             IN USE CHAR(10)
      *  HIPOTECA                 IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(1,0)
      *  ID-CLIENTE               IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(3,0)
      *  MAX-N-HIP                IN USE THROUGH TEMP VAR SQL-VAR-0005 DECIMAL(7,0)
      *  NOMBRE                   IN USE CHAR(25)
      *  SEARCH-APELLI        NOT IN USE
      *  ST-COUNT             NOT IN USE
      **********************************************************************
