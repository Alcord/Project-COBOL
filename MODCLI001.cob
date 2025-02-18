      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MODCLI001.
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
<<<<<<< HEAD
           05 SQLVERSN PIC 99 VALUE 02.
           05 SQLCODE  PIC S9(9) COMP-5.
           05 SQLERRM.
               49 SQLERRML PIC S9(4) COMP-5.
               49 SQLERRMC PIC X(486).
           05 SQLERRD OCCURS 6 TIMES PIC S9(9) COMP-5.
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 14.
           05 SQL-COUNT  PIC S9(9) COMP-5.
           05 SQL-ADDR   POINTER OCCURS 14 TIMES.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 14 TIMES.
           05 SQL-TYPE   PIC X OCCURS 14 TIMES.
           05 SQL-PREC   PIC X OCCURS 14 TIMES.
      **********************************************************************
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER.
=======
           05 SQLVERSN PIC 99 VALUE 03.
           05 SQLCODE  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQLERRM.
               49 SQLERRML PIC S9(4) COMP-5 VALUE ZERO.
               49 SQLERRMC PIC X(486).
           05 SQLERRD OCCURS 6 TIMES PIC S9(9) COMP-5 VALUE ZERO.
           05 FILLER   PIC X(4).
           05 SQL-HCONN USAGE POINTER VALUE NULL.
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 13.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 13 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 13 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 13 TIMES.
           05 SQL-PREC   PIC X OCCURS 13 TIMES.
      **********************************************************************
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
>>>>>>> origin/main
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 17.
           05 SQL-STMT   PIC X(17) VALUE 'SELECT DATABASE()'.
      **********************************************************************
       01 SQL-STMT-1.
<<<<<<< HEAD
           05 SQL-IPTR   POINTER.
=======
           05 SQL-IPTR   POINTER VALUE NULL.
>>>>>>> origin/main
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 40.
           05 SQL-STMT   PIC X(40) VALUE 'SELECT MAX(ID_CLIENTE) + 1 FRO
      -    'M clientes'.
      **********************************************************************
       01 SQL-STMT-2.
<<<<<<< HEAD
           05 SQL-IPTR   POINTER.
=======
           05 SQL-IPTR   POINTER VALUE NULL.
>>>>>>> origin/main
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 13.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 72.
           05 SQL-STMT   PIC X(72) VALUE 'INSERT INTO clientes VALUES(?,
      -    '?,?,CURRENT_DATE,?,?,?,?,?,?,?,?,?,null,?)'.
      **********************************************************************
       01 SQL-STMT-3.
<<<<<<< HEAD
           05 SQL-IPTR   POINTER.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 257.
           05 SQL-STMT   PIC X(257) VALUE 'SELECT C.ID_CLIENTE,C.DOC_CLI
      -    'ENTE,C.NOMBRE_CLIENTE,C.APELLIDOS_CLIENTE,C.DIRECCION_CLIENT
      -    'E,C.TELEF_CLIENTE,C.EMAIL_CLIENTE,C.TARJETA,C.CREDITO,C.HIPO
      -    'TECA,C.CTA_ACTIVA,C.SALDO_CLIENTE,C.FECHA_CIERRE FROM banco.
      -    'clientes C WHERE C.DOC_CLIENTE = TRIM(?) LIMIT 1'.
      **********************************************************************
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER.
=======
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 182.
           05 SQL-STMT   PIC X(182) VALUE 'SELECT C.ID_CLIENTE,C.DOC_CLI
      -    'ENTE,C.NOMBRE_CLIENTE,C.APELLIDOS_CLIENTE,C.DIRECCION_CLIENT
      -    'E,C.TELEF_CLIENTE,C.EMAIL_CLIENTE FROM banco.clientes C WHER
      -    'E C.DOC_CLIENTE = TRIM(?) LIMIT 1'.
      **********************************************************************
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER VALUE NULL.
>>>>>>> origin/main
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 64.
           05 SQL-STMT   PIC X(64) VALUE 'UPDATE CLIENTES SET NOMBRE_CLI
      -    'ENTE = TRIM(?) WHERE ID_CLIENTE =?'.
      **********************************************************************
       01 SQL-STMT-5.
<<<<<<< HEAD
           05 SQL-IPTR   POINTER.
=======
           05 SQL-IPTR   POINTER VALUE NULL.
>>>>>>> origin/main
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 67.
           05 SQL-STMT   PIC X(67) VALUE 'UPDATE CLIENTES SET APELLIDOS_
      -    'CLIENTE = TRIM(?) WHERE ID_CLIENTE =?'.
      **********************************************************************
       01 SQL-STMT-6.
<<<<<<< HEAD
           05 SQL-IPTR   POINTER.
=======
           05 SQL-IPTR   POINTER VALUE NULL.
>>>>>>> origin/main
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 67.
           05 SQL-STMT   PIC X(67) VALUE 'UPDATE CLIENTES SET DIRECCION_
      -    'CLIENTE = TRIM(?) WHERE ID_CLIENTE =?'.
      **********************************************************************
<<<<<<< HEAD
       01 SQL-STMT-7.
           05 SQL-IPTR   POINTER.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 63.
           05 SQL-STMT   PIC X(63) VALUE 'UPDATE CLIENTES SET TELEF_CLIE
      -    'NTE = TRIM(?) WHERE ID_CLIENTE =?'.
      **********************************************************************
       01 SQL-STMT-8.
           05 SQL-IPTR   POINTER.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 63.
           05 SQL-STMT   PIC X(63) VALUE 'UPDATE CLIENTES SET EMAIL_CLIE
      -    'NTE = TRIM(?) WHERE ID_CLIENTE =?'.
      **********************************************************************
       01 SQL-STMT-9.
           05 SQL-IPTR   POINTER.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 63.
           05 SQL-STMT   PIC X(63) VALUE 'UPDATE CLIENTES SET EMAIL_CLIE
      -    'NTE = TRIM(?) WHERE ID_CLIENTE =?'.
      **********************************************************************
=======
>>>>>>> origin/main
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(3) COMP-3.
           05 SQL-VAR-0003  PIC S9(3) COMP-3.
           05 SQL-VAR-0004  PIC S9(1) COMP-3.
           05 SQL-VAR-0005  PIC S9(1) COMP-3.
           05 SQL-VAR-0006  PIC S9(1) COMP-3.
           05 SQL-VAR-0007  PIC S9(1) COMP-3.
           05 SQL-VAR-0008  PIC S9(13)V9(2) COMP-3.
      *******       END OF PRECOMPILER-GENERATED VARIABLES           *******
      **********************************************************************
<<<<<<< HEAD

       COPY "BD001".
=======
       01   DB-CONN.
           05  DB-USER                 PIC X(20) VALUE 'mysql'.
           05  DB-PASSWORD             PIC X(20) VALUE 'root'.
           05  DB-NAME                 PIC X(20) VALUE 'banco'.
           05  DB-HOST                 PIC X(20) VALUE 'localhost'.
           05  DB-PORT                 PIC 9(5)  VALUE 3306.
>>>>>>> origin/main

      *EXEC SQL
      *    BEGIN DECLARE SECTION
      *END-EXEC

       01  SEARCH-APELLI PIC X(10).

       01  DB-VARS.
           05  WS-MAX-ID      PIC 9(3).
           05  DB-DOCUMENT    PIC X(12).
           05  BUFFER                  PIC X(1024).
           05  ST-COUNT                PIC 9(6).
           05  CLIENT.
               10  ID-CLIENTE                PIC 9(3).
               10  TIPO-DOC                  PIC X(3).
               10  DOCUMENT                  PIC X(12).
               10  FECHA-ALTA                PIC X(10).
               10  NOMBRE                    PIC X(25).
               10  APELLIDO                  PIC X(25).
               10  DIRECCION                 PIC X(45).
               10  TELEFONO                  PIC X(12).
               10  MAIL                      PIC X(40).
               10  TARJETA                   PIC 9(1).
               10  CREDITO                    PIC 9(1).
               10  HIPOTECA                   PIC 9(1).
               10  ACTIVA                    PIC 9(1).
               10  FECHA-CIERRE              PIC X(10).
               10  SALDO                     PIC 9(12)V99.

<<<<<<< HEAD
=======


>>>>>>> origin/main
      *    EXEC SQL
      *        END DECLARE SECTION
      *    END-EXEC

       01  WS-MAINMENU.
           05 WS-LINE                  PIC X(30) VALUE ALL "-".
           05 WS-TITLE.
               10 WS-LEFT-WALL         PIC X(01) VALUE "|".
               10 WS-TXT-TITLE         PIC X(30) VALUE SPACES.
               10 WS-RGTH-WALL         PIC X(01) VALUE "|".
           05 WS-SUBTITLE.
               10 WS-SPACE             PIC X(05) VALUE SPACES.
               10 WS-TXT-SUBTITLE      PIC X(25) VALUE SPACES.
           05 WS-MENU.
               10 WS-TXTMN1 PIC X(30) VALUE "1. Registra nuevo cliente".
               10 WS-TXTMN2 PIC X(30) VALUE "2. Actualizar Info".
               10 WS-TXTMN3 PIC X(30) VALUE "3. Eliminar cliente".
               10 WS-TXTMN4 PIC X(30) VALUE "4. Consultar cliente".
               10 WS-TXTMN5 PIC X(30) VALUE "5. Salir".

       01  WS-UPDATE-MENU.
           05 WS-UD-MENU.
               10 WS-UD-MN1 PIC X(30) VALUE "1. Actualizar Nombre".
               10 WS-UD-MN2 PIC X(30) VALUE "2. Actualizar Apellido".
               10 WS-UD-MN3 PIC X(30) VALUE "3. Actualizar Dirección".
               10 WS-UD-MN4 PIC X(30) VALUE "4. Actualizar Telefono".
               10 WS-UD-MN5 PIC X(30) VALUE "5. Actualizar Correo".
               10 WS-UD-MN6 PIC X(30) VALUE "6. Volver".

       01  WS-CLIENT.
               05  WS-ID-CLIENTE                PIC 9(3).
               05  WS-TIPO-DOC                  PIC X(3).
               05  WS-DOCUMENT                  PIC X(12).
               05  WS-FECHA-ALTA                PIC X(10).
               05  WS-NOMBRE                    PIC X(25).
               05  WS-APELLIDO                  PIC X(25).
               05  WS-DIRECCION                 PIC X(45).
               05  WS-TELEFONO                  PIC X(12).
               05  WS-MAIL                      PIC X(40).
               05  WS-TARJETA                   PIC 9(1).
               05  WS-CREDITO                   PIC 9(1).
               05  WS-HIPOTECA                  PIC 9(1).
               05  WS-ACTIVA                    PIC 9(1).
               05  WS-FECHA-CIERRE              PIC X(10).
               05  WS-SALDO                     PIC 9(12)V99.

       01  WS-OPTION                            PIC 9 VALUE 0.
<<<<<<< HEAD
       01  WS-SALDO-PRNT                        PIC Z9(12),99.
=======

       *>01  WS-FECHA-RAW   PIC X(6).

       *> 01  WS-FECHA-FORMAT.
           *> 02  WS-YYYY    PIC 9(4).
           *> 02  WS-SEP1    PIC X VALUE "-".
           *> 02  WS-MM      PIC 99.
           *> 02  WS-SEP2    PIC X VALUE "-".
           *> 02  WS-DD      PIC 99.
>>>>>>> origin/main

       LINKAGE SECTION.
       01  LK-OPTION PIC 9(1).
       PROCEDURE DIVISION USING LK-OPTION.
           PERFORM 0100-INICIO THRU 0210-CLIENTES.
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
<<<<<<< HEAD
=======
               SET SQL-HCONN OF SQLCA TO NULL
>>>>>>> origin/main
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA

           DISPLAY 'BASE DE DATOS ACTUAL: ' BUFFER.

           PERFORM 0291-SQLSTATE-CHECK.
      *-----------------------------------------------------------------
       0110-END.

       0100-END.

       0200-PROCEDIMIENTO.

       0210-CLIENTES.
<<<<<<< HEAD
           INITIALIZE WS-OPTION WS-TXT-TITLE.
           MOVE "Menu clientes   " TO WS-TXT-TITLE(07:16).
=======
       *>    MENU TITLE
           GO TO 0240-UPDT-CLIENT

           INITIALIZE WS-OPTION.
           MOVE "Menu clientes" TO WS-TXT-TITLE(07:13).
>>>>>>> origin/main
           MOVE ":Modulos disponibles:" TO WS-TXT-SUBTITLE.

           PERFORM 0221-PRINT-MENU.
           DISPLAY "Ingrese una opcion: ".
           ACCEPT WS-OPTION.

           EVALUATE WS-OPTION
               WHEN 1
                *>   DISPLAY "nuevo cliente..."
                   PERFORM 0230-NEW-CLIENT
                   PERFORM 0210-CLIENTES
               WHEN 2
           *>   Actualizar info Cliente
<<<<<<< HEAD
           *>        DISPLAY "Actualizar clientes"
                   PERFORM 0240-UPDATE-CLIENT
                   PERFORM 0210-CLIENTES
               WHEN 3
               *> Eliminar clientes
                   PERFORM 0250-BAJA-CLIENTE
                   PERFORM 0210-CLIENTES
               WHEN 4
                   PERFORM 0260-DETALLE-CLIENTE
                   PERFORM 0210-CLIENTES
=======
                   DISPLAY "Actualizar clientes"
                   PERFORM 0240-UPDT-CLIENT
               WHEN 3

                   PERFORM 0210-CLIENTES
               WHEN 4

                   PERFORM 0210-CLIENTES

>>>>>>> origin/main
               WHEN 5
                   PERFORM 0300-FIN

               WHEN OTHER
                   DISPLAY "(" WS-OPTION ") - " "Opcion invalida."
                   PERFORM 0210-CLIENTES

<<<<<<< HEAD
           DISPLAY "DEBUG: (-1) Salida incorrecta".
=======
           DISPLAY "Salida incorrecta".
>>>>>>> origin/main

       0221-PRINT-MENU.
           DISPLAY "+" WS-LINE "+".
           DISPLAY WS-TITLE.
           DISPLAY "+" WS-LINE "+".
           DISPLAY WS-LEFT-WALL WS-SPACE WS-TXT-SUBTITLE WS-RGTH-WALL.
           DISPLAY WS-LEFT-WALL WS-TXTMN1 WS-RGTH-WALL.
           DISPLAY WS-LEFT-WALL WS-TXTMN2 WS-RGTH-WALL.
           DISPLAY WS-LEFT-WALL WS-TXTMN3 WS-RGTH-WALL.
           DISPLAY WS-LEFT-WALL WS-TXTMN4 WS-RGTH-WALL.
           DISPLAY WS-LEFT-WALL WS-TXTMN5 WS-RGTH-WALL.
           DISPLAY "+" WS-LINE "+".
       0221-END.

<<<<<<< HEAD
       0222-PRINT-UPDATE-MENU.
=======
       0222-PRINT-UD-MENU.
>>>>>>> origin/main
           DISPLAY "+" WS-LINE "+".
           DISPLAY WS-TITLE.
           DISPLAY "+" WS-LINE "+".
           DISPLAY WS-LEFT-WALL WS-SPACE WS-TXT-SUBTITLE WS-RGTH-WALL.
           DISPLAY WS-LEFT-WALL WS-UD-MN1 WS-RGTH-WALL.
           DISPLAY WS-LEFT-WALL WS-UD-MN2 WS-RGTH-WALL.
           DISPLAY WS-LEFT-WALL WS-UD-MN3 WS-RGTH-WALL.
           DISPLAY WS-LEFT-WALL WS-UD-MN4 WS-RGTH-WALL.
           DISPLAY WS-LEFT-WALL WS-UD-MN5 WS-RGTH-WALL.
           DISPLAY WS-LEFT-WALL WS-UD-MN6 WS-RGTH-WALL.
           DISPLAY "+" WS-LINE "+".
       0222-END.

       0230-NEW-CLIENT.
           MOVE "Registrar cliente" TO WS-TXT-TITLE(07:17).
           DISPLAY "+" WS-LINE "+".
           DISPLAY WS-TITLE.
           DISPLAY "+" WS-LINE "+".

           DISPLAY "-Inserte el tipo de documento:"
           ACCEPT WS-TIPO-DOC
           *> VALIDAR TIPO DOC

           DISPLAY "Inserte el documento: "
           ACCEPT WS-DOCUMENT
           *> VALIDAR DOCUENT

           DISPLAY "Inserte el Nombre:"
           ACCEPT WS-NOMBRE

           DISPLAY "Inserte el Apellido:"
           ACCEPT WS-APELLIDO

           DISPLAY "Inserte la direccion:"
           ACCEPT WS-DIRECCION

           DISPLAY "Inserte el telefono:"
           ACCEPT WS-TELEFONO

           DISPLAY "Inserte el correo:"
           ACCEPT WS-MAIL

<<<<<<< HEAD
           *> WS-MAX-ID Indice para el siguiente registro
=======
           *> Indice para el siguiente registro
>>>>>>> origin/main
      *    EXEC SQL
      *        SELECT MAX(ID_CLIENTE) + 1
      *        INTO :WS-MAX-ID
      *        FROM clientes
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
<<<<<<< HEAD
=======
               SET SQL-HCONN OF SQLCA TO NULL
>>>>>>> origin/main
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA
           MOVE SQL-VAR-0001 TO WS-MAX-ID

           PERFORM 0231-INSERT-CLIENT.
       0230-END.

       0231-INSERT-CLIENT.
           *> Mover de WS a las de DB
           MOVE WS-MAX-ID TO ID-CLIENTE.
           MOVE WS-TIPO-DOC TO TIPO-DOC.
           MOVE WS-DOCUMENT TO DOCUMENT.
           *>MOVE WS-FECHA-FORMAT TO FECHA-ALTA.
           MOVE WS-NOMBRE TO NOMBRE.
           MOVE WS-APELLIDO TO APELLIDO.
           MOVE WS-TELEFONO TO TELEFONO.
           MOVE WS-MAIL TO MAIL.
           MOVE WS-DIRECCION TO DIRECCION.

           *> Valores por defecto para cuentas nuevas
           MOVE 1 TO WS-ACTIVA
           MOVE 0.0 TO WS-SALDO
           MOVE 0 TO TARJETA.
           MOVE 0 TO CREDITO.
           MOVE 0 TO HIPOTECA.

      *    EXEC SQL
      *        INSERT INTO clientes
      *        VALUES(
      *             :ID-CLIENTE,
      *             :TIPO-DOC,
      *             :DOCUMENT,
      *             CURRENT_DATE,
      *             :NOMBRE,
      *             :APELLIDO,
      *             :DIRECCION,
      *             :TELEFONO,
      *             :MAIL,
      *             :TARJETA,
      *             :CREDITO,
      *             :HIPOTECA,
      *             :ACTIVA,
      *             null,
      *             :SALDO
      *             )
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(1)
               MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 TIPO-DOC
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 3 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 DOCUMENT
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 12 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 NOMBRE
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 25 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 APELLIDO
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 25 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 DIRECCION
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 45 TO SQL-LEN(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 TELEFONO
               MOVE 'X' TO SQL-TYPE(7)
               MOVE 12 TO SQL-LEN(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 MAIL
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 40 TO SQL-LEN(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(9)
               MOVE 1 TO SQL-LEN(9)
               MOVE X'00' TO SQL-PREC(9)
               SET SQL-ADDR(10) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(10)
               MOVE 1 TO SQL-LEN(10)
               MOVE X'00' TO SQL-PREC(10)
               SET SQL-ADDR(11) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(11)
               MOVE 1 TO SQL-LEN(11)
               MOVE X'00' TO SQL-PREC(11)
               SET SQL-ADDR(12) TO ADDRESS OF
                 SQL-VAR-0007
               MOVE '3' TO SQL-TYPE(12)
               MOVE 1 TO SQL-LEN(12)
               MOVE X'00' TO SQL-PREC(12)
               SET SQL-ADDR(13) TO ADDRESS OF
                 SQL-VAR-0008
               MOVE '3' TO SQL-TYPE(13)
               MOVE 8 TO SQL-LEN(13)
               MOVE X'02' TO SQL-PREC(13)
               MOVE 13 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
<<<<<<< HEAD
=======
               SET SQL-HCONN OF SQLCA TO NULL
>>>>>>> origin/main
           END-IF
           MOVE ID-CLIENTE
             TO SQL-VAR-0003
           MOVE TARJETA
             TO SQL-VAR-0004
           MOVE CREDITO
             TO SQL-VAR-0005
           MOVE HIPOTECA
             TO SQL-VAR-0006
           MOVE ACTIVA
             TO SQL-VAR-0007
           MOVE SALDO
             TO SQL-VAR-0008
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA

           PERFORM 0291-COMMIT.
       0231-END.


<<<<<<< HEAD
       0232-SEARCH-CLIENT.
=======
       0232-SEARCH-CLIE.
>>>>>>> origin/main
           MOVE WS-DOCUMENT TO DB-DOCUMENT

      *    EXEC SQL
      *    SELECT  C.ID_CLIENTE,
      *            C.DOC_CLIENTE,
      *            C.NOMBRE_CLIENTE,
      *            C.APELLIDOS_CLIENTE,
      *            C.DIRECCION_CLIENTE,
      *            C.TELEF_CLIENTE,
<<<<<<< HEAD
      *            C.EMAIL_CLIENTE,
      *            C.TARJETA,
      *            C.CREDITO,
      *            C.HIPOTECA,
      *            C.CTA_ACTIVA,
      *            C.SALDO_CLIENTE,
      *            C.FECHA_CIERRE
=======
      *            C.EMAIL_CLIENTE
>>>>>>> origin/main

      *    INTO    :ID-CLIENTE,
      *            :DOCUMENT,
      *            :NOMBRE,
      *            :APELLIDO,
      *            :DIRECCION,
      *            :TELEFONO,
<<<<<<< HEAD
      *            :MAIL,
      *            :TARJETA,
      *            :CREDITO,
      *            :HIPOTECA,
      *            :ACTIVA,
      *            :SALDO,
      *            :FECHA-CIERRE
=======
      *            :MAIL
>>>>>>> origin/main

      *    FROM banco.clientes C
      *    WHERE C.DOC_CLIENTE = TRIM(:DB-DOCUMENT)
      *    LIMIT 1
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-3 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0003
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
                 DIRECCION
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 45 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 TELEFONO
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 12 TO SQL-LEN(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 MAIL
               MOVE 'X' TO SQL-TYPE(7)
               MOVE 40 TO SQL-LEN(7)
               SET SQL-ADDR(8) TO ADDRESS OF
<<<<<<< HEAD
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(8)
               MOVE 1 TO SQL-LEN(8)
               MOVE X'00' TO SQL-PREC(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(9)
               MOVE 1 TO SQL-LEN(9)
               MOVE X'00' TO SQL-PREC(9)
               SET SQL-ADDR(10) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(10)
               MOVE 1 TO SQL-LEN(10)
               MOVE X'00' TO SQL-PREC(10)
               SET SQL-ADDR(11) TO ADDRESS OF
                 SQL-VAR-0007
               MOVE '3' TO SQL-TYPE(11)
               MOVE 1 TO SQL-LEN(11)
               MOVE X'00' TO SQL-PREC(11)
               SET SQL-ADDR(12) TO ADDRESS OF
                 SQL-VAR-0008
               MOVE '3' TO SQL-TYPE(12)
               MOVE 8 TO SQL-LEN(12)
               MOVE X'02' TO SQL-PREC(12)
               SET SQL-ADDR(13) TO ADDRESS OF
                 FECHA-CIERRE
               MOVE 'X' TO SQL-TYPE(13)
               MOVE 10 TO SQL-LEN(13)
               SET SQL-ADDR(14) TO ADDRESS OF
                 DB-DOCUMENT
               MOVE 'X' TO SQL-TYPE(14)
               MOVE 12 TO SQL-LEN(14)
               MOVE 14 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-3
                                   SQLCA
=======
                 DB-DOCUMENT
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 12 TO SQL-LEN(8)
               MOVE 8 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-3
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
>>>>>>> origin/main
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-3
                               SQLCA
           MOVE SQL-VAR-0003 TO ID-CLIENTE
<<<<<<< HEAD
           MOVE SQL-VAR-0004 TO TARJETA
           MOVE SQL-VAR-0005 TO CREDITO
           MOVE SQL-VAR-0006 TO HIPOTECA
           MOVE SQL-VAR-0007 TO ACTIVA
           MOVE SQL-VAR-0008 TO SALDO
                   .

           PERFORM 0291-SQLSTATE-CHECK.
       0232-END.

       0233-UPDATE-DATA-CLIENT.
           INITIALIZE WS-CLIENT
=======
                   .

           PERFORM 0291-SQLSTATE-CHECK

           IF SQLCODE NOT EQUAL 0
           DISPLAY "Client not Found"
           PERFORM 0240-UPDT-CLIENT
           END-IF.

       0232-END.

       0233-UTD-CLIENT.
>>>>>>> origin/main

           EVALUATE WS-OPTION
               WHEN 1
                   DISPLAY "Inserte el nuevo nombre: "
                   ACCEPT WS-NOMBRE

                   MOVE WS-NOMBRE TO NOMBRE

      *            EXEC SQL
      *            UPDATE CLIENTES
      *            SET  NOMBRE_CLIENTE = TRIM(:NOMBRE)
      *            WHERE ID_CLIENTE =: ID-CLIENTE
      *            END-EXEC
           IF SQL-PREP OF SQL-STMT-4 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 NOMBRE
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 25 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(2)
               MOVE 2 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-4
                                   SQLCA
<<<<<<< HEAD
=======
               SET SQL-HCONN OF SQLCA TO NULL
>>>>>>> origin/main
           END-IF
           MOVE ID-CLIENTE
             TO SQL-VAR-0003
           CALL 'OCSQLEXE' USING SQL-STMT-4
                               SQLCA

                   PERFORM 0291-SQLSTATE-CHECK

                   PERFORM 0291-COMMIT

                   IF SQLCODE = 0
                   DISPLAY "Se ha actualizado el Nombre del cliente"
                   END-IF

                WHEN 2
                   DISPLAY "Inserte el nuevo apellido: "
                   ACCEPT WS-APELLIDO

                   MOVE WS-APELLIDO TO APELLIDO

      *            EXEC SQL
      *            UPDATE CLIENTES
      *            SET  APELLIDOS_CLIENTE = TRIM(:APELLIDO)
      *            WHERE ID_CLIENTE =: ID-CLIENTE
      *            END-EXEC
           IF SQL-PREP OF SQL-STMT-5 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 APELLIDO
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 25 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(2)
               MOVE 2 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-5
                                   SQLCA
<<<<<<< HEAD
=======
               SET SQL-HCONN OF SQLCA TO NULL
>>>>>>> origin/main
           END-IF
           MOVE ID-CLIENTE
             TO SQL-VAR-0003
           CALL 'OCSQLEXE' USING SQL-STMT-5
                               SQLCA

                   PERFORM 0291-SQLSTATE-CHECK

                   PERFORM 0291-COMMIT

                   IF SQLCODE = 0
                   DISPLAY "Se ha actualizado el Apellido del cliente"
                   END-IF

                WHEN 3
                   DISPLAY "Inserte nueva direccion: "
                   ACCEPT WS-DIRECCION

                   MOVE WS-DIRECCION TO DIRECCION

      *            EXEC SQL
      *            UPDATE CLIENTES
      *            SET  DIRECCION_CLIENTE = TRIM(:DIRECCION)
      *            WHERE ID_CLIENTE =: ID-CLIENTE
      *            END-EXEC
           IF SQL-PREP OF SQL-STMT-6 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 DIRECCION
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 45 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(2)
               MOVE 2 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-6
                                   SQLCA
<<<<<<< HEAD
=======
               SET SQL-HCONN OF SQLCA TO NULL
>>>>>>> origin/main
           END-IF
           MOVE ID-CLIENTE
             TO SQL-VAR-0003
           CALL 'OCSQLEXE' USING SQL-STMT-6
                               SQLCA

                   PERFORM 0291-SQLSTATE-CHECK

                   PERFORM 0291-COMMIT

                   IF SQLCODE = 0
                   DISPLAY "Se ha actualizado la direccion del cliente"
                   END-IF

<<<<<<< HEAD
                WHEN 4
                   DISPLAY "Inserte nuevo no. de telefono: "
                   ACCEPT WS-TELEFONO

                   MOVE WS-TELEFONO TO TELEFONO

      *            EXEC SQL
      *            UPDATE CLIENTES
      *            SET  TELEF_CLIENTE = TRIM(:TELEFONO)
      *            WHERE ID_CLIENTE =: ID-CLIENTE
      *            END-EXEC
           IF SQL-PREP OF SQL-STMT-7 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 TELEFONO
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 12 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(2)
               MOVE 2 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-7
                                   SQLCA
           END-IF
           MOVE ID-CLIENTE
             TO SQL-VAR-0003
           CALL 'OCSQLEXE' USING SQL-STMT-7
                               SQLCA

                   PERFORM 0291-SQLSTATE-CHECK

                   PERFORM 0291-COMMIT

                   IF SQLCODE = 0
                   DISPLAY "Se ha actualizado el telefono del cliente"
                   END-IF

                WHEN 5
                   DISPLAY "Inserte nuevo correo electronico: "
                   ACCEPT WS-MAIL

                   MOVE WS-MAIL TO MAIL

      *            EXEC SQL
      *            UPDATE CLIENTES
      *            SET  EMAIL_CLIENTE = TRIM(:MAIL)
      *            WHERE ID_CLIENTE =: ID-CLIENTE
      *            END-EXEC
           IF SQL-PREP OF SQL-STMT-8 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 MAIL
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 40 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(2)
               MOVE 2 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-8
                                   SQLCA
           END-IF
           MOVE ID-CLIENTE
             TO SQL-VAR-0003
           CALL 'OCSQLEXE' USING SQL-STMT-8
                               SQLCA

                   PERFORM 0291-SQLSTATE-CHECK

                   PERFORM 0291-COMMIT

                   IF SQLCODE = 0
                   DISPLAY "Se ha actualizado el correo del cliente"
                   END-IF

                WHEN 6
                   DISPLAY "Regresando..."
                   EXIT PARAGRAPH

           END-EVALUATE.
       0233-END.


       0240-UPDATE-CLIENT.
=======
           END-EVALUATE.

       0233-END.



       0240-UPDT-CLIENT.
>>>>>>> origin/main
           INITIALIZE CLIENT WS-OPTION.

           MOVE "Actualizar info." TO WS-TXT-TITLE(07:16).
           DISPLAY "+" WS-LINE "+".
           DISPLAY WS-TITLE.
           DISPLAY "+" WS-LINE "+".
<<<<<<< HEAD
           DISPLAY "Ingrese (-1) para salir"
           DISPLAY "Ingrese el Docuemento del cliente:"
           ACCEPT WS-DOCUMENT

           MOVE FUNCTION TRIM(WS-DOCUMENT) TO WS-DOCUMENT

           DISPLAY "Documento ingresado:" WS-DOCUMENT "$"

           IF WS-DOCUMENT = "-1"
               DISPLAY "Regresando a Menu Clientes..."
               EXIT PARAGRAPH
           END-IF

           PERFORM 0232-SEARCH-CLIENT.

           IF SQLCODE NOT EQUAL 0
               DISPLAY "Client not Found"
               PERFORM 0240-UPDATE-CLIENT
           END-IF

           PERFORM UNTIL WS-OPTION = 6
               PERFORM 0241-PRNT-MN-UPDATE
               DISPLAY "Dato a actualizar:"
               ACCEPT WS-OPTION
               PERFORM 0233-UPDATE-DATA-CLIENT
           END-PERFORM.
       0240-END.


       0250-BAJA-CLIENTE.
           MOVE "Dar de Baja a Cliente" TO WS-TXT-TITLE(03:21)
           DISPLAY "+" WS-LINE "+"
           DISPLAY WS-TITLE
           DISPLAY "+" WS-LINE "+"

           DISPLAY "(-1) Para salir"
           DISPLAY "Ingrese el Documento del cliente a dar de baja:"
           ACCEPT WS-DOCUMENT

           IF WS-DOCUMENT = "-1"
               DISPLAY "Regresando a Menu Clientes..."
               EXIT PARAGRAPH
           END-IF

           PERFORM 0232-SEARCH-CLIENT.

           IF SQLCODE NOT EQUAL 0
               DISPLAY "Client not Found"
               PERFORM 0250-BAJA-CLIENTE
           END-IF

           IF TARJETA = 0  AND CREDITO = 0 AND HIPOTECA = 0
               AND ACTIVA = 1 AND SALDO = 0.00
               PERFORM 0251-UPDATE-BAJA
           ELSE
               PERFORM 0243-ERROR-UPDATE
               PERFORM 0250-BAJA-CLIENTE
           END-IF.
       0250-END.

       0251-UPDATE-BAJA.
      *    EXEC SQL
      *        UPDATE CLIENTES
      *        SET  EMAIL_CLIENTE = TRIM(:MAIL)
      *        WHERE ID_CLIENTE =: ID-CLIENTE
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-9 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 MAIL
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 40 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(2)
               MOVE 2 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-9
                                   SQLCA
           END-IF
           MOVE ID-CLIENTE
             TO SQL-VAR-0003
           CALL 'OCSQLEXE' USING SQL-STMT-9
                               SQLCA

           PERFORM 0291-COMMIT

           IF SQLCODE = 0
               DISPLAY "Se ha dado de baja al cliente"
           END-IF.
       0251-END.

       0260-DETALLE-CLIENTE.
           MOVE "Detalle de cliente" TO WS-TXT-TITLE(04:18)
           DISPLAY "+" WS-LINE "+"
           DISPLAY WS-TITLE
           DISPLAY "+" WS-LINE "+"

           DISPLAY "(-1) Para salir"
           DISPLAY "Ingrese el Documento del cliente a consultar:"
           ACCEPT WS-DOCUMENT

           IF WS-DOCUMENT = "-1"
               DISPLAY "Regresando a Menu Clientes..."
               EXIT PARAGRAPH
           END-IF

           PERFORM 0232-SEARCH-CLIENT.

           IF SQLCODE NOT EQUAL 0
               DISPLAY "Client not Found"
               PERFORM 0260-DETALLE-CLIENTE
           END-IF

           PERFORM 0261-PRNT-DETALLE-CLIENTE

           PERFORM 0260-DETALLE-CLIENTE.

       0260-END.

       0261-PRNT-DETALLE-CLIENTE.

           INITIALIZE WS-SALDO-PRNT
           MOVE SALDO TO WS-SALDO-PRNT

           DISPLAY "**********************************************"
           DISPLAY "          DETALLE DE CUENTA                  "
           DISPLAY "**********************************************"
           DISPLAY " Cliente ID        : " FUNCTION TRIM(ID-CLIENTE)
           DISPLAY " Documento         : " FUNCTION TRIM(DOCUMENT)
           DISPLAY " Nombre            : " FUNCTION TRIM(NOMBRE)
           DISPLAY " Apellidos         : " FUNCTION TRIM(APELLIDO)
           DISPLAY " Dirección         : " FUNCTION TRIM(DIRECCION)
           DISPLAY " Teléfono          : " FUNCTION TRIM(TELEFONO)
           DISPLAY " Correo            : " FUNCTION TRIM(MAIL)
           DISPLAY "----------------------------------------------"

           IF TARJETA = 1
               DISPLAY " Tarjeta: Si"
           ELSE
               DISPLAY " Tarjeta: No"
           END-IF

           IF CREDITO = 1
               DISPLAY " Credito: Si"
           ELSE
               DISPLAY " Credito: No"
           END-IF

           IF HIPOTECA = 1
               DISPLAY " Hipoteca: Si"
           ELSE
               DISPLAY " Hipoteca: No"
           END-IF

           IF ACTIVA = 1
               DISPLAY " Cuenta activa"
           ELSE
               DISPLAY " Cuenta dada de baja "
               DISPLAY "En: " FUNCTION TRIM(FECHA-CIERRE)
           END-IF

           DISPLAY "----------------------------------------------"
           DISPLAY " Saldo disponible  : $" WS-SALDO-PRNT
           DISPLAY "**********************************************".


       0261-END.



       0241-PRNT-MN-UPDATE.
=======
           DISPLAY "Ingrese el Docuemento del cliente:"
           ACCEPT WS-DOCUMENT

           PERFORM 0232-SEARCH-CLIE.

           PERFORM UNTIL WS-OPTION = 6
               PERFORM 0241-PRNT-MN-UPDT
               DISPLAY "Dato a actualizar:"
               ACCEPT WS-OPTION
               PERFORM 0233-UTD-CLIENT

           END-PERFORM.


       0240-END.

       0241-PRNT-MN-UPDT.
>>>>>>> origin/main
           MOVE "Actualizar info." TO WS-TXT-TITLE(07:16)
           DISPLAY "+" WS-LINE "+"
           DISPLAY WS-TITLE
           DISPLAY "+" WS-LINE "+"

<<<<<<< HEAD
           PERFORM 0242-PRNT-CLIENT
=======

           PERFORM 0242-PRNT-CLT


           MOVE "1. Actualizar Nombre" TO WS-TXTMN1
           MOVE "2. Actualizar Apellido" TO WS-TXTMN2
           MOVE "3. Actualizar Dirección" TO WS-TXTMN3
           MOVE "4. Actualizar Telefono" TO WS-TXTMN4
           MOVE "5. Actualizar Correo" TO WS-TXTMN5
           MOVE "6. Volver" TO WS-TXTMN5


>>>>>>> origin/main
           DISPLAY WS-LEFT-WALL WS-SPACE WS-TXT-SUBTITLE WS-RGTH-WALL
           DISPLAY WS-LEFT-WALL WS-UD-MN1 WS-RGTH-WALL
           DISPLAY WS-LEFT-WALL WS-UD-MN2 WS-RGTH-WALL
           DISPLAY WS-LEFT-WALL WS-UD-MN3 WS-RGTH-WALL
           DISPLAY WS-LEFT-WALL WS-UD-MN4 WS-RGTH-WALL
           DISPLAY WS-LEFT-WALL WS-UD-MN5 WS-RGTH-WALL
           DISPLAY WS-LEFT-WALL WS-UD-MN6 WS-RGTH-WALL
           DISPLAY "+" WS-LINE "+".
<<<<<<< HEAD
       0241-END.

       0242-PRNT-CLIENT.
=======

       0241-END.

       0242-PRNT-CLT.

>>>>>>> origin/main
           DISPLAY '--------------------------------'
           DISPLAY "Informacion del cliente"
           DISPLAY '--------------------------------'
           DISPLAY '> DOCUMENT   | ' DOCUMENT
           DISPLAY '> NOMBRE     | ' NOMBRE
           DISPLAY '> APELLIDO   | ' APELLIDO
           DISPLAY '> DIRECCION  | ' DIRECCION
           DISPLAY '> TELEFONO   | ' TELEFONO
           DISPLAY '> MAIL       | ' MAIL
           DISPLAY '--------------------------------'.
<<<<<<< HEAD
       0242-END.

       0243-ERROR-UPDATE.
           DISPLAY "x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x"
           DISPLAY "La cuenta no se puede cerrar debido a:"

           IF TARJETA = 1
               DISPLAY "La cuenta posee tarjetas activas."
           END-IF.

           IF CREDITO = 1
               DISPLAY "La cuenta posee creditos pendientes."
           END-IF.

           IF HIPOTECA = 1
               DISPLAY "La cuenta posee una hipoteca pendiente."
           END-IF.

           IF ACTIVA = 0
               DISPLAY "La cuenta ya no esta ACTIVA."
           END-IF.

           IF SALDO NOT = 0.00
               DISPLAY "La cuenta posee saldo."
           END-IF.
           DISPLAY "x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x".
       0243-END.

       0291-COMMIT.
=======

       0242-END.

       0291-COMMIT.
      *-----------------------------------------------------------------
      * COMMIT CHANGES
      *-----------------------------------------------------------------
>>>>>>> origin/main
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
               IF SQLCODE > 0 AND NOT = 100
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
       EXIT PROGRAM.
       0300-END.

      *-----------------------------------------------------------------
      * REVISA SQLSTATE E IMPRIME ERRORES SI EXISTEN
      *-----------------------------------------------------------------

       END PROGRAM MODCLI001.
      **********************************************************************
<<<<<<< HEAD
      *  : ESQL for GnuCOBOL/OpenCobol Version 2 (2021.05.29) Build May 29 2021
=======
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024
>>>>>>> origin/main

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  ACTIVA                   IN USE THROUGH TEMP VAR SQL-VAR-0007 DECIMAL(1,0)
      *  APELLIDO                 IN USE CHAR(25)
      *  BUFFER                   IN USE CHAR(1024)
      *  CLIENT               NOT IN USE
      *  CREDITO                  IN USE THROUGH TEMP VAR SQL-VAR-0005 DECIMAL(1,0)
      *  DB-DOCUMENT              IN USE CHAR(12)
      *  DB-VARS              NOT IN USE
      *  DB-VARS.ACTIVA       NOT IN USE
      *  DB-VARS.APELLIDO     NOT IN USE
      *  DB-VARS.BUFFER       NOT IN USE
      *  DB-VARS.CLIENT       NOT IN USE
      *  DB-VARS.CREDITO      NOT IN USE
      *  DB-VARS.DB-DOCUMENT  NOT IN USE
      *  DB-VARS.DIRECCION    NOT IN USE
      *  DB-VARS.DOCUMENT     NOT IN USE
      *  DB-VARS.FECHA-ALTA   NOT IN USE
      *  DB-VARS.FECHA-CIERRE NOT IN USE
      *  DB-VARS.HIPOTECA     NOT IN USE
      *  DB-VARS.ID-CLIENTE   NOT IN USE
      *  DB-VARS.MAIL         NOT IN USE
      *  DB-VARS.NOMBRE       NOT IN USE
      *  DB-VARS.SALDO        NOT IN USE
      *  DB-VARS.ST-COUNT     NOT IN USE
      *  DB-VARS.TARJETA      NOT IN USE
      *  DB-VARS.TELEFONO     NOT IN USE
      *  DB-VARS.TIPO-DOC     NOT IN USE
      *  DB-VARS.WS-MAX-ID    NOT IN USE
      *  DIRECCION                IN USE CHAR(45)
      *  DOCUMENT                 IN USE CHAR(12)
      *  FECHA-ALTA           NOT IN USE
<<<<<<< HEAD
      *  FECHA-CIERRE             IN USE CHAR(10)
=======
      *  FECHA-CIERRE         NOT IN USE
>>>>>>> origin/main
      *  HIPOTECA                 IN USE THROUGH TEMP VAR SQL-VAR-0006 DECIMAL(1,0)
      *  ID-CLIENTE               IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(3,0)
      *  MAIL                     IN USE CHAR(40)
      *  NOMBRE                   IN USE CHAR(25)
      *  SALDO                    IN USE THROUGH TEMP VAR SQL-VAR-0008 DECIMAL(15,2)
      *  SEARCH-APELLI        NOT IN USE
      *  ST-COUNT             NOT IN USE
      *  TARJETA                  IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(1,0)
      *  TELEFONO                 IN USE CHAR(12)
      *  TIPO-DOC                 IN USE CHAR(3)
      *  WS-MAX-ID                IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(3,0)
      **********************************************************************
