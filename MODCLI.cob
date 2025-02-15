      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MODCLI.
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
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 2.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 2 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 2 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 2 TIMES.
           05 SQL-PREC   PIC X OCCURS 2 TIMES.
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
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 40.
           05 SQL-STMT   PIC X(40) VALUE 'SELECT MAX(ID_CLIENTE) + 1 FRO
      -    'M clientes'.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 30.
           05 SQL-STMT   PIC X(30) VALUE 'INSERT INTO clientes VALUES(?)
      -    ''.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0008  PIC S9(3) COMP-3.
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

       01  SEARCH-APELLI PIC X(10).

       01  DB-VARS.
           05  BUFFER                  PIC X(1024).
           05  ST-COUNT                PIC 9(6).
           05  CLIENT.
               10  ID-CLIENTE                 PIC 9(3).
               10  TIPO-DOC                   PIC X(3).
               10  DOCUMENT                PIC X(12).
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

       01  WS-MAX-ID      PIC 9(3).

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
               10 WS-TXTMN2 PIC X(30) VALUE "2. actualizar Info".
               10 WS-TXTMN3 PIC X(30) VALUE "3. eliminar cliente".
               10 WS-TXTMN4 PIC X(30) VALUE "4. Consultar cliente".
               10 WS-TXTMN5 PIC X(30) VALUE "5. Salir".

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
               05  WS-CREDITO                    PIC 9(1).
               05  WS-HIPOTECA                  PIC 9(1).
               05  WS-ACTIVA                    PIC 9(1).
               05  WS-FECHA-CIERRE              PIC X(10).
               05  WS-SALDO                     PIC 9(12)V99.

       01  WS-OPTION                         PIC 9 VALUE 0.

       01  WS-FECHA-RAW   PIC X(6).


       01  WS-FECHA-FORMAT.
           02  WS-YYYY    PIC 9(4).
           02  WS-SEP1    PIC X VALUE "-".
           02  WS-MM      PIC 99.
           02  WS-SEP2    PIC X VALUE "-".
           02  WS-DD      PIC 99.

       PROCEDURE DIVISION.
           PERFORM 0100-INICIO THRU 0200-PROCEDIMIENTO.
           STOP RUN.

       0100-INICIO.
      *    MENU TITLE
           MOVE "MAIN MENU" TO WS-TXT-TITLE(11:9).
           MOVE ":Modulos disponibles:" TO WS-TXT-SUBTITLE.

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

           PERFORM SQLSTATE-CHECK.
      *-----------------------------------------------------------------
       0110-END.


       0100-END.

       0200-PROCEDIMIENTO.
           INITIALIZE WS-OPTION.
           PERFORM 0220-PRINT-MENU.
           DISPLAY "Ingrese una opcion: ".
           ACCEPT WS-OPTION.

           EVALUATE WS-OPTION
               WHEN 1
                   DISPLAY "nuevo cliente..."
                   PERFORM 0230-NEW-CLIENT
                   PERFORM 0200-PROCEDIMIENTO
               WHEN 2

                   PERFORM 0200-PROCEDIMIENTO
               WHEN 3

                   PERFORM 0200-PROCEDIMIENTO
               WHEN 4

                   PERFORM 0200-PROCEDIMIENTO

               WHEN 5
                   DISPLAY "Saliendo del Sistema... Adios..."
                   STOP RUN

               WHEN OTHER
                   DISPLAY "Opcion invalida."
                   PERFORM 0200-PROCEDIMIENTO.



       0220-PRINT-MENU.
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
       0220-END.

       0230-NEW-CLIENT.

           MOVE "New Client" TO WS-TXT-TITLE(11:10).
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

           *> PREGUNTAR POR TARJETA
           MOVE 1 TO WS-ACTIVA
           MOVE 0.0 TO WS-SALDO

           ACCEPT WS-FECHA-RAW  FROM DATE
           MOVE "20" TO WS-YYYY (1:2)
           MOVE WS-FECHA-RAW(1:2) TO WS-YYYY (3:2)
           MOVE WS-FECHA-RAW(3:2) TO WS-MM
           MOVE WS-FECHA-RAW(5:2) TO WS-DD

      *    EXEC SQL
      *        SELECT MAX(ID_CLIENTE) + 1
      *        INTO :WS-MAX-ID
      *        FROM clientes
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0008
               MOVE '3' TO SQL-TYPE(1)
               MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA
           MOVE SQL-VAR-0008 TO WS-MAX-ID

           DISPLAY "MAXIMO: " WS-MAX-ID

           PERFORM SQLSTATE-CHECK.

           MOVE WS-MAX-ID TO ID-CLIENTE.
           MOVE WS-TIPO-DOC TO TIPO-DOC.
           MOVE WS-DOCUMENT TO DOCUMENT.
           MOVE WS-FECHA-FORMAT TO FECHA-ALTA.
           MOVE WS-NOMBRE TO NOMBRE.
           MOVE WS-APELLIDO TO APELLIDO.
           MOVE WS-TELEFONO TO TELEFONO.
           MOVE WS-MAIL TO MAIL.
           MOVE 0 TO TARJETA.
           MOVE 0 TO CREDITO.
           MOVE 0 TO HIPOTECA.
           MOVE 1 TO ACTIVA.
           MOVE 0 TO SALDO.


      *    EXEC SQL
      *        INSERT INTO clientes
      *        VALUES(
      *>          :ID-CLIENTE,
      *>          :TIPO-DOC,
      *>          :DOCUMENT,
      *>          :FECHA-ALTA,
      *>          :NOMBRE,
      *>          :APELLIDO,
      *>          :DIRECCION,
      *>          :TELEFONO,
      *>          :MAIL,
      *>          :
      *       :CLIENT
      *        )
      *        END-EXEC
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 CLIENT
               MOVE 'S' TO SQL-TYPE(1)
               MOVE 0 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA

      *-----------------------------------------------------------------
      * COMMIT CHANGES
      *-----------------------------------------------------------------
      *    EXEC SQL
      *        COMMIT
      *    END-EXEC
           CALL 'OCSQLCMT' USING SQLCA END-CALL
           PERFORM SQLSTATE-CHECK.

       0230-END.




       0200-END.

       0300-FIN.
       DISPLAY "FIN".
       0300-END.


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


       END PROGRAM MODCLI.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  ACTIVA               NOT IN USE
      *  APELLIDO             NOT IN USE
      *  BUFFER                   IN USE CHAR(1024)
      *  CLIENT                   IN USE UNKNOWN
      *  CREDITO              NOT IN USE
      *  DB-VARS              NOT IN USE
      *  DB-VARS.ACTIVA       NOT IN USE
      *  DB-VARS.APELLIDO     NOT IN USE
      *  DB-VARS.BUFFER       NOT IN USE
      *  DB-VARS.CLIENT       NOT IN USE
      *  DB-VARS.CREDITO      NOT IN USE
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
      *  DIRECCION            NOT IN USE
      *  DOCUMENT             NOT IN USE
      *  FECHA-ALTA           NOT IN USE
      *  FECHA-CIERRE         NOT IN USE
      *  HIPOTECA             NOT IN USE
      *  ID-CLIENTE           NOT IN USE
      *  MAIL                 NOT IN USE
      *  NOMBRE               NOT IN USE
      *  SALDO                NOT IN USE
      *  SEARCH-APELLI        NOT IN USE
      *  ST-COUNT             NOT IN USE
      *  TARJETA              NOT IN USE
      *  TELEFONO             NOT IN USE
      *  TIPO-DOC             NOT IN USE
      *  WS-MAX-ID                IN USE THROUGH TEMP VAR SQL-VAR-0008 DECIMAL(3,0)
      **********************************************************************
