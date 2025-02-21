      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MODHIP.
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
           05 SQLVERSN PIC 99 VALUE 02.
           05 SQLCODE  PIC S9(9) COMP-5.
           05 SQLERRM.
               49 SQLERRML PIC S9(4) COMP-5.
               49 SQLERRMC PIC X(486).
           05 SQLERRD OCCURS 6 TIMES PIC S9(9) COMP-5.
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 2.
           05 SQL-COUNT  PIC S9(9) COMP-5.
           05 SQL-ADDR   POINTER OCCURS 2 TIMES.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 2 TIMES.
           05 SQL-TYPE   PIC X OCCURS 2 TIMES.
           05 SQL-PREC   PIC X OCCURS 2 TIMES.
      **********************************************************************

       COPY "BD001".

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

       01  DB-EXTRA-INFO.
           05   DB-N-CTACTE                PIC 9(12).
           05   DB-COUNT-CTE               PIC 99.
           05   DB-AUX-DOC                 PIC X(12).

      *EXEC SQL
      *    END DECLARE SECTION
      *END-EXEC


       01 WS-MAINMENU.
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

       01 WS-MENU-PAGO.
           05 WS-PG-OPTION1 PIC X(30) VALUE "1. Pagar 1 o más cuotas".
           05 WS-PG-OPTION2 PIC X(30) VALUE "2. Pagar totalidad".
           05 WS-PG-OPTION3 PIC X(30) VALUE "3. Salir".

       01 WS-OPTION PIC 9 VALUE 0.






       PROCEDURE DIVISION.
       PERFORM 0100-INICIO THRU 0210-HIPOTECAS.
       STOP RUN.

       0100-INICIO.

       0100-FIN.


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

       WHEN 3 DISPLAY "Realizar pago"
           PERFORM 0250-REALIZA-PAGO
           PERFORM 0210-HIPOTECAS

       WHEN 4
           PERFORM 0300-FIN

       WHEN OTHER
           DISPLAY "(" WS-OPTION ") - " "Opcion invalida."
           PERFORM 0210-HIPOTECAS

       DISPLAY "Salida incorrecta".


       0230-NEW-HIPOTECA.
       0230-END.


       0240-CONSULT-DEUDA.
       0240-END.

       0250-REALIZA-PAGO.
           DISPLAY "realizando pago ............".
       0250-END.

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

       END PROGRAM MODHIP.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCobol Version 2 (2021.05.29) Build May 29 2021

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  ACTIVA               NOT IN USE
      *  APELLIDO             NOT IN USE
      *  BUFFER               NOT IN USE
      *  CLIENT               NOT IN USE
      *  CREDITO              NOT IN USE
      *  DB-AUX-DOC           NOT IN USE
      *  DB-COUNT-CTE         NOT IN USE
      *  DB-DOCUMENT          NOT IN USE
      *  DB-EXTRA-INFO        NOT IN USE
      *  DB-EXTRA-INFO.DB-AUX-DOC NOT IN USE
      *  DB-EXTRA-INFO.DB-COUNT-CTE NOT IN USE
      *  DB-EXTRA-INFO.DB-N-CTACTE NOT IN USE
      *  DB-N-CTACTE          NOT IN USE
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
      *  WS-MAX-ID            NOT IN USE
      **********************************************************************
