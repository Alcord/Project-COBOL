      ******************************************************************
      * Author: Kevin Cabrera
      * Date: 14/02/2025
      * Purpose: Proyecto del curso de COBOL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. MAIN.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
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
               10 WS-TXTMN1 PIC X(30) VALUE "1. ABM Clientes".
               10 WS-TXTMN2 PIC X(30) VALUE "2. Cuentas Corrientes".
               10 WS-TXTMN3 PIC X(30) VALUE "3. Tarjetas de Credito".
               10 WS-TXTMN4 PIC X(30) VALUE "4. Hipotecas".
               10 WS-TXTMN5 PIC X(30) VALUE "5. Proceso Batch".
               10 WS-TXTMN6 PIC X(30) VALUE "6. Salir".

       LOCAL-STORAGE SECTION.
       01  WS-OPTION       PIC 9(01).

      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
           PERFORM 0100-INICIO THRU 0200-PROCEDIMIENTO.
           STOP RUN.

           0100-INICIO.
           *>DISPLAY "INICIO".
      *    MENU TITLE
           MOVE "MAIN MENU" TO WS-TXT-TITLE(11:9).
           MOVE ":Modulos disponibles:" TO WS-TXT-SUBTITLE.
           0100-END.

           0200-PROCEDIMIENTO.
           INITIALIZE WS-OPTION.
           PERFORM 0220-PRINT-MENU.
           DISPLAY "Ingrese una opcion: ".
           ACCEPT WS-OPTION.

           EVALUATE WS-OPTION
               WHEN 1
                   DISPLAY "Modulo ABM Clientes..."
                   CALL 'MODCLI001' USING WS-OPTION
                   PERFORM 0200-PROCEDIMIENTO
               WHEN 2
                   DISPLAY "Modulo Cuentas Corrientes..."
                   CALl "TARJCRE001"
                   PERFORM 0200-PROCEDIMIENTO
               WHEN 3
                   DISPLAY "Modulo Tarjetas de Credito..."
                   CALL "TARJCRE001"
                   PERFORM 0200-PROCEDIMIENTO
               WHEN 4
                   DISPLAY "Modulo Hipotecas..."
                   PERFORM 0200-PROCEDIMIENTO
               WHEN 5
                   DISPLAY "Modulo Proceso Batch..."
                   PERFORM 0200-PROCEDIMIENTO
               WHEN 6
                   DISPLAY "Saliendo del Sistema... Adios..."
               WHEN OTHER
                   DISPLAY "Opción invalida, ingrese del 1 al 6".
                   STOP RUN.

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
           DISPLAY WS-LEFT-WALL WS-TXTMN6 WS-RGTH-WALL.
           DISPLAY "+" WS-LINE "+".
           0220-END.

           0200-END.

           0300-FIN.
           DISPLAY "FIN".
           0300-END.

       END PROGRAM MAIN.
