      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALEMAIL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  WS-AT-SIGN  PIC 99 VALUE 0.
       01  WS-DOT      PIC 99 VALUE 0.

       LINKAGE SECTION.
       01  LK-EMAIL    PIC X(40).
       01  LK-VAL-EMAIL  PIC 9.

       PROCEDURE DIVISION USING LK-EMAIL LK-VAL-EMAIL.
       MAIN-PROCEDURE.
       INSPECT LK-EMAIL TALLYING WS-AT-SIGN FOR ALL '@'.
       INSPECT LK-EMAIL TALLYING WS-DOT FOR ALL '.'.

       IF WS-AT-SIGN = 1 AND WS-DOT >= 1
           MOVE 1 TO LK-VAL-EMAIL
       END-IF.

       END PROGRAM VALEMAIL.
