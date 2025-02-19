      ******************************************************************
      * Author: Kevin Cabrera
      * Date: 14/02/2025
      * Purpose: Validar cedula
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. VALCED.
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

       LOCAL-STORAGE SECTION.
       01  WS-CEDULA              PIC X(11).
       01  WS-PAS                 PIC X(10).
       01  WS-PROVINCIA           PIC 9(2).
       01  WS-TERCER-DIGITO       PIC 9.
       01  WS-DIGITO-VERIFICADOR  PIC 9.
       01  WS-SUMA                PIC 9(3) VALUE 0.
       01  WS-COEFICIENTES.
           05 WS-COEFICIENTE  OCCURS 9 TIMES PIC 9 VALUE 0.
       01  WS-RESIDUO             PIC 9.
       01  I                      PIC 9 VALUE 1.
       01  WS-DIGITO-TEMP         PIC 9.
       01  WS-COEFICIENTE-TEMP    PIC 9.

       01  WS-VALIDACION          PIC 99.


       LINKAGE SECTION.
       01  LK-CEDULAVALIDA      PIC 9(1) VALUE 0.
       01  LK-STRING            PIC X(15).
       01  LK-TIPO              PIC x(3).
      *-----------------------
       PROCEDURE DIVISION USING LK-CEDULAVALIDA LK-STRING LK-TIPO.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
           PERFORM 0100-INICIO THRU 0200-PROCESO.
           EXIT PROGRAM.

       0100-INICIO.
           DISPLAY "Documento: " LK-STRING
           DISPLAY "Tipo: " LK-TIPO.

           MOVE 2 TO WS-COEFICIENTE(1).
           MOVE 1 TO WS-COEFICIENTE(2).
           MOVE 2 TO WS-COEFICIENTE(3).
           MOVE 1 TO WS-COEFICIENTE(4).
           MOVE 2 TO WS-COEFICIENTE(5).
           MOVE 1 TO WS-COEFICIENTE(6).
           MOVE 2 TO WS-COEFICIENTE(7).
           MOVE 1 TO WS-COEFICIENTE(8).
           MOVE 2 TO WS-COEFICIENTE(9).
       0100-END.

       0200-PROCESO.
           INITIALIZE WS-SUMA
           EVALUATE LK-TIPO

            WHEN "DNI"
            DISPLAY "Tipo de documento: DNI"

            MOVE FUNCTION TRIM(LK-STRING) TO WS-CEDULA
            MOVE FUNCTION LENGTH(WS-CEDULA) TO WS-VALIDACION

            *> Validaciones iniciales de la cédula
            IF WS-VALIDACION NOT = "10"
                DISPLAY "Error: La cédula debe tener  10 caracteres."
                EXIT PROGRAM
            ELSE IF NOT WS-CEDULA NUMERIC
                DISPLAY "Error:La cédula debe contener solo números."
                EXIT PROGRAM
            END-IF

            *> Extraer componentes de la cédula
            MOVE WS-CEDULA(1:2) TO WS-PROVINCIA
            MOVE WS-CEDULA(3:1) TO WS-TERCER-DIGITO
            MOVE WS-CEDULA(10:1) TO WS-DIGITO-VERIFICADOR

            *> Validaciones de provincia y tercer dígito
            IF WS-PROVINCIA < 1 OR WS-PROVINCIA > 24
                DISPLAY "Código de provincia inválido (debe 01-24)."
                EXIT PROGRAM
            ELSE IF WS-TERCER-DIGITO < 0 OR WS-TERCER-DIGITO > 6
                DISPLAY "El tercer dígito debe estar entre 0 y 6."
                EXIT PROGRAM
            END-IF

            *> Cálculo del dígito verificador
            MOVE 0 TO WS-SUMA
            PERFORM VARYING I FROM 1 BY 1 UNTIL I > 9
              MOVE FUNCTION NUMVAL(WS-CEDULA(I:1)) TO WS-DIGITO-TEMP
             COMPUTE WS-DIGITO-TEMP = WS-DIGITO-TEMP * WS-COEFICIENTE(I)
              IF WS-DIGITO-TEMP > 9 THEN
                    COMPUTE WS-DIGITO-TEMP = WS-DIGITO-TEMP - 9
              END-IF
              COMPUTE WS-SUMA = WS-SUMA + WS-DIGITO-TEMP
            END-PERFORM

            *> Calcular residuo de la suma con 10
            DIVIDE WS-SUMA BY 10 GIVING WS-RESIDUO REMAINDER WS-RESIDUO

            IF WS-RESIDUO NOT = 0 THEN
                COMPUTE WS-RESIDUO = 10 - WS-RESIDUO
            END-IF

            *> Validación del dígito verificador
            IF WS-RESIDUO = WS-DIGITO-VERIFICADOR
                    DISPLAY "Cédula ecuatoriana válida."
                    MOVE "1" TO LK-CEDULAVALIDA
            ELSE
                    DISPLAY "Error en el dígito verificador."
                    EXIT PROGRAM
            END-IF



           WHEN "PAS"
               DISPLAY "Tipo de documento: PAS"

           MOVE FUNCTION TRIM(LK-STRING) TO WS-CEDULA
           MOVE FUNCTION LENGTH(WS-CEDULA) TO WS-VALIDACION

           *> Verificar que el pasaporte tenga entre 8 y 9 caracteres alfanuméricos
           IF WS-VALIDACION < "8" OR WS-VALIDACION > "9" THEN
               DISPLAY "El pasaporte debe tener entre 8 y 9 caracteres."
               EXIT PROGRAM
           END-IF

           *> Verificar que comience con 1 o 2 letras y siga con 7 números
           IF WS-CEDULA(1:1) ALPHABETIC AND WS-CEDULA(2:7) NUMERIC THEN
               DISPLAY "Pasaporte válido."
               MOVE "1" TO LK-CEDULAVALIDA
           ELSE IF WS-CEDULA(1:2) ALPHABETIC
               AND WS-CEDULA(3:7) NUMERIC THEN

               DISPLAY "Pasaporte válido."
               MOVE "1" TO LK-CEDULAVALIDA
           ELSE
               DISPLAY "Debe tener 1 o 2 letras seguidas de 7 números."
               EXIT PROGRAM
           END-IF


           WHEN OTHER
               DISPLAY "Error: Tipo de documento inválido."
               EXIT PARAGRAPH
           END-EVALUATE.

       0200-END.
      ** add other procedures here
       END PROGRAM VALCED.
