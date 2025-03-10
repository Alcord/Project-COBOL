       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIEMEN001.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE-DETAIL ASSIGN TO "CONSOLIDADO.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE-DETAIL.
       01  RPT-DETALLE-CLI   PIC X(200).

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
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 5.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 5 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 5 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 5 TIMES.
           05 SQL-PREC   PIC X OCCURS 5 TIMES.
      **********************************************************************
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 455.
           05 SQL-STMT   PIC X(455) VALUE 'SELECT C.ID_CLIENTE,C.NOMBRE_
      -    'CLIENTE,COALESCE(CT.SALDO_ACTUAL,0) SALDO_CTACTE,COALESCE(H.
      -    'SALDO_ACTUAL,0) SALDO_HIPOTECA,COALESCE(T.LIQUIDACION_MES,0)
      -    'SALDO_TARJETA FROM CLIENTES C LEFT JOIN (SELECT MAX(ID_CTACT
      -    'ES) ID_CTACTES,MAX(ID_CLIENTE) ID_CLIENTE,MAX(SALDO_ACTUAL) 
      -    'SALDO_ACTUAL FROM CTACTES GROUP BY ID_CLIENTE) CT ON C.ID_CL
      -    'IENTE = CT.ID_CLIENTE LEFT JOIN HIPOTECAS H ON C.ID_CLIENTE 
      -    '= H.ID_CLIENTE LEFT JOIN TARJETAS T ON C.ID_CLIENTE = T.ID_C
      -    'LIENTE'.
           05 SQL-CNAME  PIC X(18) VALUE 'CURSOR_CONSOLIDADO'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 60.
           05 SQL-STMT   PIC X(60) VALUE 'SELECT DATE_FORMAT(LAST_DAY(CU
      -    'RDATE()),''%d/%m/%Y'') FROM DUAL'.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 72.
           05 SQL-STMT   PIC X(72) VALUE 'SELECT CONCAT(UPPER(MONTHNAME(
      -    'CURDATE())),'' '',YEAR(CURDATE())) FROM DUAL'.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 152.
           05 SQL-STMT   PIC X(152) VALUE 'SELECT SUM(CASE WHEN CTA_ACTI
      -    'VA = 1 THEN 1 ELSE 0 END) TOTAL_ACTIVAS,SUM(CASE WHEN CTA_AC
      -    'TIVA = 0 THEN 1 ELSE 0 END) TOTAL_INACTIVAS FROM BANCO.CLIEN
      -    'TES'.
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
           05 SQL-VAR-0004  PIC S9(11) COMP-3.
           05 SQL-VAR-0005  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0006  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0007  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0009  PIC S9(11) COMP-3.
           05 SQL-VAR-0010  PIC S9(11) COMP-3.
      *******       END OF PRECOMPILER-GENERATED VARIABLES           *******
      **********************************************************************
       COPY "BD001".


      *    EXEC SQL
      *        BEGIN DECLARE SECTION
      *    END-EXEC
       01  WT-FECHA-CORTE              PIC X(10).
       01  WT-PERIODO                  PIC X(15).

       01  DB-VARS.
           05  BUFFER                  PIC X(1024).
           05  ST-COUNT                PIC 9(6).
           05  CTACTE.
               10  WT-MONTO                   PIC S9(8)V99.
               10  WS-EXISTE-CLIENTE          PIC X(01).
               10  WS-NEWID-CTACTE            PIC 999.

       01  DB-CONSOLIDADO-DETALLE.
           05  WC-ID-CLIENTE                  PIC 9(10).
           05  WC-NOMBRE                      PIC X(25).
           05  WC-SALDO-CTACTE                PIC 9(12)V99.
           05  WC-SALDO-HIPOTECA              PIC 9(12)V99.
           05  WC-SALDO-TARJETA               PIC 9(12)V99.
           05  WC-SALDO-TOTAL                 PIC 9(12)V99.

       01  DB-CONSOLIDADO-CLIENTE.
           05  WC-CLIENTE-ACTIVO              PIC 9(10).
           05  WC-CLIENTE-INACTIVO            PIC 9(10).

      *    EXEC SQL
      *        END DECLARE SECTION
      *    END-EXEC

       01  FECHA.
           05  WS-DIA             PIC 99.
           05  WS-MES             PIC 99.
           05  WS-ANIO            PIC 9999.

       01  WS-FECHA-FORMATO   PIC XX/XX/XXXX.


       01  FILLER-0            PIC X(16) VALUE 'FECHA DE CORTE: '.
       01  FILLER-1            PIC X(35) VALUE
                                 '                        GENERADO: '.

       01  FILLER-2            PIC X(16) VALUE 'PERIODO       : '.
       01  FILLER-3            PIC X(30) VALUE
                                 '                   USUARIO : '.

       01  WS-USUARIO          PIC X(50)  VALUE 'ADM-1'.
      *> Cabecera Reporte consolidado

       01  WS-HEADER-00             PIC X(58) VALUE
           "----------------------------------------------------------".
       01  WS-HEADER-01             PIC X(26) VALUE
           "--------------------------".
       01  WS-HEADER-02             PIC X(58) VALUE
           "*******************      INFORME FINANCIERO CONSOLIDADO   ".
       01  WS-HEADER-03             PIC X(26) VALUE
           "  ************************".
       01  WS-HEADER-04             PIC X(58) VALUE
           "ID CLIENTE| NOMBRES                   |SALDO CTACTE  |SALD".
       01  WS-HEADER-05           PIC X(26) VALUE
           "O HIPOTECA|SALDO TARJETA |".

      *> Fin Cabecera Reporte consolidado


       01  WS-CONSOLIDADO-DETAIL.
           03 REG-DETAIL OCCURS 100 TIMES.
              05  RPT-ID-CLIENTE                  PIC 9(10).
              05  RPT-NOMBRE                      PIC X(25).
              05  RPT-SALDO-CTACTE                PIC ZZZZZZ9.99-.
              05  RPT-SALDO-HIPOTECA              PIC ZZZZZZ9.99-.
              05  RPT-SALDO-TARJETA               PIC ZZZZZZ9.99-.
              05  RPT-SALDO-TOTAL                 PIC ZZZZZZ9.99-.

       01  WS-INDICADORES-DETAIL.
           05 WS-AC-SALDO-CTACTE                  PIC S9(12)V99.
           05 WS-AC-SALDO-HIPOTECA                PIC S9(12)V99.
           05 WS-AC-SALDO-TARJETA                 PIC S9(12)V99.
           05 WS-AC-NRO-HIPOTECAS                 PIC 9(03).
           05 WS-AC-NRO-CTACTE                    PIC 9(03).
           05 WS-AC-NRO-TARJETA                   PIC 9(03).

       01  WS-INDICADORES-IMPRESION.
           05 WS-IM-SALDO-CTACTE                  PIC ZZZZZZ9.99-.
           05 WS-IM-SALDO-HIPOTECA                PIC ZZZZZZ9.99-.
           05 WS-IM-SALDO-TARJETA                 PIC ZZZZZZ9.99-.
           05 WS-IM-NRO-HIPOTECAS                 PIC ZZZZZZ9.
           05 WS-IM-CLIENTE-ACTIVO                PIC ZZZZZ.
           05 WS-IM-CLIENTE-INACTIVO              PIC ZZZZZ.

       01  WS-OPCION                   PIC 9.
       01  WS-CONT                     PIC 999.
       01  WS-CONTX                    PIC 999.
       01  WS-FECHA                    PIC X(10).

       01  CONT-INDICE.
           05  WS-CT-SAL                   PIC ZZZZ9.
           05  WS-CT-HIP                   PIC ZZZZ9.
           05  WS-CT-TAR                   PIC ZZZZ9.

       LINKAGE SECTION.
       01 LK-USER-ID PIC 9(1).  *> Recibir� un ID de usuario

       PROCEDURE DIVISION USING LK-USER-ID.
       MAIN-PROGRAM.
           PERFORM 0100-INICIO.
           PERFORM 100-MENU.

       100-MENU.
           DISPLAY "===================================="
           DISPLAY "     REPORTE CIERRE MENSUAL         "
           DISPLAY "===================================="
           DISPLAY "1 - Consolidacion de informaci�n e Indicadores".
           DISPLAY "2 - Salir"
           DISPLAY "Seleccione una opci�n: "
           ACCEPT WS-OPCION

           EVALUATE WS-OPCION
               WHEN 1
                   PERFORM 100-GENERAR-CONSOLIDADO
               WHEN 2
                   PERFORM 0300-FIN
               WHEN OTHER
                   DISPLAY "Opci�n inv�lida, intente nuevamente."
           END-EVALUATE.


       320-EXTRAE-CONSOLIDADO-CURSOR.
           MOVE ZEROES TO WS-CONT WS-CONTX.
           INITIALIZE DB-CONSOLIDADO-DETALLE WS-INDICADORES-DETAIL.
           MOVE ZEROES TO WC-SALDO-CTACTE
                          WC-SALDO-HIPOTECA
                          WC-SALDO-TARJETA
                          WC-ID-CLIENTE.
           MOVE SPACES TO WC-NOMBRE.
      *    EXEC SQL
      *        DECLARE CURSOR_CONSOLIDADO CURSOR FOR
      *        SELECT C.ID_CLIENTE, C.NOMBRE_CLIENTE,
      *         COALESCE(CT.SALDO_ACTUAL, 0) SALDO_CTACTE,
      *         COALESCE(H.SALDO_ACTUAL, 0) SALDO_HIPOTECA,
      *         COALESCE(T.LIQUIDACION_MES, 0)SALDO_TARJETA
      *         FROM CLIENTES C
      *        LEFT JOIN (SELECT MAX(ID_CTACTES) ID_CTACTES,
      *                          MAX(ID_CLIENTE) ID_CLIENTE,
      *                          MAX(SALDO_ACTUAL) SALDO_ACTUAL
      *                     FROM CTACTES
      *                   GROUP BY ID_CLIENTE) CT
      *                             ON C.ID_CLIENTE = CT.ID_CLIENTE
      *       LEFT JOIN HIPOTECAS H ON C.ID_CLIENTE = H.ID_CLIENTE
      *       LEFT JOIN TARJETAS T ON C.ID_CLIENTE = T.ID_CLIENTE

      *    END-EXEC
      *    EXEC SQL
      *        OPEN CURSOR_CONSOLIDADO
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               MOVE 0 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
           END-IF
           CALL 'OCSQLOCU' USING SQL-STMT-0
                               SQLCA
           END-CALL

           PERFORM UNTIL SQLCODE = 100
      *        EXEC SQL
      *          FETCH CURSOR_CONSOLIDADO
      *          INTO
      *            :WC-ID-CLIENTE,
      *            :WC-NOMBRE,
      *            :WC-SALDO-CTACTE,
      *            :WC-SALDO-HIPOTECA,
      *            :WC-SALDO-TARJETA
      *        END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0004
           MOVE '3' TO SQL-TYPE(1)
           MOVE 6 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             WC-NOMBRE
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 25 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             SQL-VAR-0005
           MOVE '3' TO SQL-TYPE(3)
           MOVE 8 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0006
           MOVE '3' TO SQL-TYPE(4)
           MOVE 8 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             SQL-VAR-0007
           MOVE '3' TO SQL-TYPE(5)
           MOVE 8 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
           MOVE 5 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-0
                               SQLCA
           MOVE SQL-VAR-0004 TO WC-ID-CLIENTE
           MOVE SQL-VAR-0005 TO WC-SALDO-CTACTE
           MOVE SQL-VAR-0006 TO WC-SALDO-HIPOTECA
           MOVE SQL-VAR-0007 TO WC-SALDO-TARJETA

            IF SQLCODE NOT = 100 THEN
          *> Formatear l�nea del reporte
               ADD 1 TO WS-CONT
               MOVE WC-ID-CLIENTE     TO RPT-ID-CLIENTE    (WS-CONT)
               MOVE WC-NOMBRE         TO RPT-NOMBRE        (WS-CONT)
               MOVE WC-SALDO-CTACTE   TO RPT-SALDO-CTACTE  (WS-CONT)
               MOVE WC-SALDO-HIPOTECA TO RPT-SALDO-HIPOTECA(WS-CONT)
               MOVE WC-SALDO-TARJETA  TO RPT-SALDO-TARJETA (WS-CONT)

               COMPUTE WC-SALDO-TOTAL = WC-SALDO-CTACTE +
                                        WC-SALDO-HIPOTECA +
                                        WC-SALDO-TARJETA
               MOVE WC-SALDO-TOTAL   TO RPT-SALDO-TOTAL    (WS-CONT)


          *> Acumulaci�n de totales

                 ADD WC-SALDO-CTACTE   TO WS-AC-SALDO-CTACTE
                 ADD WC-SALDO-HIPOTECA TO WS-AC-SALDO-HIPOTECA
                 ADD WC-SALDO-TARJETA  TO WS-AC-SALDO-TARJETA

                 IF WC-SALDO-HIPOTECA > 0 THEN
                    ADD 1 TO WS-AC-NRO-HIPOTECAS
                 END-IF

                 IF WC-SALDO-CTACTE > 0 THEN
                    ADD 1 TO WS-AC-NRO-CTACTE
                 END-IF

                 IF WC-SALDO-TARJETA > 0 THEN
                    ADD 1 TO WS-AC-NRO-TARJETA
                 END-IF


           ELSE
               DISPLAY ' NO TIENE MAS FILAS LA TABLA DE DB'
           END-IF
           END-PERFORM
      *    EXEC SQL CLOSE CURSOR_CONSOLIDADO END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-0
                               SQLCA
                                                     .

       000-IMPRIME-DETALLE.
           MOVE ZEROES TO  WS-CONTX.
           INITIALIZE CONT-INDICE.
           PERFORM VARYING WS-CONTX FROM 1 BY 1
                                           UNTIL WS-CONTX > WS-CONT
              STRING RPT-ID-CLIENTE  (WS-CONTX)     "|"
                     RPT-NOMBRE      (WS-CONTX)     "  | "
                     RPT-SALDO-CTACTE(WS-CONTX)     "  | "
                     RPT-SALDO-HIPOTECA(WS-CONTX)   "  | "
                     RPT-SALDO-TARJETA (WS-CONTX)   "  |"
                  INTO RPT-DETALLE-CLI

              DISPLAY RPT-DETALLE-CLI(1:100)
              WRITE RPT-DETALLE-CLI
           END-PERFORM
             MOVE SPACES TO RPT-DETALLE-CLI
             STRING WS-HEADER-00 WS-HEADER-01 INTO RPT-DETALLE-CLI
             WRITE RPT-DETALLE-CLI
             DISPLAY RPT-DETALLE-CLI

               MOVE WS-AC-SALDO-CTACTE    TO WS-IM-SALDO-CTACTE
               MOVE WS-AC-SALDO-HIPOTECA  TO WS-IM-SALDO-HIPOTECA
               MOVE WS-AC-SALDO-TARJETA   TO WS-IM-SALDO-TARJETA
               MOVE WS-AC-NRO-HIPOTECAS   TO WS-IM-NRO-HIPOTECAS


           DISPLAY "***************************************************"
           DISPLAY "   INDICADORES      |           VALOR"
           DISPLAY "***************************************************"
           DISPLAY " CREDITO OTORGADO   |    " WS-IM-SALDO-HIPOTECA
           DISPLAY " CUENTA CORRIENTE   |    " WS-IM-SALDO-CTACTE
           DISPLAY " SALDO TOTAL TARJETA|    " WS-IM-SALDO-TARJETA
           DISPLAY " CLIENTE ACTIVO     |    " WS-IM-CLIENTE-ACTIVO
           DISPLAY " CLIENTE INACTIVO   |    " WS-IM-CLIENTE-INACTIVO
           DISPLAY "***************************************************"

            WRITE RPT-DETALLE-CLI
            FROM "***************************************"
               WRITE RPT-DETALLE-CLI
            FROM "   INDICADORES      | NRO |   VALOR"
            WRITE RPT-DETALLE-CLI
             FROM "***************************************"

            MOVE WS-IM-NRO-HIPOTECAS TO WS-CT-HIP
            MOVE SPACES TO RPT-DETALLE-CLI
            STRING" CREDITO OTORGADO   |"WS-CT-HIP"|  "
                                                    WS-IM-SALDO-HIPOTECA
                INTO RPT-DETALLE-CLI
               WRITE RPT-DETALLE-CLI

            MOVE WS-AC-NRO-CTACTE TO WS-CT-SAL
               MOVE SPACES TO RPT-DETALLE-CLI
             STRING" CUENTA CORRIENTE   |"WS-CT-SAL"|  "
                                                    WS-IM-SALDO-CTACTE
                 INTO RPT-DETALLE-CLI
               WRITE RPT-DETALLE-CLI

            MOVE WS-AC-NRO-TARJETA TO WS-CT-TAR
               MOVE SPACES TO RPT-DETALLE-CLI
             STRING  " SALDO TOTAL TARJETA|"WS-CT-TAR"|  "
                                                    WS-IM-SALDO-TARJETA
                 INTO RPT-DETALLE-CLI
                WRITE RPT-DETALLE-CLI

             MOVE SPACES TO RPT-DETALLE-CLI
             STRING  " CLIENTE ACTIVO     |"WS-IM-CLIENTE-ACTIVO"|  "
                                                    "      0.00"
                 INTO RPT-DETALLE-CLI
                WRITE RPT-DETALLE-CLI

             MOVE SPACES TO RPT-DETALLE-CLI
             STRING  " CLIENTE INACTIVO   |"WS-IM-CLIENTE-INACTIVO"|  "
                                                    "      0.00"
                 INTO RPT-DETALLE-CLI
                WRITE RPT-DETALLE-CLI
            WRITE RPT-DETALLE-CLI

            FROM "***************************************"
           MOVE SPACES TO RPT-DETALLE-CLI.

       000-CIERRE-ARCHIVO.
           CLOSE RPT-FILE-DETAIL.

       000-IMPRIME-CABECERA.

           DISPLAY"                                                  "
           STRING WS-HEADER-00 WS-HEADER-01 INTO RPT-DETALLE-CLI
           WRITE RPT-DETALLE-CLI
           DISPLAY RPT-DETALLE-CLI
           STRING WS-HEADER-02 WS-HEADER-03 INTO RPT-DETALLE-CLI
           WRITE RPT-DETALLE-CLI
           DISPLAY RPT-DETALLE-CLI
           STRING WS-HEADER-00 WS-HEADER-01 INTO RPT-DETALLE-CLI
           WRITE RPT-DETALLE-CLI
           DISPLAY RPT-DETALLE-CLI

           MOVE SPACES TO RPT-DETALLE-CLI
           STRING FILLER-0 WT-FECHA-CORTE FILLER-1 WS-FECHA-FORMATO
              INTO RPT-DETALLE-CLI
           WRITE RPT-DETALLE-CLI
           DISPLAY RPT-DETALLE-CLI

           MOVE SPACES TO RPT-DETALLE-CLI
           STRING FILLER-2 WT-PERIODO FILLER-3 WS-USUARIO
              INTO RPT-DETALLE-CLI
           WRITE RPT-DETALLE-CLI
           DISPLAY RPT-DETALLE-CLI

           STRING WS-HEADER-00 WS-HEADER-01 INTO RPT-DETALLE-CLI
           WRITE RPT-DETALLE-CLI
           DISPLAY RPT-DETALLE-CLI

           MOVE SPACES TO RPT-DETALLE-CLI
           STRING WS-HEADER-04 WS-HEADER-05 INTO RPT-DETALLE-CLI
           WRITE RPT-DETALLE-CLI
           DISPLAY RPT-DETALLE-CLI
           STRING WS-HEADER-00 WS-HEADER-01 INTO RPT-DETALLE-CLI
           WRITE RPT-DETALLE-CLI
           DISPLAY RPT-DETALLE-CLI
           MOVE SPACES TO RPT-DETALLE-CLI.

       310-APERTURA-ARCHIVO.

           OPEN OUTPUT RPT-FILE-DETAIL
           ACCEPT WS-FECHA FROM DATE
           STRING "20" WS-FECHA(1:2) INTO WS-ANIO
           MOVE WS-FECHA(3:2) TO WS-MES
           MOVE WS-FECHA(5:2) TO WS-DIA
           STRING WS-DIA "/" WS-MES "/" WS-ANIO INTO WS-FECHA-FORMATO
           MOVE SPACES TO WT-FECHA-CORTE WT-PERIODO

      *    EXEC SQL
      *       SELECT DATE_FORMAT(LAST_DAY(CURDATE()), '%d/%m/%Y')
      *         INTO :WT-FECHA-CORTE
      *        FROM DUAL
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WT-FECHA-CORTE
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 10 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA
                   .
      *    EXEC SQL
      *          SELECT CONCAT(UPPER(MONTHNAME(CURDATE())), ' ',
      *                                     YEAR(CURDATE()))
      *            INTO :WT-PERIODO
      *          FROM DUAL
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WT-PERIODO
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 15 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
                   .
       330-EXTRAE-TOTAL-CLIENTE.
           MOVE ZEROES TO WS-IM-CLIENTE-ACTIVO WS-IM-CLIENTE-INACTIVO
      *    EXEC SQL
      *      SELECT
      *         SUM(CASE WHEN CTA_ACTIVA = 1 THEN 1 ELSE 0 END)
      *                                             TOTAL_ACTIVAS,
      *         SUM(CASE WHEN CTA_ACTIVA = 0 THEN 1 ELSE 0 END)
      *                                             TOTAL_INACTIVAS
      *      INTO :WC-CLIENTE-ACTIVO,
      *           :WC-CLIENTE-INACTIVO
      *     FROM BANCO.CLIENTES
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-3 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0009
               MOVE '3' TO SQL-TYPE(1)
               MOVE 6 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0010
               MOVE '3' TO SQL-TYPE(2)
               MOVE 6 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-3
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-3
                               SQLCA
           MOVE SQL-VAR-0009 TO WC-CLIENTE-ACTIVO
           MOVE SQL-VAR-0010 TO WC-CLIENTE-INACTIVO
                   .

           MOVE WC-CLIENTE-ACTIVO    TO WS-IM-CLIENTE-ACTIVO.
           MOVE WC-CLIENTE-INACTIVO  TO WS-IM-CLIENTE-INACTIVO.


       100-GENERAR-CONSOLIDADO.
           PERFORM 310-APERTURA-ARCHIVO.
           PERFORM 330-EXTRAE-TOTAL-CLIENTE.
           PERFORM 320-EXTRAE-CONSOLIDADO-CURSOR.
           PERFORM 000-IMPRIME-CABECERA.
           PERFORM 000-IMPRIME-DETALLE.
           PERFORM 000-CIERRE-ARCHIVO.
           PERFORM 100-MENU.

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

       0300-FIN.
           DISPLAY "Regresando a Menu Principal..."
           EXIT PROGRAM.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  BUFFER                   IN USE CHAR(1024)
      *  CTACTE               NOT IN USE
      *  CURSOR_CONSOLIDADO       IN USE CURSOR
      *  DB-CONSOLIDADO-CLIENTE NOT IN USE
      *  DB-CONSOLIDADO-CLIENTE.WC-CLIENTE-ACTIVO NOT IN USE
      *  DB-CONSOLIDADO-CLIENTE.WC-CLIENTE-INACTIVO NOT IN USE
      *  DB-CONSOLIDADO-DETALLE NOT IN USE
      *  DB-CONSOLIDADO-DETALLE.WC-ID-CLIENTE NOT IN USE
      *  DB-CONSOLIDADO-DETALLE.WC-NOMBRE NOT IN USE
      *  DB-CONSOLIDADO-DETALLE.WC-SALDO-CTACTE NOT IN USE
      *  DB-CONSOLIDADO-DETALLE.WC-SALDO-HIPOTECA NOT IN USE
      *  DB-CONSOLIDADO-DETALLE.WC-SALDO-TARJETA NOT IN USE
      *  DB-CONSOLIDADO-DETALLE.WC-SALDO-TOTAL NOT IN USE
      *  DB-VARS              NOT IN USE
      *  DB-VARS.BUFFER       NOT IN USE
      *  DB-VARS.CTACTE       NOT IN USE
      *  DB-VARS.ST-COUNT     NOT IN USE
      *  DB-VARS.WS-EXISTE-CLIENTE NOT IN USE
      *  DB-VARS.WS-NEWID-CTACTE NOT IN USE
      *  DB-VARS.WT-MONTO     NOT IN USE
      *  ST-COUNT             NOT IN USE
      *  WC-CLIENTE-ACTIVO        IN USE THROUGH TEMP VAR SQL-VAR-0009 DECIMAL(11,0)
      *  WC-CLIENTE-INACTIVO      IN USE THROUGH TEMP VAR SQL-VAR-0010 DECIMAL(11,0)
      *  WC-ID-CLIENTE            IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(11,0)
      *  WC-NOMBRE                IN USE CHAR(25)
      *  WC-SALDO-CTACTE          IN USE THROUGH TEMP VAR SQL-VAR-0005 DECIMAL(15,2)
      *  WC-SALDO-HIPOTECA        IN USE THROUGH TEMP VAR SQL-VAR-0006 DECIMAL(15,2)
      *  WC-SALDO-TARJETA         IN USE THROUGH TEMP VAR SQL-VAR-0007 DECIMAL(15,2)
      *  WC-SALDO-TOTAL       NOT IN USE
      *  WS-EXISTE-CLIENTE    NOT IN USE
      *  WS-NEWID-CTACTE      NOT IN USE
      *  WT-FECHA-CORTE           IN USE CHAR(10)
      *  WT-MONTO             NOT IN USE
      *  WT-PERIODO               IN USE CHAR(15)
      **********************************************************************
