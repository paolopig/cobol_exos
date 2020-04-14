      ******************************************************************
      * Author: PAOLO PIGNOTTI
      * Date: 28/11/2019
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXOCHAINEVRAC.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  CHAINE.
           05 ELEM-CHAINE OCCURS 80 PIC X.
       77  LEN-ENREG PIC 99.
       77  NB PIC XXX.
       77  RUE PIC X(20).
       77  CP PIC X(5).
       77  VILLE PIC X(20).
       77  CMPL PIC X(20).
       77  I PIC 99 VALUE 1.
       77  INIZIO PIC 99 VALUE 0.
       77  FINE PIC 99.
       77  TROUVE PIC 9 VALUE 0.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INIT
           PERFORM TRAIT UNTIL I > 80
           PERFORM FIN
           STOP RUN.
       INIT.
           DISPLAY "Saisir la chaine de max 80 caractères: "
           ACCEPT CHAINE.
       TRAIT.
      *     DISPLAY ELEM-CHAINE(I)
           IF (ELEM-CHAINE(I) <> " " AND INIZIO = 0 )
               THEN
               MOVE I TO INIZIO
               DISPLAY INIZIO " Inizio"
           END-IF
           IF (ELEM-CHAINE(I) = " " AND INIZIO <> 0 )
               THEN
               MOVE I TO FINE
               DISPLAY FINE " Fine"

               IF TROUVE = 4
                    THEN
                    SUBTRACT INIZIO FROM FINE GIVING LEN-ENREG
                    MOVE CHAINE(INIZIO:LEN-ENREG) TO CMPL
                    MOVE 5 TO TROUVE
                    MOVE 0 TO INIZIO
                    MOVE 0 TO FINE
                 END-IF

                IF TROUVE = 3
                    THEN
                    SUBTRACT INIZIO FROM FINE GIVING LEN-ENREG
                    MOVE CHAINE(INIZIO:LEN-ENREG) TO VILLE
                    MOVE 4 TO TROUVE
                   MOVE 0 TO INIZIO FINE
                 END-IF

                IF TROUVE = 2
                    THEN
                    SUBTRACT INIZIO FROM FINE GIVING LEN-ENREG
                    MOVE CHAINE(INIZIO:LEN-ENREG) TO CP
                    MOVE 3 TO TROUVE
                   MOVE 0 TO INIZIO FINE
                 END-IF

               IF TROUVE = 1
                   THEN
                   SUBTRACT INIZIO FROM FINE GIVING LEN-ENREG
                   MOVE CHAINE(INIZIO:LEN-ENREG) TO RUE
                   MOVE 2 TO TROUVE
                   MOVE 0 TO INIZIO FINE
                END-IF

               IF TROUVE = 0
                   THEN
                   SUBTRACT INIZIO FROM FINE GIVING LEN-ENREG
                   MOVE CHAINE(INIZIO:LEN-ENREG) TO NB
                   MOVE 1 TO TROUVE
                   MOVE 0 TO INIZIO FINE
                END-IF

           END-IF

           ADD 1 TO I.
       FIN.
           DISPLAY NB
           DISPLAY RUE
           DISPLAY CP
           DISPLAY VILLE
           DISPLAY CMPL .

       END PROGRAM EXOCHAINEVRAC.
