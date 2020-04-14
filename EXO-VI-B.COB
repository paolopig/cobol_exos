      ******************************************************************
      * Author: PAOLO PIGNOTTI
      * Date: JOUR DATE HERE
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXO-VI-B.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NOMFIC
               ASSIGN TO "C:\Users\Cobol\Noms.txt".
           SELECT PRENOMFIC
               ASSIGN TO "C:\Users\Cobol\Prenoms.txt".
           SELECT NOMPRENOM
               ASSIGN TO "C:\Users\Cobol\NomsPrenoms.txt".
      *
       DATA DIVISION.
       FILE SECTION.
       FD  NOMFIC.
       01  ENREG-NOM.
           05 NOCLI PIC 9(2).
           05 NOM PIC X(5).

       FD  PRENOMFIC.
       01  ENREG-PNM.
           05 NOCLI PIC 9(2).
           05 PNM PIC X(5).

       FD  NOMPRENOM.
       01  ENREG-NOMPNM.
           05 NOCLI PIC 9(2).
           05 NOM PIC X(5).
           05 PNM PIC X(5).

       WORKING-STORAGE SECTION.
       77  EOFNOM  PIC 9 VALUE 0.
       77  EOFPNM  PIC 9 VALUE 0.
       77  EOFALL  PIC 9 VALUE 0.
       77  CPTNOM PIC 99.
       77  CPTPNM PIC 99.
       77  CPTNOMPNM PIC 99.
       77  CPTNOMSANSPNM PIC 99.
       77  CPTPNMSANSNOM PIC 99.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INIT
           PERFORM TRAIT UNTIL EOFNOM = 1
           PERFORM FIN
           STOP RUN.
       INIT.
           OPEN INPUT NOMFIC PRENOMFIC OUTPUT NOMPRENOM
           READ NOMFIC
               AT END
                   MOVE 1 TO EOFNOM
                   MOVE 99 TO NOCLI OF NOMFIC
           END-READ.
           READ PRENOMFIC
               AT END
                   MOVE 1 TO EOFPNM
                   MOVE 99 TO NOCLI OF PRENOMFIC

           END-READ.
       TRAIT.
      *     IF (EOFNOM = 1 AND EOFPNM = 1)
      *         THEN MOVE 1 TO EOFALL
           EVALUATE NOCLI OF ENREG-NOM
               WHEN = NOCLI OF ENREG-PNM
                  STRING ENREG-NOM PNM OF ENREG-PNM INTO ENREG-NOMPNM
                  DISPLAY ENREG-NOMPNM " **** "
                  WRITE ENREG-NOMPNM
                  END-WRITE
                   ADD 1 TO CPTNOMPNM
                   READ NOMFIC
                       AT END
                           MOVE 1 TO EOFNOM
                           MOVE 99 TO NOCLI OF NOMFIC
                   END-READ
                   READ PRENOMFIC
                       AT END
                           MOVE 1 TO EOFPNM
                           MOVE 99 TO NOCLI OF PRENOMFIC
                   END-READ
               WHEN < NOCLI OF ENREG-PNM
                   ADD 1 TO CPTNOMSANSPNM
                   READ NOMFIC
                       AT END
                           MOVE 1 TO EOFNOM
                           MOVE 99 TO NOCLI OF NOMFIC
                   END-READ
               WHEN > NOCLI OF ENREG-PNM
                    ADD 1 TO CPTPNMSANSNOM
                       READ PRENOMFIC
                           AT END
                               MOVE 1 TO EOFPNM
                               MOVE 99 TO NOCLI OF PRENOMFIC
                       END-READ
               END-EVALUATE.

       FIN.
           CLOSE NOMFIC PRENOMFIC NOMPRENOM
           DISPLAY "Nb enregs : " CPTNOMPNM
           DISPLAY "Nb Noms orphelins : " CPTNOMSANSPNM
           DISPLAY "Nb Prenoms orphelins : " CPTPNMSANSNOM.

       END PROGRAM EXO-VI-B.
