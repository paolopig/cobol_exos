       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXO10.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIFIC
               ASSIGN TO "C:\Users\Cobol\Clients.txt".
           SELECT CLIH
               ASSIGN TO "C:\Users\Cobol\ClientsHommes.txt".
           SELECT CLIF
               ASSIGN TO "C:\Users\Cobol\ClientsFemmes.txt".
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CLIFIC.
       01  ENREG-CLIENT.
           05 NOM PIC X(5).
           05 PRENOM PIC X(5).
           05 SEXE PIC X.
      *
       FD  CLIH.
       01  ENREG-H PIC X(11).
      *
       FD  CLIF.
       01  ENREG-F PIC X(11).
      *
       WORKING-STORAGE SECTION.
       77  HOMME PIC 99.
       77  FEMME PIC 99.
       77  NEUTRE PIC 99.
       77  HOMMEECRAN PIC ZZ.
       77  FEMMEECRAN PIC ZZ.
       77  NEUTREECRAN PIC ZZ.
       77  EOF  PIC 9 VALUE 0.
       77  ZON PIC X(11).
      *
       PROCEDURE DIVISION.
      *
       MAIN-PROCEDURE.
           PERFORM INIT
           PERFORM TRAIT UNTIL EOF = 1
           PERFORM FIN
           STOP RUN.

       INIT.
           OPEN INPUT CLIFIC
           OPEN OUTPUT CLIH
           OPEN OUTPUT CLIF
           READ CLIFIC INTO ZON
               AT END MOVE 1 TO EOF
           END-READ.

       TRAIT.
           DISPLAY ENREG-CLIENT " **** "
           EVALUATE SEXE
               WHEN "M"
      *            MOVE ENREG-CLIENT TO ENREG-H
                   WRITE ENREG-H FROM ZON
                   END-WRITE
                   ADD 1 TO HOMME
               WHEN "F"
      *            MOVE ENREG-CLIENT TO ENREG-F
                   WRITE ENREG-F FROM ZON
                   END-WRITE
                   ADD 1 TO FEMME
               WHEN OTHER
                   ADD 1 TO NEUTRE
               END-EVALUATE.

           READ CLIFIC  INTO ZON
               AT END MOVE 1 TO EOF
           END-READ.

       FIN.
           PERFORM DISPLAY-RESULT
           CLOSE CLIFIC CLIH CLIF.


       DISPLAY-RESULT.
           MOVE HOMME TO HOMMEECRAN
           MOVE FEMME TO FEMMEECRAN
           MOVE NEUTRE TO NEUTREECRAN
           DISPLAY "_____________________________".
           DISPLAY "| Hommes | Femmes  | Neutre |".
           DISPLAY "_____________________________".
           DISPLAY "|   " HOMMEECRAN  "   |   "
           FEMMEECRAN "    |  " NEUTREECRAN.


       END PROGRAM EXO10.
