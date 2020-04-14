       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXO99.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIFIC
               ASSIGN TO "C:\Users\Cobol\Clients.txt".
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CLIFIC.
       01  ENREG-CLIENT.
           05 NOM PIC X(5).
           05 PRENOM PIC X(5).
           05 SEXE PIC X.
      *
       WORKING-STORAGE SECTION.
       01  HOMME PIC 99.
       01  FEMME PIC 99.
       01  NEUTRE PIC 99.
       01  EOF  PIC 9 VALUE 0.

       PROCEDURE DIVISION.

       DEBUT.
           OPEN INPUT CLIFIC
           READ CLIFIC
               AT END MOVE 1 TO EOF
           END-READ
           PERFORM LIRECLIENT UNTIL EOF = 1
           PERFORM DISPLAY-RESULT
           PERFORM FIC-CLOSE

           STOP RUN.

       LIRECLIENT.
           DISPLAY ENREG-CLIENT " **** "
           EVALUATE SEXE
           WHEN "M"
               ADD 1 TO HOMME
           WHEN "F"
               ADD 1 TO FEMME
           WHEN OTHER
               ADD 1 TO NEUTRE
           END-EVALUATE.

           READ CLIFIC
               AT END MOVE 1 TO EOF
           END-READ.

       DISPLAY-RESULT.
           DISPLAY "Hommes / Femmes  / Neutre".
           DISPLAY HOMME "     /" FEMME "       /" NEUTRE.

       FIC-CLOSE.
           CLOSE CLIFIC.

       END PROGRAM EXO99.
