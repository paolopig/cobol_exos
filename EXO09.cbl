       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXO09.
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
       01  EOF  PIC 9 VALUE 0.

       PROCEDURE DIVISION.

       DEBUT.
           PERFORM LIRECLIENT UNTIL EOF = 1
           PERFORM FIC-CLOSE
           PERFORM DISPLAY-RESULT
           STOP RUN.

       LIRECLIENT.
           OPEN INPUT CLIFIC
           READ CLIFIC
               AT END MOVE 1 TO EOF
              DISPLAY ENREG-CLIENT
           EVALUATE SEXE
               WHEN "M"
                   ADD 1 TO HOMME
               WHEN "F"
                   ADD 1 TO FEMME

               END-EVALUATE

           END-READ.

       FIC-CLOSE.
           CLOSE CLIFIC.

       DISPLAY-RESULT.
           DISPLAY HOMME "/" FEMME.
       END PROGRAM EXO09.
