       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXO9-1.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIFIC
                ASSIGN TO "C:\Users\Cobol\Clients2.txt".
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
       01  SORTIR PIC 9 VALUE 0.

       PROCEDURE DIVISION.

       DEBUT.
           OPEN OUTPUT CLIFIC
           PERFORM WRITE-CLIENT UNTIL SORTIR = 1
           PERFORM CLOSE-FIC
           STOP RUN.
       WRITE-CLIENT.
           DISPLAY "Saisir le Nom : "
           ACCEPT NOM.
           DISPLAY "Saisir le Prenom : "
           ACCEPT PRENOM.
           DISPLAY "Saisir le Sexe : "
           ACCEPT SEXE.

           WRITE ENREG-CLIENT
           END-WRITE

           DISPLAY "Sortir ? 0-non / 1-oui : "
           ACCEPT SORTIR.
       CLOSE-FIC.
           CLOSE CLIFIC.

       END PROGRAM EXO9-1.
