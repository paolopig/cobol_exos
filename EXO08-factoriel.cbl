       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXO08.
      *
       ENVIRONMENT DIVISION.
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       01  N PIC 9.
       01  RESULTAT PIC 999999 VALUE 1.
       01  I PIC 9 VALUE 1.
      *
       PROCEDURE DIVISION.
       DEBUT.
           PERFORM INIT
           PERFORM CALCUL UNTIL I > N
           PERFORM AFFICHE_RESULTAT
           STOP RUN.
       INIT.
           DISPLAY "Saisir un entier N petit 0-9 : "
           ACCEPT N.
       CALCUL.
           COMPUTE RESULTAT = RESULTAT * I
           ADD 1 TO I.
       AFFICHE_RESULTAT.
           DISPLAY "Le resultat factoriel de N! est :".
           DISPLAY RESULTAT.
       END PROGRAM EXO08.
