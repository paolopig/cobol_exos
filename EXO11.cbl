       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXO11.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  SPGM PIC X(6) VALUE "SPRO01".
       77  F PIC 9 VALUE 0.
       77  RES-ECRAN PIC ZZZV,99.

       01  PARAMETRES.
           05 R PIC 9(3)V99.
           05 RESULTAT PIC 9(3)V99.
           05 FONCTION PIC X.
           05 CODERETOUR PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INIT
           PERFORM TRAIT UNTIL F = 1
           PERFORM FIN
           STOP RUN.

       INIT.
           INITIALISE R RESULTAT FONCTION.
       TRAIT.
           DISPLAY "Saisir le rayon R: "
           ACCEPT R.
           DISPLAY "Saisir la fonction : "
           DISPLAY "S/Surface, P/Perimetre ou F/Fin de traitement : "
           ACCEPT FONCTION
           EVALUATE FONCTION
           WHEN "S"
               CALL SPGM USING PARAMETRES
               MOVE RESULTAT TO RES-ECRAN
               DISPLAY "Surface = " RES-ECRAN " mt"
               DISPLAY "Code retour = " CODERETOUR
           WHEN "P"
               CALL SPGM USING PARAMETRES
               MOVE RESULTAT TO RES-ECRAN
               DISPLAY "Perimetre = " RES-ECRAN "mt"
               DISPLAY "Code retour = " CODERETOUR
           WHEN "F"
               Move 1 to F
           WHEN OTHER
               DISPLAY "Saisie incorrecte"
               DISPLAY "Saisir la fonction : "
               DISPLAY "S/Surface, P/Perimetre ou F/Fin de traitement"
               ACCEPT FONCTION
           END-EVALUATE .

       FIN.
           DISPLAY "Goodbye !".

       END PROGRAM EXO11.
