       IDENTIFICATION DIVISION.
       PROGRAM-ID. SPRO01.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  PI PIC 9(1)V99 VALUE 3.14.
       LINKAGE SECTION.
       01 PARAMETRES.
           05 R PIC 9(3)V99.
           05 RESULTAT PIC 9(3)V99.
           05 FONCTION PIC X.
           05 CODERETOUR PIC 9.

       PROCEDURE DIVISION USING PARAMETRES.
       MAIN-PROCEDURE.
           PERFORM INIT.
           PERFORM TRAIT.
           PERFORM FIN.
           GOBACK.

       INIT.
           INITIALISE RESULTAT.

       TRAIT.
            EVALUATE FONCTION
               WHEN = "S"
                   COMPUTE RESULTAT = PI * R ** 2
                   MOVE 1 TO CODERETOUR
               WHEN = "P"
                   COMPUTE RESULTAT = 2 * PI * R
                   MOVE 1 TO CODERETOUR
               WHEN OTHER
                   GOBACK
           END-EVALUATE .
       FIN.

       END PROGRAM SPRO01.
