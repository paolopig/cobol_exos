      ******************************************************************
      * Author: PAOLO PIGNOTTI
      * Date: 28/11/2019
      * Objectif :  le pgm va enlever les doublons
      * et créer un fichier sortie sans doublon
      * - v3 : comparaison des noms, si identique on garde le premier
      *  sinon on garde le premier et on crée un
      *  enregistrement avec l'autre nom (id = max +1)
      * - v4 : meme v3, avec recuperation des trous
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXO-VI-C-V4.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLI-FIC
               ASSIGN TO "C:\Users\Cobol\ClientsIN.txt".
           SELECT CLI-OUT-FIC
               ASSIGN TO "C:\Users\Cobol\ClientsOUTV4.txt".
           SELECT CLI-OUT-FIC2
               ASSIGN TO "C:\Users\Cobol\ClientsOUTV4-2.txt".
           SELECT CLI-TMP
               ASSIGN TO "C:\Users\Cobol\ClientsATrierTemp.txt".
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CLI-FIC.
       01  ENREG-CLIENT.
           05 IDCLI PIC 99.
           05 NOMCLI PIC X(5).
           05 PNMCLI PIC X(5).
      *
       FD  CLI-OUT-FIC.
       01  ENREG-CLI-OUT.
           05 IDCLI-OUT PIC 99.
           05 NOMCLI-OUT PIC X(5).
           05 PNMCLI-OUT PIC X(5).

       FD  CLI-OUT-FIC2.
       01  ENREG-CLI-OUT2 PIC X(12).


       SD  CLI-TMP.
       01  ENREG-CLI-TMP.
           05 IDCLI-TMP PIC 99.
           05 NOMCLI-TMP PIC X(5).
           05 PNMCLI-TMP PIC X(5).
      *
       WORKING-STORAGE SECTION.
       77  EOF PIC 9 VALUE 0.
       77  EOFMAX PIC 9 VALUE 0.
       77  ZON  PIC X(12).
       77  OLD-IDCLI PIC 99 VALUE 0.
       77  OLD-NOMCLI PIC X(5).
       77  CPT-ENREG PIC 99 VALUE 0.
       77  CPT-ENRDBL PIC 99 VALUE 0.
       77  ID-MAX PIC 99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INIT
           PERFORM TRAIT UNTIL EOF = 1
           PERFORM FIN
           STOP RUN.
       INIT.
           PERFORM RECHERCHE-MAX
           OPEN INPUT CLI-FIC OUTPUT CLI-OUT-FIC

           READ CLI-FIC
               AT END MOVE 1 TO EOF
           END-READ.
       TRAIT.

           IF IDCLI <> OLD-IDCLI
               THEN
               MOVE ENREG-CLIENT TO ENREG-CLI-OUT
               WRITE ENREG-CLI-OUT
               END-WRITE
               MOVE IDCLI TO OLD-IDCLI
               MOVE NOMCLI TO OLD-NOMCLI
               ADD 1 TO CPT-ENREG
           END-IF

           IF IDCLI = OLD-IDCLI
               THEN
      *    Comparaison des noms...
               IF NOMCLI <> OLD-NOMCLI
                   THEN
                   ADD 1 TO ID-MAX
                   MOVE ID-MAX TO IDCLI
                   MOVE ENREG-CLIENT TO ENREG-CLI-OUT
               WRITE ENREG-CLI-OUT
               END-WRITE
      *         MOVE IDCLI TO OLD-IDCLI
               ADD 1 TO CPT-ENRDBL
           END-IF

           READ CLI-FIC INTO ZON
               AT END MOVE 1 TO EOF
           END-READ.
       FIN.
           CLOSE CLI-FIC CLI-OUT-FIC
           SORT CLI-TMP
           ASCENDING KEY IDCLI-TMP
           USING CLI-OUT-FIC
           GIVING CLI-OUT-FIC2

           PERFORM DISPLAY-RESULT.

       DISPLAY-RESULT.
           DISPLAY "Nombre d'enregistrements s/doublons: " CPT-ENREG.
           DISPLAY "Nombre d'enregistrements doublons: " CPT-ENRDBL.

       RECHERCHE-MAX.
           PERFORM INIT-MAX.
           PERFORM TRAIT-MAX UNTIL EOFMAX = 1.
           PERFORM FIN-MAX.
       INIT-MAX.
           OPEN INPUT CLI-FIC.
       TRAIT-MAX.
           READ CLI-FIC
               AT END MOVE 1 TO EOFMAX
           END-READ.
               MOVE IDCLI TO ID-MAX.
       FIN-MAX.
           DISPLAY ID-MAX " ID-Max".
           CLOSE CLI-FIC.
       END PROGRAM EXO-VI-C-V4.
