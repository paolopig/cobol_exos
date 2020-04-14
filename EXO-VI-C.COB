      ******************************************************************
      * Author: PAOLO PIGNOTTI
      * Date: 28/11/2019
      * Objectif :  le pgm va enlever les doublons
      * et créer un fichier sortie sans doublon
      * - V1 : doublon sur idcli, on garde le premier nom
      *  et on enleve les autres
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXO-VI-C.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLI-FIC
               ASSIGN TO "C:\Users\Cobol\ClientsIN.txt".
           SELECT CLI-OUT-FIC
               ASSIGN TO "C:\Users\Cobol\ClientsOUT.txt".
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
       01  ENREG-CLI-OUT PIC X(12).
      *
       WORKING-STORAGE SECTION.
       77  EOF PIC 9 VALUE 0.
       77  ZON  PIC X(12).
       77  OLD-IDCLI PIC 99 VALUE 0.
       77  CPT-ENREG PIC 99 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INIT
           PERFORM TRAIT UNTIL EOF = 1
           PERFORM FIN
           STOP RUN.
       INIT.
           OPEN INPUT CLI-FIC OUTPUT CLI-OUT-FIC
           READ CLI-FIC INTO ZON
               AT END MOVE 1 TO EOF
           END-READ.
       TRAIT.
           IF IDCLI <> OLD-IDCLI
               THEN
               WRITE ENREG-CLI-OUT FROM ZON
               END-WRITE
               MOVE IDCLI TO OLD-IDCLI
               ADD 1 TO CPT-ENREG
           END-IF
           READ CLI-FIC INTO ZON
               AT END MOVE 1 TO EOF
           END-READ.
       FIN.
           PERFORM DISPLAY-RESULT
           CLOSE CLI-FIC CLI-OUT-FIC.

       DISPLAY-RESULT.
           DISPLAY "Nombre d'enregistrements s/doublons: " CPT-ENREG.

       END PROGRAM EXO-VI-C.
