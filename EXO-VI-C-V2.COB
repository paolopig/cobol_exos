      ******************************************************************
      * Author: PAOLO PIGNOTTI
      * Date: 28/11/2019
      * Objectif :  le pgm va enlever les doublons
      * et créer un fichier sortie sans doublon
      * - V2 : doublon sur idcli, on garde le dernier nom
      *  et on enleve les autres
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXO-VI-C-V2.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLI-FIC
               ASSIGN TO "../ClientsIN.txt".
           SELECT CLI-OUT-FIC
               ASSIGN TO "../ClientsOUT.txt".
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
       01  PREV-ENREG-CLIENT PIC X(12).
       77  EOF PIC 9 VALUE 0.
       77  PREV-IDCLI PIC 99 VALUE 0.
       77  CPT-ENREG PIC 99 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INIT
           PERFORM TRAIT UNTIL EOF = 1
           PERFORM FIN
           STOP RUN.
       INIT.
           OPEN INPUT CLI-FIC OUTPUT CLI-OUT-FIC
           READ CLI-FIC
               AT END MOVE 1 TO EOF
           END-READ.
           MOVE IDCLI TO PREV-IDCLI.
           MOVE ENREG-CLIENT TO PREV-ENREG-CLIENT.

       TRAIT.
           READ CLI-FIC
               AT END MOVE 1 TO EOF
           END-READ.
           IF IDCLI = PREV-IDCLI
               MOVE IDCLI TO PREV-IDCLI
               MOVE ENREG-CLIENT TO PREV-ENREG-CLIENT
           END-IF

           IF IDCLI <> PREV-IDCLI
               MOVE PREV-ENREG-CLIENT TO ENREG-CLI-OUT
               WRITE ENREG-CLI-OUT
               END-WRITE
               ADD 1 TO CPT-ENREG
               MOVE IDCLI TO PREV-IDCLI
               MOVE ENREG-CLIENT TO PREV-ENREG-CLIENT
           END-IF.

       FIN.
           MOVE ENREG-CLIENT TO ENREG-CLI-OUT
           WRITE ENREG-CLI-OUT
           END-WRITE
           ADD 1 TO CPT-ENREG

           PERFORM DISPLAY-RESULT
           CLOSE CLI-FIC CLI-OUT-FIC.

       DISPLAY-RESULT.
           DISPLAY "Nombre d'enregistrements s/doublons: " CPT-ENREG.

       END PROGRAM EXO-VI-C-V2.
