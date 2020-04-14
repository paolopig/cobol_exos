      *****************************************************************
      * Author: PAOLO PIGNOTTI
      * Date: 03/12/2019
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUBBLESORT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FIC-A-TRIER
               ASSIGN TO "../BubblesortATrier.txt".
           SELECT FIC-TRIE
               ASSIGN TO "../BubblesortTrie2.txt".
       DATA DIVISION.
       FILE SECTION.
       FD  FIC-A-TRIER.
       01  ENREG-EMPL.
           05 ID-EMPL PIC 9(2).
           05 NOM-EMPL PIC X(10).

       FD  FIC-TRIE.
       01  ENREG-TRI.
           05 ID-EMPL-TRI PIC 9(2).
           05 NOM-EMPL-TRI PIC X(10).

       WORKING-STORAGE SECTION.
       01  ENREG-TMP.
           05 ID-EMPL-TMP PIC 9(2).
           05 NOM-EMPL-TMP PIC X(10).

       01  ZON.
           05 ID-EMPL-ZON PIC 9(2).
           05 NOM-EMPL-ZON PIC X(10).

       77  CPT-PASSAGES PIC 999 VALUE 0.
       77  EOF  PIC 9 VALUE 0.


       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM INIT
           PERFORM TRAIT UNTIL EOF = 1
           PERFORM FIN.
           STOP RUN.
       INIT.
           OPEN INPUT FIC-A-TRIER OUTPUT FIC-TRIE

           READ FIC-A-TRIER INTO ZON
               AT END MOVE 1 TO EOF
           END-READ
               MOVE ID-EMPL TO ID-EMPL-TMP
               MOVE NOM-EMPL TO NOM-EMPL-TMP
      *         DISPLAY ID-EMPL "init actuel " ID-EMPL-TMP  "tmp "
            READ FIC-A-TRIER
               AT END MOVE 1 TO EOF
           END-READ.

       TRAIT.
           IF ID-EMPL-TMP < ID-EMPL
               THEN
               MOVE ID-EMPL-TMP TO ID-EMPL-TRI
               MOVE NOM-EMPL-TMP TO NOM-EMPL-TRI
               WRITE ENREG-TRI
               END-WRITE
               DISPLAY ID-EMPL " entregistree <"
               MOVE ID-EMPL TO ID-EMPL-TMP
               MOVE NOM-EMPL TO NOM-EMPL-TMP
           ELSE
               MOVE ID-EMPL TO ID-EMPL-TRI
               MOVE NOM-EMPL TO NOM-EMPL-TRI
               WRITE ENREG-TRI
               END-WRITE
               DISPLAY ID-EMPL " entregistree >"
           END-IF

      *     DISPLAY ID-EMPL "actuel " ID-EMPL-TMP  "tmp ".
            READ FIC-A-TRIER
               AT END MOVE 1 TO EOF
           END-READ.

       FIN.
           MOVE ID-EMPL-TMP TO ID-EMPL-TRI
           MOVE NOM-EMPL-TMP TO NOM-EMPL-TRI
           WRITE ENREG-TRI
           END-WRITE
           ADD 1 TO CPT-PASSAGES

           DISPLAY " Nb passages : "CPT-PASSAGES

           CLOSE FIC-A-TRIER FIC-TRIE.

       END PROGRAM BUBBLESORT.
