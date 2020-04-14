      ******************************************************************
      * Author: PAOLO PIGNOTTI
      * Date: 03/12/2019
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUBBLESORT-V3.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FIC-A-TRIER
               ASSIGN TO "C:\Users\Cobol\BubblesortATrier.txt".
           SELECT FIC-TRIE
               ASSIGN TO "C:\Users\Cobol\BubblesortTrie.txt".
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
       77  CPT-PASSAGES PIC 999 VALUE 0.
       77  CPT-PASS-TMP PIC 999 VALUE 0.
       77  OLD-ID PIC 999 VALUE 0.
       77  SORT-OK PIC 9 VALUE 0.
       77  EOF  PIC 9 VALUE 0.
       77  SENS-LECTURE PIC 9 VALUE 0.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM INIT.
           PERFORM PROCEDURE-BOUCLE2 UNTIL SORT-OK = 1
           PERFORM FIN
           STOP RUN.
       INIT.

       PROCEDURE-BOUCLE2.
           PERFORM INIT-BOUCLE2
           PERFORM TRAIT UNTIL EOF = 1
           PERFORM FIN-BOUCLE2.

       INIT-BOUCLE2.
           MOVE 1 TO SORT-OK.
           MOVE 0 TO CPT-PASS-TMP
           MOVE 0 TO EOF

           PERFORM OUVERTURE
           PERFORM LECTURE-INIT
           DISPLAY ID-EMPL "init 0 actuel " ID-EMPL-TMP  "tmp "
           DISPLAY ID-EMPL-TRI "init 1 actuel " ID-EMPL-TMP  "tmp "
           MOVE ID-EMPL-TMP TO OLD-ID

           PERFORM LECTURE.

       OUVERTURE.
           IF SENS-LECTURE = 0
               THEN
               OPEN INPUT FIC-A-TRIER OUTPUT FIC-TRIE
           ELSE
               OPEN OUTPUT FIC-A-TRIER INPUT FIC-TRIE
           END-IF.

       LECTURE-INIT.
           IF SENS-LECTURE = 0
               THEN
                READ FIC-A-TRIER
                   AT END MOVE 1 TO EOF
               END-READ
               MOVE ID-EMPL TO ID-EMPL-TMP
               MOVE NOM-EMPL TO NOM-EMPL-TMP
           ELSE
                READ FIC-TRIE
                   AT END MOVE 1 TO EOF
               END-READ
               MOVE ID-EMPL-TRI TO ID-EMPL-TMP
               MOVE NOM-EMPL-TRI TO NOM-EMPL-TMP
           END-IF.

       LECTURE.
           IF SENS-LECTURE = 0
               THEN
                READ FIC-A-TRIER
                   AT END MOVE 1 TO EOF
               END-READ
           ELSE
                READ FIC-TRIE
                   AT END MOVE 1 TO EOF
               END-READ
           END-IF.

       ECRITURE.
           IF SENS-LECTURE = 0
               THEN
                   IF ID-EMPL-TMP < ID-EMPL
                       THEN
                       MOVE ID-EMPL-TMP TO ID-EMPL-TRI
                       MOVE NOM-EMPL-TMP TO NOM-EMPL-TRI
                       WRITE ENREG-TRI
                       END-WRITE
                       MOVE ID-EMPL TO ID-EMPL-TMP
                       MOVE NOM-EMPL TO NOM-EMPL-TMP
                       MOVE ID-EMPL-TRI TO OLD-ID
                   ELSE
                       MOVE ID-EMPL TO ID-EMPL-TRI
                       MOVE NOM-EMPL TO NOM-EMPL-TRI
                       IF OLD-ID > ID-EMPL-TRI
                           THEN MOVE 0 TO SORT-OK
                       END-IF
                       WRITE ENREG-TRI
                       END-WRITE
                       ADD 1 TO CPT-PASS-TMP
                       MOVE ID-EMPL-TRI TO OLD-ID
                   END-IF
           ELSE
                   IF ID-EMPL-TMP < ID-EMPL-TRI
                       THEN
                       MOVE ID-EMPL-TMP TO ID-EMPL
                       MOVE NOM-EMPL-TMP TO NOM-EMPL
                       WRITE ENREG-EMPL
                       END-WRITE
                       MOVE ID-EMPL-TRI TO ID-EMPL-TMP
                       MOVE NOM-EMPL-TRI TO NOM-EMPL-TMP
                       MOVE ID-EMPL TO OLD-ID
                   ELSE
                       MOVE ID-EMPL-TRI TO ID-EMPL
                       MOVE NOM-EMPL-TRI TO NOM-EMPL
                       IF OLD-ID > ID-EMPL
                           THEN MOVE 0 TO SORT-OK
                       END-IF
                       WRITE ENREG-EMPL
                       END-WRITE
                       ADD 1 TO CPT-PASS-TMP
                       MOVE ID-EMPL TO OLD-ID
                   END-IF
           END-IF.

       TRAIT.
           IF SENS-LECTURE = 0
               THEN
                   IF ID-EMPL-TMP < ID-EMPL
                       THEN
                       PERFORM ECRITURE
                   ELSE
                       PERFORM ECRITURE
                       ADD 1 TO CPT-PASS-TMP
                   END-IF
                   DISPLAY ID-EMPL "0actuel " ID-EMPL-TMP  "tmp "
                   DISPLAY ID-EMPL-TRI "1actuel " ID-EMPL-TMP  "tmp "
                   PERFORM LECTURE
           ELSE
                   IF ID-EMPL-TMP < ID-EMPL-TRI
                       THEN
                       PERFORM ECRITURE
                   ELSE
                       PERFORM ECRITURE
                       ADD 1 TO CPT-PASS-TMP
                   END-IF
                   DISPLAY ID-EMPL-TRI "actuel 0 " ID-EMPL-TMP  "tmp "
                   PERFORM LECTURE
           END-IF.

       ECRITURE-FINALE.
           IF SENS-LECTURE = 0
               THEN
                   MOVE ID-EMPL-TMP TO ID-EMPL-TRI
                   MOVE NOM-EMPL-TMP TO NOM-EMPL-TRI
                   WRITE ENREG-TRI
                   END-WRITE
           ELSE
                   MOVE ID-EMPL-TMP TO ID-EMPL
                   MOVE NOM-EMPL-TMP TO NOM-EMPL
                   WRITE ENREG-EMPL
                   END-WRITE
           END-IF.

       FIN-BOUCLE2.
           PERFORM ECRITURE-FINALE
           ADD 1 TO CPT-PASSAGES
           DISPLAY ID-EMPL "dernier0 - actuel " ID-EMPL-TMP  "tmp ".
           DISPLAY ID-EMPL-TRI "dernier1 - actuel " ID-EMPL-TMP  "tmp ".
           DISPLAY " Nb passages : "CPT-PASSAGES

           CLOSE FIC-A-TRIER FIC-TRIE
      *    Pour eviter les boucles infinies en cas de panne
           IF CPT-PASSAGES >20
               THEN
               MOVE 1 TO SORT-OK
           END-IF

           IF SENS-LECTURE = 0
               THEN
               MOVE 1 TO SENS-LECTURE
           ELSE
               MOVE 0 TO SENS-LECTURE
           END-IF
           DISPLAY " sense lecture : "SENS-LECTURE
           DISPLAY " CPT-PASS-TMP : "CPT-PASS-TMP
           DISPLAY " Sort Ok : "SORT-OK.
       FIN.
           DISPLAY " Nb passages final : "CPT-PASSAGES.

       END PROGRAM BUBBLESORT-V3.
