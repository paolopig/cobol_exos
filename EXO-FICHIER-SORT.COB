      ******************************************************************
      * Author: PAOLO PIGNOTTI
      * Date: 28/11/2019
      * Exo pour fichier SORT
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXO-FICHIER-SORT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLI-FIC
               ASSIGN TO "C:\Users\Cobol\ClientsIN.txt".
           SELECT CLI-TMP
               ASSIGN TO "C:\Users\Cobol\ClientsATrierTemp.txt".
           SELECT CLI-SORT
               ASSIGN TO "C:\Users\Cobol\ClientsTries.txt".
       DATA DIVISION.
       FILE SECTION.
       FD  CLI-FIC.
       01  ENREG-CLI-FIC.
           05 IDCLI PIC 99.
           05 NOMCLI PIC X(5).
           05 PNMCLI PIC X(5).

       SD  CLI-TMP.
       01  ENREG-CLI-TMP.
           05 IDCLI PIC 99.
           05 NOMCLI PIC X(5).
           05 PNMCLI PIC X(5).

       FD  CLI-SORT.
       01  ENREG-CLI-SORT  PIC X(12).

       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INIT
           PERFORM TRAIT
           PERFORM FIN
           STOP RUN.
       INIT.

       TRAIT.
           SORT CLI-TMP
           ASCENDING KEY NOMCLI OF ENREG-CLI-FIC
           USING CLI-FIC
           GIVING CLI-SORT.
       FIN.

       END PROGRAM EXO-FICHIER-SORT.
