      ******************************************************************
      * Author: PAOLO PIGNOTTI
      * Date: 04/12/2019
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXO-VI-G.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  CHAINE-X.
           05 ELEM-X OCCURS 5 PIC 9.
.      01  CHAINE-Y.
           05 ELEM-Y OCCURS 5 PIC 9.
       01  RESULTAT.
           05 RSLT-NUM OCCURS 10 PIC 9.
       01  RES-PART.
           05 RES-PT-NUMERIQUE OCCURS 10 PIC 9.
       77  CMPT-X PIC 99 VALUE 1.
       77  CMPT-Y PIC 99 VALUE 1.
       77  CMPT-LIGNE PIC 99 VALUE 0.
       77  NB-PASS PIC 999 VALUE 0.
       77  RESTE-CPT10 PIC 9.
       77  RESTE-A-REPORTER PIC 9.
       77  RETENUE PIC 9.
       77  SOMME-ZON PIC 99.

       77  ZON PIC 99.
       01  ZON2.
           05 ZONP OCCURS 2 PIC 9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INIT
           PERFORM TRAIT UNTIL CMPT-Y = 0
           PERFORM FIN
           STOP RUN.
       INIT.
      *>      MOVE "000000999999999999" TO CHAINE-X
      *>      MOVE "00000000009999999" TO CHAINE-Y
           MOVE "00999" TO CHAINE-X
           MOVE "00099" TO CHAINE-Y
           MOVE 5 TO CMPT-X.
           MOVE 5 TO CMPT-Y.
       TRAIT.

           PERFORM PROCEDURE2.
      *     SUBTRACT 1 FROM CMPT-Y .
       FIN.
           DISPLAY RESULTAT.

       PROCEDURE2.
           PERFORM INIT2
           PERFORM TRAIT2 UNTIL CMPT-X = 0
           PERFORM FIN2.
       INIT2.
           MOVE 5 TO CMPT-X
           MOVE 0 TO ZON
           MOVE 0 TO RETENUE.
       TRAIT2.
      *     MOVE 0 TO RESTE-A-REPORTER


           ADD CMPT-X TO CMPT-Y GIVING CMPT-LIGNE
           COMPUTE ZON =  ELEM-Y(CMPT-Y) * ELEM-X(CMPT-X)

           ADD RETENUE TO ZON
           ADD RSLT-NUM(CMPT-LIGNE) TO ZON

           MOVE ZON TO ZON2


           MOVE ZONP(2) TO RSLT-NUM(CMPT-LIGNE)

           MOVE ZONP(1) TO RETENUE


           DISPLAY " ELEM Y " CMPT-Y
           DISPLAY " ELEM X " CMPT-X
           DISPLAY " CMPT-LIGNE  : "  CMPT-LIGNE
           DISPLAY " ZONP(2)  : "  ZONP(2)
           DISPLAY " ZONP(1)   : "  ZONP(1)
           DISPLAY " RETENUE : "  RETENUE
           DISPLAY " RSLT-NUMERIQUE  : "  RESULTAT
           SUBTRACT 1 FROM CMPT-X
           DISPLAY " ____________________________________________" .

       FIN2.

           SUBTRACT 1 FROM CMPT-Y
           DISPLAY " FIN2 - ELEM Y " CMPT-Y
           DISPLAY " FIN2 - ELEM X " CMPT-X
      *     DISPLAY " PART " RES-PART
           DISPLAY " FIN2 - TOTAL " RESULTAT.

       END PROGRAM EXO-VI-G.
