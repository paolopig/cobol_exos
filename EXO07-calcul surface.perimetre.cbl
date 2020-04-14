       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXO07.
      *
       ENVIRONMENT DIVISION.
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       01  R PIC 9(3)V99.
       01  SURFACE PIC 9(3)V99.
       01  PERIMETRE PIC 9(3)V99.
       01  PI PIC 9(1)V99 VALUE 3.14.
      *
       PROCEDURE DIVISION.
      *    DISPLAY PI
           DISPLAY "Saisir le rayon R: "
           ACCEPT R
           COMPUTE SURFACE = PI * R ** 2
           DISPLAY "Surface = " SURFACE
           COMPUTE PERIMETRE = 2 * PI * R
           DISPLAY "Perimetre = " PERIMETRE.
       END PROGRAM EXO07.
