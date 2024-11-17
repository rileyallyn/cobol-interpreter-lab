IDENTIFICATION DIVISION.
PROGRAM-ID. BRANCHING.
DATA DIVISION.
    WORKING-STORAGE SECTION.
        05 A PIC S9(2) VALUE 1.
        05 B PIC S9(2) VALUE 2.
PROCEDURE DIVISION.
    IF A > B
        DISPLAY 'A is greater than B'
    ELSE IF A = B
        DISPLAY 'A is equal to B'
    ELSE
        DISPLAY 'B is greater than A'
    END-IF
    STOP RUN.
