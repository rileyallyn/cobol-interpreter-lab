IDENTIFICATION DIVISION.
PROGRAM-ID. LOOPING.
DATA DIVISION.
WORKING-STORAGE SECTION.
    01  WS-I PIC 9(3) VALUE 1.

PROCEDURE DIVISION.
    PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
        DISPLAY WS-I
    END-PERFORM
    STOP RUN.
    