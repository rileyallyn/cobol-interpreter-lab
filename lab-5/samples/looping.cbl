IDENTIFICATION DIVISION.
PROGRAM-ID. LOOPING.
PROCEDURE DIVISION.
    PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
        DISPLAY I
    END-PERFORM
    STOP RUN.
