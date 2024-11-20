IDENTIFICATION DIVISION.
PROGRAM-ID. sorting.
DATA DIVISION.
WORKING-STORAGE SECTION.
    01  WS-SORT-AREA.
    05  WS-SORT-TABLE.
        10  WS-SORT-ROW PIC S9(4) OCCURS 100.
    05  WS-TEMP-ROW     PIC S9(4).
    05  WS-ROW-MAX      PIC S9(4) COMP VALUE 100.
    05  WS-SORT-MAX     PIC S9(4) COMP.
    05  WS-I            PIC S9(4) COMP.
    05  WS-J            PIC S9(4) COMP.
    05  WS-INDEX        PIC S9(4) COMP.

PROCEDURE DIVISION.
*> Initialize test data
    MOVE 30 TO WS-SORT-ROW(1)
    MOVE 10 TO WS-SORT-ROW(2)
    MOVE 50 TO WS-SORT-ROW(3)
    MOVE 20 TO WS-SORT-ROW(4)
    MOVE 40 TO WS-SORT-ROW(5)
    MOVE 5 TO WS-SORT-MAX

*> * Display original array
    DISPLAY "Original Array Contents:"
    DISPLAY "---------------------"
    PERFORM VARYING WS-INDEX FROM 1 BY 1
        UNTIL WS-INDEX > WS-SORT-MAX
        DISPLAY "Element " WS-INDEX ": " WS-SORT-ROW(WS-INDEX)
    END-PERFORM
    DISPLAY ""

*> * Simplified bubble sort
    PERFORM VARYING WS-I FROM 1 BY 1 
        UNTIL WS-I > WS-SORT-MAX - 1
        PERFORM VARYING WS-J FROM 1 BY 1 
            UNTIL WS-J > WS-SORT-MAX - WS-I
            IF WS-SORT-ROW(WS-J) > WS-SORT-ROW(WS-J + 1)
                MOVE WS-SORT-ROW(WS-J) TO WS-TEMP-ROW
                MOVE WS-SORT-ROW(WS-J + 1) TO WS-SORT-ROW(WS-J)
                MOVE WS-TEMP-ROW TO WS-SORT-ROW(WS-J + 1)
            END-IF
        END-PERFORM
    END-PERFORM

*> * Display sorted array
    DISPLAY "Sorted Array Contents:"
    DISPLAY "--------------------"
    PERFORM VARYING WS-INDEX FROM 1 BY 1
        UNTIL WS-INDEX > WS-SORT-MAX
        DISPLAY "Element " WS-INDEX ": " WS-SORT-ROW(WS-INDEX)
    END-PERFORM

    STOP RUN.
    