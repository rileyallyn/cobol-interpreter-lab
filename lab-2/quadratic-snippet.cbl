       /*code from https://www.quora.com/What-is-a-COBOL-program-that-will-solve-a-quadratic-equation

       IDENTIFICATION DIVISION.

       PROGRAM-ID. QuadraticSolver.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       77 a PIC S9(5)V9(5) COMP-3.

       77 b PIC S9(5)V9(5) COMP-3.

       77 c PIC S9(5)V9(5) COMP-3.

       77 discriminant PIC S9(5)V9(5) COMP-3.

       77 root1 PIC S9(5)V9(5) COMP-3.

       77 root2 PIC S9(5)V9(5) COMP-3.

       77 square-root-discriminant PIC S9(5)V9(5) COMP-3.

       PROCEDURE DIVISION.

       DISPLAY "Enter the coefficients of the quadratic equation (a, b, c): ".

       ACCEPT a

       ACCEPT b

       ACCEPT c

       COMPUTE discriminant = (b ** 2) - (4 * a * c)

       IF discriminant > 0

       COMPUTE square-root-discriminant = FUNCTION SQRT(discriminant)

       COMPUTE root1 = (-b + square-root-discriminant) / (2 * a)

       COMPUTE root2 = (-b - square-root-discriminant) / (2 * a)

       DISPLAY "The equation has two distinct real roots: "

       DISPLAY "Root 1: " root1

       DISPLAY "Root 2: " root2

       ELSE IF discriminant = 0

       COMPUTE root1 = -b / (2 * a)

       DISPLAY "The equation has one real root: "

       DISPLAY "Root: " root1

       ELSE

       DISPLAY "The equation has no real roots."

       STOP RUN.