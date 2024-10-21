# Language Proposal

### 1. Hello World
- source used: [Cobol Emulator](https://www.jdoodle.com/execute-cobol-online)
- display the text "Hello World!" in the terminal
- https://www.tutorialspoint.com/cobol/cobol_program_structure.htm this website has useful information about the structure of the cobol language 
- the Identification Division is mandatory for all programs and sub divisions have unique sentences within them and determine what is done with variables or IO in this example

    #### Input
        there is no input for this example
    #### Output
        the string "Hello World!" is sent to output via the included DISPLAY function

### 2. Quadratic Function
- Code altered from [Marco Biagini](https://www.quora.com/What-is-a-COBOL-program-that-will-solve-a-quadratic-equation)
- Solves a quadratic equation of the form **ax^2 + bx + c = 0**

    #### Input
      Coefficients are a fixed value with a = 1, b = 5, c = 6
    #### Output
      EQUATION: (1x^2) + 5x + 6 = 0
 
      The equation has two distinct real roots: 
      Root 1: -02.00
      Root 2: -03.00

### 3. Integer Sort

### 4. Language Features
- #### Identifiers
    All variables are declared within DATA DIVISION. Variable names accept letters(A-Z), digits(0-9), and hyphens(-). In the syntax of:
    
        <level-number> <variable-name>
            [PIC/PICTURE <data-type(variable-length>)]
            [VALUE <literal-value>]
    Statements in [ ] are optional. Example:

        01 INT-VAR      PIC 9(03 VALUE 123).

- #### Special words
    - Note: Keywords are not case sensitive but good practice is to only use uppercase
    - Division statements divide the structure of the code. Some examples are PROGRAM-ID, DATA DIVISION, WORKING STORAGE DIVISION, PROCEDURE DIVISION, and STOP RUN.
    - DISPLAY outputs data to user
    - MOVE sends data from one variable to another
    - ACCEPT allows for user input to go into a variable
    - Supports boolean values TRUE and FALSE


- #### Literal
        *> This is a comment in COBOL
            
    Non-numeric

            DISPLAY "This is a string in COBOL".
            DISPLAY 'This is a string in COBOL'.

    Numeric

            Digits 0 - 9
            Positive numbers: +10
            Negative numbers: -10
            Decimal: 10.00

- #### Math Operators
    Supports additon (+), subraction (-), multiplication (*), and division (/) and exponentation (**). Equations begin with COMPUTE.

            COMPUTE total = 1 + 2

- #### Relational Operators
    Supports =, >, <, >=, <=, NOT =

            IF a > b

- #### Delimiters
    Section headers and verb statements should end with a separator period (.)

            PROCEDURE DIVISION.
                MOVE "Hello" TO VAR1.

    Commas (,) can separate two variables

            ADD 3 TO VAR1, VAR2.

- #### Lists
    Supports arrays (known in COBOL as tables). Declared in DATA DIVISION. In the syntax of:

        01 <table-name>.
          02 <variable-name>    [PIC <data-type(length1)>]
                                OCCURS <int1> [TO <int2>] TIMES
                                [DEPENDING ON]
                                [DESCENDING|ASCENDING KEY IS <key_var>]
                                [INDEXED BY <index_name>] 
    Statements in [ ] are optional. Example:

        01  Students
            03 Student-grades  PIC 9(03) OCCURS 6 TIMES.                                                 

- #### Loops
    Loops begin with a PERFORM statement and end with END-PERFORM. Note that a separator period (.) should not used within a PERFORM block.

            PERFORM <conditional statement>
                <statements-block>
            END-PERFORM.

- #### Branching
    Uses IF/END-IF and ELSE statements and nested statements. Note that a separator period (.) should not used within an IF statement.
    
            IF <condition>
                DISPLAY "Option 1"
            ELSE
                DISPLAY "Option 2"
            END-IF.

` Language Feature Sources `

- [tutorialspoint.com](https://www.tutorialspoint.com/cobol/cobol_basic_syntax.htm)

- [mainframestechhelp.com](https://www.mainframestechhelp.com/tutorials/cobol/)
