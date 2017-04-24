       identification division.
       program-id. ASGN06RB.
       AUTHOR.     RYAN BROOKS.
      * ASSIGNMENT 6 - MATH CALCULATIONS
       environment division.
       configuration section.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
                   
       data division.
       working-storage section.
       01  WS-FN-FL-WORKING-STORAGE.
      * Add & Modify working storage elements as needed.  Some
      * required elements are coded incorrectly, others are missing.
           05 WS-INPUT-1           PIC 9(02)    VALUE 0.
           05 WS-INPUT-2           PIC 9(02)    value 0.
           05 WS-AVG               PIC 99.9     value 0.
           05 WS-AVG-RND           PIC 9(02)    value 0.
           05 WS-AVG-INT           PIC 9(03)    value 0.
           05 WS-USER-AGE          PIC 9(02)    VALUE 0.
           05 WS-WEIGHT-IN         PIC 9(03)    VALUE 0.
           05 WS-WEIGHT-MET        PIC 99.9     VALUE 0.
           05 WS-INCH-HGT-IN       PIC 9(03)    VALUE 0.
           05 WS-CM-HGT            PIC 99.9     VALUE 0.
           05 WS-BMI               PIC 99.99    VALUE 0.
           05 WS-BMI-MET           PIC 99.99    VALUE 0.
           05 WS-LAST-LEAP-YEAR    PIC 9(04)    VALUE 2016.
           05 WS-NEXT-LEAP-YEAR-1  PIC 9(04)    VALUE 0.
           05 WS-NEXT-LEAP-YEAR-2  PIC 9(04)    VALUE 0.
           05 WS-NEXT-LEAP-YEAR-3  PIC 9(04)    VALUE 0.
           05 WS-NEXT-LEAP-YEAR-4  PIC 9(04)    VALUE 0.
           05 WS-UNTIL-LEAP-YEAR   PIC 9(01)    VALUE 0.
           05 WS-TILL-CENTURY      PIC 9(02)    VALUE 0.
           05 WS-YEARS-OLD         PIC 9(02)    VALUE 0.
           05 WS-END               PIC X(01)    VALUE SPACE.
           
       01  WS-TIME.
           05 WS-TIME-HH           PIC X(02).
           05 WS-TIME-MM           PIC X(02).
           05 WS-TIME-SS           PIC X(02).
           05 WS-TIME-HS           PIC X(02).
           
       01  WS-TODAYS-DATE.
           05 WS-8-DATE-YEAR       PIC 9(04).
              77 WS-8-DATE-CC      PIC 9(02).
              77 WS-8-DATE-YY      PIC 9(02).
           77 WS-8-DATE-MM         PIC X(02).
           77 WS-8-DATE-DD         PIC X(02).
        
       PROCEDURE division.
       BEGIN.
           display "START ASGN06RB FOR Ryan Brooks".
           ACCEPT WS-TODAYS-DATE from date yyyymmdd.
           DISPLAY "PROGRAM EXECUTION DATE: " WS-TODAYS-DATE.
           ACCEPT WS-TIME from time.
           DISPLAY "PROGRAM EXECUTION START TIME: " WS-TIME-HH
                   ":" WS-TIME-MM.
           display "******".
      * COMPLETE THE PROCEDURE DIVISION TO PERFORM THESE CALCULATIONS.
      * THIS WILL ALSO INCLUDE THE COMPLETION OF SOME WORKING STORAGE
      * ELEMENTS, AND THE CREATION OF SOME ELEMENTS. 
        
      * CALCULATE THE THE AVERAGE, UP TO 2 DECIMAL PLACES, OF 
      * TWO 2 DIGIT NUMBERS, ONCE USING THE ROUNDED STATEMENT, 
      * THE 2ND WITHOUT ROUNDING.
           DISPLAY "CALCULATE THE AVG OF 2 WHOLE NUMBERS".
           DISPLAY "Enter a two-digit number".
           ACCEPT WS-INPUT-1.
           DISPLAY "Enter another two-digit number".
           ACCEPT WS-INPUT-2.
           COMPUTE WS-AVG-INT = WS-INPUT-1 + WS-INPUT-2.
           compute WS-AVG = WS-AVG-INT / 2.
           DISPLAY "The Average of " WS-INPUT-1 " and " WS-INPUT-2 
                   " is " WS-AVG.
           compute WS-AVG-RND rounded = WS-AVG-INT / 2.
           DISPLAY "The Rounded Average of " WS-INPUT-1 " and " 
           WS-INPUT-2 " is " WS-AVG-RND.           
           DISPLAY "******"
      * 2016 WAS A LEAP YEAR. CALCULATE AND DISPLAY EACH OF THE 
      * NEXT 4 LEAP YEARS.
           display "CALCULATE LEAP YEARS:"
           COMPUTE WS-NEXT-LEAP-YEAR-1 = WS-LAST-LEAP-YEAR + 4.
           DISPLAY "NEXT LEAP YEAR: " WS-NEXT-LEAP-YEAR-1.
           COMPUTE WS-NEXT-LEAP-YEAR-2 = WS-NEXT-LEAP-YEAR-1 + 4.
           DISPLAY "2ND LEAP YEAR FROM NOW: " WS-NEXT-LEAP-YEAR-2.
           COMPUTE WS-NEXT-LEAP-YEAR-3 = WS-NEXT-LEAP-YEAR-2 + 4.
           DISPLAY "3RD LEAP YEAR FROM NOW: " WS-NEXT-LEAP-YEAR-3.
           COMPUTE WS-NEXT-LEAP-YEAR-4 = WS-NEXT-LEAP-YEAR-3 + 4.
           DISPLAY "4TH LEAP YEAR FROM NOW: " WS-NEXT-LEAP-YEAR-4.
           
      * DISPLAY FOR THE USER HOW MANY YEARS UNTIL THE NEXT LEAP YEAR
           compute WS-UNTIL-LEAP-YEAR = WS-NEXT-LEAP-YEAR-1 - 
                   WS-8-DATE-YEAR
           display "The next leap year will be in " WS-UNTIL-LEAP-YEAR
                   " years".
           
      * ASK THE USER FOR THEIR AGE.  DISPLAY HOW OLD THEY
      * WILL BE FOR THE NEXT 2 LEAP YEARS.
           DISPLAY "What is your age?".
           accept WS-USER-AGE.           
           compute WS-YEARS-OLD = WS-USER-AGE + WS-UNTIL-LEAP-YEAR.
           DISPLAY "YOU WILL BE: " WS-YEARS-OLD " ON NEXT LEAP YEAR".
           compute WS-YEARS-OLD = WS-YEARS-OLD + 4.
           DISPLAY "YOU WILL BE: " WS-YEARS-OLD " IN 2 LEAP YEARS".
           
      * DISPLAY FOR THE USER HOW MANY YEARS UNTIL THEY TURN 100
           compute WS-TILL-CENTURY = 100 - WS-USER-AGE.
           display "You have " WS-TILL-CENTURY " years until 100".
           display "******".
           
      * PROMPT THE USE FOR THEIR WEIGHT IN POUNDS AND HEIGHT IN INCHES.
      * CALCULATE AND DISPLAY THE USERS BMI TO TWO DECIMAL PLACES.
      * LOOK UP FORMULA ON WEB AND USE COBOL COMPUTE STATEMENT TO 
      * TO CALCULATE THE BMI.
           display "ENTER YOUR WEIGHT IN LBS: ".
           ACCEPT WS-WEIGHT-IN.
           display "ENTER YOUR HEIGHT IN INCHES: ".
           ACCEPT WS-INCH-HGT-IN.
           COMPUTE WS-BMI = WS-WEIGHT-IN * 703 / WS-INCH-HGT-IN ** 2
           DISPLAY "YOUR BMI IS: " WS-BMI.
           display "******".
           
           compute WS-CM-HGT = WS-INCH-HGT-IN * 2.54.
           display "You are " WS-CM-HGT "cm tall".
           compute WS-WEIGHT-MET = WS-WEIGHT-IN * 0.453592.
           display "You weigh " WS-WEIGHT-MET "kg".
      *    compute WS-BMI-MET = WS-WEIGHT-MET / WS-CM-HGT ** 2.
      *    display "Your metric BMI is: " WS-BMI-MET.
           display "******".
           
      * FOR 5 EXTRA POINTS, CONVERT THE INPUT WEIGHT & HEIGHT TO 
      * METRIC MEASUREMENTS, DISPLAY THEM AND METRIC BMI.
           
           DISPLAY "END OF ASSIGNMENT 06".
           DISPLAY "Press Enter to exit".
           ACCEPT WS-END.
           GOBACK.