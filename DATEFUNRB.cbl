       IDENTIFICATION DIVISION.
       program-id. DATEFUNRB.
       AUTHOR.     RYAN BROOKS.
      *DETERMINE NUMBER OF DAYS UNTIL END OF SEMESTER
       data division.
       working-storage section.
       01 WS-START-DATE        pic 9(08)   VALUE 20170116.
       01 WS-END-DATE          PIC 9(08)   VALUE 20170513.
       01 WS-CURRENT-DATE      pic 9(08)   VALUE 0.
       01 WS-DAYS-FROM-START   pic 9(03)   VALUE 0.
       01 WS-DAYS-FROM-END     pic 9(03)   VALUE 0.
       01 ANY-KEY              pic X.
       
       procedure division.
       
      * GET DATE AND PERFORM CALCULATIONS
       MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE.
       compute WS-DAYS-FROM-END = function integer-of-date (WS-END-DATE)
                           - function integer-of-date (WS-CURRENT-DATE).
       compute WS-DAYS-FROM-START = function integer-of-date
                                    (WS-CURRENT-DATE)
                           - function integer-of-date (WS-START-DATE).
       
      * DISPLAY CALCULATED VALUES
       display "RYAN BROOKS".
       display "RUN ON " WS-CURRENT-DATE.
       display "THERE ARE " WS-DAYS-FROM-END " DAYS UNTIL THE END OF"
               " THE SEMESTER".
       display "THERE HAVE BEEN " WS-DAYS-FROM-START " DAYS SINCE THE"
               " SEMESTER STARTED".
       display " ".
       display "PRESS ANY KEY TO EXIT".
       accept ANY-KEY.
       
           goback.
           
       end program DATEFUNRB.
