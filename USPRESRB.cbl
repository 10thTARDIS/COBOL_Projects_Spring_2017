000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID. USPRESRB.                                            
000300* TABLE STORAGE AND ACCESS.   
000400* READ A PRESIDENT FILE AND LOAD THE PRESIDENT TABLE.
000500* ACCEPT A PRESIDENT NUMBER AND DISPLAY THE NAME                       
000600 ENVIRONMENT DIVISION.                                                    
000700 CONFIGURATION SECTION.                                                   
000800 INPUT-OUTPUT SECTION.                                                    
000900 FILE-CONTROL.  
001000     SELECT US-PRES-IN                                            
001100       ASSIGN TO "C:\Users\Ryan\Desktop\USPRES.DAT"    
001200       ORGANIZATION IS LINE SEQUENTIAL                                    
001300       FILE STATUS IS PRES-FILE-STATUS.                                                               
001400 DATA DIVISION.                                                           
001500 FILE SECTION. 
001600 FD  US-PRES-IN                                                   
001700     RECORDING MODE IS F                                                  
001800     DATA RECORD IS PRES-RECORD-IN.                                    
001900                                                                          
002000 01 PRES-RECORD-IN          PIC X(27).  
002100                                                                                
002200 WORKING-STORAGE SECTION. 
002300 01 PRES-FILE-STATUS        PIC X(02).
002400
002500 01 WS-PRES-TABLE.
002600    05 WS-PRES-ENTRY OCCURS 45 TIMES.               
002700       10 WS-PRES-INDEX     PIC X(02).
002800       10 WS-PRES-NAME      PIC X(25).
002900 
003000 01 WS-PRES-NUMBER          PIC 9(02)    VALUE ZERO.
003100    88 VALID-PRES                        VALUE 01 THRU 45.
003200 01 WS-PRES-READ-SWITCH     PIC 9        VALUE 1.
003300    88 END-OF-PRES                       VALUE 0.
003400 01 WS-PRES-SUB             PIC 9(02).
003500    88 VALID-SUB                         VALUE 1 THRU 45.
003600    88 INVALID-SUB                       VALUE 0, 46 THRU 99.
003700 01 USRINPUT                PIC X(02).
003800    88 LISTALL                           VALUE "A".
003900    88 QUIT                              VALUE "X".
004000 01 LISTINDEX               pic 9(02)    VALUE ZERO.
004100    
004200 
004300 PROCEDURE DIVISION.
004400 
004500 0000-DRIVER.
004600     DISPLAY "USPRESRB BY RYAN BROOKS".
004700     OPEN INPUT US-PRES-IN.
004800     
004900     move zeros to WS-PRES-TABLE.    
005000     
005100     PERFORM 1000-READ-PRES-FILE VARYING WS-PRES-SUB
005200         FROM 1 BY 1 UNTIL END-OF-PRES.
005300         
005400     perform until USRINPUT = "X"
005500          PERFORM 2000-SELECT-PRESIDENT
005600          
005700          display " "
005800          display "You picked " WS-PRES-INDEX(WS-PRES-NUMBER)
005900                  ", President " WS-PRES-NAME(WS-PRES-NUMBER)
006000          display " "
006100          set WS-PRES-NUMBER to 0
006200     END-PERFORM.
006300     
006400     GOBACK.
006500  
006600 1000-READ-PRES-FILE.
006700     READ US-PRES-IN INTO WS-PRES-ENTRY (WS-PRES-SUB)
006800       AT END MOVE 0 TO WS-PRES-READ-SWITCH.
006900       
007000 2000-SELECT-PRESIDENT.
007100     PERFORM UNTIL VALID-PRES
007200          display " "
007300          DISPLAY "Which president (01-45)?  "
007400                  "X exits, A lists all. "
007500          
007600          ACCEPT USRINPUT
007700             if USRINPUT = "A" or "a"
007800               perform 3000-DISPLAY-ALL
007900             ELSE IF USRINPUT = "X" or "x"
008000                 goback
008100             ELSE IF USRINPUT is numeric
008200                 move USRINPUT to WS-PRES-NUMBER
008300                 if not VALID-PRES
008400                     display " "
008500                     display "Please enter a valid input"
008600                 end-if
008700             ELSE
008800                 display " "
008900                 display "Please enter a valid input"
009000             end-if
009100                          
009200      END-PERFORM.
009300  
009400 3000-DISPLAY-ALL.
009500     set LISTINDEX to 0.
009600     perform until LISTINDEX = 45
009700         display "President " WS-PRES-NAME(LISTINDEX + 1)
009800         ADD 1 TO LISTINDEX
009900     END-PERFORM.   
010000        