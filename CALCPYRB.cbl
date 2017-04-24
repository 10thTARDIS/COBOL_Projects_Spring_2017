000100 IDENTIFICATION DIVISION.                                          
000200 PROGRAM-ID.    CALCPYRB.                                                        
000300 AUTHOR.        RYAN BROOKS                                         
000400**REMARKS.                                                                        
000500*	******************************************************************              
000600*	* -READ IN PAYROLL FILE AND USE THE HOURS WORKED AND RATE TO CALCULATE:            
000700*	*    GROSS PAY            (RATE X HOURS, OVER 40 HOURS IS 1.5X RATE)            
000800*	*    STATE TAX            (6.00% X GROSS)                                       
000900*	*    MEDICARE/SOC SEC TAX (7.65% X GROSS)                                       
001000*	*    LOCAL TAX            (1.00% X GROSS)                                       
001100*	*    FEDERAL TAX                                                                
001200*	*      IF DEDUCTIONS = 0  (20.00% X GROSS)                                      
001300*	*      IF DEDUCTIONS = 1  (18.00% X GROSS)                                      
001400*	*      IF DEDUCTIONS = 2  (15.00% X GROSS)                                      
001500*	*      IF DEDUCTIONS = 3  (12.00% X GROSS)                                      
001600*	*      IF DEDUCTIONS = 4  (10.00% X GROSS)                                      
001700*	*    NET PAY              (GROSS - TAXES)                                       
001800*	*                                                                               
001900*	* -CREATE NEW PAYROLL FILE (PAYROLL-OUT) WITH ABOVE CALCULATIONS COMPLETED.                  
002000*	*                                                                               
002100*	* -PROGRAM SHOULD DISPLAY FIVE COUNTS:                                           
002200*	*   RECORDS READ (COUNT ONE RECORD FOR                                          
002300*	*   RECORDS WRITTEN                                                             
002400*	*   FILE TOTAL GROSS AMOUNT (FORAMATTED WITH $ SIGNS)                           
002500*	*   FILE TOTAL NET AMOUNT (FORAMATTED WITH $ SIGNS)
002600*	*   FILE TOTAL WITHHELD (FORMATTED WITH $ SIGNS, (EQUALS GROSS - NET))                             
002700*	*************************************************************************          
002800 ENVIRONMENT DIVISION.                                                            
002900 CONFIGURATION SECTION.                                                           
003000 INPUT-OUTPUT SECTION.                                                            
003100 FILE-CONTROL.                                                                    
003200       SELECT PAYROLL-IN  ASSIGN TO 
003300             "C:\Users\Ryan\Desktop\PAYROLL1.txt"
003400         organization is line sequential
003500         file status is WS-PAYROLL-IN-STATUS.
003600       SELECT PAYROLL-OUT ASSIGN TO 
003700             "C:\Users\Ryan\Desktop\PAYROLL2.txt"
003800         organization is line sequential
003900         file status is WS-PAYROLL-OUT-STATUS.             
004000                                                                                  
004100 DATA DIVISION.                                                                   
004200  FILE SECTION.                                                                   
004300                                                                                  
004400 FD  PAYROLL-IN                                                                   
004500     RECORDING MODE IS F                                                        
004600     DATA RECORD IS PAYROLL-REC-IN.                                                  
004700 01  PAYROLL-REC-IN.                                                                  
004800     05  PAYROLL-PAY-DATE         PIC X(06).                                    
004900     05  PAYROLL-EMP-NUMBER       PIC X(06).                                    
005000     05  PAYROLL-EMP-HOURS        PIC 9(02)V99.                                 
005100     05  PAYROLL-EMP-RATE         PIC 9(02)V99.                                 
005200     05  PAYROLL-EMP-DEDUCTIONS   PIC X(01).                                    
005300                                                                                  
005400 FD  PAYROLL-OUT                                                                  
005500     RECORDING MODE IS F                                                        
005600     DATA RECORD IS PAYROLL-REC-OUT.                                                  
005700 01  PAYROLL-REC-OUT.                                                                
005800     05  PAYROLL-PAY-DATE         PIC X(06).                                    
005900     05  PAYROLL-EMP-NUMBER       PIC X(06).                                    
006000     05  PAYROLL-EMP-HOURS        PIC 9(02)V99.                                 
006100     05  PAYROLL-EMP-RATE         PIC 9(02)V99.                                 
006200     05  PAYROLL-EMP-DEDUCTIONS   PIC X(01).
006300     05  PAYROLL-EMP-CALCULATIONS.                                           
006400         10 PAYROLL-EMP-GROSS     PIC 9(04)V99.                           
006500         10 PAYROLL-EMP-SSN-MED   PIC 9(03)V99.                            
006600         10 PAYROLL-EMP-STATE     PIC 9(03)V99.                            
006700         10 PAYROLL-EMP-LOCAL     PIC 9(03)V99.                            
006800         10 PAYROLL-EMP-FED       PIC 9(03)V99.                            
006900         10 PAYROLL-EMP-NET       PIC 9(04)V99.                            
007000                                                                                  
007100 WORKING-STORAGE SECTION.                                                         
007200* ADD THE WORKING STORAGE FIELDS NEEDED: 
007300*	   RECORDS READ
007400*	   RECORDS WRITTEN
007500*	   TOTAL NET
007600*	   TOTAL GROSS
007700*	   TOTAL WITH HELD              
007800*	   FILE STATUSES (WS-PAYROLL-IN-STATUS, WS-PAYROLL-OUT-STATUS)
007900*	   ANY 88 LEVELS YOU WOULD LIKE TO USE            
008000*	******************************************************************              
008100  01 WS-RECORDS-READ.
008200     05  PAYROLL-PAY-DATE-READ       PIC X(06).              
008300 	   05  PAYROLL-EMP-NUMBER-READ     PIC X(06).                      
008400 	   05  PAYROLL-EMP-HOURS-READ      PIC 9(02)V99.                  
008500 	   05  PAYROLL-EMP-RATE-READ       PIC 9(02)V99.                 
008600 	   05  PAYROLL-EMP-DEDUCTIONS-READ PIC 9(01).
008700  01 WS-RECORD-COUNT-IN              pic 99 VALUE 0.
008800  01 WS-RECORDS-WRITTEN.
008900     05 PAYROLL-EMP-GROSS-WRITE      PIC 9(04)V99.           
009000     05 PAYROLL-EMP-SSN-MED-WRITE    PIC 9(03)V99.
009100     05 PAYROLL-EMP-STATE-WRITE      PIC 9(03)V99.
009200     05 PAYROLL-EMP-LOCAL-WRITE      PIC 9(03)V99.   
009300     05 PAYROLL-EMP-FED-WRITE        PIC 9(03)V99.     
009400     05 PAYROLL-EMP-NET-WRITE        PIC 9(04)V99.
009500  01 WS-RECORD-COUNT-OUT             pic 99 VALUE 0.
009600  01 WS-SINGLE-WITHHELD              pic 9(06)v99.
009700* Unformatted fields
009800  01 WS-TOTAL-GROSS               pic 9(06)V99 VALUE 0.
009900  01 WS-TOTAL-NET                 pic 9(06)V99 VALUE 0.
010000  01 WS-TOTAL-WITHHELD            pic 9(06)V99 VALUE 0.
010100* Formatted fields
010200  01 WS-TOTAL-GROSS-FORMAT        PIC $ZZZ,ZZZ.99.
010300  01 WS-TOTAL-NET-FORMAT          PIC $ZZZ,ZZZ.99.
010400  01 WS-TOTAL-WITHHELD-FORMAT     PIC $ZZZ,ZZZ.99.
010500* File status
010600  01 WS-PAYROLL-IN-STATUS         PIC X(02).
010700     88 PAYROLL-IN-SUCCESSFUL        VALUE "00".
010800     88 END-OF-PAYROLL-IN            VALUE "10".
010900     88 INVALID-VALUE-IN-FILE        VALUE "11" THRU "99".
011000  01 WS-PAYROLL-OUT-STATUS        pic X(02).
011100     88 GOOD-PAYROLL-FILE-WRITE      VALUE "00".
011200  01 ANY-KEY                      pic X(01).
011300  
011400                                                                                  
011500 PROCEDURE DIVISION.  
011600  
011700     DISPLAY "CALCPYRB FOR RYAN BROOKS".
011800     
011900* INITIALIZE:  OPEN FILES, PERFORM 1ST READ.
012000     PERFORM 1000-INITIALIZE.
012200     PERFORM 2000-READ-PAYROLL.
012400     
012500*    PERFORM LOOP TO:
012600*	    -DO CALCULATIONS
012700*	    -UPDATE TOTALS
012800*     -MOVE INPUT FIELDS AND CALCULATED FIELDS TO OUTPUT-RECORD   
012900*	    -WRITE OUT NEW RECORD
013000*	    -READ NEXT RECORD
013100      
013200      perform until END-OF-PAYROLL-IN
013300         perform 3000-CALCULATIONS
013400         perform 4000-UPDATE-TOTALS
013500         perform 5000-WRITE-RECORDS
013600         perform 2000-READ-PAYROLL
013700      END-PERFORM.
013800      
013900      perform 6000-MOVE-RECORDS
014000         
014100      
014200*	   DISPLAY PROGRAM TOTALS
014300     display "RECORDS READ:                " WS-RECORD-COUNT-IN.
014400     display "RECORDS WRITTEN:             " WS-RECORD-COUNT-OUT.
014500     DISPLAY "TOTAL GROSS AMOUNT: " WS-TOTAL-GROSS-FORMAT.
014600     DISPLAY "TOTAL NET AMOUNT:   " WS-TOTAL-NET-FORMAT.
014700     DISPLAY "TOTAL WITHHELD:     " WS-TOTAL-WITHHELD-FORMAT.
014800     
014900*	   CLOSE FILES
015000     close PAYROLL-IN
015100           PAYROLL-OUT.
015300     display "Press any key to exit.".
015400         accept ANY-KEY.
015500     
015600     GOBACK.                                                            
015700 
015800 1000-INITIALIZE.
015900     OPEN INPUT PAYROLL-IN.
016000     OPEN OUTPUT PAYROLL-OUT.
016100     MOVE ZEROES to WS-RECORDS-READ
016200                    WS-RECORDS-WRITTEN.
016300     
016400 2000-READ-PAYROLL.
016500* Read the payroll file in and populate the Working Section
016600  READ PAYROLL-IN INTO WS-RECORDS-READ
016700       AT END
016800         DISPLAY "END OF PAYROLL FILE"
016900       NOT AT END
017000         ADD 1 TO WS-RECORD-COUNT-IN.
017200 
017300 3000-CALCULATIONS.
017400* Perform the required calculations
017500* ACCEPT PAYROLL-EMP-GROSS-WRITE
017600      IF PAYROLL-EMP-HOURS-READ < 40 
017700          COMPUTE PAYROLL-EMP-GROSS-WRITE =
017800          PAYROLL-EMP-HOURS-READ * PAYROLL-EMP-RATE-READ
017900      ELSE 
018000      IF PAYROLL-EMP-HOURS-READ > 40
018100          COMPUTE PAYROLL-EMP-GROSS-WRITE =
018200                  (40 * PAYROLL-EMP-RATE-READ) +
018300                  ((PAYROLL-EMP-HOURS-READ - 40) + 
018400                  (1.5 * PAYROLL-EMP-RATE-READ))
018500      END-IF   
018600     COMPUTE PAYROLL-EMP-STATE-WRITE   = 
018700                                PAYROLL-EMP-GROSS-WRITE *.06.
018800     COMPUTE PAYROLL-EMP-SSN-MED-WRITE = 
018900                                PAYROLL-EMP-GROSS-WRITE * .0765.
019000     COMPUTE PAYROLL-EMP-LOCAL-WRITE   = 
019100                                PAYROLL-EMP-GROSS-WRITE *  .01.
019200*    ACCEPT PAYROLL-EMP-FED-WRITE 
019300     IF PAYROLL-EMP-DEDUCTIONS-READ = 0
019400          COMPUTE PAYROLL-EMP-FED-WRITE =
019500                         PAYROLL-EMP-GROSS-WRITE * .20
019600     IF PAYROLL-EMP-DEDUCTIONS-READ = 1 
019700           COMPUTE PAYROLL-EMP-FED-WRITE =
019800                         PAYROLL-EMP-GROSS-WRITE * .18
019900      ELSE
020000     IF PAYROLL-EMP-DEDUCTIONS-READ = 2
020100           COMPUTE PAYROLL-EMP-FED-WRITE =
020200                         PAYROLL-EMP-GROSS-WRITE * .15
020300      ELSE 
020400     IF PAYROLL-EMP-DEDUCTIONS-READ = 3
020500           COMPUTE PAYROLL-EMP-FED-WRITE =
020600                         PAYROLL-EMP-GROSS-WRITE * .12
020700      ELSE
020800     IF PAYROLL-EMP-DEDUCTIONS-READ = 4
020900           COMPUTE PAYROLL-EMP-FED-WRITE =
021000                         PAYROLL-EMP-GROSS-WRITE * .10
021100      ELSE
021200    END-IF
021300    END-IF
021400    END-IF.
021500    compute WS-SINGLE-WITHHELD =
021600            PAYROLL-EMP-GROSS-WRITE - PAYROLL-EMP-STATE-WRITE
021700            - PAYROLL-EMP-SSN-MED-WRITE - PAYROLL-EMP-LOCAL-WRITE
021800            - PAYROLL-EMP-FED-WRITE.
021900    compute PAYROLL-EMP-NET-WRITE = PAYROLL-EMP-GROSS-WRITE
022000                                  - WS-SINGLE-WITHHELD.
022200 
022300 4000-UPDATE-TOTALS.
022400* Update the totals
022500    compute WS-TOTAL-GROSS = WS-TOTAL-GROSS + 
                                   PAYROLL-EMP-GROSS-WRITE.
022600    compute WS-TOTAL-WITHHELD = WS-TOTAL-WITHHELD +
          WS-SINGLE-WITHHELD.
022700    compute WS-TOTAL-NET = WS-TOTAL-GROSS - WS-TOTAL-WITHHELD.
022800 
022900 5000-WRITE-RECORDS.
023000* Write the records to file.
023100  WRITE PAYROLL-REC-OUT FROM WS-RECORDS-WRITTEN.
023200     if GOOD-PAYROLL-FILE-WRITE 
023300        add 1 to WS-RECORD-COUNT-OUT
023500     else  
023600        display "BAD WRITE - FILE STATUS: " 
023700          WS-PAYROLL-OUT-STATUS.
023800
023900 6000-MOVE-RECORDS.
024000     MOVE WS-TOTAL-GROSS TO WS-TOTAL-GROSS-FORMAT.
024100     MOVE WS-TOTAL-NET TO WS-TOTAL-NET-FORMAT.
024200     MOVE WS-TOTAL-WITHHELD TO WS-TOTAL-WITHHELD-FORMAT.