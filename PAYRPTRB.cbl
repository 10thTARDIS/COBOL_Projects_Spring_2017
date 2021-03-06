000100 IDENTIFICATION DIVISION.                                          
000200 PROGRAM-ID.    PAYRPTRB.                                                   
000300 AUTHOR.        RYAN BROOKS                                         
000400          
000500 ENVIRONMENT DIVISION.                                                            
000600 CONFIGURATION SECTION.                                                           
000700 INPUT-OUTPUT SECTION.                                                            
000800 FILE-CONTROL.                                                                    
000900       SELECT PAYROLL-IN  ASSIGN TO 
001000             "C:\Users\Ryan\Desktop\PAYROLLDT.txt"
001100         organization is line sequential
001200         file status is WS-PAYROLL-IN-STATUS.
001300       SELECT PAYROLL-OUT ASSIGN TO 
001400             "C:\Users\Ryan\Desktop\PAYROLLOUT.txt"
001500         organization is line sequential
001600         file status is WS-PAYROLL-OUT-STATUS.
001700       SELECT PAYROLL-REPORT
001800         ASSIGN To "C:\Users\Ryan\Desktop\PAYROLLREPORT.rpt"
001900         organization is LINE sequential
002000         file status is WS-SHAPE-REPORT-STATUS.
002100                                                                                  
002200 DATA DIVISION.                                                                   
002300  FILE SECTION.                                                                   
002400                                                                                  
002500 FD  PAYROLL-IN                                                                   
002600     RECORDING MODE IS F                                                        
002700     DATA RECORD IS PAYROLL-REC-IN.                                                  
002800 01  PAYROLL-REC-IN.                                                                  
002900     05  PAYROLL-PAY-DATE.
003000         10 PAY-MONTH             PIC X(02).
003100         10 PAY-DAY               PIC X(02).
003200         10 PAY-YEAR              PIC X(02).                           
003300     05  PAYROLL-EMP-NUMBER       PIC X(06).                                    
003400     05  PAYROLL-EMP-HOURS        PIC 9(02)V99.                                 
003500     05  PAYROLL-EMP-RATE         PIC 9(02)V99.                                 
003600     05  PAYROLL-EMP-DEDUCTIONS   PIC X(01).                                    
003700                                                                                  
003800 FD  PAYROLL-OUT                                                                  
003900     RECORDING MODE IS F                                                        
004000     DATA RECORD IS PAYROLL-REC-OUT.                                                  
004100 01  PAYROLL-REC-OUT.                                                                
004200     05  PAYROLL-PAY-DATE         PIC X(06).                                    
004300     05  PAYROLL-EMP-NUMBER       PIC X(06).                                    
004400     05  PAYROLL-EMP-HOURS        PIC 9(02)V99.                                 
004500     05  PAYROLL-EMP-RATE         PIC 9(02)V99.                                 
004600     05  PAYROLL-EMP-DEDUCTIONS   PIC X(01).
004700     05  PAYROLL-EMP-CALCULATIONS.                                           
004800         10 PAYROLL-EMP-GROSS     PIC 9(04)V99.                           
004900         10 PAYROLL-EMP-SSN-MED   PIC 9(03)V99.                            
005000         10 PAYROLL-EMP-STATE     PIC 9(03)V99.                            
005100         10 PAYROLL-EMP-LOCAL     PIC 9(03)V99.                            
005200         10 PAYROLL-EMP-FED       PIC 9(03)V99.                            
005300         10 PAYROLL-EMP-NET       PIC 9(04)V99.
005400 01 SHAPE-REC-OUT              PIC X(39).                          
005500 
005600 FD  PAYROLL-REPORT
005700     RECORDING MODE IS F
005800     DATA RECORD IS PAYROLL-REPORT-RECORD.                  
005900 01 PAYROLL-REPORT-RECORD        PIC X(133).          
006000                                                                                                                                  
006100 WORKING-STORAGE SECTION.                                                         
006200  01 WS-RECORDS-READ.
006300     05  PAYROLL-PAY-DATE-READ       PIC X(06).              
006400 	   05  PAYROLL-EMP-NUMBER-READ     PIC X(06).                      
006500 	   05  PAYROLL-EMP-HOURS-READ      PIC 9(02)V99.                  
006600 	   05  PAYROLL-EMP-RATE-READ       PIC 9(02)V99.                 
006700 	   05  PAYROLL-EMP-DEDUCTIONS-READ PIC 9(01).
006800  01 WS-RECORD-COUNT-IN              pic 99 VALUE 0.
006900  01 WS-RECORDS-WRITTEN.
007000     05 PAYROLL-EMP-GROSS-WRITE      PIC 9(04)V99.           
007100     05 PAYROLL-EMP-SSN-MED-WRITE    PIC 9(03)V99.
007200     05 PAYROLL-EMP-STATE-WRITE      PIC 9(03)V99.
007300     05 PAYROLL-EMP-LOCAL-WRITE      PIC 9(03)V99.   
007400     05 PAYROLL-EMP-FED-WRITE        PIC 9(03)V99.     
007500     05 PAYROLL-EMP-NET-WRITE        PIC 9(04)V99.
007600  01 WS-RECORD-COUNT-OUT             pic 99 VALUE 0.
007700  01 WS-SINGLE-WITHHELD              pic 9(06)v99.
007800  01 WS-SINGLE-NET                   pic 9(06)v99.
007900  01 PAYROLL-EMP-BASE                pic 9(06)v99 VALUE 0.
008000  01 PAYROLL-EMP-OVERTIME            pic 9(06)v99 VALUE 0.
008100* Unformatted fields
008200  01 WS-TOTAL-GROSS               pic 9(06)V99 VALUE 0.
008300  01 WS-TOTAL-NET                 pic 9(06)V99 VALUE 0.
008400  01 WS-TOTAL-WITHHELD            pic 9(06)V99 VALUE 0.
008500  01 WS-TOTAL-HOURS               pic 9(06) VALUE 0.
008600  01 WS-TOTAL-OVERTIME            pic 9(06) VALUE 0.
008700* Formatted fields
008800  01 WS-TOTAL-GROSS-FORMAT        PIC $ZZZ,ZZZ.99.
008900  01 WS-TOTAL-NET-FORMAT          PIC $ZZZ,ZZZ.99.
009000  01 WS-TOTAL-WITHHELD-FORMAT     PIC $ZZZ,ZZZ.99.
009100* File status
009200  01 WS-PAYROLL-IN-STATUS         PIC X(02).
009300     88 PAYROLL-IN-SUCCESSFUL        VALUE "00".
009400     88 END-OF-PAYROLL-IN            VALUE "10".
009500     88 INVALID-VALUE-IN-FILE        VALUE "11" THRU "99".
009600  01 WS-PAYROLL-OUT-STATUS        pic X(02).
009700     88 GOOD-PAYROLL-FILE-WRITE      VALUE "00".
009800  01 WS-PAYROLL-REPORT-STATUS        pic X(02).
009900     88 GOOD-PAYROLL-RPT-FILE-WRITE      VALUE "00".
010000  01 ANY-KEY                      pic X(01).
        05 PREV-DATE.
003000     88 PAY-MONTH             PIC X(02).
003100     88 PAY-DAY               PIC X(02).
003200     88 PAY-YEAR              PIC X(02).
010100  
010200  
010300 01  PAGE-HDG-01.
010400     05 PAGE-HDG-01-CC         PIC X(01).
010500     05 FILLER                 PIC X(60) VALUE SPACES.    
010600     05 FILLER                 PIC X(14) VALUE "PAYROLL REPORT".
010700     05 FILLER                 PIC X(30) VALUE SPACES.
010800     05 FILLER                 PIC X(06) VALUE "PAGE: ".
010900     05 PAGE-HDG-01-PAGE       PIC ZZ9.   
011000 01  PAGE-HDG-02.
011100     05 PAGE-HDG-02-CC         PIC X(01).
011200     05 FILLER                 PIC X(66) VALUE SPACES.
011300     05 FILLER                 PIC X(02) VALUE "BY".
011400     05 FILLER                 PIC X(56) VALUE SPACES.
011500 01  PAGE-HDG-03.
011600     05 PAGE-HDG-02-CC         PIC X(01).
011700     05 FILLER                 PIC X(62) VALUE SPACES.
011800     05 FILLER                 PIC X(11) VALUE "RYAN BROOKS".
011900     05 FILLER                 PIC X(56) VALUE SPACES.
012000 01  PAGE-HDG-04.
012100     05 PAGE-HDG-02-CC         PIC X(01).
012200     05 FILLER                 PIC X(60) VALUE SPACES.
012300     05 FILLER                 PIC X(06) VALUE "AS OF ".
012400     05 PAGE-HDG-02-DATE       PIC XX/XX/XXXX.
012500     05 FILLER                 PIC X(56) VALUE SPACES.
012600 01  PAGE-HDG-05.
012700     05 PAGE-HDG-03-CC         PIC X(01).
012800     05 FILLER                 PIC X(09) VALUE SPACES.
012900     05 FILLER                 PIC X(07) VALUE "PAYDATE".
013000     05 FILLER                 PIC X(05) VALUE SPACES.
013100     05 FILLER                 PIC X(08) VALUE "EMP. NUM". 
013200     05 FILLER                 PIC X(04) VALUE SPACES.
013300     05 FILLER                 PIC X(05) VALUE "HOURS".
013400     05 FILLER                 PIC X(05) VALUE SPACES.
013500     05 FILLER                 PIC X(04) VALUE "RATE".  
013600     05 FILLER                 PIC X(05) VALUE SPACES.
013700     05 FILLER                 PIC X(08) VALUE "BASE PAY". 
013800     05 FILLER                 PIC X(05) VALUE SPACES.
013900     05 FILLER                 PIC X(08) VALUE "OVERTIME".                                                                                                                                                                                   
014000     05 FILLER                 PIC X(08) VALUE SPACES.  
014100     05 FILLER                 PIC X(05) VALUE "GROSS".
014200     05 FILLER                 PIC X(09) VALUE SPACES.
014300     05 FILLER                 PIC X(06) VALUE "DEDUCT".
014400     05 FILLER                 PIC X(09) VALUE SPACES.
014500     05 FILLER                 PIC X(03) VALUE "NET".
014600     05 FILLER                 PIC X(05) VALUE SPACES.
014700     
014800 01  DETAIL-LINE-PAYROLL.
014900     05 DTL-CC                 PIC X(01).
015000     05 FILLER                 PIC X(09) VALUE SPACES.
015100     05 DTL-DATE               PIC ZZ/ZZ/ZZ.
015200     05 FILLER                 PIC X(05) VALUE SPACES.
015300     05 DTL-EMP-NUM            PIC ZZZZZZ.
015400     05 FILLER                 PIC X(05) VALUE SPACES.
015500     05 DTL-HOURS-WORKED       PIC ZZ9.
015600     05 FILLER                 PIC X(05) VALUE SPACES.
015700     05 DTL-HOUR-RATE          PIC $Z9.99.
015800     05 FILLER                 PIC X(05) VALUE SPACES.
015900     05 DTL-BASE-PAY           PIC $Z,ZZ9.99.
016000     05 FILLER                 PIC X(05) VALUE SPACES.
016100     05 DTL-OVERTIME           PIC $Z,ZZ9.99.
016200     05 FILLER                 PIC X(05) VALUE SPACES.
016300     05 DTL-GROSS              PIC $Z,ZZ9.99.
016400     05 FILLER                 PIC X(05) VALUE SPACES.
016500     05 DTL-DEDUCT             PIC $Z,ZZ9.99.
016600     05 FILLER                 PIC X(05) VALUE SPACES.
016700     05 DTL-NET                PIC $Z,ZZ9.99.
016800     05 FILLER                 PIC X(05) VALUE SPACES.
016900     
017000 01  DETAIL-LINE-LINE. 
017100     05 FILLER                 PIC X(10) VALUE SPACES.
017200     05 FILLER                 PIC X(115) VALUE ALL "_".
017300     05 FILLER                 PIC X(21) VALUE SPACES.
017400     
017500 01  WS-COST-OUT               PIC $ZZZ,ZZ9.99.
017600 
017700 01  TOTALS-LINE.
017800     05 TOTAL-LINE-CC          PIC X(01).
017900     05 FILLER                 PIC X(09) VALUE SPACES.
018000     05 TOTALS-LINE-DATE       PIC XX/XX/XX.
018100     05 FILLER                 PIC X(05) VALUE SPACES.
018200     05 FILLER                 PIC X(07) VALUE "TOTALS:".
018300     05 FILLER                 PIC X(03) VALUE SPACES.
018400     05 TOTAL-HOURS            PIC Z,ZZ9.
018500     05 FILLER                 PIC X(29) VALUE SPACES.
018600     05 TOTAL-OVERTIME         PIC $ZZ,ZZ9.99.
018700     05 FILLER                 PIC X(04) VALUE SPACES.
018800     05 TOTAL-GROSS            PIC $ZZ,ZZ9.99.
018900     05 FILLER                 PIC X(04) VALUE SPACES.
019000     05 TOTAL-DEDUCT           PIC $ZZ,ZZ9.99.
019100     05 FILLER                 PIC X(04) VALUE SPACES.
019200     05 TOTAL-NET              PIC $ZZ,ZZ9.99.
019300 
019400 01  PAGE-STUFF.
019500     05 WS-PAGE-COUNT           PIC 999 VALUE 0.
019600     05 WS-LINE-COUNT           PIC 99 VALUE 0.
019700 
019800 01  WS-SHAPE-FILE-IN-STATUS   pic X(02).
019900     88 SHAPE-FILE-IN-SUCCESSFUL         VALUE "00".
020000     88 END-OF-SHAPE-FILE                VALUE "10".
020100     88 INVALID-SHAPE-IN-FILE            VALUE "11" THRU "99".
020200     88 SHAPE-FILE-NOT-READY             VALUE "01" THRU "99".
020300     
020400 01  WS-SHAPE-FILE-OUT-STATUS  pic X(02).
020500     88 GOOD-SHAPE-FILE-WRITE            VALUE "00".
020600 01  WS-SHAPE-REPORT-STATUS    pic X(02).
020700     88 GOOD-SHAPE-REPORT                VALUE "00".           
020800 
020900 01 WS-RUN-DATE                PIC X(08).
021000 01 WS-RULER                   PIC X(39)
021100    VALUE "----+----1----+----2----+----3----+----".
021200 01 WS-REPORT-RULER.
021300    05 FILLER                  PIC X(50)
021400       VALUE "----+----1----+----2----+----3----+----4----+----5".
021500    05 FILLER                  PIC X(50)
021600       VALUE "----+----6----+----7----+----8----+----9----+----0".
021700    05 FILLER                  PIC X(44)
021800       VALUE "----+----1----+----2----+----3---".        
021900                                                                                  
022000 PROCEDURE DIVISION.  
022100  
022200     DISPLAY "CALCPYRB FOR RYAN BROOKS".
022300     
022400* INITIALIZE:  OPEN FILES, PERFORM 1ST READ.
022500     PERFORM 1000-INITIALIZE.
022600     PERFORM 2000-READ-PAYROLL.
022700      
022800      perform until END-OF-PAYROLL-IN
022900         perform 3000-CALCULATIONS
023000         perform 2000-READ-PAYROLL
023100      END-PERFORM.
023200      
023300      perform 6000-MOVE-RECORDS
023400      write SHAPE-REC-OUT from WS-RULER.
023500      perform 9000-CREATE-REPORT-TOTAL-LINE.
023600         
023700      
023800*	   DISPLAY PROGRAM TOTALS
023900     display "RECORDS READ:                " WS-RECORD-COUNT-IN.
024000     display "RECORDS WRITTEN:             " WS-RECORD-COUNT-OUT.
024100     DISPLAY "TOTAL GROSS AMOUNT: " WS-TOTAL-GROSS-FORMAT.
024200     DISPLAY "TOTAL NET AMOUNT:   " WS-TOTAL-NET-FORMAT.
024300     DISPLAY "TOTAL WITHHELD:     " WS-TOTAL-WITHHELD-FORMAT.
024400     
024500*	   CLOSE FILES
024600     close PAYROLL-IN
024700           PAYROLL-OUT
024800           PAYROLL-REPORT.
024900     display "Press any key to exit.".
025000         accept ANY-KEY.
025100     
025200     GOBACK.                                                            
025300 
025400 1000-INITIALIZE.
025500     ACCEPT WS-RUN-DATE FROM DATE.
025600     MOVE   WS-RUN-DATE TO   PAGE-HDG-02-DATE
025700                             TOTALS-LINE-DATE.
025800     OPEN INPUT PAYROLL-IN.
025900     OPEN OUTPUT PAYROLL-OUT
026000                 PAYROLL-REPORT.
026100     MOVE ZEROES to WS-RECORDS-READ
026200                    WS-RECORDS-WRITTEN.
026300     WRITE SHAPE-REC-OUT FROM WS-RULER.
026400     PERFORM 8000-WRITE-HEADINGS.
026500     
026600 2000-READ-PAYROLL.
026700* Read the payroll file in and populate the Working Section
026800  READ PAYROLL-IN INTO WS-RECORDS-READ
026900       AT END
027000         DISPLAY "END OF PAYROLL FILE"
027100       NOT AT END
027200         ADD 1 TO WS-RECORD-COUNT-IN.
027300 
027400 3000-CALCULATIONS.
027500* Perform the required calculations
027600* ACCEPT PAYROLL-EMP-GROSS-WRITE
027700      IF PAYROLL-EMP-HOURS of PAYROLL-REC-IN < 40 
027800          COMPUTE PAYROLL-EMP-GROSS-WRITE =
027900          PAYROLL-EMP-HOURS of PAYROLL-REC-IN
028000          * PAYROLL-EMP-RATE of PAYROLL-REC-IN
028100          compute PAYROLL-EMP-BASE = PAYROLL-EMP-GROSS-WRITE
028200          compute PAYROLL-EMP-OVERTIME = 0
028300      ELSE 
028400      IF PAYROLL-EMP-HOURS of PAYROLL-REC-IN > 40
028500          COMPUTE PAYROLL-EMP-GROSS-WRITE =
028600                  (40 * PAYROLL-EMP-RATE of PAYROLL-REC-IN) +
028700                  ((PAYROLL-EMP-HOURS of PAYROLL-REC-IN - 40) * 
028800                  (1.5 * PAYROLL-EMP-RATE of PAYROLL-REC-IN))
028900          compute PAYROLL-EMP-BASE =
029000                  (40 * PAYROLL-EMP-RATE of PAYROLL-REC-IN)
029100          compute PAYROLL-EMP-OVERTIME =
029200                  (PAYROLL-EMP-HOURS of PAYROLL-REC-IN - 40) * 
029300                  (1.5 * PAYROLL-EMP-RATE of PAYROLL-REC-IN)
029400       ELSE
029500         if PAYROLL-EMP-HOURS of PAYROLL-REC-IN = 40
029600          COMPUTE PAYROLL-EMP-GROSS-WRITE =
029700          PAYROLL-EMP-HOURS of PAYROLL-REC-IN
029800          * PAYROLL-EMP-RATE of PAYROLL-REC-IN
029900          compute PAYROLL-EMP-BASE = PAYROLL-EMP-GROSS-WRITE
030000          compute PAYROLL-EMP-OVERTIME = 0        
030100      END-IF.  
030200     COMPUTE PAYROLL-EMP-STATE-WRITE   = 
030300                                PAYROLL-EMP-GROSS-WRITE *.06.
030400     COMPUTE PAYROLL-EMP-SSN-MED-WRITE = 
030500                                PAYROLL-EMP-GROSS-WRITE * .0765.
030600     COMPUTE PAYROLL-EMP-LOCAL-WRITE   = 
030700                                PAYROLL-EMP-GROSS-WRITE *  .01.
030800*    ACCEPT PAYROLL-EMP-FED-WRITE 
030900     IF PAYROLL-EMP-DEDUCTIONS-READ = 0
031000          COMPUTE PAYROLL-EMP-FED-WRITE =
031100                         PAYROLL-EMP-GROSS-WRITE * .20
031200     else
031300     IF PAYROLL-EMP-DEDUCTIONS-READ = 1 
031400           COMPUTE PAYROLL-EMP-FED-WRITE =
031500                         PAYROLL-EMP-GROSS-WRITE * .18
031600      ELSE
031700     IF PAYROLL-EMP-DEDUCTIONS-READ = 2
031800           COMPUTE PAYROLL-EMP-FED-WRITE =
031900                         PAYROLL-EMP-GROSS-WRITE * .15
032000      ELSE 
032100     IF PAYROLL-EMP-DEDUCTIONS-READ = 3
032200           COMPUTE PAYROLL-EMP-FED-WRITE =
032300                         PAYROLL-EMP-GROSS-WRITE * .12
032400      ELSE
032500     IF PAYROLL-EMP-DEDUCTIONS-READ = 4
032600           COMPUTE PAYROLL-EMP-FED-WRITE =
032700                         PAYROLL-EMP-GROSS-WRITE * .10
032800    END-IF.
032900    compute WS-SINGLE-WITHHELD =
033000            PAYROLL-EMP-STATE-WRITE
033100            + PAYROLL-EMP-SSN-MED-WRITE + PAYROLL-EMP-LOCAL-WRITE
033200            + PAYROLL-EMP-FED-WRITE.
033300    compute PAYROLL-EMP-NET-WRITE = PAYROLL-EMP-GROSS-WRITE
033400                                  - WS-SINGLE-WITHHELD.
033500    compute WS-SINGLE-NET = PAYROLL-EMP-GROSS-WRITE -
033600                            WS-SINGLE-WITHHELD.
033700    
033800    perform 4000-UPDATE-TOTALS.
033900    perform 5000-WRITE-RECORDS.
034000    perform 7000-CREATE-DETAIL-LINE.
034100 
034200 4000-UPDATE-TOTALS.
034300* Update the totals
034400    compute WS-TOTAL-GROSS = WS-TOTAL-GROSS + 
034500                             PAYROLL-EMP-GROSS-WRITE.
034600    compute WS-TOTAL-WITHHELD = WS-TOTAL-WITHHELD +
034700    WS-SINGLE-WITHHELD.
034800    compute WS-TOTAL-NET = WS-TOTAL-GROSS - WS-TOTAL-WITHHELD.
034900    compute WS-TOTAL-HOURS = WS-TOTAL-HOURS + 
035000            PAYROLL-EMP-HOURS of PAYROLL-REC-IN.
035100    compute WS-TOTAL-OVERTIME = WS-TOTAL-OVERTIME + 
035200            PAYROLL-EMP-OVERTIME.
035300 
035400 5000-WRITE-RECORDS.
035500* Write the records to file.
035600  WRITE PAYROLL-REC-OUT FROM WS-RECORDS-WRITTEN.
035700     if GOOD-PAYROLL-FILE-WRITE 
035800        add 1 to WS-RECORD-COUNT-OUT
035900     else  
036000        display "BAD WRITE - FILE STATUS: " 
036100          WS-PAYROLL-OUT-STATUS.
036200
036300 6000-MOVE-RECORDS.
036400     MOVE WS-TOTAL-GROSS TO WS-TOTAL-GROSS-FORMAT.
036500     MOVE WS-TOTAL-NET TO WS-TOTAL-NET-FORMAT.
036600     MOVE WS-TOTAL-WITHHELD TO WS-TOTAL-WITHHELD-FORMAT.
036700     
036800 7000-CREATE-DETAIL-LINE.
036900     move PAYROLL-PAY-DATE of PAYROLL-REC-IN 
037000                                    to DTL-DATE.
037100     move PAYROLL-EMP-NUMBER of PAYROLL-REC-IN
037200                                    to DTL-EMP-NUM.
037300     move PAYROLL-EMP-HOURS of PAYROLL-REC-IN
037400                                    to DTL-HOURS-WORKED.
037500     move PAYROLL-EMP-RATE of PAYROLL-REC-IN
037600                                    to DTL-HOUR-RATE.
037700     move PAYROLL-EMP-BASE          to DTL-BASE-PAY.
037800     move PAYROLL-EMP-OVERTIME      to DTL-OVERTIME.
037900     move PAYROLL-EMP-GROSS-WRITE   TO DTL-GROSS.
038000     move WS-SINGLE-WITHHELD        to DTL-DEDUCT.
038100     move WS-SINGLE-NET             to DTL-NET.
038200     
038300     add 1 to WS-LINE-COUNT.
038400     if WS-LINE-COUNT > 50
038500       PERFORM 8000-WRITE-HEADINGS.
             
           IF PAYROLL-PAY-DATE of PAYROLL-REC-IN is not equal to
               PREV-DATE
               perform 8000-WRITE-HEADINGS
               perform 10000-CREATE-DATE-TOTAL-LINE
           END-IF.
038600     
           set PREV-DATE to PAYROLL-PAY-DATE of PAYROLL-REC-IN.
038700     write PAYROLL-REPORT-RECORD from DETAIL-LINE-PAYROLL
038800       after advancing 01 lines.
038900 
039000 8000-WRITE-HEADINGS.
039100     add 1 to WS-PAGE-COUNT.
039200     move WS-PAGE-COUNT TO PAGE-HDG-01-PAGE.
039300     WRITE PAYROLL-REPORT-RECORD FROM WS-REPORT-RULER
039400       after advancing page.
039500     WRITE PAYROLL-REPORT-RECORD 
039600      FROM PAGE-HDG-01 after advancing 01 LINES.
039700     WRITE PAYROLL-REPORT-RECORD
039800      FROM PAGE-HDG-02 after advancing 01 LINES.
039900     WRITE PAYROLL-REPORT-RECORD 
040000      FROM PAGE-HDG-03 AFTER ADVANCING 01 LINES.
040100     WRITE PAYROLL-REPORT-RECORD 
040200      FROM PAGE-HDG-04 AFTER ADVANCING 01 LINES.
040300     WRITE PAYROLL-REPORT-RECORD 
040400      FROM PAGE-HDG-05 AFTER ADVANCING 02 LINES.
040500     MOVE 4 TO WS-LINE-COUNT.
040600 
040700 9000-CREATE-REPORT-TOTAL-LINE.
040800     MOVE WS-TOTAL-HOURS  TO TOTAL-HOURS.
040900     move WS-TOTAL-OVERTIME to TOTAL-OVERTIME
041000     MOVE WS-TOTAL-GROSS  TO TOTAL-GROSS.
041100     move WS-TOTAL-WITHHELD to TOTAL-DEDUCT.
041200     MOVE WS-TOTAL-NET    TO TOTAL-NET.
041300     write PAYROLL-REPORT-RECORD from DETAIL-LINE-LINE
041400       AFTER ADVANCING 01 LINES.
041500     write PAYROLL-REPORT-RECORD from TOTALS-LINE
041600      AFTER ADVANCING 02 LINES.
041700     write PAYROLL-REPORT-RECORD from WS-REPORT-RULER
041800       after advancing 02 LINES.