000100 identification division.
000200 program-id. MATHSHPS.
000300* Reads file of rug orders; calculate area and perimeter.
000400* Determines price of rug.
000500* Write out new file with results.
000600* Creates a report of the input. 
000700
000800 environment division.
000900 configuration section.
001000 INPUT-OUTPUT SECTION.
001100 FILE-CONTROL.
001200 select SHAPE-FILE-IN
001300   assign to "C:\Users\Ryan\Desktop\MATHSHPS.txt"
001400*  assign to "C:\Users\call1\MATHSHPS-BIG.txt" 
001500*  assign to "C:\Users\call1\MATHSHPS-BIGGER.txt"          
001600   organization is line sequential
001700   file status is WS-SHAPE-FILE-IN-STATUS.
001800   
001900 SELECT SHAPE-FILE-OUT
002000   ASSIGN To "C:\Users\Ryan\Desktop\MATHSHPSOUT.dat"
002100   organization is line sequential
002200   file status is WS-SHAPE-FILE-OUT-STATUS.
002300   
002400 SELECT SHAPE-REPORT
002500   ASSIGN To "C:\Users\Ryan\Desktop\SHAPERPT1.rpt"
002600   organization is LINE sequential
002700   file status is WS-SHAPE-REPORT-STATUS.         
002800
002900 DATA DIVISION.
003000 FILE SECTION.
003100 FD  SHAPE-FILE-IN                                    
003200     RECORDING MODE IS F  
003300     DATA RECORD IS SHAPE-REC-IN.                  
003400 01 SHAPE-REC-IN               PIC X(21).
003500 
003600 FD  SHAPE-FILE-OUT                                   
003700     RECORDING MODE IS F  
003800     DATA RECORD IS SHAPE-REC-OUT.                  
003900 01 SHAPE-REC-OUT              PIC X(39).
004000 
004100 FD  SHAPE-REPORT
004200     RECORDING MODE IS F
004300     DATA RECORD IS SHAPE-REPORT-RECORD.                  
004400 01 SHAPE-REPORT-RECORD        PIC X(133).
004500 
004600 working-storage section.
004700 01  CALCULATED-SHAPES-RECORD.
004800     05 SHAPE-TYPE             PIC X(10).
004900     05 SIZE-1                 PIC 9(03).
005000     05 SIZE-2                 PIC 9(03).
005100     05 SQ-FT-PRICE            PIC 9(03)V99.
005200     05 CALCULATED-FIELDS-OUT.
005300        10 AREA-OUT            PIC 9(06)V99.
005400        10 PERIMETER-OUT       PIC 9(04).
005500        10 PRICE-OUT           PIC 9(06)V99.
005600     
005700 01  WS-CALCULATION-FIELDS.
005800     05 WS-RECORD-COUNT-IN      PIC 9(02).
005900     05 WS-RECORD-COUNT-OUT     PIC 9(02).
006000     05 WS-TOTAL-FILE-COST      PIC 9(08)V99.
006100     05 WS-TOTAL-FILE-AREA      PIC 9(08)V99.
006200     05 WS-TOTAL-FILE-PERIMETER PIC 9(06)V99.
006300     05 WS-PAGE-COUNT           PIC 999.
006400     05 WS-LINE-COUNT           PIC 99.
006500*
006600* STD PAGES ARE 133 CHARACTERS, SO THE SUM OF PIC CLAUSE
006700* LENGHTS SHOULD ADD UP TO 133, OR THE WIDTH OF THE REPORT
006800* STD OF THE ORGANIZATION. THE HEADING TEXT IS USUALLY CENTERED.
006900 01  PAGE-HDG-01.
007000     05 PAGE-HDG-01-CC         PIC X(01).
007100     05 FILLER                 PIC X(60) VALUE "Ryan Brooks".    
007200     05 FILLER                 PIC X(12) VALUE "SHAPE REPORT".
007300     05 FILLER                 PIC X(30) VALUE SPACES.
007400     05 FILLER                 PIC X(06) VALUE "PAGE: ".
007500     05 PAGE-HDG-01-PAGE       PIC ZZ9.   
007600 01  PAGE-HDG-02.
007700     05 PAGE-HDG-02-CC         PIC X(01).
007800     05 FILLER                 PIC X(56) VALUE SPACES.
007900     05 FILLER                 PIC X(10) VALUE "RUN DATE: ".
008000     05 PAGE-HDG-02-DATE       PIC XX/XX/XXXX.
008100     05 FILLER                 PIC X(56) VALUE SPACES.
008200 01  PAGE-HDG-03.
008300     05 PAGE-HDG-03-CC         PIC X(01).
008400     05 FILLER                 PIC X(10) VALUE SPACES.
008500     05 FILLER                 PIC X(05) VALUE "SHAPE".
008600     05 FILLER                 PIC X(13) VALUE SPACES.
008700     05 FILLER                 PIC X(06) VALUE "SIDE 1". 
008800     05 FILLER                 PIC X(07) VALUE SPACES.
008900     05 FILLER                 PIC X(06) VALUE "SIDE 2".
009000     05 FILLER                 PIC X(10) VALUE SPACES.
009100     05 FILLER                 PIC X(04) VALUE "AREA".  
009200     05 FILLER                 PIC X(10) VALUE SPACES.
009300     05 FILLER                 PIC X(09) VALUE "PERIMETER". 
009400     05 FILLER                 PIC X(13) VALUE SPACES.
009500     05 FILLER                 PIC X(05) VALUE "PRICE".                                                                                                                                                                                           
009600     05 FILLER                 PIC X(15) VALUE SPACES.  
009700     05 FILLER                 PIC X(15) VALUE "PRICE SQ/FT".
009800     05 FILLER                 PIC X(20) VALUE SPACES.
009900     
010000 01  DETAIL-LINE-SHAPE.
010100     05 DTL-CC                 PIC X(01).
010200     05 FILLER                 PIC X(09) VALUE SPACES.
010300     05 DTL-SHAPE              PIC X(10).
010400     05 FILLER                 PIC X(10) VALUE SPACES.
010500     05 DTL-SHAPE-SIZE-1       PIC ZZ9.
010600     05 FILLER                 PIC X(10) VALUE SPACES.
010700     05 DTL-SHAPE-SIZE-2       PIC ZZ9.
010800     05 FILLER                 PIC X(10) VALUE SPACES.
010900     05 DTL-SHAPE-AREA         PIC ZZZ,ZZ9.
011000     05 FILLER                 PIC X(10) VALUE SPACES.
011100     05 DTL-SHAPE-PERIMETER    PIC ZZZ,ZZ9.
011200     05 FILLER                 PIC X(10) VALUE SPACES.
011300     05 DTL-SHAPE-PRICE        PIC Z,ZZZ,ZZZ.99.
011400     05 FILLER                 PIC X(10) VALUE SPACES.
011500     05 DTL-SQ-FT-PRICE        PIC Z,ZZZ,ZZZ.99.
011600     05 FILLER                 PIC X(10) VALUE SPACES.
011700     
011800 01  DETAIL-LINE-LINE. 
011900     05 FILLER                 PIC X(10) VALUE SPACES.
012000     05 FILLER                 PIC X(115) VALUE ALL "_".
012100     05 FILLER                 PIC X(21) VALUE SPACES.
012200     
012300 01  WS-COST-OUT               PIC $ZZZ,ZZZ.99.
012400 
012500 01  TOTALS-LINE.
012600     05 TOTAL-LINE-CC          PIC X(01).
012700     05 FILLER                 PIC X(09) VALUE SPACES.
012800     05 FILLER                 PIC X(11) VALUE "FILE TOTALS".
012900     05 FILLER                 PIC X(06) VALUE SPACES.
013000     05 FILLER                 PIC X(14) VALUE "RECORD COUNT: ".
013100     05 TOTAL-LINE-COUNT       PIC Z,ZZ9.
013200     05 FILLER                 PIC X(07) VALUE SPACES.
013300     05 TOTAL-LINE-AREA        PIC ZZ,ZZZ,ZZ9.
013400     05 FILLER                 PIC X(10) VALUE SPACES.
013500     05 TOTAL-LINE-PERIMETER   PIC ZZZ,ZZ9.
013600     05 FILLER                 PIC X(08) VALUE SPACES.
013700     05 TOTAL-LINE-PRICE       PIC $$$,ZZZ,ZZ9.99.
013800                                                                  
013900 
014000 01  WS-SHAPE-FILE-IN-STATUS   pic X(02).
014100     88 SHAPE-FILE-IN-SUCCESSFUL         VALUE "00".
014200     88 END-OF-SHAPE-FILE                VALUE "10".
014300     88 INVALID-SHAPE-IN-FILE            VALUE "11" THRU "99".
014400     88 SHAPE-FILE-NOT-READY             VALUE "01" THRU "99".
014500     
014600 01  WS-SHAPE-FILE-OUT-STATUS  pic X(02).
014700     88 GOOD-SHAPE-FILE-WRITE            VALUE "00".
014800 01  WS-SHAPE-REPORT-STATUS    pic X(02).
014900     88 GOOD-SHAPE-REPORT                VALUE "00".           
015000 
015100 01 WS-RUN-DATE                PIC X(08).
015200 01 WS-RULER                   PIC X(39)
015300    VALUE "----+----1----+----2----+----3----+----".
015400 01 WS-REPORT-RULER.
015500    05 FILLER                  PIC X(50)
015600       VALUE "----+----1----+----2----+----3----+----4----+----5".
015700    05 FILLER                  PIC X(50)
015800       VALUE "----+----6----+----7----+----8----+----9----+----0".
015900    05 FILLER                  PIC X(44)
016000       VALUE "----+----1----+----2----+----3---".                                                              
016100     
016200 procedure division.
016300
016400     DISPLAY "START SHAPERPT".
016500     
016600     PERFORM XXXX-INITIALIZE.
016700     PERFORM XXXX-READ-SHAPES.
016800     
016900     perform until END-OF-SHAPE-FILE
017000       perform XXXX-CALCULATE-FIELDS
017100       perform XXXX-WRITE-SHAPES-RECORD
017200       perform XXXX-CREATE-DETAIL-LINE
017300       perform XXXX-READ-SHAPES
017400     END-PERFORM.
017500     
017600     write SHAPE-REC-OUT from WS-RULER.
017700     perform XXXX-CREATE-REPORT-TOTAL-LINE.
017800     move WS-TOTAL-FILE-COST to WS-COST-OUT.
017900     
018000     display " FILE COST      : " WS-COST-OUT.
018100     display " RECORDS READ   : " WS-RECORD-COUNT-IN.
018200     display " RECORDS WRITTEN: " WS-RECORD-COUNT-OUT.
018300     display "END OF SHAPERPT".
018400     
018500     close SHAPE-FILE-IN
018600           SHAPE-FILE-OUT
018700           SHAPE-REPORT.
018800   goback.
018900       
019000 XXXX-CALCULATE-FIELDS.
019100     COMPUTE AREA-OUT      = SIZE-1 * SIZE-2.
019200     COMPUTE PERIMETER-OUT = (SIZE-1 * 2) + (SIZE-2 * 2).
019300     COMPUTE PRICE-OUT     = AREA-OUT * SQ-FT-PRICE.
019400     compute WS-TOTAL-FILE-COST
019500                           = WS-TOTAL-FILE-COST + PRICE-OUT.
019600     compute WS-TOTAL-FILE-AREA
019700                           = WS-TOTAL-FILE-AREA + AREA-OUT.
019800     compute WS-TOTAL-FILE-PERIMETER
019900                           = WS-TOTAL-FILE-PERIMETER +
020000                             PERIMETER-OUT. 
020100     
020200 XXXX-READ-SHAPES.
020300     read SHAPE-FILE-IN into CALCULATED-SHAPES-RECORD
020400       at end
020500         display "END OF SHAPE FILE"
020600       not AT end
020700         add 1 to WS-RECORD-COUNT-IN.
020800         
020900 XXXX-WRITE-SHAPES-RECORD.
021000     WRITE SHAPE-REC-OUT FROM CALCULATED-SHAPES-RECORD
021100       after advancing 01 LINES.
021200     if GOOD-SHAPE-FILE-WRITE 
021300        add 1 to WS-RECORD-COUNT-OUT
021400     else  
021500        display "BAD WRITE - FILE STATUS: " 
021600          WS-SHAPE-FILE-OUT-STATUS.
021700          
021800 XXXX-CREATE-DETAIL-LINE.
021900     move SHAPE-TYPE    to DTL-SHAPE.
022000     move SIZE-1        to DTL-SHAPE-SIZE-1.
022100     move SIZE-2        to DTL-SHAPE-SIZE-2.
022200     move AREA-OUT      to DTL-SHAPE-AREA.
022300     move PERIMETER-OUT to DTL-SHAPE-PERIMETER.
022400     move PRICE-OUT     to DTL-SHAPE-PRICE.
022500     move SQ-FT-PRICE   TO DTL-SQ-FT-PRICE.
022600     
022700     add 1 to WS-LINE-COUNT.
022800     if WS-LINE-COUNT > 50
022900       PERFORM XXXX-WRITE-HEADINGS.
023000     
023100     write SHAPE-REPORT-RECORD from DETAIL-LINE-SHAPE
023200       after advancing 01 lines.
023300    
023400     
023500 XXXX-CREATE-REPORT-TOTAL-LINE.
023600     MOVE WS-RECORD-COUNT-OUT TO TOTAL-LINE-COUNT.
023700     MOVE WS-TOTAL-FILE-AREA  TO TOTAL-LINE-AREA.
023800     MOVE WS-TOTAL-FILE-PERIMETER
023900                              TO TOTAL-LINE-PERIMETER.
024000     MOVE WS-TOTAL-FILE-COST  TO TOTAL-LINE-PRICE.
024100     write SHAPE-REPORT-RECORD from DETAIL-LINE-LINE
024200       AFTER ADVANCING 01 LINES.
024300     write SHAPE-REPORT-RECORD from TOTALS-LINE
024400      AFTER ADVANCING 02 LINES.
024500     write SHAPE-REPORT-RECORD from WS-REPORT-RULER
024600       after advancing 01 LINES.                                                                          
024700 
024800 XXXX-INITIALIZE.
024900     ACCEPT WS-RUN-DATE FROM DATE.
025000     MOVE   WS-RUN-DATE TO   PAGE-HDG-02-DATE.
025100     OPEN INPUT  SHAPE-FILE-IN.
025200     OPEN OUTPUT SHAPE-FILE-OUT
025300                 SHAPE-REPORT.
025400     MOVE ZEROES to WS-CALCULATION-FIELDS 
025500                    CALCULATED-FIELDS-OUT.
025600     WRITE SHAPE-REC-OUT FROM WS-RULER.
025700     PERFORM XXXX-WRITE-HEADINGS.
025800 
025900 XXXX-WRITE-HEADINGS.
026000     add 1 to WS-PAGE-COUNT.
026100     move WS-PAGE-COUNT TO PAGE-HDG-01-PAGE.
026200     WRITE SHAPE-REPORT-RECORD FROM WS-REPORT-RULER
026300       after advancing page.
026400     WRITE SHAPE-REPORT-RECORD 
026500      FROM PAGE-HDG-01 after advancing 01 LINES.
026600     WRITE SHAPE-REPORT-RECORD
026700      FROM PAGE-HDG-02 after advancing 01 LINES.
026800     WRITE SHAPE-REPORT-RECORD FROM PAGE-HDG-03
026900      AFTER ADVANCING 02 LINES.
027000     MOVE 4 TO WS-LINE-COUNT.