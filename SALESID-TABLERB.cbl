000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID. SALESSUM.                                                      
000300* THE PROGRAM READS IN THE SALESIDAMT.DAT FILE AND NEEDS TO 
000400* WRITE OUT TWO SUMMARY-FILES; SALES-FILE-OUT AND STATE-FILE-OUT.
000500* EACH SUMMARY FILE HAS ONE RECORD FOR EACH KEY (SALESID OR STATE)
000600* WITH THE TOTAL AMOUNT FOR THAT KEY ON THE RECORD.
000700* YOU MAY WRITE OUT A TRAILER RECORD FOR EACH FILE FOR EXTRA 
000800* CREDIT.  THE PROGRAM IS MOSTLY CODED AND YOU WILL NEED TO 
000900* ADD CODE TO COMPLETE THE ASSIGMENT. THIS IS INDICATED, BUT YOU
001000* MAY HAVE TO ADD ADDITIONAL CODE TO GET IT TO WORK AND COMPILE.                                     
001100 ENVIRONMENT DIVISION.                                                    
001200 CONFIGURATION SECTION.                                                   
001300 INPUT-OUTPUT SECTION.                                                    
001400 FILE-CONTROL.  
001500     SELECT SALES-FILE-IN                                                 
001600       ASSIGN TO "C:\Users\Ryan\Downloads\SALESIDAMT.DAT"      
001700       ORGANIZATION IS LINE SEQUENTIAL                                    
001800       FILE STATUS IS SALES-FILE-STATUS.  
001900     SELECT SALES-FILE-OUT                                                 
002000       ASSIGN TO "C:\Users\Ryan\Downloads\SALESIDOUT.DAT"      
002100       ORGANIZATION IS LINE SEQUENTIAL. 
002200     SELECT STATE-FILE-OUT                                                 
002300       ASSIGN TO "C:\Users\Ryan\Downloads\STATESOUT.DAT"      
002400       ORGANIZATION IS LINE SEQUENTIAL.                                                                         
002500                                                   
002600 DATA DIVISION.                                                           
002700 FILE SECTION.                                                            
002800 FD SALES-FILE-IN                                                        
002900     RECORDING MODE IS F                                                  
003000     DATA RECORD ISSALES-RECORD-IN.                                       
003100 01 SALES-RECORD-IN.
003200    05 SALES-STATE-IN            PIC X(02).
003300    05 SALES-ID-IN               PIC X(03).
003400    05 SALES-AMOUNT-IN           PIC 9(04)V99.
003500          
003600 FD  SALES-FILE-OUT                                                        
003700     RECORDING MODE IS F                                                  
003800     DATA RECORD IS SALES-RECORD-OUT.                                       
003900 01 SALES-RECORD-OUT.
004000    05 SALES-ID-OUT              PIC X(03).
004100    05 SALES-ID-AMT-OUT          PIC 9(06)V99.
004200
004300 FD  STATE-FILE-OUT                                                        
004400     RECORDING MODE IS F                                                  
004500     DATA RECORD IS STATE-RECORD-OUT.                                       
004600 01 STATE-RECORD-OUT.
004700    05 STATE-ID-OUT              PIC X(02).
004800    05 STATE-ID-AMT-OUT          PIC 9(06)V99.       
004900       
005000 WORKING-STORAGE SECTION. 
005100 01 TABLE-SALES-ID-ENTRY OCCURS 99 TIMES.
005200    05 TABLE-SALES-ID            PIC X(03).
005300    05 TABLE-SALES-ID-AMOUNT     PIC 9(06)V99.
005400 
005500    
005600* YOU MUST CREATE A TABLE HERE TO RECORD THE STATE ENTRIES
005700* AND THE AMOUNT FOR EACH STATE. IT WILL LOOK LIKE THE TABLE
005800* ABOVE, BUT BE FOR THE STATE. YOU WILL ALSO NEED TO CREATE
005900* A SUBSCRIPT FOR THE STATE TABLE, LIKE WS-STATE-SUB.
006000  
006100 01 TABLE-STATE-ENTRY OCCURS 50 TIMES.
006200    05 TABLE-STATE            PIC X(03).
006300    05 TABLE-STATE-AMOUNT     PIC 9(06)V99.
006400        
006500 01 WS-COUNTERS.
006600    05 WS-SALES-TOTAL           PIC 9(06)V99 VALUE 0.
006700    05 WS-STATE-TOTAL           PIC 9(06)V99 VALUE 0.
006800    05 WS-ID-SUB                PIC 9(02).
006900    05 WS-STATE-SUB             PIC 9(02).
007000
007100 01 WS-DATA-SWITCH              pic 9     VALUE 1.
007200    88 EOF                                VALUE 0.
007300 01 SALES-FILE-STATUS           PIC X(02).
007400    88 NO-DATA                            VALUE '02' THRU '99'.
007500        
007600 PROCEDURE DIVISION.                                                      
007700                                                                          
007800 0000-DRIVER.                                                             
007900     DISPLAY 'SALESSUM PROGRAM START'. 
008000     OPEN INPUT  SALES-FILE-IN.
008100     OPEN OUTPUT SALES-FILE-OUT
008200                 STATE-FILE-OUT. 
008300     PERFORM 1000-INITIALIZE-TABLE.
008400     READ SALES-FILE-IN.
008500     PERFORM 2000-POPULATE-TABLES 
008600       UNTIL NO-DATA
008700          or WS-DATA-SWITCH = 0.
008800     PERFORM 3000-WRITE-SUMMARY-FILES.
008900     PERFORM 4000-WRITE-TRAILERS.
009000     CLOSE SALES-FILE-IN
009100           SALES-FILE-OUT
009200           STATE-FILE-OUT.
009300     GOBACK.
009400     
009500 1000-INITIALIZE-TABLE.
009600* INITIALIZE THE SALES ID TABLE: 
009700     PERFORM VARYING WS-ID-SUB FROM 1 BY 1
009800       UNTIL WS-ID-SUB > 98
009900          MOVE SPACES TO TABLE-SALES-ID (WS-ID-SUB)
010000          MOVE ZEROES TO TABLE-SALES-ID-AMOUNT (WS-ID-SUB)
010100     END-PERFORM.
010200     
010300* INITIALIZE THE STATE TABLE: 
010400     PERFORM VARYING WS-STATE-SUB FROM 1 by 1
010500       UNTIL WS-STATE-SUB > 49
010600          MOVE spaces to TABLE-STATE (WS-STATE-SUB)
010700          MOVE zeroes to TABLE-STATE-AMOUNT (WS-STATE-SUB) 
010800     END-PERFORM.       
010900     
011000 2000-POPULATE-TABLES.
011100
011200* SEARCH SALES TABLE FOR THE SALES ID OR AN ENTRY WITH SPACES:     
011300     PERFORM VARYING WS-ID-SUB FROM 1 BY 1
011400       UNTIL WS-ID-SUB > 98
011500          OR TABLE-SALES-ID (WS-ID-SUB) = SPACES
011600          OR TABLE-SALES-ID (WS-ID-SUB) = SALES-ID-IN
011700     END-PERFORM.
011800     
011900* CHECK TO SEE IF THE SALE ID WAS FOUND:      
012000     IF TABLE-SALES-ID (WS-ID-SUB) = SALES-ID-IN
012100       ADD SALES-AMOUNT-IN TO TABLE-SALES-ID-AMOUNT (WS-ID-SUB)
012200     ELSE 
012300       ADD SALES-AMOUNT-IN TO TABLE-SALES-ID-AMOUNT (WS-ID-SUB)
012400       MOVE SALES-ID-IN TO TABLE-SALES-ID (WS-ID-SUB).
012500       
012600* SEARCH STATE TABLE FOR THE STATE ID OR AN ENTRY WITH SPACES:     
012700     PERFORM VARYING WS-STATE-SUB FROM 1 BY 1 
012800       UNTIL WS-STATE-SUB > 49
012900          OR TABLE-STATE (WS-STATE-SUB) = SPACES
013000          OR TABLE-STATE (WS-STATE-SUB) = SALES-STATE-IN
013100     END-PERFORM.
013200     
013300* CHECK TO SEE IF THE STATE WAS FOUND:      
013400     IF TABLE-STATE (WS-STATE-SUB) = SALES-STATE-IN
013500       ADD SALES-AMOUNT-IN TO TABLE-STATE-AMOUNT (WS-STATE-SUB)
013600     ELSE 
013700       add SALES-AMOUNT-IN to TABLE-STATE-AMOUNT (WS-STATE-SUB)
013800       move SALES-STATE-IN to TABLE-STATE (WS-STATE-SUB).     
013900       
014000     READ SALES-FILE-IN
014100       AT END MOVE 0 TO WS-DATA-SWITCH.
014200       
014300 3000-WRITE-SUMMARY-FILES.
014400
014500* READ THROUGH EACH SALES ID TABLE OCCURENCE AND MOVE TO THE 
014600* SALES SUMMARY OUTPUT RECORD AND WRITE THE RECORD.
014700     PERFORM VARYING WS-ID-SUB FROM 1 BY 1
014800       UNTIL WS-ID-SUB > 98
014900          OR TABLE-SALES-ID(WS-ID-SUB) = SPACES
015000       MOVE TABLE-SALES-ID(WS-ID-SUB) TO SALES-ID-OUT
015100       MOVE TABLE-SALES-ID-AMOUNT (WS-ID-SUB) TO SALES-ID-AMT-OUT
015200       WRITE SALES-RECORD-OUT
015300       ADD SALES-ID-AMT-OUT  TO WS-SALES-TOTAL
015400     END-PERFORM.  
015500
015600* READ THROUGH EACH STATE ID TABLE OCCURENCE AND MOVE TO THE 
015700* STATE SUMMARY OUTPUT RECORD AND WRITE THE RECORD. 
015800     PERFORM VARYING WS-STATE-SUB from 1 by 1
015900       UNTIL WS-STATE-SUB > 49
016000          OR TABLE-STATE (WS-STATE-SUB) = SPACES
016100       MOVE TABLE-STATE (WS-STATE-SUB) TO STATE-ID-OUT
016200       MOVE TABLE-STATE-AMOUNT (WS-STATE-SUB) TO STATE-ID-AMT-OUT
016300       WRITE STATE-RECORD-OUT
016400       ADD STATE-ID-AMT-OUT TO WS-STATE-TOTAL
016500     END-PERFORM.               
016600     
016700 4000-WRITE-TRAILERS.
016800* IF YOU WANT EXTRA CREDIT FOR CREATING A TRAILER RECORD, REMOVE 
016900* EXIT STATEMENT AND INSERT THE EXTRA CREDIT CODE:  
017000     EXIT.
