000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. RPCGAMRB.
000300 AUTHOR.  Ryan Brooks.
000400* ASK USED FOR ROCK, PAPER OR SCISSORS.
000500* COMPARE THEIR INPUT ANSSWER TO THE RANDOM NUMBER
000600* GENERATED BY THE PROGRAM.  PROVIDE WINNER/LOSER FEEDBACK. 
000700 
000800 DATA DIVISION.
000900 WORKING-STORAGE SECTION.
001000  
001100 01  WS-TODAYS-DATE          pic x(08).
001200 01  WS-TIME.
001300     05 WS-TIME-HH           pic 9(02).
001400     05 WS-TIME-MM           PIC 9(02).
001500     05 WS-TIME-SS           PIC 9(02).
001600     05 WS-TIME-MS.
001700        10 WS-TIME-MS1       PIC 9(01).
001800        10 WS-TIME-MS2       PIC 9(01).
001900           88 RANDOM-ROCK              VALUE 0 1 2.     
002000           88 RANDOM-PAPER             VALUE 3 4 5.
002100           88 RANDOM-SCISSORS          VALUE 6 7 8.
002200           88 VALID-RANDOM-DIGIT       VALUE 0 THRU 8.
002300              
002400 01 WS-CHOICE                PIC X(01).
002500    88 ROCK-CHOSEN                     VALUE 'R' 'r'.
002600    88 PAPER-CHOSEN                    VALUE 'P' 'p'.
002700    88 SCISSORS-CHOSEN                 VALUE 'S' 's'.
002800    88 VALID-CHOICE                    VALUE 'R' 'r'
002900                                             'P' 'p'
003000                                             'S' 's'.
003100 PROCEDURE DIVISION.
003200 0000-MAINLINE.
003300     DISPLAY 'START RPSGAMRB BY RYAN BROOKS'.
003400     ACCEPT WS-TODAYS-DATE from date yyyymmdd.
003500     DISPLAY "PROGRAM EXECUTION DATE      : " WS-TODAYS-DATE.
003600     ACCEPT WS-TIME from time.
003700     DISPLAY "PROGRAM EXECUTION START TIME: " WS-TIME.
003800  
003900* PROMPT THE USER FOR GAME CHOICE UNTIL A VALID CHOICE IS MADE. 
004000     PERFORM UNTIL VALID-CHOICE
004100       DISPLAY "R FOR ROCK, P FOR PAPER, S FOR SCISSORS"
004200       ACCEPT WS-CHOICE
004300     END-PERFORM
004400* DISPLAY THE USERS CHOICE HERE WITH IF STATEMENTS.
004500* (YOU ADD THIS CODE):
004600  IF ROCK-CHOSEN  
004700             DISPLAY "YOU CHOSE ROCK, SOLID CHOICE."         
004800     ELSE    
004900         IF PAPER-CHOSEN 
005000             DISPLAY "YOU CHOSE PAPER, YOU HAVE THIS COVERED."
005100     ELSE
005200         IF SCISSORS-CHOSEN
005300             DISPLAY "YOU CHOSE SCISSORS, HOPE IT'S NOT A ROCK."
005400     END-IF
005500
005600
005700* QUASI-RANDOM NUMBER GENERATOR FOR GAME    
005800     MOVE 9 TO WS-TIME-MS2.
005900     PERFORM UNTIL VALID-RANDOM-DIGIT
006000       ACCEPT WS-TIME from time
006100       display "TIME DIGIT: " WS-TIME-MS2  
006200     END-PERFORM  
006300     
006400* WRITE CODE IF / EVALUATE TO DISPLAY THE COMPUTER CHOICE.
006500* (YOU ADD THIS CODE): 
006600  
006700  IF RANDOM-PAPER
006800      display "The computer chooses paper."
006900      IF ROCK-CHOSEN
007000          DISPLAY "Paper beats rock, computer wins."
007100      ELSE 
007200          IF SCISSORS-CHOSEN
007300              DISPLAY "Scissors win, paper loses; player wins."
007400      else
007500         if PAPER-CHOSEN
007600             DISPLAY "Tie!"
007700         END-IF
007800      END-IF
007900  ELSE
008000      IF RANDOM-ROCK
008100         display "The computer chooses rock."
008200         IF PAPER-CHOSEN
008300             DISPLAY "Paper wins equals you win."
008400         ELSE
008500             IF SCISSORS-CHOSEN 
008600                 DISPLAY "Rock crushes scissors, computer wins."
008700         ELSE
008800             if ROCK-CHOSEN
008900                 display "Tie!"
009000             END-IF
009100         END-IF
009200      END-IF
009300  ELSE
009400      IF RANDOM-SCISSORS
009500          display "The computer chooses scissors."
009600          IF ROCK-CHOSEN
009700              DISPLAY "You win, you crushed the scissors."
009800          ELSE
009900              IF PAPER-CHOSEN
010000                  DISPLAY "Scissors cuts paper, computer wins."
010100          ELSE
010200             if SCISSORS-CHOSEN
010300                 DISPLAY "Tie!"
010400             END-IF
010500          END-IF
010600      END-IF
010700  END-IF
010800* COMPARE THE USER INPUT TO THE RANDOM CHOICE
010900* RULES OF THE GAME ARE:
011000*   PAPER COVERS ROCK - PAPER WINS ROCK LOSES 
011100*   ROCK CRUSHES SCISSORS - ROCK WINS, SCISSORS LOSES
011200*   SCISSORS CUTS PAPER - SCISSORS WIN, PAPER LOSES
011300* USE THE EVALUTE TO DECLARE THE RESULTS.
011400* (YOU CODE THIS) 
011500     DISPLAY "Program exiting"
011600     GOBACK.