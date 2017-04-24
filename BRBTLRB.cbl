000100 identification division.
000200 program-id. BRBTLRB.
000300 author. RYAN BROOKS.
000400
000500 data division.
000600 working-storage section.
000700 01 USR-INPUT   pic 99  VALUE 99.
000800 01 BOTTLES      pic 99.
000900 01 COUNTER     pic 99.
001000 01 ANY-KEY      PIC X(01).
001100
001200 procedure division.
001300*Prompt for number of bottles
001400     display "How many bottles are you counting down?".
001500     display "Enter a number between 1 and 99: ".
001600     accept USR-INPUT.
001700     display "".
001800*Set the correct number of bottles
001900     set BOTTLES to USR-INPUT.
002000     set COUNTER to BOTTLES.
002100*Write the song
002200*I know I could have done everything without four displays,
002300*but it was easier for me to follow the logic this way.
002400     perform until COUNTER equals 1
002500         display BOTTLES " bottles of beer on the wall, " with no advancing
002600         display BOTTLES " bottles of beer."
002700         set BOTTLES = BOTTLES - 1
002800         display "Take 1 down, pass it around, " with no advancing
002900         display BOTTLES " bottles of beer on the wall..."
003000         display ""
003100         set COUNTER = BOTTLES 
003200     end-perform.
003300*Write final verse
003400     display "1 bottle of beer on the wall," with no advancing.
003500     display "1 bottle of beer,".
003600     display "Take 1 down, pass it around, " with no advancing.
003700     display "no more bottles of beer on the wall...".
003800     display "Time to B Double E Double R U-N, BEER RUN!".
003900     display "".
004000*Prompt for exit
004100     display "Press any key to exit."
004200         accept ANY-KEY.
004300     goback.
