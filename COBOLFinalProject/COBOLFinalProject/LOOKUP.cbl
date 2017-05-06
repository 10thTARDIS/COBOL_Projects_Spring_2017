       IDENTIFICATION DIVISION.
       PROGRAM-ID LOOKUP as "SalesFile.LOOKUP".
       
      *THIS PROGRAM LOOKS UP SALES OR PRODUCT INFORMATION.
      *IT USES THE COPYBOOK LOOKUP.cpy AS THE PARAMETER LAYOUT
      *OF FIELDS PASSED TO AND SENT FROM THE PROGRAM.
      *
      *SET THE LOOKUP INDICATOR TO 'S' TO LOOK UP SALES DATA FOR
      *A SALES-ID.  SALES NAME AND RETURN CODE OF '0' IS RETURNED
      *IF THE LOOK UP IS SUCESSFUL.
      *
      *SET THE LOOKUP INDICATOR TO 'P' TO LOOK UP PRODUCT DATA FOR
      *A PRODUCT-ID.  PRODUCT NAME, PRICE AND RETURN CODE OF '0' 
      *IS RETURNED IF THE LOOK UP IS SUCESSFUL.
      *
      *A "K" IS RETURNED IF THE KEY IS NOT FOUND.
      *AN "I" IS RETURNED IF THE LOOK-UP TYPE IS NOT "S" OR "P".
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 SALES-LOOKUP-DATA.
          05 FILLER PIC X(18) VALUE '100ANNE TEAK      '.
          05 FILLER PIC X(18) VALUE '101BARRY CADE     '.
          05 FILLER PIC X(18) VALUE '102BARB DWYER     '.
          05 FILLER PIC X(18) VALUE '103CAM PAYNE      '.
          05 FILLER PIC X(18) VALUE '104MYRA MEINS     '.
          
       01 FILLER REDEFINES SALES-LOOKUP-DATA.  
          05 SALES-LOOKUP-ENTRY OCCURS 99 TIMES.
             10 SALES-ID-KEY            PIC X(03).
             10 SALES-NAME              PIC X(15).
             
       01 PRODUCT-LOOKUP-DATA.
          05 FILLER PIC X(20) VALUE '10001SLINKY    00300'.
          05 FILLER PIC X(20) VALUE '10002CHARGER   01400'.
          05 FILLER PIC X(20) VALUE '10003BELT      03100'.
          05 FILLER PIC X(20) VALUE '10004MOP       01200'.
          05 FILLER PIC X(20) VALUE '10005BASKET    02900'.
          
       01 FILLER REDEFINES PRODUCT-LOOKUP-DATA.  
          05 PRODUCT-LOOKUP-ENTRY OCCURS 99 TIMES.
             10 PRODUCT-ID-KEY          PIC X(05).
             10 PRODUCT-NAME            PIC X(10).   
             10 PRODUCT-PRICE           PIC 9(03)V99.
             
       01 TABLE-SUB                     PIC 9 VALUE 0.

       LINKAGE SECTION.
       copy "LOOKUP.cpy".

       PROCEDURE DIVISION USING WS-LOOKUP-FIELDS.
           
           DISPLAY "LOOKUP".
      *    DISPLAY WS-LOOKUP-FIELDS.
      *    DISPLAY WS-LOOKUP-TYPE.
           IF PRODUCT-LOOKUP 
             PERFORM 0200-FIND-PRODUCT
           ELSE
             IF SALES-LOOKUP
               PERFORM 0100-FIND-SALES-NAME
             ELSE
               MOVE 'I' TO WS-LOOKUP-TYPE.

           GOBACK. 
           
       0100-FIND-SALES-NAME.           
           
           PERFORM VARYING TABLE-SUB FROM 1 BY 1
               UNTIL WS-SALES-ID = SALES-ID-KEY(TABLE-SUB)
                  OR TABLE-SUB > 8
           END-PERFORM. 
           
           IF WS-SALES-ID = SALES-ID-KEY(TABLE-SUB)
             MOVE SALES-NAME(TABLE-SUB) TO WS-SALES-NAME
             MOVE '0' to WS-LOOKUP-TYPE
           ELSE 
             MOVE 'NOT FOUND' TO WS-SALES-NAME
             MOVE 'K'         TO WS-LOOKUP-TYPE.
             
       0200-FIND-PRODUCT.           
           
           PERFORM VARYING TABLE-SUB FROM 1 BY 1
               UNTIL WS-PRODUCT-ID = PRODUCT-ID-KEY(TABLE-SUB)
                  OR TABLE-SUB > 8
           END-PERFORM. 
           
           IF WS-PRODUCT-ID = PRODUCT-ID-KEY(TABLE-SUB)
             MOVE PRODUCT-NAME(TABLE-SUB) TO WS-PRODUCT-NAME
             MOVE PRODUCT-PRICE(TABLE-SUB) TO WS-PRODUCT-PRICE
             MOVE '0' to WS-LOOKUP-TYPE
           ELSE 
             MOVE 'NOT FOUND' TO WS-PRODUCT-NAME
             MOVE ZEROS       TO WS-PRODUCT-PRICE
             MOVE 'K'         TO WS-LOOKUP-TYPE.             