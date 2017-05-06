      * LOOKUP 
        01 WS-LOOKUP-FIELDS.
          05 WS-LOOKUP-TYPE             PIC X(01).
             88 PRODUCT-LOOKUP                    VALUE 'P'.
             88 SALES-LOOKUP                      VALUE 'S'.
             88 INVALID-LOOKUP-TYPE               VALUE 'I'.
             88 INVALID-LOOKUP-KEY                VALUE 'K'.
             88 SUCESSFUL-LOOKUP                  VALUE '0'.
          05 WS-LOOKUP-AREA.
             10 WS-SALES-ID             PIC X(03)    VALUE SPACES.
             10 WS-PRODUCT-ID           PIC X(05)    VALUE SPACES.
             10 WS-SALES-NAME           PIC X(15)    VALUE SPACES.
             10 WS-PRODUCT-NAME         PIC X(10)    VALUE SPACES.
             10 WS-PRODUCT-PRICE-X      PIC X(07)    VALUE SPACES.
             10 WS-PRODUCT-PRICE REDEFINES
                WS-PRODUCT-PRICE-X      PIC 9(05)V99.