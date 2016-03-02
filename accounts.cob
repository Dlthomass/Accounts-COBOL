       IDENTIFICATION DIVISION. 
       PROGRAM-ID. Accounts. 
      * 
      *    THIS PROGRAM COMPUTES AND PRINTS THE NEW YEAR-TO-DATE   
      *    SALES, RETURNS, AND NET FOR EACH SALESPERSON IN THE     
      *    SALESPERSON FILE.  THE PROGRAM ALSO ACCUMULATES AND   
      *    PRINTS THE TOTAL NEW YEAR-TO-DATE SALES, RETURNS, AND 
      *    NET FOR ALL SALESPEOPLE.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. XYZ-1.
       OBJECT-COMPUTER. XYZ-1.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE    ASSIGN TO "RECD1.dat"
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-FILE      ASSIGN TO "ACCTRPT.OUT"
                   ORGANIZATION IS LINE SEQUENTIAL.
      * 
       DATA DIVISION. 
       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-DATA               PIC X(101). 
       FD  REPORT-FILE.
       01  REPORT-DATA                 PIC X(132).
       WORKING-STORAGE SECTION. 
       01  WS-FLAGS.
           05  WS-EOF-FLAG             PIC X. 
       01  WS-COUNTERS.
           05  WS-LINE-COUNTER         PIC 99.
           05  WS-PAGE-COUNTER         PIC 99.
       01  WS-COMPUTATIONAL-FIELDS. 
           05  WS-PREV-BALANCE         PIC 9(6)V99. 
           05  WS-CUR-PURCHASES        PIC 9(6)V99. 
           05  WS-CUR-PAYCREDS         PIC 9(6)V99.
           05  WS-CUR-BALANCE          PIC 9(6)V99.
       01  WS-TOTALING-FIELDS.
           05  WS-TOTAL-PREV-BALANCE   PIC 9(7)V99.
           05  WS-TOTAL-CUR-PURCHASES  PIC 9(7)V99.                      
           05  WS-TOTAL-CUR-PAYCREDS   PIC 9(7)V99. 
           05  WS-TOTAL-CUR-BALANCE    PIC 9(7)V99.           
       01  CUSTOMER-RECORD. 
           05  CU-NUMBER               PIC XXXXX.
           05  CU-NAME                 PIC X(18). 
           05  FILLER                  PIC X(44).
           05  CU-PREV-BALANCE         PIC 9(6)V99.
           05  CU-CUR-PURCHASES        PIC 9(6)V99.
           05  CU-CUR-PAYMENTS         PIC 9(6)V99. 
           05  CU-CUR-CREDITS          PIC 9(6)V99. 
           05  FILLER                  PIC XX.
       01  REPORT-TITLE-LINE.
           05  FILLER                  PIC X(20)  VALUE SPACES.
           05  FILLER                  PIC X(43)  VALUE
                   "CHRIS' BODACIOUS ACCOUNTS RECIEVABLE REPORT".
           05  FILLER                  PIC X(13)  VALUE SPACES.
           05  FILLER                  PIC X(5)   VALUE "PAGE ".
           05  PAGE-NUM                PIC ZZ.
           05  FILLER                  PIC X(49)  VALUE SPACES.
       01  COLUMN-HEADING-LINE-1.
           05  FILLER                  PIC X      VALUE SPACES.
           05  FILLER                  PIC X(8)   VALUE
                   "CUSTOMER".
           05  FILLER                  PIC X(26)  VALUE SPACES.
           05  FILLER                  PIC X(24)  VALUE
                   "PREVIOUS         CURRENT".
           05  FILLER                  PIC X(7)   VALUE SPACES.
           05  FILLER                  PIC X(23)  VALUE
                   "CURRENT         CURRENT".
           05  FILLER                  PIC X(43)  VALUE SPACES.
       01  COLUMN-HEADING-LINE-2.
           05  FILLER                  PIC XX     VALUE SPACES.
           05  FILLER                  PIC X(24)  VALUE 
                   "NUMBER     CUSTOMER NAME".
           05  FILLER                  PIC X(9)   VALUE SPACES.
           05  FILLER                  PIC X(25)  VALUE
                   "BALANCE         PURCHASES".
           05  FILLER                  PIC X(5)   VALUE SPACES.
           05  FILLER                  PIC X(24)  VALUE
                   "PMTS/CRDTS       BALANCE".
           05  FILLER                  PIC X(43)  VALUE SPACES.
       01  DETAIL-LINE. 
           05  FILLER                  PIC X(3)   VALUE SPACES. 
           05  DL-NUMBER               PIC X(5).
           05  FILLER                  PIC X(3)   VALUE SPACES. 
           05  DL-NAME                 PIC X(18). 
           05  FILLER                  PIC X(4)   VALUE SPACES. 
           05  DL-PREV-BALANCE         PIC ZZZ,ZZZ.99. 
           05  FILLER                  PIC XX     VALUE "CR". 
           05  FILLER                  PIC X(5)   VALUE SPACES. 
           05  DL-CUR-PURCHASES        PIC ZZZ,ZZZ.99.
           05  FILLER                  PIC X(5)   VALUE SPACES.
           05  DL-CUR-PAYCREDS         PIC ZZZ,ZZZ.99.
           05  FILLER                  PIC X(5)   VALUE SPACES.
           05  DL-CUR-BALANCE          PIC ZZZ,ZZZ.99.
           05  FILLER                  PIC XX     VALUE "CR". 
           05  FILLER                  PIC X(40)  VALUE SPACES. 
       01  TOTAL-LINE.
           05  FILLER                  PIC X(24)  VALUE SPACES.
           05  FILLER                  PIC X(7)   VALUE "TOTALS ".
           05  TL-TOTAL-PREV-BALANCE   PIC Z,ZZZ,ZZZ.99.
           05  FILLER                  PIC X(5)   VALUE "CR   ".
           05  TL-TOTAL-CUR-PURCHASES  PIC Z,ZZZ,ZZZ.99.
           05  FILLER                  PIC XXX    VALUE SPACES.
           05  TL-TOTAL-CUR-PAYCREDS   PIC Z,ZZZ,ZZZ.99.
           05  FILLER                  PIC XXX    VALUE SPACES.
           05  TL-TOTAL-CUR-BALANCE    PIC Z,ZZZ,ZZZ.99.
           05  FILLER                  PIC XX     VALUE "CR".
           05  FILLER                  PIC X(40)  VALUE SPACES.
      *                                       
       PROCEDURE DIVISION.
      * 
       A000-MAIN-CONTROL.
           OPEN INPUT CUSTOMER-FILE 
               OUTPUT REPORT-FILE
           PERFORM B010-INITIALIZE-WORKING-DATA
           PERFORM B020-WRITE-HEADINGS
           PERFORM B030-READ-INPUT
           PERFORM B040-PRODUCE-REPORT-BODY
               UNTIL WS-EOF-FLAG IS EQUAL TO "Y"
           PERFORM B050-WRITE-TOTALS
           CLOSE CUSTOMER-FILE, REPORT-FILE
           STOP RUN.
      * 
       B010-INITIALIZE-WORKING-DATA.
           MOVE "N" TO WS-EOF-FLAG
           MOVE ZERO TO WS-PAGE-COUNTER
           MOVE ZERO TO WS-TOTAL-PREV-BALANCE
           MOVE ZERO TO WS-TOTAL-CUR-PURCHASES
           MOVE ZERO TO WS-TOTAL-CUR-PAYCREDS
           MOVE ZERO TO WS-TOTAL-CUR-BALANCE.
      *
       B020-WRITE-HEADINGS.
           ADD 1 TO WS-PAGE-COUNTER
           MOVE WS-PAGE-COUNTER TO PAGE-NUM
           WRITE REPORT-DATA FROM REPORT-TITLE-LINE
               AFTER ADVANCING PAGE
           WRITE REPORT-DATA FROM COLUMN-HEADING-LINE-1
               AFTER ADVANCING 2 LINES
           WRITE REPORT-DATA FROM COLUMN-HEADING-LINE-2
               AFTER ADVANCING 1 LINE
           MOVE SPACES TO REPORT-DATA
           WRITE REPORT-DATA
               AFTER ADVANCING 1 LINE
           MOVE ZERO TO WS-LINE-COUNTER.
      *
       B030-READ-INPUT.
           READ CUSTOMER-FILE INTO CUSTOMER-RECORD
               AT END MOVE "Y" TO WS-EOF-FLAG
           END-READ.
      *
       B040-PRODUCE-REPORT-BODY.
           IF WS-LINE-COUNTER IS GREATER THAN 24
      * Had to move the WRITE-HEADINGS section here
               ADD 1 TO WS-PAGE-COUNTER
               MOVE WS-PAGE-COUNTER TO PAGE-NUM
               WRITE REPORT-DATA FROM REPORT-TITLE-LINE
                   AFTER ADVANCING PAGE
               WRITE REPORT-DATA FROM COLUMN-HEADING-LINE-1
                   AFTER ADVANCING 2 LINES
               WRITE REPORT-DATA FROM COLUMN-HEADING-LINE-2
                   AFTER ADVANCING 1 LINE
               MOVE SPACES TO REPORT-DATA
               WRITE REPORT-DATA
                   AFTER ADVANCING 1 LINE
               MOVE ZERO TO WS-LINE-COUNTER
           END-IF
           PERFORM C010-CALCULATE-VALUES
           PERFORM C020-ACCUMULATE-TOTALS
           PERFORM C030-WRITE-DETAIL-OUTPUT
           ADD 1 TO WS-LINE-COUNTER
           PERFORM B030-READ-INPUT.
      * 
       B050-WRITE-TOTALS.
           MOVE WS-TOTAL-PREV-BALANCE TO TL-TOTAL-PREV-BALANCE
           MOVE WS-TOTAL-CUR-PURCHASES TO TL-TOTAL-CUR-PURCHASES
           MOVE WS-TOTAL-CUR-PAYCREDS TO TL-TOTAL-CUR-PAYCREDS
           MOVE WS-TOTAL-CUR-BALANCE TO TL-TOTAL-CUR-BALANCE
           WRITE REPORT-DATA FROM TOTAL-LINE
               AFTER ADVANCING 3 LINES.
      * 
       C010-CALCULATE-VALUES.
           MOVE CU-PREV-BALANCE TO WS-PREV-BALANCE
           MOVE CU-CUR-PURCHASES TO WS-CUR-PURCHASES
           ADD CU-CUR-PAYMENTS, CU-CUR-CREDITS
               GIVING WS-CUR-PAYCREDS
           ADD WS-PREV-BALANCE, WS-CUR-PURCHASES
               GIVING WS-CUR-BALANCE
           SUBTRACT WS-CUR-PAYCREDS FROM WS-CUR-BALANCE.
      *
       C020-ACCUMULATE-TOTALS.
           ADD WS-PREV-BALANCE TO WS-TOTAL-PREV-BALANCE
           ADD WS-CUR-PURCHASES TO WS-TOTAL-CUR-PURCHASES
           ADD WS-CUR-PAYCREDS TO WS-TOTAL-CUR-PAYCREDS 
           ADD WS-CUR-BALANCE TO WS-TOTAL-CUR-BALANCE.   
      * 
       C030-WRITE-DETAIL-OUTPUT.
           MOVE CU-NUMBER TO DL-NUMBER
           MOVE CU-NAME TO DL-NAME
           MOVE WS-PREV-BALANCE TO DL-PREV-BALANCE
           MOVE WS-CUR-PURCHASES TO DL-CUR-PURCHASES 
           MOVE WS-CUR-PAYCREDS TO DL-CUR-PAYCREDS
           MOVE WS-CUR-BALANCE TO DL-CUR-BALANCE
           WRITE REPORT-DATA FROM DETAIL-LINE
               AFTER ADVANCING 1 LINE.
