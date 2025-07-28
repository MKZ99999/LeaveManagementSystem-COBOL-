       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEAVEBAL-RETRIEVE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.


           SELECT BALANCE-FILE ASSIGN TO "../LEAVEBAL.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BAL-EMP-ID
               FILE STATUS IS BAL-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD BALANCE-FILE.
       01 BALANCE-RECORD.
           05 BAL-EMP-ID     PIC X(5).
           05 BAL-EMP-NAME   PIC X(20).
           05 BALANCE        PIC 99.

       WORKING-STORAGE SECTION.
       01 BAL-STATUS   PIC XX.
       01 END-FILE     PIC X VALUE "N".



       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           OPEN INPUT BALANCE-FILE
           IF BAL-STATUS NOT = "00"
               DISPLAY "Error opening file. Status: " BAL-STATUS
               STOP RUN
           END-IF

           DISPLAY "EMP-ID  NAME                 BALANCE"
           DISPLAY "-------------------------------"

           MOVE "00000" TO BAL-EMP-ID
           START BALANCE-FILE KEY IS >= BAL-EMP-ID
               INVALID KEY
                   DISPLAY "No data found."
                   MOVE "Y" TO END-FILE
           END-START


           PERFORM UNTIL END-FILE = "Y"
               READ BALANCE-FILE NEXT RECORD
                   AT END
                       MOVE "Y" TO END-FILE
                   NOT AT END
                       DISPLAY BAL-EMP-ID "   "
                               BAL-EMP-NAME "   "
                               BALANCE
               END-READ
           END-PERFORM

           CLOSE BALANCE-FILE
           DISPLAY "Retrieval complete"
           STOP RUN.
