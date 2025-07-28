      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LEAVE-FILE ASSIGN TO "../LEAVE.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LEAVE-REQ-ID
               FILE STATUS IS LEAVE-STATUS-CODE.
       DATA DIVISION.
       FILE SECTION.
       FD LEAVE-FILE.
       01 LEAVE-RECORD.
           05 LEAVE-REQ-ID PIC X(8).
           05 LEAVE-EMP-ID PIC X(5).
           05 LEAVE-DATE PIC 9(8).
           05 LEAVE-REASON PIC X(50).
           05 LEAVE-STATUS PIC X(8).
           05 LEAVE-MANAGER-ID PIC X(5).
           05 LEAVE-DECISION-DATE PIC 9(8).

       WORKING-STORAGE SECTION.
       01 LEAVE-STATUS-CODE PIC XX.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            OPEN OUTPUT LEAVE-FILE
            IF LEAVE-STATUS-CODE = "00"
               DISPLAY "Leave-file created successfully."
            ELSE
                DISPLAY "Failed to create leave.dat status:"
                LEAVE-STATUS-CODE
            END-IF
            CLOSE LEAVE-FILE

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
