
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEAVEBAL-INIT.

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
           05 BAL-EMP-ID PIC X(5).
           05 BAL-EMP-NAME PIC X(20).
           05 BAL-ANNUAL PIC 99.
           05 BAL-SICK PIC 99.
           05 BAL-CASUAL PIC 99.

       WORKING-STORAGE SECTION.
       01 BAL-STATUS PIC XX.
       01 CHOICE PIC 9.
       01 TEMP-ID PIC X(5).
       01 TEMP-NAME PIC X(20).
       01 DEFAULT-ANNUAL PIC 99 VALUE 20.
       01 DEFAULT-SICK PIC 99 VALUE 12.
       01 DEFAULT-CASUAL PIC 99 VALUE 8.
       01 END-FILE PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN I-O BALANCE-FILE

           IF BAL-STATUS NOT = "00"
               DISPLAY "LEAVEBAL.DAT not found. Creating file..."
               OPEN OUTPUT BALANCE-FILE
               CLOSE BALANCE-FILE
               OPEN I-O BALANCE-FILE
               IF BAL-STATUS NOT = "00"
                   DISPLAY "Failed to open. Status: " BAL-STATUS
                   STOP RUN
               END-IF
           END-IF

           PERFORM UNTIL CHOICE = 4
               DISPLAY SPACE
               DISPLAY "***** Employee Management *****"
               DISPLAY "Select option:"
               DISPLAY "+----------------------+"
               DISPLAY "| 1 - Add new employee |"
               DISPLAY "| 2 - Delete employee  |"
               DISPLAY "| 3 - View all balances|"
               DISPLAY "| 4 - Exit             |"
               DISPLAY "+----------------------+"
               DISPLAY "Your choice: "
               ACCEPT CHOICE

               EVALUATE CHOICE
                   WHEN 1
                       DISPLAY "Enter employee ID: "
                       ACCEPT TEMP-ID
                       MOVE TEMP-ID TO BAL-EMP-ID

                       READ BALANCE-FILE
                       INVALID KEY

                       DISPLAY "Enter employee name: "
                       ACCEPT TEMP-NAME

                       MOVE TEMP-NAME TO BAL-EMP-NAME
                       MOVE DEFAULT-ANNUAL TO BAL-ANNUAL
                       MOVE DEFAULT-SICK TO BAL-SICK
                       MOVE DEFAULT-CASUAL TO BAL-CASUAL

                       WRITE BALANCE-RECORD INVALID KEY
                           DISPLAY "Record already exists."
                       NOT INVALID KEY
                           DISPLAY "Added with default leave balances:"
                           DISPLAY " - Annual: " BAL-ANNUAL
                           DISPLAY " - Sick:   " BAL-SICK
                           DISPLAY " - Casual: " BAL-CASUAL
                       END-WRITE

                       NOT INVALID KEY
                           DISPLAY "Record already exists."

                   WHEN 2
                       DISPLAY "Enter employee ID to delete: "
                       ACCEPT TEMP-ID
                       MOVE TEMP-ID TO BAL-EMP-ID

                       READ BALANCE-FILE INVALID KEY
                           DISPLAY "Employee ID not found."
                       NOT INVALID KEY
                           DELETE BALANCE-FILE
                           DISPLAY "Employee deleted successfully."
                       END-READ

                   WHEN 3
                       MOVE "00000" TO BAL-EMP-ID
                       MOVE "N" TO END-FILE

                       START BALANCE-FILE KEY IS >= BAL-EMP-ID
                           INVALID KEY
                               DISPLAY "No Employee Found."
                               MOVE "Y" TO END-FILE
                       END-START

                       IF END-FILE NOT = "Y"
                       DISPLAY "ID     NAME                  ANNUAL-LEAVE  SICK-LEAVE  CASUAL-LEAVE"
                       DISPLAY "-------------------------------------------------------------------"
                           PERFORM UNTIL END-FILE = "Y"
                               READ BALANCE-FILE NEXT RECORD
                                   AT END
                                       MOVE "Y" TO END-FILE
                                   NOT AT END
                                       DISPLAY BAL-EMP-ID "  "
                                               BAL-EMP-NAME "       "
                                               BAL-ANNUAL "          "
                                               BAL-SICK "            "
                                               BAL-CASUAL "          "
                               END-READ
                           END-PERFORM
                       END-IF

                   WHEN 4
                   DISPLAY "Exiting program. Goodbye!"

                   WHEN OTHER
                   DISPLAY "Invlaid choice. Enter 1,2,3 or 4."
               END-EVALUATE
           END-PERFORM
           CLOSE BALANCE-FILE

           STOP RUN.
