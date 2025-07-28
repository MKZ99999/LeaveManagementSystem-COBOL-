
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

           SELECT BALANCE-FILE ASSIGN TO "../LEAVEBAL.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BAL-EMP-ID
               FILE STATUS IS BAL-STATUS.

           SELECT MANAGER-FILE ASSIGN TO "../manager.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS MANAGER-ID
               FILE STATUS IS MANAGER-STATUS.
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
           05 LEAVE-DECISION-DATE PIC X(8).
           05 LEAVE-TYPE PIC X(10).

       FD BALANCE-FILE.
       01 BALANCE-RECORD.
           05 BAL-EMP-ID PIC X(5).
           05 BAL-EMP-NAME PIC X(20).
           05 BAL-ANNUAL PIC 99.
           05 BAL-SICK PIC 99.
           05 BAL-CASUAL PIC 99.

       FD MANAGER-FILE.
       01 MANAGER-RECORD.
           05 MANAGER-ID PIC X(5).
           05 MANAGER-NAME PIC X(20).
           05 MANAGER-PASSWORD PIC X(20).

       WORKING-STORAGE SECTION.
       01 BAL-STATUS PIC XX.
       01 LEAVE-STATUS-CODE PIC XX.
       01 USER-ROLE PIC 9 VALUE 0.
          88 EMPLOYEE VALUE 1.
          88 MANAGER VALUE 2.

       01 USER-CHOICE PIC 9 VALUE 0.
       01 MANAGER-CHOICE PIC 9 VALUE 0.
       01 LOOP-FLAG PIC X VALUE 'T'.

       01 TEMP-EMP-ID PIC X(5).
       01 TEMP-LEAVE-DATE PIC 9(8).
       01 TEMP-REASON PIC X(50).
       01 REQ-ID PIC X(8).
       01 APPLY-AGAIN PIC X VALUE 'Y'.
       01 FOUND-FLAG PIC X VALUE 'N'.

       01 TEMP-MGR-ID PIC X(5).
       01 TODAY-DATE PIC 9(8).
       01 FOUND-PENDING PIC X VALUE 'N'.
       01 DECISION-INPUT PIC X.
       01 DUMMY-INPUT PIC X.
       01 FOUND-RECORD PIC X VALUE 'N'.
       01 VALID-DATE-FLAG PIC X VALUE 'Y'.
       01 CURRENT-DATE-VAL PIC 9(8).
       01 CURRENT-YEAR-VAL PIC 9(4).

       01 MANAGER-STATUS PIC XX.
       01 AUTH-MGR-ID PIC X(5).
       01 AUTH-MGR-PWD PIC X(20).
       01 AUTH-SUCCESS-FLAG PIC X VALUE "N".

       01 WS-TEMP-INT-DATE PIC S9(9) COMP.

       01 TEMP-START-DATE      PIC 9(8).
       01 TEMP-END-DATE        PIC 9(8).
       01 WS-CURRENT-LOOP-DATE PIC 9(8).
       01 WS-NUMERIC-START     PIC 9(8).
       01 WS-NUMERIC-END       PIC 9(8).
       01 WS-DAYS-DIFF         PIC 9(4) COMP.
       01 WS-INTEGER-DATE      PIC S9(9) COMP.
       01 WS-DATE-FOUND-FLAG   PIC X VALUE 'N'.
       01 DUMMY-KEY PIC X(8) VALUE LOW-VALUES.

       01 TEMP-LEAVE-TYPE PIC 9.
       01 WS-LEAVE-TYPE-TEXT PIC X(10).

       01 WS-REQUESTED-DAYS PIC 99.
       01 MAX-ANNUAL-DAYS PIC 99 VALUE 10.
       01 MAX-SICK-DAYS PIC 99 VALUE 5.
       01 MAX-CASUAL-DAYS PIC 99 VALUE 3.



       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM START-PROGRAM
           PERFORM ROLE-SELECTION
           PERFORM DISPLAY-MENU
           PERFORM EXIT-PROGRAM
           STOP RUN.

           START-PROGRAM.
           DISPLAY "+-----------------------------------+"
           DISPLAY "|  START LEAVE MANAGEMENT SYSTEM    |"
           DISPLAY "+-----------------------------------+".

           OPEN I-O LEAVE-FILE
           IF LEAVE-STATUS-CODE NOT = "00"
               DISPLAY "Error opening LEAVE.DAT. Status: "
               LEAVE-STATUS-CODE
               STOP RUN
           END-IF

           OPEN I-O BALANCE-FILE
           IF BAL-STATUS NOT = '00'
               DISPLAY "Error opening LEAVEBAL.DAT. Status: " BAL-STATUS
               STOP RUN
           END-IF

           OPEN I-O MANAGER-FILE
           IF MANAGER-STATUS NOT = "00"
               DISPLAY "Error opening MANAGER.DAT. Status: "
               MANAGER-STATUS
               STOP RUN
           END-IF.

           ROLE-SELECTION.
           PERFORM UNTIL USER-ROLE = 1 OR USER-ROLE = 2
               DISPLAY "+--------------------------+"
               DISPLAY "|  Prompt for Role:        |"
               DISPLAY "|  1. Employee  2. Manager |"
               DISPLAY "+--------------------------+"
               DISPLAY "Enter your role (1 or 2): "
               ACCEPT USER-ROLE

               IF USER-ROLE NOT = 1 AND USER-ROLE NOT = 2
                   DISPLAY "Invalid input. Please enter 1 or 2."
               ELSE IF MANAGER
                   PERFORM AUTHENTICATE-MANAGER
                   IF AUTH-SUCCESS-FLAG = "N"
                       DISPLAY "Authentication failed. Try again."
                       MOVE 0 TO USER-ROLE
                   END-IF
               END-IF
           END-PERFORM.

           AUTHENTICATE-MANAGER.
               MOVE 'N' TO AUTH-SUCCESS-FLAG
               DISPLAY "+--------------------+"
               DISPLAY "| Manager Login      |"
               DISPLAY "+--------------------+"
               DISPLAY "Enter Manger ID: "
               ACCEPT AUTH-MGR-ID
               DISPLAY "Enter Password: "
               ACCEPT AUTH-MGR-PWD

               MOVE AUTH-MGR-ID TO MANAGER-ID
               READ MANAGER-FILE
                   INVALID KEY
                       DISPLAY "Invalid manager ID."
                   NOT INVALID KEY
                       IF FUNCTION TRIM(AUTH-MGR-PWD)=FUNCTION
                                          TRIM(MANAGER-PASSWORD)
                           DISPLAY "Log in successful."
                           MOVE 'Y' TO AUTH-SUCCESS-FLAG
                       ELSE
                           DISPLAY "Incorrect password."
                       END-IF
                END-READ.

           DISPLAY-MENU.
            PERFORM UNTIL LOOP-FLAG = 'F'

            IF MANAGER
                DISPLAY "+-----------------------------+"
                DISPLAY "|       Manager Menu          |"
                DISPLAY "+-----------------------------+"
                DISPLAY "| 1. Approve/Reject Leave     |"
                DISPLAY "| 2. Generate Report          |"
                DISPLAY "| 3. View Leave Balances      |"
                DISPLAY "| 4. Employee Leave Taken Data|"
                DISPLAY "| 5. Exit                     |"
                DISPLAY "+-----------------------------+"
                DISPLAY "Enter your choice (1-5): "
                ACCEPT MANAGER-CHOICE
            ELSE
               DISPLAY "+-----------------------------+"
               DISPLAY "|      Employee Menu          |"
               DISPLAY "+-----------------------------+"
               DISPLAY "| 1. Apply for Leave          |"
               DISPLAY "| 2. Generate Report          |"
               DISPLAY "| 3. View My Leave Balance    |"
               DISPLAY "| 4. Exit                     |"
               DISPLAY "+-----------------------------+"
               DISPLAY "Enter your choice (1-4): "
               ACCEPT USER-CHOICE
               END-IF
               PERFORM HANDLE-ACTION
            END-PERFORM.

            HANDLE-ACTION.
                IF MANAGER
                    EVALUATE MANAGER-CHOICE
                       WHEN 1
                           PERFORM APPROVE-REJECT-LEAVE
                       WHEN 2
                           PERFORM GENERATE-REPORT
                       WHEN 3
                           PERFORM DISPLAY-LEAVE-BALANCES
                       WHEN 4
                           PERFORM EMPLOYEE-LEAVE-TAKEN-DATA
                       WHEN 5
                           MOVE 'F' TO LOOP-FLAG
                       WHEN OTHER
                           DISPLAY "Invalid manager choice."
                     END-EVALUATE
                ELSE
                    EVALUATE USER-CHOICE
                       WHEN 1
                           PERFORM APPLY-LEAVE
                       WHEN 2
                           PERFORM GENERATE-REPORT
                       WHEN 3
                           PERFORM DISPLAY-LEAVE-BALANCES
                       WHEN 4
                           MOVE 'F' TO LOOP-FLAG
                       WHEN OTHER
                           DISPLAY "Invalid employee choice."
                    END-EVALUATE
                END-IF.

            APPLY-LEAVE.
               CLOSE LEAVE-FILE
               OPEN I-O LEAVE-FILE
               IF LEAVE-STATUS-CODE NOT = "00"
                   DISPLAY "Error opening LEAVE.DAT. Status: "
                   LEAVE-STATUS-CODE
                   GO TO APPROVE-REJECT-CLEANUP
               END-IF

               move spaces to TEMP-EMP-ID
               PERFORM UNTIL TEMP-EMP-ID NOT = SPACES
                   DISPLAY "Enter Your Employee ID: "
                   ACCEPT TEMP-EMP-ID
                   IF TEMP-EMP-ID = SPACES
                       DISPLAY "Error:ID cannot be blank."
                   END-IF
               END-PERFORM

               MOVE TEMP-EMP-ID TO BAL-EMP-ID
               READ BALANCE-FILE INVALID KEY
                   DISPLAY "Employee ID not found. Application denied."
                   DISPLAY "Press enter to return to menu..."
                   ACCEPT DUMMY-INPUT
                   EXIT PARAGRAPH
               END-READ

               MOVE 0 TO TEMP-LEAVE-TYPE
               PERFORM UNTIL TEMP-LEAVE-TYPE >= 1 AND
               TEMP-LEAVE-TYPE <= 3
               DISPLAY "+--------------------+"
               DISPLAY "| Select Leave Type  |"
               DISPLAY "+--------------------+"
               DISPLAY "| 1. Annual Leave    |"
               DISPLAY "| 2. Sick Leave      |"
               DISPLAY "| 3. Casual Leave    |"
               DISPLAY "+--------------------+"
               DISPLAY "Enter leave type (1-3): "
               ACCEPT TEMP-LEAVE-TYPE
               EVALUATE TEMP-LEAVE-TYPE
               WHEN 1
                   MOVE "ANNUAL" TO WS-LEAVE-TYPE-TEXT
                   IF BAL-ANNUAL = 0
                       DISPLAY "Cannot apply for annual leave.Balance:0"
                       DISPLAY "Press Enter to return menu...."
                       ACCEPT DUMMY-INPUT
                       EXIT PARAGRAPH
                   ELSE IF BAL-ANNUAL < 5
                       DISPLAY "!Annual leave balance low("BAL-ANNUAL")"
                   END-IF
               WHEN 2
                   MOVE "SICK"  to WS-LEAVE-TYPE-TEXT
                   IF BAL-SICK = 0
                       DISPLAY "Cannot apply for sick leave.Balance: 0."
                       DISPLAY "Press Enter to return menu...."
                       ACCEPT DUMMY-INPUT
                       EXIT PARAGRAPH
                   ELSE IF BAL-SICK < 5
                       DISPLAY "!Your sick balance is low("BAL-SICK")!"
                   END-IF
               WHEN 3
                   MOVE "CASUAL" TO WS-LEAVE-TYPE-TEXT
                   IF BAL-CASUAL = 0
                       DISPLAY "Cannot apply for Casual leave.Balance:0"
                       DISPLAY "Press Enter to return menu...."
                       ACCEPT DUMMY-INPUT
                       EXIT PARAGRAPH
                   ELSE IF BAL-CASUAL < 5
                       DISPLAY "!!Casual balance is  low("BAL-CASUAL")!"
                   END-IF
               WHEN OTHER
                   DISPLAY "Invalid leave type. Please enter 1,2 or 3."
               END-EVALUATE
            END-PERFORM

               MOVE FUNCTION CURRENT-DATE(1:8) TO CURRENT-DATE-VAL
               MOVE CURRENT-DATE-VAL(1:4) TO CURRENT-YEAR-VAL
               MOVE 'Y' TO APPLY-AGAIN

               PERFORM UNTIL APPLY-AGAIN NOT = 'Y'
                   MOVE '00' TO LEAVE-STATUS-CODE
                   MOVE 'N' TO FOUND-FLAG
                   MOVE 'N' TO VALID-DATE-FLAG

               PERFORM UNTIL VALID-DATE-FLAG = 'Y'
                   DISPLAY "Enter Start Date(YYYYMMDD): "
                   ACCEPT TEMP-START-DATE
                   DISPLAY "Enter End Date(YYYYMMDD): "
                   ACCEPT TEMP-END-DATE

                   IF TEMP-START-DATE IS NUMERIC AND
                      TEMP-END-DATE IS NUMERIC AND
                       LENGTH OF FUNCTION TRIM(TEMP-START-DATE)= 8 AND
                       LENGTH OF FUNCTION TRIM(TEMP-END-DATE)= 8
                       COMPUTE WS-NUMERIC-START =
                       FUNCTION INTEGER-OF-DATE(TEMP-START-DATE)
                       COMPUTE WS-NUMERIC-END =
                       FUNCTION INTEGER-OF-DATE(TEMP-END-DATE)
                       IF WS-NUMERIC-START = 0 OR WS-NUMERIC-END = 0
                           DISPLAY "Error: Invalid calendar date."
                       ELSE
                       IF TEMP-START-DATE > TEMP-END-DATE
                           DISPLAY "Start date cannot be after End Date!!."
                       ELSE
                       IF TEMP-START-DATE < CURRENT-DATE-VAL
                           DISPLAY "Error: Leave date can't be in past."
                       ELSE
                           IF TEMP-START-DATE(1:4) NOT= CURRENT-YEAR-VAL
                           OR TEMP-END-DATE(1:4) NOT= CURRENT-YEAR-VAL
                           DISPLAY "Leave must be within current year!!"
                       ELSE
                           COMPUTE WS-REQUESTED-DAYS =
                           (FUNCTION INTEGER-OF-DATE(TEMP-END-DATE) -
                            FUNCTION INTEGER-OF-DATE(TEMP-START-DATE))
                            + 1
                       EVALUATE WS-LEAVE-TYPE-TEXT
                       WHEN "ANNUAL"
                       IF WS-REQUESTED-DAYS > MAX-ANNUAL-DAYS
                        DISPLAY "Error:annual request exceeds 10 days."
                        DISPLAY "Requested: "WS-REQUESTED-DAYS" days."
                       ELSE
                           MOVE 'Y' TO VALID-DATE-FLAG
                       END-IF
                       WHEN "SICK"
                       IF WS-REQUESTED-DAYS > MAX-SICK-DAYS
                        DISPLAY "Error:sick request exceeds 5 days."
                        DISPLAY "Requested: "WS-REQUESTED-DAYS" days."
                       ELSE
                           MOVE 'Y' TO VALID-DATE-FLAG
                       END-IF
                       WHEN "CASUAL"
                       IF WS-REQUESTED-DAYS > MAX-CASUAL-DAYS
                        DISPLAY "Error:casual request exceeds 3 days."
                        DISPLAY "Requested: "WS-REQUESTED-DAYS" days."
                       ELSE
                           MOVE 'Y' TO VALID-DATE-FLAG
                       END-IF
                       WHEN OTHER
                         DISPLAY "Error:Unknown leave type."
                       END-EVALUATE
                     END-IF
                   END-IF
                   END-IF
                ELSE
                   DISPLAY "Error: Use YYYYMMDD format for dates."
                   END-IF
               END-PERFORM

               DISPLAY "Enter reason for leave: "
               ACCEPT TEMP-REASON

               MOVE TEMP-START-DATE TO WS-CURRENT-LOOP-DATE

               PERFORM UNTIL WS-CURRENT-LOOP-DATE > TEMP-END-DATE
                   MOVE 'N' to FOUND-FLAG
                   MOVE DUMMY-KEY TO LEAVE-REQ-ID
                   START LEAVE-FILE KEY >= LEAVE-REQ-ID
                   INVALID KEY CONTINUE
                   END-START

               PERFORM UNTIL LEAVE-STATUS-CODE NOT = '00'
                   READ LEAVE-FILE NEXT RECORD
                       AT END
                           MOVE '99' TO LEAVE-STATUS-CODE
                       NOT AT END
                           IF LEAVE-EMP-ID = TEMP-EMP-ID AND
                               LEAVE-DATE = WS-CURRENT-LOOP-DATE
                               MOVE 'Y' TO FOUND-FLAG
                               EXIT PERFORM
                           END-IF
                   END-READ
               END-PERFORM

               IF FOUND-FLAG = 'Y'
                   DISPLAY "Leave already applied for: "
                   WS-CURRENT-LOOP-DATE
               ELSE
                   MOVE TEMP-EMP-ID(4:2) TO REQ-ID(1:2)
                   MOVE WS-CURRENT-LOOP-DATE(3:6) TO REQ-ID(3:6)

                   MOVE REQ-ID TO LEAVE-REQ-ID

                   MOVE TEMP-EMP-ID TO LEAVE-EMP-ID
                   MOVE WS-CURRENT-LOOP-DATE TO LEAVE-DATE
                   MOVE TEMP-REASON TO LEAVE-REASON
                   MOVE WS-LEAVE-TYPE-TEXT TO LEAVE-TYPE
                   MOVE "APPLIED" TO LEAVE-STATUS
                   MOVE SPACES TO LEAVE-MANAGER-ID
                   MOVE ZEROS TO LEAVE-DECISION-DATE

                   WRITE LEAVE-RECORD INVALID KEY
                       DISPLAY "Error saving leave on : "
                       WS-CURRENT-LOOP-DATE
                   NOT INVALID KEY
                       DISPLAY "Leave applied: " WS-CURRENT-LOOP-DATE
                   END-WRITE
               END-IF

              COMPUTE WS-INTEGER-DATE =
              FUNCTION INTEGER-OF-DATE(WS-CURRENT-LOOP-DATE)
              ADD 1 TO WS-INTEGER-DATE
              MOVE FUNCTION DATE-OF-INTEGER(WS-INTEGER-DATE) TO
              WS-CURRENT-LOOP-DATE
            END-PERFORM
            MOVE 'N' TO APPLY-AGAIN
            END-PERFORM


            DISPLAY "Press Enter to return to menu..."
            ACCEPT DUMMY-INPUT.

           VALIDATE-DATE.
               COMPUTE WS-TEMP-INT-DATE =
               FUNCTION INTEGER-OF-DATE(TEMP-LEAVE-DATE)
               IF WS-TEMP-INT-DATE = 0
               DISPLAY "Error: is not a valid calendar date"
               MOVE 'N' TO VALID-DATE-FLAG
           ELSE
               MOVE 'Y' TO VALID-DATE-FLAG
           END-IF.

           APPROVE-REJECT-LEAVE.
            CLOSE LEAVE-FILE
            OPEN I-O LEAVE-FILE
            IF LEAVE-STATUS-CODE NOT = "00"
               DISPLAY "Error opening LEAVE.DAT for approval. Status: "
               LEAVE-STATUS-CODE
               GO TO APPROVE-REJECT-CLEANUP
            END-IF

            MOVE FUNCTION CURRENT-DATE(1:8) TO TODAY-DATE

            MOVE SPACES TO LEAVE-REQ-ID
            START LEAVE-FILE KEY IS >= LEAVE-REQ-ID
               INVALID KEY
                   DISPLAY "No leave requests found."
                   GO TO APPROVE-REJECT-CLEANUP
            END-START

            MOVE 'N' TO FOUND-PENDING
            PERFORM UNTIL LEAVE-STATUS-CODE NOT = '00'
               READ LEAVE-FILE NEXT RECORD
                   AT END
                       MOVE '99' TO LEAVE-STATUS-CODE
                   NOT AT END

                       IF FUNCTION TRIM(LEAVE-STATUS) = "APPLIED"
                           MOVE 'Y' TO FOUND-PENDING
                           MOVE LEAVE-EMP-ID TO BAL-EMP-ID
                           READ BALANCE-FILE INVALID KEY
                               MOVE SPACES TO BAL-EMP-NAME
                           END-READ

                           DISPLAY "Request ID: " LEAVE-REQ-ID
                           DISPLAY "Employee ID: " LEAVE-EMP-ID
                           DISPLAY "Employee Name: " BAL-EMP-NAME
                           DISPLAY "Leave date: " LEAVE-DATE
                           DISPLAY "Reason: " LEAVE-REASON
                           MOVE SPACES TO DECISION-INPUT
                           PERFORM UNTIL DECISION-INPUT = 'A' OR
                                         DECISION-INPUT = 'R'
                               DISPLAY "Approve (A) or Reject (R)? "
                               ACCEPT DECISION-INPUT

                           EVALUATE DECISION-INPUT
                           WHEN 'A'
                               MOVE "APPROVED" TO LEAVE-STATUS
                               MOVE AUTH-MGR-ID TO LEAVE-MANAGER-ID
                               MOVE TODAY-DATE TO LEAVE-DECISION-DATE
                               PERFORM SUBTRACT-LEAVE-BALANCE
                               REWRITE LEAVE-RECORD
                               DISPLAY "Leave approved."
                           WHEN 'R'
                               MOVE "REJECTED" TO LEAVE-STATUS
                               MOVE AUTH-MGR-ID TO LEAVE-MANAGER-ID
                               MOVE TODAY-DATE TO LEAVE-DECISION-DATE
                               REWRITE LEAVE-RECORD
                               DISPLAY "Leave rejected."
                           WHEN OTHER
                               DISPLAY "Invalid choice."
                           END-EVALUATE
                           END-PERFORM
                      END-IF
                END-READ
            END-PERFORM

            IF FOUND-PENDING = 'N'
                DISPLAY "No pending leave requests found."
            END-IF.

            APPROVE-REJECT-CLEANUP.
                DISPLAY "Approve/reject process complete.".

            SUBTRACT-LEAVE-BALANCE.
                MOVE LEAVE-EMP-ID TO BAL-EMP-ID
                READ BALANCE-FILE INVALID KEY
                   DISPLAY "Employee not found in balance file."
                NOT INVALID KEY
                EVALUATE LEAVE-TYPE
                WHEN "ANNUAL"
                   IF BAL-ANNUAL > 0
                       SUBTRACT 1 FROM BAL-ANNUAL
                       REWRITE BALANCE-RECORD
                   ELSE
                       DISPLAY "Warning: Annual leave balance is zero!!"
                   END-IF
                WHEN "SICK"
                   IF BAL-SICK > 0
                       SUBTRACT 1 FROM BAL-SICK
                       REWRITE BALANCE-RECORD
                   ELSE
                       DISPLAY "Warning: Sick balance is zero!!"
                   END-IF
                WHEN "CASUAL"
                   IF BAL-CASUAL > 0
                       SUBTRACT 1 FROM BAL-CASUAL
                       REWRITE BALANCE-RECORD
                   ELSE
                       DISPLAY "Warning: Casual balance is zero!!"
                   END-IF
                WHEN OTHER
                   DISPLAY "Error: Unknown leave type for subtraction."
                END-EVALUATE
                END-READ.

            DISPLAY-LEAVE-BALANCES.
               CLOSE BALANCE-FILE
               IF EMPLOYEE
                   MOVE SPACES TO TEMP-EMP-ID
                   DISPLAY "Enter your Employee ID: "
                   ACCEPT TEMP-EMP-ID
                   OPEN INPUT BALANCE-FILE
                   IF BAL-STATUS NOT = '00'
                       DISPLAY "Error opening LeaveBal.dat. Status: "
                       BAL-STATUS
                       EXIT PARAGRAPH
                   END-IF

                   MOVE TEMP-EMP-ID TO BAL-EMP-ID
                   READ BALANCE-FILE INVALID KEY
                       DISPLAY "Employee ID not found."
                   NOT INVALID KEY
                       DISPLAY "Your Leave Balance:"
                       DISPLAY "---------------------"
                       DISPLAY "Employee ID: " BAL-EMP-ID
                       DISPLAY "Name       : " BAL-EMP-NAME
                       DISPLAY "Annual     : " BAL-ANNUAL
                       DISPLAY "Sick       : " BAL-SICK
                       DISPLAY "Casual     : " BAL-CASUAL
                   END-READ
                   CLOSE BALANCE-FILE
               ELSE
                   CLOSE BALANCE-FILE
                   OPEN INPUT BALANCE-FILE
                   IF BAL-STATUS NOT = '00'
                       DISPLAY "Error opening LeaveBal.dat. Status: "
                       BAL-STATUS
                       EXIT PARAGRAPH
                   END-IF

                   DISPLAY "All Employee Leave Balances"
                   DISPLAY "---------------------------------------------------------------------------|"
                   DISPLAY "EMP-ID    EMP-NAME           ANUAL-LEAVE    SICK-LEAVE   CASCUAL-LEAVE     |"
                   DISPLAY "---------------------------------------------------------------------------|"

                   MOVE SPACES TO BAL-EMP-ID
                   START BALANCE-FILE KEY >= BAL-EMP-ID
                       INVALID KEY
                           DISPLAY "No balance records found."
                           CLOSE BALANCE-FILE
                           EXIT PARAGRAPH
                   END-START

                   PERFORM UNTIL BAL-STATUS NOT = '00'
                       READ BALANCE-FILE NEXT RECORD
                           AT END
                               MOVE '99' TO BAL-STATUS
                           NOT AT END
                               DISPLAY BAL-EMP-ID "    "
                                       BAL-EMP-NAME (1:20) "    "
                                       BAL-ANNUAL "             "
                                       BAL-SICK "             "
                                       BAL-CASUAL
                       END-READ
                   END-PERFORM

                   CLOSE BALANCE-FILE
                   DISPLAY "Leave balance listing complete."
                   END-IF.
               DISPLAY "Press enter to return to menu..."
               ACCEPT DUMMY-INPUT.

            GENERATE-REPORT.
               DISPLAY "Opening Leave Report..."
               IF EMPLOYEE
                   MOVE SPACES TO TEMP-EMP-ID
                   DISPLAY "Enter your Employee ID: "
                   ACCEPT TEMP-EMP-ID
               END-IF
               CLOSE LEAVE-FILE
               OPEN INPUT LEAVE-FILE
               IF LEAVE-STATUS-CODE = '00'
                   DISPLAY "Leave.dat opened successfully. "
               ELSE
                   DISPLAY "Error opening: Status: " LEAVE-STATUS-CODE
                   EXIT PROGRAM
               END-IF

               MOVE SPACES TO LEAVE-REQ-ID
               START LEAVE-FILE KEY >= LEAVE-REQ-ID
                   INVALID KEY
                       DISPLAY "No leave records found."
                       CLOSE LEAVE-FILE
                       EXIT PARAGRAPH
               END-START

               MOVE 'N' TO FOUND-RECORD
               DISPLAY "|--------------------------------------------------------------------------------------------------|"
               DISPLAY "|REQ-ID    EMP-ID    LEAVE-DATE   TYPE        STATUS   MANAGER  DECISION-DATE     REASON           |"
               DISPLAY "|--------------------------------------------------------------------------------------------------|"

               PERFORM UNTIL LEAVE-STATUS-CODE NOT = '00'
                   READ LEAVE-FILE NEXT RECORD
                       AT END
                           MOVE '99' TO LEAVE-STATUS-CODE
                       NOT AT END

                           IF LEAVE-DECISION-DATE = "00000000" OR LEAVE-DECISION-DATE = SPACES
                                MOVE "PENDING" TO LEAVE-DECISION-DATE

                            END-IF

                           IF MANAGER OR (LEAVE-EMP-ID = TEMP-EMP-ID)
                               MOVE 'Y' TO FOUND-RECORD
                               DISPLAY LEAVE-REQ-ID "    "
                                       LEAVE-EMP-ID "    "
                                       LEAVE-DATE   "    "
                                       LEAVE-TYPE   "   "
                                       LEAVE-STATUS "   "
                                       LEAVE-MANAGER-ID "    "
                                       LEAVE-DECISION-DATE "    "
                                       LEAVE-REASON (1:20)
                           END-IF
                    END-READ
               END-PERFORM

               IF FOUND-RECORD = 'N'
                   DISPLAY "No leave records found."
               END-IF

               CLOSE LEAVE-FILE
               DISPLAY "Leave Report Complete."
               DISPLAY "Press Enter to return to menu..."
               ACCEPT DUMMY-INPUT.

            EMPLOYEE-LEAVE-TAKEN-DATA.
               MOVE SPACES TO TEMP-EMP-ID
               DISPLAY "Enter employee ID to view leave taken: "
               ACCEPT TEMP-EMP-ID
               CLOSE LEAVE-FILE
               OPEN INPUT LEAVE-FILE

               IF LEAVE-STATUS-CODE NOT = '00'
                   DISPLAY "Error opening leave.dat. Status: "
                   LEAVE-STATUS-CODE
                   EXIT PARAGRAPH
               END-IF

               MOVE SPACES TO LEAVE-REQ-ID
               MOVE 'N' TO FOUND-FLAG
               MOVE '00' TO LEAVE-STATUS-CODE

               START LEAVE-FILE KEY >= LEAVE-REQ-ID
                   INVALID KEY
                       DISPLAY "No records found."
                       CLOSE LEAVE-FILE
                       EXIT PARAGRAPH
               END-START

               DISPLAY "|--------------------------------------------------------------------------------------------------|"
               DISPLAY "|REQ-ID    LEAVE-DATE    LEAVE-TYPE      STATUS   MANAGER-ID   DECISION-DATE     REASON            |"
               DISPLAY "|--------------------------------------------------------------------------------------------------|"

               PERFORM UNTIL LEAVE-STATUS-CODE NOT = '00'
                   READ LEAVE-FILE NEXT RECORD
                       AT END
                           MOVE '99' TO LEAVE-STATUS-CODE
                       NOT AT END
                           IF LEAVE-EMP-ID = TEMP-EMP-ID
                               MOVE 'Y' TO FOUND-FLAG
                               DISPLAY LEAVE-REQ-ID "   "
                                   LEAVE-DATE "      "
                                   LEAVE-TYPE "    "
                                   LEAVE-STATUS "    "
                                   LEAVE-MANAGER-ID "        "
                                   LEAVE-DECISION-DATE "         "
                                   LEAVE-REASON (1:20)
                           END-IF
                   END-READ
               END-PERFORM

               IF FOUND-FLAG = 'N'
                   DISPLAY "No leave records found for this employee."
               END-IF

               CLOSE LEAVE-FILE
               DISPLAY "Employee leave history listing complete."
               DISPLAY "Press enter to return to menu...."
               ACCEPT DUMMY-INPUT.

           EXIT-PROGRAM.
           CLOSE LEAVE-FILE
           CLOSE BALANCE-FILE
           CLOSE MANAGER-FILE
           DISPLAY "+-------------------------+"
           DISPLAY "|   Exiting the Program   |"
           DISPLAY "+-------------------------+"
           DISPLAY "Thank you. Goodbye!"

           STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
