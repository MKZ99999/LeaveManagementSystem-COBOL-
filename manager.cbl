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
           SELECT MANAGER-FILE ASSIGN TO "../manager.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS MANAGER-ID
               FILE STATUS IS MANAGER-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD MANAGER-FILE.
       01 MANAGER-RECORD.
           05 MANAGER-ID PIC X(5).
           05 MANAGER-NAME PIC X(20).
           05 MANAGER-PASSWORD PIC X(20).


       WORKING-STORAGE SECTION.
       01 MANAGER-STATUS PIC XX.
       01 TEMP-MGR-ID PIC X(5).
       01 TEMP-MGR-NAME PIC X(20).
       01 TEMP-MGR-PWD PIC X(20).
       01 CHOICE PIC 9.
       01 PWD-LENGTH PIC 99.
       01 END-FILE PIC X VALUE "N".
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            OPEN I-O MANAGER-FILE
            IF MANAGER-STATUS NOT="00"
                DISPLAY "Manager.dat not found. Creating file..."
                OPEN OUTPUT MANAGER-FILE
                CLOSE MANAGER-FILE
                OPEN I-O MANAGER-FILE
                IF MANAGER-STATUS NOT = "00"
                    DISPLAY "Failed to open. Status: " MANAGER-STATUS
                    STOP RUN
                END-IF
            END-IF

            PERFORM UNTIL CHOICE = 3
               DISPLAY SPACE
               DISPLAY "***** Manager Management System *****"
               DISPLAY "Select option:"
               DISPLAY "+-----------------------+"
               DISPLAY "| 1 - Add new manager   |"
               DISPLAY "| 2 - View all managers |"
               DISPLAY "| 3 - Exit              |"
               DISPLAY "+-----------------------+"
               DISPLAY "Your choice: "
               ACCEPT CHOICE

               EVALUATE CHOICE
                   WHEN 1
                       PERFORM ADD-MANAGER
                   WHEN 2
                       PERFORM VIEW-MANAGERS
                   WHEN 3
                       DISPLAY "Exiting program. Goodbye!"
                   WHEN OTHER
                       DISPLAY "Invalid choice.Please enter 1, 2, or 3."
               END-EVALUATE

           END-PERFORM
               CLOSE MANAGER-FILE
               STOP RUN.

           ADD-MANAGER.
           DISPLAY "Enter Manager ID (5 chars): "
           ACCEPT TEMP-MGR-ID
           MOVE TEMP-MGR-ID TO MANAGER-ID

           READ MANAGER-FILE
               INVALID KEY
                   DISPLAY "Enter Manager Name (20 chars): "
                   ACCEPT TEMP-MGR-NAME

                   MOVE 0 TO PWD-LENGTH
                   PERFORM UNTIL PWD-LENGTH >= 6
                       DISPLAY "Enter manager password: "
                       ACCEPT TEMP-MGR-PWD
                       COMPUTE PWD-LENGTH
                       = FUNCTION LENGTH(FUNCTION TRIM(TEMP-MGR-PWD))
                       IF PWD-LENGTH < 6
                           DISPLAY "Password is too short."
                       END-IF
                   END-PERFORM

                   MOVE TEMP-MGR-NAME TO MANAGER-NAME
                   MOVE TEMP-MGR-PWD  TO MANAGER-PASSWORD

                   WRITE MANAGER-RECORD INVALID KEY
                       DISPLAY "Error: Manager record already exists."
                   NOT INVALID KEY
                       DISPLAY "Manager added successfully."
                   END-WRITE
               NOT INVALID KEY
                   DISPLAY "Manager already exists. Cannot add."
           END-READ.

       VIEW-MANAGERS.
           MOVE "00000" TO MANAGER-ID
           MOVE "N" TO END-FILE

           START MANAGER-FILE KEY IS >= MANAGER-ID
               INVALID KEY
                   DISPLAY "No managers found in the file."
                   MOVE "Y" TO END-FILE
           END-START

           IF END-FILE NOT = "Y"
               DISPLAY SPACE
               DISPLAY "ID     NAME                 PASSWORD"
               DISPLAY "------------------------------------"

               PERFORM UNTIL END-FILE = "Y"
                   READ MANAGER-FILE NEXT RECORD
                       AT END
                           MOVE "Y" TO END-FILE
                       NOT AT END
                           DISPLAY MANAGER-ID "  "
                                   MANAGER-NAME "  "
                                   MANAGER-PASSWORD
                   END-READ
               END-PERFORM
               DISPLAY "-------------------------------------------"
           END-IF.
       END PROGRAM YOUR-PROGRAM-NAME.
