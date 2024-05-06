*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZAFO_BUSTYPE....................................*
DATA:  BEGIN OF STATUS_ZAFO_BUSTYPE                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_BUSTYPE                  .
CONTROLS: TCTRL_ZAFO_BUSTYPE
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: ZAFO_DICT.......................................*
DATA:  BEGIN OF STATUS_ZAFO_DICT                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_DICT                     .
CONTROLS: TCTRL_ZAFO_DICT
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: ZAFO_NOTES......................................*
DATA:  BEGIN OF STATUS_ZAFO_NOTES                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_NOTES                    .
CONTROLS: TCTRL_ZAFO_NOTES
            TYPE TABLEVIEW USING SCREEN '0007'.
*...processing: ZAFO_NOTES_ALLO.................................*
DATA:  BEGIN OF STATUS_ZAFO_NOTES_ALLO               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_NOTES_ALLO               .
CONTROLS: TCTRL_ZAFO_NOTES_ALLO
            TYPE TABLEVIEW USING SCREEN '0009'.
*...processing: ZAFO_OBJECT.....................................*
DATA:  BEGIN OF STATUS_ZAFO_OBJECT                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_OBJECT                   .
CONTROLS: TCTRL_ZAFO_OBJECT
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZAFO_OBJECT_ACT.................................*
DATA:  BEGIN OF STATUS_ZAFO_OBJECT_ACT               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_OBJECT_ACT               .
CONTROLS: TCTRL_ZAFO_OBJECT_ACT
            TYPE TABLEVIEW USING SCREEN '0013'.
*...processing: ZAFO_PRINT_ALLO.................................*
DATA:  BEGIN OF STATUS_ZAFO_PRINT_ALLO               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_PRINT_ALLO               .
CONTROLS: TCTRL_ZAFO_PRINT_ALLO
            TYPE TABLEVIEW USING SCREEN '0014'.
*...processing: ZAFO_PRINT_RULE.................................*
DATA:  BEGIN OF STATUS_ZAFO_PRINT_RULE               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_PRINT_RULE               .
CONTROLS: TCTRL_ZAFO_PRINT_RULE
            TYPE TABLEVIEW USING SCREEN '0015'.
*...processing: ZAFO_PRINT_RULED................................*
DATA:  BEGIN OF STATUS_ZAFO_PRINT_RULED              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_PRINT_RULED              .
CONTROLS: TCTRL_ZAFO_PRINT_RULED
            TYPE TABLEVIEW USING SCREEN '0016'.
*...processing: ZAFO_RULE.......................................*
DATA:  BEGIN OF STATUS_ZAFO_RULE                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_RULE                     .
CONTROLS: TCTRL_ZAFO_RULE
            TYPE TABLEVIEW USING SCREEN '0010'.
*...processing: ZAFO_RULE_CODE..................................*
DATA:  BEGIN OF STATUS_ZAFO_RULE_CODE                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_RULE_CODE                .
CONTROLS: TCTRL_ZAFO_RULE_CODE
            TYPE TABLEVIEW USING SCREEN '0012'.
*...processing: ZAFO_RULE_DETAIL................................*
DATA:  BEGIN OF STATUS_ZAFO_RULE_DETAIL              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_RULE_DETAIL              .
CONTROLS: TCTRL_ZAFO_RULE_DETAIL
            TYPE TABLEVIEW USING SCREEN '0011'.
*...processing: ZAFO_SCREEN.....................................*
DATA:  BEGIN OF STATUS_ZAFO_SCREEN                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_SCREEN                   .
CONTROLS: TCTRL_ZAFO_SCREEN
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZAFO_SCREEN_TEXT................................*
DATA:  BEGIN OF STATUS_ZAFO_SCREEN_TEXT              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_SCREEN_TEXT              .
CONTROLS: TCTRL_ZAFO_SCREEN_TEXT
            TYPE TABLEVIEW USING SCREEN '0008'.
*...processing: ZAFO_SEL_SCREEN.................................*
DATA:  BEGIN OF STATUS_ZAFO_SEL_SCREEN               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_SEL_SCREEN               .
CONTROLS: TCTRL_ZAFO_SEL_SCREEN
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZAFO_SEL_VALUE..................................*
DATA:  BEGIN OF STATUS_ZAFO_SEL_VALUE                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_SEL_VALUE                .
CONTROLS: TCTRL_ZAFO_SEL_VALUE
            TYPE TABLEVIEW USING SCREEN '0006'.
*...processing: ZAFO_UEB_GROUP..................................*
DATA:  BEGIN OF STATUS_ZAFO_UEB_GROUP                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_UEB_GROUP                .
CONTROLS: TCTRL_ZAFO_UEB_GROUP
            TYPE TABLEVIEW USING SCREEN '0017'.
*.........table declarations:.................................*
TABLES: *ZAFO_BUSTYPE                  .
TABLES: *ZAFO_DICT                     .
TABLES: *ZAFO_NOTES                    .
TABLES: *ZAFO_NOTES_ALLO               .
TABLES: *ZAFO_OBJECT                   .
TABLES: *ZAFO_OBJECT_ACT               .
TABLES: *ZAFO_PRINT_ALLO               .
TABLES: *ZAFO_PRINT_RULE               .
TABLES: *ZAFO_PRINT_RULED              .
TABLES: *ZAFO_RULE                     .
TABLES: *ZAFO_RULE_CODE                .
TABLES: *ZAFO_RULE_DETAIL              .
TABLES: *ZAFO_SCREEN                   .
TABLES: *ZAFO_SCREEN_TEXT              .
TABLES: *ZAFO_SEL_SCREEN               .
TABLES: *ZAFO_SEL_VALUE                .
TABLES: *ZAFO_UEB_GROUP                .
TABLES: ZAFO_BUSTYPE                   .
TABLES: ZAFO_DICT                      .
TABLES: ZAFO_NOTES                     .
TABLES: ZAFO_NOTES_ALLO                .
TABLES: ZAFO_OBJECT                    .
TABLES: ZAFO_OBJECT_ACT                .
TABLES: ZAFO_PRINT_ALLO                .
TABLES: ZAFO_PRINT_RULE                .
TABLES: ZAFO_PRINT_RULED               .
TABLES: ZAFO_RULE                      .
TABLES: ZAFO_RULE_CODE                 .
TABLES: ZAFO_RULE_DETAIL               .
TABLES: ZAFO_SCREEN                    .
TABLES: ZAFO_SCREEN_TEXT               .
TABLES: ZAFO_SEL_SCREEN                .
TABLES: ZAFO_SEL_VALUE                 .
TABLES: ZAFO_UEB_GROUP                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
