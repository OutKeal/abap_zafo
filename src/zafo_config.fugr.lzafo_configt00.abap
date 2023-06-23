*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZAFO_BUSTYPE....................................*
DATA:  BEGIN OF STATUS_ZAFO_BUSTYPE                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_BUSTYPE                  .
CONTROLS: TCTRL_ZAFO_BUSTYPE
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZAFO_CONTRACT_T.................................*
DATA:  BEGIN OF STATUS_ZAFO_CONTRACT_T               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_CONTRACT_T               .
CONTROLS: TCTRL_ZAFO_CONTRACT_T
            TYPE TABLEVIEW USING SCREEN '0013'.
*...processing: ZAFO_COST.......................................*
DATA:  BEGIN OF STATUS_ZAFO_COST                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_COST                     .
CONTROLS: TCTRL_ZAFO_COST
            TYPE TABLEVIEW USING SCREEN '0011'.
*...processing: ZAFO_OBJECT.....................................*
DATA:  BEGIN OF STATUS_ZAFO_OBJECT                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_OBJECT                   .
CONTROLS: TCTRL_ZAFO_OBJECT
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZAFO_POST_RULE..................................*
DATA:  BEGIN OF STATUS_ZAFO_POST_RULE                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_POST_RULE                .
CONTROLS: TCTRL_ZAFO_POST_RULE
            TYPE TABLEVIEW USING SCREEN '0007'.
*...processing: ZAFO_PRINT......................................*
DATA:  BEGIN OF STATUS_ZAFO_PRINT                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_PRINT                    .
CONTROLS: TCTRL_ZAFO_PRINT
            TYPE TABLEVIEW USING SCREEN '0012'.
*...processing: ZAFO_QC_MODEL_H.................................*
DATA:  BEGIN OF STATUS_ZAFO_QC_MODEL_H               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_QC_MODEL_H               .
CONTROLS: TCTRL_ZAFO_QC_MODEL_H
            TYPE TABLEVIEW USING SCREEN '0008'.
*...processing: ZAFO_QC_MODEL_I.................................*
DATA:  BEGIN OF STATUS_ZAFO_QC_MODEL_I               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_QC_MODEL_I               .
CONTROLS: TCTRL_ZAFO_QC_MODEL_I
            TYPE TABLEVIEW USING SCREEN '0010'.
*...processing: ZAFO_QC_MODEL_T.................................*
DATA:  BEGIN OF STATUS_ZAFO_QC_MODEL_T               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_QC_MODEL_T               .
CONTROLS: TCTRL_ZAFO_QC_MODEL_T
            TYPE TABLEVIEW USING SCREEN '0009'.
*...processing: ZAFO_REASON.....................................*
DATA:  BEGIN OF STATUS_ZAFO_REASON                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_REASON                   .
CONTROLS: TCTRL_ZAFO_REASON
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: ZAFO_SCREEN.....................................*
DATA:  BEGIN OF STATUS_ZAFO_SCREEN                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_SCREEN                   .
CONTROLS: TCTRL_ZAFO_SCREEN
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZAFO_SCREEN_BTY.................................*
DATA:  BEGIN OF STATUS_ZAFO_SCREEN_BTY               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_SCREEN_BTY               .
CONTROLS: TCTRL_ZAFO_SCREEN_BTY
            TYPE TABLEVIEW USING SCREEN '0016'.
*...processing: ZAFO_SCREEN_TEXT................................*
DATA:  BEGIN OF STATUS_ZAFO_SCREEN_TEXT              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_SCREEN_TEXT              .
CONTROLS: TCTRL_ZAFO_SCREEN_TEXT
            TYPE TABLEVIEW USING SCREEN '0014'.
*...processing: ZAFO_SEL_SCREEN.................................*
DATA:  BEGIN OF STATUS_ZAFO_SEL_SCREEN               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_SEL_SCREEN               .
CONTROLS: TCTRL_ZAFO_SEL_SCREEN
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: ZAFO_SEL_SCR_BTY................................*
DATA:  BEGIN OF STATUS_ZAFO_SEL_SCR_BTY              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_SEL_SCR_BTY              .
CONTROLS: TCTRL_ZAFO_SEL_SCR_BTY
            TYPE TABLEVIEW USING SCREEN '0017'.
*...processing: ZAFO_SEL_VALUE..................................*
DATA:  BEGIN OF STATUS_ZAFO_SEL_VALUE                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_SEL_VALUE                .
CONTROLS: TCTRL_ZAFO_SEL_VALUE
            TYPE TABLEVIEW USING SCREEN '0006'.
*...processing: ZAFO_TQC_REASON.................................*
DATA:  BEGIN OF STATUS_ZAFO_TQC_REASON               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAFO_TQC_REASON               .
CONTROLS: TCTRL_ZAFO_TQC_REASON
            TYPE TABLEVIEW USING SCREEN '0015'.
*.........table declarations:.................................*
TABLES: *ZAFO_BUSTYPE                  .
TABLES: *ZAFO_CONTRACT_T               .
TABLES: *ZAFO_COST                     .
TABLES: *ZAFO_OBJECT                   .
TABLES: *ZAFO_POST_RULE                .
TABLES: *ZAFO_PRINT                    .
TABLES: *ZAFO_QC_MODEL_H               .
TABLES: *ZAFO_QC_MODEL_I               .
TABLES: *ZAFO_QC_MODEL_T               .
TABLES: *ZAFO_REASON                   .
TABLES: *ZAFO_SCREEN                   .
TABLES: *ZAFO_SCREEN_BTY               .
TABLES: *ZAFO_SCREEN_TEXT              .
TABLES: *ZAFO_SEL_SCREEN               .
TABLES: *ZAFO_SEL_SCR_BTY              .
TABLES: *ZAFO_SEL_VALUE                .
TABLES: *ZAFO_TQC_REASON               .
TABLES: ZAFO_BUSTYPE                   .
TABLES: ZAFO_CONTRACT_T                .
TABLES: ZAFO_COST                      .
TABLES: ZAFO_OBJECT                    .
TABLES: ZAFO_POST_RULE                 .
TABLES: ZAFO_PRINT                     .
TABLES: ZAFO_QC_MODEL_H                .
TABLES: ZAFO_QC_MODEL_I                .
TABLES: ZAFO_QC_MODEL_T                .
TABLES: ZAFO_REASON                    .
TABLES: ZAFO_SCREEN                    .
TABLES: ZAFO_SCREEN_BTY                .
TABLES: ZAFO_SCREEN_TEXT               .
TABLES: ZAFO_SEL_SCREEN                .
TABLES: ZAFO_SEL_SCR_BTY               .
TABLES: ZAFO_SEL_VALUE                 .
TABLES: ZAFO_TQC_REASON                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
