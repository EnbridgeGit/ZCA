*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTRT_CONFIG.....................................*
DATA:  BEGIN OF STATUS_ZTRT_CONFIG                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTRT_CONFIG                   .
CONTROLS: TCTRL_ZTRT_CONFIG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTRT_CONFIG                   .
TABLES: ZTRT_CONFIG                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
