* 2000/08/23 mdemeest 4.6B Please identify all changes with UGL      UGL
*                          Copied RSEIDOC2 to ZYREDR001              UGL
*
REPORT RSEIDOC2 LINE-SIZE 132 MESSAGE-ID E0.
INCLUDE AUTH2TOP.                    " alle Konstanten für Berechtigg
INCLUDE <ICON>.
include cnt4defs.
*----------------------------------------------------------------------*
tables: EDID4.                                                      "UGL
data:   column(1).                                                  "UGL
data:   color_change(1).                                            "UGL
data:   ze1edk01 like e1edk01.                                      "UGL
data:   ze1idku1 like e1idku1.                                      "UGL

include zrseidoc_dat.                                               "UGL
include rseidoc_alv.
*include rseidoc_tree.

include rseidoc_alv_cl.
*include rseidoc_tree_cl.                                           "UGL

*include rseidoc_f01.                                               "UGL
form fill_special_values tables lt_rows structure lvc_s_row.        "UGL
endform.                                                            "UGL
form show_idoc tables lt_rows structure lvc_s_row.                  "UGL
endform.                                                            "UGL

*include rseidoc_i01.                                               "UGL
*include rseidoc_o01.                                               "UGL
*----------------------------------------------------------------------*

INITIALIZATION.
 MOVE TEXT-018 TO SOS_TABL.
 MOVE TEXT-019 TO SOS_TAB2.
 MOVE TEXT-020 TO SOS_TAB3.
 MOVE TEXT-999 TO SOS_TAB4.        "Title for tab                   UGL

START-OF-SELECTION.
  READ TABLE CREDAT INDEX 1.
  READ TABLE CRETIM INDEX 1.
  perform authority_check_rseidoc2_disp.
*------------------------------------------------------------------- UGL
* Begin of SAP code that is not required                             UGL
*------------------------------------------------------------------- UGL
* PERFORM SELECT_EDIDC.
* READ TABLE INT_EDIDC INDEX 1.
* IF SY-SUBRC EQ 0.
*   DESCRIBE TABLE INT_EDIDC LINES NUMBER.
*   IF NUMBER GT 1.                " eine Liste soll ausgegeben werden
*    i_edidc[] = int_edidc[].
*    perform fill_alv_list.
*    gs_variant-report = sy-repid.
*    x_save = 'A'.
*    create object vh_event.
*    call screen 100.
*   ELSE.                          " nur ein IDoc direkt anzeigen
*     sel_index = 0.
*     PERFORM AUTHORITY_CHECK_RSEIDOC2_IDOC USING AUTHORITY_OK.

*     IF AUTHORITY_OK = FALSE.
*       EXIT.
*     ENDIF.
*     submit idoc_tree_control with docnum = int_edidc-docnum
*                              and return.
*   ENDIF.
* ELSE.
*   MESSAGE I113.
* ENDIF.
*------------------------------------------------------------------- UGL
* End of code that is not required                                   UGL
*------------------------------------------------------------------- UGL
*----------------------------------------------------------------------*
*    SELECT_EDIDC                                                      *
*----------------------------------------------------------------------*
*FoRM SELECT_EDIDC.                                                 "UGL
* nachsehen, ob man all entries benutzen kann - falls man aus der
* Statisktik gerufen wurde, konnte es zu Problemen kommen beim IN-State-
* ment, weil nur eine begrenzte Größe möglich ist
* SELECT_ALL_USE = 'Y'.                                             "UGL
*  LOOP AT DOCNUM.                                                  "UGL
*    IF DOCNUM-SIGN NE 'I' OR DOCNUM-OPTION NE 'EQ'.                "UGL
*      SELECT_ALL_USE = 'N'.                                        "UGL
*      EXIT.                                                        "UGL
*    ENDIF.                                                         "UGL
*  ENDLOOP.                                                         "UGL
*  IF SY-SUBRC NE 0.                                                "UGL
*    SELECT_ALL_USE = 'N'.                                          "UGL
*  ENDIF.                                                           "UGL
*  IF SELECT_ALL_USE = 'N'.                                         "UGL
    SELECT * FROM EDIDC INTO TABLE INT_EDIDC
*     WHERE       UPDDAT >= CREDAT-LOW                              "UGL
*      AND         DOCNUM  IN DOCNUM                                "UGL
      WHERE       DOCNUM  IN DOCNUM                                 "UGL
      AND         STATUS  IN STATUS
      AND         DIRECT  IN DIRECT
      AND         IDOCTP  IN IDOCTP
      AND         CIMTYP  IN CIMTYP
      AND         MESTYP  IN MESTYP
*      and         mescod  in mescod                                "UGL
*      and         mesfct  in mesfct                                "UGL
      AND         TEST    IN TEST
      AND         SNDPOR  IN SNDPOR
      AND         SNDPRT  IN SNDPRT
      AND         SNDPFC  IN SNDPFC
      AND         SNDPRN  IN SNDPRN
      AND         RCVPOR  IN RCVPOR
      AND         RCVPRT  IN RCVPRT
      AND         RCVPFC  IN RCVPFC
      AND         RCVPRN  IN RCVPRN
      AND         REFINT  IN REFINT
      AND         REFGRP  IN REFGRP
      AND         REFMES  IN REFMES
      AND         ARCKEY  IN ARCKEY
*     AND         CREDAT  IN CREDAT                                 "UGL
      and         credat >  credat-low                              "UGL
      and         credat <= enddat-low                              "UGL
      AND         CRETIM  IN CRETIM
*      AND         UPDDAT  IN UPDDAT                                "UGL
*      AND         UPDTIM  IN UPDTIM                                "UGL
*      AND         STD     IN STD                                   "UGL
*      AND         STDVRS  IN STDVRS                                "UGL
*      AND         STDMES  IN STDMES                                "UGL
    ORDER BY PRIMARY KEY.          " Sortierung ist schon erfolgt
*  ELSE.                            " select_all_use = 'Y'          "UGL
*    SELECT * FROM EDIDC INTO TABLE INT_EDIDC                       "UGL
*                  FOR ALL ENTRIES IN DOCNUM                        "UGL
*      WHERE       UPDDAT >= CREDAT-LOW                             "UGL
*      AND         DOCNUM  =  DOCNUM-LOW                            "UGL
*      AND         STATUS  IN STATUS                                "UGL
*      AND         DIRECT  IN DIRECT                                "UGL
*      AND         IDOCTP  IN IDOCTP                                "UGL
*      AND         CIMTYP  IN CIMTYP                                "UGL
*      AND         MESTYP  IN MESTYP                                "UGL
*      and         mescod  in mescod                                "UGL
*      and         mesfct  in mesfct                                "UGL
*      AND         TEST    IN TEST                                  "UGL
*      AND         SNDPOR  IN SNDPOR                                "UGL
*      AND         SNDPRT  IN SNDPRT                                "UGL
*      AND         SNDPFC  IN SNDPFC                                "UGL
*      AND         SNDPRN  IN SNDPRN                                "UGL
*      AND         RCVPOR  IN RCVPOR                                "UGL
*      AND         RCVPRT  IN RCVPRT                                "UGL
*      AND         RCVPFC  IN RCVPFC                                "UGL
*      AND         RCVPRN  IN RCVPRN                                "UGL
*      AND         REFINT  IN REFINT                                "UGL
*      AND         REFGRP  IN REFGRP                                "UGL
*      AND         REFMES  IN REFMES                                "UGL
*      AND         ARCKEY  IN ARCKEY                                "UGL
*      AND         CREDAT  IN CREDAT                                "UGL
*      AND         CRETIM  IN CRETIM                                "UGL
*      AND         UPDDAT  IN UPDDAT                                "UGL
*      AND         UPDTIM  IN UPDTIM                                "UGL
*      AND         STD     IN STD                                   "UGL
*      AND         STDVRS  IN STDVRS                                "UGL
*      AND         STDMES  IN STDMES                                "UGL
*    ORDER BY PRIMARY KEY.          " Sortierung ist schon erfolgt  "UGL
*  ENDIF.                                                           "UGL
*ENDFORM.                                                           "UGL
                                                                    "UGL
  read table int_edidc index 1.                                     "UGL
  if sy-subrc = 0.                                                  "UGL
     move '1' to column.                                            "UGL
     perform show_IDOC_LIST TABLES INT_EDIDC USING '0' '1'.         "UGL
  ELSE.                                                             "UGL
     NEW-PAGE NO-TITLE NO-HEADING.                                  "UGL
     ULINE 1(80).                                                   "UGL
     WRITE: /1 '|', 80 '|'.                                         "UGL
     FORMAT COLOR 1.                                                "UGL
     WRITE: 2 RPT_HDR, 70 SY-DATUM.                                 "UGL
                                                                    "UGL
     WRITE: /1 '|', 80 '|'.                                         "UGL
     uline 2(78).                                                   "UGL
     format intensified off.                                        "UGL
                                                                    "UGL
     write: /1 '|', 80 '|'.         "Titles                          UGL
     write: 2 text-002, 12 '|',                                     "UGL
           16 text-010, 23 '|',                                     "UGL
           26 text-011, 32 '|',                                     "UGL
           33 text-014, 43 '|',                                     "UGL
           44 text-005.                                             "UGL
                                                                    "UGL
     uline /1(80).                                                  "UGL
                                                                    "UGL
     WRITE: /1 '|', 80 '|'.                                         "UGL
                                                                    "UGL
     write: /1 '|', 80 '|',                                         "UGL
             2  'NO EXCEPTIONS FOUND FOR THIS REPORT.'.             "UGL
     WRITE: /1 '|', 80 '|'.                                         "UGL
     uline /1(80).                                                  "UGL
  endif.                                                            "UGL
                                                                    "UGL
  include ZYEDR002.                                                 "UGL
                                                                    "UGL
  at line-selection.                                                "UGL
* status records                                                    "UGL
  if authority_ok = false.                                          "UGL
     exit.                                                          "UGL
  endif.                                                            "UGL
  move int_edidc to edidc.                                          "UGL
  call function 'EDI_DOCUMENT_STATUS_DISPLAY'                       "UGL
     exporting                                                      "UGL
        docnum                        = int_edidc-docnum            "UGL
     exceptions                                                     "UGL
        no_status_record_found        = 01.                         "UGL
                                                                    "UGL
  at user-command.                                                  "UGL
     case sy-ucomm.                                                 "UGL
* control record                                                    "UGL
     when 'CONT'.                                                   "UGL
       perform authority_check_rseidoc2_idoc using authority_ok.    "UGL
       if authority_ok = false.                                     "UGL
          exit.                                                     "UGL
       endif.                                                       "UGL
       call function 'EDI_DOCUMENT_CONTROL_DISPLAY'                 "UGL
          exporting                                                 "UGL
                docnum                       = int_edidc-docnum     "UGL
          exceptions                                                "UGL
                control_record_not_exist     = 01.                  "UGL
                                                                    "UGL
* status record                                                     "UGL
     when 'STAT'.                                                   "UGL
       perform authority_check_rseidoc2_idoc using authority_ok.    "UGL
       if authority_ok = false.                                     "UGL
          exit.                                                     "UGL
       endif.                                                       "UGL
       call function 'EDI_DOCUMENT_STATUS_DISPLAY'                  "UGL
          exporting                                                 "UGL
                docnum                       = int_edidc-docnum     "UGL
          exceptions                                                "UGL
                no_status_record_found       = 01.                  "UGL
                                                                    "UGL
* data record                                                       "UGL
     when 'DATA'.                                                   "UGL
       perform authority_check_rseidoc2_idoc using authority_ok.    "UGL
       if authority_ok = false.                                     "UGL
          exit.                                                     "UGL
       endif.                                                       "UGL
       call function 'EDI_DOCUMENT_DATA_DISPLAY'                    "UGL
          exporting                                                 "UGL
                docnum                       = int_edidc-docnum     "UGL
          exceptions                                                "UGL
                no_data_record_found         = 01.                  "UGL
                                                                    "UGL
     when 'EESC'.                                                   "UGL
        leave program.                                              "UGL
     when 'ENDE'.                                                   "UGL
        leave program.                                              "UGL
     when 'BACK'.                                                   "UGL
        set screen 0.                                               "UGL
        leave screen.                                               "UGL
                                                                    "UGL
     when 'PRI'.                                                    "UGL
       perform authority_check_rseidoc2_idoc using authority_ok.    "UGL
       if authority_ok = false.                                     "UGL
          exit.                                                     "UGL
       endif.                                                       "UGL
       perform print_list.                                          "UGL
                                                                    "UGL
     when 'LOLD'.                                                   "UGL
       column = 1.                                                  "UGL
       perform write_list tables int_edidc.                         "UGL
                                                                    "UGL
     when 'LNEW'.                                                   "UGL
       column = 2.                                                  "UGL
       perform write_other_list tables int_edidc.                   "UGL
                                                                    "UGL
     when 'SDOC'.                                                   "UGL
       sort int_edidc by credat cretim.                             "UGL
       sy-lsind = 0.                                                "UGL
       if column = 1.                                               "UGL
          perform write_list tables int_edidc.                      "UGL
       else.                                                        "UGL
          perform write_other_list tables int_edidc.                "UGL
       endif.                                                       "UGL
                                                                    "UGL
     when 'SSTA'.                                                   "UGL
       sort int_edidc by status docnum.                             "UGL
       sy-lsind = 0.                                                "UGL
       if column = 1.                                               "UGL
          perform write_list tables int_edidc.                      "UGL
       else.                                                        "UGL
          perform write_other_list tables int_edidc.                "UGL
       endif.                                                       "UGL
                                                                    "UGL
     when 'SDIR'.                                                   "UGL
       sort int_edidc by direct docnum.                             "UGL
       sy-lsind = 0.                                                "UGL
       if column = 1.                                               "UGL
          perform write_list tables int_edidc.                      "UGL
       else.                                                        "UGL
          perform write_other_list tables int_edidc.                "UGL
       endif.                                                       "UGL
                                                                    "UGL
     when 'AUFF'.                                                   "UGL
       refresh int_edidc.                                           "UGL
       clear int_edidc.                                             "UGL
       select * from edidc into table int_edidc                     "UGL
*      WHERE       UPDDAT >= CREDAT-LOW                             "UGL
*      AND         DOCNUM  =  DOCNUM-LOW                            "UGL
       where       DOCNUM  in DOCNUM                                "UGL
       AND         STATUS  IN STATUS                                "UGL
       AND         DIRECT  IN DIRECT                                "UGL
       AND         IDOCTP  IN IDOCTP                                "UGL
*      AND         CIMTYP  IN CIMTYP                                "UGL
       AND         MESTYP  IN MESTYP                                "UGL
*      and         mescod  in mescod                                "UGL
*      and         mesfct  in mesfct                                "UGL
       AND         TEST    IN TEST                                  "UGL
       AND         SNDPOR  IN SNDPOR                                "UGL
       AND         SNDPRT  IN SNDPRT                                "UGL
       AND         SNDPFC  IN SNDPFC                                "UGL
       AND         SNDPRN  IN SNDPRN                                "UGL
       AND         RCVPOR  IN RCVPOR                                "UGL
       AND         RCVPRT  IN RCVPRT                                "UGL
       AND         RCVPFC  IN RCVPFC                                "UGL
       AND         RCVPRN  IN RCVPRN                                "UGL
       AND         REFINT  IN REFINT                                "UGL
       AND         REFGRP  IN REFGRP                                "UGL
       AND         REFMES  IN REFMES                                "UGL
       AND         ARCKEY  IN ARCKEY                                "UGL
       AND         CREDAT  > credat-low                             "UGL
       and         credat <= enddat-low                             "UGL
       AND         CRETIM  IN CRETIM                                "UGL
*      AND         UPDDAT  IN UPDDAT                                "UGL
*      AND         UPDTIM  IN UPDTIM                                "UGL
*      AND         STD     IN STD                                   "UGL
*      AND         STDVRS  IN STDVRS                                "UGL
*      AND         STDMES  IN STDMES                                "UGL
     ORDER BY PRIMARY KEY.          " Sortierung ist schon erfolgt  "UGL
      move '1' to column.                                           "UGL
      perform show_idoc_list tables int_edidc using '0' '1'.        "UGL
  endcase.                                                          "UGL
                                                                    "UGL
*----------------------------------------------------------------------*
*       Form  AUTHORITY_CHECK_RSEIDOC2_DISP                            *
*----------------------------------------------------------------------*
*       checks, whether the actual user has the necessary authority    *
*       if the user is not authorized the program stoppes and          *
*       a message is displayed to the user                             *
*----------------------------------------------------------------------*
* Output:                                                              *
*       AUTHORITY_OK - if the current user is authorized for the       *
*                      action, so the parameter is set to TRUE         *
*                      otherwise to FALSE                              *
*----------------------------------------------------------------------*
FORM AUTHORITY_CHECK_RSEIDOC2_DISP.

  AUTHORITY-CHECK OBJECT   AUTHORITY_OBJ_EDI_MONITOR
        ID 'EDI_TCD' FIELD AUTHORITY_TCODE_RSEIDOC2
        ID 'ACTVT' FIELD AUTHORITY_ACTIVITY_DISPLAY
        ID 'EDI_DIR' DUMMY
        ID 'EDI_MES' DUMMY
        ID 'EDI_PRN' DUMMY
        ID 'EDI_PRT' DUMMY.

  IF SY-SUBRC NE 0.
* authoritycheck negative; message for the user and stop program
    MESSAGE E168.
  ENDIF.

ENDFORM.                               " AUTHORITY_CHECK_RSEIDOC2_DISP
*-----------------------  UGL  NOTE  -------------------------------"UGL
*  Code to end of program is SAP's and is not used in UGL version   "UGL
*-------------------------------------------------------------------"UGL

*----------------------------------------------------------------------*
*       Form  AUTHORITY_CHECK_RSEIDOC2_IDOC                            *
*----------------------------------------------------------------------*
FORM AUTHORITY_CHECK_RSEIDOC2_IDOC USING L_AUTHORITY_OK.

  IF INT_EDIDC-DIRECT EQ '1'.        " dann Empfänger abzutesten
    AUTHORITY-CHECK OBJECT   AUTHORITY_OBJ_EDI_MONITOR
        ID 'EDI_TCD' FIELD AUTHORITY_TCODE_RSEIDOC2
        ID 'ACTVT' FIELD AUTHORITY_ACTIVITY_DISPLAY
        ID 'EDI_DIR' FIELD INT_EDIDC-DIRECT
        ID 'EDI_MES' FIELD INT_EDIDC-MESTYP
        ID 'EDI_PRN' FIELD INT_EDIDC-RCVPRN
        ID 'EDI_PRT' FIELD INT_EDIDC-RCVPRT.

    IF SY-SUBRC = 0.
* authoritycheck positive
      MOVE TRUE  TO L_AUTHORITY_OK.
    ELSE.
* authoritycheck negative; message for the user and stop program
      MESSAGE I169 WITH INT_EDIDC-MESTYP INT_EDIDC-DIRECT
                        INT_EDIDC-RCVPRT INT_EDIDC-RCVPRN.
      MOVE FALSE TO L_AUTHORITY_OK.
    ENDIF.
  ELSE.                                 " Eingang -> Sender abzutesten
    AUTHORITY-CHECK OBJECT   AUTHORITY_OBJ_EDI_MONITOR
        ID 'EDI_TCD' FIELD AUTHORITY_TCODE_RSEIDOC2
        ID 'ACTVT' FIELD AUTHORITY_ACTIVITY_DISPLAY
        ID 'EDI_DIR' FIELD INT_EDIDC-DIRECT
        ID 'EDI_MES' FIELD INT_EDIDC-MESTYP
        ID 'EDI_PRN' FIELD INT_EDIDC-SNDPRN
        ID 'EDI_PRT' FIELD INT_EDIDC-SNDPRT.

    IF SY-SUBRC = 0.
* authoritycheck positive
      MOVE TRUE  TO L_AUTHORITY_OK.
    ELSE.
* authoritycheck negative; message for the user and stop program
      MESSAGE I169 WITH INT_EDIDC-MESTYP INT_EDIDC-DIRECT
                        INT_EDIDC-SNDPRT INT_EDIDC-SNDPRN.
      MOVE FALSE TO L_AUTHORITY_OK.
    ENDIF.
  ENDIF.
ENDFORM.                               " AUTHORITY_CHECK_RSEIDOC2_IDOC
