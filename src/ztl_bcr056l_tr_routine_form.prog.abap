*&---------------------------------------------------------------------*
*&  Include           ZTL_BCR056L_TR_ROUTINE_FORM
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&  Include           ZTL_BCR056L_TR_ROUTINE_FORM
*&---------------------------------------------------------------------*

"======================================================================="
"------------------Project Forms----------------------------------------"
"======================================================================="
*&---------------------------------------------------------------------*
*&      Form  validate_project
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*FORM validate_project .
*
*  " cts project structure--------------------------"
*  TYPES: BEGIN OF s_ctsproj,
*           trkorr TYPE ctsproject-externalid,
*         END OF s_ctsproj.
*
*  DATA: lwa_trkorr TYPE s_ctsproj.
*
*  SELECT SINGLE externalid
*    FROM ctsproject
*    INTO lwa_trkorr
*    WHERE externalid IN s_extid.
*  IF sy-subrc <> 0.
*    MESSAGE e003(ztr).
*  ENDIF.
*
*ENDFORM.                    "VALIDATE_PROJECT


*&---------------------------------------------------------------------*
*&      fORM  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       tOP OF THE alv WITH DATE TIME AND FILEPATH
*----------------------------------------------------------------------*
*FORM top_of_page1.
*  DATA:
*    lv_string(1000) TYPE c,
*    lv_lines        TYPE i,
*    lv_linesc(10)   TYPE c.
*
*  IF p_obj IS NOT INITIAL.
*    FORMAT INTENSIFIED ON.
*    WRITE: /1(160) text-h07 CENTERED.
*    SKIP 1.
*    WRITE : /1(15) text-h08, sy-datum, '/', sy-uzeit.
*
*    "Total No. OF Records Selected
*    DESCRIBE TABLE i_final_p LINES lv_lines.
*    lv_linesc = lv_lines.
*    CONCATENATE text-h09 lv_linesc
*                      INTO lv_string SEPARATED BY space.
*    WRITE:130 lv_string.
*    CLEAR lv_string.
*    WRITE :/1(15) text-h11, 17 s_extid-low.
*    WRITE :141 text-h12, 162 sy-uname.
*  ELSEIF p_trlist IS NOT INITIAL.
*    FORMAT INTENSIFIED ON.
*    WRITE: /1(160) text-h10 CENTERED.
*    SKIP 1.
*    WRITE : /1(15) text-h08, sy-datum, '/', sy-uzeit.
*
*    "tOTAL nO. OF rECORDS sELECTED
*    DESCRIBE TABLE i_trlist LINES lv_lines.
*    lv_linesc = lv_lines.
*    CONCATENATE text-h09 lv_linesc
*                      INTO lv_string SEPARATED BY space.
*    WRITE:130 lv_string.
*    CLEAR lv_string.
*    WRITE :/1(15) text-h11, 17 s_extid-low.
*    WRITE :141 text-h12, 162 sy-uname.
*
*  ENDIF.
*
*  SKIP.
*ENDFORM.                    "TOP_OF_PAGE

*&---------------------------------------------------------------------*
*&      fORM  display_alv
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
*FORM display_alv_project .

*  TYPE-POOLS: slis.
*
*  DATA: t_fldcat      TYPE slis_t_fieldcat_alv.
*  "wa_fldcat     TYPE slis_fieldcat_alv.
*
*
*  DATA:
* " li_layout           TYPE TABLE OF slis_layout_alv,
*  lwa_layout          TYPE          slis_layout_alv,
*  lt_event            TYPE TABLE OF slis_alv_event,
*  lwa_event           TYPE          slis_alv_event.
*
** To colour a cell.
*  "DATA ls_cellcolour  TYPE lvc_s_scol.
*
*  CONSTANTS :
* lc_top(11)           TYPE c VALUE 'TOP_OF_PAGE',
* lc_x(1)              TYPE c VALUE 'X'.


"field catalog--------------------------------------------------------------------"
*  IF rd_obj1 IS NOT INITIAL.
*    "perform FILL_FIELDCAT using 'externalps' 'eXTERNAL pROJECT sYSTEM' changing t_fldcat.
*    PERFORM fill_fieldcat USING 'EXTERNALID' 'Project ID' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'SRCSYSTEM' 'Source System' CHANGING t_fldcat.
*    "perform FILL_FIELDCAT using 'srcclient' 'sOURCE cLIENT' changing t_fldcat.
*    PERFORM fill_fieldcat USING 'PROJ_NAME' 'Project Name' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'DESCRIPTN' 'Project Description' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'TRKORR' 'Request/Task' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'TRFUNCTION' 'Type' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'TRSTATUS' 'Status' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'TARSYSTEM' 'Target System' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'DATE' 'Transport Date' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'TIME' 'Transport Time' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'STATUS' 'Transport Status' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'KORRDEV' 'Category' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'AS4USER' 'User' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'AS4TEXT' 'Request Description' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'PGMID' 'Program ID' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'OBJECT' 'Object' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'OBJ_NAME' 'Object Name' CHANGING t_fldcat.
*  ELSEIF rd_trlt1 IS NOT INITIAL.

"perform FILL_FIELDCAT using 'externalps' 'eXTERNAL pROJECT sYSTEM' changing t_fldcat.
*    PERFORM fill_fieldcat USING 'EXTERNALID' 'Project ID' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'SRCSYSTEM' 'Source System' CHANGING t_fldcat.
*    "perform FILL_FIELDCAT using 'srcclient' 'sOURCE cLIENT' changing t_fldcat.
*    PERFORM fill_fieldcat USING 'PROJ_NAME' 'Project Name' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'DESCRIPTN' 'Project Description' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'TRKORR' 'Request/Task' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'TRFUNCTION' 'Type' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'TRSTATUS' 'Status' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'TARSYSTEM' 'Target System' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'DATE' 'Transport Date' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'TIME' 'Transport Time' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'STATUS' 'Transport Status' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'KORRDEV' 'Category' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'AS4USER' 'User' CHANGING t_fldcat.
*    PERFORM fill_fieldcat USING 'AS4TEXT' 'Request Description' CHANGING t_fldcat.
*  ENDIF.
*
*  DATA: s_fieldcat LIKE LINE OF t_fldcat.
*  s_fieldcat-hotspot = 'X'.
*
*  MODIFY t_fldcat FROM s_fieldcat TRANSPORTING hotspot
*  WHERE fieldname = 'TRKORR'.
*
*  lwa_layout-lights_fieldname = 'ICON'.
*  lwa_layout-colwidth_optimize = lc_x.
*  lwa_layout-zebra             = lc_x.
*  lwa_event-name               = lc_top.
*  lwa_event-form               = lc_top.
*  APPEND lwa_event TO lt_event.
*
*  IF rd_obj1 IS NOT INITIAL.         "---------------------To display Custom Object List
*    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
*      EXPORTING
*        i_callback_program      = sy-repid
*        is_layout               = lwa_layout
*        it_fieldcat             = t_fldcat
*        i_callback_user_command = 'USER_COMMAND'
*        i_save                  = 'A'
*        it_events               = lt_event
*      TABLES
*        t_outtab                = t_final_p
*      EXCEPTIONS
*        program_error           = 1
*        OTHERS                  = 2.
*
*    IF sy-subrc <> 0.
*      MESSAGE e002(ztr).
*    ENDIF.
*
*  ELSEIF rd_trlt1 IS NOT INITIAL.   "--------------------To display TR List
*    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
*      EXPORTING
*        i_callback_program      = sy-repid
*        is_layout               = lwa_layout
*        it_fieldcat             = t_fldcat
*        i_callback_user_command = 'USER_COMMAND1'
*        i_save                  = 'A'
*        it_events               = lt_event
*      TABLES
*        t_outtab                = t_trlist
*      EXCEPTIONS
*        program_error           = 1
*        OTHERS                  = 2.
*
*    IF sy-subrc <> 0.
*      MESSAGE e002(ztr).
*    ENDIF.
*
*
*  ENDIF.

*ENDFORM.                    "display_alv


*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UCOMM      text
*      -->SELFIELD   text
*----------------------------------------------------------------------*
**FORM user_command1 USING ucomm LIKE sy-ucomm
**selfield TYPE slis_selfield.
**
**  IF ucomm = '&IC1'.
**
**    IF t_final_p IS NOT INITIAL.
**      READ TABLE t_final_p INTO wa_final_p INDEX selfield-tabindex.
**      IF sy-subrc = 0.
***      SET PARAMETER ID 'DFD' FIELD wa_final-trkorr.
**        CALL TRANSACTION 'SE01' AND SKIP FIRST SCREEN.
**      ENDIF.
**    ELSEIF t_trlist IS NOT INITIAL.
**      READ TABLE t_trlist INTO wa_final_p INDEX selfield-tabindex.
**      IF sy-subrc = 0.
***      SET PARAMETER ID 'DFD' FIELD wa_final-trkorr.
**        CALL TRANSACTION 'SE01' AND SKIP FIRST SCREEN.
**      ENDIF.
**    ENDIF.
**
**  ENDIF.
**
**ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      fORM  FILL_FIELDCAT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->text1      TEXT
*      -->text2      TEXT
*      -->i_fldcat   TEXT
*----------------------------------------------------------------------*
FORM fill_fieldcat USING ip_text1 TYPE slis_fieldname ip_text2 TYPE slis_fieldname  CHANGING tp_fldcat TYPE STANDARD TABLE.
  DATA: lwa_fldcat  TYPE slis_fieldcat_alv.

  lwa_fldcat-fieldname = ip_text1.
  lwa_fldcat-seltext_m = ip_text2.
  APPEND lwa_fldcat TO tp_fldcat.
  CLEAR lwa_fldcat.
ENDFORM.                    "FILL_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  FILL_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_FIELD    text
*      -->FIELD1     text
*----------------------------------------------------------------------*
FORM fill_range  TABLES r_field USING ip_field1 .

  wa_field-sign = 'I'.
  wa_field-option = 'EQ'.
  wa_field-low = ip_field1.
  APPEND wa_field TO r_field.
  CLEAR wa_field.
ENDFORM.                    "FILL_RANGE
*&---------------------------------------------------------------------*
*&      Form  FILL_RANGE_CB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM fill_range_cb .
*  IF ck_prgm IS NOT INITIAL. "PROGRAM
*    PERFORM fill_range TABLES r_field USING 'PROG'.
*  ENDIF.
*  IF ck_fung IS NOT INITIAL."FUNCTION GROUP
*    PERFORM fill_range TABLES r_field USING 'FUGR'.
*  ENDIF.
*  IF ck_lgdb IS NOT INITIAL."LOGICAL DATABASE
*    PERFORM fill_range TABLES r_field USING 'LDBA'.
*  ENDIF.
*  IF ck_msgc IS NOT INITIAL."MESSAGE CLASS
*    PERFORM fill_range TABLES r_field USING 'MSAG'.
*  ENDIF.
*  IF ck_trns IS NOT INITIAL."TRANSACTION CODE
*    PERFORM fill_range TABLES r_field USING 'TRAN'.
*  ENDIF.
*  IF ck_funm IS NOT INITIAL."FUCNTIONAL MODULE
*    PERFORM fill_range TABLES r_field USING 'FUNC'.
*  ENDIF.
*  IF ck_dlgm IS NOT INITIAL. "dIALOG MODULE
*    PERFORM fill_range TABLES r_field USING 'DIAL'.
*  ENDIF.
*  IF ck_dbtb IS NOT INITIAL. "dATABASE TABLES
*    PERFORM fill_range TABLES r_field USING 'TABL'.
*  ENDIF.
*  IF ck_shlp IS NOT INITIAL. "sEARCH hELP
*    PERFORM fill_range TABLES r_field USING 'SHLP'.
*  ENDIF.
*  IF ck_domn IS NOT INITIAL. "dOMAIN
*    PERFORM fill_range TABLES r_field USING 'DOMA'.
*  ENDIF.
*  IF ck_dtel IS NOT INITIAL. "dATA ELEMENT
*    PERFORM fill_range TABLES r_field USING 'DTEL'.
*  ENDIF.
*  IF ck_auth IS NOT INITIAL. "AUTH GROUPS
*    PERFORM fill_range TABLES r_field USING 'SUSO'.
*  ENDIF.
*  IF ck_type IS NOT INITIAL. "TYPE GROUPS
*    PERFORM fill_range TABLES r_field USING 'TYPE'.
*  ENDIF.
*  IF ck_ttyp IS NOT INITIAL. "TABLE TYPE
*    PERFORM fill_range TABLES r_field USING 'TTYP'.
*  ENDIF.
*  IF ck_stru IS NOT INITIAL. "STRUCTURE
*    PERFORM fill_range TABLES r_field USING 'STRU'.
*  ENDIF.
*  IF ck_lock IS NOT INITIAL. "LOCK OBJCETS
*    PERFORM fill_range TABLES r_field USING 'ENQU'.
*  ENDIF.
*  IF ck_clas IS NOT INITIAL. "CLASS
*    PERFORM fill_range TABLES r_field USING 'CLAS'.
*  ENDIF.
*  IF ck_intf IS NOT INITIAL. "INTERFACES
*    PERFORM fill_range TABLES r_field USING 'INTF'.
*  ENDIF.
*  IF ck_enhm IS NOT INITIAL. "ENHANCEMENTS
*    PERFORM fill_range TABLES r_field USING 'ENHO'.
*  ENDIF.
*
*
*ENDFORM.                    " FILL_RANGE_CB
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM data_process_project.
*
*  "=================================================================================="
*  " Data process
*  "=================================================================================="
*  IF s_extid IS NOT INITIAL.
*    "Project details from ctsproject(ASSIGNMENT OF A CTS PROJECT TO AN EXTERNAL)TABLE
*    SELECT SINGLE *
*     INTO wa_ctsproj
*     FROM ctsproject
*     WHERE externalid IN s_extid.
*
*    IF  sy-subrc = 0 AND wa_ctsproj IS NOT INITIAL.
*      "select transport from  (change & transport system: attributes of a request) table
*      "relation b/n project and TR'S ===> E070A-REFERENCE = CTSPROJ-TRKORR
*      SELECT trkorr
*             reference
*        FROM e070a
*        INTO TABLE t_e070a_p
*        "LEFT OUTER JOIN e070c on e070c~client = ctsproject~srcclient
*        "INTO CORRESPONDING FIELDS OF TABLE i_e070a
*        WHERE  reference EQ wa_ctsproj-trkorr.
*      IF sy-subrc = 0.
*
*        SORT t_e070a_p BY trkorr.
*        DELETE ADJACENT DUPLICATES FROM t_e070a_p COMPARING trkorr.
*
*        "Get TR Details-----------------------------------------"
*        SELECT trkorr
*               trfunction
*               trstatus
*               tarsystem
*               korrdev
*               as4user
*               as4date
*               as4time
*               strkorr
*              FROM e070
*          INTO TABLE t_e070_p
*          FOR ALL ENTRIES IN t_e070a_p
*          WHERE trkorr EQ t_e070a_p-trkorr.
*        IF sy-subrc = 0.
*          SORT t_e070_p BY trstatus.
*        ENDIF.
*
*        "Get not released TR's------------------------------------"
*        DATA:lv_index TYPE sy-index.
*        READ TABLE t_e070_p WITH KEY trstatus = 'D' BINARY SEARCH TRANSPORTING NO FIELDS.
*        IF sy-subrc = 0.
*          lv_index = sy-index.
*          LOOP AT t_e070_p INTO wa_e070_p FROM lv_index.
*            IF sy-subrc = 0.
*              IF wa_e070_p-trstatus NE 'D'.
*                EXIT.
*              ENDIF.
*              APPEND wa_e070_p TO t_e070_1p.
*              CLEAR wa_e070_p.
*            ENDIF.
*          ENDLOOP.
*        ENDIF.
*
*        "Reference pass, to get the task object list--------------"
*        IF t_e070_1p IS NOT INITIAL.
*          SELECT trkorr
*                trfunction
*                trstatus
*                tarsystem
*                korrdev
*                as4user
*                as4date
*                as4time
*                strkorr
*               FROM e070
*           INTO TABLE t_e070_2p
*           FOR ALL ENTRIES IN t_e070_1p
*           WHERE strkorr EQ t_e070_1p-trkorr.
*          IF sy-subrc = 0.
*            APPEND LINES OF t_e070_2p TO t_e070_p.
*            CLEAR: t_e070_1p,t_e070_2p.
*            SORT t_e070_p BY trkorr trstatus.
*          ENDIF.
*        ENDIF.
*
*        "Get TR description--------------------------------------"
*        IF t_e070_p IS NOT INITIAL.
*          SELECT trkorr
*                 as4text
*           FROM e07t
*           INTO TABLE t_e07t_p
*           FOR ALL ENTRIES IN t_e070_p
*           WHERE trkorr EQ t_e070_p-trkorr.
*          IF sy-subrc = 0.
*            SORT t_e07t_p BY trkorr.
*          ENDIF.
*        ENDIF.
*
*        "Get object list------------------------------------------"
*        IF t_e070_p IS NOT INITIAL.
*          SELECT trkorr
*                 pgmid
*                 object
*                 obj_name
*            FROM e071
*            INTO TABLE t_e071_p
*            FOR ALL ENTRIES IN t_e070_p
*            WHERE "PGMID eq 'r3tr' "or PGMID eq 'limu' ) and
*                  "OBJECT in R_FIELD AND
*                   trkorr EQ t_e070_p-trkorr.
*          IF sy-subrc IS INITIAL .
*            DELETE t_e071_p WHERE NOT object IN r_field.
*            IF rd_trlis IS INITIAL.
*              "DELETE t_e071_p WHERE NOT obj_name CP 'Z*'.
**              DELETE t_e071_p WHERE NOT ( obj_name CP 'Z*' OR
**                          obj_name CP 'Y*' OR
**                          obj_name CP 'LZ*' OR
**                          obj_name CP 'LY*' OR
**                          obj_name CP 'M*' OR
**                          obj_name CP 'S*').
*            ENDIF.
*            SORT t_e071_p BY trkorr.
*          ENDIF.
*
*        ENDIF.
*
*      ENDIF.
*
*      "================================================================================="
*      " To Read Transport Import Log to get the Target System Details
*      "================================================================================="
*      LOOP AT t_e070_p INTO wa_e070_p.
*
*        ls_request-header-trkorr = wa_e070_p-trkorr.
*        CALL FUNCTION 'TRINT_READ_REQUEST_HEADER'
*          EXPORTING
*            iv_read_e070 = 'X'
*            iv_read_e07t = 'X'
*          CHANGING
*            cs_request   = ls_request-header
*          EXCEPTIONS
*            OTHERS       = 1.
*        IF sy-subrc <> 0.
*          ls_request-header-trkorr = wa_e070_p-trkorr.
*        ENDIF.
*
*        CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
*          EXPORTING
*            iv_trkorr   = wa_e070_p-trkorr
*            is_settings = ls_settings            "iv_dir_type = iv_dir_type
*          IMPORTING
*            es_cofile   = ls_request-cofile
*            ev_user     = lv_username
*            ev_project  = ls_request-project.
*        IF ls_request-header-as4user = space.
*          ls_request-header-as4user = lv_username.
*        ENDIF.
*        ls_request-cofile_filled = 'X'.
*        APPEND ls_request TO lt_requests.
*        CLEAR wa_e070_p-trkorr.
*      ENDLOOP.
*
*      SORT lt_requests BY header-trkorr.
*
*      DATA: lv_strkorr TYPE e070-strkorr.
*
*      "======================================================================================"
*      " To get Transport List Used In the Project
*      "======================================================================================"
*      IF rd_trlt1 IS NOT INITIAL.
*
*        LOOP AT t_e070_p INTO wa_e070_p.
*
*          wa_trlist-externalid = wa_ctsproj-externalid.
*          wa_trlist-srcsystem = wa_ctsproj-srcsystem.
*          wa_trlist-proj_name = wa_ctsproj-trkorr.
*          wa_trlist-descriptn = wa_ctsproj-descriptn.
*          wa_trlist-trkorr = wa_e070_p-trkorr.
*          wa_trlist-trfunction = wa_e070_p-trfunction.
*          wa_trlist-trstatus = wa_e070_p-trstatus.
*
*          "----------------------------------------------------------------------
*          IF wa_e070_p-strkorr IS INITIAL.
*            READ TABLE lt_requests ASSIGNING <fs_requests>  WITH KEY header-trkorr = wa_e070_p-trkorr BINARY SEARCH.
*            IF sy-subrc = 0.
*              SORT <fs_requests>-cofile-systems BY systemid DESCENDING.
*            ENDIF.
*          ELSEIF wa_e070_p-strkorr IS NOT INITIAL.   "higher level request exists
*            CLEAR lv_strkorr.
*            lv_strkorr = wa_e070_p-strkorr.
*            CLEAR wa_e070_p.
*            READ TABLE t_e070_p INTO wa_e070_p WITH KEY trkorr = lv_strkorr BINARY SEARCH.
*            IF sy-subrc = 0.
*              READ TABLE lt_requests ASSIGNING <fs_requests>  WITH KEY header-trkorr = lv_strkorr BINARY SEARCH.
*              IF sy-subrc = 0.
*                SORT <fs_requests>-cofile-systems BY systemid DESCENDING.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*
*          IF <fs_requests> IS ASSIGNED.
*            READ TABLE <fs_requests>-cofile-systems ASSIGNING <fs_systems> INDEX 1.
*            IF sy-subrc = 0.
*              wa_trlist-tarsystem = <fs_systems>-systemid.
*
*              SORT <fs_systems>-steps BY clientid DESCENDING.
*              READ TABLE <fs_systems>-steps ASSIGNING <fs_steps> INDEX 1.
*              IF sy-subrc = 0.
*
*                READ TABLE <fs_steps>-actions ASSIGNING <fs_actions> INDEX 1.
*                IF sy-subrc = 0.
*
*                  wa_trlist-date = <fs_actions>-date.
*                  wa_trlist-time = <fs_actions>-time.
*
*                  IF <fs_systems>-rc = 0.
*                    wa_trlist-icon = '3'.
*                    wa_trlist-status = 'Suuccessfully Transported with RC 0'.
*                  ELSEIF <fs_systems>-rc = 4.
*                    wa_trlist-icon = '2'.
*                    wa_trlist-status = 'Suuccessfully Transported with RC 4 (Warning)'.
*                  ELSEIF <fs_systems>-rc = 8.
*                    wa_trlist-icon = '1'.
*                    wa_trlist-status = 'Transported with RC 8 (Error)'.
*                  ELSEIF <fs_systems>-rc = 12.
*                    wa_trlist-icon = '1'.
*                    wa_trlist-status = 'Transported with RC 12 (Import Error)'.
*                  ENDIF.
*                ENDIF.
*              ENDIF.
*            ELSE.
*              wa_trlist-tarsystem = wa_e070_p-tarsystem.
*              wa_trlist-date = wa_e070_p-as4date.
*              wa_trlist-time = wa_e070_p-as4time.
*
*              IF wa_e070_p-trstatus EQ 'D' OR
*               wa_e070_p-trstatus EQ 'L'.
*                IF lv_strkorr IS NOT INITIAL.
*                  wa_trlist-icon = '3'.
*                  wa_trlist-status = 'Tranport Modifiable'.
*                ELSE.
*                  wa_trlist-icon = '3'.
*                  wa_trlist-status = 'Tranport released'.
*                ENDIF.
*              ELSE.
*                wa_trlist-icon = '3'.
*                wa_trlist-status = 'Tranport released'.
*              ENDIF.
*            ENDIF.
*          ELSE.
*            IF wa_e070_p-trstatus EQ 'D' OR
*             wa_e070_p-trstatus EQ 'L'.
*              IF lv_strkorr IS NOT INITIAL.
*                wa_trlist-icon = '3'.
*                wa_trlist-status = 'Tranport Modifiable'.
*              ELSE.
*                wa_trlist-icon = '3'.
*                wa_trlist-status = 'Tranport released'.
*              ENDIF.
*            ELSE.
*              wa_trlist-icon = '3'.
*              wa_trlist-status = 'Tranport released'.
*            ENDIF.
*          ENDIF.
*
*          "-------------------------------------------------------
*          "wa_trlist-tarsystem = wa_e070-tarsystem.
*          wa_trlist-korrdev = wa_e070_p-korrdev.
*          wa_trlist-as4user = wa_e070_p-as4user.
**            ENDIF.
*          READ TABLE t_e07t_p INTO wa_e07t_p WITH KEY trkorr = wa_e070_p-trkorr BINARY SEARCH.
*          IF sy-subrc = 0.
*            wa_trlist-as4text = wa_e07t_p-as4text.
*          ENDIF.
*          APPEND wa_trlist TO t_trlist.
*          CLEAR wa_trlist.
*          CLEAR wa_final_p.
*        ENDLOOP.
*        IF t_e071_p IS INITIAL OR t_trlist IS INITIAL.
*          MESSAGE s001(ztr) DISPLAY LIKE 'E'.
*          LEAVE LIST-PROCESSING.
*        ENDIF.
*
*
*        "====================================================================================="
*        " To include and Exclude Copy TR and Not released TR's
*        "====================================================================================="
*        "Include Copy TR's----------------------------------------------------------"
*        IF ck_copy1 IS NOT INITIAL AND
*          t_trlist IS NOT INITIAL.
*          SORT t_trlist BY trkorr trfunction.
*        ELSE.
*          "Exlude Copy TR's
*          DELETE t_trlist WHERE trfunction EQ 'T'.
*          SORT t_trlist BY trkorr .
*        ENDIF.
*
*        "Include Not released TR's---------------------------------------------------"
*        IF ck_rel1 IS NOT INITIAL AND
*         t_trlist IS NOT INITIAL.
*          SORT t_trlist BY trkorr trfunction.
*        ELSE.
*          "Exlude not released TR's
*          DELETE t_trlist WHERE ( trstatus EQ 'D' OR trstatus EQ 'L').
*          SORT t_trlist BY trkorr.
*        ENDIF.
*
*        "Target System Check----------------------------------------------------------"
*        IF s_trgt1 IS NOT INITIAL.
*          DELETE t_trlist WHERE NOT tarsystem = s_trgt1-low.
*        ENDIF.
*
*      ENDIF.
*      "======================================================================================"
*      " To get Object List Used in the Project                                               "
*      "======================================================================================"
*      IF rd_trlt1 IS INITIAL.
*        IF t_e071_p IS NOT INITIAL.
*          "SORT COMPARING TR NUMBER AND OBJECT NAME
*          SORT t_e071_p BY trkorr obj_name.
*          DELETE ADJACENT DUPLICATES FROM t_e071_p COMPARING trkorr obj_name.
*
*          LOOP AT t_e071_p INTO wa_e071_p.
*            "WA_FINAL-EXTERNALPS = WA_CTSPROJ-EXTERNALPS.
*            wa_final_p-externalid = wa_ctsproj-externalid.
*            wa_final_p-srcsystem = wa_ctsproj-srcsystem.
*            "wa_final_p-SRCCLIENT = WA_CTSPROJ-SRCCLIENT.
*            wa_final_p-proj_name = wa_ctsproj-trkorr.
*            wa_final_p-descriptn = wa_ctsproj-descriptn.
*
*            wa_final_p-trkorr = wa_e071_p-trkorr.
*
*
*
*            READ TABLE t_e070_p INTO wa_e070_p WITH KEY trkorr = wa_e071_p-trkorr BINARY SEARCH.
*            IF sy-subrc = 0.
*              wa_final_p-trfunction = wa_e070_p-trfunction.
*              wa_final_p-trstatus = wa_e070_p-trstatus.
*              "wa_final_p-tarsystem = wa_e070_p-tarsystem.
*              wa_final_p-korrdev = wa_e070_p-korrdev.
*              wa_final_p-as4user = wa_e070_p-as4user.
*
*
*              IF wa_e070_p-strkorr IS INITIAL.
*                READ TABLE lt_requests ASSIGNING <fs_requests>  WITH KEY header-trkorr = wa_e070_p-trkorr BINARY SEARCH.
*                IF sy-subrc = 0.
*                  SORT <fs_requests>-cofile-systems BY systemid DESCENDING.
*                ENDIF.
*              ELSEIF wa_e070_p-strkorr IS NOT INITIAL.   "higher level request exists
*                CLEAR lv_strkorr.
*                lv_strkorr = wa_e070_p-strkorr.
*                CLEAR wa_e070_p.
*                READ TABLE t_e070_p INTO wa_e070_p WITH KEY trkorr = lv_strkorr BINARY SEARCH.
*                IF sy-subrc = 0.
*                  READ TABLE lt_requests ASSIGNING <fs_requests>  WITH KEY header-trkorr = lv_strkorr BINARY SEARCH.
*                  IF sy-subrc = 0.
*                    SORT <fs_requests>-cofile-systems BY systemid DESCENDING.
*                  ENDIF.
*                ENDIF.
*              ENDIF.
*
*              IF <fs_requests> IS ASSIGNED.
*                READ TABLE <fs_requests>-cofile-systems ASSIGNING <fs_systems> INDEX 1.
*                IF sy-subrc = 0.
*                  wa_final_p-tarsystem = <fs_systems>-systemid.
*
*                  SORT <fs_systems>-steps BY clientid DESCENDING.
*                  READ TABLE <fs_systems>-steps ASSIGNING <fs_steps> INDEX 1.
*                  IF sy-subrc = 0.
*
*                    READ TABLE <fs_steps>-actions ASSIGNING <fs_actions> INDEX 1.
*                    IF sy-subrc = 0.
*
*                      wa_final_p-date = <fs_actions>-date.
*                      wa_final_p-time = <fs_actions>-time.
*
*                      IF <fs_systems>-rc = 0.
*                        wa_final_p-icon = '3'.
*                        wa_final_p-status = 'Suuccessfully Transported with RC 0'.
*                      ELSEIF <fs_systems>-rc = 4.
*                        wa_final_p-icon = '2'.
*                        wa_final_p-status = 'Suuccessfully Transported with RC 4 (Warning)'.
*                      ELSEIF <fs_systems>-rc = 8.
*                        wa_final_p-icon = '1'.
*                        wa_final_p-status = 'Transported with RC 8 (Error)'.
*                      ELSEIF <fs_systems>-rc = 12.
*                        wa_final_p-icon = '1'.
*                        wa_final_p-status = 'Transported with RC 12 (Import Error)'.
*                      ENDIF.
*                    ENDIF.
*                  ENDIF.
*                ELSE.
*                  wa_final_p-tarsystem = wa_e070_p-tarsystem.
*                  wa_final_p-date = wa_e070_p-as4date.
*                  wa_final_p-time = wa_e070_p-as4time.
*
*                  IF wa_e070_p-trstatus EQ 'D' OR
*                   wa_e070_p-trstatus EQ 'L'.
*                    IF lv_strkorr IS NOT INITIAL.
*                      wa_final_p-icon = '3'.
*                      wa_final_p-status = 'Tranport Modifiable'.
*                    ELSE.
*                      wa_final_p-icon = '3'.
*                      wa_final_p-status = 'Tranport released'.
*                    ENDIF.
*                  ELSE.
*                    wa_final_p-icon = '3'.
*                    wa_final_p-status = 'Tranport released'.
*                  ENDIF.
*                ENDIF.
*              ELSE.
*                IF wa_e070_p-trstatus EQ 'D' OR
*                 wa_e070_p-trstatus EQ 'L'.
*                  IF lv_strkorr IS NOT INITIAL.
*                    wa_final_p-icon = '3'.
*                    wa_final_p-status = 'Tranport Modifiable'.
*                  ELSE.
*                    wa_final_p-icon = '3'.
*                    wa_final_p-status = 'Tranport released'.
*                  ENDIF.
*                ELSE.
*                  wa_final_p-icon = '3'.
*                  wa_final_p-status = 'Tranport released'.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*
*            READ TABLE t_e07t_p INTO wa_e07t_p WITH KEY trkorr = wa_e071_p-trkorr BINARY SEARCH.
*            IF sy-subrc = 0.
*              wa_final_p-as4text = wa_e07t_p-as4text.
*            ENDIF.
*
*            wa_final_p-pgmid = wa_e071_p-pgmid.
*            wa_final_p-object = wa_e071_p-object.
*            wa_final_p-obj_name = wa_e071_p-obj_name.
*
*            APPEND wa_final_p TO t_final_p.
*            CLEAR wa_final_p.
*            CLEAR wa_e07t_p.
*
*          ENDLOOP.
*          CLEAR wa_ctsproj.
*
*          SORT t_final_p BY trkorr trstatus pgmid object obj_name.
*          DELETE ADJACENT DUPLICATES FROM t_final_p COMPARING trkorr trstatus pgmid object obj_name.
*
*
*          IF t_e071_p IS INITIAL OR t_final_p IS INITIAL.
*            MESSAGE s001(ztr) DISPLAY LIKE 'E'.
*            LEAVE LIST-PROCESSING.
*          ENDIF.
*
*        ENDIF.
*
*
*        "====================================================================================="
*        " To include and Exclude Copy TR and Not released TR's
*        "====================================================================================="
*        "Include Copy TR's----------------------------------------------------------"
*        IF ck_copy1 IS NOT INITIAL AND
*          t_final_p IS NOT INITIAL.
*          SORT t_final_p BY trkorr trfunction.
*        ELSE.
*          "Exlude Copy TR's
*          DELETE t_final_p WHERE trfunction EQ 'T'.
*          SORT t_final_p BY trkorr .
*        ENDIF.
*
*        "Include Not released TR's---------------------------------------------------"
*        IF ck_rel1 IS NOT INITIAL AND
*         t_final_p IS NOT INITIAL.
*          SORT t_final_p BY trkorr trfunction.
*        ELSE.
*          "Exlude not released TR's
*          DELETE t_final_p WHERE ( trstatus EQ 'D' OR trstatus EQ 'L').
*          SORT t_final_p BY trkorr.
*        ENDIF.
*
*        "Target System Check--------------------------------------------------------"
*        IF s_trgt1 IS NOT INITIAL.
*          DELETE t_final_p WHERE NOT tarsystem = s_trgt1-low.
*        ENDIF.
*
*        "Object Type range------------------------------------------------------------"
*        IF r_field IS NOT INITIAL.
*          DELETE t_final_p WHERE NOT object IN r_field.
*        ENDIF.
*
*      ENDIF. "End for CTS project
*
*    ENDIF. "End for project ID
*  ENDIF.
*
*ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  CTSPROJ_F4HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM ctsproj_f4help .
*
*  TYPES: BEGIN OF s_externalid,
*       externalid TYPE ctsproject-externalid,
*       END OF s_externalid.
*
*  DATA:   li_ctsproject TYPE STANDARD TABLE OF s_externalid.
*
*  " Select data for F4 help----------------------------------------------"
*
*  SELECT externalid FROM ctsproject
*      INTO TABLE li_ctsproject.
*  IF sy-subrc EQ 0.
*    SORT li_ctsproject BY externalid.
*  ENDIF.
*
*  "Function module get F4 help for a field--------------------------------"
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      retfield        = 'EXTERNALID'
*      dynpprog        = sy-repid    " Program name
*      dynpnr          = sy-dynnr    " Screen number
*      dynprofield     = 'S_EXTID'   " F4 help need field
*      value_org       = 'S'
*    TABLES
*      value_tab       = li_ctsproject    " F4 help values
*    EXCEPTIONS
*      parameter_error = 1
*      no_values_found = 2
*      OTHERS          = 3.
*
*ENDFORM.                    " CTSPROJ_F4HELP


"=========================================================================="
"------------------Report Forms--------------------------------------------"
"=========================================================================="


*&---------------------------------------------------------------------*
*&      Form  VALIDATE_PROGRAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_program .

  " cts project structure--------------------------"
  TYPES: BEGIN OF s_prog,
           obj_name TYPE e071-obj_name,
         END OF s_prog.

  DATA: lwa_prog TYPE s_prog.

*  SELECT SINGLE obj_name
*    FROM e071
*    INTO lwa_prog "UP TO 1 ROWS
*    WHERE ( pgmid EQ 'R3TR' OR
*            pgmid EQ 'LIMU' ) AND
*         obj_name IN s_pgm.
*  IF sy-subrc <> 0.
*    MESSAGE e004(ztr).
*  ENDIF.

ENDFORM.                    " VALIDATE_PROGRAM
*&---------------------------------------------------------------------*
*&      Form  FILL_RANGE_CHECKBOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_range_checkbox .
  REFRESH r_field.
  IF rd_reprt IS NOT INITIAL.
*    ck_prgm = ck_prgm1.
*    ck_fung = ck_fung1.
*    ck_lgdb = ck_lgdb1.
*    ck_msgc = ck_msgc1.
*    ck_trns = ck_trns1.
*    ck_funm = ck_funm1.
*    ck_dlgm = ck_dlgm1.
*    ck_dbtb = ck_dbtb1.
*    ck_shlp = ck_shlp1.
*    ck_domn = ck_domn1.
*    ck_dtel = ck_dtel1.
*    ck_auth = ck_auth1.
*    ck_type = ck_type1.
*    ck_ttyp = ck_ttyp1.
*    ck_stru = ck_stru1.
*    ck_lock = ck_lock1.
*    ck_clas = ck_clas1.
*    ck_intf = ck_intf1.
*    ck_enhm = ck_enhm1.

    IF ck_prgm1 IS NOT INITIAL . "PROGRAM
      PERFORM fill_range TABLES r_field USING 'PROG'.
      PERFORM fill_range TABLES r_field USING 'REPS'.
      PERFORM fill_range TABLES r_field USING 'REPT'.
      PERFORM fill_range TABLES r_field USING 'DYNP'.
    ENDIF.
    IF ck_fung1 IS NOT INITIAL."FUNCTION GROUP
      PERFORM fill_range TABLES r_field USING 'FUGR'.
      "PERFORM fill_range TABLES r_field USING 'FUGT'.
      PERFORM fill_range TABLES r_field USING 'FUGS'.
      PERFORM fill_range TABLES r_field USING 'FUGX'.
    ENDIF.
    IF ck_lgdb1 IS NOT INITIAL ."LOGICAL DATABASE
      PERFORM fill_range TABLES r_field USING 'LDBA'.
    ENDIF.
    IF ck_msgc1 IS NOT INITIAL ."MESSAGE CLASS
      PERFORM fill_range TABLES r_field USING 'MSAG'.
    ENDIF.
    IF ck_trns1 IS NOT INITIAL."TRANSACTION CODE
      PERFORM fill_range TABLES r_field USING 'TRAN'.
    ENDIF.
    IF ck_funm1 IS NOT INITIAL ."FUCNTIONAL MODULE
      PERFORM fill_range TABLES r_field USING 'FUNC'.
    ENDIF.
    IF ck_dlgm1 IS NOT INITIAL. "dIALOG MODULE
      PERFORM fill_range TABLES r_field USING 'DIAL'.
    ENDIF.
    IF ck_dbtb1 IS NOT INITIAL . "dATABASE TABLES
      PERFORM fill_range TABLES r_field USING 'TABL'.
      "PERFORM fill_range TABLES r_field USING 'TABU'.
      "PERFORM fill_range TABLES r_field USING 'TOBJ'.
    ENDIF.
    IF ck_shlp1 IS NOT INITIAL . "sEARCH hELP
      PERFORM fill_range TABLES r_field USING 'SHLP'.
      "PERFORM fill_range TABLES r_field USING 'SHLD'.
      "PERFORM fill_range TABLES r_field USING 'SHLX'.
    ENDIF.
    IF ck_domn1 IS NOT INITIAL . "dOMAIN
      PERFORM fill_range TABLES r_field USING 'DOMA'.
    ENDIF.
    IF ck_dtel1 IS NOT INITIAL . "dATA ELEMENT
      PERFORM fill_range TABLES r_field USING 'DTEL'.
    ENDIF.
    IF ck_auth1 IS NOT INITIAL . "AUTH GROUPS
      PERFORM fill_range TABLES r_field USING 'SUSO'.
    ENDIF.
    IF ck_type1 IS NOT INITIAL. "TYPE GROUPS
      PERFORM fill_range TABLES r_field USING 'TYPE'.
    ENDIF.
    IF ck_ttyp1 IS NOT INITIAL . "TABLE TYPE
      PERFORM fill_range TABLES r_field USING 'TTYP'.
    ENDIF.
    IF ck_stru1 IS NOT INITIAL . "STRUCTURE
      PERFORM fill_range TABLES r_field USING 'STRU'.
    ENDIF.
    IF ck_lock1 IS NOT INITIAL . "LOCK OBJCETS
      PERFORM fill_range TABLES r_field USING 'ENQU'.
    ENDIF.
    IF ck_clas1 IS NOT INITIAL . "CLASS
      PERFORM fill_range TABLES r_field USING 'CLAS'.
    ENDIF.
    IF ck_intf1 IS NOT INITIAL . "INTERFACES
      PERFORM fill_range TABLES r_field USING 'INTF'.
    ENDIF.
    IF ck_enhm1 IS NOT INITIAL . "ENHANCEMENTS
      PERFORM fill_range TABLES r_field USING 'ENHO'.
      PERFORM fill_range TABLES r_field USING 'ENHC'.
      PERFORM fill_range TABLES r_field USING 'ENHS'.
      PERFORM fill_range TABLES r_field USING 'ENSC'.
    ENDIF.
    IF ck_smfm1 IS NOT INITIAL . "Smartforms
      PERFORM fill_range TABLES r_field USING 'SSFO'.
    ENDIF.
  ENDIF.
*
*
ENDFORM.                    " FILL_RANGE_CHECKBOX
*&---------------------------------------------------------------------*
*&      fORM  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       tOP OF THE alv WITH DATE TIME AND FILEPATH
*----------------------------------------------------------------------*
FORM top_of_page.
  DATA:
    lv_string(1000) TYPE c,
    lv_lines        TYPE i,
    lv_linesc(10)   TYPE c.

  IF rd_reprt IS NOT INITIAL. "--------------------report/object

    IF rd_trlis IS NOT INITIAL.
      FORMAT INTENSIFIED ON.
      WRITE: /1(175) text-h07 CENTERED.
    ELSEIF rd_obj IS NOT INITIAL.
      FORMAT INTENSIFIED ON.
      WRITE: /1(175) text-h10 CENTERED.
    ENDIF.
    SKIP 1.
    WRITE : /1(15) text-h08, sy-datum, '/', sy-uzeit.

    "Total No. OF Records Selected
    DESCRIBE TABLE t_final_r LINES lv_lines.
    lv_linesc = lv_lines.
    CONCATENATE text-h09 lv_linesc
                      INTO lv_string." SEPARATED BY space.
    IF rd_trlis IS NOT INITIAL.
      WRITE:130 lv_string.
    ELSE.
      WRITE:140 lv_string.
    ENDIF.
    IF rd_trlis IS NOT INITIAL.
      WRITE :/1(15) text-h11, 17 s_pgm-low.
      WRITE :142 text-h12, 161 sy-uname.
    ELSEIF rd_obj IS NOT INITIAL.
      WRITE :/1(15) text-h11, 17 s_pgm-low.
      WRITE :152 text-h12, 170 sy-uname.
    ENDIF.

    CLEAR lv_string.
    SKIP.

*  ELSEIF rd_proj IS NOT INITIAL."------------------project
*
*    IF rd_obj1 IS NOT INITIAL. "--------object list
*      FORMAT INTENSIFIED ON.
*      WRITE: /1(160) text-h27 CENTERED.
*      SKIP 1.
*      WRITE : /1(15) text-h08, sy-datum, '/', sy-uzeit.
*
*      "Total No. OF Records Selected
*      DESCRIBE TABLE t_final_p LINES lv_lines.
*      lv_linesc = lv_lines.
*      CONCATENATE text-h09 lv_linesc
*                        INTO lv_string SEPARATED BY space.
*      WRITE:130 lv_string.
*      CLEAR lv_string.
*      WRITE :/1(15) text-h22, 19 s_extid-low.
*      WRITE :142 text-h12, 162 sy-uname.
*    ELSEIF rd_trlt1 IS NOT INITIAL."----------TR list
*      FORMAT INTENSIFIED ON.
*      WRITE: /1(160) text-h30 CENTERED.
*      SKIP 1.
*      WRITE : /1(15) text-h08, sy-datum, '/', sy-uzeit.
*
*      "tOTAL nO. OF rECORDS sELECTED
*      DESCRIBE TABLE t_trlist LINES lv_lines.
*      lv_linesc = lv_lines.
*      CONCATENATE text-h09 lv_linesc
*                        INTO lv_string SEPARATED BY space.
*      WRITE:130 lv_string.
*      CLEAR lv_string.
*      WRITE :/1(15) text-h22, 19 s_extid-low.
*      WRITE :142 text-h12, 162 sy-uname.
*      SKIP.
*    ENDIF.
  ELSEIF rd_cnflc IS NOT INITIAL .

    IF t_conflict IS NOT INITIAL.
      FORMAT INTENSIFIED ON.
      WRITE: /1(135) text-h31 CENTERED.
      SKIP 1.
      WRITE : /1(15) text-h08, 20 sy-datum, '/', sy-uzeit.

      "Total No. OF Records Selected
      DESCRIBE TABLE t_conflict LINES lv_lines.
      lv_linesc = lv_lines.
      CONCATENATE text-h09 lv_linesc
                        INTO lv_string." SEPARATED BY space.
      WRITE:80 lv_string.
      CLEAR lv_string.
      WRITE :/1(15) text-h38, 20 sy-sysid.
      WRITE :90 text-h39, 111 p_system.
      IF s_req IS NOT INITIAL.
        WRITE :/1(15) text-h40, 20 s_req-low.
        WRITE :93 text-h12, 111 sy-uname.
      ELSEIF s_report IS NOT INITIAL.
        WRITE :/1(15) text-h11, 20 s_report-low.
        WRITE :92 text-h12, 111 sy-uname.
*      ELSEIF s_smrtfm IS NOT INITIAL.
*        WRITE :/1(15) text-h41, 20 s_smrtfm-low.
*        WRITE :92 text-h12, 111 sy-uname.
      ELSEIF s_table IS NOT INITIAL.
        WRITE :/1(15) text-h42, 20 s_table-low.
        WRITE :93 text-h12, 111 sy-uname.
      ELSEIF s_domain IS NOT INITIAL.
        WRITE :/1(15) text-h43, 20 s_domain-low.
        WRITE :93 text-h12, 111 sy-uname.
      ELSEIF s_fugp IS NOT INITIAL.
        WRITE :/1(15) text-h44, 20 s_fugp-low.
        WRITE :93 text-h12, 111 sy-uname.
      ELSEIF s_func IS NOT INITIAL.
        WRITE :/1(15) text-h45, 20 s_func-low.
        WRITE :93 text-h12, 111 sy-uname.
      ENDIF.
      SKIP.

    ELSEIF t_deptr IS NOT INITIAL.
      FORMAT INTENSIFIED ON.
      WRITE: /1(135) text-h04 CENTERED.
      SKIP 1.
      WRITE : /1(15) text-h08, 20 sy-datum, '/', sy-uzeit.

      "Total No. OF Records Selected
      DESCRIBE TABLE t_deptr LINES lv_lines.
      lv_linesc = lv_lines.
      CONCATENATE text-h09 lv_linesc
                        INTO lv_string." SEPARATED BY space.
      WRITE:120 lv_string.
      CLEAR lv_string.

      WRITE :/1(15) text-h38, 20 sy-sysid.
      WRITE :130 text-h39, 152 p_system.
      WRITE :/1(15) text-h18, 20 s_req-low.
      WRITE :134 text-h12, 152 sy-uname.
      SKIP.
    ELSEIF t_missing IS NOT INITIAL.
      FORMAT INTENSIFIED ON.
      WRITE: /1(135) text-h05 CENTERED.
      SKIP 1.
      WRITE : /1(15) text-h08,  20 sy-datum, '/', sy-uzeit.

      "Total No. OF Records Selected
      DESCRIBE TABLE t_missing LINES lv_lines.
      lv_linesc = lv_lines.
      CONCATENATE text-h09 lv_linesc
                        INTO lv_string ." SEPARATED BY space.
      WRITE:110 lv_string.
      CLEAR lv_string.

      WRITE :/1(15) text-h38, 20 sy-sysid.
      WRITE :120 text-h39, 143 p_system.
      WRITE :/1(15) text-h18, 20 s_req-low.
      WRITE :124 text-h12, 143 sy-uname.
      SKIP.
    ENDIF.


  ENDIF.
ENDFORM.                    "TOP_OF_PAGE



*&---------------------------------------------------------------------*
*&      fORM  display_alv
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM display_alv_object.

  "  TYPE-POOLS: slis.

  DATA: t_fldcat      TYPE slis_t_fieldcat_alv.


  DATA:
    lwa_layout TYPE          slis_layout_alv,
    lt_event   TYPE TABLE OF slis_alv_event,
    lwa_event  TYPE          slis_alv_event.


  CONSTANTS :
    lc_top(11) TYPE c VALUE 'TOP_OF_PAGE',
    lc_x(1)    TYPE c VALUE 'X'.


  "field catalog--------------------------------------------------------------------"

  PERFORM fill_fieldcat USING 'TRKORR' 'Request/Task' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'TRFUNCTION' 'Type' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'TRSTATUS' 'Status' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'TARSYSTEM' 'Target System' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'DATE' 'Transport Date' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'TIME' 'Transport Time' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'STATUS' 'Transport Status' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'KORRDEV' 'Category' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'AS4USER' 'User' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'AS4TEXT' 'Request Description' CHANGING t_fldcat.
*  PERFORM fill_fieldcat USING 'AS4TEXT' 'Request Description' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'PGMID' 'Program ID' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'OBJECT' 'Object' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'OBJ_NAME' 'Object Name' CHANGING t_fldcat.

  DATA: s_fieldcat LIKE LINE OF t_fldcat.
  s_fieldcat-hotspot = 'X'.

  MODIFY t_fldcat FROM s_fieldcat TRANSPORTING hotspot
  WHERE fieldname = 'TRKORR'.

  lwa_layout-lights_fieldname = 'ICON'.
  lwa_layout-colwidth_optimize = lc_x.
  lwa_layout-zebra             = lc_x.
  lwa_event-name               = lc_top.
  lwa_event-form               = lc_top.

  APPEND lwa_event TO lt_event.

  IF rd_obj IS NOT INITIAL.         "---------------------To display Custom Object List
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_callback_program      = sy-repid
        is_layout               = lwa_layout
        it_fieldcat             = t_fldcat
        i_callback_user_command = 'USER_COMMAND'
        i_save                  = 'A'
        it_events               = lt_event
      TABLES
        t_outtab                = t_final_r
      EXCEPTIONS
        program_error           = 1
        OTHERS                  = 2.

    IF sy-subrc <> 0.
      MESSAGE e002(ztr).
    ENDIF.

  ELSEIF rd_trlis IS NOT INITIAL.   "--------------------To display TR List
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_callback_program      = sy-repid
        is_layout               = lwa_layout
        it_fieldcat             = t_fldcat
        i_callback_user_command = 'USER_COMMAND'
        i_save                  = 'A'
        it_events               = lt_event
      TABLES
        t_outtab                = t_final_r
      EXCEPTIONS
        program_error           = 1
        OTHERS                  = 2.

    IF sy-subrc <> 0.
      MESSAGE e002(ztr).
    ENDIF.
  ENDIF.

ENDFORM.                    "display_alv


*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UCOMM      text
*      -->SELFIELD   text
*----------------------------------------------------------------------*
FORM user_command USING ucomm LIKE sy-ucomm
selfield TYPE slis_selfield.

  IF ucomm = '&IC1'.
    IF t_final_r IS NOT INITIAL.
      READ TABLE t_final_r INTO wa_final_r INDEX selfield-tabindex.
      IF sy-subrc = 0.
        "SET PARAMETER ID 'DFD' FIELD wa_final_r-trkorr.
        SET PARAMETER ID 'KOR' FIELD selfield-value.
        CALL TRANSACTION 'SE01' AND SKIP FIRST SCREEN.
        IF sy-subrc = 0.
          " SET PARAMETER ID 'BNAME' FIELD wa_final_r-trkorr.
          " CALL TRANSACTION 'SE01' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.
    ELSEIF t_final_p IS NOT INITIAL.
      READ TABLE t_final_p INTO wa_final_p INDEX selfield-tabindex.
      IF sy-subrc = 0.
        SET PARAMETER ID 'KOR' FIELD selfield-value.
        CALL TRANSACTION 'SE01' AND SKIP FIRST SCREEN.
        IF sy-subrc = 0.
        ENDIF.
      ENDIF.
    ELSEIF t_trlist IS NOT INITIAL.
      READ TABLE t_trlist INTO wa_final_p INDEX selfield-tabindex.
      IF sy-subrc = 0.
        SET PARAMETER ID 'KOR' FIELD selfield-value.
        CALL TRANSACTION 'SE01' AND SKIP FIRST SCREEN.
        IF sy-subrc = 0.
        ENDIF.
      ENDIF.

    ELSEIF t_conflict IS NOT INITIAL.
      CLEAR wa_final.
      READ TABLE t_conflict INTO wa_final INDEX selfield-tabindex.
      IF sy-subrc = 0.
        SET PARAMETER ID 'KOR' FIELD selfield-value.
        CALL TRANSACTION 'SE01' AND SKIP FIRST SCREEN.
        IF sy-subrc = 0.
        ENDIF.
      ENDIF.
    ELSEIF t_deptr IS NOT INITIAL.
      CLEAR wa_final.
      READ TABLE t_deptr INTO wa_final INDEX selfield-tabindex.
      IF sy-subrc = 0.
        SET PARAMETER ID 'KOR' FIELD selfield-value.
        CALL TRANSACTION 'SE01' AND SKIP FIRST SCREEN.
        IF sy-subrc = 0.
        ENDIF.
      ENDIF.
    ELSEIF t_missing IS NOT INITIAL.
      CLEAR wa_final.
      READ TABLE t_deptr INTO wa_final INDEX selfield-tabindex.
      IF sy-subrc = 0.
        SET PARAMETER ID 'KOR' FIELD selfield-value.
        CALL TRANSACTION 'SE01' AND SKIP FIRST SCREEN.
        IF sy-subrc = 0.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.



ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      fORM  FILL_FIELDCAT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->text1      TEXT
*      -->text2      TEXT
*      -->i_fldcat   TEXT
*----------------------------------------------------------------------*
*FORM fill_fieldcat USING text1 TYPE slis_fieldname text2 TYPE slis_fieldname  CHANGING i_fldcat TYPE STANDARD TABLE.
*  DATA: "i_fldcat  type slis_t_fieldcat_alv,
*     wa_fldcat  TYPE slis_fieldcat_alv.
*  wa_fldcat-fieldname = text1.
*  wa_fldcat-seltext_m = text2.
*  APPEND wa_fldcat TO i_fldcat.
*  CLEAR wa_fldcat.
*ENDFORM.                    "FILL_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  FILL_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_FIELD    text
*      -->FIELD1     text
*----------------------------------------------------------------------*
*FORM fill_range  TABLES r_field USING field1 .
*  wa_field-sign = 'I'.
*  wa_field-option = 'EQ'.
*  wa_field-low = field1.
*  APPEND wa_field TO r_field.
*  CLEAR wa_field.
*ENDFORM.                    "FILL_RANGE
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_process_object.
  DATA: lv_strkorr TYPE e070-strkorr.
  IF s_pgm IS NOT INITIAL AND s_trgt IS NOT INITIAL.
*    IF s_pgm IS NOT INITIAL.
    "Get all the TR's used By the object
    SELECT trkorr
           pgmid
           object
           obj_name
      FROM e071
      INTO TABLE t_e071_r1
      WHERE pgmid IN r_pgmid AND
            object EQ 'PROG' AND
           obj_name IN s_pgm .
    IF sy-subrc = 0.
      SORT t_e071_r1 BY trkorr obj_name.
      DELETE ADJACENT DUPLICATES FROM t_e071_r1 COMPARING trkorr obj_name.
    ENDIF.

*    08/06/2018
    Perform all_objects_check.

    IF t_curr_temp IS NOT INITIAL.
      SELECT trkorr
          pgmid
          object
          obj_name
     FROM e071
     INTO TABLE t_e071_r
     FOR ALL ENTRIES IN t_curr_temp
     WHERE pgmid IN r_pgmid AND
           object IN r_object AND
           obj_name EQ t_curr_temp-obj_name.
      IF sy-subrc = 0.

        SORT t_e071_r BY trkorr object obj_name.
        DELETE ADJACENT DUPLICATES FROM t_e071_r COMPARING trkorr object obj_name.

      ENDIF.
    ENDIF.
*    08/06/2018

    IF t_e071_r IS NOT INITIAL.
      PERFORM p_target_system.
    ENDIF.

    IF t_e071_r IS NOT INITIAL.
      "Get status of all the TR's
      SELECT trkorr
             trfunction
             trstatus
             tarsystem
             korrdev
             as4user
             as4date
             as4time
             strkorr
            FROM e070
        INTO TABLE t_e070_r
        FOR ALL ENTRIES IN t_e071_r
        WHERE trkorr EQ t_e071_r-trkorr.
*        AND tarsystem IN S_TRGT.
      IF sy-subrc = 0.
        SORT t_e070_r BY trkorr.
      ENDIF.
    ENDIF.

    IF t_re071_r IS NOT INITIAL.
      SELECT trkorr
           pgmid
           object
           obj_name
      FROM e071
      INTO TABLE t_e071tr_r
      FOR ALL ENTRIES IN t_re071_r
      WHERE trkorr EQ t_re071_r-trkorr."08/06/2018 was listing subsequent objects other than used in custom report
*      WHERE object EQ t_re071_r-object and "08/06/2018
*      obj_name EQ t_re071_r-obj_name. "08/06/2018
      IF sy-subrc = 0.
        "DELETE t_e071tr_r WHERE NOT obj_name CP 'Z*' .
*        DELETE t_e071tr_r WHERE NOT ( obj_name CP 'Z*' OR
*                                obj_name CP 'Y*' OR
*                                obj_name CP 'LZ*' OR
*                                obj_name CP 'LY*' OR
*                                obj_name CP 'M*' OR
*                                obj_name CP 'S*').
        DELETE t_e071tr_r WHERE NOT object IN r_field.
        SORT t_e071tr_r BY obj_name.
        DELETE ADJACENT DUPLICATES FROM t_e071tr_r COMPARING obj_name.
      ENDIF.

      "Get all TR's Description
      SELECT trkorr
             as4text
          FROM e07t
          INTO TABLE t_e07t_r
          FOR ALL ENTRIES IN t_re071_r
          WHERE trkorr EQ t_re071_r-trkorr.
      IF sy-subrc = 0.
        SORT t_e07t_r BY trkorr.
      ENDIF.
    ENDIF.


    "================================================================================="
    " To Read Transport Import Log to get the Target System Details
    "================================================================================="
    LOOP AT t_re070_r INTO wa_e070_r.

      ls_request-header-trkorr = wa_e070_r-trkorr.
      CALL FUNCTION 'TRINT_READ_REQUEST_HEADER'
        EXPORTING
          iv_read_e070 = 'X'
          iv_read_e07t = 'X'
        CHANGING
          cs_request   = ls_request-header
        EXCEPTIONS
          OTHERS       = 1.
      IF sy-subrc <> 0.
        ls_request-header-trkorr = wa_e070_r-trkorr.
      ENDIF.

      CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
        EXPORTING
          iv_trkorr   = wa_e070_r-trkorr
          is_settings = ls_settings            "iv_dir_type = iv_dir_type
        IMPORTING
          es_cofile   = ls_request-cofile
          ev_user     = lv_username
          ev_project  = ls_request-project.
      IF ls_request-header-as4user = space.
        ls_request-header-as4user = lv_username.
      ENDIF.
      ls_request-cofile_filled = 'X'.
      APPEND ls_request TO lt_requests.
      CLEAR wa_e070_r-trkorr.
    ENDLOOP.

    SORT lt_requests BY header-trkorr.


    "================================================================================="
    " Data Process
    "================================================================================="
    IF rd_obj IS NOT INITIAL.
      CLEAR wa_e071_r.
      LOOP AT t_e071tr_r INTO wa_e071_r.
        wa_final_r-trkorr = wa_e071_r-trkorr.
*      LOOP AT t_re071_r INTO wa_e071_r.
*        wa_final_r-trkorr = wa_e071_r-trkorr.

        READ TABLE t_e070_r INTO wa_e070_r WITH KEY trkorr = wa_e071_r-trkorr BINARY SEARCH.
*         READ TABLE t_re070_r INTO wa_e070_r WITH KEY trkorr = wa_e071_r-trkorr BINARY SEARCH.
        IF sy-subrc = 0.
          wa_final_r-trfunction = wa_e070_r-trfunction.
          wa_final_r-trstatus = wa_e070_r-trstatus.
          wa_final_r-korrdev = wa_e070_r-korrdev.
          wa_final_r-as4user = wa_e070_r-as4user.

          "Read Import Log Details----------------------------------------------------------------------"
          IF wa_e070_r-strkorr IS INITIAL.
            READ TABLE lt_requests ASSIGNING <fs_requests>  WITH KEY header-trkorr = wa_e070_r-trkorr BINARY SEARCH.
            IF sy-subrc = 0.
              SORT <fs_requests>-cofile-systems BY systemid DESCENDING.
            ENDIF.
          ELSEIF wa_e070_r-strkorr IS NOT INITIAL.   "higher level request exists
            CLEAR lv_strkorr.
            lv_strkorr = wa_e070_r-strkorr.
            CLEAR wa_e070_r.
            READ TABLE t_e070_r INTO wa_e070_r WITH KEY trkorr = lv_strkorr BINARY SEARCH.
*            READ TABLE t_re070_r INTO wa_e070_r WITH KEY trkorr = lv_strkorr BINARY SEARCH.
            IF sy-subrc = 0.
              READ TABLE lt_requests ASSIGNING <fs_requests>  WITH KEY header-trkorr = lv_strkorr BINARY SEARCH.
              IF sy-subrc = 0.
                SORT <fs_requests>-cofile-systems BY systemid DESCENDING.
              ENDIF.
            ENDIF.
          ENDIF.

          IF <fs_requests> IS ASSIGNED.
            READ TABLE <fs_requests>-cofile-systems ASSIGNING <fs_systems> WITH KEY systemid = s_trgt-low.        "INDEX 1.
            IF sy-subrc = 0.
              wa_final_r-tarsystem = <fs_systems>-systemid.

              SORT <fs_systems>-steps BY clientid DESCENDING.
              READ TABLE <fs_systems>-steps ASSIGNING <fs_steps> INDEX 1.
              IF sy-subrc = 0.

                READ TABLE <fs_steps>-actions ASSIGNING <fs_actions> INDEX 1.
                IF sy-subrc = 0.

                  wa_final_r-date = <fs_actions>-date.
                  wa_final_r-time = <fs_actions>-time.

                  IF <fs_systems>-rc = 0.
                    wa_final_r-icon = 3.
                    wa_final_r-status = 'Successfully Transported with RC 0'.
                  ELSEIF <fs_systems>-rc = 4.
                    wa_final_r-icon = 2.
                    wa_final_r-status = 'Successfully Transported with RC 4 (Warning)'.
                  ELSEIF <fs_systems>-rc = 8.
                    wa_final_r-icon = 1.
                    wa_final_r-status = 'Transported with RC 8 (Error)'.
                  ELSEIF <fs_systems>-rc = 12.
                    wa_final_r-icon = 1.
                    wa_final_r-status = 'Transported with RC 12 (Import Error)'.
                  ENDIF.
                ENDIF.
              ENDIF.
            ELSE.
              wa_final_r-tarsystem = wa_e070_r-tarsystem.
              wa_final_r-date = wa_e070_r-as4date.
              wa_final_r-time = wa_e070_r-as4time.

              IF wa_e070_r-trstatus EQ 'D' OR
               wa_e070_r-trstatus EQ 'L'.
                IF lv_strkorr IS NOT INITIAL.
                  wa_final_r-icon = 3.
                  wa_final_r-status = 'Transport Modifiable'.
                ELSE.
                  wa_final_r-icon = 3.
                  wa_final_r-status = 'Transport released'.
                ENDIF.
              ELSE.
                wa_final_r-icon = 3.
                wa_final_r-status = 'Transport released'.
              ENDIF.
            ENDIF.
          ELSE.
            IF wa_final_r-trstatus EQ 'D' OR
             wa_final_r-trstatus EQ 'L'.
              IF lv_strkorr IS NOT INITIAL.
                wa_final_r-icon = 2.
                wa_final_r-status = 'Transport Modifiable'.
              ELSE.
                wa_final_r-icon = 3.
                wa_final_r-status = 'Transport released'.
              ENDIF.
            ELSE.
              wa_final_r-icon = 3.
              wa_final_r-status = 'Transport released'.
            ENDIF.
          ENDIF.
        ENDIF.

        READ TABLE t_e07t_r INTO wa_e07t_r WITH KEY trkorr = wa_e071_r-trkorr BINARY SEARCH.
        IF sy-subrc = 0.
          wa_final_r-as4text = wa_e07t_r-as4text.
        ENDIF.
        wa_final_r-pgmid = wa_e071_r-pgmid.
        wa_final_r-object =  wa_e071_r-object.
        wa_final_r-obj_name = wa_e071_r-obj_name.
        APPEND wa_final_r TO t_final_r.
        CLEAR wa_final_r.
        CLEAR wa_e071_r.
      ENDLOOP.

    ELSEIF rd_trlis IS NOT INITIAL.
      CLEAR wa_e071_r.

      LOOP AT t_re071_r INTO wa_e071_r.
        wa_final_r-trkorr = wa_e071_r-trkorr.

        READ TABLE t_re070_r INTO wa_e070_r WITH KEY trkorr = wa_e071_r-trkorr BINARY SEARCH.
        IF sy-subrc = 0.
          wa_final_r-trfunction = wa_e070_r-trfunction.
          wa_final_r-trstatus = wa_e070_r-trstatus.
          wa_final_r-korrdev = wa_e070_r-korrdev.
          wa_final_r-as4user = wa_e070_r-as4user.
          "----------------------------------------------------------------------"
          IF wa_e070_r-strkorr IS INITIAL.
            READ TABLE lt_requests ASSIGNING <fs_requests>  WITH KEY header-trkorr = wa_e070_r-trkorr BINARY SEARCH.
            IF sy-subrc = 0.
              SORT <fs_requests>-cofile-systems BY systemid DESCENDING.
            ENDIF.
          ELSEIF wa_e070_r-strkorr IS NOT INITIAL.   "higher level request exists
            CLEAR lv_strkorr.
            lv_strkorr = wa_e070_r-strkorr.
            CLEAR wa_e070_r.
            READ TABLE t_re070_r INTO wa_e070_r WITH KEY trkorr = lv_strkorr BINARY SEARCH.
            IF sy-subrc = 0.
              READ TABLE lt_requests ASSIGNING <fs_requests>  WITH KEY header-trkorr = lv_strkorr BINARY SEARCH.
              IF sy-subrc = 0.
                SORT <fs_requests>-cofile-systems BY systemid DESCENDING.
              ENDIF.
            ENDIF.
          ENDIF.

          IF <fs_requests> IS ASSIGNED.
            READ TABLE <fs_requests>-cofile-systems ASSIGNING <fs_systems> WITH KEY systemid = s_trgt-low.           "INDEX 1.
            IF sy-subrc = 0.
              wa_final_r-tarsystem = <fs_systems>-systemid.

              SORT <fs_systems>-steps BY clientid DESCENDING.
              READ TABLE <fs_systems>-steps ASSIGNING <fs_steps> INDEX 1.
              IF sy-subrc = 0.

                READ TABLE <fs_steps>-actions ASSIGNING <fs_actions> INDEX 1.
                IF sy-subrc = 0.

                  wa_final_r-date = <fs_actions>-date.
                  wa_final_r-time = <fs_actions>-time.

                  IF <fs_systems>-rc = 0.
                    wa_final_r-icon = 3.
                    wa_final_r-status = 'Successfully Transported with RC 0'.
                  ELSEIF <fs_systems>-rc = 4.
                    wa_final_r-icon = 2.
                    wa_final_r-status = 'Successfully Transported with RC 4 (Warning)'.
                  ELSEIF <fs_systems>-rc = 8.
*                    wa_final_r-icon = 3.
                    wa_final_r-icon = 1.
                    wa_final_r-status = 'Transported with RC 8 (Error)'.
                  ELSEIF <fs_systems>-rc = 12.
*                    wa_final_r-icon = 3.
                    wa_final_r-icon = 1.
                    wa_final_r-status = 'Transported with RC 12 (Import Error)'.
                  ENDIF.
                ENDIF.
              ENDIF.
            ELSE.
              wa_final_r-tarsystem = wa_e070_r-tarsystem.
              wa_final_r-date = wa_e070_r-as4date.
              wa_final_r-time = wa_e070_r-as4time.

              IF wa_e070_r-trstatus EQ 'D' OR
               wa_e070_r-trstatus EQ 'L'.
                IF lv_strkorr IS NOT INITIAL.
                  wa_final_r-icon = 3.
                  wa_final_r-status = 'Transport Modifiable'.
                ELSE.
                  wa_final_r-icon = 3.
                  wa_final_r-status = 'Transport released'.
                ENDIF.
              ELSE.
                wa_final_r-icon = 3.
                wa_final_r-status = 'Transport released'.
              ENDIF.
            ENDIF.
          ELSE.
*            IF wa_e070_r-trstatus EQ 'D' OR
*             wa_e070_r-trstatus EQ 'L'.
            IF wa_final_r-trstatus EQ 'D' OR
            wa_final_r-trstatus EQ 'L'.

              IF lv_strkorr IS NOT INITIAL.
                wa_final_r-icon = 2.
                wa_final_r-status = 'Transport Modifiable'.
              ELSE.
                wa_final_r-icon = 3.
                wa_final_r-status = 'Transport released'.
              ENDIF.
            ELSE.
              wa_final_r-icon = 3.
              wa_final_r-status = 'Transport released'.
            ENDIF.
          ENDIF.
        ENDIF.

        READ TABLE t_e07t_r INTO wa_e07t_r WITH KEY trkorr = wa_e071_r-trkorr BINARY SEARCH.
        IF sy-subrc = 0.
          wa_final_r-as4text = wa_e07t_r-as4text.
        ENDIF.
        wa_final_r-pgmid = wa_e071_r-pgmid.
        wa_final_r-object =  wa_e071_r-object.
        wa_final_r-obj_name = wa_e071_r-obj_name.
        APPEND wa_final_r TO t_final_r.
        CLEAR wa_final_r.
      ENDLOOP.


    ENDIF.

    IF t_final_r IS NOT INITIAL.
      SORT t_final_r BY trkorr obj_name.
    ENDIF.

    "====================================================================================="
    " To include and Exclude Copy TR and Not released TR's
    "====================================================================================="
    "Include Copy TR's----------------------------------------------------------"
    IF ck_copy IS NOT INITIAL AND
      t_final_r IS NOT INITIAL.
      SORT t_final_r BY trkorr trfunction.
    ELSE.
      "Exlude Copy TR's
      DELETE t_final_r WHERE trfunction EQ 'T'.
      SORT t_final_r BY trkorr .
    ENDIF.

    "Include Not released TR's---------------------------------------------------"
    IF ck_rel IS NOT INITIAL AND
     t_final_r IS NOT INITIAL.
      SORT t_final_r BY trkorr trfunction.
    ELSE.
      "Exlude not released TR's
      DELETE t_final_r WHERE ( trstatus EQ 'D' OR trstatus EQ 'L').
      SORT t_final_r BY trkorr.
    ENDIF.

*    "Target System Check--------------------------------------------------------"
*    IF s_trgt IS NOT INITIAL.
*      DELETE t_final_r WHERE NOT tarsystem = s_trgt-low.
*    ENDIF.

    "Object Type range------------------------------------------------------------"
    IF r_field IS NOT INITIAL.
      DELETE t_final_r WHERE NOT object IN r_field.
    ENDIF.
*delete t_final_r where not  trfunction EQ 'K' OR trfunction EQ 'W'.
*  ENDIF.
  ELSEIF s_pgm IS NOT INITIAL AND s_trgt IS INITIAL.
    "Get all the TR's used By the object
    SELECT trkorr
           pgmid
           object
           obj_name
      FROM e071
      INTO TABLE t_e071_r1
      WHERE pgmid IN r_pgmid AND
            "object EQ 'PROG' AND
           obj_name IN s_pgm .
    IF sy-subrc = 0.
      SORT t_e071_r1 BY trkorr obj_name.
      DELETE ADJACENT DUPLICATES FROM t_e071_r1 COMPARING trkorr obj_name.
    ENDIF.

*    08/06/2018
    Perform all_objects_check.

    IF t_curr_temp IS NOT INITIAL.
      SELECT trkorr
          pgmid
          object
          obj_name
     FROM e071
     INTO TABLE t_e071_r
     FOR ALL ENTRIES IN t_curr_temp
     WHERE pgmid IN r_pgmid AND
           object IN r_object AND
           obj_name EQ t_curr_temp-obj_name.
      IF sy-subrc = 0.

        SORT t_e071_r BY trkorr object obj_name.
        DELETE ADJACENT DUPLICATES FROM t_e071_r COMPARING trkorr object obj_name.

      ENDIF.
    ENDIF.
*    08/06/2018

    IF t_e071_r IS NOT INITIAL.
      "Get status of all the TR's
      SELECT trkorr
             trfunction
             trstatus
             tarsystem
             korrdev
             as4user
             as4date
             as4time
             strkorr
            FROM e070
        INTO TABLE t_e070_r
        FOR ALL ENTRIES IN t_e071_r
        WHERE trkorr EQ t_e071_r-trkorr.
*        AND tarsystem IN S_TRGT.
      IF sy-subrc = 0.
        SORT t_e070_r BY trkorr.
      ENDIF.
    ENDIF.

    IF t_e071_r IS NOT INITIAL.
      SELECT trkorr
           pgmid
           object
           obj_name
      FROM e071
      INTO TABLE t_e071tr_r
      FOR ALL ENTRIES IN t_e071_r
*      WHERE trkorr EQ t_e071_r-trkorr."08/06/2018 was listing subsequent objects other than used in custom report
      WHERE pgmid EQ t_e071_r-pgmid and
        object EQ t_e071_r-object and "08/06/2018
      obj_name EQ t_e071_r-obj_name. "08/06/2018
      IF sy-subrc = 0.
        "DELETE t_e071tr_r WHERE NOT obj_name CP 'Z*' .
*        DELETE t_e071tr_r WHERE NOT ( obj_name CP 'Z*' OR
*                                obj_name CP 'Y*' OR
*                                obj_name CP 'LZ*' OR
*                                obj_name CP 'LY*' OR
*                                obj_name CP 'M*' OR
*                                obj_name CP 'S*').
        DELETE t_e071tr_r WHERE NOT object IN r_field.
        SORT t_e071tr_r BY obj_name.
        DELETE ADJACENT DUPLICATES FROM t_e071tr_r COMPARING obj_name.
      ENDIF.

      "Get all TR's Description
      SELECT trkorr
             as4text
          FROM e07t
          INTO TABLE t_e07t_r
          FOR ALL ENTRIES IN t_e071_r
          WHERE trkorr EQ t_e071_r-trkorr.
      IF sy-subrc = 0.
        SORT t_e07t_r BY trkorr.
      ENDIF.
    ENDIF.


    "================================================================================="
    " To Read Transport Import Log to get the Target System Details
    "================================================================================="
    LOOP AT t_e070_r INTO wa_e070_r.

      ls_request-header-trkorr = wa_e070_r-trkorr.
      CALL FUNCTION 'TRINT_READ_REQUEST_HEADER'
        EXPORTING
          iv_read_e070 = 'X'
          iv_read_e07t = 'X'
        CHANGING
          cs_request   = ls_request-header
        EXCEPTIONS
          OTHERS       = 1.
      IF sy-subrc <> 0.
        ls_request-header-trkorr = wa_e070_r-trkorr.
      ENDIF.

      CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
        EXPORTING
          iv_trkorr   = wa_e070_r-trkorr
          is_settings = ls_settings            "iv_dir_type = iv_dir_type
        IMPORTING
          es_cofile   = ls_request-cofile
          ev_user     = lv_username
          ev_project  = ls_request-project.
      IF ls_request-header-as4user = space.
        ls_request-header-as4user = lv_username.
      ENDIF.
      ls_request-cofile_filled = 'X'.
      APPEND ls_request TO lt_requests.
      CLEAR wa_e070_r-trkorr.
    ENDLOOP.

    SORT lt_requests BY header-trkorr.




    "================================================================================="
    " Data Process
    "================================================================================="
    IF rd_obj IS NOT INITIAL.
      CLEAR wa_e071_r.
      LOOP AT t_e071tr_r INTO wa_e071_r.
        wa_final_r-trkorr = wa_e071_r-trkorr.

        READ TABLE t_e070_r INTO wa_e070_r WITH KEY trkorr = wa_e071_r-trkorr BINARY SEARCH.
        IF sy-subrc = 0.
          wa_final_r-trfunction = wa_e070_r-trfunction.
          wa_final_r-trstatus = wa_e070_r-trstatus.
          wa_final_r-korrdev = wa_e070_r-korrdev.
          wa_final_r-as4user = wa_e070_r-as4user.

          "Read Import Log Details----------------------------------------------------------------------"
          IF wa_e070_r-strkorr IS INITIAL.
            READ TABLE lt_requests ASSIGNING <fs_requests>  WITH KEY header-trkorr = wa_e070_r-trkorr BINARY SEARCH.
            IF sy-subrc = 0.
              SORT <fs_requests>-cofile-systems BY systemid DESCENDING.
            ENDIF.
          ELSEIF wa_e070_r-strkorr IS NOT INITIAL.   "higher level request exists
            CLEAR lv_strkorr.
            lv_strkorr = wa_e070_r-strkorr.
            CLEAR wa_e070_r.
            READ TABLE t_e070_r INTO wa_e070_r WITH KEY trkorr = lv_strkorr BINARY SEARCH.
            IF sy-subrc = 0.
              READ TABLE lt_requests ASSIGNING <fs_requests>  WITH KEY header-trkorr = lv_strkorr BINARY SEARCH.
              IF sy-subrc = 0.
                SORT <fs_requests>-cofile-systems BY systemid DESCENDING.
              ENDIF.
            ENDIF.
          ENDIF.

          IF <fs_requests> IS ASSIGNED.
            READ TABLE <fs_requests>-cofile-systems ASSIGNING <fs_systems> INDEX 1.
            IF sy-subrc = 0.
              wa_final_r-tarsystem = <fs_systems>-systemid.

              SORT <fs_systems>-steps BY clientid DESCENDING.
              READ TABLE <fs_systems>-steps ASSIGNING <fs_steps> INDEX 1.
              IF sy-subrc = 0.

                READ TABLE <fs_steps>-actions ASSIGNING <fs_actions> INDEX 1.
                IF sy-subrc = 0.

                  wa_final_r-date = <fs_actions>-date.
                  wa_final_r-time = <fs_actions>-time.

                  IF <fs_systems>-rc = 0.
                    wa_final_r-icon = 3.
                    wa_final_r-status = 'Successfully Transported with RC 0'.
                  ELSEIF <fs_systems>-rc = 4.
                    wa_final_r-icon = 2.
                    wa_final_r-status = 'Successfully Transported with RC 4 (Warning)'.
                  ELSEIF <fs_systems>-rc = 8.
                    wa_final_r-icon = 1.
                    wa_final_r-status = 'Transported with RC 8 (Error)'.
                  ELSEIF <fs_systems>-rc = 12.
                    wa_final_r-icon = 1.
                    wa_final_r-status = 'Transported with RC 12 (Import Error)'.
                  ENDIF.
                ENDIF.
              ENDIF.
            ELSE.
              wa_final_r-tarsystem = wa_e070_r-tarsystem.
              wa_final_r-date = wa_e070_r-as4date.
              wa_final_r-time = wa_e070_r-as4time.

              IF wa_e070_r-trstatus EQ 'D' OR
               wa_e070_r-trstatus EQ 'L'.
                IF lv_strkorr IS NOT INITIAL.
                  wa_final_r-icon = 3.
                  wa_final_r-status = 'Tranport Modifiable'.
                ELSE.
                  wa_final_r-icon = 3.
                  wa_final_r-status = 'Tranport released'.
                ENDIF.
              ELSE.
                wa_final_r-icon = 3.
                wa_final_r-status = 'Tranport released'.
              ENDIF.
            ENDIF.
          ELSE.
            IF wa_final_r-trstatus EQ 'D' OR
             wa_final_r-trstatus EQ 'L'.
              IF lv_strkorr IS NOT INITIAL.
                wa_final_r-icon = 2.
                wa_final_r-status = 'Tranport Modifiable'.
              ELSE.
                wa_final_r-icon = 3.
                wa_final_r-status = 'Tranport released'.
              ENDIF.
            ELSE.
              wa_final_r-icon = 3.
              wa_final_r-status = 'Tranport released'.
            ENDIF.
          ENDIF.
        ENDIF.

        READ TABLE t_e07t_r INTO wa_e07t_r WITH KEY trkorr = wa_e071_r-trkorr BINARY SEARCH.
        IF sy-subrc = 0.
          wa_final_r-as4text = wa_e07t_r-as4text.
        ENDIF.
        wa_final_r-pgmid = wa_e071_r-pgmid.
        wa_final_r-object =  wa_e071_r-object.
        wa_final_r-obj_name = wa_e071_r-obj_name.
        APPEND wa_final_r TO t_final_r.
        CLEAR wa_final_r.
        CLEAR wa_e071_r.
      ENDLOOP.

    ELSEIF rd_trlis IS NOT INITIAL.
      CLEAR wa_e071_r.

      LOOP AT t_e071_r INTO wa_e071_r.
        wa_final_r-trkorr = wa_e071_r-trkorr.

        READ TABLE t_e070_r INTO wa_e070_r WITH KEY trkorr = wa_e071_r-trkorr BINARY SEARCH.
        IF sy-subrc = 0.
          wa_final_r-trfunction = wa_e070_r-trfunction.
          wa_final_r-trstatus = wa_e070_r-trstatus.
          wa_final_r-korrdev = wa_e070_r-korrdev.
          wa_final_r-as4user = wa_e070_r-as4user.
          "----------------------------------------------------------------------"
          IF wa_e070_r-strkorr IS INITIAL.
            READ TABLE lt_requests ASSIGNING <fs_requests>  WITH KEY header-trkorr = wa_e070_r-trkorr BINARY SEARCH.
            IF sy-subrc = 0.
              SORT <fs_requests>-cofile-systems BY systemid DESCENDING.
            ENDIF.
          ELSEIF wa_e070_r-strkorr IS NOT INITIAL.   "higher level request exists
            CLEAR lv_strkorr.
            lv_strkorr = wa_e070_r-strkorr.
            CLEAR wa_e070_r.
            READ TABLE t_e070_r INTO wa_e070_r WITH KEY trkorr = lv_strkorr BINARY SEARCH.
            IF sy-subrc = 0.
              READ TABLE lt_requests ASSIGNING <fs_requests>  WITH KEY header-trkorr = lv_strkorr BINARY SEARCH.
              IF sy-subrc = 0.
                SORT <fs_requests>-cofile-systems BY systemid DESCENDING.
              ENDIF.
            ENDIF.
          ENDIF.

          IF <fs_requests> IS ASSIGNED.
            READ TABLE <fs_requests>-cofile-systems ASSIGNING <fs_systems> INDEX 1.
            IF sy-subrc = 0.
              wa_final_r-tarsystem = <fs_systems>-systemid.

              SORT <fs_systems>-steps BY clientid DESCENDING.
              READ TABLE <fs_systems>-steps ASSIGNING <fs_steps> INDEX 1.
              IF sy-subrc = 0.

                READ TABLE <fs_steps>-actions ASSIGNING <fs_actions> INDEX 1.
                IF sy-subrc = 0.

                  wa_final_r-date = <fs_actions>-date.
                  wa_final_r-time = <fs_actions>-time.

                  IF <fs_systems>-rc = 0.
                    wa_final_r-icon = 3.
                    wa_final_r-status = 'Successfully Transported with RC 0'.
                  ELSEIF <fs_systems>-rc = 4.
                    wa_final_r-icon = 2.
                    wa_final_r-status = 'Successfully Transported with RC 4 (Warning)'.
                  ELSEIF <fs_systems>-rc = 8.
                    wa_final_r-icon = 3.
                    wa_final_r-status = 'Transported with RC 8 (Error)'.
                  ELSEIF <fs_systems>-rc = 12.
                    wa_final_r-icon = 3.
                    wa_final_r-status = 'Transported with RC 12 (Import Error)'.
                  ENDIF.
                ENDIF.
              ENDIF.
            ELSE.
              wa_final_r-tarsystem = wa_e070_r-tarsystem.
              wa_final_r-date = wa_e070_r-as4date.
              wa_final_r-time = wa_e070_r-as4time.

              IF wa_e070_r-trstatus EQ 'D' OR
               wa_e070_r-trstatus EQ 'L'.
                IF lv_strkorr IS NOT INITIAL.
                  wa_final_r-icon = 3.
                  wa_final_r-status = 'Tranport Modifiable'.
                ELSE.
                  wa_final_r-icon = 3.
                  wa_final_r-status = 'Tranport released'.
                ENDIF.
              ELSE.
                wa_final_r-icon = 3.
                wa_final_r-status = 'Tranport released'.
              ENDIF.
            ENDIF.
          ELSE.
*            IF wa_e070_r-trstatus EQ 'D' OR
*             wa_e070_r-trstatus EQ 'L'.
            IF wa_final_r-trstatus EQ 'D' OR
            wa_final_r-trstatus EQ 'L'.

              IF lv_strkorr IS NOT INITIAL.
                wa_final_r-icon = 2.
                wa_final_r-status = 'Tranport Modifiable'.
              ELSE.
                wa_final_r-icon = 3.
                wa_final_r-status = 'Tranport released'.
              ENDIF.
            ELSE.
              wa_final_r-icon = 3.
              wa_final_r-status = 'Tranport released'.
            ENDIF.
          ENDIF.
        ENDIF.

        READ TABLE t_e07t_r INTO wa_e07t_r WITH KEY trkorr = wa_e071_r-trkorr BINARY SEARCH.
        IF sy-subrc = 0.
          wa_final_r-as4text = wa_e07t_r-as4text.
        ENDIF.
        wa_final_r-pgmid = wa_e071_r-pgmid.
        wa_final_r-object =  wa_e071_r-object.
        wa_final_r-obj_name = wa_e071_r-obj_name.
        APPEND wa_final_r TO t_final_r.
        CLEAR wa_final_r.
      ENDLOOP.


    ENDIF.

    IF t_final_r IS NOT INITIAL.
      SORT t_final_r BY trkorr obj_name.
    ENDIF.

    "====================================================================================="
    " To include and Exclude Copy TR and Not released TR's
    "====================================================================================="
    "Include Copy TR's----------------------------------------------------------"
    IF ck_copy IS NOT INITIAL AND
      t_final_r IS NOT INITIAL.
      SORT t_final_r BY trkorr trfunction.
    ELSE.
      "Exlude Copy TR's
      DELETE t_final_r WHERE trfunction EQ 'T'.
      SORT t_final_r BY trkorr .
    ENDIF.

    "Include Not released TR's---------------------------------------------------"
    IF ck_rel IS NOT INITIAL AND
     t_final_r IS NOT INITIAL.
      SORT t_final_r BY trkorr trfunction.
    ELSE.
      "Exlude not released TR's
      DELETE t_final_r WHERE ( trstatus EQ 'D' OR trstatus EQ 'L').
      SORT t_final_r BY trkorr.
    ENDIF.

    "Target System Check--------------------------------------------------------"
    IF s_trgt IS NOT INITIAL.
      DELETE t_final_r WHERE NOT tarsystem = s_trgt-low.
    ENDIF.

    "Object Type range------------------------------------------------------------"
    IF r_field IS NOT INITIAL.
      DELETE t_final_r WHERE NOT object IN r_field.
    ENDIF.
*delete t_final_r where not  trfunction EQ 'K' OR trfunction EQ 'W'.
  ENDIF.

*  IF s_pgm IS NOT INITIAL.
*    IF S_TRGT IS NOT INITIAL.
*      "--------------------------Local Data Declarations--------------------"
*  DATA: lv_obj_name TYPE e071-obj_name,
*        lv_obj_name1 TYPE e071-obj_name.
*
*  REFRESH: t_remote1, t_remote,t_re071,t_re070,t_out_tab,t_conflict.
*
*  "======================================================================="
*  "-------------------Get Target System Details---------------------------"
*  "======================================================================="
*
**  PERFORM objects_request.
*
*  IF t_e071 IS NOT INITIAL.
*    CLEAR: t_lines, wa_e071, t_options, t_out_tab .
*    REFRESH: t_options, t_fields, t_out_tab.
** Get lines of internal table
*    DESCRIBE TABLE t_e071 LINES t_lines.
**Populating first line for selection criteria for fetching from target system
*    t_text = '(' .
*    APPEND t_text TO t_options.
*
*    LOOP AT t_e071 INTO wa_e071.
*      SPLIT wa_e071-obj_name AT space INTO lv_obj_name lv_obj_name1.
*      CLEAR wa_e071-obj_name.
*      wa_e071-obj_name = lv_obj_name.
*      IF sy-tabix NE t_lines.
**Populating other line for selection criteria for fetching from target system
*        CONCATENATE 'OBJ_NAME = ''' wa_e071-obj_name ''' OR' INTO t_text.
*      ELSE.
**Populating last line for selection criteria for fetching from target system
*        CONCATENATE 'OBJ_NAME = ''' wa_e071-obj_name ''')' INTO t_text.
*      ENDIF.
*      APPEND t_text TO t_options.
*      CLEAR t_text.
*    ENDLOOP.
*
*    "-------------------------------------------------------------------------------"
*    "--------------RFC Call to read E071 table from Target System-------------------"
*    "-------------------------------------------------------------------------------"
*    DATA: lwa_rfcdest TYPE rfcdes-rfcdest.
*    "To check RFC Destination exists or not before read
*    SELECT SINGLE rfcdest
*           FROM rfcdes
*           INTO lwa_rfcdest
*           WHERE rfcdest = p_system.
*    IF lwa_rfcdest IS NOT INITIAL. "RFC Destination exists or not
***calling RFC FM and fetching table Object Entries of Requests/Tasks (E071)
*      CALL FUNCTION 'RFC_READ_TABLE' DESTINATION p_system
*        EXPORTING
*          query_table          = 'E071'
*        TABLES
*          OPTIONS              = t_options
*          fields               = t_fields
*          data                 = t_out_tab
*        EXCEPTIONS
*          table_not_available  = 1
*          table_without_data   = 2
*          option_not_valid     = 3
*          field_not_valid      = 4
*          not_authorized       = 5
*          data_buffer_exceeded = 6
*          OTHERS               = 7.
*      IF sy-subrc <> 0.
*        MESSAGE s016(ztr) DISPLAY LIKE 'E'.
*        LEAVE LIST-PROCESSING.
*      ENDIF.
*    ELSE.
*      MESSAGE s017(ztr) DISPLAY LIKE 'E'.
*      LEAVE LIST-PROCESSING.
*    ENDIF.
*    "-----------Formatting the output table received from the RFC FM--------"
*    LOOP AT t_out_tab.
*      wa_e071-trkorr   = t_out_tab+0(20).
*      "wa_e071-as4pos   = i_out_tab+20(06).
*      wa_e071-pgmid    = t_out_tab+26(04).
*      wa_e071-object   = t_out_tab+30(04).
*      wa_e071-obj_name = t_out_tab+34(120).
*      APPEND wa_e071 TO t_re071.
*      CLEAR wa_e071 .
*    ENDLOOP.
*  ENDIF.
*  SORT t_re071 BY trkorr.
*
*
*  IF t_re071 IS NOT INITIAL.
*    CLEAR: t_lines, wa_e071, t_options.
** Get lines of internal table
*    DESCRIBE TABLE t_re071 LINES t_lines.
**Populating first line for selection criteria for fetching from target system
*    t_text = '(' .
*    APPEND t_text TO t_options1.
*
*    LOOP AT t_re071 INTO wa_e071.
*      IF sy-tabix NE t_lines.
**Populating other line for selection criteria for fetching from target system
*        CONCATENATE 'TRKORR = ''' wa_e071-trkorr ''' OR' INTO t_text. "Other lines
*      ELSE.
**Populating last line for selection criteria for fetching from target system
*        CONCATENATE 'TRKORR = ''' wa_e071-trkorr ''')' INTO t_text. "Last line
*      ENDIF.
*      APPEND t_text TO t_options1.
*      CLEAR t_text.
*    ENDLOOP.
*
*    "-------------------------------------------------------------------------------"
*    "--------------RFC Call to read E070 table from Target System-------------------"
*    "-------------------------------------------------------------------------------"
*    IF lwa_rfcdest IS NOT INITIAL.    "RFC Destination exists or not
** Calling RFC FM and fetching table Header of Requests/Tasks (E070)
*      CALL FUNCTION 'RFC_READ_TABLE' DESTINATION p_system
*        EXPORTING
*          query_table          = 'E070'
*        TABLES
*          OPTIONS              = t_options1
*          fields               = t_fields1
*          data                 = t_out_tab1
*        EXCEPTIONS
*          table_not_available  = 1
*          table_without_data   = 2
*          option_not_valid     = 3
*          field_not_valid      = 4
*          not_authorized       = 5
*          data_buffer_exceeded = 6
*          OTHERS               = 7.
*      IF sy-subrc <> 0.
*        MESSAGE s016(ztr) DISPLAY LIKE 'E'.
*        LEAVE LIST-PROCESSING.
*      ENDIF.
*    ELSE.
*      MESSAGE s017(ztr) DISPLAY LIKE 'E'.
*      LEAVE LIST-PROCESSING.
*    ENDIF.
** Formatting the output table received from the RFC FM
*    LOOP AT t_out_tab1.
*      wa_e070-trkorr     = t_out_tab1+0(20).
*      wa_e070-trstatus   = t_out_tab1+21(01).
*      wa_e070-as4user    = t_out_tab1+36(12).
*      wa_e070-as4date    = t_out_tab1+48(08).
*      wa_e070-as4time    = t_out_tab1+56(06).
*      APPEND wa_e070 TO t_re070.
*      CLEAR wa_e070 .
*    ENDLOOP.
*  ENDIF.
*
*  "-----------------------------------------------------------------------------------"
*  "------------------Build Final Target System Object table---------------------------"
*  "-----------------------------------------------------------------------------------"
** loop at the internal table containing all objects with all TR's
*  SORT t_e070 BY trkorr.
*  LOOP AT t_re071 INTO wa_e071.
** Get the corresponding date and time for the corresponding Object and TR no.
*    READ TABLE t_e070 INTO wa_e070 WITH KEY trkorr = wa_e071-trkorr BINARY SEARCH.
*    IF sy-subrc = 0.
** Move all entries to get all the objects with their TR no, Date and time
** in the correct format fetched from target system
*      MOVE: wa_e071-trkorr   TO wa_remote-trkorr,
*            "wa_e071-as4pos   TO wa_remote-as4pos,
*            wa_e071-pgmid    TO wa_remote-pgmid,
*            wa_e071-object   TO wa_remote-object,
*            wa_e071-obj_name TO wa_remote-obj_name,
*            wa_e070-trstatus TO wa_remote-trstatus,
*            wa_e070-as4user  TO wa_remote-as4user,
*            wa_e070-as4date  TO wa_remote-as4date,
*            wa_e070-as4time  TO wa_remote-as4time.
*
*      APPEND wa_remote TO t_remote.
*      CLEAR  wa_remote.
*    ENDIF.
*
*  ENDLOOP.
*
*  "Copy to remote1 table---------------------"
*  SORT t_remote BY obj_name as4date DESCENDING as4time DESCENDING.
*  t_remote1[] = t_remote[].
*
*  SORT t_remote1 BY trkorr object obj_name.
*  DELETE ADJACENT DUPLICATES FROM t_remote1 COMPARING trkorr object obj_name.
*
*  SORT t_present BY trkorr object obj_name.
**   SORT t_present BY obj_name DESCENDING.
*  DELETE ADJACENT DUPLICATES FROM t_present COMPARING trkorr object obj_name.
*  "=========================================================================="
*  "--------------------------To Biuld Conflict Table-------------------------"
*  "=========================================================================="
*  "loop at all target system entries
**  LOOP AT t_remote1 INTO wa_remote1.
*LOOP at t_present INTO wa_present.
*    "check if all those TR#s in Target system exist in the present system
**    READ TABLE t_present INTO wa_present WITH KEY trkorr = wa_remote1-trkorr
**                                                  object = wa_remote1-object
**                                                obj_name = wa_remote1-obj_name BINARY SEARCH.
*  READ TABLE t_remote1 INTO wa_remote1 with KEY trkorr = wa_present-trkorr
*                                                object = wa_present-object
*                                                obj_name = wa_present-obj_name BINARY SEARCH.
*    IF sy-subrc <> 0.
**      "Check if the TR made as never transport
**      READ TABLE i_trnever INTO wa_trnever WITH KEY trkorr = wa_present-trkorr.
**      if sy-subrc <> 0.
*
*      "if the TR#s doesn#t exist, move the detail of TR#s
**       if wa_remote1-trkorr is not INITIAL.
**      wa_conflict-trkorr = wa_remote1-trkorr.
**      wa_conflict-trstatus = wa_remote1-trstatus.
**      wa_conflict-as4user = wa_remote1-as4user.
**      wa_conflict-as4date = wa_remote1-as4date.
**      wa_conflict-as4time = wa_remote1-as4time.
**      wa_conflict-pgmid  = wa_remote1-pgmid.
**      wa_conflict-object = wa_remote1-object.
**      wa_conflict-obj_name = wa_remote1-obj_name.
*
*      wa_conflict-trkorr = wa_present-trkorr.
*      wa_conflict-trstatus = wa_present-trstatus.
*      wa_conflict-as4user = wa_present-as4user.
*      wa_conflict-as4date = wa_present-as4date.
*      wa_conflict-as4time = wa_present-as4time.
*      wa_conflict-pgmid  = wa_present-pgmid.
*      wa_conflict-object = wa_present-object.
*      wa_conflict-obj_name = wa_present-obj_name.
*      wa_conflict-comments = text-h33. "  'NOK...Conflict exists'.
*      wa_conflict-icon = '1'.
*      APPEND wa_conflict TO t_conflict.
*      CLEAR : wa_remote1 , wa_present.
*      CLEAR wa_conflict.
**      endif.
*    ELSE.
*      "move the object with detail with no conflict
*      wa_conflict-trkorr = wa_remote1-trkorr.
*      wa_conflict-trstatus = wa_remote1-trstatus.
*      wa_conflict-as4user = wa_remote1-as4user.
*      wa_conflict-as4date = wa_remote1-as4date.
*      wa_conflict-as4time = wa_remote1-as4time.
*      wa_conflict-pgmid  = wa_remote1-pgmid.
*      wa_conflict-object = wa_remote1-object.
*      wa_conflict-obj_name = wa_remote1-obj_name.
*
*      wa_conflict-comments = text-h34.  "'OK...Conflict does not exists'.
*      wa_conflict-icon = '3'.
*      APPEND wa_conflict TO t_conflict.
*      CLEAR : wa_present , wa_remote1.
*      CLEAR wa_conflict.
*    ENDIF.
*  ENDLOOP.
*
*  REFRESH : t_re071,t_re070,t_out_tab.
*
**ENDFORM.                    " TARGET_OBJECTS
*      ENDIF.
*      ENDIF.

ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  PROGRAM_F4HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM program_f4help .

  TYPES: BEGIN OF s_obj_name,
           obj_name TYPE e071-obj_name,
         END OF s_obj_name.

  DATA:   li_object TYPE STANDARD TABLE OF s_obj_name.

  " Select data for F4 help----------------------------------------------"

  SELECT obj_name FROM e071
      INTO TABLE li_object
      WHERE pgmid = 'R3TR'.
  IF sy-subrc EQ 0.
    SORT li_object BY obj_name.
  ENDIF.
  "Function module get F4 help for a field--------------------------------"

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'OBJ_NAME'
      dynpprog        = sy-repid    " Program name
      dynpnr          = sy-dynnr    " Screen number
      dynprofield     = 'S_PGM'   " F4 help need field
      value_org       = 'S'
    TABLES
      value_tab       = li_object    " F4 help values
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDFORM.                    " PROGRAM_F4HELP
*&---------------------------------------------------------------------*
*&      Form  MANDATORY_VALIDATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mandatory_validation .

*  IF rd_proj IS NOT INITIAL.      "----------Project ID obligatory
*    IF s_extid IS INITIAL.
*      MESSAGE: s011(ztr) DISPLAY LIKE 'E'.
*      LEAVE LIST-PROCESSING.
*    ENDIF.
*  ELSE
  IF rd_reprt IS NOT INITIAL. "----------Report Obligatory
    IF s_pgm IS INITIAL.
      MESSAGE: s012(ztr) DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ELSEIF rd_cnflc IS NOT INITIAL."---------Target system Obligatory
    IF p_system IS INITIAL.
      MESSAGE: s013(ztr) DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
    IF rd_req IS NOT INITIAL. "-------------transport
      IF s_req IS INITIAL.
        MESSAGE: s015(ztr) DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ELSEIF rd_objtp IS NOT INITIAL."------------Object Type
      IF rd_rep IS NOT INITIAL AND
         s_report IS INITIAL."---------------report
        MESSAGE: s014(ztr) DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
*      ELSEIF rd_smrt IS NOT INITIAL AND
*             s_smrtfm IS INITIAL."-------------smart form
*        MESSAGE: s014(ztr) DISPLAY LIKE 'E'.
*        LEAVE LIST-PROCESSING.
      ELSEIF rd_tbl IS NOT INITIAL AND
            s_table IS INITIAL."-----------------table
        MESSAGE: s014(ztr) DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ELSEIF rd_dmn IS NOT INITIAL AND
             s_domain IS INITIAL."------------domain
        MESSAGE: s014(ztr) DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ELSEIF rd_fugp IS NOT INITIAL AND
             s_fugp IS INITIAL."--------------function group
        MESSAGE: s014(ztr) DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ELSEIF rd_func IS NOT INITIAL AND
             s_func IS INITIAL."--------------function module
        MESSAGE: s014(ztr) DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " MANDATORY_VALIDATION



"======================================================================="
"------------------Conflict check Forms---------------------------------"
"======================================================================="


*&---------------------------------------------------------------------*
*&      Form  VALIDATE_REQUEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_request .

  DATA: lwa_trkorr TYPE e071-trkorr.

  SELECT SINGLE trkorr
    FROM e071
    INTO lwa_trkorr "UP TO 1 ROWS
    WHERE trkorr IN s_req.
  IF sy-subrc NE 0.
    MESSAGE e018(ztr).
  ENDIF.

ENDFORM.                    " VALIDATE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_report .

  DATA: lwa_prog TYPE e071-obj_name.

  SELECT SINGLE obj_name
    FROM e071
    INTO lwa_prog "UP TO 1 ROWS
    WHERE pgmid IN r_pgmid AND
         obj_name IN s_report.
  IF sy-subrc <> 0.
    MESSAGE e004(ztr).
  ENDIF.

ENDFORM.                    " VALIDATE_REPORT
*&---------------------------------------------------------------------*
*&      Form  OBJECTS_REQUEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM objects_request .


  "--------------To get all the object related object type--------------"

  PERFORM check_obj_type TABLES r_check.


  IF rd_req IS NOT INITIAL."--------------------------------------TR
    REFRESH :t_e071, t_e071_1,t_e071_2,t_e070,
             t_e07t,t_present,t_final.

    "Objcets list used in a TR---------------------------"
    SELECT trkorr
          pgmid
          object
          obj_name
     FROM e071
     INTO TABLE t_e071
     WHERE trkorr IN s_req.
    IF sy-subrc = 0.
      SORT t_e071 BY trkorr object obj_name.
      DELETE ADJACENT DUPLICATES FROM t_e071 COMPARING trkorr object obj_name.
      DELETE  t_e071 WHERE pgmid  EQ 'CORR' AND object EQ 'RELE'.
*      DELETE t_e071 WHERE NOT ( obj_name CP 'Z*' OR
*                                obj_name CP 'Y*' OR
*                                obj_name CP 'LZ*' OR
*                                obj_name CP 'LY*' OR
*                                obj_name CP 'M*' OR
*                                obj_name CP 'S*').
    ENDIF.


    "All the TR's of individual objects-------------------"
    IF t_e071 IS NOT INITIAL.

      SELECT trkorr
          pgmid
          object
          obj_name
     FROM e071
     INTO TABLE t_e071_1
     FOR ALL ENTRIES IN t_e071
     WHERE pgmid IN r_pgmid AND
          object IN r_object AND
         obj_name EQ t_e071-obj_name.
      IF sy-subrc = 0.
        SORT t_e071_1 BY trkorr object obj_name.
        DELETE ADJACENT DUPLICATES FROM t_e071_1 COMPARING trkorr object obj_name.
      ENDIF.
    ENDIF.

  ELSEIF rd_objtp IS NOT INITIAL. "----------------------------Object Type

    IF s_report IS INITIAL.            "Report
      IF s_table IS NOT INITIAL.       "Table
        APPEND s_table TO s_report.
      ELSEIF s_domain IS NOT INITIAL.  "Domain
        APPEND s_domain TO s_report.
        s_report = s_domain.
      ELSEIF s_fugp IS NOT INITIAL.    "Function Group
        APPEND s_fugp TO s_report.
      ELSEIF s_func IS NOT INITIAL.    "Function Module
        APPEND s_func TO s_report.
      ENDIF.
    ENDIF.

    "TR list used by the object------------------------"
    IF r_check IS NOT INITIAL.        "Object Type
      SELECT trkorr
             pgmid
             object
             obj_name
        FROM e071
        INTO TABLE t_e071
        WHERE pgmid IN r_pgmid AND
             object IN r_check AND
            obj_name IN s_report.
      IF sy-subrc = 0.
        SORT t_e071 BY trkorr object obj_name.
        DELETE ADJACENT DUPLICATES FROM t_e071 COMPARING trkorr object obj_name.
      ENDIF.
    ENDIF.

    "Objcets list used in a object------------------------"
    IF t_e071 IS NOT INITIAL.
      SELECT trkorr
          pgmid
          object
          obj_name
     FROM e071
     INTO TABLE t_e071_1
     FOR ALL ENTRIES IN t_e071
     WHERE trkorr EQ t_e071-trkorr.
      IF sy-subrc = 0.
        SORT t_e071_1 BY trkorr object obj_name.
        DELETE ADJACENT DUPLICATES FROM t_e071_1 COMPARING trkorr object obj_name.
        "DELETE  t_e071_1 WHERE pgmid  EQ 'CORR' AND object EQ 'RELE'.
        "DELETE t_e071_1 WHERE object EQ 'TABU'.

*        DELETE t_e071_1 WHERE NOT ( obj_name CP 'Z*' OR
*                                obj_name CP 'Y*' OR
*                                obj_name CP 'LZ*' OR
*                                obj_name CP 'LY*' OR
*                                obj_name CP 'M*' OR
*                                obj_name CP 'S*').
        "SORT t_e071_1 BY pgmid object obj_name.
      ENDIF.
    ENDIF.

    "All the TR's of individual objects---------------------"
    IF t_e071_1 IS NOT INITIAL.
      SELECT trkorr
          pgmid
          object
          obj_name
     FROM e071
     INTO TABLE t_e071_2
     FOR ALL ENTRIES IN t_e071_1
     WHERE pgmid IN r_pgmid AND
           object EQ t_e071_1-object AND
           obj_name EQ t_e071_1-obj_name.
      IF sy-subrc = 0.
        SORT t_e071_2 BY trkorr object obj_name.
        DELETE ADJACENT DUPLICATES FROM t_e071_2 COMPARING trkorr object obj_name.
      ENDIF.
    ENDIF.

    REFRESH t_e071.
    t_e071[] = t_e071_1[].
    REFRESH t_e071_1.
    t_e071_1[] = t_e071_2[].

  ENDIF.

  "ALL TR status-----------------------------------------"
  IF t_e071_1 IS NOT INITIAL.
    SELECT trkorr
           trfunction
           trstatus
           tarsystem
           korrdev
           as4user
           as4date
           as4time
           strkorr
          FROM e070
      INTO TABLE t_e070
      FOR ALL ENTRIES IN t_e071_1
      WHERE trkorr EQ t_e071_1-trkorr.
    IF sy-subrc = 0.
      SORT t_e070 BY trkorr.
***
*      delete t_e070 where strkorr is initial.
***
    ENDIF.

    "Get TR description-----------------------------------"
    SELECT trkorr
           as4text
       FROM e07t
       INTO TABLE t_e07t
       FOR ALL ENTRIES IN t_e071_1
       WHERE trkorr EQ t_e071_1-trkorr.
    IF sy-subrc = 0.
      SORT t_e07t BY trkorr.
    ENDIF.
  ENDIF.

  "Populate final internal table having all the details-----------------------"

  IF rd_req IS NOT INITIAL OR
     s_report IS NOT INITIAL OR
     s_table IS NOT INITIAL OR
     s_domain IS NOT INITIAL OR
     s_fugp IS NOT INITIAL OR
     s_func IS NOT INITIAL.
    LOOP AT t_e071_1 INTO wa_e071.
      wa_final-trkorr = wa_e071-trkorr.

      READ TABLE t_e070 INTO wa_e070 WITH KEY trkorr = wa_e071-trkorr BINARY SEARCH.
      IF sy-subrc = 0.
        wa_final-trfunction = wa_e070-trfunction.
        wa_final-trstatus = wa_e070-trstatus.
        wa_final-tarsystem = wa_e070-tarsystem.
        wa_final-korrdev = wa_e070-korrdev.
        wa_final-as4user = wa_e070-as4user.
        wa_final-as4date = wa_e070-as4date.
        wa_final-as4time = wa_e070-as4time.
      ENDIF.
      READ TABLE t_e07t INTO wa_e07t WITH KEY trkorr = wa_e071-trkorr BINARY SEARCH.
      IF sy-subrc = 0.
        wa_final-as4text = wa_e07t-as4text.
      ENDIF.

      wa_final-pgmid = wa_e071-pgmid.
      wa_final-object = wa_e071-object.
      wa_final-obj_name = wa_e071-obj_name.
      APPEND wa_final TO t_final.

      CLEAR wa_final.
      CLEAR wa_e07t.
    ENDLOOP.

    IF t_final IS INITIAL.
      MESSAGE s001(ztr) DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

  t_present[] = t_final[].

* Sort it to get the latest TR no based on date and time on any object
  DELETE t_present WHERE NOT ( trfunction EQ 'K' OR trfunction EQ 'W' OR  trfunction EQ 'T' OR trfunction EQ 'S' ). "5/6/2018
  SORT t_present BY obj_name as4date DESCENDING as4time DESCENDING.


ENDFORM.                    " OBJECTS_REQUEST

*&---------------------------------------------------------------------*
*&      Form  MISSING_OBJECTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM missing_objects .

  REFRESH:t_created,t_e071,t_output_list, t_output_list1,t_missing.

  "----------------------------------------------------------------------"
  "-------------------objects included in the TR-------------------------"
  "----------------------------------------------------------------------"
  IF rd_req IS NOT INITIAL.

    "Objcets list used in a TR
    SELECT trkorr
          pgmid
          object
          obj_name
     FROM e071
     INTO TABLE t_e071
     WHERE trkorr IN s_req .
    IF sy-subrc = 0.
      SORT t_e071 BY trkorr obj_name.
      DELETE ADJACENT DUPLICATES FROM t_e071 COMPARING trkorr obj_name.
*      DELETE t_e071 WHERE NOT ( obj_name CP 'Z*' OR
*                                obj_name CP 'Y*' OR
*                                obj_name CP 'LZ*' OR
*                                obj_name CP 'LY*' OR
*                                obj_name CP 'M*' OR
*                                obj_name CP 'S*').

      LOOP AT t_e071 INTO wa_e071 WHERE  pgmid EQ 'R3TR'
       AND object = 'PROG'.
        MOVE wa_e071-obj_name TO s_report-low.
        APPEND s_report .
      ENDLOOP.
    ENDIF.
  ENDIF.


  "----------------------------------------------------------------------"
  "------------------Get created object list-----------------------------"
  "----------------------------------------------------------------------"
  LOOP AT s_report.

    pa_prog = s_report-low.

    CALL FUNCTION 'REPOSITORY_ENVIRONMENT_SET_RFC'
      EXPORTING
        obj_type          = 'PROG'
        environment_types = t_environment_selection
        object_name       = pa_prog
      TABLES
        environment       = t_output_list
        source_objects    = t_source_searched.

    APPEND LINES OF t_output_list TO t_output_list1.
    CLEAR t_output_list.
  ENDLOOP.

  t_created[] = t_output_list1[].
  SORT t_created BY object.
  DELETE ADJACENT DUPLICATES FROM t_created COMPARING object.
*  Removed on 22/5/2018
*  DELETE t_created WHERE NOT ( encl_obj CP 'Z*' OR
*                                encl_obj CP 'Y*' OR
*                                encl_obj CP 'LZ*' OR
*                                encl_obj CP 'LY*' OR
*                                encl_obj CP 'M*' OR
*                                encl_obj CP 'S*').

  DELETE t_created WHERE NOT ( object CP 'Z*' ).

  "----------------------------------------------------------------------"
  "-----------------Get missing objects in the TR------------------------"
  "----------------------------------------------------------------------"
  SORT t_e071 BY obj_name.
  LOOP AT t_created INTO wa_created.
    READ TABLE t_e071 INTO wa_e071 WITH KEY obj_name = wa_created-object BINARY SEARCH.
    IF sy-subrc = 0.
      wa_missing-comments = 'OK...Created Object has been included in the TR'.
      wa_missing-icon = '3'.
    ELSE.
      wa_missing-comments = 'NOK...Created Object Missing in the TR, May lead to Error/Dump'.
      wa_missing-icon = '1'.
    ENDIF.

    wa_missing-type = wa_created-type.
    wa_missing-object = wa_created-object.
    wa_missing-call_obj = wa_created-call_obj.
    wa_missing-call_type = wa_created-call_type.
    APPEND wa_missing TO t_missing.
    CLEAR wa_missing.
  ENDLOOP.

ENDFORM.                    " MISSING_OBJECTS
*&---------------------------------------------------------------------*
*&      Form  TARGET_OBJECTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM target_objects .

  "--------------------------Local Data Declarations--------------------"
  DATA: lv_obj_name  TYPE e071-obj_name,
        lv_obj_name1 TYPE e071-obj_name,
        lv_con_tr    TYPE trkorr.

  DATA wa_temp TYPE s_curr_temp.
  DATA wa_temp1 TYPE s_curr_temp1.

  DATA: t_curr_temp2 TYPE STANDARD TABLE OF s_curr_temp,
        t_re071_temp TYPE STANDARD TABLE OF s_e071,
        wa_e071_temp TYPE s_e071,
        t_e071_temp  TYPE STANDARD TABLE OF s_e071.

  REFRESH: t_remote1, t_remote,t_re071,t_re070,t_out_tab,t_conflict.

  "======================================================================="
  "-------------------Get Target System Details---------------------------"
  "======================================================================="
  IF t_e071 IS NOT INITIAL.
    CLEAR: t_lines, wa_e071, t_options, t_out_tab .
    REFRESH: t_options, t_fields, t_out_tab.

*Populating first line for selection criteria for fetching from target system
    t_text = '(' .
    APPEND t_text TO t_options.

    t_e071_temp[] = t_e071[].
    SORT t_e071_temp by pgmid object obj_name.
    DELETE ADJACENT DUPLICATES FROM t_e071_temp COMPARING pgmid object obj_name.

* Get lines of internal table
    DESCRIBE TABLE t_e071_temp LINES t_lines.

    LOOP AT t_e071_temp INTO wa_e071.
*      SPLIT wa_e071-obj_name AT space INTO lv_obj_name lv_obj_name1.
*      CLEAR wa_e071-obj_name.
*      wa_e071-obj_name = lv_obj_name.
*      IF sy-tabix NE t_lines.
**Populating other line for selection criteria for fetching from target system
*        CONCATENATE 'OBJ_NAME = ''' wa_e071-obj_name ''' OR' INTO t_text.
*      ELSE.
**Populating last line for selection criteria for fetching from target system
*        CONCATENATE 'OBJ_NAME = ''' wa_e071-obj_name ''')' INTO t_text.
*      ENDIF.
      IF sy-tabix NE t_lines.
*Populating other line for selection criteria for fetching from target system
*        CONCATENATE '(' ' PGMID = ''' wa_e071-pgmid ''' AND'
*                         ' OBJECT = ''' wa_e071-object ''' AND'
*                         ' OBJ_NAME = ''' wa_e071-obj_name ''' )' INTO t_text.
        CONCATENATE '(' ' PGMID = ''' wa_e071-pgmid ''' AND'
                         ' OBJECT = ''' wa_e071-object ''' AND' INTO t_text.
        APPEND t_text to t_options.
        CLEAR: t_text.

        CONCATENATE ' OBJ_NAME = ''' wa_e071-obj_name ''' )' INTO t_text.
        t_text1 = ' OR'.

      ELSE.
*Populating last line for selection criteria for fetching from target system
*        CONCATENATE '(' ' PGMID = ''' wa_e071-pgmid ''' AND '
*                         ' OBJECT = ''' wa_e071-object ''' AND '
*                         ' OBJ_NAME = ''' wa_e071-obj_name ''' )' ' )' INTO t_text.
        CONCATENATE '(' ' PGMID = ''' wa_e071-pgmid ''' AND '
                         ' OBJECT = ''' wa_e071-object ''' AND ' INTO t_text.
        APPEND t_text to t_options.
        clear t_text.

        CONCATENATE ' OBJ_NAME = ''' wa_e071-obj_name ''' )' ' )' INTO t_text.
      ENDIF.
      APPEND t_text TO t_options.
      IF t_text1 IS NOT INITIAL.
        APPEND t_text1 to t_options.
      ENDIF.
      CLEAR: t_text, t_text1.
    ENDLOOP.
  ENDIF.                                                    "5/6/2018

  "-------------------------------------------------------------------------------"
  "--------------RFC Call to read E071 table from Target System-------------------"
  "-------------------------------------------------------------------------------"
  DATA: lwa_rfcdest TYPE rfcdes-rfcdest.
  "To check RFC Destination exists or not before read
  SELECT SINGLE rfcdest
         FROM rfcdes
         INTO lwa_rfcdest
         WHERE rfcdest = p_system.
  IF lwa_rfcdest IS NOT INITIAL. "RFC Destination exists or not
**calling RFC FM and fetching table Object Entries of Requests/Tasks (E071)
    CALL FUNCTION 'RFC_READ_TABLE' DESTINATION p_system
      EXPORTING
        query_table          = 'E071'
      TABLES
        options              = t_options
        fields               = t_fields
        data                 = t_out_tab
      EXCEPTIONS
        table_not_available  = 1
        table_without_data   = 2
        option_not_valid     = 3
        field_not_valid      = 4
        not_authorized       = 5
        data_buffer_exceeded = 6
        OTHERS               = 7.
    IF sy-subrc <> 0.
      MESSAGE s016(ztr) DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ELSE.
    MESSAGE s017(ztr) DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  "-----------Formatting the output table received from the RFC FM--------"
  IF t_out_tab[] IS NOT INITIAL.
    LOOP AT t_out_tab.
      wa_e071-trkorr   = t_out_tab+0(20).
      "wa_e071-as4pos   = i_out_tab+20(06).
      wa_e071-pgmid    = t_out_tab+26(04).
      wa_e071-object   = t_out_tab+30(04).
      wa_e071-obj_name = t_out_tab+34(120).
      APPEND wa_e071 TO t_re071.
      CLEAR wa_e071 .
    ENDLOOP.
*  ENDIF."5/6/2018
    SORT t_re071 BY trkorr.


    IF t_re071 IS NOT INITIAL.
      CLEAR: t_lines, wa_e071, t_options.
* Get lines of internal table
      DESCRIBE TABLE t_re071 LINES t_lines.
*Populating first line for selection criteria for fetching from target system
      t_text = '(' .
      APPEND t_text TO t_options1.

      LOOP AT t_re071 INTO wa_e071.
        IF sy-tabix NE t_lines.
*Populating other line for selection criteria for fetching from target system
          CONCATENATE 'TRKORR = ''' wa_e071-trkorr ''' OR' INTO t_text. "Other lines
        ELSE.
*Populating last line for selection criteria for fetching from target system
          CONCATENATE 'TRKORR = ''' wa_e071-trkorr ''')' INTO t_text. "Last line
        ENDIF.
        APPEND t_text TO t_options1.
        CLEAR t_text.
      ENDLOOP.

      "-------------------------------------------------------------------------------"
      "--------------RFC Call to read E070 table from Target System-------------------"
      "-------------------------------------------------------------------------------"
      IF lwa_rfcdest IS NOT INITIAL.    "RFC Destination exists or not
* Calling RFC FM and fetching table Header of Requests/Tasks (E070)
        CALL FUNCTION 'RFC_READ_TABLE' DESTINATION p_system
          EXPORTING
            query_table          = 'E070'
          TABLES
            options              = t_options1
            fields               = t_fields1
            data                 = t_out_tab1
          EXCEPTIONS
            table_not_available  = 1
            table_without_data   = 2
            option_not_valid     = 3
            field_not_valid      = 4
            not_authorized       = 5
            data_buffer_exceeded = 6
            OTHERS               = 7.
        IF sy-subrc <> 0.
          MESSAGE s016(ztr) DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        ENDIF.
      ELSE.
        MESSAGE s017(ztr) DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
* Formatting the output table received from the RFC FM
      LOOP AT t_out_tab1.
        wa_e070-trkorr     = t_out_tab1+0(20).
        wa_e070-trfunction     = t_out_tab1+20(01).
        wa_e070-trstatus   = t_out_tab1+21(01).
        wa_e070-as4user    = t_out_tab1+36(12).
        wa_e070-as4date    = t_out_tab1+48(08).
        wa_e070-as4time    = t_out_tab1+56(06).
        APPEND wa_e070 TO t_re070.
        CLEAR wa_e070 .
      ENDLOOP.
    ENDIF.
    DELETE t_re070 WHERE NOT trfunction EQ 'K' OR trfunction EQ 'W'.

* New changes
    SORT t_senvi_tab BY object.
    SORT t_re070 BY trkorr.
    t_curr_temp2[] = t_curr_temp[].
    DELETE t_curr_temp WHERE devclass NE '$TMP'.
    LOOP AT t_curr_temp INTO wa_temp.
      READ TABLE t_senvi_tab INTO wa_senvi_tab WITH KEY object = wa_temp-obj_name BINARY SEARCH.
      IF sy-subrc EQ 0.
        wa_temp1-object = wa_temp-obj_name.
        wa_temp1-call_obj = wa_senvi_tab-call_obj.
        wa_temp1-type = wa_senvi_tab-type.
        APPEND wa_temp1 TO t_curr_temp1.
        CLEAR: wa_temp1, wa_senvi_tab.
      ENDIF.
    ENDLOOP.
    SORT t_curr_temp1 BY object.
    LOOP AT t_re071 INTO wa_e071.
      READ TABLE t_re070 INTO wa_e070 WITH KEY trkorr = wa_e071-trkorr BINARY SEARCH.
      IF sy-subrc EQ 0.
        READ TABLE t_curr_temp1 INTO wa_temp1 WITH  KEY call_obj = wa_e071-obj_name BINARY SEARCH.
        IF sy-subrc EQ 0.

          wa_conflict-trkorr = wa_e070-trkorr.
          wa_conflict-trstatus = wa_e070-trstatus.
          wa_conflict-as4user = wa_e070-as4user.
          wa_conflict-as4date = wa_e070-as4date.
          wa_conflict-as4time = wa_e070-as4time.
          wa_conflict-pgmid  = wa_e071-pgmid.
          wa_conflict-object = wa_e071-object.
          wa_conflict-obj_name = wa_e071-obj_name.
          wa_conflict-comments = 'NOK..Has objects saved in $TMP'.
          wa_conflict-icon = '1'.
          APPEND wa_conflict TO t_conflict.

*        ELSE.
*          wa_conflict-trkorr = wa_e070-trkorr.
*          wa_conflict-trstatus = wa_e070-trstatus.
*          wa_conflict-as4user = wa_e070-as4user.
*          wa_conflict-as4date = wa_e070-as4date.
*          wa_conflict-as4time = wa_e070-as4time.
*          wa_conflict-pgmid  = wa_e071-pgmid.
*          wa_conflict-object = wa_e071-object.
*          wa_conflict-obj_name = wa_e071-obj_name.
*          wa_conflict-comments = 'OK... Conflict does not exist'.
*          wa_conflict-icon = '3'.
*          APPEND wa_conflict TO t_conflict.
        ENDIF.
      ENDIF.
      clear wa_conflict.

    ENDLOOP.
    t_e071_temp[] = t_e071[].
    DELETE t_e071_temp WHERE trkorr+0(3) NE 'D30'.
    sort t_e071_temp by obj_name.
    DELETE ADJACENT DUPLICATES FROM t_e071_temp COMPARING obj_name.
    LOOP AT t_e071_temp INTO wa_e071.
      CLEAR: ls_request.
      REFRESH: lt_requests.
*    IF p_system = 'P01'.
*      LOOP AT t_e070 INTO wa_e070 WHERE TRFUNCTION = 'K'.
*        READ TABLE t_re070 INTO wa_e0701 WITH KEY trkorr = wa_e071-trkorr BINARY SEARCH.
*        IF sy-subrc NE 0.
*          READ TABLE t_e070 INTO wa_e070 WITH KEY trkorr = wa_e071-trkorr BINARY SEARCH.
*          wa_conflict-trkorr = wa_e070-trkorr.
*          wa_conflict-trstatus = wa_e070-trstatus.
*          wa_conflict-as4user = wa_e070-as4user.
*          wa_conflict-as4date = wa_e070-as4date.
*          wa_conflict-as4time = wa_e070-as4time.
*          wa_conflict-pgmid  = wa_e071-pgmid.
*          wa_conflict-object = wa_e071-object.
*          wa_conflict-obj_name = wa_e071-obj_name.
*          wa_conflict-comments = 'NOK... Conflict exist'.
*          wa_conflict-icon = '1'.
*          APPEND wa_conflict TO t_conflict.
*        ENDIF.
*      ENDLOOP.
*    IF p_system = 'D22'.
*      DESCRIBE TABLE t_re070 LINES t_lines.
*      READ TABLE t_re070 INTO wa_e070 INDEX t_lines.
      t_re071_temp[] = t_re071[].
      DELETE t_re071_temp WHERE trkorr+0(3) NE 'D30'.
      DELETE t_re071_temp where obj_name <> wa_e071-obj_name.

      DESCRIBE TABLE t_re071_temp LINES t_lines.
      IF t_lines = 0.
              READ TABLE t_re070 INTO wa_e0701 WITH KEY trkorr = wa_e071-trkorr BINARY SEARCH.
        IF sy-subrc NE 0.
          READ TABLE t_e070 INTO wa_e070 WITH KEY trkorr = wa_e071-trkorr BINARY SEARCH.
          wa_conflict-trkorr = wa_e070-trkorr.
          wa_conflict-trstatus = wa_e070-trstatus.
          wa_conflict-as4user = wa_e070-as4user.
          wa_conflict-as4date = wa_e070-as4date.
          wa_conflict-as4time = wa_e070-as4time.
          wa_conflict-pgmid  = wa_e071-pgmid.
          wa_conflict-object = wa_e071-object.
          wa_conflict-obj_name = wa_e071-obj_name.
          wa_conflict-comments = 'Object does not exist in Target system'.
          wa_conflict-icon = '2'.
          APPEND wa_conflict TO t_conflict.
        ENDIF.
       CONTINUE.
      ENDIF.
      READ TABLE t_re071_temp INTO wa_e071_temp WITH KEY
                              trkorr = s_req-low.
      IF sy-subrc EQ 0.
        t_lines = sy-tabix.
        t_lines = t_lines - 1.
        IF t_lines GT 0.
          READ TABLE t_re071_temp INTO wa_e071_temp INDEX t_lines.
        ENDIF.
      ELSE.
        READ TABLE t_re071_temp INTO wa_e071_temp INDEX t_lines.
      ENDIF.


      CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
        EXPORTING
          iv_trkorr   = wa_e071_temp-trkorr
          iv_dir_type = 'T'
        IMPORTING
          es_cofile   = ls_request-cofile
          ev_user     = lv_username
          ev_project  = ls_request-project.

      APPEND ls_request to lt_requests.
      LOOP AT lt_requests INTO ls_request.
        IF ls_request-cofile IS NOT INITIAL.
          IF ls_request-cofile-systems[] IS NOT INITIAL.
            READ TABLE ls_request-cofile-systems ASSIGNING <fs_systems> WITH KEY systemid = 'P01'.
            IF sy-subrc = 0 .
              DESCRIBE TABLE <fs_systems>-steps LINES t_lines.
              IF t_lines LE 1.
                READ TABLE t_e070 INTO wa_e070 WITH KEY trkorr = WA_E071_TEMP-TRKORR BINARY SEARCH.
                wa_conflict-trkorr = s_req-Low.
                wa_conflict-trstatus = wa_e070-trstatus.
                wa_conflict-as4user = wa_e070-as4user.
                wa_conflict-as4date = wa_e070-as4date.
                wa_conflict-as4time = wa_e070-as4time.
                wa_conflict-pgmid  = WA_E071_TEMP-pgmid.
                wa_conflict-object = wa_e071-object.
                wa_conflict-obj_name = wa_e071-obj_name.
                wa_conflict-comments = 'NOK... Conflict exist'.
                wa_conflict-icon = '1'.
                APPEND wa_conflict TO t_conflict.
              ELSE.
                READ TABLE t_e070 INTO wa_e070 WITH KEY trkorr = s_req-low BINARY SEARCH.
                wa_conflict-trkorr = s_req-Low.
                wa_conflict-trstatus = wa_e070-trstatus.
                wa_conflict-as4user = wa_e070-as4user.
                wa_conflict-as4date = wa_e070-as4date.
                wa_conflict-as4time = wa_e070-as4time.
                wa_conflict-pgmid  = wa_e071-pgmid.
                wa_conflict-object = wa_e071-object.
                wa_conflict-obj_name = wa_e071-obj_name.
                wa_conflict-comments = 'OK... No Conflict exist'.
                wa_conflict-icon = '3'.
                APPEND wa_conflict TO t_conflict.
            endif.

          ENDIF.
         ENDIF.
        ELSE.
          READ TABLE t_e070 INTO wa_e070 WITH KEY trkorr = s_req-low BINARY SEARCH.
                wa_conflict-trkorr = s_req-Low.
                wa_conflict-trstatus = wa_e070-trstatus.
                wa_conflict-as4user = wa_e070-as4user.
                wa_conflict-as4date = wa_e070-as4date.
                wa_conflict-as4time = wa_e070-as4time.
                wa_conflict-pgmid  = wa_e071-pgmid.
                wa_conflict-object = wa_e071-object.
                wa_conflict-obj_name = wa_e071-obj_name.
                wa_conflict-comments = 'OK... No Conflict exist'.
                wa_conflict-icon = '3'.
                APPEND wa_conflict TO t_conflict.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ELSE.
    t_e071_temp[] = t_e071[].
    DELETE t_e071_temp where pgmid <> 'R3TR'.
    sort t_e071_temp by obj_name.
    DELETE ADJACENT DUPLICATES FROM t_e071_temp COMPARING obj_name.
    LOOP AT t_e071_temp INTO wa_e071.
      READ TABLE t_re070 INTO wa_e0701 WITH KEY trkorr = wa_e071-trkorr BINARY SEARCH.
        IF sy-subrc NE 0.
          READ TABLE t_e070 INTO wa_e070 WITH KEY trkorr = wa_e071-trkorr BINARY SEARCH.
          wa_conflict-trkorr = wa_e070-trkorr.
          wa_conflict-trstatus = wa_e070-trstatus.
          wa_conflict-as4user = wa_e070-as4user.
          wa_conflict-as4date = wa_e070-as4date.
          wa_conflict-as4time = wa_e070-as4time.
          wa_conflict-pgmid  = wa_e071-pgmid.
          wa_conflict-object = wa_e071-object.
          wa_conflict-obj_name = wa_e071-obj_name.
          wa_conflict-comments = 'Object does not exist in Target system'.
          wa_conflict-icon = '2'.
          APPEND wa_conflict TO t_conflict.
        ENDIF.
    ENDLOOP.

  ENDIF.

* New changes


*7/62018 Considering only target system data
  "-----------------------------------------------------------------------------------"
  "------------------Build Final Target System Object table---------------------------"
  "-----------------------------------------------------------------------------------"
**** loop at the internal table containing all objects with all TR's
***  SORT t_e070 BY trkorr.
***  LOOP AT t_re071 INTO wa_e071.
**** Get the corresponding date and time for the corresponding Object and TR no.
***    READ TABLE t_e070 INTO wa_e070 WITH KEY trkorr = wa_e071-trkorr BINARY SEARCH.
***    IF sy-subrc = 0.
**** Move all entries to get all the objects with their TR no, Date and time
**** in the correct format fetched from target system
***      MOVE: wa_e071-trkorr   TO wa_remote-trkorr,
***            "wa_e071-as4pos   TO wa_remote-as4pos,
***            wa_e071-pgmid    TO wa_remote-pgmid,
***            wa_e071-object   TO wa_remote-object,
***            wa_e071-obj_name TO wa_remote-obj_name,
***            wa_e070-trstatus TO wa_remote-trstatus,
***            wa_e070-as4user  TO wa_remote-as4user,
***            wa_e070-as4date  TO wa_remote-as4date,
***            wa_e070-as4time  TO wa_remote-as4time.
***
***      APPEND wa_remote TO t_remote.
***      CLEAR  wa_remote.
***    ENDIF.
***
***  ENDLOOP.
***
***  "Copy to remote1 table---------------------"
***  SORT t_remote BY obj_name as4date DESCENDING as4time DESCENDING.
***  t_remote1[] = t_remote[].
***
***  SORT t_remote1 BY trkorr object obj_name.
***  DELETE ADJACENT DUPLICATES FROM t_remote1 COMPARING trkorr object obj_name.
***endif."5/6/2018
***
***  SORT t_present BY trkorr object obj_name.
****   SORT t_present BY obj_name DESCENDING.
***  DELETE ADJACENT DUPLICATES FROM t_present COMPARING trkorr object obj_name.
***
***  "=========================================================================="
***  "--------------------------To Biuld Conflict Table-------------------------"
***  "=========================================================================="
***  "loop at all target system entries
****  LOOP AT t_remote1 INTO wa_remote1.
***  LOOP AT t_present INTO wa_present.
***    "check if all those TR#s in Target system exist in the present system
****    READ TABLE t_present INTO wa_present WITH KEY trkorr = wa_remote1-trkorr
****                                                  object = wa_remote1-object
****                                                obj_name = wa_remote1-obj_name BINARY SEARCH.
***    READ TABLE t_remote1 INTO wa_remote1 WITH KEY trkorr = wa_present-trkorr
***                                                  object = wa_present-object
***                                                  obj_name = wa_present-obj_name BINARY SEARCH.
***    IF sy-subrc <> 0.
****      "Check if the TR made as never transport
****      READ TABLE i_trnever INTO wa_trnever WITH KEY trkorr = wa_present-trkorr.
****      if sy-subrc <> 0.
***
***      "if the TR#s doesn#t exist, move the detail of TR#s
****       if wa_remote1-trkorr is not INITIAL.
****      wa_conflict-trkorr = wa_remote1-trkorr.
****      wa_conflict-trstatus = wa_remote1-trstatus.
****      wa_conflict-as4user = wa_remote1-as4user.
****      wa_conflict-as4date = wa_remote1-as4date.
****      wa_conflict-as4time = wa_remote1-as4time.
****      wa_conflict-pgmid  = wa_remote1-pgmid.
****      wa_conflict-object = wa_remote1-object.
****      wa_conflict-obj_name = wa_remote1-obj_name.
***
***      wa_conflict-trkorr = wa_present-trkorr.
***      wa_conflict-trstatus = wa_present-trstatus.
***      wa_conflict-as4user = wa_present-as4user.
***      wa_conflict-as4date = wa_present-as4date.
***      wa_conflict-as4time = wa_present-as4time.
***      wa_conflict-pgmid  = wa_present-pgmid.
***      wa_conflict-object = wa_present-object.
***      wa_conflict-obj_name = wa_present-obj_name.
***      IF wa_conflict-trstatus = 'R'.
***        wa_conflict-comments = text-h34. "  'OK...Conflict does not exists'.
***        wa_conflict-icon = '3'.
***      ELSE.
***        wa_conflict-comments = text-h33. "  'NOK...Conflict exists'.
***        wa_conflict-icon = '1'.
***      ENDIF.
***      APPEND wa_conflict TO t_conflict.
***      CLEAR : wa_remote1 , wa_present.
***      CLEAR wa_conflict.
****      endif.
***    ELSE.
***      "move the object with detail with no conflict
***      wa_conflict-trkorr = wa_remote1-trkorr.
***      wa_conflict-trstatus = wa_remote1-trstatus.
***      wa_conflict-as4user = wa_remote1-as4user.
***      wa_conflict-as4date = wa_remote1-as4date.
***      wa_conflict-as4time = wa_remote1-as4time.
***      wa_conflict-pgmid  = wa_remote1-pgmid.
***      wa_conflict-object = wa_remote1-object.
***      wa_conflict-obj_name = wa_remote1-obj_name.
***
***      wa_conflict-comments = text-h34.  "'OK...Conflict does not exists'.
***      wa_conflict-icon = '3'.
***      APPEND wa_conflict TO t_conflict.
***      CLEAR : wa_present , wa_remote1.
***      CLEAR wa_conflict.
***    ENDIF.
***  ENDLOOP.
*  7/6/2018 considering only target system data

*  DELETE t_conflict WHERE trkorr+0(3) NE 'NRD'."5/6/2018 to consider NRD system requests
*  DELETE t_conflict WHERE trkorr IN s_req and icon = '3'.                  "5/6/2018
  REFRESH : t_re071,t_re070,t_out_tab.

ENDFORM.                    " TARGET_OBJECTS
*&---------------------------------------------------------------------*
*&      Form  DEPENDENCY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM conflict_check .

  DATA: t_dep_temp1 TYPE STANDARD TABLE OF s_final_temp.

  REFRESH: t_dep_temp,t_tadir,t_senvi_tab,t_e071_p,
           t_e070_p,t_e07t_p,t_current .

  "----------Source System Ovjects & TR list-----------------"
  t_dep_temp[] = t_present.
  SORT t_dep_temp BY trkorr object obj_name.
  DELETE ADJACENT DUPLICATES FROM t_dep_temp COMPARING trkorr object obj_name.

  "Get all repository objects from TADIR table-------------"
  IF NOT t_dep_temp IS INITIAL.
    SELECT
    object
    obj_name
    devclass
    FROM tadir
    INTO TABLE t_tadir
    FOR ALL ENTRIES IN t_dep_temp
    WHERE ( pgmid = 'R3TR' ) AND
        object  = t_dep_temp-object AND
       obj_name = t_dep_temp-obj_name.
    IF sy-subrc = 0.
      SORT t_tadir BY object obj_name.
    ENDIF.
  ENDIF.

  "-----------Get deep custom objects used inside the objects----------------"
  IF t_dep_temp IS NOT INITIAL.
    SORT t_dep_temp BY object obj_name.
    LOOP AT t_tadir INTO wa_tadir.
      READ TABLE t_dep_temp INTO wa_curr WITH KEY object = wa_tadir-object
                                                obj_name = wa_tadir-obj_name BINARY SEARCH.
      IF sy-subrc EQ 0.
        "Get deep custom objects used inside the objects
        PERFORM repository_environment USING wa_curr.
      ENDIF.
    ENDLOOP.

*    IF .
*
*    ENDIF.
    t_dep_temp1[] = t_dep_temp[].
    SORT t_dep_temp1 by obj_name.
    DELETE ADJACENT DUPLICATES FROM t_dep_temp1 COMPARING obj_name.

    LOOP AT t_dep_temp1 INTO wa_curr WHERE object EQ 'FUNC'.
      "Get deep custom objects used inside the objects
      PERFORM repository_environment USING wa_curr.
    ENDLOOP.

    "---------------------Check for Custom Objects-----------------------------"
    "Checking for z/y objects
    LOOP AT t_senvi_tab INTO wa_senvi_tab.
      IF wa_senvi_tab-object+0(1) = 'Z' OR
         wa_senvi_tab-object+0(1) = 'Y' OR
         wa_senvi_tab-object+0(2) = 'LZ' OR
  (      wa_senvi_tab-object+4(1) = 'Z' AND
         wa_senvi_tab-object+0(1) = 'S' ).
***      Local Objects check "ND11544"
*      IF wa_senvi_tab-object+0(5) = 'Z'.
        wa_senvi_tab-genflag = 'D'.
        MODIFY t_senvi_tab FROM wa_senvi_tab .
        CLEAR wa_senvi_tab.
      ENDIF.
    ENDLOOP.
    DELETE t_senvi_tab WHERE genflag <> 'D'.

    "---------------------To Find Function Group & Function Module------------"
    "Find Function Group for function module and get all objects
    "used in it(Data element,domain, search helps etc)
    PERFORM function_modules.

    "-----------------------To Get Repository Objects-------------------------"
    PERFORM get_from_tadir.

    "-------------------Delete Objects Havinf $TMP package--------------------"
*    DELETE t_tadir WHERE devclass EQ '$TMP'.

  ENDIF.

  "============================================================================="
  "-----------"Get TR list for deep custom objects------------------------------"
  "============================================================================="
  "Get TR list for deep custom objects like domain, data elements, search helps.
  IF t_tadir IS NOT INITIAL."Objcets list used in a TR

    CLEAR wa_tadir.
    LOOP AT t_tadir INTO wa_tadir.
      wa_curr_temp-object = wa_tadir-object.
      wa_curr_temp-obj_name = wa_tadir-obj_name.
      wa_curr_temp-devclass = wa_tadir-devclass.
      APPEND wa_curr_temp TO t_curr_temp.
      CLEAR wa_curr_temp.
    ENDLOOP.

    SORT t_curr_temp BY object obj_name.
    DELETE ADJACENT DUPLICATES FROM t_curr_temp COMPARING object obj_name.

    "All the TR's of individual objects--------------------"
    IF t_curr_temp IS NOT INITIAL.
      SELECT trkorr
          pgmid
          object
          obj_name
     FROM e071
     INTO TABLE t_e071_p
     FOR ALL ENTRIES IN t_curr_temp
     WHERE pgmid IN r_pgmid AND
           object IN r_object AND
           obj_name EQ t_curr_temp-obj_name.
      IF sy-subrc = 0.

        SORT t_e071_p BY trkorr object obj_name.
        DELETE ADJACENT DUPLICATES FROM t_e071_p COMPARING trkorr object obj_name.
*        DELETE t_e071_p WHERE obj_name(5) NE 'Z'.
*        DELETE t_e071_p WHERE NOT ( obj_name CP 'Z*' OR
*                                 obj_name CP 'Y*' OR
*                                 obj_name CP 'LZ*' OR
*                                 obj_name CP 'LY*' OR
*                                 obj_name CP 'M*' OR
*                                 obj_name CP 'S*').

      ENDIF.
    ENDIF.

    "Get ALL TR status-----------------------------------"
    IF t_e071_p IS NOT INITIAL.
      SELECT trkorr
             trfunction
             trstatus
             tarsystem
             korrdev
             as4user
             as4date
             as4time
             strkorr
            FROM e070
        INTO TABLE t_e070_p
        FOR ALL ENTRIES IN t_e071_p
        WHERE trkorr EQ t_e071_p-trkorr.
      IF sy-subrc = 0.
        SORT t_e070_p BY trkorr.
      ENDIF.

      "Get TR description-----------------------------"
      SELECT trkorr
               as4text
         FROM e07t
         INTO TABLE t_e07t_p
         FOR ALL ENTRIES IN t_e071_p
         WHERE trkorr EQ t_e071_p-trkorr.
      IF sy-subrc = 0.
        SORT t_e07t_p BY trkorr.
      ENDIF.
    ENDIF.

    "---------------------Biuld Final Table------------------------"
    CLEAR wa_e071.
    CLEAR wa_final.
    SORT t_e070_p BY trkorr.
    LOOP AT t_e071_p INTO wa_e071.
      wa_final-trkorr = wa_e071-trkorr.
      CLEAR wa_e070.
      READ TABLE t_e070_p INTO wa_e070 WITH KEY trkorr = wa_e071-trkorr BINARY SEARCH.
      IF sy-subrc = 0.
        wa_final-trfunction = wa_e070-trfunction.
        wa_final-trstatus = wa_e070-trstatus.
        wa_final-tarsystem = wa_e070-tarsystem.
        wa_final-korrdev = wa_e070-korrdev.
        wa_final-as4user = wa_e070-as4user.
        wa_final-as4date = wa_e070-as4date.
        wa_final-as4time = wa_e070-as4time.
      ENDIF.
      CLEAR wa_e07t.
      READ TABLE t_e07t_p INTO wa_e07t WITH KEY trkorr = wa_e071-trkorr BINARY SEARCH.
      IF sy-subrc = 0.
        wa_final-as4text = wa_e07t-as4text.
      ENDIF.

      wa_final-pgmid = wa_e071-pgmid.
      wa_final-object = wa_e071-object.
      wa_final-obj_name = wa_e071-obj_name.
      APPEND wa_final TO t_current.

      CLEAR wa_final.
    ENDLOOP.

* Sort it to get the latest TR no based on date and time on any object
    DELETE t_current WHERE NOT ( trfunction EQ 'K' OR trfunction EQ 'W'  or trfunction EQ 'S' ). "5/6/2018
    SORT t_current BY obj_name as4date DESCENDING as4time DESCENDING.

    IF t_e071_p IS INITIAL OR t_current IS INITIAL.
      MESSAGE s001(ztr) DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

  "------------------------Final internal table having all the objects------------------"
  "Final internal table having all the objects(including data elements , domains etc) and there TR's
  "if thse TR's exists in target sytem then no conflicts exists.(have to check target system using RFC)
  IF t_current IS NOT INITIAL.
    APPEND LINES OF t_e071_p TO t_e071.
    SORT t_e071 BY trkorr object obj_name.
    DELETE ADJACENT DUPLICATES FROM t_e071 COMPARING trkorr object obj_name.

    APPEND LINES OF t_e070_p TO t_e070.
    SORT t_e070 BY trkorr .
    DELETE ADJACENT DUPLICATES FROM t_e070 COMPARING trkorr .

    APPEND LINES OF t_current TO t_present.
    SORT t_present BY trkorr object obj_name.
    DELETE ADJACENT DUPLICATES FROM t_present COMPARING trkorr object obj_name.
  ENDIF.

  REFRESH: t_e071_p,t_e070_p.

  "To get never transport requests from the source system-----------------"
*  IF t_present IS NOT INITIAL.
*    SELECT trkorr
*           tarsystem
*           udate
*           uname
*           requester
*           FROM ztr_never
*           INTO TABLE t_trnever
*           FOR ALL ENTRIES IN t_present
*           WHERE trkorr EQ t_present-trkorr.
*    if sy-subrc = 0.
*      SORT t_trnever BY trkorr.
*    ENDIF.
*  ENDIF.


ENDFORM.                    " DEPENDENCY_CHECK
*&---------------------------------------------------------------------*
*&      Form  ENVIRONMENT_TYPES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM environment_types .

  t_environment_selection-prog = 'X'. "Program
  t_environment_selection-fugr = 'X'. "Function group
  t_environment_selection-ldba = 'X'. "Logical database
  t_environment_selection-msag = 'X'. "Message class
  t_environment_selection-tran = 'X'. "Transaction
  t_environment_selection-func = 'X'. "Function module
  t_environment_selection-dial = 'X'. "Dialog module
  t_environment_selection-tabl = 'X'. "Database tables
  t_environment_selection-shlp = 'X'. "Search Help
  t_environment_selection-doma = 'X'. "Domain
  t_environment_selection-dtel = 'X'. "Data element
  t_environment_selection-view = 'X'. "View
  t_environment_selection-mcob = 'X'. "Matchcode object
  t_environment_selection-para = 'X'. "SET/GET parameters
  t_environment_selection-conv = 'X'. "Conversion exit
  t_environment_selection-suso = 'X'. "Authorization objects
  t_environment_selection-type = 'X'. "Type groups
  t_environment_selection-ttyp = 'X'. "Table types
  t_environment_selection-stru = 'X'. "Structure
  t_environment_selection-enqu = 'X'. "Lock object
  t_environment_selection-sqlt = 'X'. "SQL tables
  t_environment_selection-clas = 'X'. "Class
  t_environment_selection-intf = 'X'. "Interfaces
  t_environment_selection-udmo = 'X'. "Data model
  t_environment_selection-shi3 = 'X'. "Area menu
  t_environment_selection-cntx = 'X'. "Context
  t_environment_selection-ttab = 'X'. "Table types
  t_environment_selection-enho = 'X'. "enhancements
*  t_environment_selection-stob = 'X'. "entities  " akmadasu
  t_environment_selection-DEVC = 'X'. "package

ENDFORM.                    " ENVIRONMENT_TYPES


*&---------------------------------------------------------------------*
*&      Form  FILL_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_R_FIELD  text
*      -->P_2101   text
*----------------------------------------------------------------------*
*FORM fill_range TABLES r_field USING field1 .
*
*  wa_field-sign = 'I'.
*  wa_field-option = 'EQ'.
*  wa_field-low = field1.
*  APPEND wa_field TO r_field.
*  CLEAR wa_field.
*
*ENDFORM.                    " FILL_RANGE
*&---------------------------------------------------------------------*
*&      Form  REPOSITORY_ENVIRONMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM repository_environment USING lwa_curr TYPE s_final.

  "--------------------------------------------------------------------"
  " Local Data Declaration
  "--------------------------------------------------------------------"
  DATA : lv_obj_type TYPE euobj-id VALUE 'P',
         p_prog      TYPE tadir-obj_name.
  DATA:  wa_senvi_tab   TYPE senvi.


  DATA : t_senvi_tab1 TYPE STANDARD TABLE OF senvi,
         t_rsfind1    TYPE STANDARD TABLE OF rsfind,
         t_rsfind     TYPE STANDARD TABLE OF rsfind.
  CONSTANTS: lc_1 TYPE i VALUE '1'.
  "---------------------------------------------------------------------"

  "Object Type
  IF lwa_curr-object IS NOT INITIAL.
    MOVE lwa_curr-object TO lv_obj_type.
  ENDIF.

  "Object Name
  IF lwa_curr-obj_name IS NOT INITIAL.
    MOVE lwa_curr-obj_name TO p_prog.
  ENDIF.
  CLEAR: t_senvi_tab1,t_rsfind1.

  " if object type and object name are not initial
  IF NOT lv_obj_type IS INITIAL AND p_prog IS NOT INITIAL.
    "Get deep custom objects used using the FM
    CALL FUNCTION 'REPOSITORY_ENVIRONMENT_ALL'
      EXPORTING
        obj_type          = lv_obj_type
        environment_types = t_environment_selection
        object_name       = p_prog
*       PARALLEL_TASK     =
        deep              = lc_1
*       WITH_MEMORY       = ' '
      TABLES
        environment_tab   = t_senvi_tab1
        source_objects    = t_rsfind1.
  ENDIF.

  " Append the objects for which deep objects has been determined
  MOVE lv_obj_type TO wa_itab-object.
  MOVE p_prog TO wa_itab-obj_name.
  MOVE lv_obj_type TO wa_senvi_tab-type.
  MOVE p_prog TO wa_senvi_tab-object.
  APPEND wa_itab TO t_itab.
  CLEAR: wa_itab, wa_senvi_tab.

  "Append all determinaed deep objects
  APPEND LINES OF t_senvi_tab1 TO t_senvi_tab.
  APPEND LINES OF t_rsfind1 TO t_rsfind.
ENDFORM.                    " REPOSITORY_ENVIRONMENT
*&---------------------------------------------------------------------*
*&      Form  FUNCTION_MODULES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM function_modules .

  "---------------------------------------------------------------------"
  "Constants
  CONSTANTS:lc_func(4) TYPE c VALUE 'FUNC',
            lc_fugr(4) TYPE c VALUE 'FUGR',
            lc_stru(4) TYPE c VALUE 'STRU',
            lc_tabl(4) TYPE c VALUE 'TABL',
            lc_tabd(4) TYPE c VALUE 'TABD',
            lc_dtel(4) TYPE c VALUE 'DTEL',
            lc_dted(4) TYPE c VALUE 'DTED'.

  CONSTANTS:lc_z(1)  TYPE c VALUE 'Z',
            lc_y(1)  TYPE c VALUE 'Y',
            lc_lz(2) TYPE c VALUE 'LZ',
            lc_zz(2) TYPE c VALUE 'ZZ'.
  "----------------------------------------------------------------------"

  "Check for Function Module
  LOOP AT t_senvi_tab INTO wa_senvi_tab.
    CASE wa_senvi_tab-type.
      WHEN lc_func.        " If it is function module
        MOVE wa_senvi_tab-type TO wa_itab-object.
        MOVE wa_senvi_tab-object TO wa_itab-func.
        APPEND wa_itab TO t_itab1.
        CLEAR wa_itab.
      WHEN OTHERS.        " if any other object
        MOVE wa_senvi_tab-object TO wa_itab-obj_name.
        MOVE wa_senvi_tab-type TO wa_itab-object.
        APPEND wa_itab TO t_itab.
        CLEAR wa_itab.
    ENDCASE.
  ENDLOOP.

  SORT t_itab1 BY func.
  DELETE ADJACENT DUPLICATES FROM t_itab1 COMPARING func.

  "Selecting Function group for a function module
  IF NOT t_itab1[] IS INITIAL.
    SELECT funcname
           area
           FROM enlfdir
      INTO TABLE t_enlfdir
      FOR ALL ENTRIES IN t_itab1
      WHERE funcname = t_itab1-func.
    IF sy-subrc = 0.
      SORT t_enlfdir BY funcname area.
    ENDIF.
    LOOP AT t_enlfdir INTO wa_enlfdir.
      MOVE wa_enlfdir-area TO wa_itab-obj_name.
      MOVE lc_fugr TO wa_itab-object.
      APPEND wa_itab TO t_itab.
      CLEAR: wa_enlfdir, wa_itab.
    ENDLOOP.
  ENDIF.


  CLEAR t_itab2.

  SORT t_itab BY object.
  LOOP AT t_itab INTO wa_itab .
    "Get all the data elements for Y/Z (table or structure).
    IF wa_itab-object EQ lc_stru OR wa_itab-object EQ lc_tabl
                                  OR wa_itab-object EQ lc_tabd.
      IF  wa_itab-obj_name+0(1) = lc_z OR
          wa_itab-obj_name+0(1) = lc_y OR
          wa_itab-obj_name+0(2) = lc_lz OR
          wa_itab-obj_name+4(1) = lc_z OR
          wa_itab-obj_name+0(2) = lc_zz .
*      IF  wa_itab-obj_name+0(5) = 'Z'.
        APPEND wa_itab TO t_itab2.
        CLEAR wa_itab.
      ENDIF.
    ELSEIF wa_itab-object EQ lc_dtel OR wa_itab-object EQ lc_dted .

      "Get all the domains, Parameter id and search helps for Y/Z (table or structure).
      IF wa_itab-obj_name+0(1) = lc_y OR wa_itab-obj_name+0(1) = lc_z.
*      IF wa_itab-obj_name+0(5) = 'Z' .

        REFRESH t_dd04ll.
        MOVE wa_itab-obj_name TO wa_dd04ll-rollname.
        APPEND wa_dd04ll TO t_dd04ll.
        CLEAR wa_dd04ll.

      ENDIF.
    ENDIF.
  ENDLOOP.

  "Get all the data elements for Y/Z (table or structure).
  IF t_itab2 IS NOT INITIAL.
    PERFORM get_data_element_domain.
  ENDIF.

  "Get all the domains, Parameter id and search helps for Y/Z (table or structure).
  IF t_dd04ll IS NOT INITIAL .
    PERFORM repository_environment_select.
  ENDIF.


  LOOP AT t_environment INTO wa_environment.
    READ TABLE t_dep_temp INTO wa_curr WITH KEY object = wa_environment-type
                                              obj_name = wa_environment-object BINARY SEARCH.
    IF sy-subrc NE 0.
      MOVE wa_environment-type TO wa_itab-object.
      MOVE wa_environment-object TO wa_itab-obj_name.
      APPEND wa_itab TO t_itab.
      CLEAR wa_itab.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " FUNCTION_MODULES
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_ELEMENT_DOMAIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_element_domain .

  IF t_itab2 IS NOT INITIAL.
    SELECT tabname
          rollname
     FROM dd03l
     INTO TABLE t_dd03l
     FOR ALL ENTRIES IN t_itab2
     WHERE tabname EQ t_itab2-obj_name.
    IF sy-subrc = 0.
      LOOP AT t_dd03l INTO wa_dd03l.
        MOVE 'DTEL' TO wa_itab-object.
        MOVE wa_dd03l-fieldname TO wa_itab-obj_name.
        APPEND wa_itab TO t_itab.
        CLEAR wa_itab.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_DATA_ELEMENT_DOMAIN
*&---------------------------------------------------------------------*
*&      Form  REPOSITORY_ENVIRONMENT_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM repository_environment_select .

  "----------------------------------------------------------------------"
  CONSTANTS: lc_dtel(4) TYPE c VALUE 'DTEL',
             lc_para(4) TYPE c VALUE 'PARA',
             lc_doma(4) TYPE c VALUE 'DOMA',
             lc_r(1)    TYPE c VALUE 'R',
             lc_c(1)    TYPE c VALUE 'C',
             lc_i(1)    TYPE c VALUE 'I',
             lc_clas(4) TYPE c VALUE 'CLAS',
             lc_intf(4) TYPE c VALUE 'INTF',
             lc_ttyp(4) TYPE c VALUE 'TTYP',
             lc_tabl(4) TYPE c VALUE 'TABL',
             lc_shlp(4) TYPE c VALUE 'SHLP',
             lc_d(1)    TYPE c VALUE 'D',
             lc_l(1)    TYPE c VALUE 'L',
             lc_e(1)    TYPE c VALUE 'E',
             lc_s(1)    TYPE c VALUE 'S',
             lc_0(1)    TYPE c VALUE '0'.
  CONSTANTS :lc_x(1) TYPE c VALUE 'X',
             lc_z(1) TYPE c VALUE 'Z',
             lc_y(1) TYPE c VALUE 'Y'.

  "----------------------------------------------------------------------"


  IF t_dd04ll IS NOT INITIAL.
    SELECT rollname
           domname
           memoryid
           shlpname
           refkind
           reftype
           FROM dd04l
           INTO TABLE t_dd04l
           FOR ALL ENTRIES IN t_dd04ll
           WHERE rollname EQ t_dd04ll-rollname.
    IF sy-subrc = 0.

      LOOP AT t_dd04l INTO wa_dd04l WHERE  ( rollname+0(1) = lc_y OR rollname+0(1) = lc_x ).
*                                           rollname+0(5) = 'Z*' .

        wa_environment-type   = lc_dtel.
        wa_environment-object = wa_dd04l-rollname.
        wa_environment-done   = lc_x.
        CALL FUNCTION 'REPOSITORY_ENVIRONMENT_FILL'
          EXPORTING
            environment        = wa_environment
            environment_output = space
          TABLES
            environment_table  = t_environment
          EXCEPTIONS
            OTHERS             = 0.
        IF sy-subrc NE 0.
        ENDIF.


        "Parameter id
        IF NOT wa_envi_types-para IS INITIAL
           AND NOT wa_dd04l-memoryid IS INITIAL.
          wa_environment-type        = lc_para.
          wa_environment-done        = lc_x.
          wa_environment-object      = wa_dd04l-memoryid.
          wa_environment-call_type   = lc_dtel.
          wa_environment-call_obj    = wa_dd04l-rollname.
          CALL FUNCTION 'REPOSITORY_ENVIRONMENT_FILL'
            EXPORTING
              environment        = wa_environment
              environment_output = lc_x
            TABLES
              environment_table  = t_environment
            EXCEPTIONS
              OTHERS             = 0.
          IF sy-subrc NE 0.
          ENDIF.
        ENDIF.


        "Domains
        IF NOT wa_envi_types-doma IS INITIAL
          AND  NOT wa_dd04l-domname IS INITIAL AND
          ( wa_dd04l-refkind EQ lc_d OR  wa_dd04l-refkind EQ space ).
          wa_environment-type         = lc_doma.
          wa_environment-object       = wa_dd04l-domname.
          wa_environment-done         = space.
          wa_environment-call_type    = lc_dtel.
          wa_environment-call_obj     = wa_dd04l-rollname.
          CALL FUNCTION 'REPOSITORY_ENVIRONMENT_FILL'
            EXPORTING
              environment        = wa_environment
              environment_output = lc_x
            TABLES
              environment_table  = t_environment
            EXCEPTIONS
              OTHERS             = 0.
          IF sy-subrc NE 0.
          ENDIF.
        ENDIF.

        " Reference type
        IF NOT wa_dd04l-domname IS INITIAL AND  wa_dd04l-refkind EQ lc_r.
          CLEAR wa_environment-type.
          CASE wa_dd04l-reftype.
            WHEN lc_c OR lc_i OR space.
              "classes and interfacse
              IF ( wa_envi_types-clas NE space OR wa_envi_types-intf NE  space ).
                SELECT SINGLE  clstype FROM seoclass INTO v_seoclstype
                       WHERE clsname = wa_dd04l-domname.
                IF sy-subrc EQ 0.
                  IF v_seoclstype = lc_0.
                    wa_environment-type         = lc_clas.
                  ELSE.
                    wa_environment-type         = lc_intf.
                  ENDIF.
                ENDIF.
              ENDIF.
            WHEN lc_l.
              "table type
              wa_environment-type         = lc_ttyp.
            WHEN lc_e.
              "Date element
              wa_environment-type         = lc_dtel.
            WHEN lc_s.
              "table type
              wa_environment-type         = lc_tabl.
          ENDCASE.
          IF ( wa_environment-type = lc_clas AND wa_envi_types-clas NE space )
          OR ( wa_environment-type = lc_intf AND wa_envi_types-intf NE space )
          OR ( wa_environment-type = lc_ttyp AND wa_envi_types-ttyp NE space )
          OR ( wa_environment-type = lc_dtel AND wa_envi_types-dtel NE space )
          OR ( wa_environment-type = lc_tabl AND wa_envi_types-tabl NE space ).
            wa_environment-object       = wa_dd04l-domname.
            wa_environment-done         = space.
            wa_environment-call_type    = lc_dtel.
            wa_environment-call_obj     = wa_dd04l-rollname.
            CALL FUNCTION 'REPOSITORY_ENVIRONMENT_FILL'
              EXPORTING
                environment        = wa_environment
                environment_output = lc_x
              TABLES
                environment_table  = t_environment
              EXCEPTIONS
                OTHERS             = 0.
            IF sy-subrc NE 0.
            ENDIF.
          ENDIF.
        ENDIF.


        "Search help name
        IF NOT wa_envi_types-shlp IS INITIAL
           AND NOT wa_dd04l-shlpname IS INITIAL.
          wa_environment-type        = lc_shlp.
          wa_environment-done        = space.
          wa_environment-object      = wa_dd04l-shlpname.
          wa_environment-call_type   = lc_dtel.
          wa_environment-call_obj    = wa_dd04l-rollname.
          CALL FUNCTION 'REPOSITORY_ENVIRONMENT_FILL'
            EXPORTING
              environment        = wa_environment
              environment_output = lc_x
            TABLES
              environment_table  = t_environment
            EXCEPTIONS
              OTHERS             = 0.
          IF sy-subrc NE 0.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDIF.
  ENDIF.

ENDFORM.                    " REPOSITORY_ENVIRONMENT_SELECT
*&---------------------------------------------------------------------*
*&      Form  GET_FROM_TADIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_from_tadir .

  "---------------------------------------------------------------------"
  CONSTANTS: lc_incl(4) TYPE c VALUE 'INCL',
             lc_prog(4) TYPE c VALUE 'PROG',
             lc_stru(4) TYPE c VALUE 'STRU',
             lc_tabl(4) TYPE c VALUE 'TABL',
             lc_r3tr(4) TYPE c VALUE 'R3TR'.

*  CONSTANTS:    lc_z(1)    TYPE c VALUE 'Z',
*           lc_x(1)    TYPE c VALUE 'X',
*           lc_y(1)    TYPE c VALUE 'Y',
*           lc_lz(2)   TYPE c VALUE 'LZ',
*           lc_zz(2)   TYPE c VALUE 'ZZ'.

  "-----------------------------------------------------------------------"

  APPEND LINES OF t_itab TO t_itab_dup.
  SORT t_itab_dup BY object obj_name.
  DELETE ADJACENT DUPLICATES FROM t_itab_dup COMPARING object obj_name.

  "Checking for z/y objects
  LOOP AT t_itab_dup INTO wa_itab.
    IF wa_itab-obj_name+0(1)  = 'Z' OR
       wa_itab-obj_name+0(1)  = 'Y' OR
       wa_itab-obj_name+0(2)  = 'LZ' OR
(       wa_itab-obj_name+4(1) = 'Z' AND
       wa_itab-obj_name+0(1)  = 'S' ) OR
       wa_itab-obj_name+0(2)  = 'ZZ'  .
*    IF wa_itab-obj_name+0(5)  = 'Z'.
      wa_itab-genflag = 'X'.
      MODIFY t_itab_dup FROM wa_itab.
      CLEAR wa_itab.
    ENDIF.
  ENDLOOP.
  DELETE t_itab_dup WHERE genflag <> 'X'.


  "Includes and Structures are treated as Programs and Tables while
  "storing in Tadir table
  LOOP AT t_itab_dup INTO wa_itab.
    IF wa_itab-object EQ lc_incl.
      wa_itab-object = lc_prog.
    ELSEIF wa_itab-object EQ lc_stru.
      wa_itab-object = lc_tabl.
    ENDIF.
    MODIFY t_itab_dup FROM wa_itab TRANSPORTING object.
    CLEAR wa_itab.
  ENDLOOP.

  IF NOT t_itab_dup[] IS INITIAL.
    SELECT
    object
    obj_name
    devclass FROM tadir INTO TABLE t_tadir
                               FOR ALL ENTRIES IN t_itab_dup
                               WHERE pgmid = lc_r3tr AND
                               object      = t_itab_dup-object AND
                               obj_name    = t_itab_dup-obj_name.
    IF sy-subrc = 0.
      SORT t_tadir BY  object obj_name.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_FROM_TADIR
*&---------------------------------------------------------------------*
*&      Form  DEPENDECY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dependecy_check .

  DATA: "lv_system(8) TYPE c,
    lv_tabix   TYPE sy-tabix,
    lv_strkorr TYPE e070-strkorr.

  "================================================================================="
  "---To Read Transport Import Log to get the Latest Target System Details of TR's--"
  "================================================================================="
  LOOP AT t_e070 INTO wa_e070.

    ls_request-header-trkorr = wa_e070-trkorr.
    "To get Transport Header--------------------------------"
    CALL FUNCTION 'TRINT_READ_REQUEST_HEADER'
      EXPORTING
        iv_read_e070 = 'X'
        iv_read_e07t = 'X'
      CHANGING
        cs_request   = ls_request-header
      EXCEPTIONS
        OTHERS       = 1.
    IF sy-subrc <> 0.
      ls_request-header-trkorr = wa_e070-trkorr.
    ENDIF.

    "To get Transport Import Log------------------------------"
    CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
      EXPORTING
        iv_trkorr   = wa_e070-trkorr
        is_settings = ls_settings
      IMPORTING
        es_cofile   = ls_request-cofile
        ev_user     = lv_username
        ev_project  = ls_request-project.
    IF ls_request-header-as4user = space.
      ls_request-header-as4user = lv_username.
    ENDIF.
    ls_request-cofile_filled = 'X'.
    APPEND ls_request TO lt_requests.
    CLEAR wa_e070-trkorr.
  ENDLOOP.

  SORT lt_requests BY header-trkorr.


  "============================================================================="
  "---------------Transport Dependency Check------------------------------------"
  "============================================================================="
  REFRESH t_deptr.
  SORT t_e071_1 BY obj_name.

  LOOP AT t_e071 INTO wa_e071 WHERE trkorr IN s_req.
    READ TABLE t_e071_1  WITH KEY obj_name = wa_e071-obj_name BINARY SEARCH TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      lv_tabix = sy-tabix.
      LOOP AT t_e071_1 INTO wa_e071_1 FROM lv_tabix.
        IF  wa_e071_1-obj_name = wa_e071-obj_name.
          IF wa_e071_1-trkorr NE wa_e071-trkorr.
            wa_deptr-trkorr    = wa_e071-trkorr.
            wa_deptr-obj_name  = wa_e071-obj_name.
            wa_deptr-object    = wa_e071-object.
            wa_deptr-blank     = '   '.
*            wa_deptr-dep_tr    = wa_e071_1-trkorr."commented as it captures tasks as well

            CLEAR wa_e070.
            READ TABLE t_e070 INTO wa_e070 WITH KEY trkorr = wa_e071_1-trkorr BINARY SEARCH.
            IF sy-subrc = 0.
              "IF wa_e070-strkorr IS NOT INITIAL.
              "Read higher level request data
*                READ TABLE i_e070 INTO wa_e070_1 WITH KEY trkorr = wa_e070-strkorr BINARY SEARCH.
*                if sy-subrc = 0.
*                  wa_e070 = wa_e070_1.
*                ENDIF.
*added on 30/5/2018 dependent TR details
              IF ( wa_e070-trfunction EQ 'K' ) OR
                 ( wa_e070-trfunction EQ 'W' ).
                wa_deptr-dep_tr    = wa_e071_1-trkorr.
                wa_deptr-trstatus  = wa_e070-trstatus.
              ENDIF.
* *added on 30/5/2018

*              wa_deptr-trstatus  = wa_e070-trstatus."30/5/2018
              "wa_deptr-tarsystem = wa_e070-tarsystem.

*              CONCATENATE p_system '*' INTO lv_system.
*
*              IF wa_e070-tarsystem CP lv_system AND
*                 wa_e070-trstatus = 'R'.
*                wa_deptr-comments = text-h12.   "'OK...Dependent TR already Exists in the Target System'.
*                wa_deptr-icon = '3'.
*              ELSEIF wa_e070-trfunction = 'T'.
*                wa_deptr-comments = text-h13.   "'OK...Generated Test Transport'.
*                wa_deptr-icon = '3'.
*              ELSEIF wa_e070-strkorr = wa_e071-trkorr.
*                wa_deptr-comments = text-h14.  "'OK...Higher level request of the entered TR'.
*                wa_deptr-icon = '3'.
*              ELSE.
*                wa_deptr-comments = text-h15.  "'NOK...Dependent TR does not Exists in the Target System'.
*                wa_deptr-icon = '1'.
*              ENDIF.

              IF wa_e070-strkorr IS INITIAL.
                READ TABLE lt_requests ASSIGNING <fs_requests>  WITH KEY header-trkorr = wa_e070-trkorr BINARY SEARCH.
                IF sy-subrc = 0.
                  SORT <fs_requests>-cofile-systems BY systemid DESCENDING.
                ENDIF.

              ELSEIF wa_e070-strkorr IS NOT INITIAL.   "higher level request exists
                CLEAR lv_strkorr.
                lv_strkorr = wa_e070-strkorr.   "Store Higher level request
                CLEAR wa_e070.
                "Read higher level transport
                READ TABLE t_e070 INTO wa_e070 WITH KEY trkorr = lv_strkorr BINARY SEARCH.
                IF sy-subrc = 0.
                  "Read transport log based on the higher level request
                  READ TABLE lt_requests ASSIGNING <fs_requests>  WITH KEY header-trkorr = lv_strkorr BINARY SEARCH.
                  IF sy-subrc = 0.
                    "sorting target system list descending to get latest transport system
                    SORT <fs_requests>-cofile-systems BY systemid DESCENDING.
                  ENDIF.
                ENDIF.
              ENDIF.

              IF <fs_requests> IS ASSIGNED.
                "read latest target system
                READ TABLE <fs_requests>-cofile-systems ASSIGNING <fs_systems> INDEX 1.
                IF sy-subrc = 0.
                  wa_deptr-tarsystem = <fs_systems>-systemid.

                  SORT <fs_systems>-steps BY clientid DESCENDING.
                  READ TABLE <fs_systems>-steps ASSIGNING <fs_steps> INDEX 1.
                  IF sy-subrc = 0.

                    READ TABLE <fs_steps>-actions ASSIGNING <fs_actions> INDEX 1.
                    IF sy-subrc = 0.
                      "transport date and time of transmission to the latest target system
                      wa_deptr-date = <fs_actions>-date.
                      wa_deptr-time = <fs_actions>-time.

                      "with what rc code transport has been transported
                      IF <fs_systems>-rc = 0.
                        wa_deptr-status = 'Successfully Transported with RC 0'.
                      ELSEIF <fs_systems>-rc = 4.
                        wa_deptr-status = 'Successfully Transported with RC 4 (Warning)'.
                      ELSEIF <fs_systems>-rc = 8.
                        wa_deptr-status = 'Transported with RC 8 (Error)'.
                      ELSEIF <fs_systems>-rc = 12.
                        wa_deptr-status = 'Transported with RC 12 (Import Error)'.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ELSE.
                  "if no latest target system "read latest target system
                  "get transport status from source system
                  wa_deptr-tarsystem = wa_e070-tarsystem.
                  wa_deptr-date = wa_e070-as4date.
                  wa_deptr-time = wa_e070-as4time.

                  "Transport status check in the source system
                  IF wa_e070-trstatus EQ 'D' OR
                   wa_e070-trstatus EQ 'L'.
                    IF lv_strkorr IS NOT INITIAL.
                      wa_deptr-status = 'Tranport Modifiable'.
                    ELSE.
                      wa_deptr-status = 'Tranport released'.
                    ENDIF.
                  ELSE.
                    wa_deptr-status = 'Tranport released'.
                  ENDIF.
                ENDIF.
              ELSE.
                "if transport doesn't exists in the tranport import log
                ""Transport statsu check in the source system
                IF wa_e070-trstatus EQ 'D' OR
                 wa_e070-trstatus EQ 'L'.
                  IF lv_strkorr IS NOT INITIAL.
                    wa_deptr-status = 'Tranport Modifiable'.
                  ELSE.
                    wa_deptr-status = 'Tranport released'.
                  ENDIF.
                ELSE.
                  wa_deptr-status = 'Tranport released'.
                ENDIF.
              ENDIF.
*              ENDIF."30/5/2018

              "Dependency status comment
              IF <fs_requests> IS ASSIGNED.
                READ TABLE <fs_requests>-cofile-systems TRANSPORTING NO FIELDS WITH KEY systemid = p_system .
                IF sy-subrc = 0.
                  wa_deptr-comments = 'OK...Dependent TR already Exists in the Target System'.
                  wa_deptr-icon = '3'.
                ELSEIF wa_e070-trfunction = 'T'.
                  wa_deptr-comments = 'OK...Generated Test Transport'.
                  wa_deptr-icon = '3'.
                ELSEIF wa_e070-strkorr = wa_e071-trkorr.
                  wa_deptr-comments = 'OK...Higher level request of the entered TR'.
                  wa_deptr-icon = '3'.
                ELSE.
*              30/5/2018
                  IF wa_deptr-dep_tr IS INITIAL.
                    wa_deptr-comments = 'No Dependent TR exists'.
                    wa_deptr-icon = '3'.
                  ELSE.
*             30/5/2018
                    wa_deptr-comments = 'NOK...Dependent TR does not Exists in the Target System'.
                    wa_deptr-icon = '1'.
                  ENDIF."30/5/2018
                ENDIF.
              ENDIF.


              APPEND wa_deptr TO t_deptr.
              CLEAR wa_deptr.
            ENDIF.

          ENDIF.
        ENDIF.
        "ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " DEPENDECY_CHECK

*&---------------------------------------------------------------------*
*&      fORM  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       tOP OF THE alv WITH DATE TIME AND FILEPATH
*----------------------------------------------------------------------*
*FORM top_of_page.
*  DATA:
*    lv_string(1000) TYPE c,
*    lv_lines        TYPE i,
*    lv_linesc(10)   TYPE c.
*  IF i_conflict IS NOT INITIAL.
*    FORMAT INTENSIFIED ON.
*    WRITE: /1(135) text-h01 CENTERED.
*    SKIP 1.
*    WRITE : /1(15) text-h02, 20 sy-datum, '/', sy-uzeit.
*
*    "Total No. OF Records Selected
*    DESCRIBE TABLE i_conflict LINES lv_lines.
*    lv_linesc = lv_lines.
*    CONCATENATE text-h03 lv_linesc
*                      INTO lv_string." SEPARATED BY space.
*    WRITE:80 lv_string.
*    CLEAR lv_string.
*    WRITE :/1(15) text-h08, 20 sy-sysid.
*    WRITE :90 text-h09, 111 p_system.
*    IF s_req IS NOT INITIAL.
*      WRITE :/1(15) text-h18, 20 s_req-low.
*      WRITE :90 text-h19, 111 sy-uname.
*    ELSEIF s_smrtfm IS NOT INITIAL.
*      WRITE :/1(15) text-h20, 20 s_smrtfm-low.
*      WRITE :90 text-h19, 111 sy-uname.
*    ELSEIF s_table IS NOT INITIAL.
*      WRITE :/1(15) text-h20, 20 s_table-low.
*      WRITE :90 text-h19, 111 sy-uname.
*    ELSEIF s_domain IS NOT INITIAL.
*      WRITE :/1(15) text-h20, 20 s_domain-low.
*      WRITE :90 text-h19, 111 sy-uname.
*    ELSEIF s_fugp IS NOT INITIAL.
*      WRITE :/1(15) text-h20, 20 s_fugp-low.
*      WRITE :90 text-h19, 111 sy-uname.
*    ELSEIF s_func IS NOT INITIAL.
*      WRITE :/1(15) text-h20, 20 s_func-low.
*      WRITE :90 text-h19, 111 sy-uname.
*    ENDIF.
*    SKIP.
*
*  ELSEIF i_deptr IS NOT INITIAL.
*    FORMAT INTENSIFIED ON.
*    WRITE: /1(135) text-h04 CENTERED.
*    SKIP 1.
*    WRITE : /1(15) text-h02, 20 sy-datum, '/', sy-uzeit.
*
*    "Total No. OF Records Selected
*    DESCRIBE TABLE i_deptr LINES lv_lines.
*    lv_linesc = lv_lines.
*    CONCATENATE text-h03 lv_linesc
*                      INTO lv_string." SEPARATED BY space.
*    WRITE:120 lv_string.
*    CLEAR lv_string.
*
*    WRITE :/1(15) text-h08, 20 sy-sysid.
*    WRITE :130 text-h09, 152 p_system.
*    WRITE :/1(15) text-h18, 20 s_req-low.
*    WRITE :130 text-h19, 152 sy-uname.
*    SKIP.
*  ELSEIF i_missing IS NOT INITIAL.
*    FORMAT INTENSIFIED ON.
*    WRITE: /1(135) text-h05 CENTERED.
*    SKIP 1.
*    WRITE : /1(15) text-h02,  20 sy-datum, '/', sy-uzeit.
*
*    "Total No. OF Records Selected
*    DESCRIBE TABLE i_missing LINES lv_lines.
*    lv_linesc = lv_lines.
*    CONCATENATE text-h03 lv_linesc
*                      INTO lv_string ." SEPARATED BY space.
*    WRITE:110 lv_string.
*    CLEAR lv_string.
*
*    WRITE :/1(15) text-h08, 20 sy-sysid.
*    WRITE :120 text-h09, 143 p_system.
*    WRITE :/1(15) text-h18, 20 s_req-low.
*    WRITE :120 text-h19, 141 sy-uname.
*    SKIP.
*  ENDIF.
*
*ENDFORM.                    "TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv .

  IF rd_conf IS NOT INITIAL.
    "Diplay Conflicts---------------------------"
    PERFORM display_conflicts.
  ELSEIF rd_dep IS NOT INITIAL.
    "Diplay Dependency--------------------------"
    PERFORM display_dependecy.
  ELSEIF rd_miss IS NOT INITIAL.
    "Diplay Missing Objects---------------------"
    PERFORM display_missing_objects.
  ENDIF.


ENDFORM.                    " DISPLAY_ALV

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_CONFLICTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_conflicts .


  "TYPE-POOLS: slis.

  DATA: t_fldcat      TYPE slis_t_fieldcat_alv.

  DATA:
    " lt_layout           TYPE TABLE OF slis_layout_alv,
    lwa_layout TYPE          slis_layout_alv,
    lt_event   TYPE TABLE OF slis_alv_event,
    lwa_event  TYPE          slis_alv_event.


  CONSTANTS :
    lc_top(11) TYPE c VALUE 'TOP_OF_PAGE',
    lc_x(1)    TYPE c VALUE 'X'.


  "field catalog--------------------------------------------------------------------"

  PERFORM fill_fieldcat USING 'TRKORR' 'Request' CHANGING t_fldcat."5/6/2018 Removed /Task
  "PERFORM fill_fieldcat USING 'TRFUNCTION' 'Type' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'TRSTATUS' 'Status' CHANGING t_fldcat.
  "PERFORM fill_fieldcat USING 'TARSYSTEM' 'Target System' CHANGING t_fldcat.
  "PERFORM fill_fieldcat USING 'KORRDEV' 'Category' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'AS4USER' 'User' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'AS4DATE' 'Date' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'AS4TIME' 'Time' CHANGING t_fldcat.
  "PERFORM fill_fieldcat USING 'AS4TEXT' 'Request Description' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'PGMID' 'Program ID' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'OBJECT' 'Object' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'OBJ_NAME' 'Object Name' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'COMMENTS' 'Comments' CHANGING t_fldcat.


  DATA: s_fieldcat LIKE LINE OF t_fldcat.
  s_fieldcat-hotspot = 'X'.

  MODIFY t_fldcat FROM s_fieldcat TRANSPORTING hotspot
  WHERE fieldname = 'TRKORR'.

  lwa_layout-lights_fieldname = 'ICON'.
  lwa_layout-colwidth_optimize = lc_x.
  lwa_layout-zebra             = lc_x.
  lwa_event-name               = lc_top.
  lwa_event-form               = lc_top.
  APPEND lwa_event TO lt_event.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_callback_program      = sy-repid
      is_layout               = lwa_layout
      it_fieldcat             = t_fldcat
      i_callback_user_command = 'USER_COMMAND'
      i_save                  = 'A'
      it_events               = lt_event
    TABLES
      t_outtab                = t_conflict
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.

  IF sy-subrc <> 0.
    MESSAGE e002(ztr).
  ENDIF.


ENDFORM.                    " DISPLAY_CONFLICTS

*&---------------------------------------------------------------------*
*&      Form  FILL_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3444   text
*      -->P_3445   text
*      <--P_I_FLDCAT  text
*----------------------------------------------------------------------*
*FORM fill_fieldcat USING text1 TYPE slis_fieldname text2 TYPE slis_fieldname  CHANGING i_fldcat TYPE STANDARD TABLE.
*  DATA: "i_fldcat  type slis_t_fieldcat_alv,
*     wa_fldcat  TYPE slis_fieldcat_alv.
*  wa_fldcat-fieldname = text1.
*  wa_fldcat-seltext_m = text2.
*  APPEND wa_fldcat TO i_fldcat.
*  CLEAR wa_fldcat.
*ENDFORM.                    "fill_fieldcat

*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UCOMM      text
*      -->SELFIELD   text
*----------------------------------------------------------------------*
*FORM user_command USING ucomm LIKE sy-ucomm
*selfield TYPE slis_selfield.
*
*  IF ucomm = '&IC1'.
*
*    IF i_conflict IS NOT INITIAL.
*      CLEAR wa_final.
*      READ TABLE i_conflict INTO wa_final INDEX selfield-tabindex.
*      IF sy-subrc = 0.
*        "SET PARAMETER ID 'DFD' FIELD wa_final-trkorr.
*        CALL TRANSACTION 'SE01' AND SKIP FIRST SCREEN.
*      ENDIF.
*    ELSEIF i_deptr IS NOT INITIAL.
*      CLEAR wa_final.
*      READ TABLE i_deptr INTO wa_final INDEX selfield-tabindex.
*      IF sy-subrc = 0.
*        "SET PARAMETER ID 'DFD' FIELD wa_final-trkorr.
*        CALL TRANSACTION 'SE01' AND SKIP FIRST SCREEN.
*      ENDIF.
*    ELSEIF i_missing IS NOT INITIAL.
*      CLEAR wa_final.
*      READ TABLE i_deptr INTO wa_final INDEX selfield-tabindex.
*      IF sy-subrc = 0.
*        "SET PARAMETER ID 'DFD' FIELD wa_final-trkorr.
*        CALL TRANSACTION 'SE01' AND SKIP FIRST SCREEN.
*      ENDIF.
*
*    ENDIF.
*
*  ENDIF.
*
*ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DEPENDECY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_dependecy .

  DATA: t_fldcat      TYPE slis_t_fieldcat_alv.


  DATA:
    " li_layout           TYPE TABLE OF slis_layout_alv,
    lwa_layout TYPE          slis_layout_alv,
    lt_event   TYPE TABLE OF slis_alv_event,
    lwa_event  TYPE          slis_alv_event.

  CONSTANTS :
    lc_top(11) TYPE c VALUE 'TOP_OF_PAGE',
    lc_x(1)    TYPE c VALUE 'X'.


  "field catalog--------------------------------------------------------------------"

  PERFORM fill_fieldcat USING 'TRKORR' 'Request/Task' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'OBJ_NAME' 'Object Name' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'OBJECT' 'Object' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'BLANK' '   ' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'DEP_TR' 'Dependent Request' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'TRSTATUS' 'Status' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'TARSYSTEM' 'Latest Target System' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'DATE' 'Transport Date' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'TIME' 'Transport Time' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'STATUS' 'Transport Status' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'COMMENTS' 'Dependency Status' CHANGING t_fldcat.


  DATA: s_fieldcat LIKE LINE OF t_fldcat.
  s_fieldcat-hotspot = 'X'.

  MODIFY t_fldcat FROM s_fieldcat TRANSPORTING hotspot
  WHERE fieldname = 'TRKORR'.

  lwa_layout-lights_fieldname = 'ICON'.
  lwa_layout-colwidth_optimize = lc_x.
  lwa_layout-zebra             = lc_x.
  lwa_event-name               = lc_top.
  lwa_event-form               = lc_top.
  APPEND lwa_event TO lt_event.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_callback_program      = sy-repid
      is_layout               = lwa_layout
      it_fieldcat             = t_fldcat
      i_callback_user_command = 'USER_COMMAND'
      i_save                  = 'A'
      it_events               = lt_event
    TABLES
      t_outtab                = t_deptr
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.

  IF sy-subrc <> 0.
    MESSAGE e002(ztr).
  ENDIF.

ENDFORM.                    " DISPLAY_DEPENDECY
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MISSING_OBJECTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_missing_objects .

  DATA: t_fldcat      TYPE slis_t_fieldcat_alv.

  DATA:
    " li_layout           TYPE TABLE OF slis_layout_alv,
    lwa_layout TYPE          slis_layout_alv,
    lt_event   TYPE TABLE OF slis_alv_event,
    lwa_event  TYPE          slis_alv_event.

  CONSTANTS :
    lc_top(11) TYPE c VALUE 'TOP_OF_PAGE',
    lc_x(1)    TYPE c VALUE 'X'.


  "field catalog--------------------------------------------------------------------"

  PERFORM fill_fieldcat USING 'TYPE' 'Object Type' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'OBJECT' 'Object' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'CALL_OBJ' 'Call Object  ' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'CALL_TYPE' 'CALL Type' CHANGING t_fldcat.
  PERFORM fill_fieldcat USING 'COMMENTS' 'Comments' CHANGING t_fldcat.


  DATA: s_fieldcat LIKE LINE OF t_fldcat.
  s_fieldcat-hotspot = 'X'.

  MODIFY t_fldcat FROM s_fieldcat TRANSPORTING hotspot
  WHERE fieldname = 'TRKORR'.

  lwa_layout-lights_fieldname = 'ICON'.
  lwa_layout-colwidth_optimize = lc_x.
  lwa_layout-zebra             = lc_x.
  lwa_event-name               = lc_top.
  lwa_event-form               = lc_top.
  APPEND lwa_event TO lt_event.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_callback_program      = sy-repid
      is_layout               = lwa_layout
      it_fieldcat             = t_fldcat
      i_callback_user_command = 'USER_COMMAND'
      i_save                  = 'A'
      it_events               = lt_event
    TABLES
      t_outtab                = t_missing
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.

  IF sy-subrc <> 0.
    MESSAGE e002(ztr).
  ENDIF.

ENDFORM.                    " DISPLAY_MISSING_OBJECTS
*&---------------------------------------------------------------------*
*&      Form  SMARTFORM_PROGRAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM smartform_program .
*
*
*  "---------------Local Data Declarations-------------------------------"
*
*  CONSTANTS : lc_prog(4) TYPE c VALUE 'PROG',
*             "lc_ssfo(4) TYPE c VALUE 'SSFO',
*              lc_1    TYPE i VALUE '1'.
*
*  DATA:       lv_formname      TYPE tdsfname,
*              lv_fname         TYPE  rs38l_fnam,
*              lv_obj_type      TYPE euobj-id VALUE 'P'.
*  "i_rsfind      TYPE STANDARD TABLE OF rsfind.
*
*  "-------------Get Function Module Name of the Smartforms---------------"
*  IF s_smrtfm IS NOT INITIAL.
**Moving Smart form name to get Function Module name
*    MOVE s_smrtfm-low TO lv_formname.
*    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*      EXPORTING
*        formname           = lv_formname
*      IMPORTING
*        fm_name            = lv_fname
*      EXCEPTIONS
*        no_form            = 1
*        no_function_module = 2
*        OTHERS             = 3.
*    IF sy-subrc <> 0.
*      MESSAGE e020(ztr) WITH lv_formname." text-034.
*      EXIT.
*    ENDIF.
*
*    "----------------Get Program Name of the Smartforms---------------------"
**From Function Module fetch the Program name
*    REFRESH s_report.
*    SELECT SINGLE pname FROM tfdir INTO v_pname WHERE
*                        funcname = lv_fname.
*    IF sy-subrc = 0.
*      s_report-option = 'EQ'.
*      s_report-sign = 'I'.
*      s_report-low =  v_pname .
*      APPEND s_report.
*    ENDIF.
*
*
*    "------------Get Custom Objects Created in the Smartforms---------------"
*    MOVE v_pname TO pa_prog.
*    MOVE lc_prog TO lv_obj_type.
*    "Pass the object type and object name to get z/y objects used
*    IF NOT lv_obj_type IS INITIAL AND pa_prog IS NOT INITIAL.
*      LOOP AT s_report.
*        pa_prog = s_report-low.
*
*        CALL FUNCTION 'REPOSITORY_ENVIRONMENT_ALL'
*          EXPORTING
*            obj_type          = lv_obj_type
*            environment_types = t_environment_selection
*            object_name       = pa_prog
*            deep              = lc_1
*          TABLES
*            environment_tab   = t_senvi_tab1
*            source_objects    = t_rsfind1.
*
*        CLEAR wa_senvi_tab.
*        "--------Get All program Includes---------------"
*        AT FIRST.
*          LOOP AT t_senvi_tab1 INTO wa_senvi_tab.
*            IF  wa_senvi_tab-type = 'INCL'.
*              s_report-option = 'EQ'.
*              s_report-sign = 'I'.
*              s_report-low =  wa_senvi_tab-object .
*              APPEND s_report.
*              CLEAR wa_senvi_tab.
*            ENDIF.
*          ENDLOOP.
*        ENDAT.
*
*        APPEND LINES OF t_senvi_tab1 TO t_senvi_tab.
*        APPEND LINES OF t_rsfind1 TO t_rsfind.
*        REFRESH: t_senvi_tab1, t_rsfind1.
*      ENDLOOP.
*    ENDIF.
*
*    "------------Check for Custom Objects Created in the Smartforms----------------"
*    "Checking for z/y objects
*    LOOP AT t_senvi_tab INTO wa_senvi_tab.
*      IF wa_senvi_tab-object+0(1) = 'Z' OR
*         wa_senvi_tab-object+0(1) = 'Y' OR
*         wa_senvi_tab-object+0(2) = 'LZ' OR
*       ( wa_senvi_tab-object+4(1) = 'Z' AND
*         wa_senvi_tab-object+0(1) = 'S' ).
*        wa_senvi_tab-genflag = 'D'.
*        MODIFY t_senvi_tab FROM wa_senvi_tab.
*        CLEAR wa_senvi_tab.
*      ENDIF.
*    ENDLOOP.
*    DELETE t_senvi_tab WHERE genflag <> 'D'.
*
*    "-----------Find Function Group for function module and-------------------------"
*    "-----------get all objects used in it(Data element,domain, search helps etc)---"
*    PERFORM function_modules.
*
*    "-------------Get repository Objects--------------------------------------------"
*    PERFORM get_from_tadir.
*
*    "-------------Delete repository Objects by packege $TMP-------------------------"
*    DELETE t_tadir WHERE devclass EQ '$TMP'.
*
*  ENDIF.
*
*  "=================================================================================="
*  "---Get TR list for deep custom objects like domain, data elements, search helps---"
*  "=================================================================================="
*  "Objcets list used in a TR------------------------------"
*  IF t_tadir IS NOT INITIAL.
*
*    CLEAR wa_tadir.
*    LOOP AT t_tadir INTO wa_tadir.
*      wa_curr_temp-object = wa_tadir-object.
*      wa_curr_temp-obj_name = wa_tadir-obj_name.
*      wa_curr_temp-devclass = wa_tadir-devclass.
*      APPEND wa_curr_temp TO t_curr_temp.
*      CLEAR wa_curr_temp.
*    ENDLOOP.
*
*    SORT t_curr_temp BY object obj_name.
*    DELETE ADJACENT DUPLICATES FROM t_curr_temp COMPARING object obj_name.
*
*    "All the TR's of individual objects---------------------"
*    IF t_curr_temp IS NOT INITIAL.
*      SELECT trkorr
*          pgmid
*          object
*          obj_name
*     FROM e071
*     INTO TABLE t_e071_t
*     FOR ALL ENTRIES IN t_curr_temp
*     WHERE pgmid IN r_pgmid AND
*          "object IN r_object AND
*        obj_name EQ t_curr_temp-obj_name.
*      IF sy-subrc = 0.
*        SORT t_e071_t BY trkorr obj_name.
*        DELETE ADJACENT DUPLICATES FROM t_e071_t COMPARING trkorr obj_name.
*        DELETE t_e071_t WHERE obj_name(1) NE 'Z'.
*      ENDIF.
*    ENDIF.
*
*    "All the TR's status-----------------------------------"
*    IF t_e071_t IS NOT INITIAL.
*      SELECT trkorr
*             trfunction
*             trstatus
*             tarsystem
*             korrdev
*             as4user
*             as4date
*             as4time
*             strkorr
*            FROM e070
*        INTO TABLE t_e070_t
*        FOR ALL ENTRIES IN t_e071_t
*        WHERE trkorr EQ t_e071_t-trkorr.
*      IF sy-subrc = 0.
*        SORT t_e070_t BY trkorr.
*      ENDIF.
*
*      "Get TR description------------------------------------"
*      SELECT trkorr
*               as4text
*         FROM e07t
*         INTO TABLE t_e07t_t
*         FOR ALL ENTRIES IN t_e071_t
*         WHERE trkorr EQ t_e071_t-trkorr.
*      IF sy-subrc = 0.
*        SORT t_e07t_t BY trkorr.
*      ENDIF.
*    ENDIF.
*
*    "---------------Deep objects details----------------------------------------"
*    CLEAR wa_e071.
*    CLEAR wa_final.
*    LOOP AT t_e071_t INTO wa_e071.
*      wa_final-trkorr = wa_e071-trkorr.
*      CLEAR wa_e070.
*      "Read TR status---------------------------"
*      READ TABLE t_e070_t INTO wa_e070 WITH KEY trkorr = wa_e071-trkorr BINARY SEARCH.
*      IF sy-subrc = 0.
*        wa_final-trfunction = wa_e070-trfunction.
*        wa_final-trstatus = wa_e070-trstatus.
*        wa_final-tarsystem = wa_e070-tarsystem.
*        wa_final-korrdev = wa_e070-korrdev.
*        wa_final-as4user = wa_e070-as4user.
*        wa_final-as4date = wa_e070-as4date.
*        wa_final-as4time = wa_e070-as4time.
*      ENDIF.
*      CLEAR wa_e07t.
*      "Read TR Description----------------------"
*      READ TABLE t_e07t_t INTO wa_e07t WITH KEY trkorr = wa_e071-trkorr BINARY SEARCH.
*      IF sy-subrc = 0.
*        wa_final-as4text = wa_e07t-as4text.
*      ENDIF.
*
*      wa_final-pgmid = wa_e071-pgmid.
*      wa_final-object = wa_e071-object.
*      wa_final-obj_name = wa_e071-obj_name.
*      APPEND wa_final TO t_current.
*
*      CLEAR wa_final.
*    ENDLOOP.
*
*    "* Sort it to get the latest TR no based on date and time on any object
*    SORT t_current BY obj_name as4date DESCENDING as4time DESCENDING.
*
*    IF t_e071_t IS INITIAL OR t_current IS INITIAL.
*      MESSAGE s001(ztr) DISPLAY LIKE 'E'.
*      LEAVE LIST-PROCESSING.
*    ENDIF.
*  ENDIF.
*
*  "------------------Final Table biuld for all the objects & TR's-------------------"
*  "Final internal table having all the objects(including data elements , domains etc) and there TR's
*  "if thse TR's exists in target sytem then no conflicts exists.(have to check target system using RFC)
*  IF t_current IS NOT INITIAL.
*    APPEND LINES OF t_e071_t TO t_e071.
*    SORT t_e071 BY trkorr object obj_name.
*    DELETE ADJACENT DUPLICATES FROM t_e071 COMPARING trkorr object obj_name.
*
*    APPEND LINES OF t_e070_t TO t_e070.
*    SORT t_e070 BY trkorr .
*    DELETE ADJACENT DUPLICATES FROM t_e070 COMPARING trkorr .
*
*    APPEND LINES OF t_current TO t_present.
*    SORT t_present BY trkorr object obj_name.
*    DELETE ADJACENT DUPLICATES FROM t_present COMPARING trkorr object obj_name.
*  ENDIF.
*
*
*ENDFORM.                    " SMARTFORM_PROGRAM
*&---------------------------------------------------------------------*
*&      Form  CHECK_OBJ_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->p_r_object  text
*----------------------------------------------------------------------*
FORM check_obj_type  TABLES   p_r_object LIKE r_check.

  "-----------------------------------------------constants
  CONSTANTS:   lc_reps(4) TYPE c VALUE 'REPS',
               lc_repo(4) TYPE c VALUE 'REPO',
               lc_rept(4) TYPE c VALUE 'REPT',
               lc_prog(4) TYPE c VALUE 'PROG',
               lc_tabl(4) TYPE c VALUE 'TABL',
               lc_tabu(4) TYPE c VALUE 'TABU',
               lc_tobj(4) TYPE c VALUE 'TOBJ',
               "lc_vied(4) TYPE c VALUE 'VIED',
               "lc_viet(4) TYPE c VALUE 'VIET',
               "lc_view(4) TYPE c VALUE 'VIEW',
               "lc_dtel(4) TYPE c VALUE 'DTEL',
               "lc_dted(4) TYPE c VALUE 'DTED',
               "lc_type(4) TYPE c VALUE 'TYPE',
               "lc_typd(4) TYPE c VALUE 'TYPD',
               lc_doma(4) TYPE c VALUE 'DOMA',
               lc_domd(4) TYPE c VALUE 'DOMD',
               "lc_shld(4) TYPE c VALUE 'SHLD',
               "lc_shlx(4) TYPE c VALUE 'SHLX',
               "lc_shlp(4) TYPE c VALUE 'SHLP',
               "lc_enqd(4) TYPE c VALUE 'ENQD',
               "lc_enqu(4) TYPE c VALUE 'ENQU',
               lc_func(4) TYPE c VALUE 'FUNC',
               lc_fugt(4) TYPE c VALUE 'FUGT',
               lc_fugr(4) TYPE c VALUE 'FUGR',
               lc_fugs(4) TYPE c VALUE 'FUGS',
               lc_fugx(4) TYPE c VALUE 'FUGX',
               lc_ssfo(4) TYPE c VALUE 'SSFO',
               lc_tabd(4) TYPE c VALUE 'TABD',
               lc_vdat(4) TYPE c VALUE 'VDAT'.

  "---------------------------------------------------------"
  REFRESH p_r_object.

  IF rd_rep IS NOT INITIAL.       "--------------------Report
    PERFORM fill_type TABLES p_r_object USING lc_reps.
    PERFORM fill_type TABLES p_r_object USING lc_repo.
    PERFORM fill_type TABLES p_r_object USING lc_rept.
    PERFORM fill_type TABLES p_r_object USING lc_prog.

  ELSEIF rd_tbl IS NOT INITIAL.  "---------------------Table
    PERFORM fill_type TABLES p_r_object USING lc_tabu.
    PERFORM fill_type TABLES p_r_object USING lc_tabl.
    PERFORM fill_type TABLES p_r_object USING lc_tobj.
    PERFORM fill_type TABLES p_r_object USING lc_tabd.
    PERFORM fill_type TABLES p_r_object USING lc_vdat.

  ELSEIF rd_dmn IS NOT INITIAL.  "---------------------Domain
    PERFORM fill_type TABLES p_r_object USING lc_doma.
    PERFORM fill_type TABLES p_r_object USING lc_domd.

  ELSEIF rd_func IS NOT INITIAL. "---------------------Function Module
    PERFORM fill_type TABLES p_r_object USING lc_func.

  ELSEIF rd_fugp IS NOT INITIAL. "--------------------Function Group
    PERFORM fill_type TABLES p_r_object USING lc_fugt.
    PERFORM fill_type TABLES p_r_object USING lc_fugr.
    PERFORM fill_type TABLES p_r_object USING lc_fugs.
    PERFORM fill_type TABLES p_r_object USING lc_fugx.

*  ELSEIF rd_smrt IS NOT INITIAL. "--------------------Smartforms
*    PERFORM fill_type TABLES p_r_object USING lc_ssfo.

  ENDIF.

ENDFORM.                    " CHECK_OBJ_TYPE
*&---------------------------------------------------------------------*
*&      Form  FILL_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_p_r_object  text
*      -->P_LC_REPS  text
*----------------------------------------------------------------------*
FORM fill_type  TABLES   tp_p_r_object USING    ip_lc_reps.
  DATA lwa_check LIKE LINE OF r_check.
  lwa_check-sign = 'I'.
  lwa_check-option = 'EQ'.
  lwa_check-low = ip_lc_reps.
  APPEND lwa_check TO tp_p_r_object.
  CLEAR lwa_check.

ENDFORM.                    " FILL_TYPE
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_SMARTFORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM validate_smartform .
*  DATA: lv_form TYPE stxfadm-formname.
*
*  SELECT SINGLE formname
*    FROM stxfadm
*    INTO lv_form "UP TO 1 ROWS
*    WHERE formname IN s_smrtfm.
*  IF sy-subrc <> 0.
*    MESSAGE e005(ztr).
*  ENDIF.
*
*ENDFORM.                    " VALIDATE_SMARTFORM
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_table .
  DATA: lv_table TYPE dd02l-tabname.

  SELECT SINGLE tabname
    FROM dd02l
    INTO lv_table "UP TO 1 ROWS
    WHERE tabname IN s_table.
  IF sy-subrc <> 0.
    MESSAGE e006(ztr).
  ENDIF.

ENDFORM.                    " VALIDATE_TABLE
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_DOMAIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_domain .

  DATA: lv_domain TYPE dd01l-domname.

  SELECT SINGLE domname
    FROM dd01l
    INTO lv_domain "UP TO 1 ROWS
    WHERE domname IN s_domain.
  IF sy-subrc <> 0.
    MESSAGE e007(ztr).
  ENDIF.

ENDFORM.                    " VALIDATE_DOMAIN
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_FUNCTION_GRP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_function_grp .

  DATA: lv_fugp TYPE tlibg-area.

  SELECT SINGLE area
    FROM tlibg
    INTO lv_fugp
    WHERE area IN s_fugp.
  IF sy-subrc <> 0.
    MESSAGE e008(ztr).
  ENDIF.

ENDFORM.                    " VALIDATE_FUNCTION_GRP
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_FUNCTION_MODULE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_function_module .

  DATA: lv_func TYPE tfdir-funcname.

  SELECT SINGLE funcname
    FROM tfdir
    INTO lv_func
    WHERE funcname IN s_func.
  IF sy-subrc <> 0.
    MESSAGE e009(ztr).
  ENDIF.

ENDFORM.                    " VALIDATE_FUNCTION_MODULE
*&---------------------------------------------------------------------*
*&      Form  REFRESH_TABLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_tables .
  REFRESH : r_check,t_e071_2,t_e07t,t_final,t_created,t_output_list,
  t_source_searched,t_re071,t_re070,t_remote1,t_remote, r_field,
  t_itab1,t_enlfdir,t_dd03l,t_dd04ll,t_dd04l,t_itab_dup,t_output_list1,
  t_e071_t,t_e070_t,t_e07t_t,t_e071_r,t_e070_r,t_e071tr_r,t_e070_1p,
  t_e070_2p,t_e070a_p,t_e07t_r.

  CLEAR:wa_created,wa_remote,wa_present,wa_conflict,wa_field,wa_remote1,
     wa_missing,wa_enlfdir,wa_dd04ll,wa_e071_1,wa_e070_1,wa_deptr,v_pname,
     wa_dd04l,wa_envi_types,v_seoclstype,wa_dd03l,t_lines,t_text,
    wa_ctsproj, wa_e070_p,wa_trlist,wa_e07t_p,wa_e071_p,wa_e070_r,wa_e071_r,
    wa_e07t_r,wa_e070_1.

ENDFORM.                    " REFRESH_TABLES
*&---------------------------------------------------------------------*
*&      Form  CHECK_OBJ_TYPE_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_R_OBJECT  text
*----------------------------------------------------------------------*
FORM check_obj_type_all  TABLES   tp_r_object LIKE r_object.

  "---------------------------------------------------------"
  REFRESH tp_r_object.

  PERFORM fill_type TABLES tp_r_object USING 'ADIR'.
  PERFORM fill_type TABLES tp_r_object USING 'DEVC'.
  "--------------------Report
  PERFORM fill_type TABLES tp_r_object USING 'REPS'.
  PERFORM fill_type TABLES tp_r_object USING 'REPO'.
  PERFORM fill_type TABLES tp_r_object USING 'REPT'.
  PERFORM fill_type TABLES tp_r_object USING 'PROG'.
  "---------------------Table
  PERFORM fill_type TABLES tp_r_object USING 'TABU'.
  PERFORM fill_type TABLES tp_r_object USING 'TABL'.
  PERFORM fill_type TABLES tp_r_object USING 'TOBJ'.
  PERFORM fill_type TABLES tp_r_object USING 'TABD'.
  PERFORM fill_type TABLES tp_r_object USING 'VDAT'.
  "---------------------Domain
  PERFORM fill_type TABLES tp_r_object USING 'DOMA'.
  PERFORM fill_type TABLES tp_r_object USING 'DOMD'.
  "---------------------Function Module
  PERFORM fill_type TABLES tp_r_object USING 'FUNC'.
  "--------------------Function Group
  PERFORM fill_type TABLES tp_r_object USING 'FUGT'.
  PERFORM fill_type TABLES tp_r_object USING 'FUGR'.
  PERFORM fill_type TABLES tp_r_object USING 'FUGS'.
  PERFORM fill_type TABLES tp_r_object USING 'FUGX'.
  "--------------------Smartforms
  PERFORM fill_type TABLES tp_r_object USING 'SSFO'.
  "---------------------LOGICAL DATABASE
  PERFORM fill_type TABLES tp_r_object USING 'LDBA'.
  "-----------------------MESSAGE CLASS
  PERFORM fill_type TABLES tp_r_object USING 'MSAG'.
  "-----------------------"TRANSACTION CODE
  PERFORM fill_type TABLES tp_r_object USING 'TRAN'.
  "-----------------------dIALOG MODULE
  PERFORM fill_type TABLES tp_r_object USING 'DIAL'.
  "---------------------sEARCH hELP
  PERFORM fill_type TABLES tp_r_object USING 'SHLP'.
  "--------------------dATA ELEMENT
  PERFORM fill_type TABLES tp_r_object USING 'DTEL'.
  "-----------------------"AUTH GROUPS
  PERFORM fill_type TABLES tp_r_object USING 'SUSO'.
  "-----------------------TYPE GROUPS
  PERFORM fill_type TABLES tp_r_object USING 'TYPE'.
  "-------------------------TABLE TYPE
  PERFORM fill_type TABLES tp_r_object USING 'TTYP'.
  "---------------------------STRUCTURE
  PERFORM fill_type TABLES tp_r_object USING 'STRU'.
  "---------------------------LOCK OBJCETS
  PERFORM fill_type TABLES tp_r_object USING 'ENQU'.
  "------------------------------CLASS
  PERFORM fill_type TABLES tp_r_object USING 'CLAS'.
  "----------------------------INTERFACES
  PERFORM fill_type TABLES tp_r_object USING 'INTF'.
  "------------------------ "ENHANCEMENTS
  PERFORM fill_type TABLES tp_r_object USING 'ENHO'.
  PERFORM fill_type TABLES tp_r_object USING 'ENHC'.
  PERFORM fill_type TABLES tp_r_object USING 'ENHS'.
  PERFORM fill_type TABLES tp_r_object USING 'ENSC'.


ENDFORM.                    " CHECK_OBJ_TYPE_ALL
*&---------------------------------------------------------------------*
*&      Form  CHECK_PGMID_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_R_PGMID  text
*----------------------------------------------------------------------*
FORM check_pgmid_all  TABLES   tp_r_pgmid LIKE r_pgmid.

  "-----------------------------------------------constants
  CONSTANTS:   lc_r3tr(4) TYPE c VALUE 'R3TR',
               lc_limu(4) TYPE c VALUE 'LIMU'.

  "R3TR object PGM ID
  PERFORM fill_type TABLES tp_r_pgmid USING lc_r3tr.
  "LIMU object PGM ID
  PERFORM fill_type TABLES tp_r_pgmid USING lc_limu.

ENDFORM.                    " CHECK_PGMID_ALL
*&---------------------------------------------------------------------*
*&      Form  P_TARGET_SYSTEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p_target_system .
  "--------------------------Local Data Declarations--------------------"
  DATA: lv_obj_name  TYPE e071-obj_name,
        lv_obj_name1 TYPE e071-obj_name.

  REFRESH: t_remote1, t_remote,t_re071,t_re070,t_out_tab,t_conflict.

  "======================================================================="
  "-------------------Get Target System Details---------------------------"
  "======================================================================="
  IF t_e071_r IS NOT INITIAL.
    CLEAR: t_lines, wa_e071_r, t_options, t_out_tab .
    REFRESH: t_options, t_fields, t_out_tab.
* Get lines of internal table
    DESCRIBE TABLE t_e071_r LINES t_lines.
*Populating first line for selection criteria for fetching from target system
    t_text = '(' .
    APPEND t_text TO t_options.

    LOOP AT t_e071_r INTO wa_e071_r.
*      SPLIT wa_e071_r-obj_name AT space INTO lv_obj_name lv_obj_name1.
*      CLEAR wa_e071_r-obj_name.
*      wa_e071_r-obj_name = lv_obj_name.
      IF sy-tabix NE t_lines.
**Populating other line for selection criteria for fetching from target system
*        CONCATENATE 'OBJ_NAME = ''' wa_e071_r-obj_name ''' OR' INTO t_text.
*      ELSE.
**Populating last line for selection criteria for fetching from target system
*        CONCATENATE 'OBJ_NAME = ''' wa_e071_r-obj_name ''')' INTO t_text.
        CONCATENATE '(' ' PGMID = ''' wa_e071_r-pgmid ''' AND'
                         ' OBJECT = ''' wa_e071_r-object ''' AND' INTO t_text.
        APPEND t_text to t_options.
        CLEAR: t_text.

        CONCATENATE ' OBJ_NAME = ''' wa_e071_r-obj_name ''' )' INTO t_text.
        t_text1 = ' OR'.

      ELSE.
*Populating last line for selection criteria for fetching from target system
*        CONCATENATE '(' ' PGMID = ''' wa_e071-pgmid ''' AND '
*                         ' OBJECT = ''' wa_e071-object ''' AND '
*                         ' OBJ_NAME = ''' wa_e071-obj_name ''' )' ' )' INTO t_text.
        CONCATENATE '(' ' PGMID = ''' wa_e071_r-pgmid ''' AND '
                         ' OBJECT = ''' wa_e071_r-object ''' AND ' INTO t_text.
        APPEND t_text to t_options.
        clear t_text.

        CONCATENATE ' OBJ_NAME = ''' wa_e071_r-obj_name ''' )' ' )' INTO t_text.
      ENDIF.
      APPEND t_text TO t_options.
      IF t_text1 IS NOT INITIAL.
        APPEND t_text1 to t_options.
      ENDIF.
      CLEAR: t_text, t_text1.
    ENDLOOP.

    "-------------------------------------------------------------------------------"
    "--------------RFC Call to read E071 table from Target System-------------------"
    "-------------------------------------------------------------------------------"
    DATA: lwa_rfcdest TYPE rfcdes-rfcdest.
    "To check RFC Destination exists or not before read
    SELECT SINGLE rfcdest
           FROM rfcdes
           INTO lwa_rfcdest
           WHERE rfcdest = s_trgt-low.
    IF lwa_rfcdest IS NOT INITIAL. "RFC Destination exists or not
**calling RFC FM and fetching table Object Entries of Requests/Tasks (E071)
      CALL FUNCTION 'RFC_READ_TABLE' DESTINATION s_trgt-low
        EXPORTING
          query_table          = 'E071'
        TABLES
          options              = t_options
          fields               = t_fields
          data                 = t_out_tab
        EXCEPTIONS
          table_not_available  = 1
          table_without_data   = 2
          option_not_valid     = 3
          field_not_valid      = 4
          not_authorized       = 5
          data_buffer_exceeded = 6
          OTHERS               = 7.
      IF sy-subrc <> 0.
        MESSAGE s016(ztr) DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ELSE.
      MESSAGE s017(ztr) DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
    "-----------Formatting the output table received from the RFC FM--------"
    LOOP AT t_out_tab.
      wa_e071_r-trkorr   = t_out_tab+0(20).
      "wa_e071-as4pos   = i_out_tab+20(06).
      wa_e071_r-pgmid    = t_out_tab+26(04).
      wa_e071_r-object   = t_out_tab+30(04).
      wa_e071_r-obj_name = t_out_tab+34(120).
      APPEND wa_e071_r TO t_re071_r.
      CLEAR wa_e071_r .
    ENDLOOP.
  ENDIF.
  SORT t_re071_r BY trkorr.


  IF t_re071_r IS NOT INITIAL.
    CLEAR: t_lines, wa_e071_r, t_options.
* Get lines of internal table
    DESCRIBE TABLE t_re071_r LINES t_lines.
*Populating first line for selection criteria for fetching from target system
    t_text = '(' .
    APPEND t_text TO t_options1.

    LOOP AT t_re071_r INTO wa_e071_r.
      IF sy-tabix NE t_lines.
*Populating other line for selection criteria for fetching from target system
        CONCATENATE 'TRKORR = ''' wa_e071_r-trkorr ''' OR' INTO t_text. "Other lines
      ELSE.
*Populating last line for selection criteria for fetching from target system
        CONCATENATE 'TRKORR = ''' wa_e071_r-trkorr ''')' INTO t_text. "Last line
      ENDIF.
      APPEND t_text TO t_options1.
      CLEAR t_text.
    ENDLOOP.

    "-------------------------------------------------------------------------------"
    "--------------RFC Call to read E070 table from Target System-------------------"
    "-------------------------------------------------------------------------------"
    IF lwa_rfcdest IS NOT INITIAL.    "RFC Destination exists or not
* Calling RFC FM and fetching table Header of Requests/Tasks (E070)
      CALL FUNCTION 'RFC_READ_TABLE' DESTINATION s_trgt-low
        EXPORTING
          query_table          = 'E070'
        TABLES
          options              = t_options1
          fields               = t_fields1
          data                 = t_out_tab1
        EXCEPTIONS
          table_not_available  = 1
          table_without_data   = 2
          option_not_valid     = 3
          field_not_valid      = 4
          not_authorized       = 5
          data_buffer_exceeded = 6
          OTHERS               = 7.
      IF sy-subrc <> 0.
        MESSAGE s016(ztr) DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ELSE.
      MESSAGE s017(ztr) DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
* Formatting the output table received from the RFC FM
    LOOP AT t_out_tab1.
      wa_e070_r-trkorr     = t_out_tab1+0(19).
      wa_e070_r-trfunction = t_out_tab1+20(01).
      wa_e070_r-trstatus   = t_out_tab1+21(01).
      wa_e070_r-as4user    = t_out_tab1+36(12).
      wa_e070_r-as4date    = t_out_tab1+48(08).
      wa_e070_r-as4time    = t_out_tab1+56(06).
      APPEND wa_e070_r TO t_re070_r.
      CLEAR wa_e070_r .
    ENDLOOP.
  ENDIF.

  "-----------------------------------------------------------------------------------"
  "------------------Build Final Target System Object table---------------------------"
  "-----------------------------------------------------------------------------------"
* loop at the internal table containing all objects with all TR's
  SORT t_e070_r BY trkorr.
  LOOP AT t_re071_r INTO wa_e071_r.
* Get the corresponding date and time for the corresponding Object and TR no.
    READ TABLE t_re070_r INTO wa_e070_r WITH KEY trkorr = wa_e071_r-trkorr BINARY SEARCH.
    IF sy-subrc = 0.
* Move all entries to get all the objects with their TR no, Date and time
* in the correct format fetched from target system
      MOVE: wa_e071_r-trkorr   TO wa_remote-trkorr,
            "wa_e071-as4pos   TO wa_remote-as4pos,
            wa_e071_r-pgmid    TO wa_remote-pgmid,
            wa_e071_r-object   TO wa_remote-object,
            wa_e071_r-obj_name TO wa_remote-obj_name,
            wa_e070_r-trstatus TO wa_remote-trstatus,
            wa_e070_r-as4user  TO wa_remote-as4user,
            wa_e070_r-as4date  TO wa_remote-as4date,
            wa_e070_r-as4time  TO wa_remote-as4time.

      APPEND wa_remote TO t_remote.
      CLEAR  wa_remote.
    ENDIF.

  ENDLOOP.

  "Copy to remote1 table---------------------"
  SORT t_remote BY obj_name as4date DESCENDING as4time DESCENDING.
  t_remote1[] = t_remote[].

  SORT t_remote1 BY trkorr object obj_name.
  DELETE ADJACENT DUPLICATES FROM t_remote1 COMPARING trkorr object obj_name.

  SORT t_present BY trkorr object obj_name.
*   SORT t_present BY obj_name DESCENDING.
  DELETE ADJACENT DUPLICATES FROM t_present COMPARING trkorr object obj_name.
  "=========================================================================="
  "--------------------------To Biuld Conflict Table-------------------------"
  "=========================================================================="
  "loop at all target system entries
*  LOOP AT t_remote1 INTO wa_remote1.
  LOOP AT t_present INTO wa_present.
    "check if all those TR#s in Target system exist in the present system
*    READ TABLE t_present INTO wa_present WITH KEY trkorr = wa_remote1-trkorr
*                                                  object = wa_remote1-object
*                                                obj_name = wa_remote1-obj_name BINARY SEARCH.
    READ TABLE t_remote1 INTO wa_remote1 WITH KEY trkorr = wa_present-trkorr
                                                  object = wa_present-object
                                                  obj_name = wa_present-obj_name BINARY SEARCH.
    IF sy-subrc <> 0.
*      "Check if the TR made as never transport
*      READ TABLE i_trnever INTO wa_trnever WITH KEY trkorr = wa_present-trkorr.
*      if sy-subrc <> 0.

      "if the TR#s doesn#t exist, move the detail of TR#s
*       if wa_remote1-trkorr is not INITIAL.
*      wa_conflict-trkorr = wa_remote1-trkorr.
*      wa_conflict-trstatus = wa_remote1-trstatus.
*      wa_conflict-as4user = wa_remote1-as4user.
*      wa_conflict-as4date = wa_remote1-as4date.
*      wa_conflict-as4time = wa_remote1-as4time.
*      wa_conflict-pgmid  = wa_remote1-pgmid.
*      wa_conflict-object = wa_remote1-object.
*      wa_conflict-obj_name = wa_remote1-obj_name.

      wa_conflict-trkorr = wa_present-trkorr.
      wa_conflict-trstatus = wa_present-trstatus.
      wa_conflict-as4user = wa_present-as4user.
      wa_conflict-as4date = wa_present-as4date.
      wa_conflict-as4time = wa_present-as4time.
      wa_conflict-pgmid  = wa_present-pgmid.
      wa_conflict-object = wa_present-object.
      wa_conflict-obj_name = wa_present-obj_name.
      wa_conflict-comments = text-h33. "  'NOK...Conflict exists'.
      wa_conflict-icon = '1'.

      APPEND wa_conflict TO t_conflict.
      CLEAR : wa_remote1 , wa_present.
      CLEAR wa_conflict.
*      endif.
    ELSE.
      "move the object with detail with no conflict
      wa_conflict-trkorr = wa_remote1-trkorr.
      wa_conflict-trstatus = wa_remote1-trstatus.
      wa_conflict-as4user = wa_remote1-as4user.
      wa_conflict-as4date = wa_remote1-as4date.
      wa_conflict-as4time = wa_remote1-as4time.
      wa_conflict-pgmid  = wa_remote1-pgmid.
      wa_conflict-object = wa_remote1-object.
      wa_conflict-obj_name = wa_remote1-obj_name.

      wa_conflict-comments = text-h34.  "'OK...Conflict does not exists'.
      wa_conflict-icon = '3'.
      APPEND wa_conflict TO t_conflict.
      CLEAR : wa_present , wa_remote1.
      CLEAR wa_conflict.
    ENDIF.
  ENDLOOP.

*  REFRESH : t_re071_r,t_re070_r,t_out_tab.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALL_OBJECTS_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM all_objects_check .

  CONSTANTS: lc_incl(4) TYPE c VALUE 'INCL',
               lc_prog(4) TYPE c VALUE 'PROG',
               lc_stru(4) TYPE c VALUE 'STRU',
               lc_tabl(4) TYPE c VALUE 'TABL',
               lc_r3tr(4) TYPE c VALUE 'R3TR'.

  REFRESH:t_created,t_output_list, t_output_list1.
*delete t_e071_r1
  LOOP AT t_e071_r1 INTO wa_e071 WHERE  pgmid EQ 'R3TR'
       AND object = 'PROG'.
    MOVE wa_e071-obj_name TO s_report-low.
    APPEND s_report .
    wa_curr_temp-object = wa_e071-object.
    wa_curr_temp-obj_name = wa_e071-obj_name.
*      wa_curr_temp-devclass = wa_itab-devclass.
    APPEND wa_curr_temp TO t_curr_temp.
    CLEAR wa_curr_temp.
  ENDLOOP.

  "----------------------------------------------------------------------"
  "------------------Get created object list-----------------------------"
  "----------------------------------------------------------------------"
  LOOP AT s_report.

    pa_prog = s_report-low.

    CALL FUNCTION 'REPOSITORY_ENVIRONMENT_SET_RFC'
      EXPORTING
        obj_type          = 'PROG'
        environment_types = t_environment_selection
        object_name       = pa_prog
      TABLES
        environment       = t_output_list
        source_objects    = t_source_searched.

    APPEND LINES OF t_output_list TO t_output_list1.
    CLEAR t_output_list.
  ENDLOOP.

  t_created[] = t_output_list1[].
  SORT t_created BY object.
  DELETE ADJACENT DUPLICATES FROM t_created COMPARING object.
  DELETE t_created WHERE NOT ( object CP 'Z*' ).
*  DELETE t_created WHERE int_type is initial.

  LOOP AT t_created into wa_created.
    MOVE wa_created-object TO wa_itab-obj_name.
    MOVE wa_created-type TO wa_itab-object.
    APPEND wa_itab TO t_itab.
    CLEAR wa_itab.

  ENDLOOP.
  APPEND LINES OF t_itab TO t_itab_dup.
  SORT t_itab_dup BY object obj_name.
  DELETE ADJACENT DUPLICATES FROM t_itab_dup COMPARING object obj_name.

* LOOP AT t_itab_dup INTO wa_itab.
*    IF wa_itab-object EQ lc_incl.
*      wa_itab-object = lc_prog.
*    ELSEIF wa_itab-object EQ lc_stru.
*      wa_itab-object = lc_tabl.
*    ENDIF.
*    MODIFY t_itab_dup FROM wa_itab TRANSPORTING object.
*    CLEAR wa_itab.
*  ENDLOOP.

  LOOP AT t_itab_dup INTO wa_itab.
    wa_curr_temp-object = wa_itab-object.
    wa_curr_temp-obj_name = wa_itab-obj_name.
*      wa_curr_temp-devclass = wa_itab-devclass.
    APPEND wa_curr_temp TO t_curr_temp.
    CLEAR wa_curr_temp.
  ENDLOOP.
  sort t_curr_temp by object obj_name.
  delete adjacent duplicates from t_curr_temp comparing object obj_name.
*  IF NOT t_itab_dup[] IS INITIAL.
*    SELECT
*    object
*    obj_name
*    devclass FROM tadir INTO TABLE t_tadir
*                               FOR ALL ENTRIES IN t_itab_dup
*                               WHERE pgmid = lc_r3tr AND
*                               object      = t_itab_dup-object AND
*                               obj_name    = t_itab_dup-obj_name.
*    IF sy-subrc = 0.
*      SORT t_tadir BY  object obj_name.
*    ENDIF.
*  ENDIF.
*
*  IF t_tadir IS NOT INITIAL."Objcets list used in a TR
*
*    CLEAR wa_tadir.
*    LOOP AT t_tadir INTO wa_tadir.
*      wa_curr_temp-object = wa_tadir-object.
*      wa_curr_temp-obj_name = wa_tadir-obj_name.
*      wa_curr_temp-devclass = wa_tadir-devclass.
*      APPEND wa_curr_temp TO t_curr_temp.
*      CLEAR wa_curr_temp.
*    ENDLOOP.
*
*    SORT t_curr_temp BY object obj_name.
*    DELETE ADJACENT DUPLICATES FROM t_curr_temp COMPARING object obj_name.
*    endif.
ENDFORM.
