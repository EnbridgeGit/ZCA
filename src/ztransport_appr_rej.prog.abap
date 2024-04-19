*&---------------------------------------------------------------------*
*& Report  ZTRANSPORT_APPR_REJ
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZTRANSPORT_APPR_REJ.


"========================================================================================"
"----------------------------------Includes----------------------------------------------"
"========================================================================================"

"--------------------------Data Declaration----------------------------------------------"

INCLUDE ZTR_APPR_REJ_TOP.

"---------------------------Selection Screen---------------------------------------------"

INCLUDE ZTR_APPR_REJ_SEL_SCN.


"---------------------------At Selection screen Output---------------------------------------------"

AT SELECTION-SCREEN OUTPUT.


HELP_INFOS-CALL = 'V'.
HELP_INFOS-OBJECT = 'F'.
HELP_INFOS-PROGRAM = 'RSSYSTDB'.
HELP_INFOS-DYNPRO = '1000'.
HELP_INFOS-TABNAME = 'RFCDES'.
HELP_INFOS-FIELDNAME = 'RFCDEST'.
HELP_INFOS-FIELDTYPE = 'CHAR'.
HELP_INFOS-FIELDLNG = '32'.
HELP_INFOS-SPRAS = 'E'.
HELP_INFOS-MENUFUNCT = 'HC'.
HELP_INFOS-TITLE = 'TR Approval Rejection Program'.
HELP_INFOS-DYNPROFLD = 'P_TARSYS'.
HELP_INFOS-TCODE = SY-TCODE.
HELP_INFOS-PFKEY = '%_00'.
HELP_INFOS-REPORT = SY-REPID.
HELP_INFOS-DOCUID = 'FE'.
HELP_INFOS-POV = 'N'.
HELP_INFOS-CUROW = '12'.
HELP_INFOS-CUCOL = '35'.
HELP_INFOS-SY_DYN = 'S'.
HELP_INFOS-DYNPPROG = SY-REPID.
HELP_INFOS-STEPL = '0'.
*----------------------------------------------------------------------*
*       CLASS DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handlers DEFINITION.
  PUBLIC SECTION.
    METHODS:handle_button_click FOR EVENT button_click OF cl_gui_alv_grid
      IMPORTING es_col_id  es_row_no.
ENDCLASS.


*----------------------------------------------------------------------*
*       CLASS IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handlers IMPLEMENTATION.
  METHOD handle_button_click.
    DATA l_row TYPE c LENGTH 10.
    l_row = es_row_no-row_id.
    IF es_col_id-fieldname eq 'APPROVE' or  es_col_id-fieldname eq 'DECLINE'.
      PERFORM send_mail1 USING l_row es_col_id-fieldname.
    ELSEIF es_col_id-fieldname eq 'SLIN_CHECK'.
      PERFORM slin_check USING l_row es_col_id-fieldname.
    ELSEIF es_col_id-fieldname eq 'CHK_SEQ'.
      PERFORM check_TR_SEQ USING l_row es_col_id-fieldname.
    ELSE.
      PERFORM tr_analyzer USING l_row es_col_id-fieldname.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

"---------------------------At Selection Screen on Value Request---------------------------------------------"



AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_TARSYS.

  CALL FUNCTION 'HELP_START'
    EXPORTING
      HELP_INFOS         = HELP_INFOS
*     PROPERTY_BAG       =
   IMPORTING
     SELECTION          = SELECTION
     SELECT_VALUE       = SELECT_VALUE
     RSMDY_RET          = RSMDY_RET
    TABLES
      DYNPSELECT         = DYNPSELECT
      DYNPVALUETAB       = DYNPVALUETAB
            .
  IF SY-SUBRC EQ 0.
    P_TARSYS = SELECT_VALUE.
  ENDIF.



"---------------------------At Selection Screen on block---------------------------------------------"



AT SELECTION-SCREEN ON BLOCK b2.
  If S_TRKORR IS NOT INITIAL.
     REFRESH : IT_TADIR, IT_E071, IT_E070.
    CLEAR : wa_E070,gv_strkorr.
*    gv_trkorr = p_trkorr.
*    SELECT pgmid object obj_name devclass
*          FROM tadir
*          INTO TABLE it_tadir
*    WHERE devclass = p_dvclas.

*    IF it_tadir IS NOT INITIAL.

*      SELECT trkorr
*         FROM e071
*        INTO CORRESPONDING FIELDS OF TABLE it_e071
*        FOR ALL ENTRIES IN it_tadir where
*      obj_name = it_tadir-obj_name.
*
*      IF it_e071 is not INITIAL.
*
*        SELECT trkorr trstatus strkorr as4user
*        FROM e070
*        INTO CORRESPONDING FIELDS OF TABLE it_e070
*        FOR ALL ENTRIES IN it_e071
*        WHERE trkorr = it_e071-trkorr.
*      ENDIF.

*    ENDIF.

    SELECT trkorr trstatus as4user as4date as4time strkorr
        FROM e070
        INTO TABLE it_e070
      WHERE STRKORR IN S_trkorr.
    IF sy-subrc NE 0.
        SELECT trkorr trstatus as4user as4date as4time strkorr
        FROM e070
        INTO TABLE it_e070
        WHERE trkorr IN s_trkorr.
    ENDIF.
    IF it_e070 IS NOT INITIAL.
      SELECT trkorr as4pos pgmid object obj_name
       FROM e071
      INTO TABLE it_e071
      FOR ALL ENTRIES IN it_e070 where
       trkorr = it_e070-trkorr.
    ENDIF.

    LOOP AT it_e070 INTO wa_e070 WHERE
            trstatus = 'R' or trstatus = 'N'.
    ENDLOOP.
    IF sy-subrc eq 0.
*      SET CURSOR FIELD S_TRKORR.
*      MESSAGE text-024 TYPE 'E'.
*      EXIT.
    ENDIF.


*    READ TABLE IT_e070 into wa_e070 WITH KEY TRKORR = P_TRKORR
*                                             STRKORR = ''.
*     Checking the child TR
*    IF sy-subrc ne 0.
*      READ TABLE IT_e070 into wa_e070 WITH KEY TRKORR = P_TRKORR.
*    IF SY-SUBRC eq 0.
*      READ TABLE IT_e070 into wa_e070 WITH KEY TRKORR = wa_e070-strkorr.
*      IF sy-subrc eq 0.
*      gv_strkorr = wa_e070-trkorr.
*      else.
*       gv_strkorr = wa_e070-strkorr.
*      endif.
**          Checking the parent TR
*     ELSEIF SY-SUBRC ne 0.
*       READ TABLE IT_e070 into wa_e070 WITH KEY sTRKORR = P_TRKORR.
*       if sy-subrc eq 0.
*       gv_strkorr = wa_e070-strkorr.
*       endif.
*     ENDIF.
*     ELSE.
*       gv_strkorr = wa_e070-trkorr.
*     ENDIF.
*
*     IF gv_strkorr is not initial.
*        IF wa_e070-trstatus eq 'R' or wa_e070-trstatus eq 'N'.
*          SET CURSOR FIELD P_TRKORR.
*          MESSAGE text-024 TYPE 'E'.
*          EXIT.
*        ELSE.
*        DELETE s_mailid where low eq gv_owner.
*        CONCATENATE wa_e070-as4user '@accenture.com' into gv_owner.
*        s_mailid-low = gv_owner.
*        APPEND s_mailid.
*        ENDIF.
**      ELSE.
**        IF gv_trkorr CA 'NRD' AND strlen( gv_trkorr ) = 10.
**        SET CURSOR FIELD 'P_TRKORR'.
**        MESSAGE text-025 TYPE 'E'.
**        ELSE.
**          Message text-030 TYPE 'E'.
**        ENDIF.
*      ENDIF.

  ENDIF.


"========================================================================================"
"-------------------------Start of Selection --------------------------------------------"
"========================================================================================"


START-OF-SELECTION.
CLEAR v_msg_log.

AUTHORITY-CHECK OBJECT 'ZCHECK'

ID GV_ACTVT FIELD '16'.

AUTHORITY-CHECK OBJECT 'ZCHECK'

ID GV_ACTVT FIELD '01'.

AUTHORITY-CHECK OBJECT 'ZCHECK'

ID GV_ACTVT FIELD '02'.

PERFORM f_table_entry.

END-OF-SELECTION.


  IF sy-batch = 'X'.
    PERFORM f_background_job.

  ENDIF.

  PERFORM f_alv_output.

*&---------------------------------------------------------------------*
*&      Form  F_TABLE_ENTRY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
**Fetching the TR details
FORM f_table_entry .

  DATA: lv_tr TYPE trkorr,
        t_lines TYPE i,
        lv_rel TYPE c.
  FIELD-SYMBOLS: <fs_systems> TYPE s_ctslg_system.

*  SELECT pgmid object obj_name devclass
*          FROM tadir
*          INTO TABLE it_tadir
*  WHERE devclass = p_dvclas.
*
*  IF it_tadir IS NOT INITIAL.
*
*    SELECT trkorr as4pos pgmid object obj_name
*       FROM e071
*      INTO TABLE it_e071
*      FOR ALL ENTRIES IN it_tadir where
*    obj_name = it_tadir-obj_name.
*
*    IF it_e071 IS NOT INITIAL.
*      SELECT trkorr trstatus as4user as4date as4time strkorr
*        FROM e070
*        INTO TABLE it_e070
*        FOR ALL ENTRIES IN it_e071
*      WHERE trkorr = it_e071-trkorr.
*
*    ENDIF.

   IF S_TRKORR IS NOT INITIAL.
     SELECT trkorr trstatus as4user as4date as4time strkorr
        FROM e070
        INTO TABLE it_e070
      WHERE STRKORR IN s_trkorr.
      IF sy-subrc NE 0.
        SELECT trkorr trstatus as4user as4date as4time strkorr
        FROM e070
        INTO TABLE it_e070
        WHERE trkorr IN s_trkorr.
      ENDIF.
    IF it_e070 IS NOT INITIAL.
      SELECT trkorr as4pos pgmid object obj_name
       FROM e071
      INTO TABLE it_e071
      FOR ALL ENTRIES IN it_e070 where
       trkorr = it_e070-trkorr.
    ENDIF.
   ENDIF.

    IF it_e070 IS NOT INITIAL.
      SELECT trkorr as4text
        FROM e07t
        INTO TABLE it_e07t
        FOR ALL ENTRIES IN it_e070
      WHERE trkorr = it_e070-trkorr.

    ENDIF.

*  ENDIF.

*  LOOP AT it_tadir INTO wa_tadir.

    CLEAR wa_e071.

*    LOOP AT IT_E071 INTO WA_E071 WHERE obj_name = wa_tadir-obj_name.
    LOOP AT IT_E071 INTO WA_E071.
*      wa_final-package = wa_tadir-devclass.
      READ TABLE it_e070 INTO wa_e070 WITH KEY trkorr = wa_e071-trkorr.
      IF sy-subrc EQ 0.
        CLEAR lv_tr.
        lv_tr = wa_e070-strkorr.
        CLEAR: ls_request-cofile, ls_request-project.
        REFRESH: lt_requests.
        CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
          EXPORTING
            iv_trkorr   = lv_tr
            iv_dir_type = 'T'
          IMPORTING
            es_cofile   = ls_request-cofile
            ev_user     = lv_username
            ev_project  = ls_request-project.

        IF ls_request-header-as4user = space.
          ls_request-header-as4user = lv_username.
        ENDIF.

        ls_request-cofile_filled = 'X'.
        APPEND ls_request TO lt_requests.


        LOOP AT lt_requests INTO ls_request.

          IF ls_request-cofile IS NOT INITIAL.
            IF ls_request-cofile-systems[] IS NOT INITIAL.
               READ TABLE ls_request-cofile-systems ASSIGNING <fs_systems> WITH KEY systemid = 'P01'.
            IF sy-subrc = 0 .
              DESCRIBE TABLE <fs_systems>-steps LINES t_lines.
              IF t_lines LE 1.
                wa_final-tarsystem = 'P01'.
                CLEAR lv_rel.
              ELSE.
                 MESSAGE i020(ztr) WITH lv_tr '-Released to Production'.
                 lv_rel = 'X'.
                 exit.
               ENDIF.
              ELSE.
                clear lv_rel.
                READ TABLE ls_request-cofile-systems WITH KEY systemid = 'D22' TRANSPORTING NO FIELDS.
                IF sy-subrc = 0 .
                  wa_final-tarsystem = 'D22'.

                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDLOOP.
        IF lv_rel = 'X'.
         CONTINUE.
        ENDIF.

        wa_final-tarsystem = p_tarsys.

        CLEAR : wa_e07t.
        READ TABLE it_e07t INTO wa_e07t WITH KEY trkorr = wa_e071-trkorr.
        IF sy-subrc EQ 0.
          wa_final-as4text = wa_e07t-as4text.
        ENDIF.

        CLEAR : wa_e070.
        READ TABLE it_e070 INTO wa_e070 WITH KEY trkorr = wa_e071-trkorr.
        IF sy-subrc EQ 0.
          IF wa_e070-strkorr NE ''.
        READ TABLE it_e070 INTO wa_e070 WITH KEY trkorr = wa_e070-strkorr.
        IF SY-SUBRC NE 0.
          wa_final-strkorr = wa_e070-strkorr.
          ENDIF.
          ENDIF.

          wa_final-as4user  =   wa_e070-as4user .
          wa_final-as4date  =   wa_e070-as4date  .
          IF wa_final-strkorr is initial.
          wa_final-strkorr   =  wa_e070-trkorr  .
          ENDIF.

          IF wa_e070-trstatus = 'D'.
            wa_final-trstdes = 'Modifiable'.

          ELSEIF wa_e070-trstatus = 'L'.
            wa_final-trstdes = 'Modifiable, Protected'.

          ELSEIF wa_e070-trstatus = 'O'.
            wa_final-trstdes = 'Release Started'.

          ELSEIF wa_e070-trstatus = 'R'.
            wa_final-trstdes = 'Released'.

          ELSEIF wa_e070-trstatus = 'N'.
            wa_final-trstdes = 'Released (with Import Protection for Repaired Objects)'.
          ENDIF.

        ENDIF.
        IF wa_final-trstdes NE 'Released'.
          wa_final-approve = 'Approve'.
          wa_final-decline = 'Decline'.
        ELSE.
          wa_final-approve = 'Not Applicable'.
          wa_final-decline = 'Not Applicable'.
        ENDIF.
        wa_final-slin_check = 'Code Inspector Check'.
        wa_final-tr_analyzer = 'TR Analyzer'.
        wa_final-chk_seq    = 'TR Sequence'.
        APPEND wa_final TO it_final.
        CLEAR : wa_final, ls_request, wa_e071.
        REFRESH lt_requests.
      ENDIF.
    ENDLOOP.
*  ENDLOOP.
  LOOP AT it_final INTO wa_final WHERE trstdes = 'Released' AND tarsystem EQ ''.

    CLEAR :    ls_request-cofile,
               lv_username,
               ls_request-project.

    CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
      EXPORTING
        iv_trkorr   = wa_final-strkorr
        iv_dir_type = 'T'
      IMPORTING
        es_cofile   = ls_request-cofile
        ev_user     = lv_username
        ev_project  = ls_request-project.



    IF ls_request-header-as4user = space.
      ls_request-header-as4user = lv_username.
    ENDIF.

    ls_request-cofile_filled = 'X'.
    APPEND ls_request TO lt_requests.


    LOOP AT lt_requests INTO ls_request.

      IF ls_request-cofile IS NOT INITIAL.
        IF ls_request-cofile-systems[] IS NOT INITIAL.
          READ TABLE ls_request-cofile-systems WITH KEY systemid = 'P01' TRANSPORTING NO FIELDS.

          IF sy-subrc = 0 .
            wa_final-tarsystem = 'P01'.
          ELSE.
            READ TABLE ls_request-cofile-systems WITH KEY systemid = 'D22' TRANSPORTING NO FIELDS.
            IF sy-subrc = 0 .
              wa_final-tarsystem = 'D22'.

            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.
    IF wa_final-tarsystem IS INITIAL.
     wa_final-tarsystem = p_tarsys.
    ENDIF.

    MODIFY it_final FROM wa_final TRANSPORTING tarsystem.


  ENDLOOP.
  LOOP AT IT_FINAL INTO WA_FINAL.
*    CONCATENATE wa_final-package
    CONCATENATE wa_final-strkorr wa_final-as4text wa_final-trstdes wa_final-tarsystem wa_final-as4user
                wa_final-as4date wa_final-trkorr wa_final-approve wa_final-decline wa_final-slin_check into wa_final-dup.
    MODIFY it_final FROM wa_final TRANSPORTING dup.
  ENDLOOP.
  SORT it_final by dup.
  DELETE ADJACENT DUPLICATES FROM it_final COMPARING dup.
  LOOP AT IT_FINAL INTO WA_FINAL.
*  CONCATENATE wa_final-package
  CONCATENATE wa_final-strkorr wa_final-as4text wa_final-trstdes wa_final-tarsystem into wa_final-dup.
  MODIFY it_final FROM wa_final TRANSPORTING dup.
  ENDLOOP.
  SORT it_final by dup as4date DESCENDING.
  DELETE ADJACENT DUPLICATES FROM it_final COMPARING dup.
  LOOP AT IT_FINAL INTO WA_FINAL.
    clear wa_final-dup.
    MODIFY it_final FROM wa_final TRANSPORTING dup.
  ENDLOOP.
*  SUBRAMS2
*  DELETE IT_FINAL WHERE strkorr(4) eq text-028.
*  DELETE IT_FINAL WHERE strkorr(3) ne text-041.
*  SUBRAMS2
  IT_DISP[] = IT_FINAL[].
*  DELETE IT_DISP WHERE trstdes eq 'Released'.

*  IF P_TRKORR IS NOT INITIAL.
*    READ TABLE IT_DISP INTO WA_FINAL WITH KEY STRKORR = gv_strkorr.
*    IF SY-SUBRC eq 0.
*      DELETE IT_DISP WHERE STRKORR NE GV_STRKORR.
*    ENDIF.
*
*  ENDIF.
  IF IT_DISP IS INITIAL.
    MESSAGE text-027 TYPE 'I'.
    LEAVE LIST-PROCESSING.
    EXIT.
  ENDIF.

  SELECT * FROM ZTRT_CONFIG INTO TABLE it_zfit WHERE PARAMTYPE = 'ZTRANSPORT'.
  IF sy-subrc EQ 0.
  ENDIF.


ENDFORM.


FORM f_alv_output .
**Field Catalog for ALV display

  DATA : ls_fieldcat TYPE lvc_s_fcat.
*  ls_fieldcat-col_pos = '1'.
*  ls_fieldcat-fieldname = 'PACKAGE'.
*  ls_fieldcat-tabname   = 'IT_DISP'.
*  ls_fieldcat-reptext = 'Package'.
*  APPEND ls_fieldcat TO lt_fieldcat.
*  CLEAR ls_fieldcat.

  ls_fieldcat-col_pos = '2'.
  ls_fieldcat-fieldname = 'AS4TEXT'.
  ls_fieldcat-tabname   = 'IT_DISP'.
  ls_fieldcat-reptext = 'Description'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-col_pos = '3'.
  ls_fieldcat-fieldname = 'TRSTDES'.
  ls_fieldcat-tabname   = 'IT_DISP'.
  ls_fieldcat-reptext = 'Released status'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-col_pos = '4'.
  ls_fieldcat-fieldname = 'TARSYSTEM'.
  ls_fieldcat-tabname   = 'IT_DISP'.
  ls_fieldcat-reptext = 'Target System'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.


  ls_fieldcat-col_pos = '5'.
  ls_fieldcat-fieldname = 'AS4USER'.
  ls_fieldcat-tabname   = 'IT_DISP'.
  ls_fieldcat-reptext = 'Owner'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-col_pos = '6'.
  ls_fieldcat-fieldname = 'AS4DATE'.
  ls_fieldcat-tabname   = 'IT_DISP'.
  ls_fieldcat-reptext = 'Date of Last Change'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.



  ls_fieldcat-col_pos = '1'.
  ls_fieldcat-fieldname = 'STRKORR'.
  ls_fieldcat-tabname   = 'IT_DISP'.
  ls_fieldcat-reptext = 'Higher-Level Request'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.


  ls_fieldcat-col_pos = '7'.
  ls_fieldcat-fieldname = 'APPROVE'.
  ls_fieldcat-tabname   = 'IT_DISP'.
  ls_fieldcat-reptext = 'Approve status'.
  ls_fieldcat-style = cl_gui_alv_grid=>mc_style_button.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-col_pos = '8'.
  ls_fieldcat-fieldname = 'DECLINE'.
  ls_fieldcat-tabname   = 'IT_DISP'.
  ls_fieldcat-reptext = 'Decline status'.
  ls_fieldcat-style = cl_gui_alv_grid=>mc_style_button.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-col_pos = '9'.
  ls_fieldcat-fieldname = 'SLIN_CHECK'.
  ls_fieldcat-tabname   = 'IT_DISP'.
  ls_fieldcat-reptext = 'Code Review Check'.
  ls_fieldcat-style = cl_gui_alv_grid=>mc_style_button.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.


  ls_fieldcat-col_pos = '10'.
  ls_fieldcat-fieldname = 'TR_ANALYZER'.
  ls_fieldcat-tabname   = 'IT_DISP'.
  ls_fieldcat-reptext = 'TR Analyzer Check'.
  ls_fieldcat-style = cl_gui_alv_grid=>mc_style_button.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  wa_layout-cwidth_opt = 'X'.

  ls_fieldcat-col_pos = '11'.
  ls_fieldcat-fieldname = 'CHK_SEQ'.
  ls_fieldcat-tabname   = 'IT_DISP'.
  ls_fieldcat-reptext = 'TR Sequence'.
  ls_fieldcat-style = cl_gui_alv_grid=>mc_style_button.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  wa_layout-cwidth_opt = 'X'.

  IF it_DISP IS NOT INITIAL.
    DATA: l_repid       TYPE syrepid VALUE sy-repid,
          oref_handlers TYPE REF TO lcl_event_handlers.
    DATA l_wa_event TYPE cntl_simple_event.
    IF oref_alv IS NOT BOUND.

      CREATE OBJECT oref_alv
        EXPORTING
*         i_shellstyle      = 0
*         i_lifetime        =
          i_parent          = cl_gui_container=>screen0
*         i_appl_events     = space
*         i_parentdbg       =
*         i_applogparent    =
*         i_graphicsparent  =
*         i_name            =
*         i_fcat_complete   = space
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
      IF sy-subrc = 0.

        CREATE OBJECT oref_handlers.
        SET HANDLER oref_handlers->handle_button_click FOR oref_alv.

        CALL METHOD oref_alv->set_table_for_first_display
          EXPORTING
*           i_buffer_active               =
*           i_bypassing_buffer            =
*           i_consistency_check           =
*           i_structure_name              =
*           is_variant                    =
*           i_save                        =
*           i_default                     = 'X'
            is_layout                     = wa_layout
*           is_print                      =
*           it_special_groups             =
*           it_toolbar_excluding          =
*           it_hyperlink                  =
*           it_alv_graphics               =
*           it_except_qinfo               =
*           ir_salv_adapter               =
          CHANGING
            it_outtab                     = it_DISP
            it_fieldcatalog               = lt_fieldcat
*           it_sort                       =
*           it_filter                     =
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.
        IF sy-subrc <> 0.
          MESSAGE e001(00) WITH 'Error while ALV display'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  CALL SCREEN 0100.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 00.
      LEAVE SCREEN.
    WHEN 'EXIT'.
      SET SCREEN 00.
      LEAVE SCREEN.
    WHEN 'CANCEL'.
      SET SCREEN 00.
      LEAVE SCREEN.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  SEND_MAIL1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_mail1 USING lv_row fieldname.
  TYPES : BEGIN OF ty_e070a.
          INCLUDE STRUCTURE e070a.
  TYPES : END OF ty_e070a .
   TYPES : BEGIN OF ty_e071.
          INCLUDE STRUCTURE e071.
  TYPES : END OF ty_e071 .
  TYPES : BEGIN OF TY_TM,
          LINE(1000) TYPE C,
          END OF TY_TM.
  DATA : lwa_mailid LIKE LINE OF s_mailid,
         it_e070a   TYPE STANDARD TABLE OF ty_e070a,
         wa_e070a   TYPE ty_e070a,
         it_e071   TYPE STANDARD TABLE OF ty_e071,
         wa_e071   TYPE ty_e071,
         lv_var     TYPE string,
         lv_count   TYPE I,
         lv_lcount  TYPE I,
         lt_note    TYPE STANDARD TABLE OF txw_note,
         ls_note    TYPE txw_note,
         lv_popup   TYPE C,
         LT_THEAD   TYPE TABLE OF THEAD,
         LS_THEAD   TYPE THEAD,
         LT_LINE    TYPE TABLE OF TLINE,
         LS_LINE    TYPE TLINE,
         LV_TRKORR  TYPE TRKORR,
         LT_TM      TYPE TABLE OF TY_TM,
         LS_TM      TYPE TY_TM,
         lv_tabix   TYPE sy-tabix,
         LV_REMARKS TYPE C,
         LV_ID      TYPE DOKHL-ID VALUE 'TA',
         LV_TYP     TYPE DOKHL-TYP VALUE 'T',
         LV_OBJECT  TYPE DOKHL-OBJECT,
         LV_YR(4)   TYPE C,
         LV_MNTH(2) TYPE C,
         LV_DA(2) TYPE C,
         LV_DATUM(10)   TYPE C,
         LV_VAR1(20)  TYPE C.

** Updating the Attributes in the TR
  DELETE s_mailid where low eq gv_owner.
  READ TABLE it_disp INTO wa_final INDEX lv_row.
*  CONCATENATE wa_final-as4user '@accenture.com' into gv_owner.
*        s_mailid-low = gv_owner.
*        APPEND s_mailid.

   IF v_msg_log NE 'X'.
    READ TABLE it_zfit INTO wa_zfit WITH KEY SUBTYPE = 'REVIEWER'
                                             value1 = sy-uname.
    IF sy-subrc NE 0.
*      MESSAGE S001(00) WITH 'Not authorized Reviewer' DISPLAY LIKE 'W'.
       MESSAGE 'Not authorized Reviewer - Do not approve unless Emergency' TYPE 'I'.
       v_msg_log = 'X'.
       exit.
    ELSE.
      IF wa_final-AS4USER = sy-uname.
*        MESSAGE S001(00) WITH 'Not authorized to Review own TR' DISPLAY LIKE 'W'.
        MESSAGE 'Not authorized to Review own TR - Do not approve unless Emergency' Type 'I'.
        v_msg_log = 'X'.
        exit.
      ENDIF.
    ENDIF.
   ENDIF.

LV_OBJECT = wa_final-strkorr.
CLEAR : LV_REMARKS.
LV_YR = sy-datum(4).
LV_MNTH = sy-datum+4(2).
LV_DA = sy-datum+6(2).
CONCATENATE LV_YR '.' LV_MNTH '.' LV_DA INTO LV_DATUM.
CONCATENATE LV_DATUM '-' SY-UNAME ' :' INTO LV_VAR1.

  IF fieldname eq 'DECLINE'.

   CALL FUNCTION 'DOCU_GET'
     EXPORTING
*      EXTEND_EXCEPT                = ' '
       ID                           = LV_ID
       LANGU                        = sy-langu
       OBJECT                       = LV_OBJECT
      TYP                          = LV_TYP
*      VERSION                      = 0
*      VERSION_ACTIVE_OR_LAST       = 'L'
*      PRINT_PARAM_GET              = 'X'
*    IMPORTING
*      DOKSTATE                     =
*      DOKTITLE                     =
*      HEAD                         =
*      DOKTYP                       =
     TABLES
       LINE                         = LT_LINE
    EXCEPTIONS
      NO_DOCU_ON_SCREEN            = 1
      NO_DOCU_SELF_DEF             = 2
      NO_DOCU_TEMP                 = 3
      RET_CODE                     = 4
      OTHERS                       = 5
             .

IF LT_LINE IS INITIAL.
   LT_NOTE[] = LT_LINE[].
ELSE.
  LS_NOTE-LINE = 'PREVIOUS COMMENTS FOR REJECTION'.
  APPEND LS_NOTE TO LT_NOTE.
 LOOP AT LT_LINE INTO LS_LINE.
  LS_NOTE-LINE = LS_LINE-TDLINE.
  APPEND LS_NOTE TO LT_NOTE.
ENDLOOP.
LS_NOTE-LINE = 'NEW COMMENTS FOR REJECTION'.
APPEND LS_NOTE TO LT_NOTE.
ENDIF.

       T_TXWNOTE[]       = LT_NOTE[].

  textnote_itxw_note[] = t_txwnote[].
      textnote_edit_mode = edit_mode.

      CALL SCREEN '0205' STARTING AT 05 05 ENDING AT 77 24.

      if textnote_edit_mode = 'X'.
        t_txwnote[] = textnote_itxw_note[].
      endif.
     LT_NOTE[] = T_TXWNOTE[].

  IF TEXTNOTE_OK_CODE eq 'CONT'.
  LOOP AT LT_NOTE INTO LS_NOTE.
    SHIFT LS_NOTE-LINE LEFT DELETING LEADING SPACE.
    MODIFY LT_NOTE FROM LS_NOTE INDEX SY-TABIX.
  ENDLOOP.
DELETE LT_NOTE WHERE LINE = 'PREVIOUS COMMENTS FOR REJECTION'.
LOOP AT LT_NOTE INTO LS_NOTE.
  IF LS_NOTE-LINE(26) EQ 'NEW COMMENTS FOR REJECTION' AND LS_NOTE-LINE+26 NE ''.
    LS_NOTE-LINE = LS_NOTE-LINE+26.
    MODIFY LT_NOTE FROM LS_NOTE INDEX SY-TABIX.
  ENDIF.
ENDLOOP.
DELETE LT_NOTE WHERE LINE = 'NEW COMMENTS FOR REJECTION'.
LOOP AT LT_NOTE INTO LS_NOTE.
  READ TABLE LT_LINE INTO LS_LINE INDEX SY-TABIX.
  IF LS_LINE-TDLINE NE LS_NOTE-LINE.
      LV_REMARKS = 'X'.
      LS_LINE-TDFORMAT = ''.

      IF LS_NOTE-LINE(20) NE lv_var1.
      CONCATENATE lv_datum '-' sy-uname ' :' LS_NOTE-LINE INTO LS_LINE-TDLINE.
      ELSEIF LS_NOTE-LINE(20) EQ lv_var1.
      CONCATENATE lv_datum '-' sy-uname ' :' LS_NOTE-LINE+20 INTO LS_LINE-TDLINE.
      ENDIF.
      APPEND LS_LINE TO LT_LINE.
    ENDIF.
ENDLOOP.


IF LV_REMARKS IS NOT INITIAL.
CALL FUNCTION 'DOCU_UPD'
  EXPORTING
*   APPEND_DOCU         = ' '
    ID                  = LV_ID
    LANGU               = sy-langu
*   NO_MASTERLANG       = ' '
    OBJECT              = LV_OBJECT
*   STATE               = ' '
   TYP                 = LV_TYP
  TABLES
    LINE                = LT_LINE
 EXCEPTIONS
   RET_CODE            = 1
   OTHERS              = 2
          .
ENDIF.

  ENDIF.
ENDIF.

IF wa_final-trstdes NE 'Released'.
    SELECT * FROM e070a INTO TABLE it_e070a WHERE trkorr = wa_final-strkorr and attribute eq 'ZTRANSPORT'.
    IF sy-subrc EQ 0.
      LOOP AT it_e070a INTO wa_e070a.
        IF fieldname eq 'APPROVE'.
        CONCATENATE 'APPROVED' '-' sy-uname INTO wa_e070a-reference.
*        wa_e070a-reference = 'APPROVED'.
        ELSEIF fieldname eq 'DECLINE' AND LV_REMARKS IS NOT INITIAL.
        CONCATENATE 'DECLINED' '-' sy-uname INTO wa_e070a-reference.
*        wa_e070a-reference = 'DECLINED'.
        ENDIF.
        MODIFY it_e070a FROM wa_e070a INDEX sy-tabix.
      ENDLOOP.

      UPDATE e070a FROM TABLE it_e070a.

      ELSE.
      SELECT * FROM e070a INTO TABLE it_e070a WHERE trkorr = wa_final-strkorr.
         DESCRIBE TABLE it_e070a LINES lv_count.
          wa_e070a-attribute = 'ZTRANSPORT'.
          IF fieldname eq 'APPROVE'.
            CONCATENATE 'APPROVED' '-' sy-uname INTO wa_e070a-reference.
*        wa_e070a-reference = 'APPROVED'.
        ELSEIF fieldname eq 'DECLINE' AND LV_REMARKS IS NOT INITIAL.
          CONCATENATE 'DECLINED' '-' sy-uname INTO wa_e070a-reference.
*        wa_e070a-reference = 'DECLINED'.
        ENDIF.
        wa_e070a-pos = lv_count + 1.
        wa_e070a-trkorr = wa_final-strkorr.
        APPEND wa_e070a to it_e070a.
        INSERT e070a FROM wa_e070a.
    ENDIF.
    SELECT * FROM e071 INTO TABLE it_e071 WHERE trkorr = wa_final-strkorr.
      LOOP AT it_e071   INTO wa_e071.
        IF fieldname eq 'APPROVE'.
        wa_e071-lockflag = ''.
        ELSEIF fieldname eq 'DECLINE' AND LV_REMARKS IS NOT INITIAL.
        wa_e071-lockflag = ''.
        ENDIF.
        MODIFY it_e071 FROM wa_e071 INDEX sy-tabix.
      ENDLOOP.
      UPDATE e071 FROM TABLE it_e071.
    IF s_mailid IS NOT INITIAL.
      REFRESH : lt_message.

**FOR MODIFIABLE

      REFRESH it_status.
      it_status[] = it_final[].

      DESCRIBE TABLE it_final LINES lv_t.

      DELETE it_status WHERE trstdes NE 'Modifiable'.

      DESCRIBE TABLE it_status LINES lv_d.

** FOR Modifiable, Protected
      REFRESH it_status.
      it_status[] = it_final[].

      DELETE it_status WHERE trstdes NE 'Modifiable, Protected'.

      DESCRIBE TABLE it_status LINES lv_l.


** FOR Release Started
      REFRESH it_status.
      it_status[] = it_final[].

      DELETE it_status WHERE trstdes NE 'Release Started'.

      DESCRIBE TABLE it_status LINES lv_o.

** For Released
      REFRESH it_status.
      it_status[] = it_final[].

      DELETE it_status WHERE trstdes NE 'Released'.

      DESCRIBE TABLE it_status LINES lv_r.

** For Released (with Import Protection for Repaired Objects)
      REFRESH it_status.
      it_status[] = it_final[].

      DELETE it_status WHERE trstdes NE 'Released (with Import Protection for Repaired Objects)'.

      DESCRIBE TABLE it_status LINES lv_n.


      SELECT SINGLE adr6~smtp_addr
           INTO lv_smtp_addr
           FROM usr21 INNER JOIN adr6
             ON usr21~addrnumber = adr6~addrnumber
            AND usr21~persnumber = adr6~persnumber
      WHERE usr21~bname = sy-uname.             " Use SY-UNAME for user

*----Assign the Email id and User id to  Whom you want to Send---
* Add the recipients email address
      CLEAR lt_receivers.
      REFRESH lt_receivers.
      LOOP AT s_mailid INTO lwa_mailid.
        lt_receivers-receiver = lwa_mailid-low.
        lt_receivers-rec_type = 'U'.
        lt_receivers-com_type = 'INT'.
        lt_receivers-notif_del = 'X'.
        lt_receivers-notif_ndel = 'X'.
        APPEND lt_receivers.
        CLEAR lwa_mailid.
      ENDLOOP.

      lv_psubject = text-008.

      CLEAR lt_document_data.
      lt_document_data-doc_size = 1.
      lt_document_data-obj_langu = sy-langu.
      lt_document_data-obj_name = 'SAPRPT'.
      lt_document_data-obj_descr = lv_psubject.
      lt_document_data-sensitivty = 'F'.

      CLEAR : wa_message.

      lv_text2 = sy-uname.

      CONCATENATE lv_text1 ',' INTO wa_message-line SEPARATED BY space.
      APPEND wa_message TO lt_message.

      CLEAR wa_message.
      wa_message-line = '                                           '.
      APPEND wa_message TO lt_message.

      CLEAR wa_message.
      IF fieldname eq 'APPROVE'.
      CONCATENATE 'The Transport request ' wa_final-strkorr 'is APPROVED for release' INTO wa_message-line SEPARATED BY space.
      ELSEIF fieldname eq 'DECLINE' AND LV_REMARKS IS NOT INITIAL.
      CONCATENATE 'The Transport request ' wa_final-strkorr 'is DECLINED for release' INTO wa_message-line SEPARATED BY space.
      ENDIF.
      lv_var = wa_message-line.
      APPEND wa_message TO lt_message.

      CLEAR wa_message.
      wa_message-line = '                                           '.
      APPEND wa_message TO lt_message.

     IF LV_REMARKS IS NOT INITIAL AND fieldname eq 'DECLINE'.
      CLEAR wa_message.
      wa_message-line = 'Remarks :'.
      APPEND wa_message to lt_message.

      CLEAR wa_message.
      LOOP AT lt_line into ls_line.
       wa_message-line = ls_line-tdline.
       APPEND wa_message TO lt_message.
      ENDLOOP.


       CLEAR wa_message.
      wa_message-line = '                                           '.
      APPEND wa_message TO lt_message.


     ENDIF.


      CLEAR wa_message.
      wa_message-line = text-005.
      APPEND wa_message TO lt_message.


      CLEAR wa_message.
      wa_message-line = '                                        '.
      APPEND wa_message TO lt_message.

      CLEAR wa_message.
      CONCATENATE 'Total TR :' lv_t INTO lv_t SEPARATED BY space.
      wa_message-line = lv_t.
      APPEND wa_message TO lt_message.

      CLEAR wa_message.
      CONCATENATE 'TR in Modifiable state :' lv_d INTO lv_d SEPARATED BY space.
      wa_message-line = lv_d.
      APPEND wa_message TO lt_message.

      CLEAR wa_message.
      CONCATENATE 'TR in Modifiable, Protected state :' lv_l INTO lv_l SEPARATED BY space.
      wa_message-line = lv_l.
      APPEND wa_message TO lt_message.

      CLEAR wa_message.
      CONCATENATE 'TR in Release Started state :' lv_o INTO lv_o SEPARATED BY space.
      wa_message-line =  lv_o.
      APPEND wa_message TO lt_message.

      CLEAR wa_message.
      CONCATENATE 'TR in Released state :' lv_r INTO lv_r SEPARATED BY space.
      wa_message-line = lv_r.
      APPEND wa_message TO lt_message.

      CLEAR wa_message.
      CONCATENATE 'TR in Released (with Import Protection for Repaired Objects) :' lv_n INTO lv_n SEPARATED BY space.
      wa_message-line = lv_n.
      APPEND wa_message TO lt_message.

      CLEAR wa_message.
      wa_message-line = '                                        '.
      APPEND wa_message TO lt_message.

      CLEAR wa_message.
      wa_message-line = '                                        '.
      APPEND wa_message TO lt_message.

      CLEAR wa_message.
      wa_message-line = text-006.
      APPEND wa_message TO lt_message.


      REFRESH lt_packing_list.
      wa_packing_list-transf_bin = space.
      wa_packing_list-head_start = 1.
      wa_packing_list-head_num = 1.
      wa_packing_list-body_start = 1.
      DESCRIBE TABLE lt_message LINES wa_packing_list-body_num.
      wa_packing_list-doc_type = 'RAW'.
      APPEND wa_packing_list TO lt_packing_list.
      CLEAR wa_packing_list-body_num.

      REFRESH : lt_contents_bin.
      CLEAR : wa_contents_bin, lv_tabix.
*      CONCATENATE text-001 cl_abap_char_utilities=>horizontal_tab
      CONCATENATE text-014 cl_abap_char_utilities=>horizontal_tab
                  text-003 cl_abap_char_utilities=>horizontal_tab
                  text-004 cl_abap_char_utilities=>horizontal_tab
                  text-016 cl_abap_char_utilities=>horizontal_tab
                  text-011 cl_abap_char_utilities=>horizontal_tab
                  text-012 cl_abap_char_utilities=>horizontal_tab
                  text-002 cl_abap_char_utilities=>cr_lf
                 INTO wa_contents_bin.


      lv_tabix = lv_tabix + 1.

      INSERT wa_contents_bin INTO lt_contents_bin INDEX lv_tabix.
      CLEAR wa_contents_bin.



      LOOP AT it_final INTO wa_final.


        lv_tabix = lv_tabix + 1.

        DATA : lv_dat TYPE c LENGTH 2.
        DATA : lv_mon TYPE c LENGTH 2.
        DATA : lv_year TYPE c LENGTH 4.
        DATA : lv_date TYPE c LENGTH 10.

        lv_dat = wa_final-as4date+6(2).
        lv_mon = wa_final-as4date+4(2).
        lv_year = wa_final-as4date+0(4).

        CONCATENATE lv_dat lv_mon lv_year INTO lv_date SEPARATED BY '.'.

        IF NOT wa_final EQ cl_abap_char_utilities=>newline.



*          CONCATENATE wa_final-package cl_abap_char_utilities=>horizontal_tab
           CONCATENATE wa_final-strkorr cl_abap_char_utilities=>horizontal_tab
                      wa_final-as4text cl_abap_char_utilities=>horizontal_tab
                      wa_final-trstdes cl_abap_char_utilities=>horizontal_tab
                      wa_final-tarsystem cl_abap_char_utilities=>horizontal_tab
                      wa_final-as4user cl_abap_char_utilities=>horizontal_tab
                      lv_date cl_abap_char_utilities=>horizontal_tab
                      wa_final-trkorr cl_abap_char_utilities=>cr_lf
                 INTO wa_contents_bin.


          INSERT wa_contents_bin INTO lt_contents_bin INDEX lv_tabix.
          CLEAR wa_contents_bin.

        ENDIF.

      ENDLOOP.

      CLEAR wa_packing_list.
      wa_packing_list-transf_bin = 'X'.
      wa_packing_list-head_start = 1.
      wa_packing_list-head_num = 0.
      wa_packing_list-body_start = 1.
      DESCRIBE TABLE lt_contents_bin LINES wa_packing_list-body_num.
      wa_packing_list-doc_type = 'XLS'.
      wa_packing_list-obj_descr = 'TR RECORDS'.
      wa_packing_list-doc_size = wa_packing_list-body_num * 255.
      APPEND wa_packing_list TO lt_packing_list.

 IF fieldname eq 'APPROVE' or LV_REMARKS is not initial.
      CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
        EXPORTING
          document_data              = lt_document_data
          put_in_outbox              = 'X'
          commit_work                = 'X'
        TABLES
          packing_list               = lt_packing_list
*         OBJECT_HEADER              =
          contents_bin               = lt_contents_bin
          contents_txt               = lt_message
*         CONTENTS_HEX               =
*         OBJECT_PARA                =
*         OBJECT_PARB                =
          receivers                  = lt_receivers
        EXCEPTIONS
          too_many_receivers         = 1
          document_not_sent          = 2
          document_type_not_exist    = 3
          operation_no_authorization = 4
          parameter_error            = 5
          x_error                    = 6
          enqueue_error              = 7
          OTHERS                     = 8.

      IF sy-subrc EQ 0.

        MESSAGE lv_var TYPE 'S'.

      ENDIF.
  ELSE.
      IF TEXTNOTE_OK_CODE eq 'BACK' or TEXTNOTE_OK_CODE eq 'EXIT' or TEXTNOTE_OK_CODE eq 'BREAK' or TEXTNOTE_OK_CODE eq 'CANC'.
        MESSAGE text-050 TYPE 'S' DISPLAY LIKE 'E'.
      ELSEIF TEXTNOTE_OK_CODE eq 'CONT'.
        MESSAGE text-037 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
  ENDIF.
    ELSE.
      MESSAGE text-021 TYPE 'S'.
    ENDIF.
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  SLIN_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SLIN_CHECK USING ROW fldname.

  DATA : lt_bdcdata type STANDARD TABLE OF bdcdata,
         ls_bdcdata type bdcdata.

  READ TABLE it_disp INTO wa_final INDEX row.


  ls_bdcdata-program = 'SAPLS_CODE_INSPECTOR'.

  ls_bdcdata-dynpro = '0200'.                               " screen number

  ls_bdcdata-dynbegin = 'X'.

  Append ls_bdcdata to lt_bdcdata.
  clear ls_bdcdata.                                  " ADD this line

  ls_bdcdata-fnam = 'SCI_DYNP-X_I_OBJS_O'.

  ls_bdcdata-fval = 'X'.
  Append ls_bdcdata to lt_bdcdata.
  clear ls_bdcdata.


  ls_bdcdata-fnam = 'SCI_DYNP-O_ORDER'.

  ls_bdcdata-fval = wa_final-strkorr.

  Append ls_bdcdata to lt_bdcdata.
  clear ls_bdcdata.

  ls_bdcdata-program = 'SAPLS_CODE_INSPECTOR'.

  ls_bdcdata-dynpro = '0200'.                               " screen number

  ls_bdcdata-dynbegin = 'X'.                                 " ADD this line

  ls_bdcdata-fnam = 'BDC_OKCODE'.

  ls_bdcdata-fval = '=RUN'.
  Append ls_bdcdata to lt_bdcdata.
  clear ls_bdcdata.

  ls_bdcdata-program = 'SAPLS_CODE_INSPECTOR'.

  ls_bdcdata-dynpro = '0500'.                               " screen number

  ls_bdcdata-dynbegin = 'X'.
  Append ls_bdcdata to lt_bdcdata.
  clear ls_bdcdata.


  CALL TRANSACTION 'SCII' USING lt_bdcdata
                            MODE  'E'
                            UPDATE 'S'.
  REFRESH : lt_bdcdata.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  TR_ANALYZER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tr_analyzer USING lv_row lv_fieldname.

DATA :   lt_bdcdata type STANDARD TABLE OF bdcdata,
         ls_bdcdata type bdcdata,
         lv_system(30) TYPE C,
         lv_trkorr TYPE E070-TRKORR,
         LS_E070 TYPE ty_E070.

  READ TABLE it_disp INTO wa_final INDEX lv_row.

*  IF wa_final-trstdes eq 'Modifiable'.
    lv_system = p_tarsys.
*  ENDIF.
DATA : LT_TR TYPE TABLE OF RSPARAMS,
       LS_TR TYPE RSPARAMS.

LS_TR-SELNAME = 'RD_REPRT'.
LS_TR-KIND = 'P'.
LS_TR-SIGN = 'I'.
LS_TR-OPTION = 'EQ'.
LS_TR-LOW = ''.
APPEND LS_TR TO LT_TR.
CLEAR LS_TR.

LS_TR-SELNAME = 'RD_CNFLC'.
LS_TR-KIND = 'P'.
LS_TR-SIGN = 'I'.
LS_TR-OPTION = 'EQ'.
LS_TR-LOW = 'X'.
APPEND LS_TR TO LT_TR.
CLEAR LS_TR.

LS_TR-SELNAME = 'RD_TRLIS'.
LS_TR-KIND = 'P'.
LS_TR-SIGN = 'I'.
LS_TR-OPTION = 'EQ'.
LS_TR-LOW = ''.
APPEND LS_TR TO LT_TR.
CLEAR LS_TR.

LS_TR-SELNAME = 'CK_COPY'.
LS_TR-KIND = 'P'.
LS_TR-SIGN = 'I'.
LS_TR-OPTION = 'EQ'.
LS_TR-LOW = ''.
APPEND LS_TR TO LT_TR.
CLEAR LS_TR.

LS_TR-SELNAME = 'CK_REL'.
LS_TR-KIND = 'P'.
LS_TR-SIGN = 'I'.
LS_TR-OPTION = 'EQ'.
LS_TR-LOW = ''.
APPEND LS_TR TO LT_TR.
CLEAR LS_TR.

LS_TR-SELNAME = 'P_SYSTEM'.
LS_TR-KIND = 'P'.
LS_TR-SIGN = 'I'.
LS_TR-OPTION = 'EQ'.
LS_TR-LOW = lv_system.
APPEND LS_TR TO LT_TR.
CLEAR LS_TR.

LS_TR-SELNAME = 'RD_REQ'.
LS_TR-KIND = 'P'.
LS_TR-SIGN = 'I'.
LS_TR-OPTION = 'EQ'.
LS_TR-LOW = 'X'.
APPEND LS_TR TO LT_TR.
CLEAR LS_TR.


Select single trkorr from e071 into lv_trkorr where trkorr eq wa_final-strkorr.
  IF sy-subrc eq 0.
    LS_TR-SELNAME = 'S_REQ'.
LS_TR-KIND = 'S'.
LS_TR-SIGN = 'I'.
LS_TR-OPTION = 'EQ'.
  LS_TR-LOW = wa_final-strkorr.
  APPEND LS_TR TO LT_TR.
CLEAR LS_TR.
  ELSE.
  it_e070_1[] = it_e070[].

  SORT it_e070_1 by as4date DESCENDING.
  LOOP AT IT_E070_1 INTO LS_E070 WHERE strkorr = wa_final-strkorr.
     LS_TR-SELNAME = 'S_REQ'.
LS_TR-KIND = 'S'.
LS_TR-SIGN = 'I'.
LS_TR-OPTION = 'EQ'.
  LS_TR-LOW = ls_e070-trkorr.
   APPEND LS_TR TO LT_TR.
CLEAR LS_TR.
ENDLOOP.
  ENDIF.



LS_TR-SELNAME = 'RD_CONF'.
LS_TR-KIND = 'P'.
LS_TR-SIGN = 'I'.
LS_TR-OPTION = 'EQ'.
LS_TR-LOW = 'X'.
APPEND LS_TR TO LT_TR.
CLEAR LS_TR.

LS_TR-SELNAME = 'RD_DEP'.
LS_TR-KIND = 'P'.
LS_TR-SIGN = 'I'.
LS_TR-OPTION = 'EQ'.
LS_TR-LOW = ''.
APPEND LS_TR TO LT_TR.
CLEAR LS_TR.




SUBMIT ZTRANSPORT_ANALYZER WITH SELECTION-TABLE LT_TR AND RETURN.
  REFRESH : lt_bdcdata, LT_TR.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_BACKGROUND_JOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_background_job .

  TYPES : BEGIN OF ty_head,
            col1  TYPE c LENGTH 20,
            col2  TYPE c LENGTH 20,
            col3  TYPE c LENGTH 20,
            col4  TYPE c LENGTH 20,
            col5  TYPE c LENGTH 20,
            col6  TYPE c LENGTH 20,
            col7  TYPE c LENGTH 20,
            col8  TYPE c LENGTH 20,
            col9  TYPE c LENGTH 20,
            col10 TYPE c LENGTH 20,
            col11 TYPE c LENGTH 20,
            col12 TYPE c LENGTH 20,
            col13 TYPE c LENGTH 20,
          END OF ty_head.

  DATA : lt_head TYPE TABLE OF ty_head,
         wa_head TYPE ty_head.

  DATA : lv_head TYPE string.
  DATA : lv_data TYPE string.

  lv_file = p_fpath.
  OPEN DATASET lv_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE 0.
    WRITE :/ 'Unable to open file:', lv_file.
  ENDIF.

  wa_head-col1 = 'Package'.
  wa_head-col2 = 'Request/Task'.
  wa_head-col3 = 'Description'.
  wa_head-col4 = 'Released Status'.
  wa_head-col5 = 'Target System'.
  wa_head-col6 = 'Owner'.
  wa_head-col7 = 'Date of Last Change'.
  wa_head-col8 = 'Higher-Level Request'.
  wa_head-col9 = 'Approve'.
  wa_head-col10 = 'Decline'.
  wa_head-col11 = 'Code Inspector Check'.
  wa_head-col12 = 'TR Analyzer Check'.
  wa_head-col13 = 'TR Sequence'.
  APPEND wa_head TO lt_head.

  CLEAR lv_head.
  LOOP AT lt_head INTO wa_head.

    CONCATENATE wa_head-col1 wa_head-col8 wa_head-col3 wa_head-col4 wa_head-col5 wa_head-col6 wa_head-col7 wa_head-col2 wa_head-col9 wa_head-col10 wa_head-col11 wa_head-col12
    wa_head-col13 INTO lv_head SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
    TRANSFER lv_head TO lv_file.
  ENDLOOP.

  LOOP AT it_final INTO wa_final.
    CLEAR : lv_data.
*    CONCATENATE wa_final-package
    CONCATENATE wa_final-strkorr wa_final-as4text wa_final-trstdes wa_final-tarsystem wa_final-as4user wa_final-as4date wa_final-trkorr wa_final-approve wa_final-decline wa_final-slin_check wa_final-tr_analyzer wa_final-chk_seq
    INTO lv_data SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
    TRANSFER lv_data TO lv_file.
  ENDLOOP.

  CLOSE DATASET lv_file.

ENDFORM.

INCLUDE Ztr_appr_rej_statuso01.
*&---------------------------------------------------------------------*
*&      Form  CHECK_TR_SEQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_ROW  text
*      -->P_ES_COL_ID_FIELDNAME  text
*----------------------------------------------------------------------*
FORM CHECK_TR_SEQ  USING lv_ROW lv_fldname.

  DATA : LT_TR TYPE TABLE OF RSPARAMS,
       LS_TR TYPE RSPARAMS,
       lv_chgno(20) TYPE c,
       lv_text TYPE string,
       lv_pos TYPE i.

  READ TABLE it_disp INTO wa_final INDEX lv_row.
  CONDENSE wa_final-as4text.
  SEARCH wa_final-as4text FOR 'CHG'.
  IF sy-subrc eq 0.
   lv_pos = sy-fdpos.
   lv_chgno = wa_final-as4text+lv_pos(10).
  ENDIF.

IF lv_chgno IS INITIAL.
  MESSAGE 'Change request number not exists to view the TR List' TYPE 'I'.
  EXIT.
ENDIF.

LS_TR-SELNAME = 'P_CHGNO'.
LS_TR-KIND = 'P'.
LS_TR-SIGN = 'I'.
LS_TR-OPTION = 'EQ'.
LS_TR-LOW = lv_chgno.
APPEND LS_TR TO LT_TR.
CLEAR LS_TR.


SUBMIT ZTRANSPORT_SEQUENCE WITH SELECTION-TABLE LT_TR AND RETURN.
  REFRESH : LT_TR.

ENDFORM.                    " CHECK_TR_SEQ
