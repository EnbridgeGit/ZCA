*&---------------------------------------------------------------------*
*& Report  ZTRANSPORT_SEQUENCE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZTRANSPORT_SEQUENCE.

TYPES: BEGIN OF ty_e07t,
         trkorr  TYPE trkorr,
         as4text TYPE as4text,
       END OF ty_e07t,

      BEGIN OF ty_e070,
         trkorr    TYPE trkorr,
         trstatus  TYPE trstatus,
         as4user   TYPE tr_as4user,
         as4date   TYPE as4date,
         as4time   TYPE AS4TIME,
         strkorr   TYPE strkorr,
       END OF ty_e070,

       BEGIN OF ty_output,
         strkorr   TYPE strkorr,
         as4text TYPE as4text,
         trstatus  TYPE trstatus,
         as4user   TYPE tr_as4user,
         as4date   TYPE as4date,
         as4time   TYPE AS4TIME,
       END OF ty_output.

**Internal Table & Work Area declaration
DATA: it_e07t TYPE TABLE OF ty_e07t,
      wa_e07t TYPE ty_e07t.

DATA: it_e070 TYPE TABLE OF ty_e070,
      wa_e070 TYPE ty_e070.

DATA: lt_fieldcat TYPE lvc_t_fcat,
     wa_layout   TYPE lvc_s_layo,
     ls_fieldcat TYPE lvc_s_fcat,
     it_output TYPE STANDARD TABLE OF ty_output,
     wa_output TYPE ty_output,
     oref_alv    TYPE REF TO cl_gui_alv_grid.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(37) text-002.
PARAMETER: p_chgno TYPE char20 OBLIGATORY.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

START-OF-SELECTION.

  PERFORM get_transports.

  PERFORM output_data.

END-OF-SELECTION.

  PERFORM display_data.
*&---------------------------------------------------------------------*
*&      Form  GET_TRANSPORTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_TRANSPORTS .

  DATA: lv_trtext TYPE AS4TEXT.

*  lv_trtext = p_chgno.
 TRANSLATE p_chgno TO UPPER CASE.

  CONCATENATE '%' p_chgno '%' INTO lv_trtext.

  SELECT trkorr as4text
       FROM e07t
       INTO TABLE it_e07t
     WHERE AS4TEXT LIKE lv_trtext.
  IF sy-subrc EQ 0.
    SELECT trkorr trstatus as4user as4date as4time strkorr
       FROM e070
       INTO TABLE it_e070
      FOR ALL ENTRIES IN it_e07t
     WHERE STRKORR EQ it_e07t-trkorr.
    IF sy-subrc EQ 0.
       SORT it_e070 by as4date as4time.
    ENDIF.
  ELSE.
    MESSAGE i001(00) WITH 'No TR available for the Change request'.
    EXIT.
  ENDIF.


ENDFORM.                    " GET_TRANSPORTS
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OUTPUT_DATA .
REFRESH it_output.
CLEAR wa_output.
SORT it_e070 by strkorr as4date DESCENDING.
delete ADJACENT DUPLICATES FROM it_e070 COMPARING strkorr.
LOOP AT it_e070 INTO wa_e070.
  wa_output-strkorr = wa_e070-strkorr.
  wa_output-trstatus = wa_e070-trstatus.
  wa_output-as4user  = wa_e070-as4user.
  wa_output-as4date  = wa_e070-as4date.
  wa_output-as4time  =  wa_e070-as4time.
  READ TABLE it_e07t INTO wa_e07t WITH KEY trkorr = wa_e070-strkorr.
  IF sy-subrc eq 0.
    wa_output-as4text = wa_e07t-AS4TEXT.
  ENDIF.
 Append wa_output to it_output.
 CLEAR wa_output.
ENDLOOP.

SORT it_output BY as4date as4time trstatus DESCENDING.

ENDFORM.                    " OUTPUT_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_DATA .

IF it_output IS NOT INITIAL.
  ls_fieldcat-col_pos = '1'.
  ls_fieldcat-fieldname = 'STRKORR'.
  ls_fieldcat-tabname   = 'IT_OUTPUT'.
  ls_fieldcat-reptext = 'Transport'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-col_pos = '2'.
  ls_fieldcat-fieldname = 'AS4TEXT'.
  ls_fieldcat-tabname   = 'IT_OUTPUT'.
  ls_fieldcat-reptext = 'Description'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

ls_fieldcat-col_pos = '3'.
  ls_fieldcat-fieldname = 'TRSTATUS'.
  ls_fieldcat-tabname   = 'IT_OUTPUT'.
  ls_fieldcat-reptext = 'TR Status'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-col_pos = '4'.
  ls_fieldcat-fieldname = 'AS4USER'.
  ls_fieldcat-tabname   = 'IT_OUTPUT'.
  ls_fieldcat-reptext = 'TR Owner'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-col_pos = '5'.
  ls_fieldcat-fieldname = 'AS4DATE'.
  ls_fieldcat-tabname   = 'IT_OUTPUT'.
  ls_fieldcat-reptext = 'TR Rel/Creation Date'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.

  ls_fieldcat-col_pos = '6'.
  ls_fieldcat-fieldname = 'AS4TIME'.
  ls_fieldcat-tabname   = 'IT_OUTPUT'.
  ls_fieldcat-reptext = 'TR Rel/Creation Time'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  wa_layout-cwidth_opt = 'X'.

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
            it_outtab                     = it_output
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
  CALL SCREEN 0100.
ENDIF.

ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
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

ENDMODULE.                 " USER_COMMAND_0100  INPUT
