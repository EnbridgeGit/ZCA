*&---------------------------------------------------------------------*
*&  Include           ZTR_APPR_REJ_STATUSO01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_TEXTEDIT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_TEXTEDIT OUTPUT.
IF textnote_editor IS INITIAL.

*   set status
    SET PF-STATUS 'TEXTEDIT'.
    if not textnote_edit_mode is initial.
       SET TITLEBAR 'TEXTEDIT'.
    else.
       SET TITLEBAR 'TEXTDISP'.
    endif.

*   create control container
    CREATE OBJECT textnote_custom_container
        EXPORTING
            container_name = 'TEXTEDITOR1'
        EXCEPTIONS
            cntl_error = 1
            cntl_system_error = 2
            create_error = 3
            lifetime_error = 4
            lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc NE 0.
*      add your handling
    ENDIF.
    textnote_container = 'TEXTEDITOR1'.

*   create calls constructor, which initializes, creats and links
*   TextEdit Control
    CREATE OBJECT textnote_editor
          EXPORTING
           parent = textnote_custom_container
           wordwrap_mode =
*               cl_gui_textedit=>wordwrap_off
              cl_gui_textedit=>wordwrap_at_fixed_position
*              cl_gui_textedit=>WORDWRAP_AT_WINDOWBORDER
           wordwrap_position = textnoteline_length
           wordwrap_to_linebreak_mode = cl_gui_textedit=>true.

  ENDIF.

  CALL METHOD textnote_custom_container->link
          EXPORTING
               repid = textnote_repid
               container = textnote_container.

*           show toolbar and statusbar on this screen
  CALL METHOD textnote_editor->set_toolbar_mode
     EXPORTING
         toolbar_mode = textnote_editor->true.
  CALL METHOD textnote_editor->set_statusbar_mode
     EXPORTING
         statusbar_mode = textnote_editor->true.

* Set edit mode
  if textnote_edit_mode is initial.
     call METHOD textnote_editor->set_readonly_mode.
  endif.

*   send table to control
  textnote_table[] = textnote_itxw_note[].
  CALL METHOD textnote_editor->set_text_as_r3table
          EXPORTING table = textnote_table.

* finally flush
  CALL METHOD cl_gui_cfw=>flush
         EXCEPTIONS
           OTHERS = 1.
  IF sy-subrc NE 0.
*   add your handling
  ENDIF.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_TEXTEDIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_TEXTEDIT INPUT.
  CASE textnote_ok_code.

    WHEN 'BACK'.
      PERFORM back_program.

    WHEN 'CONT'.
*   retrieve table from control
      CALL METHOD textnote_editor->get_text_as_r3table
              IMPORTING table = textnote_table.
      textnote_itxw_note[] = textnote_table[].
      PERFORM back_program.

    WHEN 'EXIT'.
      PERFORM back_program.

    WHEN 'BREAK'.
      PERFORM back_program.

    WHEN 'CANC'.
      PERFORM back_program.

  ENDCASE.

  CLEAR textnote_ok_code.


ENDMODULE.

FORM back_program.
*     Destroy Control.
  IF NOT textnote_editor IS INITIAL.
    CALL METHOD textnote_editor->free
      EXCEPTIONS
          OTHERS = 1.
    IF sy-subrc NE 0.
*         add your handling
    ENDIF.
*       free ABAP object also
    FREE textnote_editor.
  ENDIF.

*     destroy container
  IF NOT textnote_custom_container IS INITIAL.
    CALL METHOD textnote_custom_container->free
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
*         add your handling
    ENDIF.
*       free ABAP object also
    FREE textnote_custom_container.
  ENDIF.

  CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
          OTHERS = 1.
  IF sy-subrc NE 0.
*         add your handling
  ENDIF.

  LEAVE TO SCREEN 0.

ENDFORM.                               " BACK_PROGRAM
