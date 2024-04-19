class ZCL_IM_CTS_REQUEST_CHECK definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_CTS_REQUEST_CHECK
*"* do not include other source files here!!!

  interfaces IF_EX_CTS_REQUEST_CHECK .
protected section.
*"* protected components of class ZCL_IM_CTS_REQUEST_CHECK
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_CTS_REQUEST_CHECK
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_CTS_REQUEST_CHECK IMPLEMENTATION.


method IF_EX_CTS_REQUEST_CHECK~CHECK_BEFORE_ADD_OBJECTS.
endmethod.


method IF_EX_CTS_REQUEST_CHECK~CHECK_BEFORE_CHANGING_OWNER.
endmethod.


method IF_EX_CTS_REQUEST_CHECK~CHECK_BEFORE_CREATION.

  DATA: it_ZFIT TYPE STANDARD TABLE OF ZTRT_CONFIG,
          wa_zfit TYPE ZTRT_CONFIG.
  DATA: lv_request TYPE TRKORR.

  DATA: lv_chgno(20) TYPE c,
      lv_text TYPE string,
      lv_len TYPE i,
      lv_warn_msg type c,
      lv_pos TYPE i.

  SELECT * FROM ZTRT_CONFIG INTO TABLE it_zfit WHERE PARAMTYPE = 'ZTRANSPORT'.
  IF sy-subrc EQ 0.
    READ TABLE it_zfit INTO wa_zfit WITH KEY SUBTYPE = 'REL_DATE'.
    IF sy-subrc EQ 0.
      IF sy-datum GE wa_zfit-value1.
*      ELSE.
        READ TABLE it_zfit INTO wa_zfit WITH KEY SUBTYPE = 'USER_ID'
                                                 value1 = sy-uname.
        IF sy-subrc NE 0.
*          EXIT.
          lv_warn_msg = 'X'.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ELSE.
    EXIT.
  ENDIF.


  IF text is NOT INITIAL.
*    SPLIT text at '-' INTO lv_chgno lv_text.
*    CONDENSE lv_chgno.
*    TRANSLATE lv_chgno TO UPPER CASE.
    SEARCH text FOR 'CHG'.
    IF sy-subrc eq 0.
      lv_pos = sy-fdpos.
      lv_chgno = text+lv_pos(10).
      CONDENSE lv_chgno.
      TRANSLATE lv_chgno TO UPPER CASE.
      lv_len = strlen( lv_chgno ).

      IF lv_len NE 10.
        IF lv_warn_msg = 'X'.
          MESSAGE 'Change request number is not valid (10 chars required), please correct in TR properties' TYPE 'I'.
        ELSE.
          MESSAGE 'Change request number is not valid (10 chars required)' TYPE 'E'.
          EXIT.
        ENDIF.
      ENDIF.
      IF lv_chgno+3(10) ca sy-abcde.
        IF lv_warn_msg = 'X'.
          MESSAGE 'Change request number is not valid, please correct in TR properties' TYPE 'I'.
        ELSE.
          MESSAGE 'Change request number is not valid' TYPE 'E'.
          EXIT.
        ENDIF.
      ENDIF.
    ELSE.
      IF lv_warn_msg = 'X'.
        MESSAGE 'Change request number is required in Description, please correct in TR properties' TYPE 'I'.
      ELSE.
        MESSAGE 'Change request number is required in Description' TYPE 'E'.
        EXIT.
      ENDIF.
    ENDIF..
  ENDIF.

endmethod.


method IF_EX_CTS_REQUEST_CHECK~CHECK_BEFORE_RELEASE.
  TYPES : BEGIN OF TY_E071,
              TRKORR(20) TYPE C,
              OBJ_NAME(120) TYPE C,
             END OF TY_E071.
*  TYPES : BEGIN OF TY_TADIR,
*          OBJ_NAME(120) TYPE C,
*          DEVCLASS(30) TYPE C,
*         END OF TY_TADIR.
  DATA : it_e070a TYPE STANDARD TABLE OF e070a,
         wa_e070a TYPE e070a,
         it_e071 TYPE STANDARD TABLE OF ty_e071,
         wa_e071 TYPE ty_e071,
*         it_tadir TYPE STANDARD TABLE OF ty_tadir,
*         wa_tadir TYPE ty_tadir,
         it_e070 TYPE STANDARD TABLE OF e070,
         wa_e070 TYPE e070,
         it_ZFIT TYPE STANDARD TABLE OF ZTRT_CONFIG,
         wa_zfit TYPE ZTRT_CONFIG.

 DATA: lv_request TYPE TRKORR,
       lv_value   TYPE TRVALUE.

  SELECT * FROM ZTRT_CONFIG INTO TABLE it_zfit WHERE PARAMTYPE = 'ZTRANSPORT'.
  IF sy-subrc EQ 0.
    READ TABLE it_zfit INTO wa_zfit WITH KEY SUBTYPE = 'REL_DATE'.
    IF sy-subrc EQ 0.
      IF sy-datum GE wa_zfit-value1.
*      ELSE.
        READ TABLE it_zfit INTO wa_zfit WITH KEY SUBTYPE = 'USR_REL_CHK'
                                                 value1 = sy-uname.
        IF sy-subrc NE 0.
          EXIT.
        ENDIF.
       ELSE.
         EXIT.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ELSE.
    EXIT.
  ENDIF.


  SELECT * FROM e070 INTO TABLE it_e070 where trkorr = request.
  IF SY-SUBRC eq 0.
*        READ TABLE it_e070 into wa_e070 index 1.
*        IF wa_e070-TRFUNCTION = 'K' or wa_e070-TRFUNCTION = 'W'.
*        IF request(3) eq 'NRD'.

    SELECT TRKORR OBJ_NAME FROM e071 INTO TABLE it_e071 where trkorr = request.
    IF SY-SUBRC EQ 0.
*          LOOP AT IT_E071 INTO WA_E071.
*          SELECT OBJ_NAME DEVCLASS FROM tadir INTO wa_tadir where OBJ_NAME = wa_e071-obj_name.
*          APPEND wa_tadir to it_tadir.
*          ENDSELECT.
*          ENDLOOP.
*            IF SY-SUBRC EQ 0.
*              READ TABLE it_tadir INTO wa_tadir WITH KEY DEVCLASS = p_dvclas  //Package name
**             ENDIF.
*
*              IF sy-SUBRC EQ 0.

      SELECT *
      FROM e070a
      INTO TABLE it_e070a
     WHERE trkorr = request.
      IF sy-subrc ne 0.
        READ TABLE it_e070 INTO wa_e070 WITH KEY trkorr = request.
        IF sy-subrc eq 0 and wa_e070-strkorr IS NOT INITIAL.
          SELECT *
            FROM e070a
            INTO TABLE it_e070a
            WHERE trkorr = wa_e070-STRKORR.
          IF sy-subrc eq 0.
             lv_request = wa_e070-STRKORR.
          ENDIF.
        ENDIF.
       ELSE.
         lv_request = request.
      ENDIF.


      IF it_e070a IS  NOT INITIAL.
        READ TABLE it_e070a INTO wa_e070a WITH KEY trkorr = lv_request
                                                attribute = 'ZTRANSPORT'.
        IF sy-subrc = 0.
          SPLIT wa_e070a-reference AT '-' INTO wa_e070a-reference lv_value.
          IF wa_e070a-reference = 'DECLINED'.
            MESSAGE 'TR is in Rejected Status, please review the TR before release' TYPE 'E'.
            EXIT.
          ELSEIF wa_e070a-reference EQ ' '.
            MESSAGE 'TR is not yet Approved for Release' TYPE 'E'.
            EXIT.
          ENDIF.
        ELSE.
          MESSAGE 'Kindly approve the TR to release it.' TYPE 'E'.
          EXIT.
        ENDIF.

      ENDIF.
*    ENDIF.
*    ENDIF.
    ENDIF.
*    ENDIF.
*    ENDIF.
*    ENDIF.
  ENDIF.


endmethod.
ENDCLASS.
