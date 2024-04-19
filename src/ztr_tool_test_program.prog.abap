*&---------------------------------------------------------------------*
*& Report  ZTR_TOOL_TEST_PROGRAM
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZTR_TOOL_TEST_PROGRAM.

Write: 'Test Program for TR Tool'.

DATA: it_data TYPE STANDARD TABLE OF ZTEST_TR_TOOL,
      wa_data TYPE ztest_tr_tool,
      wa_data2 TYPE ztest_tr_tool.

SELECT * FROM ZTEST_TR_TOOL into table it_data.
IF sy-subrc eq 0.
 write: 'Data'.
ENDIF.

LOOP AT it_data INTO wa_data.
 LOOP AT it_data INTO wa_data2..

 ENDLOOP.
ENDLOOP.
