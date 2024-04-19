class ZCL_TEST_TR_TOOL definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_TEST_TR_TOOL
*"* do not include other source files here!!!

  methods ZTEST_M1 .
protected section.
*"* protected components of class ZCL_TEST_TR_TOOL
*"* do not include other source files here!!!

  data V_DATA type ZTEST_DELT_TR_TOOL .

  methods ZTEST_M2 .
private section.
*"* private components of class ZCL_TEST_TR_TOOL
*"* do not include other source files here!!!

  methods ZTEST_M3 .
ENDCLASS.



CLASS ZCL_TEST_TR_TOOL IMPLEMENTATION.


method ZTEST_M1.

*DATA: it_data TYPE STANDARD TABLE OF ZTEST_TR_TOOL,
*      wa_data TYPE ztest_tr_tool,
*      wa_data2 TYPE ztest_tr_tool.
*
*SELECT * FROM ZTEST_TR_TOOL into table it_data.
*IF sy-subrc eq 0.
** write: 'Data'.
*ENDIF.

endmethod.


method ZTEST_M2.

DATA: it_data TYPE STANDARD TABLE OF ZTEST_TR_TOOL,
      wa_data TYPE ztest_tr_tool,
      wa_data2 TYPE ztest_tr_tool.

SELECT * FROM ZTEST_TR_TOOL into table it_data.
IF sy-subrc eq 0.
* write: 'Data'.
ENDIF.

endmethod.


method ZTEST_M3.

*  DATA: v_data TYPE ZTEST_DELT_TR_TOOL.
*
*  WRITE: v_data.

endmethod.
ENDCLASS.
