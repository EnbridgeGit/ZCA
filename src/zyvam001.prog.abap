REPORT ZYVAM001 MESSAGE-ID ZS NO STANDARD PAGE HEADING LINE-COUNT 65
                                                       LINE-SIZE  80.
*----------------------------------------------------------------------*
*  Author:
*    Selwyn Rodricks
*    OmniLogic Systems Group
*
*  Description:
*    This program creates the variables and their values in table TVARV.
*    It is run daily.
*
*  To add further entries in TVARV:
*    Please see on-line help of this program for details on where to
*    add source code.
*----------------------------------------------------------------------*
*                        C H A N G E S                                 *
* 1997/07/23 - PMURRAY1  ADDED VARIABLES ZAM_CUR_PERIOD AND            *
*                        ZAM_CUR_PERIOD_YEAR PER MCAUGHY               *
*                        REQUEST ORIGINATED FROM MURRAY CAUGHY         *
* 1997/10/31 - PMURRAY2  CHANGED VARIABLE RKGA2U-BOOK FROM SPACE TO X  *
* REPAIR D30K904612      IN FORM ZCO_PRKGA2U                           *
*                        CHANGED VARIABLE RKGA2U-BOOK FROM SPACE TO X  *
*                        IN FORM ZCO_PRKGA2U_M                         *
*                        REQUEST ORIGINATED FROM JAN MIERAS            *
* 1998/01/27 - PMURRAY3  ADDED VARIABLES ZFI_CUR_FISCAL_YEAR AND       *
* REPAIR D30K904944      ZFI_PRE_FISCAL_YEAR PER GERUS SIKORSKI        *
* DEVREQ DRXX0229                                                      *
*
* 1998/12/22  DSARTOR  ADD VARIABLE ZFI_NXT_FISCAL_YEAR AS PER
* D30K906681           GERUS SIKORSKI
*
* 2000/06/30  dsartor  added variable ZCO_CUR_DATE_H
*
* 2001/03/05  mdemeest #838 Added ZAM_FISCAL_YEAR changing on Jan 15
*
* 2003/10/14  mdemeest 1106 Added ZDUKE_PRE_FISCAL_YEAR and
*                           ZDUKE__PRE_FISCAL_PERIOD

* 2005/02/02  mdemeest 1089 Add ZDUKE_PER_FIRST_DAY per S. Chibbers
*                           for Beginning of Year Consolidation
* 2005/06/03  mdemeest #426 Added ZFI_ST_EST & ZFI_ST_EST_YEAR
*----------------------------------------------------------------------*
TABLES: TVARV,                         "Table of variables
        RKGA2U.                        "Structure for ZCO_PRKGA2U

DATA: BEGIN OF ENQ_KEY,                "Key for locking record
        NAME(30),
        TYPE LIKE TVARV-TYPE,
      END OF ENQ_KEY.

SELECTION-SCREEN SKIP 3.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-002.
PARAMETERS :   P_TESTRN AS CHECKBOX.   "For test run only
SELECTION-SCREEN END OF BLOCK BOX1.

START-OF-SELECTION.

*  SY-DATUM = SY-DATUM - 10.                          "Remove after test
*DO 20 TIMES.                                         "Remove after test
*  SY-DATUM = SY-DATUM + 1.                           "Remove after test
*  WRITE: / '----------'.                             "Remove after test
*  WRITE: / SY-DATUM.                                 "Remove after test
*  WRITE: / '----------'.                             "Remove after test

  PERFORM ZAM_PRE_PERIOD.              "To create ZAM_PRE_PERIOD
  PERFORM ZAM_PRE_PERIOD_YEAR.         "To create ZAM_PRE_PERIOD_YEAR

  PERFORM ZAM_CUR_PERIOD.                                   "<--PMURRAY1
  PERFORM ZAM_CUR_PERIOD_YEAR.                              "<--PMURRAY1

  perform ZAM_FISCAL_YEAR.                                  "mdemeest

  PERFORM ZMM_NXT_PERIOD.              "1st 5  days = current period
  PERFORM ZMM_NXT_PERIOD_YEAR.         "1st 5  days = current period

  PERFORM ZCO_CUR_PERIOD.              "Strictly datewise
  PERFORM ZCO_CUR_PERIOD_YEAR.         "Strictly datewise

  PERFORM ZCO_CUR_PERIOD_M.            "1st 10 days = last period
  PERFORM ZCO_CUR_PERIOD_YEAR_M.       "1st 10 days = last period year

  PERFORM ZCO_PRKGA2U.                 "Strictly datewise
  PERFORM ZCO_PRKGA2U_M.               "1st 10 days = last period

  PERFORM ZCO_CUR_DATE_M.           "1st 10 days = last day of last mth
  perform zco_cur_date_h.           "1st 15 days = last day of last mth
  PERFORM ZCO_BEG_DATE_M.           "1st 10 days = 1st day of last mth

  PERFORM ZMM_PRE_WEEK.             "Start(Sun) & End(Sat) of prev week

  PERFORM ZFI_CUR_FISCAL_YEAR.      "CURRENT FISCAL YEAR
  PERFORM ZFI_PRE_FISCAL_YEAR.      "CURRENT FISCAL YEAR -1
  PERFORM ZFI_NXT_FISCAL_YEAR.      "CURRENT FISCAL YEAR +1

  perform ZDUKE_PRE_PERIOD.
  perform ZDUKE_PRE_PERIOD_YEAR.
  perform ZDUKE_PER_FIRST_DAY.

  perform zfi_st_est.
  perform zfi_st_est_year.
*ENDDO.                                              "Remove after test

*---------------------------------------------------------------------*
*       FORM ZAM_PRE_PERIOD                                           *
*---------------------------------------------------------------------*
* This routine calculates the value of the variant and moves it to
* TVARV-LOW.  This value will be used when update to TVARV is done
*---------------------------------------------------------------------*
FORM ZAM_PRE_PERIOD.
  DATA: PRE_PERIOD      LIKE BKPF-MONAT.                    "numc(2)
  CLEAR TVARV.
  TVARV-NAME = 'ZAM_PRE_PERIOD'.

  PRE_PERIOD = SY-DATUM+4(2) - 1.
  IF PRE_PERIOD = 0.
    PRE_PERIOD = 12.
  ENDIF.
  TVARV-LOW = PRE_PERIOD.
  PERFORM CHANGE_VARIABLE.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM ZAM_PRE_PERIOD_YEAR                                      *
*---------------------------------------------------------------------*
* This routine calculates the value of the variant and moves it to
* TVARV-LOW.  This value will be used when update to TVARV is done
*---------------------------------------------------------------------*
FORM ZAM_PRE_PERIOD_YEAR.
  DATA: PRE_PERIOD      LIKE BKPF-MONAT.                    "numc(2)
  DATA: PRE_PERIOD_YEAR LIKE BKPF-GJAHR.                    "numc(4)
  CLEAR TVARV.
  TVARV-NAME = 'ZAM_PRE_PERIOD_YEAR'.

  PRE_PERIOD = SY-DATUM+4(2) - 1.
  IF PRE_PERIOD = 0.
    PRE_PERIOD = 12.
    PRE_PERIOD_YEAR = SY-DATUM(4) - 1.
  ELSE.
    PRE_PERIOD_YEAR = SY-DATUM(4).
  ENDIF.
  TVARV-LOW = PRE_PERIOD_YEAR.
  PERFORM CHANGE_VARIABLE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM ZAM_CUR_PERIOD                                           *
*---------------------------------------------------------------------*
FORM ZAM_CUR_PERIOD.
  CLEAR TVARV.
  TVARV-NAME = 'ZAM_CUR_PERIOD'.
  TVARV-LOW  = SY-DATUM+4(2).
  PERFORM CHANGE_VARIABLE.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM ZAM_CUR_PERIOD_YEAR                                      *
*---------------------------------------------------------------------*
FORM ZAM_CUR_PERIOD_YEAR.
  CLEAR TVARV.
  TVARV-NAME = 'ZAM_CUR_PERIOD_YEAR'.
  TVARV-LOW  = SY-DATUM(4).
  PERFORM CHANGE_VARIABLE.
ENDFORM.

*------------------------  ZAM_FISCAL_YEAR    ------------------------*
* This variable is updated with the current year on Jan 15            *
*---------------------------------------------------------------------*
FORM ZAM_FISCAL_YEAR.
  if sy-datum+4(4) >= '0115'.    " ==> January 15
     CLEAR TVARV.
     TVARV-NAME = 'ZAM_FISCAL_YEAR'.
     TVARV-LOW  = SY-DATUM(4).
     PERFORM CHANGE_VARIABLE.
  endif.
ENDFORM.




*---------------------------------------------------------------------*
*       FORM ZCO_CUR_PERIOD                                           *
*---------------------------------------------------------------------*
FORM ZCO_CUR_PERIOD.
  CLEAR TVARV.
  TVARV-NAME = 'ZCO_CUR_PERIOD'.
  TVARV-LOW  = SY-DATUM+4(2).
  PERFORM CHANGE_VARIABLE.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM ZCO_CUR_PERIOD_YEAR                                      *
*---------------------------------------------------------------------*
FORM ZCO_CUR_PERIOD_YEAR.
  CLEAR TVARV.
  TVARV-NAME = 'ZCO_CUR_PERIOD_YEAR'.
  TVARV-LOW  = SY-DATUM(4).
  PERFORM CHANGE_VARIABLE.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM ZMM_NXT_PERIOD                                           *
*---------------------------------------------------------------------*
FORM ZMM_NXT_PERIOD.
  DATA: NXT_PERIOD      LIKE BKPF-MONAT.                    "numc(2)
  CLEAR TVARV.
  TVARV-NAME = 'ZMM_NXT_PERIOD'.

  IF SY-DATUM+6(2) > 5.
    NXT_PERIOD = SY-DATUM+4(2) + 1.
  ELSE.
    NXT_PERIOD = SY-DATUM+4(2).
  ENDIF.

  IF NXT_PERIOD = 13.
    NXT_PERIOD = 1.
  ENDIF.
  TVARV-LOW = NXT_PERIOD.
  PERFORM CHANGE_VARIABLE.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM ZMM_NXT_PERIOD_YEAR                                      *
*---------------------------------------------------------------------*
FORM ZMM_NXT_PERIOD_YEAR.
  DATA: NXT_PERIOD      LIKE BKPF-MONAT.                    "numc(2)
  DATA: NXT_PERIOD_YEAR LIKE BKPF-GJAHR.                    "numc(4)
  CLEAR TVARV.
  TVARV-NAME = 'ZMM_NXT_PERIOD_YEAR'.

  IF SY-DATUM+6(2) > 5.
    NXT_PERIOD = SY-DATUM+4(2) + 1.
  ELSE.
    NXT_PERIOD = SY-DATUM+4(2).
  ENDIF.

  IF NXT_PERIOD = 13.
    NXT_PERIOD = 1.
    NXT_PERIOD_YEAR = SY-DATUM(4) + 1.
  ELSE.
    NXT_PERIOD_YEAR = SY-DATUM(4).
  ENDIF.
  TVARV-LOW = NXT_PERIOD_YEAR.
  PERFORM CHANGE_VARIABLE.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM ZCO_CUR_PERIOD_M                                         *
*---------------------------------------------------------------------*
FORM ZCO_CUR_PERIOD_M.
  DATA: PERIOD     LIKE BKPF-MONAT.    "numc(2)
  CLEAR TVARV.
  TVARV-NAME = 'ZCO_CUR_PERIOD_M'.

  IF SY-DATUM+6(2) > 10.
    PERIOD = SY-DATUM+4(2).
  ELSE.
    PERIOD = SY-DATUM+4(2) - 1.
  ENDIF.
  IF PERIOD = 0.
    PERIOD = 12.
  ENDIF.
  TVARV-LOW = PERIOD.
  PERFORM CHANGE_VARIABLE.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM ZCO_CUR_PERIOD_YEAR_M                                    *
*---------------------------------------------------------------------*
FORM ZCO_CUR_PERIOD_YEAR_M.
  DATA: PERIOD   LIKE BKPF-MONAT.      "numc(2)
  DATA: YEAR     LIKE BKPF-GJAHR.      "numc(4)
  CLEAR TVARV.
  TVARV-NAME = 'ZCO_CUR_PERIOD_YEAR_M'.

  YEAR = SY-DATUM(4).
  IF SY-DATUM+6(2) <= 10.
    PERIOD = SY-DATUM+4(2) - 1.
  ELSE.
    PERIOD = SY-DATUM+4(2).
  ENDIF.

  IF PERIOD = 0.
    YEAR = SY-DATUM(4) - 1.
  ENDIF.

  TVARV-LOW = YEAR.
  PERFORM CHANGE_VARIABLE.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM ZCO_PRKGA2U                                              *
*---------------------------------------------------------------------*
FORM ZCO_PRKGA2U.
  CLEAR TVARV.
  CLEAR RKGA2U.
  TVARV-NAME = 'ZCO_PRKGA2U'.

  RKGA2U-GJAHR    = SY-DATUM(4).
  RKGA2U-FROM     = SY-DATUM+4(2).
  RKGA2U-TO       = SY-DATUM+4(2).
* RKGA2U-BOOK     = SPACE.                          "-->DELETE(PMURRAY2)
  RKGA2U-BOOK     = 'X'.                            "<--INSERT(PMURRAY2)
  RKGA2U-LIST     = 'X'.
  RKGA2U-LISTSING = 'X'.
  RKGA2U-PLDAT    = '00000000'.
  RKGA2U-KURST    = 'M'.
  RKGA2U-ACTIVITY = 'RKIU'.
  RKGA2U-APPL     = 'S'.
  RKGA2U-DOCTY    = SPACE.
  RKGA2U-ANZBP    = '000'.

  TVARV-LOW = RKGA2U.
  PERFORM CHANGE_VARIABLE.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM ZCO_PRKGA2U_M                                            *
*---------------------------------------------------------------------*
FORM ZCO_PRKGA2U_M.
  CLEAR TVARV.
  CLEAR RKGA2U.
  TVARV-NAME = 'ZCO_PRKGA2U_M'.

  RKGA2U-GJAHR = SY-DATUM(4).
  IF SY-DATUM+6(2) > 10.
    RKGA2U-FROM = SY-DATUM+4(2).
  ELSE.
    RKGA2U-FROM = SY-DATUM+4(2) - 1.
  ENDIF.
  IF RKGA2U-FROM = 0.
    RKGA2U-FROM = 12.
    RKGA2U-GJAHR = SY-DATUM(4) - 1.
  ENDIF.

  RKGA2U-TO       = RKGA2U-FROM.
* RKGA2U-BOOK     = SPACE.                          "-->DELETE(PMURRAY2)
  RKGA2U-BOOK     = 'X'.                            "<--INSERT(PMURRAY2)
  RKGA2U-LIST     = 'X'.
  RKGA2U-LISTSING = 'X'.
  RKGA2U-PLDAT    = '00000000'.
  RKGA2U-KURST    = 'M'.
  RKGA2U-ACTIVITY = 'RKIU'.
  RKGA2U-APPL     = 'S'.
  RKGA2U-DOCTY    = SPACE.
  RKGA2U-ANZBP    = '000'.

  TVARV-LOW = RKGA2U.
  PERFORM CHANGE_VARIABLE.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM ZCO_CUR_DATE_M                                           *
*---------------------------------------------------------------------*
FORM ZCO_CUR_DATE_M.
  DATA: DATE          LIKE SY-DATUM.
  CLEAR TVARV.
  TVARV-NAME = 'ZCO_CUR_DATE_M'.

  DATE = SY-DATUM.
  IF SY-DATUM+6(2) <= 10.
    DATE+6(2) = '01'.
    DATE = DATE - 1.
  ENDIF.
  TVARV-LOW = DATE.
  PERFORM CHANGE_VARIABLE.
  PERFORM CHANGE_VARIABLE_S.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM ZCO_CUR_DATE_H                                           *
*---------------------------------------------------------------------*
FORM ZCO_CUR_DATE_H.
  DATA: DATE          LIKE SY-DATUM.
  CLEAR TVARV.
  TVARV-NAME = 'ZCO_CUR_DATE_H'.

  DATE = SY-DATUM.
  IF SY-DATUM+6(2) <= 15.
    DATE+6(2) = '01'.
    DATE = DATE - 1.
  ENDIF.
  TVARV-LOW = DATE.
  PERFORM CHANGE_VARIABLE.
  PERFORM CHANGE_VARIABLE_S.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM ZCO_BEG_DATE_M                                           *
*---------------------------------------------------------------------*
FORM ZCO_BEG_DATE_M.
  DATA: DATE          LIKE SY-DATUM.
  CLEAR TVARV.
  TVARV-NAME = 'ZCO_BEG_DATE_M'.

  DATE = SY-DATUM.
  DATE+6(2) = '01'.
  IF SY-DATUM+6(2) <= 10.
    DATE = DATE - 1.
    DATE+6(2) = '01'.
  ENDIF.

  TVARV-LOW = DATE.
  PERFORM CHANGE_VARIABLE.
  PERFORM CHANGE_VARIABLE_S.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM ZMM_PRE_WEEK (START & END)                               *
*---------------------------------------------------------------------*
FORM ZMM_PRE_WEEK.
  DATA: START_DATE          LIKE SY-DATUM,
        END_DATE            LIKE SY-DATUM,
        I_DAY               LIKE SCAL-INDICATOR.

  START_DATE = SY-DATUM - 13.

  DO 7 TIMES.
    CALL FUNCTION 'DATE_COMPUTE_DAY'                      "1=MON
         EXPORTING                                        "7=SUN
              DATE   = START_DATE
         IMPORTING
              DAY    = I_DAY
         EXCEPTIONS
              OTHERS = 1.
    IF I_DAY = 7.
      EXIT.
    ENDIF.
    START_DATE = START_DATE + 1.
  ENDDO.

  CLEAR TVARV.
  TVARV-NAME = 'ZMM_PRE_WEEK_START'.
  TVARV-LOW = START_DATE.
  PERFORM CHANGE_VARIABLE.

  CLEAR TVARV.
  TVARV-NAME = 'ZMM_PRE_WEEK_END'.
  END_DATE   = START_DATE + 6.
  TVARV-LOW = END_DATE.
  PERFORM CHANGE_VARIABLE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM ZFI_CUR_FISCAL_YEAR                                      *
*---------------------------------------------------------------------*
FORM ZFI_CUR_FISCAL_YEAR.
  CLEAR TVARV.
  TVARV-NAME = 'ZFI_CUR_FISCAL_YEAR'.
  TVARV-LOW  = SY-DATUM(4).
  PERFORM CHANGE_VARIABLE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM ZFI_PRE_FISCAL_YEAR                                      *
*---------------------------------------------------------------------*
FORM ZFI_PRE_FISCAL_YEAR.
  CLEAR TVARV.
  TVARV-NAME = 'ZFI_PRE_FISCAL_YEAR'.
  TVARV-LOW  = SY-DATUM(4) - 1.
  PERFORM CHANGE_VARIABLE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM ZFI_NXT_FISCAL_YEAR                                      *
*---------------------------------------------------------------------*
FORM ZFI_NXT_FISCAL_YEAR.
  CLEAR TVARV.
  TVARV-NAME = 'ZFI_NXT_FISCAL_YEAR'.
  TVARV-LOW  = SY-DATUM(4) + 1.
  PERFORM CHANGE_VARIABLE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM ZFI_PRE_FISCAL_PERIOD                                    *
*---------------------------------------------------------------------*
FORM ZDUKE_PRE_PERIOD.
  DATA: PRE_PERIOD      LIKE BKPF-MONAT.                    "numc(2)
  CLEAR TVARV.
  TVARV-NAME = 'ZDUKE_PRE_PERIOD'.

  PRE_PERIOD = SY-DATUM+4(2) - 1.
  IF PRE_PERIOD = 0.
    PRE_PERIOD = 12.
  ENDIF.
  TVARV-LOW = PRE_PERIOD.
  PERFORM CHANGE_VARIABLE.

ENDFORM.

FORM ZDUKE_PRE_PERIOD_YEAR.
  DATA: PRE_PERIOD      LIKE BKPF-MONAT.                    "numc(2)
  DATA: PRE_PERIOD_YEAR LIKE BKPF-GJAHR.                    "numc(4)
  CLEAR TVARV.
  TVARV-NAME = 'ZDUKE_PRE_PERIOD_YEAR'.

  PRE_PERIOD = SY-DATUM+4(2) - 1.
  IF PRE_PERIOD = 0.
    PRE_PERIOD = 12.
    PRE_PERIOD_YEAR = SY-DATUM(4) - 1.
  ELSE.
    PRE_PERIOD_YEAR = SY-DATUM(4).
  ENDIF.
  TVARV-LOW = PRE_PERIOD_YEAR.
  PERFORM CHANGE_VARIABLE.

ENDFORM.

*--------------------------- ZFI_ST_EST -------------------------------
form ZFI_ST_EST.
  data:  ref_doc like bkpf-xblnr,
         zdate   like bkpf-monat,
         zyear   like bkpf-gjahr.

  clear tvarv.
  tvarv-name = 'ZFI_ST_EST'.
  move 'S&T-EST-' to ref_doc.
  if sy-datum+6(2) < 24.
     compute zdate = sy-datum+4(2) - 1.
  else.
     compute zdate = sy-datum+4(2).
  endif.
  if zdate = 0.
     move '12' to zdate.
     compute zyear = sy-datum(4) - 1.
  else.
     zyear = sy-datum(4).
  endif.
  concatenate ref_doc zyear '/' zdate into ref_doc.

  tvarv-low = ref_doc.
  perform change_variable.
  perform change_variable_s.
endform.

*-------------------------- ZFI_ST_EST_YEAR ---------------------------
form ZFI_ST_EST_YEAR.
  data:  zyear  like bkpf-gjahr.

  clear tvarv.
  tvarv-name = 'ZFI_ST_EST_YEAR'.

  zyear = sy-datum(4).
  if sy-datum+4(4) < '0110'.
     compute zyear = zyear - 1.
  endif.

  tvarv-low = zyear.
  perform change_variable.
  perform change_variable_s.
endform.



*---------------------------------------------------------------------*
*       FORM ZDUKE_PER_FIRST_DAY                                      *
*---------------------------------------------------------------------*
*  Sets the date to the first day of the current month.               *
*---------------------------------------------------------------------*
FORM ZDUKE_PER_FIRST_DAY.
  DATA: DATE          LIKE SY-DATUM.
  CLEAR TVARV.
  TVARV-NAME = 'ZDUKE_PER_FIRST_DAY'.

  DATE = SY-DATUM.
  DATE+6(2) = '01'.
  TVARV-LOW = DATE.
  PERFORM CHANGE_VARIABLE.
  PERFORM CHANGE_VARIABLE_S.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM CHANGE_VARIABLE           (For Parameters)               *
*---------------------------------------------------------------------*
FORM CHANGE_VARIABLE.
  TVARV-TYPE = 'P'.
  TVARV-NUMB = '0000'.
  PERFORM ENQ_TVARV USING TVARV-NAME TVARV-TYPE.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.
  WRITE: / TVARV-NAME, TVARV-LOW.

  IF P_TESTRN <> 'X'.
    PERFORM SAVE_PARAMETER.
  ENDIF.
  PERFORM DEQ_TVARV USING TVARV-NAME TVARV-TYPE.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM CHANGE_VARIABLE_S         (For Select Options)           *
*---------------------------------------------------------------------*
FORM CHANGE_VARIABLE_S.
  TVARV-TYPE = 'S'.
  TVARV-NUMB = '0000'.
  TVARV-SIGN = 'I'.
  TVARV-OPTI = 'EQ'.

  PERFORM ENQ_TVARV USING TVARV-NAME TVARV-TYPE.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.
  WRITE: / TVARV-NAME, TVARV-LOW.

  IF P_TESTRN <> 'X'.
    PERFORM SAVE_PARAMETER.
  ENDIF.
  PERFORM DEQ_TVARV USING TVARV-NAME TVARV-TYPE.
ENDFORM.


*----------------------------------------------------------------------*
*       FORM ENQ_TVARV                                                 *
*----------------------------------------------------------------------*
*  -->  P_NAME      Name der Variablen
*       P_TYPE      Typ der Variablen
*----------------------------------------------------------------------*
FORM ENQ_TVARV USING
            P_NAME       LIKE TVARV-NAME
            P_TYPE       LIKE TVARV-TYPE.
  ENQ_KEY-NAME = P_NAME.
  ENQ_KEY-TYPE = P_TYPE.
  CALL FUNCTION 'ENQUEUE_ESVARV'
       EXPORTING
            NAME           = P_NAME
            TYPE           = P_TYPE
       EXCEPTIONS
            FOREIGN_LOCK   = 4
            SYSTEM_FAILURE = 8.
  CASE SY-SUBRC.
    WHEN 4.
      WRITE: P_NAME,  TEXT-310.
    WHEN 8.
      WRITE: ENQ_KEY, TEXT-320.
  ENDCASE.
ENDFORM.


*----------------------------------------------------------------------*
*       FORM DEQ_TVARV                                                 *
*----------------------------------------------------------------------*
*  -->  P_NAME      Name der Variablen
*       P_TYPE      Typ der Variablen
*----------------------------------------------------------------------*
FORM DEQ_TVARV USING
            P_NAME       LIKE TVARV-NAME
            P_TYPE       LIKE TVARV-TYPE.
  ENQ_KEY-NAME = P_NAME.
  ENQ_KEY-TYPE = P_TYPE.
  CALL FUNCTION 'DEQUEUE_ESVARV'
       EXPORTING
            NAME = P_NAME
            TYPE = P_TYPE.
ENDFORM.


*----------------------------------------------------------------------*
*       FORM SAVE_PARAMETER                                            *
*----------------------------------------------------------------------*
FORM SAVE_PARAMETER.
  MODIFY TVARV.
ENDFORM.


*---------------------------------------------------------------------*
*       top-of-page                                                   *
*---------------------------------------------------------------------*
TOP-OF-PAGE.
  FORMAT INTENSIFIED OFF.
  WRITE: / SY-REPID, SY-SYSID, 32 TEXT-001.
  WRITE:  63 TEXT-004, SY-DATUM.
  WRITE: /63 TEXT-006, SY-UZEIT.
  WRITE: /63 TEXT-005, SY-PAGNO.

  IF P_TESTRN = 'X'.
    WRITE: /32 TEXT-205.
  ENDIF.
  ULINE.
  WRITE: / TEXT-201, 32 TEXT-202.
  ULINE.
  FORMAT INTENSIFIED ON.
