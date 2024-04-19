REPORT zyvam001 MESSAGE-ID zs NO STANDARD PAGE HEADING LINE-COUNT 65
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
* 2006/06/12  mdemeest 4.7 upgrade TVARV changed to TVARVC
* 2011/01/03  gymana   TR804 COG added new routines to calculate first
*                            and last days of the previous, current,
*                            and next period.
* 2012/11/05  gymana  SDP24716 Adding new standardized variables as
*                     EHP5     part of the TVARV clean up.
* 2015/04/27  skapse  SDP84648 Z_BPC_YYYYMM variant Update with
*                              correct date
* 2015/06/17  gymana  SDP86050 Add Z_CURR_YEAR_JAN1 for first day of
*                              current year.
* 2017/05/17  jrhartung ACR-4322 D30K928232 - Add Payroll Period
*                                D30K928350 - Remove Z_CURR_YEAR_JAN1
*---------------------------------------------------------------------*
TABLES: tvarvc,                         "Table of variables
        rkga2u.                        "Structure for ZCO_PRKGA2U

DATA: BEGIN OF enq_key,                "Key for locking record
        name(30),
        type LIKE tvarvc-type,
      END OF enq_key.

SELECTION-SCREEN SKIP 3.
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-002.
PARAMETERS :   p_testrn AS CHECKBOX.   "For test run only
SELECTION-SCREEN END OF BLOCK box1.

START-OF-SELECTION.
*----------------------------------------------------------------------*
* Standardized variables                                               *
*----------------------------------------------------------------------*
* 2012/11/05 GYMANA SDP24716-------------------------------------------*

  PERFORM set_up_curr_period_dates.
  PERFORM set_up_prev_period_dates.
  PERFORM set_up_next_period_dates.
  PERFORM set_up_next2_period_dates.
  PERFORM setup_start_end_weeks_dates.

* 2012/11/05 GYMANA SDP24716-------------------------------------------*

  PERFORM zam_pre_period.              "To create ZAM_PRE_PERIOD
  PERFORM zam_pre_period_year.         "To create ZAM_PRE_PERIOD_YEAR

  PERFORM zam_cur_period.                                   "<--PMURRAY1
  PERFORM zam_cur_period_year.                              "<--PMURRAY1

  PERFORM zam_fiscal_year.                                  "mdemeest

  PERFORM zmm_nxt_period.              "1st 5  days = current period
  PERFORM zmm_nxt_period_year.         "1st 5  days = current period

  PERFORM zmm_prev_per_first_lastday.  "First & Last Day of Prev period
  PERFORM zmm_curr_per_first_lastday.  "First & Last Day of Curr period
  PERFORM zmm_next_per_first_lastday.  "First & Last Day of Next period

  PERFORM zco_cur_period.              "Strictly datewise
  PERFORM zco_cur_period_year.         "Strictly datewise

  PERFORM zco_cur_period_m.            "1st 10 days = last period
  PERFORM zco_cur_period_year_m.       "1st 10 days = last period year

  PERFORM zco_prkga2u.                 "Strictly datewise
  PERFORM zco_prkga2u_m.               "1st 10 days = last period

  PERFORM zco_cur_date_m.           "1st 10 days = last day of last mth
  PERFORM zco_cur_date_h.           "1st 15 days = last day of last mth
  PERFORM zco_beg_date_m.           "1st 10 days = 1st day of last mth

  PERFORM zmm_pre_week.             "Start(Sun) & End(Sat) of prev week

  PERFORM zfi_cur_fiscal_year.      "CURRENT FISCAL YEAR
  PERFORM zfi_pre_fiscal_year.      "CURRENT FISCAL YEAR -1
  PERFORM zfi_nxt_fiscal_year.      "CURRENT FISCAL YEAR +1

  PERFORM zduke_pre_period.
  PERFORM zduke_pre_period_year.
  PERFORM zduke_per_first_day.

  PERFORM zfi_st_est.
  PERFORM zfi_st_est_year.

  PERFORM f_payroll_period.                                 "D30K928232

*----------------------------------------------------------------------*
* FORM SET_UP_CURR_PERIOD_DATES                                        *
*----------------------------------------------------------------------*
* Please use the variables in this form for all current period related
* dates.
*-2012/11/05 GYMANA SDP24716-------------------------------------------*

FORM set_up_curr_period_dates.

* Z_CURR_PERIOD.
  CLEAR tvarvc.
  tvarvc-name = 'Z_CURR_PERIOD'.
  tvarvc-low  = sy-datum+4(2).
  PERFORM change_variable.
  PERFORM change_variable_s.

* Z_CURR_PERIOD_YEAR.
  CLEAR tvarvc.
  tvarvc-name = 'Z_CURR_PERIOD_YEAR'.
  tvarvc-low  = sy-datum(4).
  PERFORM change_variable.
  PERFORM change_variable_s.

* Z_CURR_CAL_YEAR
  CLEAR tvarvc.
  tvarvc-name = 'Z_CURR_CAL_YEAR'.
  tvarvc-low  = sy-datum(4).
  PERFORM change_variable.
  PERFORM change_variable_s.

* Z_CURR_YYYYMM.
  CLEAR tvarvc.
  tvarvc-name = 'Z_CURR_YYYYMM'.
  CONCATENATE sy-datum(4) sy-datum+4(2) INTO tvarvc-low.
  PERFORM change_variable.
  PERFORM change_variable_s.

* Z_CURR_PER_FIRSTDAY
  DATA: firstdate  LIKE sy-datum,
        lastdate   LIKE sy-datum.

  CLEAR: tvarvc, firstdate, lastdate.
  tvarvc-name = 'Z_CURR_PER_FIRSTDAY'.

  firstdate = sy-datum.
  firstdate+6(2) = '01'.
  tvarvc-low = firstdate.
  PERFORM change_variable.
  PERFORM change_variable_s.

*Z_CURR_PER_LASTDAY

  CLEAR tvarvc.
  tvarvc-name = 'Z_CURR_PER_LASTDAY'.

  CALL FUNCTION 'SLS_MISC_GET_LAST_DAY_OF_MONTH'
    EXPORTING
      day_in            = firstdate
    IMPORTING
      last_day_of_month = lastdate.

  tvarvc-low = lastdate.
  PERFORM change_variable.
  PERFORM change_variable_s.

*Z_BPC_YYYYMM.
  DATA lv_period TYPE char7."jahrper.
  clear lv_period. " SKAPSE Ticket # 84648 04/27/2015
  CLEAR tvarvc.
  tvarvc-name = 'Z_BPC_YYYYMM_PREV'.
  "begin of SKAPSE 05/06/2015.
  DATA: prv_period1      LIKE bkpf-monat.                    "numc(2)
  DATA: prv_period_year1 LIKE bkpf-gjahr.                    "numc(4)
*-------Calculate_PREV_PER_FIRSTDAY-------------------------------*
  clear: prv_period1,prv_period_year1.

  prv_period1 = sy-datum+4(2) - 1.

  IF prv_period1 = 0.
    prv_period1 = 12.
    prv_period_year1 = sy-datum(4) - 1.
  ELSE.
    prv_period_year1 = sy-datum(4).
  ENDIF.
  "CONCATENATE sy-datum(4) '.' sy-datum+4(2) INTO lv_period. " SKAPSE Ticket # 84648 04/27/2015
  " CONCATENATE sy-datum+4(2) '/'  sy-datum(4) INTO lv_period. " SKAPSE Ticket # 84648 04/27/2015
  CONCATENATE   prv_period_year1 '/'  prv_period1 INTO lv_period. " SKAPSE Ticket # 84648 05/04/2015
  "end of SKAPSE 05/06/2015.
  CALL FUNCTION 'CONVERSION_EXIT_PERI7_INPUT'
    EXPORTING
      input           = lv_period
    IMPORTING
      output          = tvarvc-low
    EXCEPTIONS
      input_not_valid = 1
      OTHERS          = 2.

  PERFORM change_variable.
  PERFORM change_variable_s.

  " Z_BPC_YYYYMM_PREV " SKAPSE 05/07/2015.
  CLEAR tvarvc.
  clear lv_period.
  tvarvc-name = 'Z_BPC_YYYYMM'.

  CONCATENATE   sy-datum(4) '/'  sy-datum+4(2) INTO lv_period.
  CALL FUNCTION 'CONVERSION_EXIT_PERI7_INPUT'
    EXPORTING
      input           = lv_period
    IMPORTING
      output          = tvarvc-low
    EXCEPTIONS
      input_not_valid = 1
      OTHERS          = 2.

  PERFORM change_variable.
  PERFORM change_variable_s.

ENDFORM.                    "SET_UP_CURR_PERIOD_DATES
*----------------------------------------------------------------------*
* FORM SET_UP_PREV_PERIOD_DATES                                        *
*----------------------------------------------------------------------*
* Please use the variables in this form for all previous period related
* dates.
*-2012/11/05 GYMANA SDP24716-------------------------------------------*

FORM set_up_prev_period_dates.

  DATA: prev_period      LIKE bkpf-monat.
  DATA: prev_period_year LIKE bkpf-gjahr.

* Z_PREV_PERIOD.
  CLEAR: tvarvc, prev_period, prev_period_year.
  tvarvc-name = 'Z_PREV_PERIOD'.
  prev_period = sy-datum+4(2) - 1.
  IF prev_period = 0.
    prev_period = 12.
  ENDIF.
  tvarvc-low  = prev_period.
  PERFORM change_variable.
  PERFORM change_variable_s.

* Z_PREV_PERIOD_YEAR.
  CLEAR tvarvc.
  tvarvc-name = 'Z_PREV_PERIOD_YEAR'.
  prev_period = sy-datum+4(2) - 1.
  IF prev_period = 0.
    prev_period = 12.
    prev_period_year = sy-datum(4) - 1.
  ELSE.
    prev_period_year = sy-datum(4).
  ENDIF.
  tvarvc-low = prev_period_year.
  PERFORM change_variable.
  PERFORM change_variable_s.

* Z_PREV_CAL_YEAR
  CLEAR tvarvc.
  tvarvc-name = 'Z_PREV_CAL_YEAR'.
  tvarvc-low  = sy-datum(4) - 1.
  PERFORM change_variable.
  PERFORM change_variable_s.

* Z_PREV_YYYYMM.
  CLEAR tvarvc.
  tvarvc-name = 'Z_PREV_YYYYMM'.
  CONCATENATE prev_period_year prev_period INTO tvarvc-low.
  PERFORM change_variable.
  PERFORM change_variable_s.

* Z_PREV_PER_FIRSTDAY
  DATA: firstdate  LIKE sy-datum,
        lastdate   LIKE sy-datum.

  CLEAR: tvarvc, firstdate, lastdate.
  tvarvc-name = 'Z_PREV_PER_FIRSTDAY'.

  firstdate(4) = prev_period_year.
  firstdate+4(2) = prev_period.
  firstdate+6(2) = '01'.
  tvarvc-low = firstdate.
  PERFORM change_variable.
  PERFORM change_variable_s.

*Z_PREV_PER_LASTDAY

  CLEAR tvarvc.
  tvarvc-name = 'Z_PREV_PER_LASTDAY'.

  CALL FUNCTION 'SLS_MISC_GET_LAST_DAY_OF_MONTH'
    EXPORTING
      day_in            = firstdate
    IMPORTING
      last_day_of_month = lastdate.

  tvarvc-low = lastdate.
  PERFORM change_variable.
  PERFORM change_variable_s.

ENDFORM.                    "SET_UP_PREV_PERIOD_DATES

*----------------------------------------------------------------------*
* FORM SET_UP_NEXT_PERIOD_DATES                                        *
*----------------------------------------------------------------------*
* Please use the variables in this form for all next period related
* dates.
*-2012/11/05 GYMANA SDP24716-------------------------------------------*

FORM set_up_next_period_dates.

  DATA: next_period      LIKE bkpf-monat.
  DATA: next_period_year LIKE bkpf-gjahr.

* Z_NEXT_PERIOD.
  CLEAR: tvarvc, next_period, next_period_year.
  tvarvc-name = 'Z_NEXT_PERIOD'.
  next_period = sy-datum+4(2) + 1.
  IF next_period = 13.
    next_period = 1.
  ENDIF.
  tvarvc-low  = next_period.
  PERFORM change_variable.
  PERFORM change_variable_s.

* Z_NEXT_PERIOD_YEAR.
  CLEAR tvarvc.
  tvarvc-name = 'Z_NEXT_PERIOD_YEAR'.
  next_period = sy-datum+4(2) + 1.
  IF next_period = 13.
    next_period = 1.
    next_period_year = sy-datum(4) + 1.
  ELSE.
    next_period_year = sy-datum(4).
  ENDIF.
  tvarvc-low = next_period_year.
  PERFORM change_variable.
  PERFORM change_variable_s.

* Z_NEXT_CAL_YEAR
  CLEAR tvarvc.
  tvarvc-name = 'Z_NEXT_CAL_YEAR'.
  tvarvc-low  = sy-datum(4) + 1.
  PERFORM change_variable.
  PERFORM change_variable_s.

* Z_NEXT_YYYYMM.
  CLEAR tvarvc.
  tvarvc-name = 'Z_NEXT_YYYYMM'.
  CONCATENATE next_period_year next_period INTO tvarvc-low.
  PERFORM change_variable.
  PERFORM change_variable_s.

* Z_NEXT_PER_FIRSTDAY
  DATA: firstdate  LIKE sy-datum,
        lastdate   LIKE sy-datum.

  CLEAR: tvarvc, firstdate, lastdate.
  tvarvc-name = 'Z_NEXT_PER_FIRSTDAY'.

  firstdate(4) = next_period_year.
  firstdate+4(2) = next_period.
  firstdate+6(2) = '01'.
  tvarvc-low = firstdate.
  PERFORM change_variable.
  PERFORM change_variable_s.

*Z_NEXT_PER_LASTDAY

  CLEAR tvarvc.
  tvarvc-name = 'Z_NEXT_PER_LASTDAY'.

  CALL FUNCTION 'SLS_MISC_GET_LAST_DAY_OF_MONTH'
    EXPORTING
      day_in            = firstdate
    IMPORTING
      last_day_of_month = lastdate.

  tvarvc-low = lastdate.
  PERFORM change_variable.
  PERFORM change_variable_s.

ENDFORM.                    "SET_UP_NEXT_PERIOD_DATES

*----------------------------------------------------------------------*
* FORM SET_UP_NEXT2_PERIOD_DATES                                       *
*----------------------------------------------------------------------*
* Please use the variables in this form for all next2
* (current period + 2) related dates
*-2012/11/05 GYMANA SDP24716-------------------------------------------*

FORM set_up_next2_period_dates.

  DATA: next2_period      LIKE bkpf-monat.
  DATA: next2_period_year LIKE bkpf-gjahr.

* Z_NEXT2_PERIOD.
  CLEAR: tvarvc, next2_period, next2_period_year.
  tvarvc-name = 'Z_NEXT2_PERIOD'.
  next2_period = sy-datum+4(2) + 2.

  IF next2_period = 13.
    next2_period = 1.
  ELSEIF next2_period = 14.
    next2_period = 2.
  ENDIF.

  tvarvc-low  = next2_period.
  PERFORM change_variable.
  PERFORM change_variable_s.

* Z_NEXT2_PERIOD_YEAR.
  CLEAR tvarvc.
  tvarvc-name = 'Z_NEXT2_PERIOD_YEAR'.
  next2_period = sy-datum+4(2) + 2.
  next2_period_year = sy-datum(4).
  IF next2_period = 13.
    next2_period = 1.
    next2_period_year = sy-datum(4) + 1.
  ELSEIF next2_period = 14.
    next2_period = 2.
    next2_period_year = sy-datum(4) + 1.
  ENDIF.

  tvarvc-low = next2_period_year.
  PERFORM change_variable.
  PERFORM change_variable_s.

* Z_NEXT2_YYYYMM.
  CLEAR tvarvc.
  tvarvc-name = 'Z_NEXT2_YYYYMM'.
  CONCATENATE next2_period_year next2_period INTO tvarvc-low.
  PERFORM change_variable.
  PERFORM change_variable_s.

ENDFORM.                    "SET_UP_NEXT2_PERIOD_DATES

*----------------------------------------------------------------------*
* FORM SET_UP_START_END_WEEKS_DATES                                    *
*----------------------------------------------------------------------*
* This form will set up the first and last day (SUN-SAT) of the        *
* previous, current, and next week (based on system date)              *
*-2012/11/05 GYMANA SDP24716-------------------------------------------*

FORM setup_start_end_weeks_dates.

  DATA : indate        TYPE sy-datum,
         week_firstday TYPE sy-datum,
         week_lastday  TYPE sy-datum.

*Z_CURR_WEEK_START (SUNDAY)
*Z_CURR_WEEK_END (SATURDAY)

  indate = sy-datum.
* Function returns first weekday (Monday)
  CALL FUNCTION 'BWSO_DATE_GET_FIRST_WEEKDAY'
    EXPORTING
      date_in  = indate
    IMPORTING
      date_out = week_firstday.

  week_firstday = week_firstday - 1.
  week_lastday = week_firstday + 6.

  CLEAR tvarvc.
  tvarvc-name = 'Z_CURR_WEEK_START'.
  tvarvc-low  = week_firstday.
  PERFORM change_variable.
  PERFORM change_variable_s.

  CLEAR tvarvc.
  tvarvc-name = 'Z_CURR_WEEK_END'.
  tvarvc-low  = week_lastday.
  PERFORM change_variable.
  PERFORM change_variable_s.

*Z_PREV_WEEK_START (SUNDAY)
*Z_PREV_WEEK_END (SATURDAY)

  CLEAR: indate, week_firstday, week_lastday.

* Subtract 7 days for previous week.
  indate = sy-datum - 7.
* Function returns first weekday (Monday)
  CALL FUNCTION 'BWSO_DATE_GET_FIRST_WEEKDAY'
    EXPORTING
      date_in  = indate
    IMPORTING
      date_out = week_firstday.

* Calculate the first(SUN) and last(SAT) day of week
  week_firstday = week_firstday - 1.
  week_lastday = week_firstday + 6.

  CLEAR tvarvc.
  tvarvc-name = 'Z_PREV_WEEK_START'.
  tvarvc-low  = week_firstday.
  PERFORM change_variable.
  PERFORM change_variable_s.

  CLEAR tvarvc.
  tvarvc-name = 'Z_PREV_WEEK_END'.
  tvarvc-low  = week_lastday.
  PERFORM change_variable.
  PERFORM change_variable_s.

*Z_NEXT_WEEK_START (SUNDAY)
*Z_NEXT_WEEK_END (SATURDAY)

  CLEAR: indate, week_firstday, week_lastday.

* Add 7 days for next week.
  indate = sy-datum + 7.
* Function returns first weekday (Monday)
  CALL FUNCTION 'BWSO_DATE_GET_FIRST_WEEKDAY'
    EXPORTING
      date_in  = indate
    IMPORTING
      date_out = week_firstday.

* Calculate the first(SUN) and last(SAT) day of week
  week_firstday = week_firstday - 1.
  week_lastday = week_firstday + 6.

  CLEAR tvarvc.
  tvarvc-name = 'Z_NEXT_WEEK_START'.
  tvarvc-low  = week_firstday.
  PERFORM change_variable.
  PERFORM change_variable_s.

  CLEAR tvarvc.
  tvarvc-name = 'Z_NEXT_WEEK_END'.
  tvarvc-low  = week_lastday.
  PERFORM change_variable.
  PERFORM change_variable_s.

ENDFORM.                    "SETUP_START_END_WEEKS_DATES

*---------------------------------------------------------------------*
*       FORM ZAM_PRE_PERIOD                                           *
*---------------------------------------------------------------------*
* This routine calculates the value of the variant and moves it to
* TVARVC-LOW.  This value will be used when update to TVARVC is done
*---------------------------------------------------------------------*
FORM zam_pre_period.
  DATA: pre_period      LIKE bkpf-monat.                    "numc(2)
  CLEAR tvarvc.
  tvarvc-name = 'ZAM_PRE_PERIOD'.

  pre_period = sy-datum+4(2) - 1.
  IF pre_period = 0.
    pre_period = 12.
  ENDIF.
  tvarvc-low = pre_period.
  PERFORM change_variable.
ENDFORM.                    "ZAM_PRE_PERIOD


*---------------------------------------------------------------------*
*       FORM ZAM_PRE_PERIOD_YEAR                                      *
*---------------------------------------------------------------------*
* This routine calculates the value of the variant and moves it to
* TVARVC-LOW.  This value will be used when update to TVARVC is done
*---------------------------------------------------------------------*
FORM zam_pre_period_year.
  DATA: pre_period      LIKE bkpf-monat.                    "numc(2)
  DATA: pre_period_year LIKE bkpf-gjahr.                    "numc(4)
  CLEAR tvarvc.
  tvarvc-name = 'ZAM_PRE_PERIOD_YEAR'.

  pre_period = sy-datum+4(2) - 1.
  IF pre_period = 0.
    pre_period = 12.
    pre_period_year = sy-datum(4) - 1.
  ELSE.
    pre_period_year = sy-datum(4).
  ENDIF.
  tvarvc-low = pre_period_year.
  PERFORM change_variable.
ENDFORM.                    "ZAM_PRE_PERIOD_YEAR

*---------------------------------------------------------------------*
*       FORM ZAM_CUR_PERIOD                                           *
*---------------------------------------------------------------------*
FORM zam_cur_period.
  CLEAR tvarvc.
  tvarvc-name = 'ZAM_CUR_PERIOD'.
  tvarvc-low  = sy-datum+4(2).
  PERFORM change_variable.
ENDFORM.                    "ZAM_CUR_PERIOD


*---------------------------------------------------------------------*
*       FORM ZAM_CUR_PERIOD_YEAR                                      *
*---------------------------------------------------------------------*
FORM zam_cur_period_year.
  CLEAR tvarvc.
  tvarvc-name = 'ZAM_CUR_PERIOD_YEAR'.
  tvarvc-low  = sy-datum(4).
  PERFORM change_variable.
ENDFORM.                    "ZAM_CUR_PERIOD_YEAR

*------------------------  ZAM_FISCAL_YEAR    ------------------------*
* This variable is updated with the current year on Jan 15            *
*---------------------------------------------------------------------*
FORM zam_fiscal_year.
  IF sy-datum+4(4) >= '0115'.    " ==> January 15
    CLEAR tvarvc.
    tvarvc-name = 'ZAM_FISCAL_YEAR'.
    tvarvc-low  = sy-datum(4).
    PERFORM change_variable.
  ENDIF.
ENDFORM.                    "ZAM_FISCAL_YEAR




*---------------------------------------------------------------------*
*       FORM ZCO_CUR_PERIOD                                           *
*---------------------------------------------------------------------*
FORM zco_cur_period.
  CLEAR tvarvc.
  tvarvc-name = 'ZCO_CUR_PERIOD'.
  tvarvc-low  = sy-datum+4(2).
  PERFORM change_variable.
ENDFORM.                    "ZCO_CUR_PERIOD


*---------------------------------------------------------------------*
*       FORM ZCO_CUR_PERIOD_YEAR                                      *
*---------------------------------------------------------------------*
FORM zco_cur_period_year.
  CLEAR tvarvc.
  tvarvc-name = 'ZCO_CUR_PERIOD_YEAR'.
  tvarvc-low  = sy-datum(4).
  PERFORM change_variable.
ENDFORM.                    "ZCO_CUR_PERIOD_YEAR


*---------------------------------------------------------------------*
*       FORM ZMM_NXT_PERIOD                                           *
*---------------------------------------------------------------------*
FORM zmm_nxt_period.
  DATA: nxt_period      LIKE bkpf-monat.                    "numc(2)
  CLEAR tvarvc.
  tvarvc-name = 'ZMM_NXT_PERIOD'.

  IF sy-datum+6(2) > 5.
    nxt_period = sy-datum+4(2) + 1.
  ELSE.
    nxt_period = sy-datum+4(2).
  ENDIF.

  IF nxt_period = 13.
    nxt_period = 1.
  ENDIF.
  tvarvc-low = nxt_period.
  PERFORM change_variable.
ENDFORM.                    "ZMM_NXT_PERIOD


*---------------------------------------------------------------------*
*       FORM ZMM_NXT_PERIOD_YEAR                                      *
*---------------------------------------------------------------------*
FORM zmm_nxt_period_year.
  DATA: nxt_period      LIKE bkpf-monat.                    "numc(2)
  DATA: nxt_period_year LIKE bkpf-gjahr.                    "numc(4)
  CLEAR tvarvc.
  tvarvc-name = 'ZMM_NXT_PERIOD_YEAR'.

  IF sy-datum+6(2) > 5.
    nxt_period = sy-datum+4(2) + 1.
  ELSE.
    nxt_period = sy-datum+4(2).
  ENDIF.

  IF nxt_period = 13.
    nxt_period = 1.
    nxt_period_year = sy-datum(4) + 1.
  ELSE.
    nxt_period_year = sy-datum(4).
  ENDIF.
  tvarvc-low = nxt_period_year.
  PERFORM change_variable.
ENDFORM.                    "ZMM_NXT_PERIOD_YEAR

*---------------------------------------------------------------------*
*       FORM ZMM_CURR_PER_FIRST_LASTDAY                               *
*---------------------------------------------------------------------*
FORM zmm_curr_per_first_lastday.
  DATA: firstdate  LIKE sy-datum,
        lastdate   LIKE sy-datum.

*-------Calculate ZMM_CURR_PER_FIRSTDAY-------------------------------*

  CLEAR: tvarvc, firstdate, lastdate.
  tvarvc-name = 'ZMM_CURR_PER_FIRSTDAY'.

  firstdate = sy-datum.
  firstdate+6(2) = '01'.
  tvarvc-low = firstdate.
  PERFORM change_variable.
  PERFORM change_variable_s.

*-------Calculate ZMM_CURR_PER_LASTDAY--------------------------------*

  CLEAR tvarvc.
  tvarvc-name = 'ZMM_CURR_PER_LASTDAY'.

  CALL FUNCTION 'SLS_MISC_GET_LAST_DAY_OF_MONTH'
    EXPORTING
      day_in            = firstdate
    IMPORTING
      last_day_of_month = lastdate.

  tvarvc-low = lastdate.
  PERFORM change_variable.
  PERFORM change_variable_s.
ENDFORM.                    "ZMM_CURR_PER_FIRST_LASTDAY

*---------------------------------------------------------------------*
*       FORM ZMM_NEXT_PER_FIRST_LASTDAY                               *
*---------------------------------------------------------------------*
FORM zmm_next_per_first_lastday.
  DATA: firstdate  LIKE sy-datum,
        lastdate   LIKE sy-datum.
  DATA: nxt_period      LIKE bkpf-monat.                    "numc(2)
  DATA: nxt_period_year LIKE bkpf-gjahr.                    "numc(4)

*-------Calculate ZMM_NEXT_PER_FIRSTDAY-------------------------------*

  CLEAR: tvarvc, firstdate, lastdate.
  tvarvc-name = 'ZMM_NEXT_PER_FIRSTDAY'.

  nxt_period = sy-datum+4(2) + 1.

  IF nxt_period = 13.
    nxt_period = 1.
    nxt_period_year = sy-datum(4) + 1.
  ELSE.
    nxt_period_year = sy-datum(4).
  ENDIF.

  CONCATENATE nxt_period_year nxt_period '01' INTO firstdate.
  tvarvc-low = firstdate.
  PERFORM change_variable.
  PERFORM change_variable_s.

*-------Calculate ZMM_NEXT_PER_LASTDAY-------------------------------*

  CLEAR tvarvc.
  tvarvc-name = 'ZMM_NEXT_PER_LASTDAY'.

  CALL FUNCTION 'SLS_MISC_GET_LAST_DAY_OF_MONTH'
    EXPORTING
      day_in            = firstdate
    IMPORTING
      last_day_of_month = lastdate.

  tvarvc-low = lastdate.
  PERFORM change_variable.
  PERFORM change_variable_s.
ENDFORM.                    "ZMM_NEXT_PER_FIRST_LASTDAY

*---------------------------------------------------------------------*
*       FORM ZMM_PREV_PER_FIRST_LASTDAY                               *
*---------------------------------------------------------------------*
FORM zmm_prev_per_first_lastday.
  DATA: firstdate  LIKE sy-datum,
        lastdate   LIKE sy-datum.
  DATA: prv_period      LIKE bkpf-monat.                    "numc(2)
  DATA: prv_period_year LIKE bkpf-gjahr.                    "numc(4)

*-------Calculate ZMM_PREV_PER_FIRSTDAY-------------------------------*

  CLEAR: tvarvc, firstdate, lastdate.
  tvarvc-name = 'ZMM_PREV_PER_FIRSTDAY'.

  prv_period = sy-datum+4(2) - 1.

  IF prv_period = 0.
    prv_period = 12.
    prv_period_year = sy-datum(4) - 1.
  ELSE.
    prv_period_year = sy-datum(4).
  ENDIF.

  CONCATENATE prv_period_year prv_period '01' INTO firstdate.
  tvarvc-low = firstdate.
  PERFORM change_variable.
  PERFORM change_variable_s.

*-------Calculate ZMM_PREV_PER_LASTDAY-------------------------------*

  CLEAR tvarvc.
  tvarvc-name = 'ZMM_PREV_PER_LASTDAY'.

  CALL FUNCTION 'SLS_MISC_GET_LAST_DAY_OF_MONTH'
    EXPORTING
      day_in            = firstdate
    IMPORTING
      last_day_of_month = lastdate.

  tvarvc-low = lastdate.
  PERFORM change_variable.
  PERFORM change_variable_s.
ENDFORM.                    "ZMM_PREV_PER_FIRST_LASTDAY

*---------------------------------------------------------------------*
*       FORM ZCO_CUR_PERIOD_M                                         *
*---------------------------------------------------------------------*
FORM zco_cur_period_m.
  DATA: period     LIKE bkpf-monat.    "numc(2)
  CLEAR tvarvc.
  tvarvc-name = 'ZCO_CUR_PERIOD_M'.

  IF sy-datum+6(2) > 10.
    period = sy-datum+4(2).
  ELSE.
    period = sy-datum+4(2) - 1.
  ENDIF.
  IF period = 0.
    period = 12.
  ENDIF.
  tvarvc-low = period.
  PERFORM change_variable.
ENDFORM.                    "ZCO_CUR_PERIOD_M


*---------------------------------------------------------------------*
*       FORM ZCO_CUR_PERIOD_YEAR_M                                    *
*---------------------------------------------------------------------*
FORM zco_cur_period_year_m.
  DATA: period   LIKE bkpf-monat.      "numc(2)
  DATA: year     LIKE bkpf-gjahr.      "numc(4)
  CLEAR tvarvc.
  tvarvc-name = 'ZCO_CUR_PERIOD_YEAR_M'.

  year = sy-datum(4).
  IF sy-datum+6(2) <= 10.
    period = sy-datum+4(2) - 1.
  ELSE.
    period = sy-datum+4(2).
  ENDIF.

  IF period = 0.
    year = sy-datum(4) - 1.
  ENDIF.

  tvarvc-low = year.
  PERFORM change_variable.
ENDFORM.                    "ZCO_CUR_PERIOD_YEAR_M


*---------------------------------------------------------------------*
*       FORM ZCO_PRKGA2U                                              *
*---------------------------------------------------------------------*
FORM zco_prkga2u.
  CLEAR tvarvc.
  CLEAR rkga2u.
  tvarvc-name = 'ZCO_PRKGA2U'.

  rkga2u-gjahr    = sy-datum(4).
  rkga2u-from     = sy-datum+4(2).
  rkga2u-to       = sy-datum+4(2).
* RKGA2U-BOOK     = SPACE.                          "-->DELETE(PMURRAY2)
  rkga2u-book     = 'X'.                            "<--INSERT(PMURRAY2)
  rkga2u-list     = 'X'.
  rkga2u-listsing = 'X'.
  rkga2u-pldat    = '00000000'.
  rkga2u-kurst    = 'M'.
  rkga2u-activity = 'RKIU'.
  rkga2u-appl     = 'S'.
  rkga2u-docty    = space.
  rkga2u-anzbp    = '000'.

  tvarvc-low = rkga2u.
  PERFORM change_variable.
ENDFORM.                    "ZCO_PRKGA2U


*---------------------------------------------------------------------*
*       FORM ZCO_PRKGA2U_M                                            *
*---------------------------------------------------------------------*
FORM zco_prkga2u_m.
  CLEAR tvarvc.
  CLEAR rkga2u.
  tvarvc-name = 'ZCO_PRKGA2U_M'.

  rkga2u-gjahr = sy-datum(4).
  IF sy-datum+6(2) > 10.
    rkga2u-from = sy-datum+4(2).
  ELSE.
    rkga2u-from = sy-datum+4(2) - 1.
  ENDIF.
  IF rkga2u-from = 0.
    rkga2u-from = 12.
    rkga2u-gjahr = sy-datum(4) - 1.
  ENDIF.

  rkga2u-to       = rkga2u-from.
* RKGA2U-BOOK     = SPACE.                          "-->DELETE(PMURRAY2)
  rkga2u-book     = 'X'.                            "<--INSERT(PMURRAY2)
  rkga2u-list     = 'X'.
  rkga2u-listsing = 'X'.
  rkga2u-pldat    = '00000000'.
  rkga2u-kurst    = 'M'.
  rkga2u-activity = 'RKIU'.
  rkga2u-appl     = 'S'.
  rkga2u-docty    = space.
  rkga2u-anzbp    = '000'.

  tvarvc-low = rkga2u.
  PERFORM change_variable.
ENDFORM.                    "ZCO_PRKGA2U_M


*---------------------------------------------------------------------*
*       FORM ZCO_CUR_DATE_M                                           *
*---------------------------------------------------------------------*
FORM zco_cur_date_m.
  DATA: date          LIKE sy-datum.
  CLEAR tvarvc.
  tvarvc-name = 'ZCO_CUR_DATE_M'.

  date = sy-datum.
  IF sy-datum+6(2) <= 10.
    date+6(2) = '01'.
    date = date - 1.
  ENDIF.
  tvarvc-low = date.
  PERFORM change_variable.
  PERFORM change_variable_s.
ENDFORM.                    "ZCO_CUR_DATE_M


*---------------------------------------------------------------------*
*       FORM ZCO_CUR_DATE_H                                           *
*---------------------------------------------------------------------*
FORM zco_cur_date_h.
  DATA: date          LIKE sy-datum.
  CLEAR tvarvc.
  tvarvc-name = 'ZCO_CUR_DATE_H'.

  date = sy-datum.
  IF sy-datum+6(2) <= 15.
    date+6(2) = '01'.
    date = date - 1.
  ENDIF.
  tvarvc-low = date.
  PERFORM change_variable.
  PERFORM change_variable_s.
ENDFORM.                    "ZCO_CUR_DATE_H


*---------------------------------------------------------------------*
*       FORM ZCO_BEG_DATE_M                                           *
*---------------------------------------------------------------------*
FORM zco_beg_date_m.
  DATA: date          LIKE sy-datum.
  CLEAR tvarvc.
  tvarvc-name = 'ZCO_BEG_DATE_M'.

  date = sy-datum.
  date+6(2) = '01'.
  IF sy-datum+6(2) <= 10.
    date = date - 1.
    date+6(2) = '01'.
  ENDIF.

  tvarvc-low = date.
  PERFORM change_variable.
  PERFORM change_variable_s.
ENDFORM.                    "ZCO_BEG_DATE_M


*---------------------------------------------------------------------*
*       FORM ZMM_PRE_WEEK (START & END)                               *
*---------------------------------------------------------------------*
FORM zmm_pre_week.
  DATA: start_date          LIKE sy-datum,
        end_date            LIKE sy-datum,
        i_day               LIKE scal-indicator.

  start_date = sy-datum - 13.

  DO 7 TIMES.
    CALL FUNCTION 'DATE_COMPUTE_DAY'                      "1=MON
         EXPORTING                                        "7=SUN
              date   = start_date
         IMPORTING
              day    = i_day
         EXCEPTIONS
              OTHERS = 1.
    IF i_day = 7.
      EXIT.
    ENDIF.
    start_date = start_date + 1.
  ENDDO.

  CLEAR tvarvc.
  tvarvc-name = 'ZMM_PRE_WEEK_START'.
  tvarvc-low = start_date.
  PERFORM change_variable.

  CLEAR tvarvc.
  tvarvc-name = 'ZMM_PRE_WEEK_END'.
  end_date   = start_date + 6.
  tvarvc-low = end_date.
  PERFORM change_variable.
ENDFORM.                    "ZMM_PRE_WEEK

*---------------------------------------------------------------------*
*       FORM ZFI_CUR_FISCAL_YEAR                                      *
*---------------------------------------------------------------------*
FORM zfi_cur_fiscal_year.
  CLEAR tvarvc.
  tvarvc-name = 'ZFI_CUR_FISCAL_YEAR'.
  tvarvc-low  = sy-datum(4).
  PERFORM change_variable.
ENDFORM.                    "ZFI_CUR_FISCAL_YEAR

*---------------------------------------------------------------------*
*       FORM ZFI_PRE_FISCAL_YEAR                                      *
*---------------------------------------------------------------------*
FORM zfi_pre_fiscal_year.
  CLEAR tvarvc.
  tvarvc-name = 'ZFI_PRE_FISCAL_YEAR'.
  tvarvc-low  = sy-datum(4) - 1.
  PERFORM change_variable.
ENDFORM.                    "ZFI_PRE_FISCAL_YEAR

*---------------------------------------------------------------------*
*       FORM ZFI_NXT_FISCAL_YEAR                                      *
*---------------------------------------------------------------------*
FORM zfi_nxt_fiscal_year.
  CLEAR tvarvc.
  tvarvc-name = 'ZFI_NXT_FISCAL_YEAR'.
  tvarvc-low  = sy-datum(4) + 1.
  PERFORM change_variable.
ENDFORM.                    "ZFI_NXT_FISCAL_YEAR

*---------------------------------------------------------------------*
*       FORM ZFI_PRE_FISCAL_PERIOD                                    *
*---------------------------------------------------------------------*
FORM zduke_pre_period.
  DATA: pre_period      LIKE bkpf-monat.                    "numc(2)
  CLEAR tvarvc.
  tvarvc-name = 'ZDUKE_PRE_PERIOD'.

  pre_period = sy-datum+4(2) - 1.
  IF pre_period = 0.
    pre_period = 12.
  ENDIF.
  tvarvc-low = pre_period.
  PERFORM change_variable.

ENDFORM.                    "ZDUKE_PRE_PERIOD

*&---------------------------------------------------------------------*
*&      Form  ZDUKE_PRE_PERIOD_YEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zduke_pre_period_year.
  DATA: pre_period      LIKE bkpf-monat.                    "numc(2)
  DATA: pre_period_year LIKE bkpf-gjahr.                    "numc(4)
  CLEAR tvarvc.
  tvarvc-name = 'ZDUKE_PRE_PERIOD_YEAR'.

  pre_period = sy-datum+4(2) - 1.
  IF pre_period = 0.
    pre_period = 12.
    pre_period_year = sy-datum(4) - 1.
  ELSE.
    pre_period_year = sy-datum(4).
  ENDIF.
  tvarvc-low = pre_period_year.
  PERFORM change_variable.

ENDFORM.                    "ZDUKE_PRE_PERIOD_YEAR

*--------------------------- ZFI_S&T_EST -------------------------------
FORM zfi_st_est.
  DATA:  ref_doc LIKE bkpf-xblnr,
         zdate   LIKE bkpf-monat,
         zyear   LIKE bkpf-gjahr.

  CLEAR tvarvc.
  tvarvc-name = 'ZFI_ST_EST'.
  MOVE 'S&T-EST-' TO ref_doc.
  IF sy-datum+6(2) < 24.
    COMPUTE zdate = sy-datum+4(2) - 1.
  ELSE.
    COMPUTE zdate = sy-datum+4(2).
  ENDIF.
  IF zdate = 0.
    MOVE '12' TO zdate.
    COMPUTE zyear = sy-datum(4) - 1.
  ELSE.
    zyear = sy-datum(4).
  ENDIF.
  CONCATENATE ref_doc zyear '/' zdate INTO ref_doc.

  tvarvc-low = ref_doc.
  PERFORM change_variable.
  PERFORM change_variable_s.
ENDFORM.                    "ZFI_ST_EST

*-------------------------- ZFI_S&T_EST_YEAR ---------------------------
FORM zfi_st_est_year.
  DATA:  zyear  LIKE bkpf-gjahr.

  CLEAR tvarvc.
  tvarvc-name = 'ZFI_ST_EST_YEAR'.

  zyear = sy-datum(4).
  IF sy-datum+4(4) < '0110'.
    COMPUTE zyear = zyear - 1.
  ENDIF.

  tvarvc-low = zyear.
  PERFORM change_variable.
  PERFORM change_variable_s.
ENDFORM.                    "ZFI_ST_EST_YEAR
*eject
*&---------------------------------------------------------------------*
*&      Form  f_payroll_period                              "D30K928232
*&---------------------------------------------------------------------*
*       TVARVC variables for payroll period
*----------------------------------------------------------------------*
FORM f_payroll_period.                                      "D30K928232

  TYPES:  BEGIN OF ty_wa_abkrs,
           abkrs                       TYPE abkrs,
          END   OF ty_wa_abkrs.

  DATA:    ls_zvar                     TYPE zvar,
           lt_zvar                     TYPE STANDARD TABLE OF zvar,
           ls_abkrs                    TYPE ty_wa_abkrs,
           lt_abkrs                    TYPE STANDARD TABLE
                                         OF ty_wa_abkrs,
           ls_t549a                    TYPE t549a,
           lt_t549a                    TYPE STANDARD TABLE OF t549a,
           ls_t549q                    TYPE t549q,
           lt_t549q                    TYPE STANDARD TABLE OF t549q.

  DATA:    lv_pabrj                    TYPE pabrj.

  CONSTANTS:
           lc_varname_pa               TYPE z_varname
                                       VALUE 'PAYROLL_AREA'.

  CLEAR    lt_zvar[].
  CLEAR    lt_abkrs[].
  CLEAR    lt_t549a[].
  CLEAR    lt_t549q[].

  CLEAR                                     lv_pabrj.
  MOVE     sy-datum+0(4)                 TO lv_pabrj.

* Select the program parameters containing the payroll area values
  SELECT   *
    INTO   TABLE lt_zvar
    FROM   zvar
   WHERE   programm = sy-cprog
     AND   varname  = lc_varname_pa.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

* Build the payroll area keys
  CLEAR                                     ls_zvar.
  LOOP AT  lt_zvar                     INTO ls_zvar.
    CLEAR                                   ls_abkrs.
    MOVE   ls_zvar-value1                TO ls_abkrs-abkrs.
    APPEND ls_abkrs                      TO lt_abkrs.
    CLEAR  ls_zvar.
  ENDLOOP.

*eject
* Select the payroll areas
  SELECT   *
    INTO   TABLE lt_t549a
    FROM   t549a FOR ALL ENTRIES IN lt_abkrs
   WHERE   abkrs = lt_abkrs-abkrs.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

* Select the payroll periods
  SELECT   *
    INTO   TABLE lt_t549q
    FROM   t549q FOR ALL ENTRIES IN lt_t549a
   WHERE   permo = lt_t549a-permo
     AND   pabrj = lv_pabrj.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

* Build the tvarvc variables for payroll period
  SORT     lt_t549a ASCENDING BY abkrs.
  SORT     lt_t549q ASCENDING BY permo pabrj pabrp.

  CLEAR                                     ls_t549a.
  LOOP AT  lt_t549a                    INTO ls_t549a.

    CLEAR                                   ls_t549q.
    LOOP AT  lt_t549q                  INTO ls_t549q
                                      WHERE permo  = ls_t549a-permo
                                        AND begda <= sy-datum
                                        AND endda >= sy-datum.

      CLEAR                                 tvarvc.
      CONCATENATE         'ZPAYROLLPERIOD_' ls_t549a-abkrs '_CURR'
                                       INTO tvarvc-name.
      MOVE     ls_t549q-pabrp            TO tvarvc-low.

      PERFORM  change_variable.

      CLEAR                                 tvarvc.
      CONCATENATE         'ZPAYROLLYEAR_'   ls_t549a-abkrs '_CURR'
                                       INTO tvarvc-name.
      MOVE     ls_t549q-pabrj            TO tvarvc-low.

      PERFORM  change_variable.

      EXIT.

      CLEAR  ls_t549q.
    ENDLOOP.

    CLEAR  ls_t549a.
  ENDLOOP.

ENDFORM.                    " f_payroll_period              "D30K928232
*eject
*---------------------------------------------------------------------*
*       FORM ZDUKE_PER_FIRST_DAY                                      *
*---------------------------------------------------------------------*
*  Sets the date to the first day of the current month.               *
*---------------------------------------------------------------------*
FORM zduke_per_first_day.
  DATA: date          LIKE sy-datum.
  CLEAR tvarvc.
  tvarvc-name = 'ZDUKE_PER_FIRST_DAY'.

  date = sy-datum.
  date+6(2) = '01'.
  tvarvc-low = date.
  PERFORM change_variable.
  PERFORM change_variable_s.
ENDFORM.                    "ZDUKE_PER_FIRST_DAY


*---------------------------------------------------------------------*
*       FORM CHANGE_VARIABLE           (For Parameters)               *
*---------------------------------------------------------------------*
FORM change_variable.
  tvarvc-type = 'P'.
  tvarvc-numb = '0000'.
  PERFORM enq_tvarvc USING tvarvc-name tvarvc-type.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  WRITE: / tvarvc-name, tvarvc-low.

  IF p_testrn <> 'X'.
    PERFORM save_parameter.
  ENDIF.
  PERFORM deq_tvarvc USING tvarvc-name tvarvc-type.
ENDFORM.                    "CHANGE_VARIABLE


*---------------------------------------------------------------------*
*       FORM CHANGE_VARIABLE_S         (For Select Options)           *
*---------------------------------------------------------------------*
FORM change_variable_s.
  tvarvc-type = 'S'.
  tvarvc-numb = '0000'.
  tvarvc-sign = 'I'.
  tvarvc-opti = 'EQ'.

  PERFORM enq_tvarvc USING tvarvc-name tvarvc-type.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  WRITE: / tvarvc-name, tvarvc-low.

  IF p_testrn <> 'X'.
    PERFORM save_parameter.
  ENDIF.
  PERFORM deq_tvarvc USING tvarvc-name tvarvc-type.
ENDFORM.                    "CHANGE_VARIABLE_S


*----------------------------------------------------------------------*
*FORM ENQ_TVARVC                                                 *
*----------------------------------------------------------------------*
*  -->  P_NAME      Name der Variablen
*       P_TYPE      Typ der Variablen
*----------------------------------------------------------------------*
FORM enq_tvarvc USING
            p_name       LIKE tvarvc-name
            p_type       LIKE tvarvc-type.
  enq_key-name = p_name.
  enq_key-type = p_type.
  CALL FUNCTION 'ENQUEUE_ESVARV'
    EXPORTING
      name           = p_name
      type           = p_type
    EXCEPTIONS
      foreign_lock   = 4
      system_failure = 8.
  CASE sy-subrc.
    WHEN 4.
      WRITE: p_name,  text-310.
    WHEN 8.
      WRITE: enq_key, text-320.
  ENDCASE.
ENDFORM.                    "ENQ_TVARVC


*----------------------------------------------------------------------*
*FORM DEQ_TVARVC                                                 *
*----------------------------------------------------------------------*
*  -->  P_NAME      Name der Variablen
*       P_TYPE      Typ der Variablen
*----------------------------------------------------------------------*
FORM deq_tvarvc USING
            p_name       LIKE tvarvc-name
            p_type       LIKE tvarvc-type.
  enq_key-name = p_name.
  enq_key-type = p_type.
  CALL FUNCTION 'DEQUEUE_ESVARV'
    EXPORTING
      name = p_name
      type = p_type.
ENDFORM.                    "DEQ_TVARVC


*----------------------------------------------------------------------*
*       FORM SAVE_PARAMETER                                            *
*----------------------------------------------------------------------*
FORM save_parameter.
  MOVE 'X' TO tvarvc-clie_indep.
  MODIFY tvarvc.
ENDFORM.                    "SAVE_PARAMETER


*---------------------------------------------------------------------*
*       top-of-page                                                   *
*---------------------------------------------------------------------*
TOP-OF-PAGE.
  FORMAT INTENSIFIED OFF.
  WRITE: / sy-repid, sy-sysid, 32 text-001.
  WRITE:  63 text-004, sy-datum.
  WRITE: /63 text-006, sy-uzeit.
  WRITE: /63 text-005, sy-pagno.

  IF p_testrn = 'X'.
    WRITE: /32 text-205.
  ENDIF.
  ULINE.
  WRITE: / text-201, 32 text-202.
  ULINE.
  FORMAT INTENSIFIED ON.
