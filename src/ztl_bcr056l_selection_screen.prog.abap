*&---------------------------------------------------------------------*
*&  Include           ZTL_BCR056L_SELECTION_SCREEN
*&---------------------------------------------------------------------*


"======================================================================"
"---------------------------Selection Screen---------------------------"
"======================================================================"

"----------------------------------------------------------------------"
*"------------------Title Block Design---------------------------------"
"----------------------------------------------------------------------"
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 44.
PARAMETERS: p_title(31) TYPE c DEFAULT text-h51."'** "Transport Analyzer Tool" **'.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
*
*PARAMETERS: rd_proj RADIOBUTTON GROUP p1 DEFAULT 'X' USER-COMMAND def.
*"-------------List of Transports Created Under A Project----------------"
*SELECTION-SCREEN: BEGIN OF LINE.
*SELECTION-SCREEN: POSITION 18.
*PARAMETERS: p_note1(100) TYPE c DEFAULT text-h52 VISIBLE LENGTH 80."'> List of Transports & Custom Objects Created Under A Project'
*SELECTION-SCREEN: END OF LINE.

PARAMETERS: rd_reprt RADIOBUTTON GROUP p1 DEFAULT 'X' USER-COMMAND def MODIF ID s1.
"-------------List of Transports Created Under A Object----------------"
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 18.
PARAMETERS: p_note2(100) TYPE c DEFAULT text-h53 VISIBLE LENGTH 80."'> List of Transports & Custom Objects Created Under A Object'
SELECTION-SCREEN: END OF LINE.

PARAMETERS: rd_cnflc RADIOBUTTON GROUP p1 MODIF ID s2.
"-----------------Transport / Object Conflict Check--------------------"
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 18.
PARAMETERS: p_note3(100) TYPE c DEFAULT text-h54 VISIBLE LENGTH 90. "'> Transport or Object Confilct Analyzer'
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN : END OF BLOCK b1.

*"----------------------------------------------------------------------------------"
*"------------------------Project Transports Analyzer-------------------------------"
*"----------------------------------------------------------------------------------"
*SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-002.
*SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-005.
*"REQUEST OR Task-------------------------------------"
*SELECT-OPTIONS: s_extid FOR v_externalid   NO-EXTENSION NO INTERVALS MODIF ID m1."OBLIGATORY
*"Target System---------------------------------------"
*SELECT-OPTIONS: s_trgt1 FOR v_tarsystem NO-EXTENSION NO INTERVALS MODIF ID m1.
*
*"Radio button for TR list
*SELECTION-SCREEN: BEGIN OF LINE.
*SELECTION-SCREEN: POSITION 31.
*PARAMETERS : rd_trlt1 RADIOBUTTON GROUP rb4  DEFAULT 'X' USER-COMMAND def1 MODIF ID m1.
*SELECTION-SCREEN COMMENT 1(17) text-r01 FOR FIELD rd_trlt1 MODIF ID m1.
*SELECTION-SCREEN: END OF LINE.
*
*"Radio button for object list
*SELECTION-SCREEN: BEGIN OF LINE.
*SELECTION-SCREEN: POSITION 31.
*PARAMETERS : rd_obj1 RADIOBUTTON GROUP rb4 MODIF ID  m1.
*SELECTION-SCREEN COMMENT 1(17) text-r02 FOR FIELD rd_obj1 MODIF ID m1.
*SELECTION-SCREEN: END OF LINE.
*
*"Checkbox for Include copy TR's
*SELECTION-SCREEN: BEGIN OF LINE.
*SELECTION-SCREEN: POSITION 31.
*PARAMETERS : ck_copy1 AS CHECKBOX DEFAULT 'X' MODIF ID m1.
*SELECTION-SCREEN COMMENT 1(17) text-r03 FOR FIELD ck_copy1 MODIF ID m1.
*SELECTION-SCREEN: END OF LINE.
*
*"Checkbox for Include not released TR's
*SELECTION-SCREEN: BEGIN OF LINE.
*SELECTION-SCREEN: POSITION 31.
*PARAMETERS : ck_rel1 AS CHECKBOX DEFAULT 'X' MODIF ID m1.
*SELECTION-SCREEN COMMENT 1(17) text-r04 FOR FIELD ck_rel1 MODIF ID m1.
*SELECTION-SCREEN: END OF LINE.
*
*SELECTION-SCREEN END OF BLOCK b2.
*
*SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-006.
*
*SELECTION-SCREEN: BEGIN OF LINE.
*PARAMETERS: ck_prgm AS CHECKBOX DEFAULT 'X' MODIF ID cb.    "Report
*SELECTION-SCREEN COMMENT 4(17) text-s01 FOR FIELD ck_prgm MODIF ID cb.
*SELECTION-SCREEN: POSITION 30.
*PARAMETERS: ck_fung AS CHECKBOX DEFAULT 'X' MODIF ID cb.   "Function Group
*SELECTION-SCREEN COMMENT 40(17) text-s02 FOR FIELD ck_fung MODIF ID cb.
*SELECTION-SCREEN: POSITION 60.
*PARAMETERS: ck_funm AS CHECKBOX DEFAULT 'X' MODIF ID cb.   "Function Module
*SELECTION-SCREEN COMMENT 70(17) text-s03 FOR FIELD ck_funm MODIF ID cb.
*SELECTION-SCREEN: END OF LINE.
*
*SELECTION-SCREEN: BEGIN OF LINE.
*PARAMETERS: ck_msgc AS CHECKBOX DEFAULT 'X' MODIF ID cb.    "Message Class
*SELECTION-SCREEN COMMENT 4(17) text-s04 FOR FIELD ck_msgc MODIF ID cb.
*SELECTION-SCREEN: POSITION 30.
*PARAMETERS: ck_trns AS CHECKBOX DEFAULT 'X' MODIF ID cb.   "Transaction
*SELECTION-SCREEN COMMENT 40(17) text-s05 FOR FIELD ck_trns MODIF ID cb.
*SELECTION-SCREEN: POSITION 60.
*PARAMETERS: ck_lgdb AS CHECKBOX DEFAULT 'X' MODIF ID cb.    "Logical Database
*SELECTION-SCREEN COMMENT 70(17) text-s06 FOR FIELD ck_lgdb MODIF ID cb.
*SELECTION-SCREEN: END OF LINE.
*
*SELECTION-SCREEN: BEGIN OF LINE.
*PARAMETERS: ck_dlgm AS CHECKBOX DEFAULT 'X' MODIF ID cb.    "Dialog Module
*SELECTION-SCREEN COMMENT 4(17) text-s07 FOR FIELD ck_dlgm MODIF ID cb.
*SELECTION-SCREEN: POSITION 30.
*PARAMETERS: ck_dbtb AS CHECKBOX DEFAULT 'X' MODIF ID cb.    "DB Tables
*SELECTION-SCREEN COMMENT 40(17) text-s08 FOR FIELD ck_dbtb MODIF ID cb.
*SELECTION-SCREEN: POSITION 60.
*PARAMETERS: ck_shlp AS CHECKBOX DEFAULT 'X' MODIF ID cb.    "Search helps
*SELECTION-SCREEN COMMENT 70(17) text-s09 FOR FIELD ck_shlp MODIF ID cb.
*SELECTION-SCREEN: END OF LINE.
*
*SELECTION-SCREEN: BEGIN OF LINE.
*PARAMETERS: ck_domn AS CHECKBOX DEFAULT 'X' MODIF ID cb.    "Domain
*SELECTION-SCREEN COMMENT 4(17) text-s10 FOR FIELD ck_domn MODIF ID cb.
*SELECTION-SCREEN: POSITION 30.
*PARAMETERS: ck_dtel AS CHECKBOX DEFAULT 'X' MODIF ID cb.    "Data Elemets
*SELECTION-SCREEN COMMENT 40(17) text-s11 FOR FIELD ck_dtel MODIF ID cb.
*SELECTION-SCREEN: POSITION 60.
*PARAMETERS: ck_auth AS CHECKBOX DEFAULT 'X' MODIF ID cb.    "Auth. Objects
*SELECTION-SCREEN COMMENT 70(17) text-s12 FOR FIELD ck_auth MODIF ID cb.
*SELECTION-SCREEN: END OF LINE.
*
*SELECTION-SCREEN: BEGIN OF LINE.
*PARAMETERS: ck_type AS CHECKBOX DEFAULT 'X' MODIF ID cb.    "Table Type
*SELECTION-SCREEN COMMENT 4(17) text-s13 FOR FIELD ck_type MODIF ID cb.
*SELECTION-SCREEN: POSITION 30.
*PARAMETERS: ck_ttyp AS CHECKBOX DEFAULT 'X' MODIF ID cb.    "Table Groups
*SELECTION-SCREEN COMMENT 40(17) text-s14 FOR FIELD ck_ttyp MODIF ID cb.
*SELECTION-SCREEN: POSITION 60.
*PARAMETERS: ck_stru AS CHECKBOX DEFAULT 'X' MODIF ID cb.    "Structure
*SELECTION-SCREEN COMMENT 70(17) text-s15 FOR FIELD ck_stru MODIF ID cb.
*SELECTION-SCREEN: END OF LINE.
*
*SELECTION-SCREEN: BEGIN OF LINE.
*PARAMETERS: ck_lock AS CHECKBOX DEFAULT 'X' MODIF ID cb.    "Lock Objects
*SELECTION-SCREEN COMMENT 4(17) text-s16 FOR FIELD ck_lock MODIF ID cb.
*SELECTION-SCREEN: POSITION 30.
*PARAMETERS: ck_clas AS CHECKBOX DEFAULT 'X' MODIF ID cb.    "Class
*SELECTION-SCREEN COMMENT 40(17) text-s17 FOR FIELD ck_clas MODIF ID cb.
*SELECTION-SCREEN: POSITION 60.
*PARAMETERS: ck_intf AS CHECKBOX DEFAULT 'X' MODIF ID cb.   "Interfaces
*SELECTION-SCREEN COMMENT 70(17) text-s18 FOR FIELD ck_intf MODIF ID cb.
*SELECTION-SCREEN: END OF LINE.
*
*SELECTION-SCREEN: BEGIN OF LINE.
*PARAMETERS: ck_enhm AS CHECKBOX DEFAULT 'X' MODIF ID cb.     "Enhancements
*SELECTION-SCREEN COMMENT 4(17) text-s19 FOR FIELD ck_enhm MODIF ID cb.
*SELECTION-SCREEN: POSITION 30.
*PARAMETERS: ck_smfm AS CHECKBOX DEFAULT 'X' MODIF ID cb.   "Interfaces
*SELECTION-SCREEN COMMENT 40(17) text-s21 FOR FIELD ck_smfm MODIF ID cb.
*SELECTION-SCREEN: POSITION 60.
*PARAMETERS: ck_dall AS CHECKBOX USER-COMMAND uc MODIF ID cb. "Uncheck All
*SELECTION-SCREEN COMMENT 70(17) text-s20 FOR FIELD ck_dall MODIF ID cb.
*
*SELECTION-SCREEN: END OF LINE.
*
*SELECTION-SCREEN END OF BLOCK b3.
*SELECTION-SCREEN END OF BLOCK a1.


*"----------------------------------------------------------------------------------"
*"------------------------Program Selection Block-----------------------------------"
*"----------------------------------------------------------------------------------"
SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME TITLE text-003.
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-005.
"Program name-------------------------------------"
SELECT-OPTIONS: s_pgm FOR v_pgm  MODIF ID m2." NO-EXTENSION NO INTERVALS.  "OBLIGATORY
"Target System---------------------------------------"
SELECT-OPTIONS: s_trgt FOR v_tarsystem NO-EXTENSION NO INTERVALS MODIF ID m2.

"Radio button for TR list
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 31.
PARAMETERS : rd_trlis RADIOBUTTON GROUP rb2  DEFAULT 'X' USER-COMMAND def3 MODIF ID m2.
SELECTION-SCREEN COMMENT 1(17) text-r01 FOR FIELD rd_trlis MODIF ID m2.
SELECTION-SCREEN: END OF LINE.

"Radio button for object list
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 31.
PARAMETERS : rd_obj RADIOBUTTON GROUP rb2 MODIF ID m2.
SELECTION-SCREEN COMMENT 1(17) text-r02 FOR FIELD rd_obj MODIF ID m2.
SELECTION-SCREEN: END OF LINE.

"Checkbox for Include copy TR's
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 31.
PARAMETERS : ck_copy AS CHECKBOX DEFAULT 'X' MODIF ID m2.
SELECTION-SCREEN COMMENT 1(17) text-r03 FOR FIELD ck_copy MODIF ID m2.
SELECTION-SCREEN: END OF LINE.

"Checkbox for Include not released TR's
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 31.
PARAMETERS : ck_rel AS CHECKBOX DEFAULT 'X' MODIF ID m2.
SELECTION-SCREEN COMMENT 1(17) text-r04 FOR FIELD ck_rel MODIF ID m2.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-006.

SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS: ck_prgm1 AS CHECKBOX DEFAULT 'X' MODIF ID yz.    "Report
SELECTION-SCREEN COMMENT 4(17) text-s01 FOR FIELD ck_prgm1 MODIF ID yz.
SELECTION-SCREEN: POSITION 30.
PARAMETERS: ck_fung1 AS CHECKBOX DEFAULT 'X' MODIF ID yz.   "Function Group
SELECTION-SCREEN COMMENT 40(17) text-s02 FOR FIELD ck_fung1 MODIF ID yz.
SELECTION-SCREEN: POSITION 60.
PARAMETERS: ck_funm1 AS CHECKBOX DEFAULT 'X' MODIF ID yz.   "Function Module
SELECTION-SCREEN COMMENT 70(17) text-s03 FOR FIELD ck_funm1 MODIF ID yz.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS: ck_msgc1 AS CHECKBOX DEFAULT 'X' MODIF ID yz.    "Message Class
SELECTION-SCREEN COMMENT 4(17) text-s04 FOR FIELD ck_msgc1 MODIF ID yz.
SELECTION-SCREEN: POSITION 30.
PARAMETERS: ck_trns1 AS CHECKBOX DEFAULT 'X' MODIF ID yz.   "Transaction
SELECTION-SCREEN COMMENT 40(17) text-s05 FOR FIELD ck_trns1 MODIF ID yz.
SELECTION-SCREEN: POSITION 60.
PARAMETERS: ck_lgdb1 AS CHECKBOX DEFAULT 'X' MODIF ID yz.    "Logical Database
SELECTION-SCREEN COMMENT 70(17) text-s06 FOR FIELD ck_lgdb1 MODIF ID yz.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS: ck_dlgm1 AS CHECKBOX DEFAULT 'X' MODIF ID yz.    "Dialog Module
SELECTION-SCREEN COMMENT 4(17) text-s07 FOR FIELD ck_dlgm1 MODIF ID yz.
SELECTION-SCREEN: POSITION 30.
PARAMETERS: ck_dbtb1 AS CHECKBOX DEFAULT 'X' MODIF ID yz.    "DB Tables
SELECTION-SCREEN COMMENT 40(17) text-s08 FOR FIELD ck_dbtb1 MODIF ID yz.
SELECTION-SCREEN: POSITION 60.
PARAMETERS: ck_shlp1 AS CHECKBOX DEFAULT 'X' MODIF ID yz.    "Search helps
SELECTION-SCREEN COMMENT 70(17) text-s09 FOR FIELD ck_shlp1 MODIF ID yz.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS: ck_domn1 AS CHECKBOX DEFAULT 'X' MODIF ID yz.    "Domain
SELECTION-SCREEN COMMENT 4(17) text-s10 FOR FIELD ck_domn1 MODIF ID yz.
SELECTION-SCREEN: POSITION 30.
PARAMETERS: ck_dtel1 AS CHECKBOX DEFAULT 'X' MODIF ID yz.    "Data Elemets
SELECTION-SCREEN COMMENT 40(17) text-s11 FOR FIELD ck_dtel1 MODIF ID yz.
SELECTION-SCREEN: POSITION 60.
PARAMETERS: ck_auth1 AS CHECKBOX DEFAULT 'X' MODIF ID yz.    "Auth. Objects
SELECTION-SCREEN COMMENT 70(17) text-s12 FOR FIELD ck_auth1 MODIF ID yz.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS: ck_type1 AS CHECKBOX DEFAULT 'X' MODIF ID yz.    "Table Type
SELECTION-SCREEN COMMENT 4(17) text-s13 FOR FIELD ck_type1 MODIF ID yz.
SELECTION-SCREEN: POSITION 30.
PARAMETERS: ck_ttyp1 AS CHECKBOX DEFAULT 'X' MODIF ID yz.    "Table Groups
SELECTION-SCREEN COMMENT 40(17) text-s14 FOR FIELD ck_ttyp1 MODIF ID yz.
SELECTION-SCREEN: POSITION 60.
PARAMETERS: ck_stru1 AS CHECKBOX DEFAULT 'X' MODIF ID yz.    "Structure
SELECTION-SCREEN COMMENT 70(17) text-s15 FOR FIELD ck_stru1 MODIF ID yz.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS: ck_lock1 AS CHECKBOX DEFAULT 'X' MODIF ID yz.    "Lock Objects
SELECTION-SCREEN COMMENT 4(17) text-s16 FOR FIELD ck_lock1 MODIF ID yz.
SELECTION-SCREEN: POSITION 30.
PARAMETERS: ck_clas1 AS CHECKBOX DEFAULT 'X' MODIF ID yz.    "Class
SELECTION-SCREEN COMMENT 40(17) text-s17 FOR FIELD ck_clas1 MODIF ID yz.
SELECTION-SCREEN: POSITION 60.
PARAMETERS: ck_intf1 AS CHECKBOX DEFAULT 'X' MODIF ID yz.   "Interfaces
SELECTION-SCREEN COMMENT 70(17) text-s18 FOR FIELD ck_intf1 MODIF ID yz.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS: ck_enhm1 AS CHECKBOX DEFAULT 'X' MODIF ID yz.     "Enhancements
SELECTION-SCREEN COMMENT 4(17) text-s19 FOR FIELD ck_enhm1 MODIF ID yz.
SELECTION-SCREEN: POSITION 30.
PARAMETERS: ck_smfm1 AS CHECKBOX DEFAULT 'X' MODIF ID yz.   "Interfaces
SELECTION-SCREEN COMMENT 32(17) text-s21 FOR FIELD ck_smfm1 MODIF ID yz.
SELECTION-SCREEN: POSITION 60.
PARAMETERS: ck_dall1 AS CHECKBOX USER-COMMAND uc1 MODIF ID yz. "Uncheck All
SELECTION-SCREEN COMMENT 70(17) text-s20 FOR FIELD ck_dall1 MODIF ID yz.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN END OF BLOCK b5.
SELECTION-SCREEN END OF BLOCK a2.


*"----------------------------------------------------------------------------------"
*"------------------------Conflicts Selection Block-----------------------------------"
*"----------------------------------------------------------------------------------"
SELECTION-SCREEN : BEGIN OF BLOCK a3 WITH FRAME TITLE text-004.
"-----------------Target System Block----------------------------"
SELECTION-SCREEN : BEGIN OF BLOCK b6 WITH FRAME TITLE text-007.
PARAMETERS       : p_system TYPE RFCDES-RFCDEST MODIF ID r3. "OBLIGATORY
SELECTION-SCREEN : END OF BLOCK b6.

"-----------------Object Type Block----------------------------"
SELECTION-SCREEN : BEGIN OF BLOCK b7 WITH FRAME TITLE text-008.
"----------------------------------------Transport
PARAMETERS : rd_req RADIOBUTTON GROUP gp DEFAULT 'X' USER-COMMAND def MODIF ID r2.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 30.
SELECT-OPTIONS   : s_req    FOR v_req MODIF ID rq NO INTERVALS .
SELECTION-SCREEN COMMENT 10(17) text-t01 FOR FIELD s_req MODIF ID rq.
SELECTION-SCREEN: END OF LINE.
"---------------------------------------Object Type
PARAMETERS : rd_objtp RADIOBUTTON GROUP gp MODIF ID cde.

"-------------------------Report
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 10.
PARAMETERS : rd_rep RADIOBUTTON GROUP rp1 USER-COMMAND rp MODIF ID r1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 30(17) text-t02 FOR FIELD rd_rep MODIF ID r1.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 40.
SELECT-OPTIONS   : s_report FOR v_obj MODIF ID fs NO INTERVALS.
SELECTION-SCREEN COMMENT 20(17) text-t02 FOR FIELD s_report MODIF ID fs.
SELECTION-SCREEN: END OF LINE.

"-----------------------Smartform
*SELECTION-SCREEN: BEGIN OF LINE.
*SELECTION-SCREEN: POSITION 10.
*PARAMETERS : rd_smrt RADIOBUTTON GROUP rp1 MODIF ID r1.
*SELECTION-SCREEN COMMENT 30(17) text-t05 FOR FIELD rd_smrt MODIF ID r1.
*SELECTION-SCREEN: END OF LINE.
*
*SELECTION-SCREEN: BEGIN OF LINE.
*SELECTION-SCREEN: POSITION 40.
*SELECT-OPTIONS   : s_smrtfm FOR v_obj  MODIF ID fp NO INTERVALS.
*SELECTION-SCREEN COMMENT 20(17) text-t05 FOR FIELD s_smrtfm MODIF ID fp.
*SELECTION-SCREEN: END OF LINE.

"--------------------Database Table
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 10.
PARAMETERS : rd_tbl RADIOBUTTON GROUP rp1 MODIF ID r1.
SELECTION-SCREEN COMMENT 30(17) text-t03 FOR FIELD rd_tbl MODIF ID r1.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 40.
SELECT-OPTIONS   : s_table  FOR v_obj MODIF ID fr NO INTERVALS.
SELECTION-SCREEN COMMENT 20(17) text-t03 FOR FIELD s_table MODIF ID fr.
SELECTION-SCREEN: END OF LINE.

"""----------------------Domain
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 10.
PARAMETERS : rd_dmn RADIOBUTTON GROUP rp1 MODIF ID r1.
SELECTION-SCREEN COMMENT 20(17) text-t06 FOR FIELD rd_dmn MODIF ID r1.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 40.
SELECT-OPTIONS   : s_domain  FOR v_obj MODIF ID ft NO INTERVALS.
SELECTION-SCREEN COMMENT 20(17) text-t06 FOR FIELD s_domain MODIF ID ft.
SELECTION-SCREEN: END OF LINE.

""----------------------Function Group
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 10.
PARAMETERS : rd_fugp RADIOBUTTON GROUP rp1 MODIF ID r1.
SELECTION-SCREEN COMMENT 20(17) text-t04 FOR FIELD rd_fugp MODIF ID r1.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 40.
SELECT-OPTIONS   : s_fugp  FOR v_obj MODIF ID fu NO INTERVALS.
SELECTION-SCREEN COMMENT 20(17) text-t04 FOR FIELD s_fugp MODIF ID fu.
SELECTION-SCREEN: END OF LINE.

"-----------------------Function Module
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 10.
PARAMETERS : rd_func RADIOBUTTON GROUP rp1 MODIF ID r1.
SELECTION-SCREEN COMMENT 20(17) text-t07 FOR FIELD rd_func MODIF ID r1.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 40.
SELECT-OPTIONS   : s_func  FOR v_obj MODIF ID fv NO INTERVALS.
SELECTION-SCREEN COMMENT 20(17) text-t07 FOR FIELD s_func MODIF ID fv.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN : END OF BLOCK b7.

"-----------------Conflict Check Block----------------------------"
SELECTION-SCREEN : BEGIN OF BLOCK b8 WITH FRAME TITLE text-009.
PARAMETERS       : rd_conf  RADIOBUTTON GROUP gp1 USER-COMMAND us MODIF ID b2 DEFAULT 'X',
                   rd_dep   RADIOBUTTON GROUP gp1 MODIF ID b3,
                   rd_miss  RADIOBUTTON GROUP gp1 MODIF ID b4.
SELECTION-SCREEN : END OF BLOCK b8.
SELECTION-SCREEN : END OF BLOCK a3.


*"================================================================================="
*"----------------------Screen parameters disable /enable--------------------------"
*"================================================================================="

"Dynamic Screen Modification based on the selection


"-----------------Screen parameters disable /enable---------------------"
CONSTANTS : lc_confs(2) TYPE c VALUE 'FS',
            lc_confp(2) TYPE c VALUE 'FP',
            lc_confr(2) TYPE c VALUE 'FR',
            lc_conft(2) TYPE c VALUE 'FT',
            lc_confu(2) TYPE c VALUE 'FU',
            lc_confv(2) TYPE c VALUE 'FV',
            lc_conr1(2) TYPE c VALUE 'R1',
            lc_conrq(2) TYPE c VALUE 'RQ',
            lc_conb3(2) TYPE c VALUE 'B3',
            lc_conb4(2) TYPE c VALUE 'B4',
            lc_conx(1) TYPE c VALUE 'X',
            lc_con0(1) TYPE c VALUE '0'.
            "lc_con1(1) TYPE c VALUE '1',
            "lc_title(7) TYPE c VALUE 'P_TITLE',
            "lc_note1(7) TYPE c VALUE 'P_NOTE1',
            "lc_note2(7) TYPE c VALUE 'P_NOTE2',
            "lc_note3(7) TYPE c VALUE 'P_NOTE3'.

AT SELECTION-SCREEN.
  "------------------------To Uncheck All checkbox----------------------------------"
*  IF sy-ucomm = 'UC' ."AND ck_dall = 'X'.
**    CLEAR:"ck_prgm, ck_fung,ck_funm,ck_msgc,ck_trns,ck_lgdb,ck_dlgm,
**    ck_dbtb,ck_shlp,ck_domn,ck_dtel,ck_auth,ck_type,ck_ttyp,ck_stru,
**    ck_lock,ck_clas,ck_intf,ck_enhm,ck_smfm .
*  ELSEIF sy-ucomm = 'UC' AND ck_dall = ' '.
*    ck_prgm = ck_fung = ck_funm = ck_msgc = ck_trns = ck_lgdb = ck_dlgm =
*    ck_dbtb = ck_shlp = ck_domn = ck_dtel = ck_auth = ck_type = ck_ttyp = ck_stru
*    = ck_lock = ck_clas = ck_intf = ck_enhm = ck_smfm = 'X'.
*  ELSE
    IF sy-ucomm = 'UC1' AND ck_dall1 = 'X'.
    CLEAR:ck_prgm1, ck_fung1,ck_funm1,ck_msgc1,ck_trns1,ck_lgdb1,ck_dlgm1,
     ck_dbtb1,ck_shlp1,ck_domn1,ck_dtel1,ck_auth1,ck_type1,ck_ttyp1,ck_stru1,
     ck_lock1,ck_clas1,ck_intf1,ck_enhm1,ck_smfm1.
  ELSEIF sy-ucomm = 'UC1' AND ck_dall1 = ' '.
    ck_prgm1 = ck_fung1 = ck_funm1 = ck_msgc1 = ck_trns1 = ck_lgdb1 = ck_dlgm1 =
    ck_dbtb1 = ck_shlp1 = ck_domn1 = ck_dtel1 = ck_auth1 = ck_type1 = ck_ttyp1 = ck_stru1
    = ck_lock1 = ck_clas1 = ck_intf1 = ck_enhm1 = ck_smfm1 = 'X'.
  ENDIF.
  "------------------------Clearing Invalid inputs----------------------------------"
*  IF rd_proj IS INITIAL.
*    CLEAR :s_extid,s_trgt1.
*  ELSE
    IF rd_reprt IS INITIAL.
    CLEAR:s_pgm ,s_trgt.
  ELSEIF rd_cnflc IS INITIAL.
    CLEAR :p_system.
    IF rd_req IS INITIAL.
      CLEAR s_req.
    ELSEIF rd_objtp IS INITIAL.
      IF rd_rep IS INITIAL.
        CLEAR s_report.
*      ELSEIF rd_smrt IS INITIAL.
*        CLEAR s_smrtfm.
      ELSEIF rd_tbl IS INITIAL.
        CLEAR s_table.
      ELSEIF rd_dmn IS INITIAL.
        CLEAR s_domain.
      ELSEIF rd_fugp IS INITIAL.
        CLEAR s_fugp.
      ELSEIF rd_func IS INITIAL.
        CLEAR s_func .
      ENDIF.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'P_TITLE' OR
*       screen-name = 'P_NOTE1' OR
       screen-name = 'P_NOTE2' OR
       screen-name = 'P_NOTE3'.
      screen-intensified = '1'.
      screen-input = '0'.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.
  ENDLOOP.
*  IF rd_proj EQ 'X'.            "----------------Project
*    LOOP AT SCREEN.
*      IF rd_trlt1 EQ 'X' AND  screen-group1 = 'CB'.
*        screen-active = '0'.
*        MODIFY SCREEN.
*      ENDIF.
*
*      IF  screen-group1 = 'M2' OR
*          screen-group1 = 'YZ' OR
*          screen-group1 = 'R1' OR
*           screen-group1 = 'R2' OR
*           screen-group1 = 'R3' OR
*           screen-group1 = 'CDE' OR
*           screen-group1 = 'B2' OR
*           screen-group1 = 'RQ' OR
*          screen-group1 = 'FS' OR
*           screen-group1 = 'FP' OR
*           screen-group1 = 'B3' OR
*           screen-group1 = 'B4' OR
*           screen-group1 = 'FR' OR
*           screen-group1 = 'FT' OR
*           screen-group1 = 'FU' OR
*           screen-group1 = 'FV'.
*        screen-active = '0'.
*        MODIFY SCREEN.
*        CONTINUE.
*      ENDIF.
*    ENDLOOP.
*  ELSE
    IF rd_reprt EQ 'X'.            "----------------Report
    LOOP AT SCREEN.
      IF rd_trlis EQ 'X' AND  screen-group1 = 'YZ'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF  screen-group1 = 'M1' OR
          screen-group1 = 'CB' OR
           screen-group1 = 'R1' OR
           screen-group1 = 'R2' OR
           screen-group1 = 'R3' OR
           screen-group1 = 'CDE' OR
           screen-group1 = 'B2' OR
           screen-group1 = 'RQ' OR
           screen-group1 = lc_confs OR
           screen-group1 = lc_confp OR
           screen-group1 = lc_conb3 OR
           screen-group1 = lc_conb4 OR
           screen-group1 = lc_confr OR
           screen-group1 = lc_conft OR
           screen-group1 = lc_confu OR
           screen-group1 = lc_confv.
        screen-active = '0'.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ELSEIF rd_cnflc IS NOT INITIAL.
    LOOP AT SCREEN.
      IF rd_req EQ lc_conx.         "----------------Transport Request
        IF  screen-group1 = 'M1' OR
            screen-group1 = 'CB' OR
            screen-group1 = 'M2' OR
            screen-group1 = 'YZ'.
          screen-active = lc_con0.
        ENDIF.
        IF  screen-group1 = lc_conr1 .
          screen-active = lc_con0.
        ENDIF.

        IF screen-group1 = lc_confs OR
           screen-group1 = lc_confp OR
           screen-group1 = lc_confr OR
           screen-group1 = lc_conft OR
           screen-group1 = lc_confu OR
           screen-group1 = lc_confv.
          screen-active = lc_con0.
        ENDIF.
      ENDIF.

      IF rd_objtp EQ lc_conx.           "-----------------Object Type
        IF  screen-group1 = 'M1' OR
      screen-group1 = 'CB' OR
      screen-group1 = 'M2' OR
      screen-group1 = 'YZ'.
          screen-active = lc_con0.
        ENDIF.
        IF  screen-group1 = lc_conrq.
          screen-active = lc_con0.
        ENDIF.
        IF rd_rep EQ lc_conx.           "--------------Report
          IF screen-group1 = lc_confp OR
             screen-group1 = lc_confr OR
             screen-group1 = lc_conb3 OR
             screen-group1 = lc_conb4 OR
             screen-group1 = lc_confr OR
             screen-group1 = lc_conft OR
             screen-group1 = lc_confu OR
             screen-group1 = lc_confv.
            screen-active = lc_con0.
          ENDIF.
        ENDIF.
*        IF rd_smrt EQ lc_conx.          "--------------Smartforms
*          IF screen-group1 = lc_confs OR
*             screen-group1 = lc_confr OR
*             screen-group1 = lc_conb3 OR
*             screen-group1 = lc_conb4 OR
*             screen-group1 = lc_confr OR
*             screen-group1 = lc_conft OR
*             screen-group1 = lc_confu OR
*             screen-group1 = lc_confv.
*            screen-active = lc_con0.
*          ENDIF.
*        ENDIF.
        IF rd_tbl EQ lc_conx.           "----------------Table
          IF screen-group1 = lc_confs OR
            screen-group1 = lc_confp OR
            screen-group1 = lc_conb3 OR
            screen-group1 = lc_conb4 OR
            screen-group1 = lc_conft OR
            screen-group1 = lc_confu OR
            screen-group1 = lc_confv.
            screen-active = lc_con0.
          ENDIF.
        ENDIF.
        IF rd_dmn IS NOT INITIAL.  "-----------------Domain
          IF screen-group1 = lc_confp OR
          screen-group1 = lc_confs OR
          screen-group1 = lc_conb3 OR
          screen-group1 = lc_conb4 OR
          screen-group1 = lc_confr OR
          screen-group1 = lc_confu OR
          screen-group1 = lc_confv .
            screen-active = lc_con0.
          ENDIF.
        ENDIF.
        IF rd_fugp IS NOT INITIAL. "---------------Function Group
          IF screen-group1 = lc_confp OR
          screen-group1 = lc_confs OR
          screen-group1 = lc_conb3 OR
          screen-group1 = lc_conb4 OR
          screen-group1 = lc_confr OR
          screen-group1 = lc_conft OR
          screen-group1 = lc_confv.
            screen-active = lc_con0.
          ENDIF.
        ENDIF.
        IF rd_func IS NOT INITIAL.  "--------------Function Mdoule
          IF screen-group1 = lc_confs OR
            screen-group1 = lc_confp OR
            screen-group1 = lc_conb3 OR
            screen-group1 = lc_conb4 OR
            screen-group1 = lc_confr OR
            screen-group1 = lc_conft OR
            screen-group1 = lc_confu .
            screen-active = lc_con0.
          ENDIF.
        ENDIF.
      ENDIF.
      MODIFY SCREEN.
      CONTINUE.
    ENDLOOP.
  ENDIF.
