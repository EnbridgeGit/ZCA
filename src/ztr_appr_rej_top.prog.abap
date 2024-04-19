*&---------------------------------------------------------------------*
*&  Include           ZTR_APPR_REJ_TOP
*&---------------------------------------------------------------------*

TABLES: tadir, e071, e07t, e070.
TABLES: somlreci1.
DATA : filename TYPE string,
       gv_owner TYPE string,
       gv_strkorr TYPE e070-strkorr,
       gv_trkorr TYPE string,
       gv_clean TYPE c,
       GV_ACTVT(2) TYPE C,
       HELP_INFOS LIKE HELP_INFO,
       DYNPSELECT LIKE DSELC OCCURS 10,
       DYNPVALUETAB LIKE DVAL OCCURS 10,
       SELECTION TYPE C,
       SELECT_VALUE LIKE HELP_INFO-FLDVALUE,
       RSMDY_RET LIKE RSMDY.
TYPES : BEGIN OF TY_TMP,
          ROW TYPE I,
          TR TYPE TRKORR,
          LINE(1000) TYPE C,
         END OF TY_TMP.
TYPES:BEGIN OF s_ctslg_action,
         date             LIKE tstrfcofil-trdate,
         time             LIKE tstrfcofil-trtime,
         rc               LIKE tstrfcofil-retcode,
      END OF s_ctslg_action,
      t_ctslg_actions       TYPE SORTED TABLE OF s_ctslg_action
                               WITH UNIQUE KEY date time
                               INITIAL SIZE 2.
TYPES :   BEGIN OF s_ctslg_step,
         clientid         LIKE tstrfcofil-tarclient,
         stepid           LIKE tstrfcofil-function,
         rc               LIKE tstrfcofil-retcode,
         actions          TYPE t_ctslg_actions,
      END OF s_ctslg_step,
      t_ctslg_steps         TYPE s_ctslg_step OCCURS 10.

TYPES :      BEGIN OF s_ctslg_system,
         systemid         LIKE tstrfcofil-tarsystem,
         rc               LIKE tstrfcofil-retcode,
         steps            TYPE t_ctslg_steps,
      END OF s_ctslg_system,
      t_ctslg_systems       TYPE s_ctslg_system OCCURS 10.

TYPES :    BEGIN OF s_ctslg_mergeline,
         trkorr           TYPE trkorr,
         rc               LIKE tstrfcofil-retcode,
      END OF s_ctslg_mergeline,
      t_ctslg_mergelines    TYPE s_ctslg_mergeline OCCURS 0.

TYPES:       BEGIN OF s_ctslg_cofile,
          exists           TYPE c,
          imported         TYPE c,
          del_lines_only   TYPE c,
          systems          TYPE t_ctslg_systems,
          merges           TYPE t_ctslg_mergelines,
          rc               TYPE i,
       END OF s_ctslg_cofile.


TYPES:   BEGIN OF s_ctslg_request_info,
          int_status       LIKE trsapnames-int_status,
          read_flag        LIKE trsapnames-read_flag,
          header           TYPE trwbo_request_header,
          project          TYPE trkorr_p,
          cofile           TYPE s_ctslg_cofile,
          cofile_filled    TYPE c,
       END OF s_ctslg_request_info,
       t_ctslg_request_infos TYPE s_ctslg_request_info  OCCURS 0.

TYPES: BEGIN OF s_ctslg_settings,
          error_level              TYPE  i,
          point_to_missing_steps   TYPE  c,
          detailed_depiction       TYPE  c,
          systems                  TYPE  sysnames,
       END OF s_ctslg_settings.

DATA:  ls_request          TYPE  s_ctslg_request_info,
       lt_requests         TYPE  t_ctslg_request_infos,
       ls_settings         TYPE  s_ctslg_settings,
       wa_request          TYPE  s_ctslg_request_info,
       "ls_system           TYPE  LINE OF s_ctslg_settings-systems,
       "lv_trkorr           LIKE  e070-trkorr,
       lv_username         LIKE  e070-as4user.

*
DATA : lv_smtp_addr     TYPE ad_smtpadr ,
       wa_receivers     TYPE somlreci1,
       wa_packing_list  TYPE sopcklsti1,
       wa_message       TYPE solisti1,
       wa_contents_bin  TYPE solisti1,
       lv_psubject(90)  TYPE c,
       lv_text1(20)     TYPE c VALUE 'Hello All',
       lv_text2(12)     TYPE c,
       gd_sent_all(1)  TYPE c,
       lt_receivers     LIKE somlreci1 OCCURS 0 WITH HEADER LINE,"TYPE STANDARD TABLE OF somlreci1,
       lt_packing_list  TYPE STANDARD TABLE OF sopcklsti1,
       lt_message       TYPE STANDARD TABLE OF solisti1,
       lt_document_data TYPE sodocchgi1,
       lt_contents_bin  TYPE STANDARD TABLE OF solisti1,
       lv_tabix         TYPE sy-tabix,
       GT_TMP     TYPE TABLE OF TY_TMP,
       GS_TMP     TYPE TY_TMP,
       GT_LINE    TYPE TABLE OF TLINE,
       GS_LINE    TYPE TLINE.


** Structure Definition for the output of selected TR number & Attribute

TYPES: BEGIN OF ty_final,
*         package  TYPE devclass,
         strkorr   TYPE strkorr,
         as4text  TYPE as4text,
         trstdes  TYPE DDTEXT,
         TARSYSTEM TYPE TRTARSYS,
         AS4USER   TYPE TR_AS4USER,
         AS4DATE   TYPE AS4DATE,
         TRKORR   TYPE TRKORR,
         APPROVE(30)  TYPE c,
         DECLINE(30)  TYPE c,
         DUP(1000) TYPE c,
         SLIN_CHECK(30) TYPE c,
         TR_ANALYZER(30) TYPE c,
         CHK_SEQ(30)     TYPE c,
       END OF ty_final.

**Internal Table & Work Area declaration
DATA: it_final TYPE TABLE OF ty_final,
      it_disp TYPE TABLE OF ty_final,
      wa_final TYPE ty_final.

DATA : it_status TYPE TABLE OF ty_final,
       wa_release TYPE ty_final.

DATA : lv_T TYPE STRING,
       lv_D TYPE STRING,
       LV_L TYPE STRING,
       LV_O TYPE STRING,
       LV_R TYPE STRING,
       LV_N TYPE STRING.


** Structure Definition for TR & it's Description
TYPES: BEGIN OF ty_e07t,
         trkorr  TYPE trkorr,
         as4text TYPE as4text,
       END OF ty_e07t.

**Internal Table & Work Area declaration
DATA: it_e07t TYPE TABLE OF ty_e07t,
      wa_e07t TYPE ty_e07t.


** Structure Definition for TR & it's status
TYPES: BEGIN OF ty_e070,
         trkorr    TYPE trkorr,
         trstatus  TYPE trstatus,
         as4user   TYPE tr_as4user,
         as4date   TYPE as4date,
         as4time   TYPE AS4TIME,
         strkorr   TYPE strkorr,

       END OF ty_e070.

**Internal Table & Work Area declaration
DATA: it_e070 TYPE TABLE OF ty_e070,
      it_e070_1 TYPE TABLE OF ty_e070,
      wa_e070 TYPE ty_e070.


TYPES : BEGIN OF ty_e071,
          trkorr   TYPE trkorr,
          as4pos   TYPE ddposition,
          pgmid    TYPE pgmid,
          object   TYPE trobjtype,
          obj_name TYPE trobj_name,
        END OF ty_e071.

DATA : it_e071 TYPE TABLE OF ty_e071,
       wa_e071 TYPE ty_e071.

TYPES : BEGIN OF ty_tadir,
          pgmid    TYPE pgmid,
          object   TYPE trobjtype,
          obj_name TYPE trobj_name,
          devclass TYPE devclass,
        END OF ty_tadir.

DATA : it_tadir TYPE TABLE OF ty_tadir,
       wa_tadir TYPE ty_tadir.




**Local variable to use file path in the FM GUI_DOWNLOAD
DATA: lv_file TYPE rlgrap-filename.


**Add header to download file
TYPES :BEGIN OF excel_heading,
         text(20) TYPE c,
       END OF excel_heading.

**Internal Table & WA declaration for header
DATA: lt_header TYPE TABLE OF excel_heading,
      wa_header TYPE excel_heading.

**ABAP ALAV display layout parameters.
DATA : gt_fieldcat    TYPE slis_t_fieldcat_alv,
       gs_fieldcat    TYPE slis_fieldcat_alv,
       gs_fieldlayout TYPE slis_layout_alv.

**ABAP ALV Display layout parameters
DATA:lt_fieldcat TYPE lvc_t_fcat,
     wa_layout   TYPE lvc_s_layo,
     ok_code     TYPE syucomm,
     oref_alv    TYPE REF TO cl_gui_alv_grid.
FIELD-SYMBOLS: <f1> TYPE any.

TYPE-POOLS dart1.

TYPE-POOLS shlp.                       "search help


TABLES: txw_c_strc,
        txw_c_soex,
        txw_c_glo,
        txw_dir,     "obsolete after rel 99 use of uuid
        txw_diral,   "obsolete after rel 99 use of uuid
        txw_dirseg,  "obsolete after rel 99 use of uuid
        txw_vwlog,   "obsolete after rel 99 use of uuid
        txw_dir2,
        txw_diral2,
        txw_dirsg2,
        txw_vwlog2,
        txw_c_v0,
        txw_xfiles,
        toaco.      "For business object in archive link

* table control for multiple file entry
CONSTANTS: screen_xfiles LIKE sy-dynnr VALUE '0100'.
CONTROLS: tc_xfiles TYPE TABLEVIEW USING SCREEN '0100'.
DATA: BEGIN OF tc_xfiles_tab OCCURS 0,
      mark(1) TYPE c.                  "table control checkbox
        INCLUDE STRUCTURE txw_xfiles.
DATA: END   OF tc_xfiles_tab.

DATA: glo_canceled(1) TYPE c.

* Status block to collect error messages for conversion
DATA: BEGIN OF conv_status OCCURS 0,
         tablename(50) TYPE c,
         init_cnt(6)   TYPE n,
         conv_cnt(6)   TYPE n,
         status(1)     TYPE c, "L - locked, ? - unknown, X - converted
      END OF conv_status.


*****  Data items for text editor
CONSTANTS: textnoteline_length TYPE i VALUE 72.

DATA:
* reference to wrapper class of control
      textnote_editor TYPE REF TO cl_gui_textedit,
*     reference to custom container: necessary to bind TextEdit Control
      textnote_custom_container TYPE REF TO cl_gui_custom_container,
      textnote_repid LIKE sy-repid,
      textnote_ok_code LIKE sy-ucomm,  " return code from screen
      textnote_table(textnoteline_length) TYPE c OCCURS 0,
      textnote_container(30) TYPE c.   " string for the containers

DATA: textnote_itxw_note TYPE STANDARD TABLE OF txw_note
          WITH HEADER LINE,
      textnote_edit_mode(1) TYPE c VALUE 'X',
      edit_mode TYPE C VALUE 'X',
       T_TXWNOTE TYPE TABLE OF TXW_NOTE.

* necessary to flush the automation queue
CLASS cl_gui_cfw DEFINITION LOAD.
* components for ALV grid in statistics
DATA:
      gt_seg_fieldcatalog TYPE lvc_t_fcat, "Fieldcatalog
      gt_seg_sort         TYPE lvc_t_sort, "Sortiertabelle
      gt_seg_selects TYPE lvc_t_indx WITH HEADER LINE.
*
CLASS cl_gui_column_tree DEFINITION LOAD.
DATA  g_seg_ok_code LIKE sy-ucomm.      " belongs in top-include.
DATA  g_seg_tree  TYPE REF TO cl_gui_alv_tree_simple.
DATA: g_seg_tree_container_name(30) TYPE c VALUE 'SEGMENT_CONTAINER',
        g_seg_custom_container TYPE REF TO cl_gui_custom_container.

DATA gt_seg_select_outtab TYPE lvc_index.

DATA:  BEGIN OF gt_seg_outtab OCCURS 0,
           appl    LIKE txw_dirsg2-ddtext,
           ddtext  LIKE  dd07v-ddtext,
           segtype LIKE txw_dirsg2-segtype,
           segdata LIKE txw_dirsg2-segdata,
           exp_struct LIKE txw_dirsg2-exp_struct,
       END OF gt_seg_outtab.

DATA: it_ZFIT TYPE STANDARD TABLE OF ZTRT_CONFIG,
      wa_zfit TYPE ZTRT_CONFIG,
      v_msg_log TYPE C..
