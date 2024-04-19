*&---------------------------------------------------------------------*
*&  Include           ZTL_BCR056L_GLOBAL
*&---------------------------------------------------------------------*

"----------------Report data declaration-------------------------------"
DATA: v_pgm TYPE e071-obj_name.

*DATA: v_tarsystem           TYPE e070-tarsystem,
DATA: v_tarsystem           TYPE RFCDES-RFCDEST,
      pa_prog               TYPE tadir-obj_name.


DATA: t_environment_selection TYPE envi_types,
      t_source_searched       TYPE TABLE OF rsfind,
      t_output_list           TYPE TABLE OF senvi,
      t_output_list1          TYPE TABLE OF senvi.


TYPES: BEGIN OF s_e071_r,
                    trkorr  TYPE e071-trkorr,
                    pgmid   TYPE e071-pgmid,
                   object   TYPE e071-object,
                   obj_name TYPE e071-obj_name,
                 trfunction TYPE e070-trfunction,
       END OF s_e071_r.

DATA: t_e071_r              TYPE STANDARD TABLE OF s_e071_r,
      t_e071_r1              TYPE STANDARD TABLE OF s_e071_r,
      t_e071tr_r            TYPE STANDARD TABLE OF s_e071_r,
      wa_e071_r             TYPE s_e071_r.

TYPES: BEGIN OF s_e07t_r,
                    trkorr  TYPE e07t-trkorr,
                    as4text TYPE e07t-as4text,
       END OF s_e07t_r.

DATA:             t_e07t_r  TYPE STANDARD TABLE OF s_e07t_r,
                 wa_e07t_r  TYPE s_e07t_r.


TYPES :     BEGIN OF s_e070_r,
                   trkorr  TYPE e070-trkorr,
               trfunction  TYPE e070-trfunction,
                 trstatus  TYPE e070-trstatus,
                tarsystem  TYPE e070-tarsystem,
                  korrdev  TYPE e070-korrdev,
                  as4user  TYPE e070-as4user,
                  as4date  TYPE e070-as4date,
                  as4time  TYPE e070-as4time,
                  strkorr  TYPE e070-strkorr,
         END OF s_e070_r.

DATA:               t_e070_r  TYPE STANDARD TABLE OF s_e070_r,
                    wa_e070_r  TYPE s_e070_r.

TYPES: BEGIN OF s_final_r,
                trkorr     TYPE e071-trkorr,
                trfunction TYPE e070-trfunction,
                 trstatus  TYPE e070-trstatus,
                 tarsystem TYPE e070-tarsystem,
                 date      TYPE tstrfcofil-trdate,
                 time      TYPE tstrfcofil-trtime,
                status(60) TYPE c,
                korrdev    TYPE e070-korrdev,
                as4user    TYPE e070-as4user,
                as4text    TYPE e07t-as4text,
                pgmid      TYPE e071-pgmid,
                object     TYPE e071-object,
                obj_name   TYPE e071-obj_name,
                    icon   TYPE char01,
     END OF s_final_r.

DATA :          t_final_r TYPE STANDARD TABLE OF s_final_r,
               wa_final_r TYPE s_final_r.


DATA:              r_field TYPE RANGE OF e071-object,
                  wa_field LIKE LINE OF r_field.

"=============================================================================="
"-------------Structures to Read Tranport Import Log---------------------------"
"=============================================================================="
TYPES: BEGIN OF s_ctslg_settings,
          error_level              TYPE  i,
          point_to_missing_steps   TYPE  c,
          detailed_depiction       TYPE  c,
          systems                  TYPE  sysnames,
       END OF s_ctslg_settings.

TYPE-POOLS trwbo .
TYPES: trwbo_charflag(1) TYPE c.

TYPES: BEGIN OF s_trwbo_request_header.
INCLUDE   STRUCTURE e070.
TYPES:    as4text        LIKE e07t-as4text,
          as4text_filled TYPE trwbo_charflag,
          client         LIKE e070c-client,
          tarclient      LIKE e070c-tarclient,
          clients_filled TYPE trwbo_charflag,
          tardevcl       LIKE e070m-tardevcl,
          devclass       LIKE e070m-devclass,
          tarlayer       LIKE e070m-tarlayer,
          e070m_filled   TYPE trwbo_charflag,
       END OF   s_trwbo_request_header.
TYPES: s_trwbo_request_headers TYPE s_trwbo_request_header  OCCURS 0.


TYPES:BEGIN OF s_ctslg_action,
         date             LIKE tstrfcofil-trdate,
         time             LIKE tstrfcofil-trtime,
         rc               LIKE tstrfcofil-retcode,
      END OF s_ctslg_action,
      t_ctslg_actions       TYPE SORTED TABLE OF s_ctslg_action
                               WITH UNIQUE KEY date time
                               INITIAL SIZE 2,

   BEGIN OF s_ctslg_step,
         clientid         LIKE tstrfcofil-tarclient,
         stepid           LIKE tstrfcofil-function,
         rc               LIKE tstrfcofil-retcode,
         actions          TYPE t_ctslg_actions,
      END OF s_ctslg_step,
      t_ctslg_steps         TYPE s_ctslg_step OCCURS 10,

        BEGIN OF s_ctslg_system,
         systemid         LIKE tstrfcofil-tarsystem,
         rc               LIKE tstrfcofil-retcode,
         steps            TYPE t_ctslg_steps,
      END OF s_ctslg_system,
      t_ctslg_systems       TYPE s_ctslg_system OCCURS 10,

             BEGIN OF s_ctslg_mergeline,
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

TYPES: BEGIN OF s_trwbo_request,
          h                 TYPE trwbo_request_header,
          objects           TYPE trwbo_t_e071,
          keys              TYPE trwbo_t_e071k,
          objects_filled    TYPE trwbo_charflag,
          attributes        TYPE trwbo_t_e070a,
          attributes_filled TYPE trwbo_charflag,
       END OF   s_trwbo_request.

TYPES: t_trwbo_requests        TYPE s_trwbo_request         OCCURS 0.

TYPES:   BEGIN OF s_ctslg_request_info,
          int_status       LIKE trsapnames-int_status,
          read_flag        LIKE trsapnames-read_flag,
          header           TYPE trwbo_request_header,
          project          TYPE trkorr_p,
          cofile           TYPE s_ctslg_cofile,
          cofile_filled    TYPE c,
       END OF s_ctslg_request_info,
       t_ctslg_request_infos TYPE s_ctslg_request_info  OCCURS 0,

       BEGIN OF s_ctslg_repair_info,
          int_status       LIKE trsapnames-int_status,
          header           TYPE s_trwbo_request_header,
       END OF s_ctslg_repair_info,
       t_ctslg_repair_infos TYPE s_ctslg_repair_info  OCCURS 0.

DATA:  ls_request          TYPE  s_ctslg_request_info,
       lt_requests         TYPE  t_ctslg_request_infos,
       ls_settings         TYPE  s_ctslg_settings,
       wa_request          TYPE  s_ctslg_request_info,
       "ls_system           TYPE  LINE OF s_ctslg_settings-systems,
       "lv_trkorr           LIKE  e070-trkorr,
       lv_username         LIKE  e070-as4user.


FIELD-SYMBOLS :<fs_requests> TYPE s_ctslg_request_info,
               <fs_systems> TYPE s_ctslg_system,
               <fs_steps> TYPE  s_ctslg_step  ,
               <fs_actions>  TYPE s_ctslg_action.


"====================================================================="
"------------------------project data declaration---------------------"
"====================================================================="
DATA: v_externalid          TYPE ctsproject-externalid.

TYPES:BEGIN OF s_ctsproj,
                 externalps TYPE ctsproject-externalps,
                 externalid TYPE ctsproject-externalid,
                 srcsystem  TYPE ctsproject-srcsystem,
                 srcclient  TYPE ctsproject-srcclient,
                 trkorr     TYPE ctsproject-trkorr,
                 descriptn  TYPE ctsproject-descriptn,
       END OF s_ctsproj.

DATA:  wa_ctsproj           TYPE s_ctsproj.

TYPES: BEGIN OF s_e070a_p,
                  trkorr    TYPE e070a-trkorr,
                  reference TYPE e070a-reference,
       END OF s_e070a_p.

DATA:            t_e070a_p  TYPE STANDARD TABLE OF s_e070a_p.


TYPES: BEGIN OF s_e071_p,
                    trkorr  TYPE e071-trkorr,
                    pgmid   TYPE e071-pgmid,
                   object   TYPE e071-object,
                   obj_name TYPE e071-obj_name,
       END OF s_e071_p.

DATA: t_e071_p                TYPE STANDARD TABLE OF s_e071_p,
      wa_e071_p               TYPE s_e071_p.

TYPES: BEGIN OF s_e07t_p,
                    trkorr  TYPE e07t-trkorr,
                    as4text TYPE e07t-as4text,
       END OF s_e07t_p.

DATA:               t_e07t_p  TYPE STANDARD TABLE OF s_e07t_p,
                   wa_e07t_p  TYPE s_e07t_p.


TYPES :     BEGIN OF s_e070_p,
                   trkorr  TYPE e070-trkorr,
               trfunction  TYPE e070-trfunction,
                 trstatus  TYPE e070-trstatus,
                tarsystem  TYPE e070-tarsystem,
                  korrdev  TYPE e070-korrdev,
                  as4user  TYPE e070-as4user,
                  as4date  TYPE e070-as4date,
                  as4time  TYPE e070-as4time,
                  strkorr  TYPE e070-strkorr,
         END OF s_e070_p.

DATA :              t_e070_p TYPE STANDARD TABLE OF s_e070_p,
                  t_e070_1p TYPE STANDARD TABLE OF s_e070_p,
                  t_e070_2p TYPE STANDARD TABLE OF s_e070_p,
                   wa_e070_p TYPE s_e070_p.


TYPES: BEGIN OF s_final_p,
               "EXTERNALPS type CTSPROJECT-EXTERNALPS,
                externalid TYPE ctsproject-externalid,
                srcsystem  TYPE ctsproject-srcsystem,
               "SRCCLIENT  type CTSPROJECT-SRCCLIENT,
                proj_name  TYPE ctsproject-trkorr,
                descriptn  TYPE ctsproject-descriptn,
                trkorr     TYPE e071-trkorr,
                trfunction TYPE e070-trfunction,
                 trstatus  TYPE e070-trstatus,
                 tarsystem TYPE e070-tarsystem,
                 date      TYPE tstrfcofil-trdate,
                 time      TYPE tstrfcofil-trtime,
                status(60) TYPE c,
                korrdev    TYPE e070-korrdev,
                as4user    TYPE e070-as4user,
                as4text    TYPE e07t-as4text,
                pgmid      TYPE e071-pgmid,
                object     TYPE e071-object,
                obj_name   TYPE e071-obj_name,
                    icon   TYPE char01,
     END OF s_final_p.

DATA :           t_final_p TYPE STANDARD TABLE OF s_final_p,
                wa_final_p TYPE s_final_p.

TYPES: BEGIN OF s_trlist,
               "EXTERNALPS type CTSPROJECT-EXTERNALPS,
                externalid TYPE ctsproject-externalid,
                srcsystem  TYPE ctsproject-srcsystem,
               "SRCCLIENT  type CTSPROJECT-SRCCLIENT,
                proj_name  TYPE ctsproject-trkorr,
                descriptn  TYPE ctsproject-descriptn,
                trkorr     TYPE e071-trkorr,
                trfunction TYPE e070-trfunction,
                 trstatus  TYPE e070-trstatus,
                 tarsystem TYPE e070-tarsystem,
                 date      TYPE tstrfcofil-trdate,
                 time      TYPE tstrfcofil-trtime,
                status(60) TYPE c,
                korrdev    TYPE e070-korrdev,
                as4user    TYPE e070-as4user,
                as4text    TYPE e07t-as4text,
                    icon   TYPE char01,
     END OF s_trlist.

DATA :            t_trlist TYPE STANDARD TABLE OF s_trlist,
                 wa_trlist TYPE s_trlist.


"====================================================================="
"----------------------conflict data declaration----------------------"
"====================================================================="

DATA: t_created             TYPE STANDARD TABLE OF senvi,
      wa_created            TYPE senvi.


TYPES: BEGIN OF s_e071,
                    trkorr  TYPE e071-trkorr,
                    pgmid   TYPE e071-pgmid,
                   object   TYPE e071-object,
                   obj_name TYPE e071-obj_name,
       END OF s_e071.

DATA: t_e071                TYPE STANDARD TABLE OF s_e071,
      t_e071_1              TYPE STANDARD TABLE OF s_e071,
      t_e071_2              TYPE STANDARD TABLE OF s_e071,
      t_e071_t              TYPE STANDARD TABLE OF s_e071,
      t_e071_temp           TYPE STANDARD TABLE OF s_e071,
      wa_e071               TYPE s_e071.

TYPES: BEGIN OF s_e07t,
                    trkorr  TYPE e07t-trkorr,
                    as4text TYPE e07t-as4text,
       END OF s_e07t.

DATA:               t_e07t  TYPE STANDARD TABLE OF s_e07t,
                   t_e07t_t  TYPE STANDARD TABLE OF s_e07t,
                   wa_e07t  TYPE s_e07t.

TYPES :     BEGIN OF s_e070,
                   trkorr  TYPE e070-trkorr,
               trfunction  TYPE e070-trfunction,
                 trstatus  TYPE e070-trstatus,
                tarsystem  TYPE e070-tarsystem,
                  korrdev  TYPE e070-korrdev,
                  as4user  TYPE e070-as4user,
                  as4date  TYPE e070-as4date,
                  as4time  TYPE e070-as4time,
                  strkorr  TYPE e070-strkorr,
         END OF s_e070.

DATA :              t_e070 TYPE STANDARD TABLE OF s_e070,
                  t_e070_t TYPE STANDARD TABLE OF s_e070,
                   wa_e070 TYPE s_e070,
                   wa_e0701 TYPE s_e070.


TYPES: BEGIN OF s_final,
                trkorr     TYPE e071-trkorr,
                trfunction TYPE e070-trfunction,
                 trstatus  TYPE e070-trstatus,
                 tarsystem TYPE e070-tarsystem,
                korrdev    TYPE e070-korrdev,
                as4user    TYPE e070-as4user,
                as4date    TYPE e070-as4date,
                  as4time  TYPE e070-as4time,
                as4text    TYPE e07t-as4text,
                pgmid      TYPE e071-pgmid,
                object     TYPE e071-object,
                obj_name   TYPE e071-obj_name,
     END OF s_final.

DATA :            t_final     TYPE STANDARD TABLE OF s_final,
                  t_present   TYPE STANDARD TABLE OF s_final,
                  t_current   TYPE STANDARD TABLE OF s_final,
                  wa_final    TYPE s_final,
                  wa_present  TYPE s_final,
                  wa_curr     TYPE s_final.

TYPES: BEGIN OF s_final_temp,
                trkorr     TYPE e071-trkorr,
                trfunction TYPE e070-trfunction,
                 trstatus  TYPE e070-trstatus,
                 tarsystem TYPE e070-tarsystem,
                korrdev    TYPE e070-korrdev,
                as4user    TYPE e070-as4user,
                as4date    TYPE e070-as4date,
                  as4time  TYPE e070-as4time,
                as4text    TYPE e07t-as4text,
                pgmid      TYPE e071-pgmid,
                object     TYPE e071-object,
                obj_name   TYPE tadir-obj_name,
     END OF s_final_temp.

DATA: t_dep_temp TYPE STANDARD TABLE OF s_final_temp.


*TYPES : BEGIN OF s_never,
*         trkorr TYPE ztr_never-trkorr,
*      tarsystem TYPE ztr_never-tarsystem,
*          udate TYPE ztr_never-udate,
*          uname TYPE ztr_never-uname,
*      requester TYPE ztr_never-requester,
*  END OF s_never.
*
*DATA: t_trnever  TYPE STANDARD TABLE OF s_never,
*      wa_trnever TYPE t_never.


TYPES: BEGIN OF s_conflict,
            trkorr     TYPE e071-trkorr,
           "trfunction TYPE e070-trfunction,
             trstatus  TYPE e070-trstatus,
            "tarsystem TYPE e070-tarsystem,
           "korrdev    TYPE e070-korrdev,
            as4user    TYPE e070-as4user,
            as4date    TYPE e070-as4date,
              as4time  TYPE e070-as4time,
           "as4text    TYPE e07t-as4text,
            pgmid      TYPE e071-pgmid,
            object     TYPE e071-object,
            obj_name   TYPE e071-obj_name,
         comments(100) TYPE c,
                icon   TYPE char01,
     END OF s_conflict.

DATA: t_conflict TYPE STANDARD TABLE OF s_conflict,
     wa_conflict TYPE s_conflict.


DATA:              v_req TYPE e071-trkorr,
                   v_obj TYPE e071-obj_name.


*&---------------------------------------------------------------------*
*&                     TYPES
*&---------------------------------------------------------------------*
TYPES : BEGIN OF s_curr,
           object       TYPE trobjtype,
           obj_name     TYPE tadir-obj_name,
           devclass     TYPE devclass,
           strkorr      TYPE strkorr,
           text         TYPE e07t-as4text,
           trstatus(27) TYPE c,
           ddtext       TYPE ddtext,
*           trkorr       type trkorr,
       END OF s_curr.

TYPES : BEGIN OF s_itab,
           object       TYPE tadir-object,
           obj_name     TYPE tadir-obj_name,
           func         TYPE tfdir-funcname,
           genflag(1)   TYPE c,
        END OF s_itab.

TYPES : BEGIN OF s_itab2,
           object       TYPE tadir-object,
           obj_name     TYPE dd03l-tabname,
           func         TYPE tfdir-funcname,
           genflag(1)   TYPE c,
        END OF s_itab2.

TYPES : BEGIN OF s_tadir,
          object TYPE tadir-object,
          obj_name      TYPE tadir-obj_name,
          devclass      TYPE tadir-devclass,
          strkorr       TYPE e070-strkorr,
          trkorr        TYPE e070-trkorr,
          trstatus(27)  TYPE c,
          text          TYPE e07t-as4text,
          ddtext        TYPE ddtext,
          genflag(1)    TYPE c,
        END OF s_tadir.

TYPES : BEGIN OF s_fugr,
          object(25)    TYPE c,
          obj_name      TYPE tadir-obj_name,
          devclass      TYPE tadir-devclass,
          strkorr       TYPE e070-strkorr,
          trkorr        TYPE e070-trkorr,
          trstatus(22)  TYPE c,
          text          TYPE e07t-as4text,
        END OF s_fugr.

TYPES : BEGIN OF s_enlfdir,
          funcname      TYPE enlfdir-funcname,
          area          TYPE enlfdir-area,
        END OF s_enlfdir.

TYPES : BEGIN OF s_func,
          funcname      TYPE enlfdir-funcname,
          obj_name      TYPE tadir-obj_name,
          devclass      TYPE tadir-devclass,
        END OF s_func.

TYPES : BEGIN OF s_maxtsk,
          trkorr        TYPE trkorr,
        END OF s_maxtsk.

TYPES: BEGIN OF s_dd04ll ,
          rollname      TYPE dd04l-rollname,
       END OF s_dd04ll.

TYPES: BEGIN OF s_dd04l ,
          rollname      TYPE dd04l-rollname,
          domname       TYPE dd04l-domname,
          memoryid      TYPE dd04l-memoryid,
          shlpname      TYPE dd04l-shlpname,
          refkind       TYPE dd04l-refkind,
          reftype       TYPE dd04l-reftype,
       END OF s_dd04l .

TYPES : BEGIN OF s_check,
          object        TYPE trobjtype,
        END OF s_check.

DATA :
      t_tadir       TYPE STANDARD TABLE OF s_tadir,
      t_senvi_tab   TYPE STANDARD TABLE OF senvi,
      t_senvi_tab1  TYPE STANDARD TABLE OF senvi,
     "t_curr        TYPE STANDARD TABLE OF t_curr,
      t_rsfind      TYPE STANDARD TABLE OF rsfind,
      t_rsfind1     TYPE STANDARD TABLE OF rsfind,
      t_itab        TYPE STANDARD TABLE OF s_itab,
      t_itab1       TYPE STANDARD TABLE OF s_itab,
      t_itab2        TYPE STANDARD TABLE OF s_itab2,
      t_dd03l       TYPE STANDARD TABLE OF dd03l,
      t_enlfdir     TYPE STANDARD TABLE OF s_enlfdir,
      t_itab_dup    TYPE STANDARD TABLE OF s_itab,
      t_environment TYPE STANDARD TABLE OF senvi,
      t_dd04ll      TYPE STANDARD TABLE OF s_dd04ll,
      t_dd04l       TYPE STANDARD TABLE OF s_dd04l.


*&---------------------------------------------------------------------*
*&                     WORK AREA
*&---------------------------------------------------------------------*
DATA:  wa_dd04l       TYPE s_dd04l,
       wa_dd04ll      TYPE s_dd04ll,
       wa_environment TYPE  senvi,
       wa_enlfdir     TYPE s_enlfdir,
       wa_dd03l       TYPE dd03l,
       wa_itab        TYPE s_itab,
       wa_senvi_tab   TYPE senvi,
       wa_envi_types  TYPE envi_types,
       wa_tadir       TYPE s_tadir.

*&---------------------------------------------------------------------*
*&                     VARIABLES
*&---------------------------------------------------------------------*
DATA : v_seoclstype    TYPE seoclass-clstype.

TYPES :BEGIN OF s_missing,
     type         TYPE senvi-type,
     object       TYPE senvi-object,
     call_obj     TYPE senvi-call_obj,
     call_type    TYPE senvi-call_type,
     comments(70) TYPE c,
           icon   TYPE char01,
     END OF s_missing.

DATA:  t_missing TYPE STANDARD TABLE OF s_missing,
      wa_missing TYPE s_missing.

TYPES:  BEGIN OF s_confchk,
      trkorr TYPE trkorr,
      as4pos TYPE ddposition,
       pgmid TYPE pgmid,
      object TYPE trobjtype,
    obj_name TYPE trobj_name,
    trstatus TYPE trstatus,
     as4user TYPE tr_as4user,
     as4date TYPE as4date,
     as4time TYPE as4time,
     targsys TYPE tr_target,
      status TYPE char03,
      icon   TYPE char01,
  END OF s_confchk.

TYPES:     BEGIN OF s_depchk,
       trkorr  TYPE trkorr,
       obj_typ TYPE char10,
       master  TYPE master ,
       tabname TYPE rollname,
       targsys TYPE tr_target,
       status  TYPE char03,
       local   TYPE char03,
        icon   TYPE char01,
     END OF s_depchk.

*Internal tables for formated data from systems.
DATA:t_remote     TYPE STANDARD TABLE OF s_confchk,
     t_remote1    TYPE STANDARD TABLE OF s_confchk.

*Internal tables to select data in target system
DATA: t_options    LIKE rfc_db_opt OCCURS 0 WITH HEADER LINE,
      t_fields     LIKE rfc_db_fld OCCURS 0 WITH HEADER LINE,
      t_options1   LIKE rfc_db_opt OCCURS 0 WITH HEADER LINE,
      t_fields1    LIKE rfc_db_fld OCCURS 0 WITH HEADER LINE,

*Internal table for non formated data retrieved for target system
      t_out_tab    LIKE tab512-wa  OCCURS 0 WITH HEADER LINE,
      t_out_tab1   LIKE tab512-wa  OCCURS 0 WITH HEADER LINE,
      t_re071      TYPE STANDARD TABLE OF s_e071,
      t_re071_r    TYPE STANDARD TABLE OF s_e071,
      t_re070      TYPE STANDARD TABLE OF s_e070,
      t_re070_r      TYPE STANDARD TABLE OF s_e070.

* data:*Workarea Declaration
DATA:wa_remote    TYPE s_confchk,
     wa_remote1   TYPE s_confchk.

DATA: t_text  TYPE rfc_db_opt,
      t_text1 TYPE rfc_db_opt,
      t_lines TYPE i.


"--------------------------------------------------------------------"

TYPES : BEGIN OF s_curr_temp,
      object TYPE e071-object,
    obj_name TYPE e071-obj_name,
    devclass TYPE tadir-devclass,
  END OF s_curr_temp.

  TYPES : BEGIN OF s_curr_temp1,
      object TYPE c length 180,
    call_obj TYPE c length 40,
    type TYPE c length 15,
  END OF s_curr_temp1.

DATA:t_curr_temp TYPE STANDARD TABLE OF s_curr_temp,
      t_curr_temp1 TYPE STANDARD TABLE OF s_curr_temp1,
    wa_curr_temp TYPE s_curr_temp,
    wa_curr_temp1 TYPE s_curr_temp1.


TYPES : BEGIN OF s_deptr,
      trkorr TYPE e071-trkorr,
    obj_name TYPE e071-obj_name,
      object TYPE e071-object,
    blank(4) TYPE c,
      dep_tr TYPE e071-trkorr,
    trstatus TYPE e070-trstatus,
   tarsystem TYPE e070-tarsystem,
    date     TYPE tstrfcofil-trdate,
  status(60) TYPE c,
   time      TYPE tstrfcofil-trtime,
comments(120) TYPE c,
       icon   TYPE char01,
 END OF s_deptr.

DATA : t_deptr TYPE STANDARD TABLE OF s_deptr,
      wa_deptr TYPE s_deptr.
DATA: wa_e071_1 TYPE s_e071,
      wa_e070_1 TYPE s_e070.


DATA : v_pname TYPE pname.


DATA : r_check TYPE RANGE OF e071-object,
       r_object TYPE RANGE OF e071-object,
       r_pgmid TYPE RANGE OF e071-pgmid.
