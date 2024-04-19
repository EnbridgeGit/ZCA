***INCLUDE RSEIDOC_DAT .
TABLES: EDIDC, TEDS2.

TABLES: EDPP1.

TABLES: EDOC_STAT.

TABLES: STACUST, STALIGHT, t100.

DATA: IS_VARIANT LIKE DISVARIANT.

data: need_refresh(1).
data: sel_index like sy-tabix.
DATA: NUMBER TYPE I.
DATA: SELECT_ALL_USE(1).
DATA: TIME_0 LIKE EDIDC-UPDTIM VALUE '000000',
      TIME_24 LIKE EDIDC-UPDTIM VALUE '240000'.
DATA: BEGIN OF I_STACUST OCCURS 0,
        STATUS LIKE STACUST-STATUS,
        STATVA LIKE STACUST-STATVA,
      END OF I_STACUST.

DATA: BEGIN OF I_STALIGHT OCCURS 0,
        STATVA LIKE STALIGHT-STATVA,
        STALIGHT LIKE STALIGHT-STALIGHT,
      END OF I_STALIGHT.

DATA: BEGIN OF INT_EDIDC OCCURS 50.
        INCLUDE STRUCTURE EDIDC.
DATA: END OF INT_EDIDC.

data: begin of int_edids occurs 0.
        include structure edids.
data: end of int_edids.

data: i_listedidc like listedidc occurs 0.
data: wa_listedidc like listedidc.

data: begin of i_edidc occurs 0.
        include structure edidc.
data: end of i_edidc.

data: begin of inb_edidc occurs 0,
        mestyp like edidc-mestyp,
        status like edidc-status,
        docnum like edidc-docnum,
      end of inb_edidc.

data: begin of out_edidc occurs 0,
        mestyp like edidc-mestyp,
        status like edidc-status,
        docnum like edidc-docnum,
      end of out_edidc.

data: status_01 like edidd-segnum,
      status_02 like edidd-segnum,
      status_03 like edidd-segnum,
      status_04 like edidd-segnum,
      status_05 like edidd-segnum,
      status_06 like edidd-segnum,
      status_07 like edidd-segnum,
      status_08 like edidd-segnum,
      status_09 like edidd-segnum,
      status_10 like edidd-segnum,
      status_11 like edidd-segnum,
      status_12 like edidd-segnum,
      status_13 like edidd-segnum,
      status_14 like edidd-segnum,
      status_15 like edidd-segnum,
      status_16 like edidd-segnum,
      status_17 like edidd-segnum,
      status_18 like edidd-segnum,
      status_19 like edidd-segnum,
      status_20 like edidd-segnum,
      status_21 like edidd-segnum,
      status_22 like edidd-segnum,
      status_23 like edidd-segnum,
      status_24 like edidd-segnum,
      status_25 like edidd-segnum,
      status_26 like edidd-segnum,
      status_27 like edidd-segnum,
      status_29 like edidd-segnum,
      status_30 like edidd-segnum,
      status_31 like edidd-segnum,
      status_32 like edidd-segnum,
      status_33 like edidd-segnum,
      status_34 like edidd-segnum,
      status_35 like edidd-segnum,
      status_36 like edidd-segnum,
      status_37 like edidd-segnum,
      status_38 like edidd-segnum,
      status_39 like edidd-segnum,
      status_40 like edidd-segnum,
      status_41 like edidd-segnum,
      status_42 like edidd-segnum.

data: status_50 like edidd-segnum,
      status_51 like edidd-segnum,
      status_52 like edidd-segnum,
      status_53 like edidd-segnum,
      status_54 like edidd-segnum,
      status_55 like edidd-segnum,
      status_56 like edidd-segnum,
      status_57 like edidd-segnum,
      status_58 like edidd-segnum,
      status_60 like edidd-segnum,
      status_61 like edidd-segnum,
      status_62 like edidd-segnum,
      status_63 like edidd-segnum,
      status_64 like edidd-segnum,
      status_65 like edidd-segnum,
      status_66 like edidd-segnum,
      status_68 like edidd-segnum,
      status_69 like edidd-segnum,
      status_70 like edidd-segnum,
      status_71 like edidd-segnum,
      status_73 like edidd-segnum,
      status_74 like edidd-segnum.


DATA: BEGIN OF INT_TEDS2 OCCURS 30.
        INCLUDE STRUCTURE TEDS2.
DATA: END OF INT_TEDS2.

parameters: docking like edi_help-onl_option default 'O' no-display.
SELECTION-SCREEN BEGIN OF SCREEN 1100 AS SUBSCREEN.
     SELECT-OPTIONS: CRETIM  FOR EDIDC-CRETIM DEFAULT TIME_0 TO TIME_24.
*---------------------------  UGL Change --------------------------- UGL
*SELECT-OPTIONS: CREDAT  FOR EDIDC-CREDAT DEFAULT SY-DATUM TO SY-DATUM,
SELECT-OPTIONS: CREDAT  FOR EDIDC-CREDAT DEFAULT SY-DATUM,          "UGL
                enddat  for edidc-credat default sy-datum,          "UGL
*-------------------------  End of UGL Change  --------------------- UGL
                UPDTIM  FOR EDIDC-UPDTIM DEFAULT TIME_0 TO TIME_24,
                UPDDAT  FOR EDIDC-UPDDAT.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS: DIRECT  FOR EDIDC-DIRECT,
                DOCNUM  FOR EDIDC-DOCNUM,
                STATUS  FOR EDIDC-STATUS.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS: IDOCTP  FOR EDIDC-IDOCTP,
                CIMTYP  FOR EDIDC-CIMTYP,
                MESTYP  FOR EDIDC-MESTYP.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS: SNDPOR  FOR EDOC_STAT-SNDPOR,
                SNDPRT  FOR EDPP1-PARTYP,
                SNDPRN  FOR EDOC_STAT-SNDPRN.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS: RCVPOR  FOR EDOC_STAT-RCVPOR,
                RCVPRT  FOR EDPP1-PARTYP,
                RCVPRN  FOR EDOC_STAT-RCVPRN.
SELECTION-SCREEN END OF SCREEN 1100.
SELECTION-SCREEN BEGIN OF SCREEN 1200 AS SUBSCREEN.
SELECT-OPTIONS: MESCOD  FOR EDIDC-MESCOD,
                MESFCT  FOR EDIDC-MESFCT.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS: RCVPFC  FOR EDIDC-RCVPFC,
                SNDPFC  FOR EDIDC-SNDPFC,
                TEST    FOR EDIDC-TEST.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN END OF SCREEN 1200.
SELECTION-SCREEN BEGIN OF SCREEN 1300 AS SUBSCREEN.
SELECT-OPTIONS: REFINT  FOR EDIDC-REFINT,
                REFGRP  FOR EDIDC-REFGRP,
                REFMES  FOR EDIDC-REFMES,
                ARCKEY  FOR EDIDC-ARCKEY.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS: STD     FOR EDIDC-STD,
                STDVRS  FOR EDIDC-STDVRS,
                STDMES  FOR EDIDC-STDMES.
SELECTION-SCREEN END OF SCREEN 1300.
SELECTION-SCREEN BEGIN OF SCREEN 9999 AS SUBSCREEN.               "UGL
parameters:    RPT_HDR(50) type c.                                "UGL
SELECTION-SCREEN END OF SCREEN 9999.                              "UGL

SELECTION-SCREEN BEGIN OF TABBED BLOCK IDOCTABBL FOR 20 LINES.
SELECTION-SCREEN TAB (15) SOS_TABL USER-COMMAND SOS_TAB
                 DEFAULT SCREEN 1100.
SELECTION-SCREEN TAB (15) SOS_TAB2 USER-COMMAND SOS_TAB2
                 DEFAULT SCREEN 1200.
SELECTION-SCREEN TAB (15) SOS_TAB3 USER-COMMAND SOS_TAB3
                 DEFAULT SCREEN 1300.
SELECTION-SCREEN TAB (15) SOS_TAB4 USER-COMMAND SOS_TAB4         "UGL
                 DEFAULT SCREEN 9999.                            "UGL

SELECTION-SCREEN END OF BLOCK IDOCTABBL.
