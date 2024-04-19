FUNCTION ZF_F110_PROCESS_00002040.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_REGUH) LIKE  REGUH STRUCTURE  REGUH
*"  TABLES
*"      T_FIMSG STRUCTURE  FIMSG
*"  CHANGING
*"     VALUE(C_FINAA) LIKE  FINAA STRUCTURE  FINAA
*"----------------------------------------------------------------------

* default: print payment advice
*  c_finaa-nacha = '1'.

* Send email if email address is available
* Read email address of vendor from lfb1 using I_REGUH-LIFNR.

 IF NOT I_REGUH-LIFNR IS INITIAL AND
    NOT I_REGUH-ZBUKR IS INITIAL.
    SELECT SINGLE INTAD
      INTO C_FINAA-INTAD
      FROM LFB1
     WHERE LIFNR = I_REGUH-LIFNR
       AND BUKRS = I_REGUH-ZBUKR.

       IF SY-SUBRC = 0.
          C_FINAA-NACHA = 'I'.
       ENDIF.
 ENDIF.

* if email was not possible: try fax (message type 2)
*  IF c_finaa-nacha NE 'I'.
*    IF NOT i_reguh-ztlfx IS INITIAL.
*      c_finaa-nacha      = '2'.               "Transmission Medium: Fax
*      c_finaa-tdschedule = 'IMM'.             "Send Immediately
*      c_finaa-tdteleland = i_reguh-zland.       "Country Key
*      c_finaa-tdtelenum  = i_reguh-ztlfx.       "Fax Number
*      c_finaa-formc      = 'FI_FAX_COVER_A4'. "Layout for cover sheet
*    ENDIF.
*  ENDIF.

ENDFUNCTION.
