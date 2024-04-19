*&---------------------------------------------------------------------*
*&  Include           ZTL_BCR056L_TR_VALIDATION
*&---------------------------------------------------------------------*


"------------------------------------------------------------------------"
"----------Report screen fields Search Helps & validation----------------"
"------------------------------------------------------------------------"

"Search help for Report NAme--------------------------------------"

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_pgm-low.
*  PERFORM program_f4help.
*
*  "Search help for Report NAme--------------------------------------"
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_pgm-high.
*  PERFORM program_f4help.

"Validate Program / Report Name-----------------------------------"


AT SELECTION-SCREEN ON s_pgm.
  IF rd_reprt IS NOT INITIAL AND
      s_pgm IS NOT INITIAL.
    PERFORM validate_program.
  ENDIF.
  "------------------------------------------------------------------------"
  "--------Project screen fields Search Helps & validation-----------------"
  "------------------------------------------------------------------------"
  "Search help for Project NAme--------------------------------------"

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_extid-low.
*  IF rd_proj IS NOT INITIAL.
*    PERFORM ctsproj_f4help.
*  ENDIF.
*
*  "Validate Project Name---------------------------------------------"
*
*AT SELECTION-SCREEN ON s_extid.
*  IF rd_proj IS NOT INITIAL AND
*     s_extid IS NOT INITIAL.
*    PERFORM validate_project.
*  ENDIF.

  "------------------------------------------------------------------------"
  "--------Conflict screen fields Search Helps & validation----------------"
  "------------------------------------------------------------------------"

  "Validate Transport / Request-----------------------------------"

AT SELECTION-SCREEN ON s_req.
  IF rd_cnflc IS NOT INITIAL AND
        s_req IS NOT INITIAL.
    PERFORM validate_request.
  ENDIF.

  "Validate Report/ Program---------------------------------------"

AT SELECTION-SCREEN ON s_report.
  IF rd_cnflc IS NOT INITIAL AND
     s_report IS NOT INITIAL.
    PERFORM validate_report.
  ENDIF.

  "Validate Smartform---------------------------------------------"

*AT SELECTION-SCREEN ON s_smrtfm.
*  IF rd_cnflc IS NOT INITIAL AND
*     s_smrtfm IS NOT INITIAL.
*    PERFORM validate_smartform.
*  ENDIF.

  "Validate Table-------------------------------------------------"

AT SELECTION-SCREEN ON s_table.
  IF rd_cnflc IS NOT INITIAL AND
      s_table IS NOT INITIAL.
    PERFORM validate_table.
  ENDIF.

  "Validate Domain-------------------------------------------------"

AT SELECTION-SCREEN ON s_domain.
  IF rd_cnflc IS NOT INITIAL AND
     s_domain IS NOT INITIAL.
    PERFORM validate_domain.
  ENDIF.
  "Validate Function Group-----------------------------------------"

AT SELECTION-SCREEN ON s_fugp.
  IF rd_cnflc IS NOT INITIAL AND
       s_fugp IS NOT INITIAL.
    PERFORM validate_function_grp.
  ENDIF.

  "Validate Function Module----------------------------------------"

AT SELECTION-SCREEN ON s_func.
  IF rd_cnflc IS NOT INITIAL AND
       s_func IS NOT INITIAL.
    PERFORM validate_function_module.
  ENDIF.
