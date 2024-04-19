*&---------------------------------------------------------------------*
*& Report  ZTRANSPORT_ANALYZER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZTRANSPORT_ANALYZER.

"========================================================================================"
"----------------------------------Includes----------------------------------------------"
"========================================================================================"

"--------------------------Data Declaration----------------------------------------------"

INCLUDE ZTL_BCR056L_GLOBAL.
*INCLUDE ZTL_BCR056L_GLOBAL2.
*INCLUDE ztl_bcr056l_global.

"---------------------------Selection Screen---------------------------------------------"

INCLUDE ZTL_BCR056L_SELECTION_SCREEN.
*INCLUDE ZTL_BCR056L_SELECTION_SCREEN2.
*INCLUDE ztl_bcr056l_selection_screen.

"--------------------------Search Helps & Validation-------------------------------------"

INCLUDE ZTL_BCR056L_TR_VALIDATION.
*INCLUDE ZTL_BCR056L_TR_VALIDATION2.
*INCLUDE ztl_bcr056l_tr_validation.


"========================================================================================"
"-------------------------Start 0f Selection --------------------------------------------"
"========================================================================================"

START-OF-SELECTION.

  "-------------- Validation for Obligatory Fields-----------------------------"

  PERFORM mandatory_validation.

  "-------------- Create and Fill Fange for all Object Type--------------------"

  PERFORM fill_range_checkbox.

  "---------------To get all the objects related object type-------------------"
  PERFORM check_obj_type_all TABLES r_object.

  "---------------To get all the objects related program ID--------------------"
  PERFORM check_pgmid_all TABLES r_pgmid.

*  08/06/2018
  "-------------To get all the Environment Types-------------------------"
    PERFORM environment_types.

  "============================================================================"
  " ----------------Process Project Data---------------------------------------"
  "============================================================================"

*  IF rd_proj IS NOT INITIAL.          "Project ID
*    PERFORM data_process_project.
*  ENDIF.

  "============================================================================"
  "--------------------ALV Diplay For Project data-----------------------------"
  "============================================================================"

*  IF rd_proj IS NOT INITIAL.         "Project ID
*    IF t_final_p IS NOT INITIAL OR t_trlist IS NOT INITIAL.
*      PERFORM display_alv_project.
*    ENDIF.
*  ENDIF.

  "============================================================================"
  " ----------------Process Object Data----------------------------------------"
  "============================================================================"

  IF rd_reprt IS NOT INITIAL.        "Object
    PERFORM data_process_object.
  ENDIF.

  "============================================================================"
  "--------------------ALV Diplay For Report data------------------------------"
  "============================================================================"

  IF rd_reprt IS NOT INITIAL.         "Object
    IF t_final_r IS NOT INITIAL.
      PERFORM display_alv_object.
    ENDIF.
  ENDIF.

  "============================================================================"
  "--------------------Data Processing for Conflict Check----------------------"
  "============================================================================"

  IF rd_cnflc IS NOT INITIAL.        "Conflict Check
    PERFORM refresh_tables.          "Refesh Tables

***    "-------------To get all the Environment Types-------------------------"
***    PERFORM environment_types.

    "-------------Process for Smartforms-----------------------------------"
*    IF rd_smrt IS NOT INITIAL.
*      PERFORM smartform_program.
*    ENDIF.

    "----------List of TR's and Objects in the Source System---------------"
    IF rd_conf IS NOT INITIAL OR         "Conflick Check
       rd_dep IS NOT INITIAL.            "Dependency Check
*      IF rd_smrt IS INITIAL.             "Not for smartforms
        PERFORM objects_request.
*      ENDIF.
    ENDIF.

    "--------------------------------------------------------------------------"
    "----------------------------Conflict Check--------------------------------"
    "--------------------------------------------------------------------------"
    IF rd_conf IS NOT INITIAL.           "Conflick Check
*      IF s_smrtfm IS INITIAL.           "Not For Smartforms
        PERFORM conflict_check.
*      ENDIF.
      "-----------To get TR's & Objects of Target System-------------------"
      PERFORM target_objects.
    ENDIF.

    "---------------------------------------------------------------------------"
    "----------------------------Dependency Check-------------------------------"
    "---------------------------------------------------------------------------"
    IF rd_dep IS NOT INITIAL.            "Dependency Check
      PERFORM dependecy_check.
    ENDIF.

    "---------------------------------------------------------------------------"
    "----------------------------Missing Object Check---------------------------"
    "---------------------------------------------------------------------------"
    IF rd_miss IS NOT INITIAL.          "Missing Objects Check
      PERFORM missing_objects.
    ENDIF.

    PERFORM refresh_tables.            "Refesh Tables

  ENDIF.

  "==================================================================================="
  "----------------------ALV Display Transport Conflict Check-------------------------"
  "==================================================================================="

  IF rd_cnflc IS NOT INITIAL.        "Conflict Check
    PERFORM display_alv.
  ENDIF.

  "==================================================================================="
  "-----------------------Include for all the subriutine forms------------------------"
  "==================================================================================="

INCLUDE ZTL_BCR056L_TR_ROUTINE_FORM.
*INCLUDE ZTL_BCR056L_TR_ROUTINE_FORMS22.
*  INCLUDE ztl_bcr056l_tr_routine_forms.
