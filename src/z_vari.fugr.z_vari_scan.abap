FUNCTION z_vari_scan.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(REPORT) TYPE  VARI_REPRT
*"     VALUE(VARIANT) TYPE  VARIANT
*"     VALUE(SEARCHTERM) TYPE  STRING
*"  EXPORTING
*"     VALUE(VARI_OUT_T) TYPE  ZVARI_OUT_T
*"----------------------------------------------------------------------

  DATA: t_field_info TYPE TABLE OF scr_info,
        valuetab     TYPE TABLE OF rsparams,
        values       LIKE LINE OF valuetab,
        values1      LIKE LINE OF valuetab,
        match        TYPE flag,
*        para LIKE LINE OF parameters,
        vari_out     LIKE LINE OF vari_out_t,
        valuetab_l   TYPE TABLE OF rsparamsl,
        values_l     LIKE LINE OF valuetab_l,
        values_l1    LIKE values_l.

**This is necessary to see if there is a selection screen or the program will dump
*    CLEAR t_field_info.
  CALL FUNCTION 'RS_SELSCREEN_INFO'
    EXPORTING
      report              = report
    TABLES
      field_info          = t_field_info
    EXCEPTIONS
      no_selections       = 1
      report_not_existent = 2
      subroutine_pool     = 3
      OTHERS              = 4.
  CHECK sy-subrc IS INITIAL. "ToDo Exceptionhandling

*    CATCH SYSTEM-EXCEPTIONS OTHERS = 1.
  TRY.
      CALL FUNCTION 'RS_VARIANT_CONTENTS'
        EXPORTING
          report               = report
          variant              = variant
*         MOVE_OR_WRITE        = 'W'
*         NO_IMPORT            = ' '
*         EXECUTE_DIRECT       = ' '
*   IMPORTING
*         SP                   =
        TABLES
*         L_PARAMS             =
*         L_PARAMS_NONV        =
*         L_SELOP              =
*         L_SELOP_NONV         =
          valutab              = valuetab
          valutabl             = valuetab_l
*         OBJECTS              =
*         FREE_SELECTIONS_DESC =
*         FREE_SELECTIONS_VALUE       =
*         FREE_SELECTIONS_OBJ  =
        EXCEPTIONS
          variant_non_existent = 1
          variant_obsolete     = 2
          OTHERS               = 3.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
    CATCH cx_root.
  ENDTRY.
*    ENDCATCH.

*  LOOP AT valuetab INTO values.
*    IF values-low CP searchterm OR values-high CP searchterm.
*      LOOP AT valuetab INTO values1
*        WHERE selname = values-selname.
**        WRITE: / varid-report NO-GAP, `|` NO-GAP, varid-variant NO-GAP, `|` NO-GAP, values1-selname NO-GAP, `|` NO-GAP, values1-low NO-GAP, `|` NO-GAP, values1-high NO-GAP.
*        CLEAR vari_out.
*        MOVE-CORRESPONDING values1 TO vari_out.
*        vari_out-variant = variant.
*        vari_out-report = report.
*        APPEND vari_out TO vari_out_t.
*      ENDLOOP.
*      EXIT.
*    ENDIF.
*  ENDLOOP.

*  LOOP AT valuetab_l INTO values_l.
*    IF values_l-low CP searchterm OR values_l-high CP searchterm.
*      LOOP AT valuetab_l INTO values_l1
*        WHERE selname = values_l-selname.
*        CLEAR vari_out.
*        MOVE-CORRESPONDING values_l1 TO vari_out.
*        vari_out-variant = variant.
*        vari_out-report = report.
*        APPEND vari_out TO vari_out_t.
*      ENDLOOP.
*    ENDIF.
*  ENDLOOP.

  LOOP AT valuetab_l INTO values_l
    WHERE low CP searchterm OR high CP searchterm.
    CLEAR vari_out.
    MOVE-CORRESPONDING values_l TO vari_out.
    vari_out-variant = variant.
    vari_out-report = report.
    APPEND vari_out TO vari_out_t.
  ENDLOOP.

ENDFUNCTION.
