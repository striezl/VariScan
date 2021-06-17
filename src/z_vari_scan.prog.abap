REPORT z_vari_scan.
********************************************************************************
* See https://github.com/striezl/VariScan
*
* The MIT License (MIT)
*
* Copyright (c) 2020 VariScan Contributers
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
********************************************************************************

DATA: t_varid      TYPE TABLE OF varid,
      s_varid      LIKE LINE OF t_varid,
      valuetab     TYPE TABLE OF rsparams,
      lines        TYPE i,
      percent      TYPE p DECIMALS 2,
      match        TYPE flag,
      devclass     TYPE tadir-devclass,
      taskmgr      TYPE REF TO z_vari_cl_taskmgr,
      searchterm   TYPE string,
      taskid       TYPE num8,
      columns      TYPE REF TO cl_salv_columns_table,
      display      TYPE REF TO cl_salv_display_settings,
      lkey         TYPE        salv_s_layout_key,
      layout       TYPE REF TO cl_salv_layout,
      auth_badi    TYPE REF TO z_vari_auth,
      alv          TYPE REF TO cl_salv_table,
      authorized   TYPE flag,
      dynsel       TYPE string,
      blacklist    TYPE flag,
      auth_missing TYPE abap_bool.

CONSTANTS: ablm_blacklist TYPE string VALUE 'ABLM_BLACKLIST'.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
SELECT-OPTIONS: so_rep FOR s_varid-report,
                so_devcl FOR devclass.
PARAMETERS: p_term(45) OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
PARAMETERS: p_tasks  TYPE int1 DEFAULT 5 OBLIGATORY,
            p_srvgrp TYPE rfcgr.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.

*  AUTHORITY-CHECK OBJECT 'ZVARI' ID 'ZVARI' FIELD 'X'.
  GET BADI auth_badi.
  CALL BADI auth_badi->check_authority
    RECEIVING
      ok = authorized.
  IF authorized = abap_false.
    MESSAGE e000(z_vari).
  ENDIF.

  "Default exclusions which have caused problems, e.g. dumps, might differ on various systems
  so_rep-option = 'EQ'.
  so_rep-sign = 'E'.
  so_rep-low = 'RCCARCH3'.
  APPEND so_rep.
  so_rep-low = 'RPRPAY00'.
  APPEND so_rep TO so_rep[].
  so_devcl-option = 'CP'.
  so_devcl-sign = 'E'.
  so_devcl-low = 'PC*'.
  APPEND so_devcl.
  so_devcl-option = 'EQ'.
  so_devcl-low = 'SAPWL_OLD_COLLECTOR'.
  APPEND so_devcl.

START-OF-SELECTION.

*  SELECT devclass FROM tdevc
*    INTO TABLE @DATA(t_devcl)
*    WHERE devclass IN @so_devcl.
*  LOOP AT t_devcl INTO data(devcl).
*   AUTHORITY-CHECK OBJECT 'S_PROGRAM'
*            ID 'P_GROUP' FIELD devcl
*            ID 'P_ACTION' FIELD 'VARIANT'.
*   IF sy-subrc <> 0.
*     "
*   ENDIF.
*  ENDLOOP.

  SELECT * FROM varid
   INTO TABLE t_varid
    WHERE report IN so_rep
    ORDER BY report variant.

  lines = lines( t_varid ).

  CREATE OBJECT taskmgr
    EXPORTING
      max_tasks = p_tasks.

  SELECT COUNT(*) FROM dd02l
    WHERE tabname = 'ABLM_BLACKLIST'
      AND tabclass = 'TRANSP'.
  IF sy-subrc = 0.
    blacklist = 'X'.
  ENDIF.

  LOOP AT t_varid INTO s_varid.

    CLEAR: valuetab, match.

    IF sy-tabix MOD 10 = 0.
      percent = 100 * sy-tabix / lines.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = percent
          text       = |{ TEXT-000 } { percent }%|.
    ENDIF.

    DATA secu TYPE trdir-secu.
    CLEAR secu.
    SELECT SINGLE secu FROM trdir
    INNER JOIN tadir ON tadir~object = 'PROG' AND trdir~name = tadir~obj_name
      INTO secu
      WHERE name = s_varid-report
        AND subc = '1'
        AND uccheck  = 'X'
*          AND ( rstat = 'K' OR rstat = 'P' )
        AND rstat NE 'S' "Exclude System Programs
        AND devclass IN so_devcl.
    CHECK sy-subrc = 0.
    CALL BADI auth_badi->check_authority_secu
      EXPORTING
        secu = secu
      RECEIVING
        ok   = authorized.
    IF authorized = abap_false.
      auth_missing = abap_true.
      CONTINUE.
    ENDIF.

    "This is to avoid dumps in S/4
    IF blacklist IS NOT INITIAL.
      dynsel = | EXECTYPE = 'PROG' AND EXECNAME = '{ s_varid-report }' |.
      SELECT COUNT(*) FROM (ablm_blacklist)
        WHERE (dynsel).
      CHECK sy-subrc NE 0.
    ENDIF.

    taskmgr->task_add( p_srvgrp ).
    taskid = taskmgr->get_task_id( ).

    searchterm = p_term.

    CALL FUNCTION 'Z_VARI_SCAN'
      STARTING NEW TASK taskid
      CALLING taskmgr->task_end ON END OF TASK
      EXPORTING
        report     = s_varid-report
        variant    = s_varid-variant
        searchterm = searchterm.

  ENDLOOP.

  taskmgr->wait_to_end( ).

  IF auth_missing = abap_true.
    MESSAGE s001(z_vari) DISPLAY LIKE 'W'.
  ENDIF.

*  DATA log LIKE LINE OF taskmgr->log_t. "ToDo
*  LOOP AT taskmgr->log_t INTO log.
*    WRITE: / log-task, log-msg.
*  ENDLOOP.

  cl_salv_table=>factory(
    IMPORTING
      r_salv_table   = alv
    CHANGING
      t_table        =  taskmgr->vari_out_all
  ).

  alv->set_screen_status(
    pfstatus = 'ALV1'
    report = sy-repid
    set_functions = alv->c_functions_all ).

  columns ?= alv->get_columns( ).
  columns->set_optimize( abap_true ).

  display ?= alv->get_display_settings( ).
  display->set_striped_pattern( abap_true ).

  lkey-report = sy-repid.
  layout ?= alv->get_layout( ).
  layout->set_key( lkey ).
  layout->set_default( abap_true ).
  layout->set_save_restriction( ).

  alv->display( ).
