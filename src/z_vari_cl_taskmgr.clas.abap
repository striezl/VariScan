class Z_VARI_CL_TASKMGR definition
  public
  create public .

public section.

  data LOG_T type ZVARI_LOG_T read-only .
  data VARI_OUT_ALL type ZVARI_OUT_T .

  methods CONSTRUCTOR
    importing
      !MAX_TASKS type INT1 .
  methods TASK_ADD
    importing
      !GROUP type RFCGR .
  methods TASK_END
    importing
      value(P_TASK) type CLIKE .
  methods WAIT_TO_END .
  methods GET_TASK_ID
    returning
      value(TASK_ID) type NUM8 .
protected section.

  data act_tasks type INT1 .
  data maximum_tasks type INT1 .
  data task_id type NUM8 .
private section.

  data GS_RPSSTATS type ZBC_RPSSTATS .
ENDCLASS.



CLASS Z_VARI_CL_TASKMGR IMPLEMENTATION.


METHOD CONSTRUCTOR.

    maximum_tasks    = max_tasks.
    task_id = 1.

ENDMETHOD.                    "constructor


METHOD GET_TASK_ID.

  task_id = me->task_id.

  ADD 1 TO me->task_id.

ENDMETHOD.


METHOD task_add.

  DATA: max_pbt_wps  TYPE i,
        free_pbt_wps TYPE i.

  "Wait for free task
  WAIT FOR ASYNCHRONOUS TASKS UNTIL act_tasks < maximum_tasks UP TO 3 SECONDS.

  "Resource available?
  WHILE free_pbt_wps LE 1. "Keep at least 1 task open
    CALL FUNCTION 'SPBT_INITIALIZE'
      EXPORTING
        group_name                     = group
      IMPORTING
        max_pbt_wps                    = max_pbt_wps
        free_pbt_wps                   = free_pbt_wps
      EXCEPTIONS
        invalid_group_name             = 1
        internal_error                 = 2
        pbt_env_already_initialized    = 3
        currently_no_resources_avail   = 4
        no_pbt_resources_found         = 5
        cant_init_different_pbt_groups = 6
        OTHERS                         = 7.
    IF sy-subrc = 3.
      CALL FUNCTION 'SPBT_GET_CURR_RESOURCE_INFO'
        IMPORTING
          max_pbt_wps                 = max_pbt_wps
          free_pbt_wps                = free_pbt_wps
        EXCEPTIONS
          internal_error              = 1
          pbt_env_not_initialized_yet = 2
          OTHERS                      = 3.
      IF sy-subrc <> 0.
      ENDIF.
    ENDIF.
  ENDWHILE.

  ADD 1 TO act_tasks.

ENDMETHOD.


METHOD task_end.

  DATA: msgtxt(255),
        valuetab    TYPE TABLE OF rsparams,
        vari_out_t  TYPE zvari_out_t,
        log         TYPE zvari_log.

  "Laufende prozessanzahl zurücksetzen
  SUBTRACT 1 FROM act_tasks .

  "Ergebnisse speichern
  RECEIVE RESULTS FROM FUNCTION p_task
      IMPORTING
        vari_out_t = vari_out_t
  EXCEPTIONS
    system_failure        = 1 MESSAGE msgtxt
    communication_failure = 2 MESSAGE msgtxt
*      parallelism_error     = 3
*      invalid_db_cursor     = 4
*      invalid_db_operation  = 5
    OTHERS                = 6.

  IF sy-subrc = 0.
    log-msg = 'Ok'.
  ELSE.
    log-msg = msgtxt.
  ENDIF.
  log-task = p_task.
  APPEND log TO me->log_t.

  APPEND LINES OF vari_out_t TO me->vari_out_all.

ENDMETHOD.                    "TASK_END


method WAIT_TO_END.

*  WAIT UNTIL act_tasks = 0.
  WAIT FOR ASYNCHRONOUS TASKS UNTIL me->act_tasks = 0 UP TO 3 SECONDS.

endmethod.
ENDCLASS.
