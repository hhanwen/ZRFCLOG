*&---------------------------------------------------------------------*
*&  Include  ZRFCLOG_SAVE
*&---------------------------------------------------------------------*
  DATA: lt_para    TYPE zcl_rfclog=>tt_para,
        ls_log_key TYPE zcl_rfclog=>ty_log_key,
        ls_log     TYPE zlogdata,
        lv_time1   TYPE p,
        lv_time2   TYPE p.

  FIELD-SYMBOLS: <lv_any> TYPE any.


*&=====================================================================*
*&      保存日志 （包含流水号历史纪录唯一性检查）
*&=====================================================================*
  DEFINE zrfclog_save.

    IF &1 = '01'.
      CLEAR ls_log.

      " guid
      ls_log-guid = cl_uuid_factory=>create_system_uuid( )->create_uuid_c32( ).

      " function group & function name
      zcl_rfclog=>get_calling_function(
        IMPORTING ev_func_group = ls_log-area
                  ev_func_name = ls_log-name ).

      " host name & host ip
      zcl_rfclog=>get_ext_host(
        IMPORTING ev_host_name = ls_log-ext_host
                  ev_host_ip   = ls_log-ext_ip ).

      " external system id
      ASSIGN ('IV_SYSID') TO <lv_any>.
      IF sy-subrc = 0.
        ls_log-ext_sysid = <lv_any>.
      ENDIF.

      " external sequence number
      ASSIGN ('IV_SEQNO') TO <lv_any>.
      IF sy-subrc = 0.
        ls_log-ext_seqno = <lv_any>.
      ENDIF.

      " current date & time & user
      ls_log-erdat = sy-datum.
      ls_log-zeit = sy-uzeit.
      ls_log-ernam = sy-uname.

    ENDIF.

    " run time
    IF &1 = '01'.
      GET RUN TIME FIELD lv_time1.
    ELSE.
      GET RUN TIME FIELD lv_time2.
      ls_log-runtime  = ( lv_time2 - lv_time1 ) / 1000000.
    ENDIF.

    " save point
    ls_log-indx = &1.
    " message type
    ls_log-rtype = &2.
    " message text
    ls_log-rtmsg = &3.


    " log key
    MOVE-CORRESPONDING ls_log TO ls_log_key.

    " get rfc parameters
    zcl_rfclog=>get_function_para(
      EXPORTING iv_indx = ls_log-indx
                iv_area = ls_log-area
                iv_name = ls_log-name
      IMPORTING et_para = lt_para ).

    " save log
    EXPORT (lt_para) TO DATABASE zlogdata(fl) FROM ls_log ID ls_log_key.
    COMMIT WORK.
    CLEAR: ls_log_key, lt_para.


    " check duplicate sequence number
    IF ls_log-indx = '01' AND ls_log-ext_seqno IS NOT INITIAL.
      IF zcl_rfclog=>check_log_exists( EXPORTING iv_name = ls_log-name
                                                  iv_indx = '02'
                                                  iv_sysid = ls_log-ext_sysid
                                                  iv_seqno = ls_log-ext_seqno
                                       IMPORTING es_log_key = ls_log_key ).
        zcl_rfclog=>get_function_para(
          EXPORTING
                    iv_indx = '02'
                    iv_area = ls_log-area
                    iv_name = ls_log-name
          IMPORTING et_para = lt_para ).

        IMPORT (lt_para) FROM DATABASE zlogdata(fl) ID ls_log_key
          IGNORING CONVERSION ERRORS.

        ls_log-indx = '02' .
        ls_log-rtype = 'E'.
        ls_log-rtmsg = '流水号重复，已跳过执行并返回前一次结果'.
        GET RUN TIME FIELD lv_time2.
        ls_log-runtime  = ( lv_time2 - lv_time1 ) / 1000000.

        MOVE-CORRESPONDING ls_log TO ls_log_key.
        EXPORT (lt_para) TO DATABASE zlogdata(fl) FROM ls_log ID ls_log_key.
        COMMIT WORK.
        RETURN.
      ENDIF.
    ENDIF.

  END-OF-DEFINITION.
