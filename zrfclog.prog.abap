*&---------------------------------------------------------------------*
*& Report ZRFCLOG
*&---------------------------------------------------------------------*
*& 程序描述：显示RFC日志
*& 创建日期：2017.08
*& 作者： HUANGHANWEN
*&---------------------------------------------------------------------*
REPORT zrfclog NO STANDARD PAGE HEADING.
TABLES: zlogdata.
DATA: gv_log_lines TYPE i.

*&=====================================================================*
*&      初始化
*&=====================================================================*
INITIALIZATION.
  DATA(go_controller) = NEW zcl_rfclog( ). "RFC LOG

*&=====================================================================*
*&      选择屏幕
*&=====================================================================*
  " 选择功能
  SELECTION-SCREEN:BEGIN OF BLOCK block_1 WITH FRAME TITLE TEXT-t01.
  PARAMETERS:
    p_query RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND u1,
    p_archi RADIOBUTTON GROUP g1.
  SELECTION-SCREEN END OF BLOCK block_1.

  " 查询条件
  SELECTION-SCREEN:BEGIN OF BLOCK block_2 WITH FRAME TITLE TEXT-t02.
  SELECT-OPTIONS:s_area FOR zlogdata-area MODIF ID m1.
  SELECT-OPTIONS:s_name FOR zlogdata-name MODIF ID m1.
  SELECT-OPTIONS:s_erdat FOR zlogdata-erdat MODIF ID m1.
  SELECT-OPTIONS:s_zeit FOR zlogdata-zeit MODIF ID m1.
  SELECT-OPTIONS:s_ernam FOR zlogdata-ernam MODIF ID m1.
  SELECT-OPTIONS:s_sysid FOR zlogdata-ext_sysid MODIF ID m1.
  SELECT-OPTIONS:s_seqno FOR zlogdata-ext_seqno MODIF ID m1.
  SELECTION-SCREEN END OF BLOCK block_2.

  " 归档条件
  SELECTION-SCREEN:BEGIN OF BLOCK block_3 WITH FRAME TITLE TEXT-t03.
  PARAMETERS:
    p_archi1 RADIOBUTTON GROUP g2 MODIF ID m2 USER-COMMAND u2 DEFAULT 'X',
    p_archi2 RADIOBUTTON GROUP g2 MODIF ID m2.
  SELECTION-SCREEN END OF BLOCK block_3.

  " 附加条件
  SELECTION-SCREEN:BEGIN OF BLOCK block_4 WITH FRAME TITLE TEXT-t04.
  PARAMETERS:
    c_error AS CHECKBOX  USER-COMMAND u3 MODIF ID m1,
    c_archi AS CHECKBOX  MODIF ID m1.
  SELECTION-SCREEN END OF BLOCK block_4.

*&=====================================================================*
*&      选择屏幕动态显示
*&=====================================================================*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CASE 'X'.
      WHEN p_query. "-查询日志
        IF screen-group1 = 'M1'.
          screen-active = 1.
        ELSEIF screen-group1 = 'M2'.
          screen-active = 0.
        ENDIF.
      WHEN p_archi. "-归档日志
        IF screen-group1 = 'M1'.
          screen-active = 0.
        ELSEIF screen-group1 = 'M2'.
          screen-active = 1.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
  IF s_erdat[] IS INITIAL. "查询日期默认为当天
    APPEND INITIAL LINE TO s_erdat ASSIGNING FIELD-SYMBOL(<erdat>).
    <erdat>-sign = 'I'.
    <erdat>-option = 'EQ'.
    <erdat>-low = sy-datum .
    <erdat>-high = sy-datum.
  ENDIF.

*&=====================================================================*
*&      选择屏幕事件
*&=====================================================================*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_area-low.
  zcl_rfclog=>f4_function_group( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_name-low.
  zcl_rfclog=>f4_function_module( ).

AT SELECTION-SCREEN.
  go_controller->get_para_from_selscreen(
    EXPORTING iv_program = sy-repid
              iv_screen = sy-dynnr ).

*&=====================================================================*
*&      开始执行
*&=====================================================================*
START-OF-SELECTION.
  CASE 'X'.
    WHEN p_query. "-查询日志
      IF go_controller->get_log_lines( ) < 100000. " 限制查询10万条日志
        gv_log_lines = go_controller->get_log_data( ).
        IF gv_log_lines > 0.
          CALL SCREEN 5000.
        ELSE.
          MESSAGE '没有符合条件的数据' TYPE 'S'.
        ENDIF.
      ELSE.
        MESSAGE '查询结果条目过多，请限制查询条件！' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    WHEN p_archi. "-归档日志
      go_controller->archive_log_data( ).

  ENDCASE.

*&=====================================================================*
*&      PBO MODULE
*&=====================================================================*
MODULE pbo OUTPUT.
  DATA(lv_title) = |RFC调用日志 [共{ gv_log_lines }条]|.
  SET PF-STATUS 'STATUS'.
  SET TITLEBAR 'TITLE' WITH lv_title.
  go_controller->show_alv(  ).
ENDMODULE.

*&=====================================================================*
*&      PAI MODULE
*&=====================================================================*
MODULE pai INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANC'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'REFRESH'.
      go_controller->get_log_data( ).
      go_controller->refresh_alv( ).
    WHEN 'DETAIL'.
      go_controller->show_log_detail( ).
    WHEN 'SE37'.
      go_controller->show_log_se37( ).
    WHEN 'JSON'.
      go_controller->show_log_json( ).
    WHEN 'XML'.
      go_controller->show_log_xml( ).
  ENDCASE.
ENDMODULE.
