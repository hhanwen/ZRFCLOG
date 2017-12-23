class ZCL_RFCLOG definition
  public
  create public .

public section.
  type-pools ABAP .

  types:
    BEGIN OF ty_log_key,
        guid  TYPE guid_32,
        name  TYPE rs38l_fnam,
        erdat TYPE erdat,
        zeit  TYPE uzeit,
        indx  TYPE numc2,
      END OF ty_log_key .
  types:
    BEGIN OF ty_para,
        para_name  TYPE rs38l_par_,
        para_value TYPE rs38l_par_,
      END OF ty_para .
  types:
    BEGIN OF ty_select_para,
        attribute_name TYPE string,
        sign           TYPE ddsign,
        option         TYPE ddoption,
        low            TYPE string,
        high           TYPE string,
      END OF ty_select_para .
  types:
    BEGIN OF ty_data.
        INCLUDE TYPE zlogdata.
    types archive_ind type char1.
    TYPES save_point TYPE char10.
    TYPES cellcolors TYPE lvc_t_scol.
    TYPES END OF ty_data .
  types:
    tt_para TYPE STANDARD TABLE OF ty_para .
  types:
    tt_select_para TYPE STANDARD TABLE OF ty_select_para .
  types:
    tt_data TYPE STANDARD TABLE OF ty_data .
  types:
    tt_import TYPE STANDARD TABLE OF rsimp .
  types:
    tt_export TYPE STANDARD TABLE OF rsexp .
  types:
    tt_change TYPE STANDARD TABLE OF rscha .
  types:
    tt_tables TYPE STANDARD TABLE OF rstbl .

  methods DELETE_LOG_DATA
    returning
      value(RV_RESULT) type ABAP_BOOL .
  class-methods MAIN .
  methods CONSTRUCTOR .
  methods SHOW_ALV .
  methods REFRESH_ALV .
  methods GET_SELECTED_ROW
    returning
      value(RV_ROW_INDEX) type LVC_INDEX .
  methods HANDLE_DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_GUI_ALV_GRID
    importing
      !E_ROW .
  class-methods GET_EXT_HOST
    exporting
      value(EV_HOST_NAME) type ETCALL_HOST
      value(EV_HOST_IP) type RFCIPADDR .
  methods HANDLE_TOOLBAR
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT
      !E_INTERACTIVE .
  class-methods GET_CALLING_FUNCTION
    exporting
      value(EV_FUNC_GROUP) type RS38L_AREA
      value(EV_FUNC_NAME) type RS38L_FNAM
      value(ET_TABLES) type RSFB_PARA
      value(ET_IMPORT) type RSFB_PARA
      value(ET_EXPORT) type RSFB_PARA
      value(ET_CHANGE) type RSFB_PARA .
  class-methods GET_FUNCTION_PARA
    importing
      value(IV_INDX) type NUMC2
      value(IV_AREA) type RS38L_AREA
      value(IV_NAME) type RS38L_FNAM
    exporting
      value(ET_PARA) type TT_PARA .
  methods GET_LOG_DATA
    returning
      value(RV_LINES) type I ." lines of log
  methods GET_LOG_LINES
    returning
      value(RV_LINES) type I .
  methods GET_PARA_FROM_SELSCREEN
    importing
      !IV_PROGRAM type SY-REPID default SY-REPID
      !IV_SCREEN type SY-DYNNR default SY-DYNNR .
  methods SHOW_LOG_DETAIL .
  class-methods DESCRIBE_INTERFACE
    importing
      !IV_FUNC_NAME type RS38L_FNAM
    exporting
      !ET_IMPORT type TT_IMPORT
      !ET_EXPORT type TT_EXPORT
      !ET_CHANGE type TT_CHANGE
      !ET_TABLES type TT_TABLES .
  class-methods DESCRIBE_PARA_DETAIL
    importing
      !IS_LOG type TY_DATA
      !IV_PARAMETER type PARAMETER
      !IV_TYPE type RS38L_TYP
      !IV_LIKE type LIKEFIELD
      !IV_KIND type CHAR1
    changing
      !CO_OUTPUT type ref to IF_DEMO_OUTPUT .
  methods SHOW_LOG_JSON .
  methods SHOW_LOG_XML .
  methods SHOW_LOG_SE37 .
  class-methods DESCRIBE_PARA_JSON
    importing
      !IS_LOG type TY_DATA
      !IV_PARAMETER type PARAMETER
      !IV_TYPE type RS38L_TYP
      !IV_LIKE type LIKEFIELD
      !IV_KIND type CHAR1
    changing
      !CO_OUTPUT type ref to IF_SXML_WRITER .
  class-methods DESCRIBE_PARA_XML
    importing
      !IS_LOG type TY_DATA
      !IV_PARAMETER type PARAMETER
      !IV_TYPE type RS38L_TYP
      !IV_LIKE type LIKEFIELD
      !IV_KIND type CHAR1
    changing
      !CO_OUTPUT type ref to IF_SXML_WRITER .
  class-methods DESCRIBE_PARA_SE37
    importing
      !IS_LOG type TY_DATA
      !IV_PARAMETER type PARAMETER
      !IV_TYPE type RS38L_TYP
      !IV_LIKE type LIKEFIELD
      !IV_KIND type CHAR1 .
  class-methods CREATE_JSON_ELEMENT
    importing
      !IV_NAME type STRING
      !IV_VALUE type ANY
      !IV_TYPE type ABAP_TYPEKIND
    changing
      !CO_OUTPUT type ref to IF_SXML_WRITER .
  class-methods CREATE_XML_ELEMENT
    importing
      !IV_NAME type STRING
      !IV_VALUE type ANY
      !IV_TYPE type ABAP_TYPEKIND
    changing
      !CO_OUTPUT type ref to IF_SXML_WRITER .
  class-methods F4_FUNCTION_GROUP
    importing
      !IV_DYNPROG type SY-REPID default SY-REPID
      !IV_DYNNR type SY-DYNNR default SY-DYNNR
      !IV_DYNFIELD type HELP_INFO-DYNPROFLD default 'S_AREA-LOW' .
  class-methods F4_FUNCTION_MODULE
    importing
      value(IV_DYNPROG) type SY-REPID default SY-REPID
      value(IV_DYNNR) type SY-DYNNR default SY-DYNNR
      value(IV_DYNFIELD) type HELP_INFO-DYNPROFLD default 'S_NAME-LOW' .
  class-methods CLEAR_TEST_DATA
    importing
      !IV_DATE type DATUM
      !IV_NAME type RS38L_FNAM
    returning
      value(RV_RESULT) type ABAP_BOOL .
  methods ARCHIVE_LOG_DATA
    returning
      value(RV_RESULT) type ABAP_BOOL .
  class-methods CHECK_LOG_EXISTS
    importing
      !IV_NAME type RS38L_FNAM
      !IV_INDX type NUMC2 default '02'
      !IV_SYSID type RSDRSYSID
      !IV_SEQNO type SCPRCHAR220
    exporting
      !ES_LOG_KEY type TY_LOG_KEY
    returning
      value(RV_RESULT) type ABAP_BOOL .
  PROTECTED SECTION.
private section.

  constants MV_LOG_TABLE type TABNAME value 'ZLOGDATA' ##NO_TEXT.
  constants MV_ARCHIVE_TABLE type TABNAME value 'ZLOGDATA_ARCHIVE' ##NO_TEXT.
  data GO_CONTAINER type ref to CL_GUI_CUSTOM_CONTAINER .
  data GO_ALV type ref to CL_GUI_ALV_GRID .
  data GT_FIELDCATALOG type LVC_T_FCAT .
  data GS_LAYOUT type LVC_S_LAYO .
  data GS_VARIANT type DISVARIANT .
  data GD_DATA type ref to DATA .
  data GT_PARAMETERS type TT_SELECT_PARA .
  data GT_DATA type TT_DATA .
  data GREEN type LVC_T_SCOL .
  data RED type LVC_T_SCOL .
  data GO_OUTPUT type ref to IF_DEMO_OUTPUT .
  data GO_XML type ref to IF_SXML_WRITER .
  data GO_JSON type ref to IF_SXML_WRITER .

  methods SET_FIELDCATALOG .
  methods SET_FIELD
    importing
      !IV_FIELDNAME type LVC_FNAME
      !IV_SCRTEXT_L type SCRTEXT_L
      !IV_OUTPUTLEN type LVC_OUTLEN optional
      !IV_NO_ZERO type LVC_NOZERO optional .
  methods SET_LAYOUT .
ENDCLASS.



CLASS ZCL_RFCLOG IMPLEMENTATION.


  METHOD archive_log_data.
    DATA: lv_archive_one_year  TYPE char1,
          lv_archive_half_year TYPE char1,
          lv_archive_date      TYPE datum,
          lt_archive_table     TYPE STANDARD TABLE OF zlogdata_archive.

    LOOP AT gt_parameters ASSIGNING FIELD-SYMBOL(<ls_parameter>).
      CASE <ls_parameter>-attribute_name.
        WHEN 'P_ARCHI1'.
          lv_archive_one_year = <ls_parameter>-low.
        WHEN 'P_ARCHI2'.
          lv_archive_half_year = <ls_parameter>-low.
      ENDCASE.
    ENDLOOP.

    CASE 'X'.
      WHEN lv_archive_one_year. "归档一年前的日志
        lv_archive_date = sy-datum - 365.
      WHEN lv_archive_half_year."归档半年前的日志
        lv_archive_date = sy-datum - 182.
    ENDCASE.

    CHECK lv_archive_date IS NOT INITIAL.
    SELECT * INTO TABLE @lt_archive_table
      FROM zlogdata
      WHERE erdat < @lv_archive_date.
    IF sy-subrc = 0.
      MODIFY zlogdata_archive FROM TABLE lt_archive_table.
      IF sy-subrc = 0.
        DELETE FROM zlogdata WHERE erdat < lv_archive_date.
      ENDIF.
    ENDIF.

    IF sy-subrc = 0.
      MESSAGE '日志归档成功' TYPE 'S'.
    ELSE.
      MESSAGE '没有符合条件的日志' TYPE 'S'.
    ENDIF.
  ENDMETHOD.


  METHOD check_log_exists.
    SELECT SINGLE zlogdata~* INTO CORRESPONDING FIELDS OF @ES_log_key
      FROM zlogdata
      WHERE indx = @iv_indx
       AND  NAME =  @IV_NAME
       AND ext_sysid = @iv_sysid
       AND ext_seqno = @iv_seqno.
      IF SY-SUBRC = 0.
        RV_RESULT = ABAP_true.
      ELSE.
        rv_result = abap_false.
      ENDIF.
  ENDMETHOD.


  METHOD CLEAR_TEST_DATA.

    DELETE FROM zlogdata WHERE erdat = iv_date
                           AND name = iv_name.
    IF sy-subrc = 0.
      rv_result = 'S'.
    ELSE.
      RV_RESULT = 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    DATA:  ls_color TYPE lvc_s_scol.

    ls_color-fname = 'RTYPE'.
    ls_color-color-col = '5' .
    ls_color-color-int = '1' .
    INSERT ls_color INTO TABLE green.

    ls_color-fname = 'RTMSG'.
    ls_color-color-col = '5' .
    ls_color-color-int = '1' .
    INSERT ls_color INTO TABLE green.

    ls_color-fname = 'RTYPE'.
    ls_color-color-col = '6' .
    ls_color-color-int = '1' .
    INSERT ls_color INTO TABLE red.

    ls_color-fname = 'RTMSG'.
    ls_color-color-col = '6' .
    ls_color-color-int = '1' .
    INSERT ls_color INTO TABLE red.

  ENDMETHOD.


  METHOD create_json_element.
    FIELD-SYMBOLS:
      <table>  TYPE ANY TABLE,
      <struct> TYPE            any,
      <field>  TYPE            any.

    DATA: lo_json         TYPE REF TO cl_sxml_string_writer,
          lo_node         TYPE REF TO if_sxml_value_node,
          lv_json_xstring TYPE        xstring.

    CASE iv_type.
      WHEN cl_abap_typedescr=>typekind_table " internal table
         OR cl_abap_typedescr=>kind_table.     " table type
        co_output->open_element( name = 'object' ).
        co_output->write_attribute( name = 'name' value =  iv_name  ).
*      CO_OUTPUT->write_value( CONV #( iv_value ) ).
        ASSIGN iv_value TO <table>.
        LOOP AT <table> ASSIGNING <struct>.
          DATA(lv_type) =  cl_abap_typedescr=>describe_by_data( <struct> )->type_kind.
          zcl_rfclog=>create_json_element(
            EXPORTING iv_name = 'item'
                      iv_value = <struct>
                      iv_type = lv_type
             CHANGING co_output = co_output ).
        ENDLOOP.
        co_output->close_element( ).

      WHEN cl_abap_typedescr=>kind_struct    " structure
         OR cl_abap_typedescr=>typekind_struct1.

        co_output->open_element( name = 'object' ).
        co_output->write_attribute( name = 'name' value =  iv_name  ).
        DATA(lt_components) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( iv_value ) )->components.
        LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<ls_comp>).
          ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE iv_value TO <field>.
          zcl_rfclog=>create_json_element(
            EXPORTING iv_name = CONV #( <ls_comp>-name )
                      iv_value = <field>
                      iv_type = 'C'
             CHANGING co_output = co_output ).
        ENDLOOP.
        co_output->close_element( ).

      WHEN OTHERS.
        co_output->open_element( name = 'str' ).
        co_output->write_attribute( name = 'name' value =  iv_name  ).
        co_output->write_value( CONV #( iv_value ) ).
        co_output->close_element( ).
    ENDCASE.
  ENDMETHOD.


  METHOD create_xml_element.
    FIELD-SYMBOLS:
      <table>  TYPE ANY TABLE,
      <struct> TYPE            any,
      <field>  TYPE            any.

    DATA: lo_json         TYPE REF TO cl_sxml_string_writer,
          lo_node         TYPE REF TO if_sxml_value_node,
          lv_json_xstring TYPE        xstring.

    CASE iv_type.
      WHEN cl_abap_typedescr=>typekind_table " internal table
         OR cl_abap_typedescr=>kind_table.     " table type
        co_output->open_element( name = iv_name ).
*       CO_OUTPUT->write_attribute( name = 'name' value =  iv_name  ).
*       CO_OUTPUT->write_value( CONV #( iv_value ) ).
        ASSIGN iv_value TO <table>.
        LOOP AT <table> ASSIGNING <struct>.
          DATA(lv_type) =  cl_abap_typedescr=>describe_by_data( <struct> )->type_kind.
          zcl_rfclog=>create_xml_element(
            EXPORTING iv_name = 'item'
                      iv_value = <struct>
                      iv_type = lv_type
             CHANGING co_output = co_output ).
        ENDLOOP.
        co_output->close_element( ).

      WHEN cl_abap_typedescr=>kind_struct    " structure
         OR cl_abap_typedescr=>typekind_struct1.

        co_output->open_element( name = iv_name ).
*       CO_OUTPUT->write_attribute( name = 'name' value =  iv_name  ).
        DATA(lt_components) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( iv_value ) )->components.
        LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<ls_comp>).
          ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE iv_value TO <field>.
          zcl_rfclog=>create_xml_element(
            EXPORTING iv_name = CONV #( <ls_comp>-name )
                      iv_value = <field>
                      iv_type = 'C'
             CHANGING co_output = co_output ).
        ENDLOOP.
        co_output->close_element( ).

      WHEN OTHERS.
        IF iv_name(1) <> '/'.
          co_output->open_element( name = iv_name ).
*         CO_OUTPUT->write_attribute( name = 'name' value =  iv_name  ).
          co_output->write_value( CONV #( iv_value ) ).
          co_output->close_element( ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  method DELETE_LOG_DATA.
    DATA: BEGIN OF ls_clear,
            clear1 TYPE char1,
            clear2 TYPE char1,
            clear3 TYPE char1,
            clear4 TYPE char1,
          END OF ls_clear.
    DATA: lr_area TYPE RANGE OF zlogdata-area,
          lr_name TYPE RANGE OF zlogdata-name.
    DATA: lv_date TYPE sy-datum.

    LOOP AT gt_parameters ASSIGNING FIELD-SYMBOL(<ls_parameter>).
      CASE <ls_parameter>-attribute_name.
        WHEN 'S_AREA'.
          APPEND INITIAL LINE TO lr_area ASSIGNING FIELD-SYMBOL(<ls_area>).
          MOVE-CORRESPONDING <ls_parameter> TO <ls_area>.
        WHEN 'S_NAME'.
          APPEND INITIAL LINE TO lr_name ASSIGNING FIELD-SYMBOL(<ls_name>).
          MOVE-CORRESPONDING <ls_parameter> TO <ls_name>.
        WHEN 'P_CLEAR1'.
          ls_clear-clear1 = <ls_parameter>-low.
        WHEN 'P_CLEAR2'.
          ls_clear-clear2 = <ls_parameter>-low.
        WHEN 'P_CLEAR3'.
          ls_clear-clear3 = <ls_parameter>-low.
        WHEN 'P_CLEAR4'.
          ls_clear-clear4 = <ls_parameter>-low.
      ENDCASE.
    ENDLOOP.

    CASE 'X'.
      WHEN ls_clear-clear1. "删除一年之前日志
        lv_date = sy-datum - 365.
        DELETE FROM zlogdata WHERE erdat < lv_date
                               AND area IN lr_area
                               AND name IN lr_name.
      WHEN ls_clear-clear2. "删除半年之前日志

        lv_date = sy-datum - 180.
        DELETE FROM zlogdata WHERE erdat < lv_date
                                     AND area IN lr_area
                               AND name IN lr_name.
      WHEN ls_clear-clear3."删除一个月之前日志

        lv_date = sy-datum - 30.
        DELETE FROM zlogdata WHERE erdat < lv_date
                                     AND area IN lr_area
                                      AND name IN lr_name.
      WHEN ls_clear-clear4. "删除所有日志
        DELETE FROM zlogdata WHERE
                       area IN lr_area
                       AND name IN lr_name.
    ENDCASE.


    IF sy-subrc = 0.
      MESSAGE '日志删除成功' TYPE 'S'.
    ELSE.
      MESSAGE '没有符合条件的日志' TYPE 'S'.
    ENDIF.
  endmethod.


  METHOD describe_interface.

    DATA: lt_except TYPE STANDARD TABLE OF rsexc.
    DATA: lt_documentation TYPE STANDARD TABLE OF funct.

    CALL FUNCTION 'FUNCTION_IMPORT_DOKU'
      EXPORTING
        funcname           = iv_func_name
        with_enhancements  = 'X'
      TABLES
        exception_list     = lt_except
        export_parameter   = et_export
        import_parameter   = et_import
        changing_parameter = et_change
        tables_parameter   = et_tables
        dokumentation      = lt_documentation
      EXCEPTIONS
        error_message      = 1
        function_not_found = 2
        invalid_name       = 3
        OTHERS             = 4.


  ENDMETHOD.


  METHOD describe_para_detail.

    DATA: ls_log_key TYPE ty_log_key.

    DATA: ld_table TYPE REF TO data,
          ld_line  TYPE REF TO data.

    FIELD-SYMBOLS: <para>  TYPE                 any,
                   <table> TYPE STANDARD TABLE.

    DATA: lt_para TYPE tt_para.


    IF iv_type IS NOT INITIAL.
      CREATE DATA ld_table TYPE TABLE OF (iv_type).
    ELSEIF iv_like IS NOT INITIAL.
      CREATE DATA ld_table TYPE TABLE OF (iv_like).
    ENDIF.

    ASSIGN ld_table->* TO <table>.
    IF iv_kind = 'T'.
      ASSIGN ld_table->* TO <para>.
    ELSE.
      CREATE DATA ld_line LIKE LINE OF <table>.
      ASSIGN ld_line->* TO <para>.
    ENDIF.

    CHECK <para> IS ASSIGNED.

    APPEND INITIAL LINE TO lt_para ASSIGNING FIELD-SYMBOL(<ls_para>).
    <ls_para>-para_name  = iv_parameter.
    <ls_para>-para_value = '<PARA>'.

    MOVE-CORRESPONDING is_log TO ls_log_key.
    TRY .
      if is_log-archive_ind is INITIAL.
        IMPORT (lt_para) FROM DATABASE ZLOGDATA(fl) ID ls_log_key IGNORING CONVERSION ERRORS.
      else.
        IMPORT (lt_para) FROM DATABASE zlogdata_archive(fl) ID ls_log_key IGNORING CONVERSION ERRORS.
      endif.
        co_output->write_data( EXPORTING value = <para>
                                         name = CONV #( iv_parameter ) ).
      CATCH cx_sy_import_mismatch_error.
        co_output->write_data( EXPORTING value = '参数结构有变更,无法显示旧数据'
                                 name = CONV #( iv_parameter ) ).
        MESSAGE '参数结构有变更,无法显示旧数据' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
    ENDTRY.

  ENDMETHOD.


  METHOD describe_para_json.

    DATA: ls_log_key TYPE ty_log_key.

    DATA: ld_table TYPE REF TO data,
          ld_line  TYPE REF TO data.

    FIELD-SYMBOLS: <para>  TYPE                 any,
                   <table> TYPE STANDARD TABLE.

    DATA: lt_para TYPE tt_para.


    IF iv_type IS NOT INITIAL.
      CREATE DATA ld_table TYPE TABLE OF (iv_type).
    ELSEIF iv_like IS NOT INITIAL.
      CREATE DATA ld_table TYPE TABLE OF (iv_like).
    ENDIF.

    ASSIGN ld_table->* TO <table>.
    IF iv_kind = 'T'.
      ASSIGN ld_table->* TO <para>.
    ELSE.
      CREATE DATA ld_line LIKE LINE OF <table>.
      ASSIGN ld_line->* TO <para>.
    ENDIF.

    CHECK <para> IS ASSIGNED.

    APPEND INITIAL LINE TO lt_para ASSIGNING FIELD-SYMBOL(<ls_para>).
    <ls_para>-para_name  = iv_parameter.
    <ls_para>-para_value = '<PARA>'.

    MOVE-CORRESPONDING is_log TO ls_log_key.
    TRY .
      if is_log-archive_ind is INITIAL.
        IMPORT (lt_para) FROM DATABASE zlogdata(fl) ID ls_log_key IGNORING CONVERSION ERRORS.
      else.
        IMPORT (lt_para) FROM DATABASE zlogdata_archive(fl) ID ls_log_key IGNORING CONVERSION ERRORS.
      endif.
        DATA(lv_type) =  cl_abap_typedescr=>describe_by_data( <para> )->type_kind.
        zcl_rfclog=>create_json_element(
          EXPORTING iv_name = CONV #( iv_parameter )
                    iv_value = <para>
                    iv_type = lv_type
           CHANGING co_output = co_output ).
      CATCH cx_sy_import_mismatch_error.
        MESSAGE '参数结构有变更,无法显示旧数据' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
    ENDTRY.


  ENDMETHOD.


  METHOD describe_para_se37.

    DATA: ls_log_key TYPE ty_log_key.

    DATA: ld_table TYPE REF TO data,
          ld_line  TYPE REF TO data.

    FIELD-SYMBOLS: <para>  TYPE                 any,
                   <table> TYPE STANDARD TABLE.

    DATA: lt_para TYPE tt_para.

    DATA: lv_program    TYPE trdir-name,
          lv_table_name TYPE string.

    lv_program =  is_log-name.
    TRANSLATE lv_program USING ' ='.
    lv_program+30 = 'FT'.

    IF iv_type IS NOT INITIAL.
      CREATE DATA ld_table TYPE TABLE OF (iv_type).
    ELSEIF iv_like IS NOT INITIAL.
      CREATE DATA ld_table TYPE TABLE OF (iv_like).
    ENDIF.

    ASSIGN ld_table->* TO <table>.
    IF iv_kind = 'T'.
      ASSIGN ld_table->* TO <para>.
    ELSE.
      CREATE DATA ld_line LIKE LINE OF <table>.
      ASSIGN ld_line->* TO <para>.
    ENDIF.

    CHECK <para> IS ASSIGNED.

    APPEND INITIAL LINE TO lt_para ASSIGNING FIELD-SYMBOL(<ls_para>).
    <ls_para>-para_name  = iv_parameter.
    <ls_para>-para_value = '<PARA>'.

    MOVE-CORRESPONDING is_log TO ls_log_key.
    TRY .
      if is_log-archive_ind is INITIAL.
        IMPORT (lt_para) FROM DATABASE zlogdata(fl) ID ls_log_key IGNORING CONVERSION ERRORS.
      else.
        IMPORT (lt_para) FROM DATABASE zlogdata_archive(fl) ID ls_log_key IGNORING CONVERSION ERRORS.
      endif.

        IF iv_kind <> 'T'.
          PERFORM parameter_set IN PROGRAM (lv_program) USING iv_parameter 'I' <para> IF FOUND.
        ELSE.
          lv_table_name = '(' && lv_program && ')' && '%_I' && iv_parameter && '[]'.
          ASSIGN (lv_table_name) TO FIELD-SYMBOL(<lt_table>).
          IF sy-subrc = 0.
            <lt_table> = <para>.
          ENDIF.
        ENDIF.


      CATCH cx_sy_import_mismatch_error.
        MESSAGE '参数结构有变更,无法显示旧数据' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
    ENDTRY.


  ENDMETHOD.


  METHOD describe_para_xml.

    DATA: ls_log_key TYPE ty_log_key.

    DATA: ld_table TYPE REF TO data,
          ld_line  TYPE REF TO data.

    FIELD-SYMBOLS: <para>  TYPE                 any,
                   <table> TYPE STANDARD TABLE.

    DATA: lt_para TYPE tt_para.


    IF iv_type IS NOT INITIAL.
      CREATE DATA ld_table TYPE TABLE OF (iv_type).
    ELSEIF iv_like IS NOT INITIAL.
      CREATE DATA ld_table TYPE TABLE OF (iv_like).
    ENDIF.

    ASSIGN ld_table->* TO <table>.
    IF iv_kind = 'T'.
      ASSIGN ld_table->* TO <para>.
    ELSE.
      CREATE DATA ld_line LIKE LINE OF <table>.
      ASSIGN ld_line->* TO <para>.
    ENDIF.

    CHECK <para> IS ASSIGNED.

    APPEND INITIAL LINE TO lt_para ASSIGNING FIELD-SYMBOL(<ls_para>).
    <ls_para>-para_name  = iv_parameter.
    <ls_para>-para_value = '<PARA>'.

    MOVE-CORRESPONDING is_log TO ls_log_key.
    TRY .
      if is_log-archive_ind is INITIAL.
        IMPORT (lt_para) FROM DATABASE zlogdata(fl) ID ls_log_key IGNORING CONVERSION ERRORS.
      else.
        IMPORT (lt_para) FROM DATABASE zlogdata_archive(fl) ID ls_log_key IGNORING CONVERSION ERRORS.
      endif.
*        co_output->write_data( EXPORTING value = <para>
*                                         name = CONV #( iv_parameter ) ).
        DATA(lv_type) =  cl_abap_typedescr=>describe_by_data( <para> )->type_kind.
        zcl_rfclog=>create_xml_element(
          EXPORTING iv_name = CONV #( iv_parameter )
                    iv_value = <para>
                    iv_type = lv_type
           CHANGING co_output = co_output ).
      CATCH cx_sy_import_mismatch_error.
        MESSAGE '参数结构有变更,无法显示旧数据' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
    ENDTRY.


  ENDMETHOD.


  METHOD f4_function_group.
    TYPES :BEGIN OF ty_table ,
             area  TYPE tlibt-area,
             areat TYPE tlibt-areat,
           END OF ty_table.
    DATA: lt_table TYPE STANDARD TABLE OF ty_table.
    SELECT DISTINCT tlibt~area, tlibt~areat
      INTO TABLE @lt_table
      FROM tlibt INNER JOIN zlogdata
      ON tlibt~area = zlogdata~area
    AND tlibt~spras = @sy-langu.

    IF lt_table IS NOT INITIAL.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = 'AREA'
          dynpprog        = sy-repid
          dynpnr          = sy-dynnr
          dynprofield     = 'S_AREA-LOW'
          value_org       = 'S'
        TABLES
          value_tab       = lt_table
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid
          TYPE sy-msgty
          NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD f4_function_module.
    TYPES: BEGIN OF ty_table,
             funcname TYPE tftit-funcname,
             stext    TYPE tftit-stext,
           END OF ty_table.
    DATA: lt_table TYPE TABLE OF ty_table.

    SELECT DISTINCT tftit~funcname, tftit~stext
      INTO TABLE @lt_table
      FROM tftit INNER JOIN zlogdata
      ON tftit~funcname = zlogdata~name
    AND tftit~spras = @sy-langu.

    IF lt_table IS NOT INITIAL.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = 'FUNCNAME'
          dynpprog        = iv_dynprog
          dynpnr          = iv_dynnr
          dynprofield     = iv_dynfield
          value_org       = 'S'
        TABLES
          value_tab       = lt_table
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid
          TYPE sy-msgty
          NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_calling_function.
    DATA:
      lt_callstack    TYPE         abap_callstack,
      lt_et_callstack TYPE         sys_callst,
      ls_callstack    LIKE LINE OF lt_callstack,
      ls_header       TYPE         header_fb.

    REFRESH: lt_callstack, lt_et_callstack.
    CALL FUNCTION 'SYSTEM_CALLSTACK'
      EXPORTING
        max_level    = 2
      IMPORTING
        callstack    = lt_callstack
        et_callstack = lt_et_callstack.
    READ TABLE lt_callstack  INTO ls_callstack WITH KEY blocktype = 'FUNCTION'.

    IF sy-subrc = 0.

      ev_func_name = ls_callstack-blockname.

      SELECT SINGLE area INTO ev_func_group
        FROM enlfdir
        WHERE funcname = ev_func_name.

      ls_header-area = ev_func_group.
      ls_header-name = ev_func_name.
      CALL METHOD cl_fb_parameter_db=>read
        IMPORTING
          tables = et_tables
          import = et_import
          export = et_export
          change = et_change
        CHANGING
          header = ls_header.

    ENDIF.

  ENDMETHOD.


  METHOD get_ext_host.

    DATA hostadr TYPE uinfo-hostadr.
    DATA hostaddr(8).
    DATA term TYPE uinfo-term.
    DATA xhcount TYPE i.
    DATA: iptxt(15),itimes TYPE i,itimes1 TYPE i,hx(2).
    DATA: result       TYPE i,resulttxt(3).

    CALL FUNCTION 'TH_USER_INFO'
      IMPORTING
        hostaddr = hostadr
        terminal = term.

    hostaddr = hostadr.
    DO 4 TIMES.
      hx = hostaddr+itimes1(2).
      itimes = 0.
      result = 0.
      DO 2 TIMES.
        CASE hx+itimes(1).
          WHEN 'A'.
            IF itimes = 0.
              result = result + 10 * 16.
            ELSE.
              result = result + 10.
            ENDIF.
          WHEN 'B'.
            IF itimes = 0.
              result = result + 11 * 16.
            ELSE.
              result = result + 11.
            ENDIF.
          WHEN 'C'.
            IF itimes = 0.
              result = result + 12 * 16.
            ELSE.
              result = result + 12.
            ENDIF.
          WHEN 'D'.
            IF itimes = 0.
              result = result + 13 * 16.
            ELSE.
              result = result + 13.
            ENDIF.
          WHEN 'E'.
            IF itimes = 0.
              result = result + 14 * 16.
            ELSE.
              result = result + 14.
            ENDIF.
          WHEN 'F'.
            IF itimes = 0.
              result = result + 15 * 16.
            ELSE.
              result = result + 15.
            ENDIF.
          WHEN OTHERS.
            IF itimes = 0.
              result = result + hx+itimes(1) * 16.
            ELSE.
              result = result + hx+itimes(1).
            ENDIF.
        ENDCASE.
        itimes = itimes + 1.
      ENDDO.
      resulttxt = result.
      IF iptxt <> ''.
        CONCATENATE iptxt '.' resulttxt INTO iptxt.
      ELSE.
        iptxt = resulttxt.
      ENDIF.
      itimes1 = itimes1 + 2.
    ENDDO.

    ev_host_ip = iptxt.
    ev_host_name = term.

  ENDMETHOD.


  METHOD get_function_para.

    DATA: ls_header TYPE         header_fb,
          lt_tables TYPE         rsfb_para,
          lt_import TYPE         rsfb_para,
          lt_export TYPE         rsfb_para,
          lt_change TYPE         rsfb_para,
          ls_para   LIKE LINE OF et_para.

    FIELD-SYMBOLS: <ls_para> TYPE rsfbpara.


    ls_header-area = iv_area.
    ls_header-name = iv_name.

    CALL METHOD cl_fb_parameter_db=>read
      IMPORTING
        tables = lt_tables
        import = lt_import
        export = lt_export
        change = lt_change
      CHANGING
        header = ls_header.

*&---------------------------------------------------------------------*
*&  Importing parameter data ( only beginning )
*&---------------------------------------------------------------------*
*    IF iv_indx = '01'.
    LOOP AT  lt_import ASSIGNING <ls_para>.
      ls_para-para_name = <ls_para>-parameter.
      ls_para-para_value = <ls_para>-parameter.
      APPEND ls_para TO et_para.
    ENDLOOP.
*    ENDIF.

*&---------------------------------------------------------------------*
*&  Expoting parameter data (only after rfc call)
*&---------------------------------------------------------------------*
    IF iv_indx = '02'.
      LOOP AT  lt_export ASSIGNING <ls_para>.
        ls_para-para_name = <ls_para>-parameter.
        ls_para-para_value = <ls_para>-parameter.
        APPEND ls_para TO et_para.
      ENDLOOP.
    ENDIF.

*&---------------------------------------------------------------------*
*&  Changing parameter data
*&---------------------------------------------------------------------*
    LOOP AT  lt_change ASSIGNING <ls_para>.
      ls_para-para_name = <ls_para>-parameter.
      ls_para-para_value = <ls_para>-parameter.
      APPEND ls_para TO et_para.
    ENDLOOP.

*&---------------------------------------------------------------------*
*&   Tables parameter data
*&---------------------------------------------------------------------*
    LOOP AT  lt_tables ASSIGNING <ls_para>.
      ls_para-para_name = <ls_para>-parameter.
      ls_para-para_value = <ls_para>-parameter.
      APPEND ls_para TO et_para.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_log_data.
    DATA: lr_area  TYPE RANGE OF zlogdata-area,
          lr_name  TYPE RANGE OF zlogdata-name,
          lr_erdat TYPE RANGE OF zlogdata-erdat,
          lr_zeit  TYPE RANGE OF zlogdata-zeit,
          lr_ernam TYPE RANGE OF zlogdata-ernam,
          lr_sysid TYPE  RANGE OF zlogdata-ext_sysid,
          lr_seqno TYPE RANGE OF zlogdata-ext_seqno,
          lv_error TYPE          char1,
          lv_archi TYPE          char1.

    LOOP AT gt_parameters ASSIGNING FIELD-SYMBOL(<ls_parameter>).
      CASE <ls_parameter>-attribute_name.
        WHEN 'S_AREA'.
          APPEND INITIAL LINE TO lr_area ASSIGNING FIELD-SYMBOL(<ls_area>).
          MOVE-CORRESPONDING <ls_parameter> TO <ls_area>.
        WHEN 'S_NAME'.
          APPEND INITIAL LINE TO lr_name ASSIGNING FIELD-SYMBOL(<ls_name>).
          MOVE-CORRESPONDING <ls_parameter> TO <ls_name>.
        WHEN 'S_ERDAT'.
          APPEND INITIAL LINE TO lr_erdat ASSIGNING FIELD-SYMBOL(<ls_erdat>).
          MOVE-CORRESPONDING <ls_parameter> TO <ls_erdat>.
        WHEN 'S_ZEIT'.
          APPEND INITIAL LINE TO lr_zeit ASSIGNING FIELD-SYMBOL(<ls_zeit>).
          MOVE-CORRESPONDING <ls_parameter> TO <ls_zeit>.
        WHEN 'S_ERNAM'.
          APPEND INITIAL LINE TO lr_ernam ASSIGNING FIELD-SYMBOL(<ls_ernam>).
          MOVE-CORRESPONDING <ls_parameter> TO <ls_ernam>.
        WHEN 'S_SYSID'.
          APPEND INITIAL LINE TO lr_sysid ASSIGNING FIELD-SYMBOL(<ls_sysid>).
          MOVE-CORRESPONDING <ls_parameter> TO <ls_sysid>.
        WHEN 'S_SEQNO'.
          APPEND INITIAL LINE TO lr_seqno ASSIGNING FIELD-SYMBOL(<ls_seqno>).
          MOVE-CORRESPONDING <ls_parameter> TO <ls_seqno>.
        WHEN 'C_ERROR'.
          lv_error = <ls_parameter>-low.
        WHEN 'C_ARCHI'.
          lv_archi = <ls_parameter>-low.
      ENDCASE.
    ENDLOOP.

**Get log data
    IF lv_error = abap_true.
      SELECT zlogdata~*
       FROM (me->mv_log_table)
       INTO CORRESPONDING FIELDS OF TABLE @gt_data
       WHERE name  IN @lr_name
        AND erdat IN @lr_erdat
        AND zeit  IN @lr_zeit
        AND area  IN @lr_area
        AND ernam IN @lr_ernam
        AND ext_sysid IN @lr_sysid
        AND ext_seqno IN @lr_seqno
        AND rtype = 'E'.
    ELSE.
      SELECT zlogdata~*
       FROM (me->mv_log_table)
       INTO CORRESPONDING FIELDS OF TABLE @gt_data
       WHERE name  IN @lr_name
        AND erdat IN @lr_erdat
        AND zeit  IN @lr_zeit
        AND area  IN @lr_area
        AND ernam IN @lr_ernam
        AND ext_sysid IN @lr_sysid
        AND ext_seqno IN @lr_seqno.
    ENDIF.

**Get archive log data
    IF lv_archi = abap_true.
      IF lv_error = abap_true.
        SELECT zlogdata_ARCHIVE~*,
               'X' AS ARCHIVE_ind
         FROM (me->mv_ARCHIVE_table)
         APPENDING CORRESPONDING FIELDS OF TABLE @gt_data
         WHERE name  IN @lr_name
          AND erdat IN @lr_erdat
          AND zeit  IN @lr_zeit
          AND area  IN @lr_area
          AND ernam IN @lr_ernam
          AND ext_sysid IN @lr_sysid
          AND ext_seqno IN @lr_seqno
          AND rtype = 'E'.
      ELSE.
        SELECT zlogdata_ARCHIVE~*,
               'X' AS ARCHIVE_ind
         FROM (me->mv_ARCHIVE_table)
         APPENDING CORRESPONDING FIELDS OF TABLE @gt_data
         WHERE name  IN @lr_name
          AND erdat IN @lr_erdat
          AND zeit  IN @lr_zeit
          AND area  IN @lr_area
          AND ernam IN @lr_ernam
          AND ext_sysid IN @lr_sysid
          AND ext_seqno IN @lr_seqno.
      ENDIF.
    ENDIF.


    SORT gt_data BY erdat zeit guid indx.
    DELETE ADJACENT DUPLICATES FROM gt_data COMPARING erdat zeit guid indx.

    LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<ls_data>).
      CASE <ls_data>-rtype.
        WHEN 'S'.
          <ls_data>-cellcolors = green.
        WHEN 'E'.
          <ls_data>-cellcolors = red.
      ENDCASE.
      CASE <ls_data>-indx.
        WHEN '01'.
          <ls_data>-save_point = '01 执行前'.
        WHEN '02'.
          <ls_data>-save_point = '02 执行后'.
      ENDCASE.
    ENDLOOP.

    rv_lines = lines( gt_data ).

  ENDMETHOD.


  METHOD GET_LOG_LINES.
    DATA: lr_area  TYPE RANGE OF zlogdata-area,
          lr_name  TYPE RANGE OF zlogdata-name,
          lr_erdat TYPE RANGE OF zlogdata-erdat,
          lr_zeit  TYPE RANGE OF zlogdata-zeit,
          lr_ernam TYPE RANGE OF zlogdata-ernam,
          lr_sysid TYPE  RANGE OF zlogdata-ext_sysid,
          lr_seqno TYPE RANGE OF zlogdata-ext_seqno,
          lv_error TYPE          char1,
          lv_archi TYPE          char1,
          lv_line1 type i,
          lv_line2 type i.

    LOOP AT gt_parameters ASSIGNING FIELD-SYMBOL(<ls_parameter>).
      CASE <ls_parameter>-attribute_name.
        WHEN 'S_AREA'.
          APPEND INITIAL LINE TO lr_area ASSIGNING FIELD-SYMBOL(<ls_area>).
          MOVE-CORRESPONDING <ls_parameter> TO <ls_area>.
        WHEN 'S_NAME'.
          APPEND INITIAL LINE TO lr_name ASSIGNING FIELD-SYMBOL(<ls_name>).
          MOVE-CORRESPONDING <ls_parameter> TO <ls_name>.
        WHEN 'S_ERDAT'.
          APPEND INITIAL LINE TO lr_erdat ASSIGNING FIELD-SYMBOL(<ls_erdat>).
          MOVE-CORRESPONDING <ls_parameter> TO <ls_erdat>.
        WHEN 'S_ZEIT'.
          APPEND INITIAL LINE TO lr_zeit ASSIGNING FIELD-SYMBOL(<ls_zeit>).
          MOVE-CORRESPONDING <ls_parameter> TO <ls_zeit>.
        WHEN 'S_ERNAM'.
          APPEND INITIAL LINE TO lr_ernam ASSIGNING FIELD-SYMBOL(<ls_ernam>).
          MOVE-CORRESPONDING <ls_parameter> TO <ls_ernam>.
        WHEN 'S_SYSID'.
          APPEND INITIAL LINE TO lr_sysid ASSIGNING FIELD-SYMBOL(<ls_sysid>).
          MOVE-CORRESPONDING <ls_parameter> TO <ls_sysid>.
        WHEN 'S_SEQNO'.
          APPEND INITIAL LINE TO lr_seqno ASSIGNING FIELD-SYMBOL(<ls_seqno>).
          MOVE-CORRESPONDING <ls_parameter> TO <ls_seqno>.
        WHEN 'C_ERROR'.
          lv_error = <ls_parameter>-low.
        WHEN 'C_ARCHI'.
          lv_archi = <ls_parameter>-low.
      ENDCASE.
    ENDLOOP.

**Get log data lines
    IF lv_error = abap_true.
      SELECT COUNT(*)
       FROM (me->mv_log_table)
       INTO @LV_LINE1
       WHERE name  IN @lr_name
        AND erdat IN @lr_erdat
        AND zeit  IN @lr_zeit
        AND area  IN @lr_area
        AND ernam IN @lr_ernam
        AND ext_sysid IN @lr_sysid
        AND ext_seqno IN @lr_seqno
        AND rtype = 'E'.
    ELSE.
      SELECT COUNT(*)
       FROM (me->mv_log_table)
       INTO @LV_LINE1
       WHERE name  IN @lr_name
        AND erdat IN @lr_erdat
        AND zeit  IN @lr_zeit
        AND area  IN @lr_area
        AND ernam IN @lr_ernam
        AND ext_sysid IN @lr_sysid
        AND ext_seqno IN @lr_seqno.
    ENDIF.

**Get archive log data lines
    IF lv_archi = abap_true.
      IF lv_error = abap_true.
        SELECT COUNT(*)
         FROM (me->mv_ARCHIVE_table)
         into @lv_line2
         WHERE name  IN @lr_name
          AND erdat IN @lr_erdat
          AND zeit  IN @lr_zeit
          AND area  IN @lr_area
          AND ernam IN @lr_ernam
          AND ext_sysid IN @lr_sysid
          AND ext_seqno IN @lr_seqno
          AND rtype = 'E'.
      ELSE.
        SELECT COUNT(*)
         FROM (me->mv_ARCHIVE_table)
         into @lv_line2
         WHERE name  IN @lr_name
          AND erdat IN @lr_erdat
          AND zeit  IN @lr_zeit
          AND area  IN @lr_area
          AND ernam IN @lr_ernam
          AND ext_sysid IN @lr_sysid
          AND ext_seqno IN @lr_seqno.
      ENDIF.
    ENDIF.

    rv_lines = lv_line1 + lv_line2.

  ENDMETHOD.


  METHOD get_para_from_selscreen.
    "Method-Local Data Declarations:
    DATA lt_dynpro_fields TYPE STANDARD TABLE OF dynpread.
    FIELD-SYMBOLS <ls_dynpro_field> LIKE LINE OF lt_dynpro_fields.
    DATA lo_matcher TYPE REF TO cl_abap_matcher.
    DATA lt_matches TYPE match_result_tab.
    FIELD-SYMBOLS <ls_match> LIKE LINE OF lt_matches.
    FIELD-SYMBOLS <ls_submatch> TYPE submatch_result.
    DATA lt_fields TYPE string_table.
    FIELD-SYMBOLS <lv_field> LIKE LINE OF lt_fields.
    DATA lv_field_name TYPE string.
    FIELD-SYMBOLS <lv_parameter> TYPE any.
    FIELD-SYMBOLS <lt_selopt> TYPE INDEX TABLE.
    FIELD-SYMBOLS <ls_selopt> TYPE any.
    FIELD-SYMBOLS <lv_temp> TYPE any.
    FIELD-SYMBOLS <ls_parameter> LIKE LINE OF gt_parameters.

    "Initialization:
    REFRESH gt_parameters.

    "Determine the names of the selection screen parameters
    "provided on the screen:
    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = iv_program
        dynumb               = iv_screen
        translate_to_upper   = 'X'
        request              = 'A'
      TABLES
        dynpfields           = lt_dynpro_fields
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        invalid_parameter    = 7
        undefind_error       = 8
        double_conversion    = 9
        stepl_not_found      = 10
        OTHERS               = 11.

    "Scan through the field list looking for field name entries:
    LOOP AT lt_dynpro_fields ASSIGNING <ls_dynpro_field>.
      "Use regular expressions to isolate the field names:
      REFRESH lt_matches.
      lo_matcher =
        cl_abap_matcher=>create( pattern = `^%_(.+)_%_APP_%-TEXT$`
                                    text = <ls_dynpro_field>-fieldname
                             ignore_case = abap_true ).
      lt_matches = lo_matcher->find_all( ).

      READ TABLE lt_matches INDEX 1 ASSIGNING <ls_match>.
      IF sy-subrc EQ 0.
        READ TABLE <ls_match>-submatches INDEX 1 ASSIGNING <ls_submatch>.
        IF sy-subrc EQ 0.
          lv_field_name = <ls_dynpro_field>-fieldname+<ls_submatch>-offset(<ls_submatch>-length).
          APPEND lv_field_name TO lt_fields.
        ENDIF.
      ENDIF.
    ENDLOOP.

    "Now, process through each of the found selection parameters
    "andy copy the values into the selection table:
    LOOP AT lt_fields ASSIGNING <lv_field>.
      lv_field_name = |{ <lv_field> }-LOW|.
      READ TABLE lt_dynpro_fields TRANSPORTING NO FIELDS WITH KEY fieldname = lv_field_name.
      IF sy-subrc EQ 0.
        lv_field_name = |({ iv_program }){ <lv_field> }[]|.
        ASSIGN (lv_field_name) TO <lt_selopt>.
        IF <lt_selopt> IS ASSIGNED.
          LOOP AT <lt_selopt> ASSIGNING <ls_selopt>.
            APPEND INITIAL LINE TO gt_parameters ASSIGNING <ls_parameter>.
            <ls_parameter>-attribute_name = <lv_field>.

            ASSIGN COMPONENT 'SIGN' OF STRUCTURE <ls_selopt> TO <lv_temp>.
            IF sy-subrc EQ 0.
              <ls_parameter>-sign = <lv_temp>.
            ENDIF.

            ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_selopt> TO <lv_temp>.
            IF sy-subrc EQ 0.
              <ls_parameter>-option = <lv_temp>.
            ENDIF.

            ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_selopt> TO <lv_temp>.
            IF sy-subrc EQ 0.
              <ls_parameter>-low = <lv_temp>.
            ENDIF.

            ASSIGN COMPONENT 'HIGH' OF STRUCTURE <ls_selopt> TO <lv_temp>.
            IF sy-subrc EQ 0.
              <ls_parameter>-high = <lv_temp>.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ELSE.
        lv_field_name = |({ iv_program }){ <lv_field> }|.
        ASSIGN (lv_field_name) TO <lv_parameter>.
        IF <lv_parameter> IS ASSIGNED.
          APPEND INITIAL LINE TO gt_parameters ASSIGNING <ls_parameter>.
          <ls_parameter>-attribute_name = <lv_field>.
          <ls_parameter>-sign = 'I'.
          <ls_parameter>-option = 'EQ'.
          <ls_parameter>-low = <lv_parameter>.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_selected_row.

    go_alv->get_selected_rows(
      IMPORTING et_index_rows = DATA(lt_rows) ).

    IF lines( lt_rows ) = 1.
      READ TABLE lt_rows ASSIGNING FIELD-SYMBOL(<ls_row>) INDEX 1.
      IF sy-subrc = 0.
        rv_row_index = <ls_row>-index.
      ENDIF.
    ELSE.
      rv_row_index = 0.
    ENDIF.


  ENDMETHOD.


  METHOD handle_double_click.
    DATA: lt_import TYPE TABLE OF rsimp,
          lt_change TYPE TABLE OF rscha,
          lt_export TYPE TABLE OF rsexp,
          lt_tables TYPE TABLE OF rstbl,
          ls_log    TYPE  TY_DATA.

    READ TABLE gt_data ASSIGNING FIELD-SYMBOL(<ls_data>) INDEX e_row-index.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING <ls_data> TO ls_log.
    ELSE.
      RETURN.
    ENDIF.

    go_output = cl_demo_output=>new( ).

    zcl_rfclog=>describe_interface(
      EXPORTING iv_func_name = ls_log-name
      IMPORTING et_import = lt_import
                et_export = lt_export
                et_change = lt_change
                et_tables = lt_tables
                ).

    " 解析 IMPORTING 参数
    LOOP AT lt_import ASSIGNING FIELD-SYMBOL(<ls_import>).


      zcl_rfclog=>describe_para_detail(
        EXPORTING is_log = ls_log
                  iv_parameter = <ls_import>-parameter
                  iv_type = <ls_import>-typ
                  iv_like = <ls_import>-dbfield
                  iv_kind = 'I'
        CHANGING co_output = go_output ).



    ENDLOOP.

    " 解析 EXPORTING 参数
    IF ls_log-indx <> '01'.
      LOOP AT lt_export ASSIGNING FIELD-SYMBOL(<ls_export>).
        zcl_rfclog=>describe_para_detail(
          EXPORTING is_log = ls_log
                    iv_parameter = <ls_export>-parameter
                    iv_type = <ls_export>-typ
                    iv_like = <ls_export>-dbfield
                    iv_kind = 'I'
          CHANGING co_output = go_output ).

      ENDLOOP.
    ENDIF.

    " 解析 CHANGING 参数
    LOOP AT lt_change ASSIGNING FIELD-SYMBOL(<ls_change>).
      zcl_rfclog=>describe_para_detail(
       EXPORTING is_log = ls_log
                 iv_parameter = <ls_change>-parameter
                 iv_type = <ls_change>-typ
                 iv_like = <ls_change>-dbfield
                 iv_kind = 'I'
       CHANGING co_output = go_output ).

    ENDLOOP.


    " 解析 TABLES 参数
    LOOP AT lt_tables ASSIGNING FIELD-SYMBOL(<ls_tables>).
      zcl_rfclog=>describe_para_detail(
        EXPORTING is_log = ls_log
                  iv_parameter = <ls_tables>-parameter
                  iv_type = <ls_tables>-typ
                  iv_like = <ls_tables>-dbstruct
                  iv_kind = 'T'
        CHANGING co_output = go_output ).

    ENDLOOP.

    go_output->display( ).
  ENDMETHOD.


  METHOD HANDLE_TOOLBAR.

*    FIELD-SYMBOLS: <ls_toolbar> TYPE stb_button.
*
*    APPEND INITIAL LINE  TO e_object->mt_toolbar ASSIGNING <ls_toolbar>.
*    <ls_toolbar>-butn_type = 3.
*
*    APPEND INITIAL LINE  TO e_object->mt_toolbar ASSIGNING <ls_toolbar>.
*    <ls_toolbar>-function = 'ZZZZ'.
**    <ls_toolbar>-icon = icon_xls.
*    <ls_toolbar>-quickinfo = |当前显示&条日志|.
*    <ls_toolbar>-text = |当前显示&条日志|.

  ENDMETHOD.


  METHOD main.
    SUBMIT zrfclog VIA SELECTION-SCREEN.

**&---------------------------------------------------------------------*
**& Report ZRFCLOG
**&---------------------------------------------------------------------*
**& 程序描述：显示RFC日志
**& 创建日期：2017.08
**& 作者： HUANGHANWEN
**&---------------------------------------------------------------------*
*REPORT zrfclog NO STANDARD PAGE HEADING.
*TABLES: zlogdata.
*DATA: gv_log_lines TYPE i.
*
**&=====================================================================*
**&      初始化
**&=====================================================================*
*INITIALIZATION.
*  DATA(go_controller) = NEW zcl_rfclog( ). "RFC LOG
*
**&=====================================================================*
**&      选择屏幕
**&=====================================================================*
*  " 选择功能
*  SELECTION-SCREEN:BEGIN OF BLOCK block_1 WITH FRAME TITLE TEXT-t01.
*  PARAMETERS:
*    p_query RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND u1,
*    p_archi RADIOBUTTON GROUP g1.
*  SELECTION-SCREEN END OF BLOCK block_1.
*
*  " 查询条件
*  SELECTION-SCREEN:BEGIN OF BLOCK block_2 WITH FRAME TITLE TEXT-t02.
*  SELECT-OPTIONS:s_area FOR zlogdata-area MODIF ID m1.
*  SELECT-OPTIONS:s_name FOR zlogdata-name MODIF ID m1.
*  SELECT-OPTIONS:s_erdat FOR zlogdata-erdat MODIF ID m1.
*  SELECT-OPTIONS:s_zeit FOR zlogdata-zeit MODIF ID m1.
*  SELECT-OPTIONS:s_ernam FOR zlogdata-ernam MODIF ID m1.
*  SELECT-OPTIONS:s_sysid FOR zlogdata-ext_sysid MODIF ID m1.
*  SELECT-OPTIONS:s_seqno FOR zlogdata-ext_seqno MODIF ID m1.
*  SELECTION-SCREEN END OF BLOCK block_2.
*
*  " 归档条件
*  SELECTION-SCREEN:BEGIN OF BLOCK block_3 WITH FRAME TITLE TEXT-t03.
*  PARAMETERS:
*    p_archi1 RADIOBUTTON GROUP g2 MODIF ID m2 USER-COMMAND u2 DEFAULT 'X',
*    p_archi2 RADIOBUTTON GROUP g2 MODIF ID m2.
*  SELECTION-SCREEN END OF BLOCK block_3.
*
*  " 附加条件
*  SELECTION-SCREEN:BEGIN OF BLOCK block_4 WITH FRAME TITLE TEXT-t04.
*  PARAMETERS:
*    c_error AS CHECKBOX  USER-COMMAND u3 MODIF ID m1,
*    c_archi AS CHECKBOX  MODIF ID m1.
*  SELECTION-SCREEN END OF BLOCK block_4.
*
**&=====================================================================*
**&      选择屏幕动态显示
**&=====================================================================*
*AT SELECTION-SCREEN OUTPUT.
*  LOOP AT SCREEN.
*    CASE 'X'.
*      WHEN p_query.
*        IF screen-group1 = 'M1'.
*          screen-active = 1.
*        ELSEIF screen-group1 = 'M2'.
*          screen-active = 0.
*        ENDIF.
*      WHEN p_archi.
*        IF screen-group1 = 'M1'.
*          screen-active = 0.
*        ELSEIF screen-group1 = 'M2'.
*          screen-active = 1.
*        ENDIF.
*    ENDCASE.
*    MODIFY SCREEN.
*  ENDLOOP.
*  IF s_erdat[] IS INITIAL.
*    APPEND INITIAL LINE TO s_erdat ASSIGNING FIELD-SYMBOL(<erdat>).
*    <erdat>-sign = 'I'.
*    <erdat>-option = 'EQ'.
*    <erdat>-low = sy-datum .
*    <erdat>-high = sy-datum.
*  ENDIF.
*
**&=====================================================================*
**&      选择屏幕事件
**&=====================================================================*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_area-low.
*  zcl_rfclog=>f4_function_group( ).
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_name-low.
*  zcl_rfclog=>f4_function_module( ).
*
*AT SELECTION-SCREEN.
*  go_controller->get_para_from_selscreen(
*    EXPORTING iv_program = sy-repid
*              iv_screen = sy-dynnr ).
*
**&=====================================================================*
**&      开始执行
**&=====================================================================*
*START-OF-SELECTION.
*  CASE 'X'.
*    WHEN p_query.
*      IF go_controller->get_log_lines( ) < 100000. " 限制查询10万条日志
*        gv_log_lines = go_controller->get_log_data( ).
*        IF gv_log_lines > 0.
*          CALL SCREEN 5000.
*        ELSE.
*          MESSAGE '没有符合条件的数据' TYPE 'S'.
*        ENDIF.
*      ELSE.
*        MESSAGE '查询结果条目过多，请限制查询条件！' TYPE 'S' DISPLAY LIKE 'E'.
*      ENDIF.
*    WHEN p_archi.
*      go_controller->archive_log_data( ).
*  ENDCASE.
*
**&=====================================================================*
**&      PBO MODULE
**&=====================================================================*
*MODULE pbo OUTPUT.
*  DATA(lv_title) = |RFC调用日志 [共{ gv_log_lines }条]|.
*  SET PF-STATUS 'STATUS'.
*  SET TITLEBAR 'TITLE' WITH lv_title.
*  go_controller->show_alv(  ).
*ENDMODULE.
*
**&=====================================================================*
**&      PAI MODULE
**&=====================================================================*
*MODULE pai INPUT.
*  CASE sy-ucomm.
*    WHEN 'EXIT' OR 'CANC'.
*      LEAVE PROGRAM.
*    WHEN 'BACK'.
*      LEAVE TO SCREEN 0.
*    WHEN 'REFRESH'.
*      go_controller->get_log_data( ).
*      go_controller->refresh_alv( ).
*    WHEN 'DETAIL'.
*      go_controller->show_log_detail( ).
*    WHEN 'SE37'.
*      go_controller->show_log_se37( ).
*    WHEN 'JSON'.
*      go_controller->show_log_json( ).
*    WHEN 'XML'.
*      go_controller->show_log_xml( ).
*  ENDCASE.
*ENDMODULE.

  ENDMETHOD.


  METHOD refresh_alv.

    go_alv->refresh_table_display( ).

  ENDMETHOD.


  METHOD set_field.

    APPEND INITIAL LINE TO gt_fieldcatalog ASSIGNING FIELD-SYMBOL(<ls_fcat>).
    <ls_fcat>-fieldname = iv_fieldname.
    <ls_fcat>-scrtext_l = iv_scrtext_l.
    <ls_fcat>-no_zero = iv_no_zero.
    <ls_fcat>-outputlen = iv_outputlen.

  ENDMETHOD.


  METHOD set_fieldcatalog.


*    DATA: lo_struct    TYPE REF TO cl_abap_structdescr,
*          lr_data_line TYPE REF TO data,
*          lt_dfies     TYPE ddfields,
*          lv_colname   TYPE char30,
*          lv_coltext   TYPE char40,
*          lt_text_tab  TYPE TABLE OF textpool.
*
*    FIELD-SYMBOLS: <lt_data>         TYPE ANY TABLE,
*                   <ls_fieldcatalog> LIKE LINE OF gt_fieldcatalog.
*    REFRESH: gt_fieldcatalog.
*
*    ASSIGN gd_data->* TO <lt_data>.
*    CREATE DATA lr_data_line LIKE LINE OF <lt_data>.
*
*    lo_struct ?= cl_abap_typedescr=>describe_by_data_ref( lr_data_line ).
*    lt_dfies = cl_salv_data_descr=>read_structdescr( lo_struct ).
*
*    MOVE-CORRESPONDING lt_dfies TO gt_fieldcatalog.
*
*    READ TEXTPOOL sy-repid INTO lt_text_tab LANGUAGE sy-langu.
*    LOOP AT lt_text_tab INTO DATA(ls_text).
*      CLEAR: lv_colname, lv_coltext.
*      SPLIT ls_text-entry AT '::' INTO lv_colname lv_coltext.
*      IF lv_colname <> ''.
*        READ TABLE gt_fieldcatalog ASSIGNING <ls_fieldcatalog>
*          WITH KEY fieldname = lv_colname.
*        IF sy-subrc = 0.
*          <ls_fieldcatalog>-scrtext_s = lv_coltext.
*          <ls_fieldcatalog>-scrtext_m = lv_coltext.
*          <ls_fieldcatalog>-scrtext_l = lv_coltext.
*          <ls_fieldcatalog>-coltext = lv_coltext.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.

    set_field( iv_fieldname = 'AREA'
               iv_scrtext_l =  '函数组').
    set_field( iv_fieldname = 'NAME'
               iv_scrtext_l =  '函数名称').
    set_field( iv_fieldname = 'ERDAT'
               iv_scrtext_l =  '日期' ).
    set_field( iv_fieldname = 'ZEIT'
               iv_scrtext_l =  '时间' ).
    set_field( iv_fieldname = 'ERNAM'
               iv_scrtext_l =  '用户名' ).
    set_field( iv_fieldname = 'EXT_SYSID'
           iv_scrtext_l =  '外部系统标识' ).
    set_field( iv_fieldname = 'EXT_SEQNO'
       iv_scrtext_l =  '外部流水号' ).
    set_field( iv_fieldname = 'EXT_HOST'
   iv_scrtext_l =  '外部主机名' ).
    set_field( iv_fieldname = 'EXT_IP'
iv_scrtext_l =  '外部IP地址' ).
    set_field( iv_fieldname = 'SAVE_POINT'
               iv_scrtext_l =  '保存点').
    set_field( iv_fieldname = 'RUNTIME'
               iv_scrtext_l =  '运行时间(秒)'
               iv_no_zero = abap_true ).
    set_field( iv_fieldname = 'RTYPE'
               iv_scrtext_l =  '消息类型').
    set_field( iv_fieldname = 'RTMSG'
               iv_scrtext_l =  '消息内容' ).


  ENDMETHOD.


  METHOD set_layout.
*    DATA(LV_LINES) = LINES( GT_DATA ).

    CLEAR gs_layout.
    gs_layout-cwidth_opt = 'X'.
    gs_layout-sel_mode = 'A'.
*    gs_layout-col_opt = 'X'.
*    gs_layout-no_toolbar = 'X'.
    gs_layout-ctab_fname = 'CELLCOLORS'.
    gs_layout-edit = ''.
*    gs_layout-GRID_TITLE = |共 { LV_LINES } 条日志|.
    gs_layout-SMALLTITLE = 'X'.


    CLEAR gs_variant.
    gs_variant-report = sy-repid.



  ENDMETHOD.


  METHOD show_alv.


    IF go_alv IS INITIAL.

      IF go_container IS INITIAL.
        go_container = NEW cl_gui_custom_container( container_name = 'CONTAINER' ).
      ENDIF.

      IF go_alv IS INITIAL.
        go_alv = NEW cl_gui_alv_grid( i_parent = go_container ).
        SET HANDLER me->handle_double_click FOR go_alv.
        set HANDLER me->handle_toolbar for go_alv.
      ENDIF.

      me->set_fieldcatalog(  ).

      me->set_layout(  ).

      go_alv->set_table_for_first_display(
        EXPORTING
          is_variant = gs_variant
          i_save = 'U'
          is_layout = gs_layout
        CHANGING
          it_fieldcatalog = gt_fieldcatalog
          it_outtab = gt_data
           ).

    ELSE.

      refresh_alv( ).

    ENDIF.

  ENDMETHOD.


  METHOD show_log_detail.
    DATA: lt_import TYPE TABLE OF rsimp,
          lt_change TYPE TABLE OF rscha,
          lt_export TYPE TABLE OF rsexp,
          lt_tables TYPE TABLE OF rstbl,
          ls_log    TYPE  ty_data.


    go_alv->get_selected_rows(
      IMPORTING et_index_rows = DATA(lt_rows) ).


    IF lines( lt_rows ) = 1.
      READ TABLE lt_rows ASSIGNING FIELD-SYMBOL(<ls_row>) INDEX 1.
      READ TABLE gt_data ASSIGNING FIELD-SYMBOL(<ls_data>) INDEX <ls_row>-index.
      MOVE-CORRESPONDING <ls_data> TO ls_log.
      go_output = cl_demo_output=>new( ).
    ELSE.
      MESSAGE '请选择一行数据' TYPE 'S'.
      RETURN.
    ENDIF.


    zcl_rfclog=>describe_interface(
      EXPORTING iv_func_name = ls_log-name
      IMPORTING et_import = lt_import
                et_export = lt_export
                et_change = lt_change
                et_tables = lt_tables
                ).

    " 解析 IMPORTING 参数
    LOOP AT lt_import ASSIGNING FIELD-SYMBOL(<ls_import>).


      zcl_rfclog=>describe_para_detail(
        EXPORTING is_log = ls_log
                  iv_parameter = <ls_import>-parameter
                  iv_type = <ls_import>-typ
                  iv_like = <ls_import>-dbfield
                  iv_kind = 'I'
        CHANGING co_output = go_output ).



    ENDLOOP.

    " 解析 EXPORTING 参数
    IF ls_log-indx <> '01'.
      LOOP AT lt_export ASSIGNING FIELD-SYMBOL(<ls_export>).
        zcl_rfclog=>describe_para_detail(
          EXPORTING is_log = ls_log
                    iv_parameter = <ls_export>-parameter
                    iv_type = <ls_export>-typ
                    iv_like = <ls_export>-dbfield
                    iv_kind = 'I'
          CHANGING co_output = go_output ).

      ENDLOOP.
    ENDIF.

    " 解析 CHANGING 参数
    LOOP AT lt_change ASSIGNING FIELD-SYMBOL(<ls_change>).
      zcl_rfclog=>describe_para_detail(
       EXPORTING is_log = ls_log
                 iv_parameter = <ls_change>-parameter
                 iv_type = <ls_change>-typ
                 iv_like = <ls_change>-dbfield
                 iv_kind = 'I'
       CHANGING co_output = go_output ).

    ENDLOOP.


    " 解析 TABLES 参数
    LOOP AT lt_tables ASSIGNING FIELD-SYMBOL(<ls_tables>).
      zcl_rfclog=>describe_para_detail(
        EXPORTING is_log = ls_log
                  iv_parameter = <ls_tables>-parameter
                  iv_type = <ls_tables>-typ
                  iv_like = <ls_tables>-dbstruct
                  iv_kind = 'T'
        CHANGING co_output = go_output ).

    ENDLOOP.

    go_output->display( ).


  ENDMETHOD.


  METHOD show_log_json.
    DATA: lt_import TYPE TABLE OF rsimp,
          lt_change TYPE TABLE OF rscha,
          lt_export TYPE TABLE OF rsexp,
          lt_tables TYPE TABLE OF rstbl,
          ls_log    TYPE ty_data.


    go_alv->get_selected_rows(
      IMPORTING et_index_rows = DATA(lt_rows) ).


    IF lines( lt_rows ) = 1.
      READ TABLE lt_rows ASSIGNING FIELD-SYMBOL(<ls_row>) INDEX 1.
      READ TABLE gt_data ASSIGNING FIELD-SYMBOL(<ls_data>) INDEX <ls_row>-index.
      MOVE-CORRESPONDING <ls_data> TO ls_log.
*  DATA(go_xml) = CAST if_sxml_writer(
**                                cl_sxml_string_writer=>create( ) ).
*  DATA(go_json) = CAST if_sxml_writer(
*                                cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json  ) ).
    ELSE.
      MESSAGE '请选择一行数据' TYPE 'S'.
      RETURN.
    ENDIF.


    zcl_rfclog=>describe_interface(
      EXPORTING iv_func_name = ls_log-name
      IMPORTING et_import = lt_import
                et_export = lt_export
                et_change = lt_change
                et_tables = lt_tables
                ).

    go_json = CAST if_sxml_writer(
                                cl_sxml_string_writer=>create(
                                type = if_sxml=>co_xt_json  ) ).
    go_json->open_element( name = 'object' ).

    " 解析 IMPORTING 参数
    LOOP AT lt_import ASSIGNING FIELD-SYMBOL(<ls_import>).


      zcl_rfclog=>describe_para_json(
        EXPORTING is_log = ls_log
                  iv_parameter = <ls_import>-parameter
                  iv_type = <ls_import>-typ
                  iv_like = <ls_import>-dbfield
                  iv_kind = 'I'
        CHANGING co_output = go_json ).



    ENDLOOP.

    " 解析 EXPORTING 参数
    IF ls_log-indx <> '01'.
      LOOP AT lt_export ASSIGNING FIELD-SYMBOL(<ls_export>).
        zcl_rfclog=>describe_para_json(
          EXPORTING is_log = ls_log
                    iv_parameter = <ls_export>-parameter
                    iv_type = <ls_export>-typ
                    iv_like = <ls_export>-dbfield
                    iv_kind = 'I'
          CHANGING co_output = go_json ).

      ENDLOOP.
    ENDIF.

    " 解析 CHANGING 参数
    LOOP AT lt_change ASSIGNING FIELD-SYMBOL(<ls_change>).
      zcl_rfclog=>describe_para_json(
       EXPORTING is_log = ls_log
                 iv_parameter = <ls_change>-parameter
                 iv_type = <ls_change>-typ
                 iv_like = <ls_change>-dbfield
                 iv_kind = 'I'
       CHANGING co_output = go_json ).

    ENDLOOP.


    " 解析 TABLES 参数
    LOOP AT lt_tables ASSIGNING FIELD-SYMBOL(<ls_tables>).
      zcl_rfclog=>describe_para_json(
        EXPORTING is_log = ls_log
                  iv_parameter = <ls_tables>-parameter
                  iv_type = <ls_tables>-typ
                  iv_like = <ls_tables>-dbstruct
                  iv_kind = 'T'
        CHANGING co_output = go_json ).

    ENDLOOP.

    go_json->close_element( ).



    " Display final json
    DATA(json) =
      CAST cl_sxml_string_writer( go_json )->get_output(  ).
    cl_demo_output=>display_json( json ).


  ENDMETHOD.


  METHOD show_log_se37.
    DATA: lt_import TYPE TABLE OF rsimp,
          lt_change TYPE TABLE OF rscha,
          lt_export TYPE TABLE OF rsexp,
          lt_tables TYPE TABLE OF rstbl,
          ls_log    TYPE ty_data.


    go_alv->get_selected_rows(
      IMPORTING et_index_rows = DATA(lt_rows) ).


    IF lines( lt_rows ) = 1.
      READ TABLE lt_rows ASSIGNING FIELD-SYMBOL(<ls_row>) INDEX 1.
      READ TABLE gt_data ASSIGNING FIELD-SYMBOL(<ls_data>) INDEX <ls_row>-index.
      MOVE-CORRESPONDING <ls_data> TO ls_log.

    ELSE.
      MESSAGE '请选择一行数据' TYPE 'S'.
      RETURN.
    ENDIF.

    IF ls_log-indx = '02'.
      ls_log-indx = '01'.
    ENDIF.


    zcl_rfclog=>describe_interface(
      EXPORTING iv_func_name = ls_log-name
      IMPORTING et_import = lt_import
                et_export = lt_export
                et_change = lt_change
                et_tables = lt_tables
                ).

    " 解析 IMPORTING 参数
    LOOP AT lt_import ASSIGNING FIELD-SYMBOL(<ls_import>).


      zcl_rfclog=>describe_para_se37(
        EXPORTING is_log = ls_log
                  iv_parameter = <ls_import>-parameter
                  iv_type = <ls_import>-typ
                  iv_like = <ls_import>-dbfield
                  iv_kind = 'I' ).



    ENDLOOP.

    " 解析 EXPORTING 参数
    IF ls_log-indx <> '01'.
      LOOP AT lt_export ASSIGNING FIELD-SYMBOL(<ls_export>).
        zcl_rfclog=>describe_para_se37(
          EXPORTING is_log = ls_log
                    iv_parameter = <ls_export>-parameter
                    iv_type = <ls_export>-typ
                    iv_like = <ls_export>-dbfield
                    iv_kind = 'I' ).

      ENDLOOP.
    ENDIF.

    " 解析 CHANGING 参数
    LOOP AT lt_change ASSIGNING FIELD-SYMBOL(<ls_change>).
      zcl_rfclog=>describe_para_se37(
       EXPORTING is_log = ls_log
                 iv_parameter = <ls_change>-parameter
                 iv_type = <ls_change>-typ
                 iv_like = <ls_change>-dbfield
                 iv_kind = 'I' ).


    ENDLOOP.


    " 解析 TABLES 参数
    LOOP AT lt_tables ASSIGNING FIELD-SYMBOL(<ls_tables>).
      zcl_rfclog=>describe_para_se37(
        EXPORTING is_log = ls_log
                  iv_parameter = <ls_tables>-parameter
                  iv_type = <ls_tables>-typ
                  iv_like = <ls_tables>-dbstruct
                  iv_kind = 'T' ).

    ENDLOOP.

    CALL FUNCTION 'SFCS_FA_TEST_FUNCTION'
      EXPORTING
        funcname         = ls_log-name
*       SEQUENCE         =
*       RUN_TIME_ON      =
*       TRACE_SPECIAL_UNITS       =
      EXCEPTIONS
        cancelled        = 1
        generation_error = 2
        OTHERS           = 3.



  ENDMETHOD.


  METHOD show_log_xml.
    DATA: lt_import TYPE TABLE OF rsimp,
          lt_change TYPE TABLE OF rscha,
          lt_export TYPE TABLE OF rsexp,
          lt_tables TYPE TABLE OF rstbl,
          ls_log    TYPE  TY_data.


    go_alv->get_selected_rows(
      IMPORTING et_index_rows = DATA(lt_rows) ).


    IF lines( lt_rows ) = 1.
      READ TABLE lt_rows ASSIGNING FIELD-SYMBOL(<ls_row>) INDEX 1.
      READ TABLE gt_data ASSIGNING FIELD-SYMBOL(<ls_data>) INDEX <ls_row>-index.
      MOVE-CORRESPONDING <ls_data> TO ls_log.
    ELSE.
      MESSAGE '请选择一行数据' TYPE 'S'.
      RETURN.
    ENDIF.


    zcl_rfclog=>describe_interface(
      EXPORTING iv_func_name = ls_log-name
      IMPORTING et_import = lt_import
                et_export = lt_export
                et_change = lt_change
                et_tables = lt_tables
                ).

    go_xml = CAST if_sxml_writer(
                                cl_sxml_string_writer=>create(
                                 ) ).
    go_xml->open_element( name = 'root' ).

    " 解析 IMPORTING 参数
    LOOP AT lt_import ASSIGNING FIELD-SYMBOL(<ls_import>).


      zcl_rfclog=>describe_para_xml(
        EXPORTING is_log = ls_log
                  iv_parameter = <ls_import>-parameter
                  iv_type = <ls_import>-typ
                  iv_like = <ls_import>-dbfield
                  iv_kind = 'I'
        CHANGING co_output = go_xml ).



    ENDLOOP.

    " 解析 EXPORTING 参数
    IF ls_log-indx <> '01'.
      LOOP AT lt_export ASSIGNING FIELD-SYMBOL(<ls_export>).
        zcl_rfclog=>describe_para_xml(
          EXPORTING is_log = ls_log
                    iv_parameter = <ls_export>-parameter
                    iv_type = <ls_export>-typ
                    iv_like = <ls_export>-dbfield
                    iv_kind = 'I'
          CHANGING co_output = go_xml ).

      ENDLOOP.
    ENDIF.

    " 解析 CHANGING 参数
    LOOP AT lt_change ASSIGNING FIELD-SYMBOL(<ls_change>).
      zcl_rfclog=>describe_para_xml(
       EXPORTING is_log = ls_log
                 iv_parameter = <ls_change>-parameter
                 iv_type = <ls_change>-typ
                 iv_like = <ls_change>-dbfield
                 iv_kind = 'I'
       CHANGING co_output = go_xml ).

    ENDLOOP.


    " 解析 TABLES 参数
    LOOP AT lt_tables ASSIGNING FIELD-SYMBOL(<ls_tables>).
      zcl_rfclog=>describe_para_xml(
        EXPORTING is_log = ls_log
                  iv_parameter = <ls_tables>-parameter
                  iv_type = <ls_tables>-typ
                  iv_like = <ls_tables>-dbstruct
                  iv_kind = 'T'
        CHANGING co_output = go_xml ).

    ENDLOOP.

    go_xml->close_element( ).

    " Display final xml
    DATA(xml_xstring) =
      CAST cl_sxml_string_writer( go_xml )->get_output(  ).
    cl_demo_output=>display_xml( xml_xstring ).


  ENDMETHOD.
ENDCLASS.
