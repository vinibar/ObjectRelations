" [PT_br] Busca todos os objetos relacionados a um determinado objeto de
"maneira recursiva
" [EN_en] Get all related objects of a given object recursively.
CLASS ycl_object_relations DEFINITION PUBLIC CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES ty_dependents TYPE TABLE OF senvi WITH KEY type object.

    CLASS-METHODS:
      get_instance
        RETURNING VALUE(ro_instance) TYPE REF TO ycl_object_relations.

    METHODS:
      run
        IMPORTING
                  iv_object_type       TYPE seu_obj
                  iv_object_name       TYPE sobj_name
                  ir_devclas           TYPE stab_range_dev
        RETURNING VALUE(rt_dependents) TYPE ycl_object_relations=>ty_dependents.

  PRIVATE SECTION.
    CLASS-DATA mo_instance TYPE REF TO ycl_object_relations.
    DATA mt_processed_dependents TYPE ty_dependents.
    DATA mt_eu_types TYPE TABLE OF euobjv.
    DATA mt_dependents TYPE ycl_object_relations=>ty_dependents.
    DATA mr_dvclas TYPE STANDARD TABLE OF range_dev.

    METHODS get_dependents
      IMPORTING
                iv_object_type       TYPE seu_obj
                iv_object_name       TYPE sobj_name
      RETURNING VALUE(rt_dependents) TYPE ty_dependents.

    METHODS get_where_used_list
      IMPORTING
        iv_object_type     TYPE seu_obj
        iv_object_name     TYPE sobj_name
      CHANGING
        ct_environment_tab TYPE ycl_object_relations=>ty_dependents.

ENDCLASS.

CLASS ycl_object_relations IMPLEMENTATION.

  METHOD get_instance.

    IF ycl_object_relations=>mo_instance IS NOT BOUND.
      ycl_object_relations=>mo_instance = NEW ycl_object_relations( ).
    ENDIF.

    ro_instance = ycl_object_relations=>mo_instance.

  ENDMETHOD.

  METHOD run.
    mr_dvclas = ir_devclas.

    SELECT * FROM euobjv
    INTO TABLE @mt_eu_types.

    rt_dependents = me->get_dependents(
        iv_object_type = iv_object_type
        iv_object_name = iv_object_name
    ).

    SORT rt_dependents.
    DELETE ADJACENT DUPLICATES FROM rt_dependents.

  ENDMETHOD.

  METHOD get_dependents.

    DATA lt_dependents TYPE me->ty_dependents.

    " Get objects used by the given object
    CALL FUNCTION 'REPOSITORY_ENVIRONMENT_RFC'
      EXPORTING
        obj_type        = iv_object_type
        object_name     = iv_object_name
      TABLES
        environment_tab = lt_dependents.

    me->get_where_used_list(
    EXPORTING
      iv_object_type = iv_object_type
      iv_object_name = iv_object_name
      CHANGING
      ct_environment_tab = lt_dependents ).

    IF lt_dependents IS NOT INITIAL.
      SELECT * FROM tadir
          INTO TABLE @DATA(lt_tadir)
              FOR ALL ENTRIES IN @lt_dependents
                  WHERE object = @lt_dependents-type(4)
                    AND obj_name = @lt_dependents-object(40)
                    AND devclass IN @mr_dvclas
                    ORDER BY PRIMARY KEY.
    ENDIF.

    LOOP AT lt_dependents ASSIGNING FIELD-SYMBOL(<et>).

      READ TABLE lt_tadir ASSIGNING FIELD-SYMBOL(<tadir>)
             WITH KEY object = <et>-type
                      obj_name = <et>-object
                        BINARY SEARCH.

      IF sy-subrc <> 0.
        " Only TADIR objects are used
        DELETE lt_dependents
            WHERE type = <et>-type
              AND object = <et>-object.
        CONTINUE.
      ENDIF.

      READ TABLE me->mt_processed_dependents TRANSPORTING NO FIELDS
        WITH KEY type = <et>-type
                 object = <et>-object.

      IF sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.

      APPEND <et> TO mt_processed_dependents.

      DATA(lt_recursive_dependents) = me->get_dependents(
          iv_object_type = <et>-type
          iv_object_name = <et>-object && space "bypass type error
      ).

      APPEND LINES OF lt_recursive_dependents TO lt_dependents.

    ENDLOOP.

    rt_dependents = lt_dependents.

  ENDMETHOD.


  METHOD get_where_used_list.

    DATA:
      lv_object_name  TYPE e071-obj_name,
      lv_eu_objtype   TYPE rseuap-obj_type,
      lt_inputs       TYPE TABLE OF string,
      lt_founds       TYPE TABLE OF rsfindlst,
      lt_object_types TYPE TABLE OF string.

    DATA: lv_fugr_group TYPE rs38l_area,
          lv_class_name TYPE seoclsname,
          lv_menu_name  TYPE char30,
          lv_type_name  TYPE char30.

    DATA lv_obj_name TYPE e071-obj_name.

    SELECT SINGLE * FROM tadir
    INTO @DATA(lt_tadir)
    WHERE object = @iv_object_type
      AND obj_name = @iv_object_name.

    IF sy-subrc IS NOT INITIAL.
*      RETURN.
    ENDIF.

    lv_obj_name = lt_tadir-obj_name.
    CALL FUNCTION 'TR_TRANSFORM_TADIR_TO_EU_TYPE'
      EXPORTING
        iv_pgmid           = lt_tadir-pgmid
        iv_object          = lt_tadir-object
        iv_obj_name        = lv_obj_name
      IMPORTING
        ev_obj_name        = lv_object_name
        ev_obj_type        = lv_eu_objtype
      EXCEPTIONS
        no_cross_reference = 01.

    APPEND lv_obj_name TO lt_inputs.

    CALL FUNCTION 'RS_EU_CROSSREF'
      EXPORTING
        i_find_obj_cls         = lv_eu_objtype
        rekursiv               = abap_true
        no_dialog              = abap_true
        with_generated_objects = abap_true
      TABLES
        i_findstrings          = lt_inputs
        o_founds               = lt_founds
        i_scope_object_cls     = lt_object_types
        i_scope_devclass       = mr_dvclas
      EXCEPTIONS
        OTHERS                 = 9.


    LOOP AT lt_founds ASSIGNING FIELD-SYMBOL(<found>).

      READ TABLE me->mt_eu_types ASSIGNING FIELD-SYMBOL(<eu_type>)
          WITH KEY id = <found>-object_cls.

      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE ct_environment_tab ASSIGNING FIELD-SYMBOL(<environment>)
          WITH KEY type = <eu_type>-tadir
                   object = <found>-object.

      IF sy-subrc IS INITIAL.
        RETURN.
      ENDIF.

      APPEND INITIAL LINE TO ct_environment_tab ASSIGNING <environment>.
      <environment>-type = <eu_type>-tadir.
      IF <found>-encl_objec IS NOT INITIAL.
        <environment>-object = <found>-encl_objec.
      ELSE.
        <environment>-object = <found>-object.
      ENDIF.
      <environment>-call_obj = iv_object_name.
      <environment>-call_type = iv_object_type.

    ENDLOOP.

    LOOP AT ct_environment_tab ASSIGNING <environment>
      WHERE type = 'PROG'
        AND devclass IS INITIAL.

      DATA progname_with_namespace TYPE progname.
      progname_with_namespace = <environment>-object.

      CALL FUNCTION 'RS_PROGNAME_SPLIT'
        EXPORTING
          progname_with_namespace = progname_with_namespace
        IMPORTING
          fugr_group              = lv_fugr_group
          class_name              = lv_class_name
          menu_name               = lv_menu_name
          type_name               = lv_type_name
        EXCEPTIONS
          delimiter_error         = 1
          OTHERS                  = 2.

      IF lv_fugr_group IS NOT INITIAL.
        APPEND INITIAL LINE TO ct_environment_tab ASSIGNING FIELD-SYMBOL(<environment_complement>).
        <environment_complement> = <environment>.
        <environment_complement>-type = 'FUGR'.
        <environment_complement>-object = lv_fugr_group.
      ENDIF.

      IF lv_class_name IS NOT INITIAL.
        APPEND INITIAL LINE TO ct_environment_tab ASSIGNING <environment_complement>.
        <environment_complement> = <environment>.
        <environment_complement>-type = 'CLAS'.
        <environment_complement>-object = lv_class_name.
      ENDIF.

      IF lv_menu_name IS NOT INITIAL.
        " implementar
      ENDIF.

      IF lv_type_name IS NOT INITIAL.
        " implementar
      ENDIF.

    ENDLOOP.

    SELECT * FROM tadir
    INTO TABLE @DATA(tadir_table)
    FOR ALL ENTRIES IN @ct_environment_tab
    WHERE object = @ct_environment_tab-type(4)
      AND obj_name = @ct_environment_tab-object(40).

    LOOP AT ct_environment_tab ASSIGNING <environment>
        WHERE devclass IS INITIAL.

      READ TABLE tadir_table ASSIGNING FIELD-SYMBOL(<tadir>)
          WITH KEY object = <environment>-type
                   obj_name = <environment>-object.

      IF sy-subrc IS INITIAL.
        <environment>-devclass = <tadir>-devclass.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

