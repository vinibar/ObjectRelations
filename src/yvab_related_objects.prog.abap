*&---------------------------------------------------------------------*
*& Report yvab_related_objects
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report yvab_related_objects.

data g_devclas type tadir-devclass.

parameters:
  p_pgmid  type e071-pgmid default 'R3TR',
  p_object type e071-object.

select-options: s_dvclas for g_devclas
                no intervals obligatory.

selection-screen skip 1.
parameters: p_objnam type e071-obj_name.

class lcl_app definition create private.

  public section.
    class-methods:
      get_instance returning value(ro_instance) type ref to lcl_app.
    methods:
      main.

  private section.
    class-data mo_instance type ref to lcl_app.
    data mo_alv type ref to cl_salv_table.
    data mo_object_dependencies type ref to ycl_object_relations.
    methods show_list
      changing
        ct_depencies_list type ycl_object_relations=>ty_dependents
      raising
        cx_salv_msg.

endclass.

class lcl_app implementation.

  method get_instance.
    if mo_instance is not bound.
      mo_instance = new lcl_app( ).
    endif.
    ro_instance = mo_instance.
  endmethod.

  method main.

    data(lo_object_dependencies) = ycl_object_relations=>get_instance( ).
    data(lt_depencies_list) = lo_object_dependencies->run(
        iv_object_name = p_objnam && space "bypass type error
        iv_object_type = p_object && space "bypass type error
        ir_devclas = s_dvclas[]
    ).

    try.
        show_list(
              changing
                ct_depencies_list = lt_depencies_list ).
      catch cx_salv_msg.
        message 'Erro ao gerar o ALV' type 'E'.
    endtry.

  endmethod.


  method show_list.

    cl_salv_table=>factory(
      importing
        r_salv_table   = mo_alv
      changing
        t_table        = ct_depencies_list
    ).

    mo_alv->get_selections( )->set_selection_mode(
        if_salv_c_selection_mode=>row_column
    ).

    mo_alv->get_functions( )->set_default( ).

    mo_alv->get_columns( )->set_optimize( ).
    mo_alv->display( ).

  endmethod.

endclass.

start-of-selection.
  lcl_app=>get_instance( )->main( ).
