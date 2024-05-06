*&---------------------------------------------------------------------*
*& Subroutinenpool zafo_RULE_ROUTINE_TEMPLATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
PROGRAM zafo_rule_routine_template.
CLASS routine DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS routine_101 IMPORTING run     TYPE REF TO zafo_run
                                        is_rule TYPE zafo_run=>msty_rule_detail
                                        i_line  TYPE any OPTIONAL
                                        i_value TYPE any OPTIONAL
                              CHANGING  c_value TYPE any
                              RAISING   cx_static_check cx_dynamic_check.

    CLASS-METHODS routine_102 IMPORTING run     TYPE REF TO zafo_run
                                        is_rule TYPE zafo_run=>msty_rule_detail
                                        i_line  TYPE any OPTIONAL
                                        i_value TYPE any OPTIONAL
                              CHANGING  c_value TYPE any
                              RAISING   cx_static_check cx_dynamic_check.

    CLASS-METHODS routine_103 IMPORTING run     TYPE REF TO zafo_run
                                        is_rule TYPE zafo_run=>msty_rule_detail
                                        i_line  TYPE any OPTIONAL
                                        i_value TYPE any OPTIONAL
                              CHANGING  c_value TYPE any
                              RAISING   cx_static_check cx_dynamic_check.

    CLASS-METHODS routine_104 IMPORTING run     TYPE REF TO zafo_run
                                        is_rule TYPE zafo_run=>msty_rule_detail
                                        i_line  TYPE any OPTIONAL
                                        i_value TYPE any OPTIONAL
                              CHANGING  c_value TYPE any
                              RAISING   cx_static_check cx_dynamic_check.

    CLASS-METHODS routine_105 IMPORTING run     TYPE REF TO zafo_run
                                        is_rule TYPE zafo_run=>msty_rule_detail
                                        i_line  TYPE any OPTIONAL
                                        i_value TYPE any OPTIONAL
                              CHANGING  c_value TYPE any
                              RAISING   cx_static_check cx_dynamic_check.

    CLASS-METHODS routine_106 IMPORTING run     TYPE REF TO zafo_run
                                        is_rule TYPE zafo_run=>msty_rule_detail
                                        i_line  TYPE any OPTIONAL
                                        i_value TYPE any OPTIONAL
                              CHANGING  c_value TYPE any
                              RAISING   cx_static_check cx_dynamic_check.

    CLASS-METHODS routine_107 IMPORTING run     TYPE REF TO zafo_run
                                        is_rule TYPE zafo_run=>msty_rule_detail
                                        i_line  TYPE any OPTIONAL
                                        i_value TYPE any OPTIONAL
                              CHANGING  c_value TYPE any
                              RAISING   cx_static_check cx_dynamic_check.

    CLASS-METHODS routine_108 IMPORTING run     TYPE REF TO zafo_run
                                        is_rule TYPE zafo_run=>msty_rule_detail
                                        i_line  TYPE any OPTIONAL
                                        i_value TYPE any OPTIONAL
                              CHANGING  c_value TYPE any
                              RAISING   cx_static_check cx_dynamic_check.

    CLASS-METHODS routine_109 IMPORTING run     TYPE REF TO zafo_run
                                        is_rule TYPE zafo_run=>msty_rule_detail
                                        i_line  TYPE any OPTIONAL
                                        i_value TYPE any OPTIONAL
                              CHANGING  c_value TYPE any
                              RAISING   cx_static_check cx_dynamic_check.

    CLASS-METHODS routine_110 IMPORTING run     TYPE REF TO zafo_run
                                        is_rule TYPE zafo_run=>msty_rule_detail
                                        i_line  TYPE any OPTIONAL
                                        i_value TYPE any OPTIONAL
                              CHANGING  c_value TYPE any
                              RAISING   cx_static_check cx_dynamic_check.

    CLASS-METHODS routine_before_post IMPORTING run  TYPE REF TO zafo_run
                                      CHANGING  vars TYPE zafo_run=>mtty_bapi_variables
                                      RAISING   cx_static_check cx_dynamic_check.
    CLASS-METHODS routine_after_post IMPORTING run  TYPE REF TO zafo_run
                                     CHANGING  vars TYPE zafo_run=>mtty_bapi_variables
                                     RAISING   cx_static_check cx_dynamic_check.
    CLASS-METHODS get_var IMPORTING vars        TYPE zafo_run=>mtty_bapi_variables
                                    name        TYPE rs38l_par_
                          RETURNING VALUE(data) TYPE REF TO data .
ENDCLASS.

CLASS routine IMPLEMENTATION.
  METHOD routine_101.
* implementation_101
  ENDMETHOD.
  METHOD routine_102.
* implementation_102
  ENDMETHOD.
  METHOD routine_103.
* implementation_103
  ENDMETHOD.
  METHOD routine_104.
* implementation_104
  ENDMETHOD.
  METHOD routine_105.
* implementation_105
  ENDMETHOD.
  METHOD routine_106.
* implementation_106
  ENDMETHOD.
  METHOD routine_107.
* implementation_107
  ENDMETHOD.
  METHOD routine_108.
* implementation_108
  ENDMETHOD.
  METHOD routine_109.
* implementation_109
  ENDMETHOD.
  METHOD routine_110.
* implementation_110
  ENDMETHOD.
  METHOD routine_before_post.
* implementation_before_post
  ENDMETHOD.
  METHOD routine_after_post.
* implementation_after_post
  ENDMETHOD.
  METHOD get_var.

  ENDMETHOD.
ENDCLASS.
