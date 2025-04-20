CLASS zcl_work_orders_management_noa DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:

      "! <p class="shorttext synchronized" lang="en">
      "!    Ensures that a work order can be updated only if it exists and its status is valid for modification.
      "! </p>
      "! @parameter iv_customer_id | <p class="shorttext synchronized" lang="en"> Customer id</p>
      "! @parameter iv_technician_id | <p class="shorttext synchronized" lang="en"> Technician id</p>
      "! @parameter iv_priority | <p class="shorttext synchronized" lang="en"> Priority</p>
      "! @parameter rv_valid | <p class="shorttext synchronized" lang="en">
      "! Flag that returns true or false depending on whether the validation to create a work order is correct. (TRUE -> Validation is ok / FALSE -> Validation is not ok)
      "! </p>
      validate_create_order
        IMPORTING
                  iv_customer_id   TYPE string
                  iv_technician_id TYPE string
                  iv_priority      TYPE string
        RETURNING VALUE(rv_valid)  TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">
      "!     Verify that only work orders in pending status and without modification history can be deleted.
      "! </p>
      "!
      "! @parameter iv_work_order_id | <p class="shorttext synchronized" lang="en">Work order id</p>
      "! @parameter iv_status | <p class="shorttext synchronized" lang="en">Status</p>
      "! @parameter rv_valid | <p class="shorttext synchronized" lang="en">
      "! Flag that returns true or false depending on whether the validation to update a work order is correct. (TRUE -> Validation is ok / FALSE -> Validation is not ok)
      "! </p>
      validate_update_order
        IMPORTING iv_work_order_id TYPE string
                  iv_status        TYPE string
        RETURNING VALUE(rv_valid)  TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">
      "!    Validates that the status and priority values of an order are correct.
      "! </p>
      "!
      "! @parameter iv_work_order_id | <p class="shorttext synchronized" lang="en">Work order id</p>
      "! @parameter iv_status | <p class="shorttext synchronized" lang="en">Status</p>
      "! @parameter rv_valid | <p class="shorttext synchronized" lang="en">
      "! Flag that returns true or false depending on whether the validation to delete a work order is correct. (TRUE -> Validation is ok / FALSE -> Validation is not ok)
      "! </p>
      validate_delete_order
        IMPORTING iv_work_order_id TYPE string
                  iv_status        TYPE string
        RETURNING VALUE(rv_valid)  TYPE abap_bool,


      "! <p class="shorttext synchronized" lang="en">
      "!    Method that validates whether or not it is possible to delete a work order
      "! </p>
      "!
      "! @parameter iv_status | <p class="shorttext synchronized" lang="en">Status</p>
      "! @parameter iv_priority | <p class="shorttext synchronized" lang="en">Priority</p>
      "! @parameter rv_valid | <p class="shorttext synchronized" lang="en">
      "!    Flag that returns true or false depending on whether the validation of a work order status and priority are correct. (TRUE -> Validation is ok / FALSE -> Validation is not ok)
      "! </p>
      validate_status_and_priority IMPORTING iv_status       TYPE string
                                             iv_priority     TYPE string
                                   RETURNING VALUE(rv_valid) TYPE abap_bool.


  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS: c_valid_status_pending   TYPE c VALUE 'PE',
               c_valid_status_completed TYPE c VALUE 'CO',
               c_valid_priority_high    TYPE c VALUE 'A',
               c_valid_priority_low     TYPE c VALUE 'B'.
    METHODS:

      "! <p class="shorttext synchronized" lang="en">
      "!    Check that the customer exists in the table ztcustomer_noa
      "! </p>
      "!
      "! @parameter iv_customer_id | <p class="shorttext synchronized" lang="en">Customer id</p>
      "! @parameter rv_exists | <p class="shorttext synchronized" lang="en">
      "! Flag that returns true or false depending on whether or not the customer exists in the database. (TRUE -> The customer exists in the database / FALSE -> The customer doesn't exist in the database)
      "! </p>
      check_customer_exists
        IMPORTING iv_customer_id   TYPE string
        RETURNING VALUE(rv_exists) TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">
      "!    Check that the technician exists in the table zttechnician_noa
      "! </p>
      "!
      "! @parameter iv_technician_id | <p class="shorttext synchronized" lang="en">Technician id</p>
      "! @parameter rv_exists | <p class="shorttext synchronized" lang="en">
      "! Flag that returns true or false depending on whether or not the technician exists in the database. (TRUE -> The technician exists in the database / FALSE -> The technician doesn't exist in the database)
      "! </p>
      check_technician_exists
        IMPORTING iv_technician_id TYPE string
        RETURNING VALUE(rv_exists) TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">
      "! Check that the work order exists in the table ztwork_order_noa
      "! </p>
      "!
      "! @parameter iv_work_order_id | <p class="shorttext synchronized" lang="en">Work order id</p>
      "! @parameter rv_exists | <p class="shorttext synchronized" lang="en">
      "! Flag that returns true or false depending on whether or not the work order exists in the database. (TRUE -> The work order exists in the database / FALSE -> The work order doesn't exist in the database)
      "! </p>
      check_order_exists
        IMPORTING iv_work_order_id TYPE string
        RETURNING VALUE(rv_exists) TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">
      "!    Check if the work order has a record in the table ztwk_ord_ht_noa
      "! </p>
      "!
      "! @parameter iv_work_order_id | <p class="shorttext synchronized" lang="en">Work order id</p>
      "! @parameter rv_exists | <p class="shorttext synchronized" lang="en">
      "!  Flag that returns true or false depending on whether or not the work order has a record in the historical table. (TRUE -> The work order exists in the historical table / FALSE -> The work order doesn't exist in the historical table)
      "! </p>
      check_order_history
        IMPORTING iv_work_order_id TYPE string
        RETURNING VALUE(rv_exists) TYPE abap_bool.

ENDCLASS.



CLASS zcl_work_orders_management_noa IMPLEMENTATION.

  METHOD validate_create_order.
    " Check if customer exists
    DATA(lv_customer_exists) = check_customer_exists( iv_customer_id ).
    IF lv_customer_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if technician exists
    DATA(lv_technician_exists) = check_technician_exists( iv_technician_id ).
    IF lv_technician_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if priority is valid
    IF iv_priority NE c_valid_priority_high
    OR iv_priority NE c_valid_priority_low.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.

  METHOD validate_delete_order.

    " Check if the order exists
    DATA(lv_order_exists) = check_order_exists( iv_work_order_id ).
    IF lv_order_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if the order status is 'PE' (Pending)
    IF iv_status NE c_valid_status_pending.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if the order has a history (i.e., if it has been modified before)
    DATA(lv_has_history) = check_order_history( iv_work_order_id ).
    IF lv_has_history IS NOT INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.

  ENDMETHOD.

  METHOD validate_status_and_priority.
    " Validate the status value
    IF iv_status NE c_valid_status_completed
    OR iv_status NE c_valid_status_pending.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Validate the priority value
    IF iv_priority NE c_valid_priority_high
    OR iv_priority NE c_valid_priority_low.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.

  METHOD validate_update_order.
    " Check if the work order exists
    DATA(lv_order_exists) = check_order_exists( iv_work_order_id ).
    IF lv_order_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if the order status is editable (e.g., Pending)
    IF iv_status EQ c_valid_status_completed
    OR iv_status EQ c_valid_status_pending.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.

  ENDMETHOD.

  METHOD check_customer_exists.

    SELECT SINGLE FROM ztcustomer_noa
    FIELDS
    customer_id
    WHERE customer_id = @iv_customer_id
    INTO @DATA(lv_customer_id).

    rv_exists = COND abap_bool( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

  ENDMETHOD.

  METHOD check_order_exists.

    SELECT SINGLE FROM ztwork_order_noa
    FIELDS
    work_order_id
    WHERE work_order_id = @iv_work_order_id
    INTO @DATA(lv_work_order_id).

    rv_exists = COND abap_bool( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

  ENDMETHOD.

  METHOD check_order_history.

    SELECT SINGLE FROM ztwk_ord_ht_noa
    FIELDS
    history_id
    WHERE work_order_id = @iv_work_order_id
    INTO @DATA(lv_work_order_id).

    rv_exists = COND abap_bool( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

  ENDMETHOD.

  METHOD check_technician_exists.
    SELECT SINGLE FROM zttechnician_noa
    FIELDS
    technician_id
    WHERE technician_id = @iv_technician_id
    INTO @DATA(lv_technician_id).

    rv_exists = COND abap_bool( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).
  ENDMETHOD.

ENDCLASS.
