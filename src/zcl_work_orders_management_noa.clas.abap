CLASS zcl_work_orders_management_noa DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
    ty_work_order      TYPE ztwork_order_noa.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">
      "!    Constructor method
      "! </p>
      "!
      constructor,

      "! <p class="shorttext synchronized" lang="en">
      "!    Method that ensures that a work order can be updated only if it exists and its status is valid for modification.
      "! </p>
      "! @parameter iv_customer_id | <p class="shorttext synchronized" lang="en"> Customer id</p>
      "! @parameter iv_technician_id | <p class="shorttext synchronized" lang="en"> Technician id</p>
      "! @parameter iv_priority | <p class="shorttext synchronized" lang="en"> Priority</p>
      "! @parameter rv_valid | <p class="shorttext synchronized" lang="en">
      "! Flag that returns true or false depending on whether the validation to create a work order is correct. (TRUE -> Validation is ok / FALSE -> Validation is not ok)
      "! </p>
      validate_create_order
        IMPORTING
                  iv_customer_id   TYPE ztwork_order_noa-customer_id
                  iv_technician_id TYPE ztwork_order_noa-technician_id
                  iv_priority      TYPE ztwork_order_noa-priority
        RETURNING VALUE(rv_valid)  TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">
      "!     Method that verify that only work orders in pending status and without modification history can be deleted.
      "! </p>
      "!
      "! @parameter iv_work_order_id | <p class="shorttext synchronized" lang="en">Work order id</p>
      "! @parameter iv_status | <p class="shorttext synchronized" lang="en">Status</p>
      "! @parameter rv_valid | <p class="shorttext synchronized" lang="en">
      "! Flag that returns true or false depending on whether the validation to update a work order is correct. (TRUE -> Validation is ok / FALSE -> Validation is not ok)
      "! </p>
      validate_update_order
        IMPORTING iv_work_order_id TYPE ztwork_order_noa-work_order_id
                  iv_status        TYPE ztwork_order_noa-status
        RETURNING VALUE(rv_valid)  TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">
      "!    Method that validates that the status and priority values of an order are correct.
      "! </p>
      "!
      "! @parameter iv_work_order_id | <p class="shorttext synchronized" lang="en">Work order id</p>
      "! @parameter iv_status | <p class="shorttext synchronized" lang="en">Status</p>
      "! @parameter rv_valid | <p class="shorttext synchronized" lang="en">
      "! Flag that returns true or false depending on whether the validation to delete a work order is correct. (TRUE -> Validation is ok / FALSE -> Validation is not ok)
      "! </p>
      validate_delete_order
        IMPORTING iv_work_order_id TYPE ztwork_order_noa-work_order_id
                  iv_status        TYPE ztwork_order_noa-status
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
                                   RETURNING VALUE(rv_valid) TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">
      "!    Method that creates a new work order (to insert a record in the table ztwk_ord_ht_noa)
      "! </p>
      "!
      "! @parameter iv_work_order | <p class="shorttext synchronized" lang="en">Work order type (defined in the public section of this class)</p>
      "! @parameter rv_ok | <p class="shorttext synchronized" lang="en">
      "! Flag that returns true or false if the record could be inserted correctly in the ztwork_order_noa (TRUE -> The record has been inserted in successfully / FALSE -> The record hasn't been inserted in successfully).
      "! </p>
      create_work_order
        IMPORTING iv_work_order TYPE ty_work_order
        RETURNING VALUE(rv_ok)  TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">
      "!    Method that reads a work order (to read a record in the table ztwk_ord_ht_noa)
      "! </p>
      "!
      "! @parameter iv_work_order_id | <p class="shorttext synchronized" lang="en">Work order id</p>
      "! @parameter rv_ok | <p class="shorttext synchronized" lang="en">
      "!    Flag that returns true or false if the record could be read correctly in the ztwork_order_noa (TRUE -> The record has been read successfully / FALSE -> The record hasn't been read successfully).
      "! </p>
      read_work_order
        IMPORTING iv_work_order_id   TYPE ztwork_order_noa-work_order_id
        EXPORTING ev_read_work_order TYPE ty_work_order
        RETURNING VALUE(rv_ok)       TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">
      "!    Method that updates a work order (to update a record in the table ztwk_ord_ht_noa)
      "! </p>
      "!
      "! @parameter iv_work_order | <p class="shorttext synchronized" lang="en">Work order type (defined in the public section of this class)</p>
      "! @parameter rv_ok | <p class="shorttext synchronized" lang="en">
      "!    Flag that returns true or false if the record could be updated correctly in the ztwork_order_noa (TRUE -> The record has been updated successfully / FALSE -> The record hasn't been updated successfully).
      "! </p>
      update_work_order
        IMPORTING iv_work_order TYPE ty_work_order
        RETURNING VALUE(rv_ok)  TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">
      "!    Method that deletes a work order (to delete a record in the table ztwk_ord_ht_noa)
      "! </p>
      "!
      "! @parameter iv_work_order | <p class="shorttext synchronized" lang="en">Work order type (defined in the public section of this class)</p>
      "! @parameter rv_ok | <p class="shorttext synchronized" lang="en">
      "!    Flag that returns true or false if the record could be deleted correctly in the ztwork_order_noa (TRUE -> The record has been deleted successfully / FALSE -> The record hasn't been deleted successfully).
      "! </p>
      delete_work_order
        IMPORTING iv_work_order TYPE ty_work_order
        RETURNING VALUE(rv_ok)  TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en"></p>
      "! Method that checks if the user has authorizations to do an operation in the database
      "! @parameter iv_activity | <p class="shorttext synchronized" lang="en">Kind of operation in the database</p>
      "! @parameter rv_authorized | <p class="shorttext synchronized" lang="en">
      "!  Flag that returns true or false depending on whether the user is authorized to do the operation in database
      "! </p>
      check_authorization
        IMPORTING iv_activity          TYPE c
        RETURNING VALUE(rv_authorized) TYPE abap_bool.


  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS: c_valid_status_pending   TYPE string VALUE 'PE',
               c_valid_status_completed TYPE string VALUE 'CO',
               c_valid_priority_high    TYPE c VALUE 'A',
               c_valid_priority_low     TYPE c VALUE 'B'.

    DATA: t_cons_status   TYPE RANGE OF string,
          t_cons_priority TYPE RANGE OF c.

    METHODS:

      "! <p class="shorttext synchronized" lang="en">
      "!    Method that checks the customer exists in the table ztcustomer_noa
      "! </p>
      "!
      "! @parameter iv_customer_id | <p class="shorttext synchronized" lang="en">Customer id</p>
      "! @parameter rv_exists | <p class="shorttext synchronized" lang="en">
      "! Flag that returns true or false depending on whether or not the customer exists in the database. (TRUE -> The customer exists in the database / FALSE -> The customer doesn't exist in the database)
      "! </p>
      check_customer_exists
        IMPORTING iv_customer_id   TYPE ztwork_order_noa-customer_id
        RETURNING VALUE(rv_exists) TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">
      "!    Method that checks the technician exists in the table zttechnician_noa
      "! </p>
      "!
      "! @parameter iv_technician_id | <p class="shorttext synchronized" lang="en">Technician id</p>
      "! @parameter rv_exists | <p class="shorttext synchronized" lang="en">
      "! Flag that returns true or false depending on whether or not the technician exists in the database. (TRUE -> The technician exists in the database / FALSE -> The technician doesn't exist in the database)
      "! </p>
      check_technician_exists
        IMPORTING iv_technician_id TYPE ztwork_order_noa-technician_id
        RETURNING VALUE(rv_exists) TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">
      "! Method that checks that the work order exists in the table ztwork_order_noa
      "! </p>
      "!
      "! @parameter iv_work_order_id | <p class="shorttext synchronized" lang="en">Work order id</p>
      "! @parameter rv_exists | <p class="shorttext synchronized" lang="en">
      "! Flag that returns true or false depending on whether or not the work order exists in the database. (TRUE -> The work order exists in the database / FALSE -> The work order doesn't exist in the database)
      "! </p>
      check_order_exists
        IMPORTING iv_work_order_id TYPE ztwork_order_noa-work_order_id
        RETURNING VALUE(rv_exists) TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">
      "!    Method that checks if the work order has a record in the table ztwk_ord_ht_noa
      "! </p>
      "!
      "! @parameter iv_work_order_id | <p class="shorttext synchronized" lang="en">Work order id</p>
      "! @parameter rv_exists | <p class="shorttext synchronized" lang="en">
      "!  Flag that returns true or false depending on whether or not the work order has a record in the historical table. (TRUE -> The work order exists in the historical table / FALSE -> The work order doesn't exist in the historical table)
      "! </p>
      check_order_history
        IMPORTING iv_work_order_id TYPE ztwork_order_noa-work_order_id
        RETURNING VALUE(rv_exists) TYPE abap_bool.

ENDCLASS.



CLASS zcl_work_orders_management_noa IMPLEMENTATION.

  METHOD validate_create_order.

    rv_valid = abap_true.

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
    IF iv_priority IN t_cons_priority.
      rv_valid = abap_true.
    ELSE.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD validate_delete_order.

    " Check if the order exists
    DATA(lv_order_exists) = check_order_exists( iv_work_order_id ).
    IF lv_order_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if the order status is 'PE' (Pending)
    IF iv_status EQ c_valid_status_pending.
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
    IF iv_status NOT IN t_cons_status.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Validate the priority value
    IF iv_priority NOT IN t_cons_priority.
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
    IF iv_status IN t_cons_status.
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

  METHOD create_work_order.
    rv_ok = abap_false.


    DATA(lv_valid) = validate_create_order(
                      iv_customer_id   = iv_work_order-customer_id
                      iv_technician_id = iv_work_order-technician_id
                      iv_priority      = iv_work_order-priority ).

    IF lv_valid = abap_false.
      RETURN.
    ENDIF.

    INSERT INTO ztwork_order_noa VALUES @iv_work_order.

    IF sy-subrc = 0.
      rv_ok = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD delete_work_order.
    rv_ok = abap_false.

    DATA(lv_valid) = abap_false.

    validate_delete_order(
      EXPORTING
        iv_work_order_id = iv_work_order-work_order_id
        iv_status = iv_work_order-status
      RECEIVING
        rv_valid         = lv_valid ).

    IF lv_valid = abap_false.
      RETURN.
    ENDIF.

    DELETE FROM ztwork_order_noa
      WHERE work_order_id = @iv_work_order-work_order_id.

    IF sy-subrc = 0.
      rv_ok = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD read_work_order.

    rv_ok = abap_false.

    SELECT SINGLE *
    FROM ztwork_order_noa
    WHERE work_order_id = @iv_work_order_id
    INTO @ev_read_work_order.

    IF sy-subrc = 0.
      rv_ok = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD update_work_order.

    rv_ok = abap_false.

    DATA(lv_valid) = abap_false.
    validate_update_order(
      EXPORTING
        iv_work_order_id = iv_work_order-work_order_id
        iv_status        = iv_work_order-status
      RECEIVING
        rv_valid         = lv_valid ).

    IF lv_valid = abap_false.
      RETURN.
    ENDIF.

    UPDATE ztwork_order_noa FROM @iv_work_order.

    IF sy-subrc = 0.
      rv_ok = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD check_authorization.
    rv_authorized = abap_false.

    AUTHORITY-CHECK OBJECT 'ZAO_TWORK'
      ID 'ZAF_AUTH' FIELD iv_activity.

    IF sy-subrc = 0.
      rv_authorized = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD constructor.

    " Inicialice status constans range
    t_cons_status = VALUE #(
      ( sign = 'I' option = 'EQ' low = c_valid_status_pending )
      ( sign = 'I' option = 'EQ' low = c_valid_status_completed )
    ).

    " Inicialice priority constans range
    t_cons_priority = VALUE #(
      ( sign = 'I' option = 'EQ' low = c_valid_priority_high )
      ( sign = 'I' option = 'EQ' low = c_valid_priority_low )
    ).

  ENDMETHOD.

ENDCLASS.
