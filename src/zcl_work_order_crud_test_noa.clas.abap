CLASS zcl_work_order_crud_test_noa DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    DATA o_work_orders_management TYPE REF TO zcl_work_orders_management_noa.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">
      "!    Method that tests the work order creation functionality
      "! </p>
      "!
      test_create_work_order
        IMPORTING ev_out TYPE REF TO if_oo_adt_classrun_out,

      "! <p class="shorttext synchronized" lang="en">
      "!    Method that tests the work order read functionality
      "! </p>
      "!
      test_read_work_order
        IMPORTING ev_out TYPE REF TO if_oo_adt_classrun_out,
      "! <p class="shorttext synchronized" lang="en">
      "!    Method that tests the work order update functionality
      "! </p>
      "!
      test_update_work_order
        IMPORTING ev_out TYPE REF TO if_oo_adt_classrun_out,
      "! <p class="shorttext synchronized" lang="en">
      "!    Method that tests the work order delete functionality
      "! </p>
      "!
      test_delete_work_order
        IMPORTING ev_out TYPE REF TO if_oo_adt_classrun_out,

      "! <p class="shorttext synchronized" lang="en">
      "!    Method that fills in the tables zttechnician_noa and ztcustomer_noa
      "! </p>
      "!
      prepare_ddbb.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS:

      "! <p class="shorttext synchronized" lang="en">
      "!   Method that displays a message when the user tries to create a new work order
      "! </p>
      "!
      "! @parameter iv_success | <p class="shorttext synchronized" lang="en">Operation success flag</p>
      "! @parameter iv_work_order | <p class="shorttext synchronized" lang="en">Work order id</p>
      display_creation_result
        IMPORTING
          iv_success    TYPE abap_bool
          iv_work_order TYPE ztwork_order_noa
          ev_out        TYPE REF TO if_oo_adt_classrun_out,

      "! <p class="shorttext synchronized" lang="en">
      "!   Method that displays a message when the user tries to delete a work order
      "! </p>
      "!
      "! @parameter iv_success | <p class="shorttext synchronized" lang="en">Operation success flag</p>
      "! @parameter iv_work_order_id | <p class="shorttext synchronized" lang="en">Work order id</p>
      display_deletion_result
        IMPORTING
          iv_success       TYPE abap_bool
          iv_work_order_id TYPE zde_work_order_id_noa
          ev_out           TYPE REF TO if_oo_adt_classrun_out,

      "! <p class="shorttext synchronized" lang="en">
      "!    Method that displays a message when the user tries to read a work order
      "! </p>
      "!
      "! @parameter iv_success | <p class="shorttext synchronized" lang="en">Operation success flag</p>
      "! @parameter iv_work_order_id | <p class="shorttext synchronized" lang="en">Work order id</p>
      "! @parameter is_work_order | <p class="shorttext synchronized" lang="en">Work order structure</p>
      display_read_result
        IMPORTING
          iv_success       TYPE abap_bool
          iv_work_order_id TYPE zde_work_order_id_noa
          is_work_order    TYPE ztwork_order_noa
          ev_out           TYPE REF TO if_oo_adt_classrun_out,

      "! <p class="shorttext synchronized" lang="en">
      "!    Method that displays a message when the user tries to update a work order
      "! </p>
      "!
      "! @parameter iv_success | <p class="shorttext synchronized" lang="en">Operation success flag</p>
      "! @parameter is_work_order | <p class="shorttext synchronized" lang="en">Work order structure</p>
      "! @parameter ev_out | <p class="shorttext synchronized" lang="en"></p>
      display_update_result
        IMPORTING
          iv_success    TYPE abap_bool
          is_work_order TYPE ztwork_order_noa
          ev_out        TYPE REF TO if_oo_adt_classrun_out,

      "! <p class="shorttext synchronized" lang="en">
      "!    Method that displays a message depending if the users has authorization to do an operation in the database
      "! </p>
      "!
      "! @parameter iv_success | <p class="shorttext synchronized" lang="en">Operation success flag</p>
      "! @parameter iv_operation | <p class="shorttext synchronized" lang="en">Operation name</p>
      display_authorization_message
        IMPORTING
          iv_success   TYPE abap_bool
          iv_operation TYPE string
          ev_out       TYPE REF TO if_oo_adt_classrun_out.
ENDCLASS.



CLASS zcl_work_order_crud_test_noa IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    "Object creation
    o_work_orders_management = NEW zcl_work_orders_management_noa( ).

    "Prepare the database to begin the testing
    prepare_ddbb( ).

    "Test for creating a work order
    test_create_work_order( ev_out = out ).

    "Test for deleting a work order
    test_delete_work_order( ev_out = out ).

    "Test for reading a work order
    test_read_work_order( ev_out = out ).

    "Test for updating a work order
    test_update_work_order( ev_out = out ).

  ENDMETHOD.

  METHOD test_create_work_order.


    " Create a new work order
    DATA(ls_work_order_ex_1) = VALUE ztwork_order_noa(
    work_order_id  = 'WO001'
    customer_id    = 'CUST001'
    technician_id  = 'TECH001'
    creation_date  = '20240401'
    status         = 'PE'
    priority       = 'A'
    description    = 'Electrical wiring installation'
  ).

    DATA(lv_success) = o_work_orders_management->create_work_order( iv_work_order = ls_work_order_ex_1 ).

    display_creation_result(
      iv_success     = lv_success
      iv_work_order  = ls_work_order_ex_1
      ev_out         = ev_out
    ).

    " Create a new work order with the same key as the first work order that was inserted
    DATA(ls_work_order_ex_2) = VALUE ztwork_order_noa(
    work_order_id  = 'WO001'
    customer_id    = 'CUST002'
    technician_id  = 'TECH002'
    creation_date  = '20240401'
    status         = 'PE'
    priority       = 'A'
    description    = 'Extension of electrical wiring installation'
  ).

    lv_success = o_work_orders_management->create_work_order( iv_work_order = ls_work_order_ex_2 ).

    display_creation_result(
      iv_success     = lv_success
      iv_work_order  = ls_work_order_ex_2
      ev_out         = ev_out
    ).


    " Create a new work order with a customer_id that not exists in our database
    DATA(ls_work_order_ex_3) = VALUE ztwork_order_noa(
    work_order_id  = 'WO002'
    customer_id    = 'CUSTOMER'
    technician_id  = 'TECH002'
    creation_date  = '20240401'
    status         = 'PE'
    priority       = 'A'
    description    = 'Extension of electrical wiring installation'
  ).

    lv_success = o_work_orders_management->create_work_order( iv_work_order = ls_work_order_ex_3 ).

    display_creation_result(
      iv_success     = lv_success
      iv_work_order  = ls_work_order_ex_3
      ev_out         = ev_out
    ).


    " Create a new work order with a technical_id that not exists in our database
    DATA(ls_work_order_ex_4) = VALUE ztwork_order_noa(
    work_order_id  = 'WO003'
    customer_id    = 'CUST002'
    technician_id  = 'TECHNICAL'
    creation_date  = '20240401'
    status         = 'PE'
    priority       = 'A'
    description    = 'Extension of electrical wiring installation'
  ).

    lv_success = o_work_orders_management->create_work_order( iv_work_order = ls_work_order_ex_4 ).

    display_creation_result(
      iv_success     = lv_success
      iv_work_order  = ls_work_order_ex_4
      ev_out         = ev_out
    ).


    " Create a new work order
    DATA(ls_work_order_ex_5) = VALUE ztwork_order_noa(
    work_order_id  = 'WO005'
    customer_id    = 'CUST001'
    technician_id  = 'TECH001'
    creation_date  = '20240401'
    status         = 'PE'
    priority       = 'A'
    description    = 'Electrical wiring installation'
  ).

    lv_success = o_work_orders_management->create_work_order( iv_work_order = ls_work_order_ex_5 ).

    display_creation_result(
      iv_success     = lv_success
      iv_work_order  = ls_work_order_ex_5
      ev_out         = ev_out
    ).

  ENDMETHOD.

  METHOD test_delete_work_order.

    " Try to delete a work order that doesn't exists in the database
    DATA(ls_work_order_ex) = VALUE ztwork_order_noa(
    work_order_id  = 'WO003'
    customer_id    = 'CUST002'
    technician_id  = 'TECHNICAL'
    creation_date  = '20240401'
    status         = 'PE'
    priority       = 'A'
    description    = 'Extension of electrical wiring installation'
    ).

    DATA(lv_authorized) = o_work_orders_management->check_authorization( iv_activity = '06' ).

    display_authorization_message(
      iv_success = lv_authorized
      iv_operation = |Delete operation|
      ev_out     = ev_out
    ).

    IF lv_authorized EQ abap_true.
      DATA(lv_success) = o_work_orders_management->delete_work_order(
                           iv_work_order = ls_work_order_ex ).


      display_deletion_result(
        iv_success       = lv_success
        iv_work_order_id = ls_work_order_ex-work_order_id
        ev_out           = ev_out
      ).

      " Try to delete a work order that exists in the database
      DATA(ls_work_order_ex_1) = VALUE ztwork_order_noa(
      work_order_id  = 'WO001'
      customer_id    = 'CUST001'
      technician_id  = 'TECH001'
      creation_date  = '20240401'
      status         = ' '
      priority       = 'A'
      description    = 'Electrical wiring installation'
    ).

      lv_success = o_work_orders_management->delete_work_order(
                           iv_work_order = ls_work_order_ex_1 ).


      display_deletion_result(
        iv_success       = lv_success
        iv_work_order_id = ls_work_order_ex_1-work_order_id
        ev_out           = ev_out
      ).
    ENDIF.
  ENDMETHOD.

  METHOD test_read_work_order.
    DATA:lv_work_order_id TYPE ztwork_order_noa-work_order_id VALUE 'WO001',
         ls_work_order    TYPE ztwork_order_noa.

    DATA(lv_authorized) = o_work_orders_management->check_authorization( iv_activity = '03' ).

    display_authorization_message(
      iv_success = lv_authorized
      iv_operation = |Read operation|
      ev_out     = ev_out
    ).

    IF lv_authorized EQ abap_true.

      DATA(lv_success) = o_work_orders_management->read_work_order(
      EXPORTING iv_work_order_id = lv_work_order_id
      IMPORTING ev_read_work_order = ls_work_order ).

      display_read_result(
        iv_success        = lv_success
        iv_work_order_id  = lv_work_order_id
        is_work_order     = ls_work_order
        ev_out            = ev_out
      ).
    ENDIF.
  ENDMETHOD.

  METHOD test_update_work_order.

    DATA(ls_updated_order) = VALUE ztwork_order_noa(
      work_order_id  = 'WO005'
      customer_id    = 'CUST001'
      technician_id  = 'TECH005'
      creation_date  = '20240401'
      status         = ' '
      priority       = 'L'
      description    = 'Update something...'
    ).

    DATA(lv_authorized) = o_work_orders_management->check_authorization( iv_activity = '02' ).

    display_authorization_message(
      iv_success = lv_authorized
      iv_operation = |Update operation|
      ev_out     = ev_out
    ).

    IF lv_authorized EQ abap_true.
      DATA(lv_success) = o_work_orders_management->update_work_order(
                           iv_work_order = ls_updated_order ).

      display_update_result(
        iv_success    = lv_success
        is_work_order = ls_updated_order
        ev_out        = ev_out
      ).

    ENDIF.
  ENDMETHOD.

  METHOD prepare_ddbb.

    DATA lt_customers TYPE STANDARD TABLE OF ztcustomer_noa WITH EMPTY KEY.

    lt_customers = VALUE #(
      ( customer_id = 'CUST001' name = 'Juan Pérez'     address = 'Calle 1, Madrid'     phone = '600123456' )
      ( customer_id = 'CUST002' name = 'Ana Torres'     address = 'Av. Reforma, CDMX'   phone = '700234567' )
      ( customer_id = 'CUST003' name = 'Luis Gómez'     address = 'Rua das Flores, SP'  phone = '800345678' )
      ( customer_id = 'CUST004' name = 'María López'    address = 'Carrera 10, Bogotá'  phone = '900456789' )
      ( customer_id = 'CUST005' name = 'Carlos Méndez'  address = 'Av. Bolívar, Lima'   phone = '910567890' )
      ( customer_id = 'CUST006' name = 'Laura Sánchez'  address = 'Gran Vía, Madrid'    phone = '920678901' )
      ( customer_id = 'CUST007' name = 'Pedro Ortega'   address = 'Av. Mitre, Bs As'    phone = '930789012' )
      ( customer_id = 'CUST008' name = 'Lucía Ramírez'  address = 'Calle Larga, Quito'  phone = '940890123' )
      ( customer_id = 'CUST009' name = 'Marta Díaz'     address = 'Av. América, La Paz' phone = '950901234' )
      ( customer_id = 'CUST010' name = 'Jorge Silva'    address = 'Calle Real, Caracas' phone = '960012345' )
    ).

    MODIFY ztcustomer_noa FROM TABLE @lt_customers.

    DATA lt_technicians TYPE STANDARD TABLE OF zttechnician_noa WITH EMPTY KEY.

    lt_technicians = VALUE #(
      ( technician_id = 'TECH001' name = 'Andrés Soto'      specialty = 'Electricidad' )
      ( technician_id = 'TECH002' name = 'Beatriz Luján'    specialty = 'Mecánica' )
      ( technician_id = 'TECH003' name = 'Carlos Méndez'    specialty = 'HVAC' )
      ( technician_id = 'TECH004' name = 'Diana Torres'     specialty = 'Fontanería' )
      ( technician_id = 'TECH005' name = 'Eduardo Ríos'     specialty = 'Redes' )
      ( technician_id = 'TECH006' name = 'Fernanda Díaz'    specialty = 'Sistemas' )
      ( technician_id = 'TECH007' name = 'Gabriel Castillo' specialty = 'Carpintería' )
      ( technician_id = 'TECH008' name = 'Helena Cruz'      specialty = 'Soldadura' )
      ( technician_id = 'TECH009' name = 'Iván Navarro'     specialty = 'Pintura' )
      ( technician_id = 'TECH010' name = 'Juliana Ramírez'  specialty = 'Instalaciones' )
    ).

    MODIFY zttechnician_noa FROM TABLE @lt_technicians.

  ENDMETHOD.

  METHOD display_creation_result.
    IF iv_success = abap_true.
      ev_out->write( |The record with WORK ORDER ID: { iv_work_order-work_order_id } has been successfully created.| ).
    ELSE.
      ev_out->write( |The record with WORK ORDER ID: { iv_work_order-work_order_id } hasn't been successfully created.| ).
    ENDIF.
  ENDMETHOD.

  METHOD display_deletion_result.
    IF iv_success = abap_true.
      ev_out->write( |The work order { iv_work_order_id } has been deleted.| ).
    ELSE.
      ev_out->write( |The work order { iv_work_order_id } couldn't be deleted.| ).
    ENDIF.
  ENDMETHOD.

  METHOD display_read_result.
    IF iv_success = abap_true.
      ev_out->write(
        |The work order { is_work_order-work_order_id } exists in the database. | &&
        |Work order: { is_work_order-work_order_id }, Status: { is_work_order-status }, | &&
        |Customer id: { is_work_order-customer_id }, Technician id: { is_work_order-technician_id }, | &&
        |Priority: { is_work_order-priority }, Creation date: { is_work_order-creation_date }, | &&
        |Description: { is_work_order-description }|
      ).
    ELSE.
      ev_out->write( |The work order { iv_work_order_id } hasn't been found.| ).
    ENDIF.
  ENDMETHOD.

  METHOD display_update_result.
    IF iv_success = abap_true.
      ev_out->write( |The work order { is_work_order-work_order_id } has been updated successfully.| ).
    ELSE.
      ev_out->write( |The work order couldn't be updated: { is_work_order-work_order_id }.| ).
    ENDIF.
  ENDMETHOD.

  METHOD display_authorization_message.
    IF iv_success = abap_true.
      ev_out->write( |{ iv_operation } is authorized for the user { sy-uname }.| ).
    ELSE.
      ev_out->write( |{ iv_operation } is not authorized for the user { sy-uname }.| ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
