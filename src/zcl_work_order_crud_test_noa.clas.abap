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
      test_update_work_order,
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

    IF lv_success = abap_true.
      ev_out->write( | The record with WORK ORDER ID: { ls_work_order_ex_1-work_order_id }  has been successfully created. | ).
    ELSE.
      ev_out->write( | The record with WORK ORDER ID: { ls_work_order_ex_1-work_order_id }  hasn't been successfully created. | ).
    ENDIF.

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

    IF lv_success = abap_true.
      ev_out->write( | The record with WORK ORDER ID: { ls_work_order_ex_2-work_order_id }  has been successfully created. | ).
    ELSE.
      ev_out->write( | The record with WORK ORDER ID: { ls_work_order_ex_2-work_order_id }  hasn't been successfully created. | ).
    ENDIF.

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

    IF lv_success = abap_true.
      ev_out->write( | The record with WORK ORDER ID:{ ls_work_order_ex_3-work_order_id } has been successfully created. | ).
    ELSE.
      ev_out->write( | The record with WORK ORDER ID:{ ls_work_order_ex_3-work_order_id } hasn't been successfully created. | ).
    ENDIF.

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

    IF lv_success = abap_true.
      ev_out->write( | The record with WORK ORDER ID:{ ls_work_order_ex_4-work_order_id } has been successfully created.| ).
    ELSE.
      ev_out->write( | The record with WORK ORDER ID: { ls_work_order_ex_4-work_order_id } hasn't been successfully created.| ).
    ENDIF.
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


    DATA(lv_success) = o_work_orders_management->delete_work_order(
                         iv_work_order = ls_work_order_ex ).


    IF lv_success = abap_true.
      ev_out->write( |The work order { ls_work_order_ex-work_order_id } has been deleted.| ).
    ELSE.
      ev_out->write( |The work order { ls_work_order_ex-work_order_id } couldn't be deleted| ).
    ENDIF.

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


    IF lv_success = abap_true.
      ev_out->write( |The work order { ls_work_order_ex_1-work_order_id } has been deleted.| ).
    ELSE.
      ev_out->write( |The work order { ls_work_order_ex_1-work_order_id } couldn't be deleted| ).
    ENDIF.
  ENDMETHOD.

  METHOD test_read_work_order.
    DATA:lv_work_order_id TYPE ztwork_order_noa-work_order_id VALUE 'WO001',
         lv_work_order    TYPE ztwork_order_noa.

    " Verificar si existe
    DATA(lv_success) = o_work_orders_management->read_work_order(
    EXPORTING iv_work_order_id = lv_work_order_id
    IMPORTING ev_read_work_order = lv_work_order ).

    " Mostrar el resultado
    IF lv_success = abap_true.
      ev_out->write( |The work order { lv_work_order_id } exists in the database. { lv_work_order-customer_id }| ).
    ELSE.
      ev_out->write( |The work order { lv_work_order_id } hasn't been found.| ).
    ENDIF.
  ENDMETHOD.

  METHOD test_update_work_order.

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

ENDCLASS.
