# Work Order Management

## Table of Contents

- [1. General Description](#1-general-description)
- [2. Specific Requirements](#2-specific-requirements)
  - [2.1. Data Structure and Data Dictionary](#21-data-structure-and-data-dictionary)
  - [2.2. CRUD Operations (Create, Read, Update, Delete)](#22-crud-operations-create-read-update-delete)
  - [2.3. Validations and Security](#23-validations-and-security)
- [3. Database Design](#3-database-design)
  - [3.1. Database Tables](#31-database-tables)
  - [3.2. Field Definitions](#32-field-definitions)
  - [3.3. Table Relationships](#33-table-relationships)
  - [3.4. Additional Considerations](#34-additional-considerations)
- [4. Functional Validation Requirement](#4-functional-validation-requirement)
  - [4.1. Work Order Creation Validation](#41-work-order-creation-validation)
  - [4.2. Work Order Update Validation](#42-work-order-update-validation)
  - [4.3. Work Order Deletion Validation](#43-work-order-deletion-validation)
  - [4.4. Status and Priority Validation](#44-status-and-priority-validation)
- [5. Technical Details](#5-technical-details)
- [6. Test Data for Invoking CRUD Operations](#6-test-data-for-invoking-crud-operations)


---

## 1. General Description

A module will be developed in **ABAP Cloud** to manage work orders within a service company.  
This module should focus on **backend operations**, **performance optimization**, and **validations**, excluding the implementation of the **user interface**.

---

## 2. Specific Requirements

### 2.1. Data Structure and Data Dictionary

- Create and configure objects in the **Data Dictionary**:

  #### Data Types
  - Data elements and specific domains for fields such as:
    - Work order ID
    - Status
    - Priority

  #### Database Tables
  - Tables for:
    - Work orders
    - Client details
    - Assigned technicians
  - Configuration of:
    - Primary keys
    - Secondary keys
  - Relationships between tables using **foreign keys**

---

### 2.2. CRUD Operations (Create, Read, Update, Delete)

- **Create**: Program the creation of work orders with validations such as mandatory fields and correct data format.
- **Read**: Implement dynamic reads and advanced queries with filters by date, status, or client.
- **Update**: Design conditional updates for fields like status and priority, with concurrency handling (locking).
- **Delete**: Validate the deletion of work orders only under specific conditions (e.g., orders in "Pending" status).

---

### 2.3. Validations and Security

- Implement verification checks to ensure data consistency:

  - **Example**: Validate that the assigned technician exists in the employee table before creating the order.

- **Authorization handling**:
  - Use of `AUTHORITY-CHECK` to validate user permissions according to their role (view, update, delete).

---

## 3. Database Design

### 3.1. Database Tables

- **Work Orders**: `ZT_WORK_ORDER`  
- **Customers**: `ZT_CUSTOMER`  
- **Technicians**: `ZT_TECHNICIAN`  
- **Work Order History**: `ZT_WORK_ORDER_HIST`

### 3.2. Field Definitions

**ZT_WORK_ORDER (Work Orders)** – Stores the basic information about work orders.

| Field Name      | Data Type | Length | Description                                         |
|------------------|-----------|--------|-----------------------------------------------------|
| WORK_ORDER_ID    | NUMC      | 10     | Unique identifier for the work order.               |
| CUSTOMER_ID      | CHAR      | 8      | Foreign key referencing customer.                   |
| TECHNICIAN_ID    | CHAR      | 8      | Foreign key referencing technician.                 |
| CREATION_DATE    | DATS      | 8      | Creation date of the work order.                   |
| STATUS           | CHAR      | 2      | Status of the order (e.g., 'PE', 'CO').            |
| PRIORITY         | CHAR      | 1      | Priority of the work order (e.g., 'A').            |
| DESCRIPTION      | CHAR      | 50     | Detailed description of the work order.            |

---

**ZT_CUSTOMER (Customers)** – Stores information about customers.

| Field Name   | Data Type | Length | Description                      |
|--------------|-----------|--------|----------------------------------|
| CUSTOMER_ID  | NUMC      | 8      | Unique identifier for the customer. |
| NAME         | CHAR      | 50     | Customer's name.                 |
| ADDRESS      | CHAR      | 60     | Customer's address.              |
| PHONE        | CHAR      | 15     | Customer's phone number.         |

---

**ZT_TECHNICIAN (Technicians)** – Stores information about technicians.

| Field Name     | Data Type | Length   | Description                          |
|----------------|-----------|----------|--------------------------------------|
| TECHNICIAN_ID  | CHAR      | 8        | Unique identifier for the technician. |
| NAME           | STRING    | Variable | Technician's name.                   |
| SPECIALTY      | CHAR      | 20       | Area of expertise of the technician. |

**ZT_WORK_ORDER_HIST (Work Order History)** – Records the changes made to work orders.

| Field Name         | Data Type | Length | Description                                 |
|--------------------|-----------|--------|---------------------------------------------|
| HISTORY_ID         | NUMC      | 12     | Unique identifier for the history entry.    |
| WORK_ORDER_ID      | NUMC      | 10     | Foreign key referencing work order.         |
| MODIFICATION_DATE  | DATS      | 8      | Date of the modification.                   |
| CHANGE_DESCRIPTION | CHAR      | 50     | Description of the change made.             |


### 3.3. Table Relationships

**Relationship 1: ZT_WORK_ORDER → ZT_CUSTOMER**

- Type: Foreign key.
- Detail: **Foreign Key Field**: `CUSTOMER_ID` in `ZT_WORK_ORDER` references `CUSTOMER_ID` in `ZT_CUSTOMER`.
- Purpose: Links a work order to a specific customer.

**Relationship 2: ZT_WORK_ORDER → ZT_TECHNICIAN**

- Type: Foreign key.
- Detail: **Foreign Key Field**: `TECHNICIAN_ID` in `ZT_WORK_ORDER` references `TECHNICIAN_ID` in `ZT_TECHNICIAN`.
- Purpose: Links a technician to a given work order.

**Relationship 3: ZT_WORK_ORDER_HIST → ZT_WORK_ORDER**

- Type: Foreign key.
- Detail: **Foreign Key Field**: `WORK_ORDER_ID` in `ZT_WORK_ORDER_HIST` references `WORK_ORDER_ID` in `ZT_WORK_ORDER`.
- Purpose: Records the historical changes made to each work order.

### 3.4. Additional Considerations

**Consistency and Validation:**

- Use domains for fields such as `STATUS` and `PRIORITY`, defining predefined values.
- Ensure that identifiers (`CUSTOMER_ID`, `TECHNICIAN_ID`, etc.) are unique using primary keys.

## 4. Functional Validation Requirement

The implementation of validations should be done using an object-oriented ABAP class that encapsulates the logic of the following validations:

### 4.1. Work Order Creation Validation

**Functionality**: Ensure that the work order can only be created if all required fields are present and correct.

- Validate that `CUSTOMER_ID` is present and matches an existing customer in the `ZT_CUSTOMER` table.
- Validate that `TECHNICIAN_ID` is present and matches an existing technician in the `ZT_TECHNICIAN` table.
- Validate that `PRIORITY` is a valid value (e.g., `"A"` for high, `"B"` for low).

### 4.2. Work Order Update Validation

**Functionality**: Validate that updates made to a work order are correct.

- Verify that the work order exists in the database before making any changes.
- Ensure that only work orders with a status (`STATUS`) in an editable state (e.g., `"PE"` for pending) can be updated.

### 4.3. Work Order Deletion Validation

**Functionality**: Validate that work orders can only be deleted if certain conditions are met.

- Ensure that the order's status is `"PE"` (Pending) before allowing deletion.
- Verify that the order has no entries in the history table (`ZT_WORK_ORDER_HIST`) to prevent deletion of orders with registered modifications.

---

### 4.4. Status and Priority Validation

**Functionality**: Validate that the status and priority of a work order are correct.

- Verify that `STATUS` is one of the predefined valid values (e.g., `"PE"`, `"CO"`).
- Ensure that `PRIORITY` is within the valid values (e.g., `"A"` or `"B"`).

## 5. Technical Details

### Class and Methods Description

- **Method `validate_create_order`**  
  Validates that the customer, technician, and priority values are correct before allowing the creation of a work order.

- **Method `validate_update_order`**  
  Ensures that a work order can be updated only if it exists and its status is valid for modification.

- **Method `validate_delete_order`**  
  Verifies that work orders can only be deleted if they are in a "Pending" status and have no modification history.

- **Method `validate_status_and_priority`**  
  Validates that the status and priority values of a work order are correct.

#### CRUD Methods Description

- **Method `create_work_order`**:  
  Performs validation and creates a new work order if valid.

- **Method `read_work_order`**:  
  Reads the details of an existing work order.

- **Method `update_work_order`**:  
  Updates a work order, validating it before making changes.

- **Method `delete_work_order`**:  
  Deletes a work order if it meets the validation requirements.

## 6. Test Data for Invoking CRUD Operations

This class should contain test data to invoke the CRUD operations implemented in the previous class.  
Example data will be used for each operation (Create, Read, Update, Delete), ensuring that the methods execute correctly.

- **Class**: `ZCL_WORK_ORDER_CRUD_TEST`
- **Test Methods**:
  - `test_create_work_order`
  - `test_read_work_order`
  - `test_update_work_order`
  - `test_delete_work_order`

### Class and Methods Description

- **Method `test_create_work_order`**:  
  Performs a test to create a new work order.

- **Method `test_read_work_order`**:  
  Performs a test to read the details of a work order.

- **Method `test_update_work_order`**:  
  Performs a test to update a work order.

- **Method `test_delete_work_order`**:  
  Performs a test to delete a work order.



