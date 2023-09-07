*-----------------------------------------------------------------------*
* Program Name  : ZHRAUREPY_PCC_JOBNM
* Title         : Launch program for SM30 VIEW on Validations Category
* Create Date   : 01.12.2021
* Release       : ECC 6.0
* Author        : 1130848
*-----------------------------------------------------------------------*
* Description   : Direct call for view maintenance and populating data
*-----------------------------------------------------------------------*
* CHANGE HISTORY
*-----------------------------------------------------------------------*
*Date       | User ID      |Description                   |Change Label *
*-----------------------------------------------------------------------*
*01-DEC-2021| 1130848      |Initial creation              |             *
*           |              |CFAK9011268                   |             *
*           |              |                              |             *
*-----------------------------------------------------------------------*
report zhraurepy_pcc_jobnm.

constants: gc_view   type vcl_name value 'ZHRPY_PCC_JOBNM',
           gc_action type c value 'U'. "Update

call function 'VIEW_MAINTENANCE_CALL'
  exporting
    action                       = gc_action
    view_name                    = gc_view
  exceptions
    client_reference             = 1
    foreign_lock                 = 2
    invalid_action               = 3
    no_clientindependent_auth    = 4
    no_database_function         = 5
    no_editor_function           = 6
    no_show_auth                 = 7
    no_tvdir_entry               = 8
    no_upd_auth                  = 9
    only_show_allowed            = 10
    system_failure               = 11
    unknown_field_in_dba_sellist = 12
    view_not_found               = 13
    others                       = 14.
