*-----------------------------------------------------------------------*
* Program Name  : ZHRAUREPY_PCC_TEXT
* Title         : Launch program for SM34 cluster
* Create Date   : 27.08.2021
* Release       : ECC 6.0
* Author        : 1168007
*-----------------------------------------------------------------------*
* Description   : Direct call for cluster maintenance
*-----------------------------------------------------------------------*
* CHANGE HISTORY
*-----------------------------------------------------------------------*
*Date       | User ID      |Description                   |Change Label *
*-----------------------------------------------------------------------*
*27-Aug-2021| 1168007      |Initial creation              |             *
*           |              |CFAK900624                    |             *
*           |              |                              |             *
*-----------------------------------------------------------------------*
REPORT ZHRAUREPY_PCC_TEXT.

CONSTANTS: gc_cluster TYPE VCL_NAME VALUE 'ZHRPYC_PCC_TEXT', "SM34 maintenance cluster
           gc_action  TYPE c VALUE 'U'. "Update


CALL FUNCTION 'VIEWCLUSTER_MAINTENANCE_CALL'
 EXPORTING
   viewcluster_name                   = gc_cluster
   maintenance_action                 = gc_action
 EXCEPTIONS
   CLIENT_REFERENCE                   = 1
   FOREIGN_LOCK                       = 2
   VIEWCLUSTER_NOT_FOUND              = 3
   VIEWCLUSTER_IS_INCONSISTENT        = 4
   MISSING_GENERATED_FUNCTION         = 5
   NO_UPD_AUTH                        = 6
   NO_SHOW_AUTH                       = 7
   OBJECT_NOT_FOUND                   = 8
   NO_TVDIR_ENTRY                     = 9
   NO_CLIENTINDEP_AUTH                = 10
   INVALID_ACTION                     = 11
   SAVING_CORRECTION_FAILED           = 12
   SYSTEM_FAILURE                     = 13
   UNKNOWN_FIELD_IN_DBA_SELLIST       = 14
   MISSING_CORR_NUMBER                = 15
   OTHERS                             = 16.
