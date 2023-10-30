*&---------------------------------------------------------------------*
*&  Include           ZHRAUREPY_FIRECON_FILEDL02
*&---------------------------------------------------------------------*
* Report All the relevant files from the Diractory
initialization.
*

start-of-selection.
*----------------------------------------------------------------------*
* Initialize VariableS
  perform initialize_global_variables.

* Read All Files from the Data directory
  perform file_directory_read.

* Identify and Assign Sort Seqence for file process
  if gv_okfile_counter > 0.
    perform assign_sort_attributes.
  endif.

end-of-selection.
* Process Files
  if gv_okfile_counter > 0.
* Display Files available for Download
    perform display_files_for_download tables gt_audit_files.
  endif.
