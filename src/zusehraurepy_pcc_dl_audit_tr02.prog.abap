*&---------------------------------------------------------------------*
*&  Include           ZHRAUREPY_PCC_DEL_COMPL_PI02
*&---------------------------------------------------------------------*
tables: pyc_d_pyp.
data: g_application type ref to lcl_application,
      g_ok_code     type sy-ucomm.

data: gt_proc_id type table of pyc_proc_id.
data: gt_proc_id_so  type /iwbep/t_cod_select_options.
data: gt_pyp_id  type /iwbep/t_cod_select_options.
* Selection
select-options: s_pid for pyc_d_pyp-id no intervals obligatory.   " Process id
selection-screen begin of line.
selection-screen comment 1(33) text-s01 for field p_begda .
parameters: p_begda type begda obligatory.
selection-screen comment 50(5) text-s02 for field p_endda.
parameters: p_endda type endda obligatory.
selection-screen end of line.
selection-screen skip 1.
parameters: p_vari type  raldb_vari default '#PCC_DEFAULT' no-display.
