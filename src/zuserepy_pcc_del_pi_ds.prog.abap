*&---------------------------------------------------------------------*
*&  Include           PYC_SUPPORT_DEL_PI_DS
*&---------------------------------------------------------------------*
*>>> Start of MOD001--
***data: g_application type ref to lcl_application,
***      g_ok_code     type sy-ucomm.

***parameters: p_pid   type pyc_d_pyp-id obligatory, "process id
***            p_endda type endda obligatory.
*<<< End of MOD001--
*>>> Start of MOD001++
tables: pyc_d_pyp.
data: g_application type ref to lcl_application,
      g_ok_code     type sy-ucomm.
data: gt_proc_id type table of pyc_proc_id.
data: gt_proc_id_so  type /iwbep/t_cod_select_options.
data: gt_pyp_id  type /iwbep/t_cod_select_options.
* Selection Parameters
select-options: s_pid for pyc_d_pyp-id no intervals obligatory.   " Process id
parameters: p_endda type endda obligatory.          "Date
selection-screen skip 1.
parameters: p_test type xfeld default abap_true.
*<<< End of MOD001++
