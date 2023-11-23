"Name: \PR:RPCS0000\FO:ADD_JOB_STEPS_VARIA\SE:END\EI
ENHANCEMENT 0 ZUSE_RPCS0000.

* when mulitple splits occur this custom development has been created to merge the spool files
* from the same main job into the one spool report where the spool report title matches and the
* report title has been activiated in table ZHR_RPCS0000_A
        SELECT single * FROM  ZUSE_RPCS0000_A into @data(ls_config)
               WHERE  PROGRAMM    = @xprogname.

        if sy-subrc eq 0 and ls_config-call_prog is not INITIAL.

          submit (ls_config-call_prog)  USER xuser VIA JOB xjobname NUMBER xjobnumber
                                 with p_call   = xprogname
                                 with p_date   = sy-datum
                                 WITH p_jobnam = xjobname
                                 WITH p_jobcnt = xjobnumber
                                 with p_user   = xuser
                                 and return.
        endif.
ENDENHANCEMENT.
