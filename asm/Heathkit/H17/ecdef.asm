;;      ERROR CODE DEFINITIONS

        ORG     0
        DS      1               ; NO ERROR #0
EC.EOF  DS      1               ; END OF FILE
EC.EOM  DS      1               ; END OF MEDIA
EC.ILC  DS      1               ; ILLEGAL SYSCALL CODE
EC.CNA  DS      1               ; CHANNEL NOT AVAILABLE
EC.DNS  DS      1               ; DEVICE NOT SUITABLE
EC.IDN  DS      1               ; ILLEGAL DEVICE NAME
EC.IFN  DS      1               ; ILLEGAL FILE NAME
EC.NRD  DS      1               ; NO ROOM FOR DEVICE DRIVER
EC.FNO  DS      1               ; CHANNEL NOT OPEN
EC.ILR  DS      1               ; ILLEGAL REQUEST
EC.FUC  DS      1               ; FILE USAGE CONFLICT
EC.FNF  DS      1               ; FILE NAME NOT FOUND
EC.UND  DS      1               ; UNKNOWN DEVICE
EC.ICN  DS      1               ; ILLEGAL CHANNEL NUMBER
EC.DIF  DS      1               ; DIRECTORY FULL
EC.IFC  DS      1               ; ILLEGAL FILE CONTENTS
EC.NEM  DS      1               ; NOT ENOUGH MEMORY
EC.RF   DS      1               ; READ FAILURE
EC.WF   DS      1               ; WRITE FAILURE
EC.WPV  DS      1               ; WRITE PROTECTION VIOLATION
EC.WP   DS      1               ; DISK WRITE PROTECTED
EC.FAP  DS      1               ; FILE ALREADY PRESENT
EC.DDA  DS      1               ; DEVICE DRIVER ABORT
EC.FL   DS      1               ; FILE LOCKED
EC.FAO  DS      1               ; FILE ALREADY OPEN
EC.IS   DS      1               ; ILLEGAL SWITCH
EC.UUN  DS      1               ; UNKNOWN UNIT NUMBER
EC.FNR  DS      1               ; FILE NAME REQUIRED
EC.DIW  DS      1               ; DEVICE IS NOT WRITABLE (OR WRITE LOCKED)
EC.UNA  DS      1               ; UNIT NOT AVAILABLE
EC.ILV  DS      1               ; ILLEGAL VALUE
EC.ILO  DS      1               ; ILLEGAL OPTION
