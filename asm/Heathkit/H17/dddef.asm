;;      DEVICE DRIVER COMMUNICATION FLAGS
;
        ORG     0

DC.REA  DS      1               ; READ
DC.WRI  DS      1               ; WRITE
DC.RER  DS      1               ; READ REGARDLESS
DC.OPR  DS      1               ; OPEN FOR READ
DC.OPW  DS      1               ; OPEN FOR WRITE
DC.OPU  DS      1               ; OPEN FOR UPDATE
DC.CLU  DS      1               ; CLOSE
DC.ABT  DS      1               ; ABORT
DC.MOV  DS      1               ; MOUNT DEVICE
