;;      JMP VECTORS FOR ROM CODE
;
;       SEE DISK ROM FOR ADDRESSES
;
;       HOSEQU MUST BE ALTERED WHEN THIS TABLE IS ALTERED.

        ORG     D.VEC

D.SYDD  DS      3               ; JMP  R.SYDD (MUST BE FIRST)
D.MOUNT DS      3               ; JMP  R.MOUNT
D.XOK   DS      3               ; JMP  R.XOK
D.ABORT DS      3               ; JMP  R.ABORT
D.XIT   DS      3               ; JMP  R.XIT
D.READ  DS      3               ; JMP  R.READ
D.READR DS      3               ; JMP  R.READR
D.WRITE DS      3               ; JMP  R.WRITE
D.CDE   DS      3               ; JMP  R.CDE
D.DTS   DS      3               ; JMP  R.DTS
D.SDT   DS      3               ; JMP  R.SDT
D.MAI   DS      3               ; JMP  R.MAI
D.MAO   DS      3               ; JMP  R.MAO
D.LPS   DS      3               ; JMP  R.LPS
D.RDB   DS      3               ; JMP  R.RDB
D.SDP   DS      3               ; JMP  R.SDP
D.STS   DS      3               ; JMP  R.STS
D.STZ   DS      3               ; JMP  R.STZ
D.UDLY  DS      3               ; JMP  R.UDLY
D.WSC   DS      3               ; JMP  R.WSC
D.WSP   DS      3               ; JMP  R.WSP
D.WNB   DS      3               ; JMP  R.WNB
D.ERRT  DS      3               ; JMP  R.ERRT
D.DLY   DS      3               ; JMP  R.DLY
