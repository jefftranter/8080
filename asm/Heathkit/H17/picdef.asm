;;      PIC FORMAT EQUIVALENCES.

        ORG     0

PIC.ID  DS      1               ; 377Q = BINARY FILE FLAG
        DS      1               ; FILE TYPE (FT.PIC)
PIC.LEN DS      2               ; LENGTH OF ENTIRE RECORD
PIC.PTR DS      2               ; INDEX OF START OF PIC TABLE

PIC.COD                         ; CODE STARTS HERE
