;;      DIRECTORY ENTRY FORMAT.

        ORG     0

DF.EMP  EQU     377Q            ; FLAGS ENTRY EMPTY
DF.CLR  EQU     376Q            ; FLAGS ENTRY EMPTY, REST OF DIR ALSO CLEAR

DIR.NAM DS      8               ; NAME
DIR.EXT DS      3               ; EXTENSION
DIR.PRO DS      1               ; PROJECT
DIR.VER DS      1               ; VERSION
DIRIDL  EQU     $               ; FILE IDENTIFICATION LENGTH

DIR.CLU DS      1               ; CLUSTER FACTOR
DIR.FLG DS      1               ; FLAGS
        DS      1               ; RESERVED
DIR.FGN DS      1               ; FIRST GROUP NUMBER
DIR.LGN DS      1               ; LAST GROUP NUMBER
DIR.LSI DS      1               ; LAST SECTOR INDEX (IN LAST GROUP)
DIR.CRD DS      2               ; CREATION DATE
DIR.ALD DS      2               ; LAST ALTERATION DATE

DIRELEN EQU     $               ; DIRECTORY ENTRY LENGTH
