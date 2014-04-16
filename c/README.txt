This is a port of some C programs to the Briel Altair 8800 running
CP/M. They were modified to compile using the BDS C compiler. CP/M 3
binaries are included.

Porting notes:

Yum:

Includes changed (BDSC C has everything in stdio.h).
sizeof does not work for arrays, so used a constant.
Arrays cannot be initialized, but can use initptr() function instead.
No bool type, so use int.
Function declarations have to use old K&R format.
Text supports lowercase (original Apple 1 version had to be uppercase only).
Some lines (like help) made wider due to 80 column screen.
Variables cannot be initialized when declared.
No strtol() so use atoi().
Remove const qualifiers.
Clear screen using ANSI escape sequence.

Adventure:

Includes changed (BDSC C has everything in stdio.h).
sizeof does not work for arrays, so used a constant.
Arrays cannot be initialized, but can use initptr() function instead.
Function declarations have to use old K&R format.
Text supports lowercase (original Apple 1 version had to be uppercase only).
Some lines (like help) made wider due to 80 column screen.
Variables cannot be initialized when declared.
Remove const qualifiers.
Clear screen using ANSI escape sequence.
No support for enums so use #define instead.
Changes to some string functions.
