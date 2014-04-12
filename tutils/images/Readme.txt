BDS C development environment and sources for the AltairMicro T* utilities.

Disk A: REDCPM3.DSK
CP/M 3 system disk with the T*, CP, RM, TAIL, LS utility programs, the DRI
MAC assembler used by the BDS C CASM tool and the RED text editor that is
included in the BDS C package.

The editor is configured for the AltairMicro's 80x40 screen. I've tweaked the
RED key map to use the WordStar Ctrl-(E S D X) cursor movement keys so the
AltairMicro Terminal keyboard arrow keys can be used via the Shift-Ctrl-Alt-F6
WordStar/VT-100 cursor movement toggle. The RedKeys.txt file has a reference
list of the editor keys and commands. The RED sources and reference are included
in the BDS C package available at http://www.bdsoft.com/resources/bdsc.html.

Disk B: TUTILSRC.DSK
The T* utility sources/libraries plus a couple of programs I created to explore
the CP/M directory entry structure. Also the BDS C tools necessary to build
the T* utilities. CP/M 3 SUB files are used extensively to keep the build
process relatively simple. Most of the SUB files contain comments regarding
their use.
