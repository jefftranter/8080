/*
TWRITE.C
Altair 8800micro utility to copy files from the CP/M environment to the
Propeller Terminal SDCard.

Garry Jordan, 04/2014
*/

#include <stdio.h>
#include "tutils.h"

#define VERSION "2.0a"

/* Possible Propeller terminal errors */
#define ERRPARSE   1
#define ERRCONNECT 2
#define ERRDIR     3
#define ERRLEGACY  4
#define ERRMEM     5
#define ERRSD      0xff

#define YNA "\033[1mY\033[mes/\033[1mN\033[mo/\033[1mA\033[mll"

/*
#define DEBUG
*/

/* Globals */

/* Forward declarations */

main(argc, argv)
int argc;
char **argv;
{
	unsigned nsecs;
	int      i, tmp, nfiles, err;
	int      fd;
	FLAG     sys, rename;
	FLAGS8   gopts;
	char     *cp, fidx;
	FILESPEC fs;

	setmem(&fs, sizeof fs, 0);
	gopts = sys = rename = 0;

	for(i = 1; i < argc;)
	{
		if('-' == *argv[i])
		{
			for(tmp = 1; argv[i][tmp]; tmp++)
			{
				switch(argv[i][tmp])
				{
					case 'H':
					case '?':
						usage(TRUE);
					case 'O':
						gopts |= OVERWRT;
						break;
					case 'R':
						rename++;
						break;
					case 'S':
						sys++;
						break;
					case 'T':
						gopts |= TEXTEOF;
						break;
					default:
						printf("Unrecognized option: -%c\n", argv[i][tmp]);
						usage(FALSE);
				}
			}

			argc--;
			for (tmp = i; tmp < argc; tmp++)
				argv[tmp] = argv[tmp + 1];
		}
		else
			i++;
	}

	if(1 == argc)
		usage(FALSE);

	if(!tport_probe(&gopts))
	{
		puts("Can't connect to terminal\n");
		exit();
	}

	if(gopts & LEGACY)
	{
		puts("Terminal firmware v5.2 or newer required\n");
		exit();
	}

	/* Scan args to ensure well-formed filespecs */
	for(i = 1; i < argc; i++)
	{
		if(!fpath_ok(argv[i], &fs, FALSE))
		{
			printf("\007%s ?\n", argv[i]);
			exit();
		}
	}

	/* Now we can let wildexp do its thing on the source files */
	if(wildexp(&argc, &argv, sys))
	{
		printf("Wildcard expansion failed.\n");
		exit();
	}

	/* Validate "copy with rename" */
	if(argc != 3 && rename)
	{
		puts("Only two unabiguous filenames allowed with \"-r\" switch.\n");
		exit();
	}

	if(rename && 0 != uord_ok(argv[2]))
	{
		puts("User/drive: not allowed in rename target.\n");
		exit();
	}

	/* Process the file list */
	bdos(SETDMA, DEF_DMA);	/* Set default DMA address for xfers */

	for(nfiles = 0, i = 1; i < argc; i++)
	{
		if(ERROR == (fd = open(argv[i], READ)))
		{
			printf("%-17s - %s\n", argv[i], errmsg(errno()));
			continue;
		}

		nsecs = cfsize(fd);

		/* The 8800micro SDCard driver does not support partitions or
		   directories, so move past user/drive, if present. */
		if(rename)
		{
			printf("%-17s -> %s", argv[i], argv[i + 1]);
			i++;
		}
		else
			printf("%-17s", argv[i]);

		fidx = UORD_LEN(uord_ok(argv[i]));
		tmp = send_file(fd, &argv[i][fidx], &gopts, nsecs);
		close(fd);

		if(CTRL_Z == (tmp & 0x7f))
		{
			if(tmp & 0x80)
				puts(" - Ctrl-Z not in last sector. File may be binary.");
			else
				puts(" - (text)");
		}
		else if(tmp)
		{
			if(CAN == tmp)
				break;	/* Ctrl-C cancel from terminal is fatal */
			else if(ESC == tmp)
				puts(" - SDCard file create failed");
			else if(ERROR == tmp)
				puts(" - BDOS read error");
			else if(ERRSD == tmp)
				puts(" - SDCard write error");
			else if(NAK != tmp)
				puts("Protocol error (0x%x)", tmp);

			putchar('\n');
			continue;
		}

		putchar('\n');
		nfiles++;
	}

	printf("%d file(s) copied\n", nfiles);
	exit();
}

/*
Transfer file contents over the serial line to the Propeller terminal.
Return:
  NAK   - SDCard file exists and overwrite refused.
  CAN   - Ctrl-C was detected by the terminal during transfer.
  ERROR - BDOS error on file read.
*/
int send_file(fd, fname, popts, nsecs)
int      fd;
char     *fname;
FLAGS8   *popts;
unsigned nsecs;
{
	char   res;
	FLAGS8 opts;
	char   prompt[80];

	opts = *popts;

again:
	if(!prop_connect(PORT_WRITE, &opts))
		return ERROR;

	if(NAK == (res = send_fname(fname, opts)))
	{
		/* File exists on SDCard, so check options */
		sprintf(prompt, " - Replace (%s)? ", YNA);

		if(CTRL_C == (res = keyprompt("YNA", prompt)))
			exit();
		else if('N' == res)
			return NAK;
		else if('A' == res)
			*popts |= OVERWRT;

		opts |= OVERWRT;
		goto again;
		
	}
	else if(ESC == res)
		return res;		/* SDCard file could not be opened for write */
	else if(ACK != res)
		return ERROR;	/* Some sort of protocol problem */

	pputchar(nsecs & 0xff);	/* LSB of sector count */
	pputchar(nsecs >> 8);	/* MSB of sector count */

	return pfiletosd(DEF_DMA, fcbaddr(fd), nsecs);
}

/*
Show program help. Keep it terse if showall is FALSE.
*/
VOID usage(showall)
{
	if(showall)
	{
		puts("Altair 8800micro CP/M to SDCard file copy utility v");
		printf("%s\n", VERSION);
		puts("  Copy one or more files from the CP/M environment ");
		puts("to the Terminal SDCard.\n");
		puts("  Written by: Garry Jordan.\n\n");
	}

	puts("Usages: TWRITE [-o] [-t] [u/][d:]filespec\n");
	puts("\tTWRITE [-o] [-t] -r [u/][d:]filename newname\n");
	puts("\tTWRITE [-o] [-t] [u/][d:]filespec [u/][d:]filespec ...\n");

	if(!showall)
	{
		puts("Use \"TWRITE -h\" for full help.\n");
		exit();
	}

	puts("-h\tThis message.\n");
	puts(
"-o\tAll destination files with matching names will be overwritten\n");
	puts("\twithout presenting a confirmation prompt.\n");
/*****
	FIXME: wildexp matches SYS filespec without this switch.
	puts(
"-s\tFilespec to match has CP/M SYSTEM attribute set.\n");
*****/
	puts(
"-t\tFile(s) are expected to be ASCII TEXT with ctrl-Z as EOF character.\n");
	puts(
"-r\tCopy a single unambiguous filename the the SDCard using newname.\n");
	puts(
"u/\tUser area number (0-15). If omitted, the current user area is\n");
	puts(
"\tassumed. NOTE: if both a drive designator and a user area are\n");
	puts("\tgiven, the user area prefix MUST come first.\n");
	puts(
"d:\tDrive designator. If omitted, the currently-logged drive is used.\n");
	puts("\nExamples:\n");
	puts("TWRITE foo.com\n");
	puts(
"  Copy the file \"foo.com\" from the current user area and drive to the SDCard.\n\n");
	puts("TWRITE -r a:foo.com bar.com\n");
	puts(
"  Copy the file \"foo.com\" from the A: drive to the SDCard with the new\n");
	puts("  name \"bar.com\".\n\n");
	puts("TWRITE 3/b:*.*\n");
	puts(
"  Copy all files from user area #3 of the B: drive to the SDCard.\n\n");
	puts("TWRITE -o *.c a:t???.com 1/a:*.com abc.sub xyz.sub\n");
	puts(
"  Expand and copy files to the SDCard. All files that exist on the SDCard\n");
	puts(
"  with matching names will be overwritten without prompting.\n");

	exit();
}
iles that exist on the SDCard\n");
	puts(
"  with m