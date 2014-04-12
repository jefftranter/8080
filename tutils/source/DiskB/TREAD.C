/*
TREAD.C
Altair 8800micro utility to copy files from the CP/M environment to the
Propeller Terminal SDCard.

Garry Jordan, 04/2014
*/

#include <stdio.h>
#include "tutils.h"

#define VERSION "2.0a"

/*
#define DEBUG
*/

/* open() modes */
#define READ  0
#define WRITE 1
#define RDWR  2

/* Possible Propeller terminal errors */
#define ERRPARSE   1
#define ERRCONNECT 2
#define ERRDIR     3
#define ERRLEGACY  4
#define ERRMEM     5
#define ERRUSAGE   6

#define YNA "\033[1mY\033[mes/\033[1mN\033[mo/\033[1mA\033[mll"

/* Globals */

/* Forward declarations */

main(argc, argv)
int argc;
char **argv;
{
	int     i, tmp, err;
	int     numents, fd;
	char    c, key;
	char    *lastarg, *pdest;
	FLAGS8  gopts;
	TDIRENT *entry;
	char    destpath[FPATH_BUFF];
	char    prompt[80];

	numents = gopts = 0;

	for(i = 1; i < argc;)
	{
		if('-' == *argv[i])
		{
			switch(argv[i][1])
			{
				case 'H':
				case '?':
					usage(TRUE);
				case 'O':
					gopts |= OVERWRT;
					break;
				default:
					printf("Unrecognized option: %s\n", argv[i]);
					usage(FALSE);
			}

			argc--;
			for (tmp = i; tmp < argc; tmp++)
				argv[tmp] = argv[tmp + 1];
		}
		else
			i++;
	}

	if(2 == argc)
	{
		argv[2] = ".";
		argc++;
	}

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

	entry = texpand(&argv[1], argc - 1, &numents, &err);

	if(err)
	{
		switch(err)
		{
			case ERRPARSE:
				printf("\007%s ?\n", entry[numents].argname);
				break;
			case ERRCONNECT:
				puts("Terminal connect failed\n");
				break;
			case ERRDIR:
				puts("Terminal DIR failed\n");
				break;
			case ERRMEM:
				puts("Memory alloc failed\n");
				break;
			case ERRUSAGE:
				usage(FALSE);
			default:
				puts("Unknown error\n");
				break;
		}

		exit();
	}

	/* All ambiguous filespecs expanded, so do a final arg check */
	lastarg = entry[numents - 1].argname;
	c = lastarg[strlen(lastarg) - 1];

	if(numents < 2 || (numents > 2 &&
	   !('/' == c || ':' == c || !strcmp(lastarg, "."))))
	{
		usage(FALSE);
	}

	destpath[0] = '\0';

	if(strcmp(lastarg, "."))
		strcpy(destpath, lastarg);	/* User/drive and/or filename dest */

	pdest = destpath;

	if('/' == c || ':' == c)
	{
		while(*pdest)	/* Will append filename target to user/drive */
			pdest++;
	}

	/* All set to start the copy process */
	bdos(SETDMA, DEF_DMA);

	for(tmp = i = 0; i < numents - 1; i++)
	{
		if(!entry[i].exists)
		{
			printf("%-17s - No file\n", entry[i].argname);
			continue;
		}

		if('.' == c || '/' == c || ':' == c)
		{
			strcpy(pdest, entry[i].fname);
			printf("%-17s", destpath);
		}
		else
			printf("%-17s -> %s", entry[i].fname, lastarg);

		if(!(gopts & OVERWRT))
		{
			if(ERROR != (fd = open(destpath, READ)))
			{
				sprintf(prompt, " - File exists. Replace (%s)? ", YNA);
				key = keyprompt("YNA", prompt);

				if(CTRL_C == key)
					exit();

				if('N' == key)
				{
					/* Skip to next file */
					putchar('\n');
					continue;
				}

				/* Left with 'Y' or 'A' */
				if('A' == key)
					gopts |= OVERWRT;
			}
		}

		/* Do the copy */
		if(ERROR == (fd = creat(destpath)))
		{
			printf("%s", errmsg(errno()));
			exit();
		}

		if(!entry[i].numsecs)
			err = OK;	/* Don't ask for data if it's an empty file */
		else
			err = recv_file(fd, &entry[i], gopts);

		close(fd);

		if(err)
		{
			/* Disk full or user Ctrl-C means immediate exit */
			if(CAN != err)
				printf("%s (0x%04x)\n", errmsg(2), err);

			unlink(destpath);
			break;
		}

		putchar('\n');
		tmp++;
	}

        if (tmp == 1)
            printf("1 file copied\n");
        else
            printf("%d files copied\n", tmp);
	exit();
}

/*
Use the TDIR interface to expand a command line of filespec args.
Return:
  Pointer to a TDIRENT array. If the perr OUT parameter is non-zero
  an error has occurred. In this case the pcount OUT parameter
  will be the array index of the invalid filespec.
*/
TDIRENT *texpand(argv, argc, pcount, perr)
char     *argv[];	/* Array of filespecs for SDCard DIR query */
int      argc;		/* Count of filespec args */
int      *pcount;	/* OUT - Count of expanded entries */
int      *perr;		/* OUT - Parse error if non-zero */
{
	int      i, count;
	FLAGS8   topts;
	unsigned sumsecs;
	FILESPEC tmp;
	TDIRENT  *entry;

	setmem(&tmp, sizeof tmp, 0);
	*pcount = *perr = 0;
	topts = EXECDIR | DIRSTREAM;

	if(ERROR == (entry = sbrk(SYSDIRENTS * sizeof *entry)))
	{
		*perr = ERRMEM;
		return NULL;
	}

	for(i = 0; i < argc; i++)
	{
		/* Always at least one entry, so set defaults */
		entry[*pcount].argname = entry[*pcount].fname = argv[i];
		entry[*pcount].exists = entry[*pcount].numsecs = 0;

		if(!fpath_ok(argv[i], &tmp, TRUE))
		{
			if(i == argc - 1 && !strcmp(argv[i], "."))
			{
				/* Current directory "." OK if last arg */
				++*pcount;
				continue;
			}

			*perr = ERRPARSE;
			break;
		}

		if(tmp.has_uord)
		{
			/* user/drive: only allowed if last arg */
			if(i != argc - 1)
			{
				*perr = ERRUSAGE;
				break;
			}

			++*pcount;
			continue;
		}

		if(!prop_connect(PORT_DIR, &topts))
		{
			*perr = ERRCONNECT;
			break;
		}

		if(ACK != send_fname(tmp.name, topts))
		{
			*perr = ERRDIR;
			break;
		}

		count = get_fnames(entry[*pcount],
				   SYSDIRENTS - *pcount,
				   argv[i], &sumsecs);

		if(!count)
			++*pcount;
		else
			*pcount += count;
	}

	return entry;
}

/*
Receive a file from the Propeller terminal.
Parameters:
  fd - Handle to a file opended in WRITE mode.
*/
int recv_file(fd, entry, opts)
int fd;
{
	if(!prop_connect(PORT_READ, &opts))
		return ERROR;

	if(ACK != send_fname(entry->fname, opts))
		return ERROR;

	pputchar(entry->numsecs & 0xff);	/* LSB of sector count */
	pputchar(entry->numsecs >> 8);		/* MSB of sector count */

	return psdtofile(DEF_DMA, fcbaddr(fd), entry->numsecs);
}

/*
Show program help. Keep it terse if showall is FALSE.
*/
VOID usage(showall)
{
	if(showall)
	{
		puts("Altair 8800micro SDCard to CP/M file copy utility v");
		printf("%s.\n", VERSION);
		puts("  Copy one or more files from the Terminal SDCard ");
		puts("to the CP/M environment.\n");
		puts("  Written by: Garry Jordan.\n\n");
	}

	puts("Usages: TREAD [-o] filename [u/][d:]newname\n");
	puts("\tTREAD [-o] filespec\n");
	puts("\tTREAD [-o] list_of_files {<u/> or <d:> or <u/d:> or <.>}\n");

	if(!showall)
	{
		puts("Use \"TREAD -h\" for full help.\n");
		exit();
	}

	puts("-h\tThis message.\n");
	puts(
"-o\tAll destination files with matching names will be overwritten\n");
	puts("\twithout presenting a warning prompt\n");
	puts(
"u/\tUser area number (0-15). If omitted, the current user area is\n");
	puts(
"\tused. NOTE: if both a drive designator and a user area are given,\n");
	puts("\tthe user area prefix MUST come first.\n");
	puts(
"d:\tDrive designator. If omitted, the currently-logged drive is used.\n\n");
	puts(
"To copy from the SDCard to the currently-logged disk the destination\n");
	puts(
"parameter may be omitted provided the file list consists of a single\n");
	puts(
"filespec. If exactly two unambiguous filenames are given, a \"copy with\n");
	puts(
"rename\" operation is assumed. When multiple filespecs are given, a user\n");
	puts(
"area, drive or \".\" destination specifier MUST be provided.\n");
	puts("\nExamples:\nTREAD foo.c bar.c\nTREAD foo.c a:bar.c\n");
	puts("TREAD foo.c 3/b:bar.c\n");
	puts(
"  Copy \"foo.c\" from the SDCard and rename the destination to \"bar.c\".\n\n");
	puts("TREAD *.com\nTREAD *.com .\n");
	puts(
"  Copy all files on the SDCard with the extension .COM to the currently-\n");
	puts("  logged user area and drive.\n\n");
	puts("TREAD -o *.c *.com foobar.txt b:\n");
	puts(
"  Expand and copy files from the SDCard to the B: drive. All destination\n");
	puts(
"  files with matching names will be overwritten without prompting.\n");

	exit();
}
