/*
TDEL.C
Altair 8800micro utility to request a "DEL" of filenames from the
Propeller Terminal SDCard.

Garry Jordan, 03/2013
*/

#include <stdio.h>
#include "tutils.h"

#define VERSION "2.0"
#define DYNA "\033[1mY\033[mes/\033[1mN\033[mo/\033[1mA\033[mll"

/*
#define DEBUG
*/

/* Globals */

/* Forward declarations */

main(argc, argv)
int argc;
char **argv;
{
	int      i, tmp;
	int      nfspec, nfiles;
	FLAG     all;
	FLAGS8   gopts;
	FILESPEC **fspecv;
	char     c, prompt[80];

	for(all = 0, i = 1; i < argc;)
	{
		if('-' == *argv[i])
		{
			switch(argv[i][1])
			{
				case 'A':
					all++;
					break;
				case 'H':
				case '?':
					usage(TRUE);
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

	if(1 == argc)
	{
		usage(FALSE);
		exit();
	}

	gopts = EXECDEL;

	if(!tport_probe(&gopts))
	{
		puts("Can't connect to terminal\n");
		exit();
	}

	if(ERROR == (fspecv = sbrk(argc * sizeof *fspecv)))
	{
		puts("Can't allocate filespec array\n");
		exit();
	}

	fspecv[argc] = NULL;

	for(nfspec = 0, i = 1; i < argc; nfspec++, i++)
	{
		if(ERROR == (fspecv[nfspec] = sbrk(sizeof **fspecv)))
		{
			puts("Can't allocate array element\n");
			exit();
		}

		setmem(fspecv[nfspec], sizeof **fspecv, 0);

		if(!fpath_ok(argv[i], fspecv[nfspec], TRUE))
		{
			printf("\007%s ?\n", argv[i]);
			exit();
		}

		if(fspecv[nfspec]->has_uord)
			usage(FALSE);	/* u/d: designators not allowed */

		if((gopts & LEGACY) && fspecv[nfspec]->iswcard)
		{
			puts("Terminal firmware prior to v5.2 can't do wildcards\n");
			exit();
		}

		fspecv[nfspec]->opts = gopts;
	}

	/* Do the deletes */
	for(nfiles = i = 0; i < nfspec; i++)
	{
		if(!all && fspecv[i]->iswcard)
		{
			sprintf(prompt, "Delete %s (%s)? ", argv[i + 1], DYNA);

			if(CTRL_C == (c = keyprompt("YNA", prompt)))
				exit();

			putchar('\n');

			if('A' == c)
				all++;

			if('N' == c)
				continue;
		}

		nfiles += tdelete(fspecv[i], argv[i + 1]);
	}

	printf("%d file(s) deleted.\n", nfiles);	
	exit();
}

/*
Delete file(s) using the Propeller terminal SdDel interface.
*/
int tdelete(fspec, oarg)
FILESPEC *fspec;
char     *oarg;		/* Original cmd line filespec */
{
	int res, deleted;

	if(fspec->opts & LEGACY)
		puts(fspec->name);

	deleted = 0;
	prop_connect((fspec->opts & LEGACY) ? PORT_LDEL : PORT_DEL, &fspec->opts);
	res = send_fname(fspec->name, fspec->opts);

	switch(res)
	{
		case ACK:
			if(fspec->opts & LEGACY)
			{
				putchar('\n');
				deleted++;
			}

			break;
		case NAK:
			puts(" - No file\n");
			break;
		case CAN:
			break;
		default:
			puts("Protocol error\n");
	}

	/* Terminal firmware v5.2 and later outputs SDCard delete results
	   directly to the terminal screen and returns a file count. */
	if(!(fspec->opts & LEGACY))
	{
		res = pgetchar();			/* Non-zero if delete error */
		deleted = pgetchar();		/* LSB of file count */
		deleted |= pgetchar() << 8;	/* MSB of file count */

		if(!deleted && OK == res)
			printf("%s - No file\n", oarg);
	}

	return deleted;
}

/*
Show program help. Keep it terse if showall is FALSE.
*/
void usage(showall)
{
	if(showall)
	{
		puts("Altair 8800micro DEL utility v");
		printf("%s\n", VERSION);
		puts("  Delete terminal SDCard files from the CP/M environment.\n");
		puts("  Written by: Garry Jordan.\n\n");
	}

	puts("Usages: TDEL [-h] ufn_list (terminal firmware prior to v5.2)\n");
	puts("\tTDEL [-a] [-h] filespec_list\n");
	puts("  User/drive designators are not permitted.\n");

	if(!showall)
	{
		puts("Use \"TDEL -h\" for full help.\n");
		exit();
	}

	puts("-h\tThis message.\n");
	puts("-a\tDelete all wildcarded filespecs without confirmation prompt.\n");
	puts("\nExamples:\nTDEL foo.bar foo.com foo.txt\n");
	puts(
"  Must use unambiguous filenames if terminal firmware is prior to v5.2.\n\n");
	puts("TDEL -a foo.bar *.txt foobar foo.com bar.com\n");
	puts(
"  Expand and delete all files without confirmation prompting of\n");
	puts("  wildcarded filespecs.\n");
	exit();
}
uts(
"  Expand and delete all files without confirmation pr