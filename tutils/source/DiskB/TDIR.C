/*
TDIRS.C
Altair 8800micro utility to request a "DIRSTREAM" of file names from the
Propeller Terminal SDCard.

Garry Jordan, 03/2013
*/

#include <stdio.h>
#include "tutils.h"

#define VERSION "2.0"
#define MAXNAMES 512

/*
#define DEBUG
*/

/* Globals */
int order;

/* Forward declarations */
int name_comp();
int size_comp();

main(argc, argv)
int argc;
char **argv;
{
	int      i, tmp;
	int      lines, count;
	FLAG     bysize, page;
	unsigned sumsecs;
	TDIRENT  *dirent;
	FILESPEC fspec;
	int      (*qsfunc)();

	order = 1;
	count = bysize = 0;
	setmem(&fspec, sizeof fspec, 0);

	for(i = 1; i < argc;)
	{
		if('-' == *argv[i])
		{
			for(tmp = 1; argv[i][tmp]; tmp++)
			{
				switch(argv[i][tmp])
				{
					case 'D':
						order = -1;		/* Entries in descending order */
						break;
					case 'H':
					case '?':
						usage(TRUE);
					case 'F':
						fspec.opts |= DIRSTREAM;
						break;
					case 'S':
						bysize++;	/* Sort by file size */
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
	{
		argv[1] = "*.*";
		argc++;
	}

	if(argc > 2)
		usage(FALSE);

	if(!tport_probe(&fspec.opts))
	{
		puts("Can't connect to terminal\n");
		exit();
	}

	fspec.opts |= EXECDIR;

	if(!fpath_ok(argv[1], &fspec, TRUE))
	{
		printf("\007%s ?\n", argv[1]);
		exit();
	}

	if(fspec.has_uord)
		usage(FALSE);	/* u/d: designators not allowed */

	if(fspec.opts & DIRSTREAM)
	{
		if(fspec.opts & LEGACY)
		{
			puts("-f option requires terminal firmware v5.2 or newer\n");
			exit();
		}

		if(ERROR == (dirent = sbrk(MAXNAMES * sizeof(*dirent))))
		{
			puts("Memory allocation failed\n");
			exit();
		}

		puts("Scanning SDCard root folder...");
	}

	prop_connect(PORT_DIR, &fspec.opts);

	if(ACK != send_fname(fspec.name, fspec.opts))
	{
		puts("Protocol error\n");
		exit();
	}

	if(!(fspec.opts & DIRSTREAM))
		exit();		/* Done if quick DIR or pre-v5.2 terminal */

	count = get_fnames(dirent, MAXNAMES, argv[1], &sumsecs);

	if(count == MAXNAMES)
		printf("WARNING: %d entry limit reached.\n", MAXNAMES);

	puts("\nSorting directory...");

	qsfunc = bysize ? &size_comp : &name_comp;			
	qsort(dirent, count, sizeof *dirent, qsfunc);
	putchar('\n');

	for(page = 1, i = lines = tmp = 0; i < count; i++)
	{
		if(page)
			header();

		page = detail(&dirent[i], &tmp, &lines);
	}

	printf("%s--------------------------\n", tmp ? "\n" : "");
        if (count == 1)
            printf("%d KBytes in 1 file\n", secstokb(sumsecs));
        else
            printf("%d KBytes in %d files\n", secstokb(sumsecs), count);
	exit();
}

/*
Display directory header.
*/
void header()
{
	int i;

	for(i= 0; i < 3; i++)
		printf("%8s%11s%5s  ", "Name", "KBytes", "Recs");

	putchar('\n');

	for(i = 0; i < 3; i++)
		puts("------------ ------ ----- ");

	putchar('\n');
}

/*
Display a directory entry.
*/
int detail(entry, pcol, pline)
TDIRENT *entry;
int     *pcol;
int     *pline;
{
	char c;

	printf("%-12s", entry->fname);

	if(entry->numsecs > 0 && entry->numsecs < 8)
		puts("     <1");
	else
		printf("%7u", secstokb(entry->numsecs));

	printf(" %5u ", entry->numsecs);

	if(3 == ++*pcol) 
	{
		putchar('\n');
		*pcol = 0;

		if(0 == ++*pline % 37)
		{
			pressakey();
			putchar('\r');
			return TRUE;
		}
	}

	return FALSE;
}

/*
Convert SECSIZ units into KBytes.
*/
int secstokb(nsecs)
{
	int size;

	size = nsecs >> 3;

	if(nsecs % 8)
		size++;		/* Round up if not on even 1KB boundary */

	return size;
}

/*
Compare two TDIRENT fname members (ascending).
Return:
   1 if c1 > c2
  -1 if c1 < c2
   0 if c1 == c2
*/
int name_comp(c1, c2)
TDIRENT *c1, *c2;
{
	int res;

	if((res = strcmp(c1->fname, c2->fname)))
		return res > 0 ? 1 * order : -1 * order;

	return 0;
}

/*
Compare two TDIRENT numsecs members.
*/
int size_comp(c1, c2)
TDIRENT *c1, *c2;
{
	int res;

	if(c1->numsecs > c2->numsecs)
		return 1 * order;

	if(c1->numsecs < c2->numsecs)
		return -1 * order;

	/* Size equal so order by filename */
	return name_comp(c1, c2);
}

/*
Show program help. Keep it terse if showall is FALSE.
*/
void usage(showall)
{
	if(showall)
	{
		puts("Altair 8800micro DIR utility v");
		printf("%s\n", VERSION);
		puts(
"  Request a listing of Terminal SDCard files from the CP/M environment.\n");
		puts("  Written by: Garry Jordan.\n\n");
	}

	puts("Usage: TDIR [-h] [-f[d][s]] [filespec]\n");
	puts("  User/drive designators are not permitted.\n");
	puts(
"  Only one filespec may be used, and wildcard characters \"*\" and \"?\"\n");
	puts(
"  are permitted. If the filespec is omitted, all files are displayed.\n");

	if(!showall)
	{
		puts("Use TDIR -h for full help.\n");
		exit();
	}

	puts("-h\tThis message.\n");
	puts(
"-f\tFULL listing (terminal firmware v5.2 or newer). Displays the name,\n");
	puts("\tsize, and number of 128-byte records of the files.\n");
	puts(" d\tSort entries in descending order (-f switch only).\n");
	puts(" s\tSort entries by file size (-f switch only).\n");
	puts("\nExample:\n");
	puts("TDIR -fsd *.com\n");
	puts(
"  Display a FULL listing of files with a .COM extension, in descending\n");
	puts("  order by size.\n");
	exit();
}
