/*
TDSKDIR.C
Altair 8800micro utility to read the CP/M directory track(s) of a disk
image file from the SDCard and display a DIR of populated entries.

The program assumes the disk image file being read has been formatted
by the Altair 8800micro system.

Garry Jordan, 04/2013
*/

#include <stdio.h>
#include "tutils.h"

#define VERSION "1.0"

/*
#define DEBUG
*/

/* open() modes */
#define READ  0
#define WRITE 1

/* Directory attribute file type positions */
#define ATTR_RO   8
#define ATTR_SYS  9
#define ATTR_ARC 10

/* Bitflags for the above */
#define RW_DIR 0x00
#define RO     0x01
#define DIR    0x02
#define SYS    0x04
#define ARC    0x08

/* User area limit. */
#define MAXUSER 15

/* Globals */
int order;

/* Forward declarations */
int tent_comp();

main(argc, argv)
int argc;
char **argv;
{
	int      i, j, tmp, err;
	int      tent;
	FLAG     full;
	FLAGS8   opts;
#ifdef DEBUG
	FLAG     save, rdlocal; save = rdlocal = 0;
#endif
	char     c, *buff;
	unsigned nsecs, sumkb;
	char     save_name[FPATH_BUFF];
	DSKPARMS *dpb;
	CDIRENT  *cent;
	TDIRENT  **entry;

	full = opts = 0, order = 1;

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
					case 'D':
						order = -1;
						break;
					case 'F':
						full++;
						break;
#ifdef DEBUG
					case 'R':
						rdlocal++;
						break;
					case 'S':
						save++;
						break;
#endif
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

	if(2 != argc)
		usage(FALSE);

	if(!tport_probe(&opts))
	{
		puts("Can't connect to terminal\n");
		exit();
	}

	if(opts & LEGACY)
	{
		puts("Terminal firmware v5.2 or newer required\n");
		exit();
	}

#ifdef DEBUG
	if(save && rdlocal)
		usage(FALSE);
#endif

	/* 8800micro disks all the same, so get parameters for current disk */
	dpb = bdos(GETDSKPARMS);
	nsecs = tmp = (dpb->drm + 1) * DIRENTSIZE;
	nsecs /= SECSIZ;

	if(tmp % SECSIZ)
		nsecs++;	/* Round up to full sector */

	/* Size to hold all disk image directory entries */
	if(ERROR == (buff = sbrk(tmp)))
	{
		puts("Can't alloc directory entry buffer\n");
		exit();
	}

#ifdef DEBUG
	if(rdlocal)
	{
		if(ERROR == (tmp = open(argv[1], READ)))
		{
			printf("%s\n", errmsg(errno()));
			exit();
		}

		i = read(tmp, buff, nsecs);
		close(tmp);

		if(nsecs != i)
		{
			puts("Read failed\n");
			exit();
		}
	}
	else
#endif
	{
		puts("Reading directory tracks...");
		err = get_dirtrks(argv[1], buff, dpb->off, nsecs);
		putchar('\n');

		if(err)
		{
			if(NAK == err)
				puts("No file.\n");
			else if(CAN == err)
				puts("File not 88-DCDD image\n");
			else
				puts("Protocol error.\n");

			exit();
		}
	}

#ifdef DEBUG
	if(save)
		save_file(nsecs);
#endif

	/* Scan the buffer to get a count of entries we want */
	tmp = dpb->drm + 1;

	for(cent = buff, tent = i = 0; i < tmp; i++, cent++)
	{
		if(cent->st < 16)
			tent++;		/* Only want allocated user entries */
	}

	if(!tent)
	{
		/* No allocated directory entries found. */
		puts("No entries found.\n");
		exit();
	}

	if(ERROR == (entry = sbrk((tent + 1) * sizeof entry)))
	{
		puts("Can't allocate entry array\n");
		exit();
	}

	/* Turn the raw CDIRENT struct into a more friendly TDIRENT
	   struct for sorting and display. */
	for(cent = buff, tent = i = 0; i < tmp; i++, cent++)
	{
		if(cent->st > MAXUSER)
			continue;		/* Only interested in user entries */

		if(ERROR == (entry[tent] = sbrk(sizeof **entry)))
		{
			puts("Can't allocate entry\n");
			exit();
		}

		setmem(entry[tent], sizeof *entry, 0);
		entry[tent]->cpment = cent;
		entry[tent]->fname = sbrk(FNAME_LEN);
		entry[tent]->extnum = (cent->xl & 0x1f) | ((cent->xh & 0x3f) << 5);

		for(j = 0; j < 8; j++)
			entry[tent]->fname[j] = cent->f0_f7[j] & 0x7f;

		entry[tent]->fname[j++] = ' ';
		entry[tent]->fname[j++] = cent->t0_t2[0] & 0x7f;
		entry[tent]->fname[j++] = cent->t0_t2[1] & 0x7f;
		entry[tent]->fname[j++] = cent->t0_t2[2] & 0x7f;
		entry[tent++]->fname[j] = '\0';
	}

	entry[tent] = NULL;

	/* Sort by filename and extent number, display the results */
	puts("Sorting entries...");
	qsort(entry, tent, sizeof *entry, &tent_comp);
	putchar('\n');

	/* Show the results */
	for(tmp = 1, sumkb = i = j = 0; *entry;)
	{
		if((*entry)->extnum > 0 && *(entry + 1))
		{
			if(!strcmp((*entry)->fname, (*(entry + 1))->fname))
			{
				entry++;
				tent--;
				continue;
			}
		}

		if(full && tmp)
		{
			printf("Directory of: %s\n", argv[1]);
			header();
		}

		if(full)
			tmp = fdetail(*entry, dpb, &sumkb, i, &j);
		else
			tmp = detail(*entry, dpb, &sumkb, i);

		entry++;
		i++;

		if(tmp)
		{
			pressakey();
			putchar('\r');
		}
	}

	if((full && i & 1) || (!full && i % 4))
		putchar('\n');

	puts("----------------------------\n");
        if (tent == 1)
            printf("1 file, %uk used on disk\n", sumkb);
        else
            printf("%d files, %uk used on disk\n", tent, sumkb);
	exit();
}

/*
Receive a block of SDCard data from the Propeller terminal.
*/
int get_dirtrks(fname, buff, trkoff, nsecs)
char     *fname;
char     *buff;
unsigned trkoff;	/* # of system tracks to seek past */
unsigned nsecs;		/* Sectors to read */
{
	int    res;
	FLAGS8 opts;

	opts = EXECDIR;

	if(!prop_connect(PORT_READ, &opts))
		return ERROR;

	if(ACK != (res = send_fname(fname, opts)))
		return res;

	/* 88-DCDD track offset and sector count always < 255 */
	pputchar(trkoff);
	pputchar(nsecs);

	/* Wait for Propeller to seek into SDCard file */
	if(ACK != pgetchar())
		return ERROR;

	return psdtomem(buff, nsecs);
}

#ifdef DEBUG
void save_file(defname, buff, nsecs)
char     *defname, *buff;
unsigned nsecs;
{
	int fd, err;

	puts("Saving directory tracks to local file...\n");

	if(ERROR == (fd = creat(defname)))
		printf("%s\n", errmsg(errno()));
	else
	{
		err = write(fd, buff, nsecs);

		if(err < 0)
			printf("%s\n", errmsg(errno()));
		else if(err != nsecs)
			puts("Short write - disk full?\n");

		close(fd);
	}
}
#endif

void header()
{
	int i;

	for(i = 0; i < 2; i++)
		puts("     Name       Bytes  Recs    Attr    ");

	putchar('\n');

	for(i = 0; i < 2; i++)
		puts("--------------- ------ ----- --------- ");

	putchar('\n');
}

/*
Show user/filename only.
*/
int detail(entry, dpb, psumkb, idx)
TDIRENT  *entry;
DSKPARMS *dpb;
unsigned *psumkb;
int      idx;
{
	unsigned recs, kb;

	/* Calculate record count and disk blocks used */
	recs = entry->extnum << 7 | (entry->cpment->rc & 0x7f);
	kb = recs & dpb->blm;

	if(kb)
		kb = (dpb->blm + 1) - kb;

	kb = (recs + kb) >> (dpb->bsh - 1);
	*psumkb += kb;

	printf("%2d/%-12s", entry->cpment->st, entry->fname);

	if(!(++idx % 4))
		putchar('\n');
	else
		puts(" | ");

	return FALSE;
}

/*
Useful details that can be gleaned from just the directory entries.
Return:
  TRUE when at screen page boundary; otherwise FALSE.
*/
int fdetail(entry, dpb, psumkb, idx, plines)
TDIRENT  *entry;
DSKPARMS *dpb;
unsigned *psumkb;
int      idx;
int      *plines;
{
	int      i;
	char     c;
	FLAGS8   attr;
	unsigned recs, kb;

	/* Calculate record count and disk blocks used */
	recs = entry->extnum << 7 | (entry->cpment->rc & 0x7f);
	kb = recs & dpb->blm;

	if(kb)
		kb = (dpb->blm + 1) - kb;

	kb = (recs + kb) >> (dpb->bsh - 1);
	*psumkb += kb;

	/* Get file attributes */
	attr = RW_DIR;

	for(i = 0; i < 3; i++)
	{
		if((c = entry->cpment->t0_t2[i] & 0x80))
		{
			switch(i)
			{
				case ATTR_RO:
					attr |= RO;
					break;
				case ATTR_SYS:
					attr |= SYS;
					break;
				case ATTR_ARC:
					attr |= ARC;
					break;
			}
		}
	}

	printf("%2d/%-12s %5uk %5u %-3s %-2s %c  ",
           entry->cpment->st, entry->fname, kb, recs,
	       attr & SYS ? "Sys" : "Dir",
	       attr & RO ? "RO" : "RW",
	       attr & ARC ? 'A' : ' ');

	if(idx & 1)
	{
		putchar('\n');

		if(0 == ++*plines % 36)
			return TRUE;
	}

	return FALSE;
}

int tent_comp(c1, c2)
TDIRENT **c1, **c2;
{
	int tmp;

	if((tmp = strcmp((*c1)->fname, (*c2)->fname)))
		return tmp > 0 ? 1 * order : -1 * order;

	/* Multiple entries for the same file, so sort by extent #
	   in ascending order */
	if((*c1)->extnum > (*c2)->extnum)
		return 1;

	if((*c1)->extnum < (*c2)->extnum)
		return -1;

	return 0;
}
	
/*
Show program help. Keep it terse if showall is FALSE.
*/
VOID usage(showall)
{
	if(showall)
	{
		puts("Altair 8800micro CP/M disk image DIR utility v");
		printf("%s.\n", VERSION);
		puts("  Read the directory tracks of a CP/M disk image file from");
		puts(" the terminal\n  SDCard and display allocated entries.\n");
		puts("  Written by: Garry Jordan.\n\n");
	}

#ifdef DEBUG
	puts("Usage: TDSKDIR [-h] [-d] [-f] [-r] [-s] image_name\n");
#else
	puts("Usage: TDSKDIR [-h] [-d] [-f] image_name\n");
#endif

	if(!showall)
	{
		puts("Use \"TDSKDIR -h\" for full help.\n");
		exit();
	}

	puts("-h\tThis message.\n");
	puts("-d\tSort filenames in descending order.\n");
	puts("-f\tFULL listing. Displays the user area, name, size, number of\n");
	puts(
"\t128-byte records, and attributes of the files.\n");
#ifdef DEBUG
	puts("-r\tRead a directory image from a CP/M disk, where image_name is\n");
	puts("\ta file that was previously saved with the \"-s\" switch.\n");
	puts("-s\tSave the in-memory directory image to a CP/M disk.\n");
	puts("NOTE: The -r and -s switches are mutually exclusive.\n");
#endif
	exit();
}
