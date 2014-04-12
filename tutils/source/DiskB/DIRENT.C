/*
DIRENT.C
Display the directory extent for a given filename.
*/

#include <stdio.h>
#include "tutils.h"

/* Globals */

/* Forward declarations */

main(argc, argv)
int argc;
char **argv;
{
	int      i, j, curdsk;
	char     *cp;
	char     fcb[36];
	char     attrs[16];
	unsigned log_ext;
	FILESPEC fspec;
	CDIRENT  *entry;

	if(argc != 2)
	{
fail:
		puts("Usage: DIRENT filespec\n");
		exit();
	}

	if(!fpath_ok(argv[1], &fspec, TRUE))
		goto fail;

	setmem(&fcb[0], 36, 0);

	curdsk = bdos(GETCURDSK);

	if(':' == fspec.path[1])
		bdos(SELDSK, fspec.path[0] - 'A');

	for(cp = fspec.name, j = 1, i = 0; i < 8; i++)
	{
		if(!*cp || '.' == *cp)
			fcb[j++] = ' ';
		else
			fcb[j++] = *cp++;
	}

	if('.' == *cp)
		cp++;

	for(i = 0; i < 3; i++)
	{
		if(!*cp)
			fcb[j++] = ' ';
		else
			fcb[j++] = *cp++;
	}

	fcb[12] = '?';	/* Search for all entries matching the ufn */
	bdos(SETDMA, DEF_DMA);
	j = 0;

	if(255 == (i = bdos(SRCHFIRST, fcb)))
	{
		puts("No file.\n");
		goto bail;
	}

again:
	if(3 == j++)
	{
		keyprompt(" ", "Press space...");
		putchar('\n');
		j = 0;
	}

	cp = DEF_DMA;
	entry = cp + (i << 5);

	printf("%-17s\n-----------------\n", argv[1]);
	printf("st:    0x%02x\n", entry->st);
	puts("file:  ");

	for(i = 0; i < 8; i++)
		putchar(entry->f0_f7[i] & 0x7f);

	for(i = 0; i < 3; i++)
		putchar(entry->t0_t2[i] & 0x7f);

	printf(" - %s %s %s\n",
	       entry->t0_t2[0] & 0x80 ? "Sys" : "Dir",
	       entry->t0_t2[1] & 0x80 ? "RO" : "RW",
	       entry->t0_t2[2] & 0x80 ? "Archive" : "");

	log_ext = (entry->xl & 0x1f) | ((entry->xh & 0x3f) << 5);
	printf("xhxl:  0x%04x\n", log_ext);
	printf("ent#:    %4u\n", log_ext >> 1);
	printf("bc:      0x%02x\n", entry->bc);
	printf("rc:      0x%02x\n", entry->rc);
	printf("#recs:   %4u\n", log_ext << 7 | (entry->rc & 0x7f));
	printf("alloc:   ");
	
	for(i = 0; i < 16; i++)
	{
		printf("%4u", entry->blkptr.alb[i]);

		if(!((i + 1) % 8))
		{
			puts("\n         ");
			continue;
		}
	}

	putchar('\n');

	if(255 != (i = bdos(SRCHNEXT, fcb)))
		goto again;

bail:
	bdos(SELDSK, curdsk);
	exit();
}

	}

	putchar('\n');

	if(255 != (i = bdos(SRCHNEXT, fcb)))
		goto again;

bail:
	bdos(SELDSK, curdsk);