/*
DSKPARMS.C
Test program for the TCOMMON.C functions.
*/

#include <stdio.h>
#include "tutils.h"

/* Globals */

/* Forward declarations */

main(argc, argv)
int argc;
char **argv;
{
	DSKPARMS *dpb;
	char     *alv, cur_drive;
	int      i, j, k, bits;
	int      bsizkb, dsizkb, dirkb, kbfree;
	int      dirents;
	char     fcb[36];
	char     *dma;

	cur_drive = bdos(GETCURDSK);

	if(argc == 2 && 2 == strlen(argv[1]) && ':' == argv[1][1])
		bdos(SELDSK, argv[1][0] - 'A');	/* Set requested drive */

	dpb = bdos(GETDSKPARMS);
	alv = bdos(GETDSKALLOC);

	printf("\033[H\033[JDisk Parameter Table:\n");
	printf("SPT: %u\nBSH: %u\nBLM: %u\n", dpb->spt, dpb->bsh, dpb->blm);
	printf("EXM: %u\nDSM: %u\nDRM: %u\n", dpb->exm, dpb->dsm, dpb->drm);
	printf("AL0|AL1: %08b|%08b\n", dpb->al0, dpb->al1);
	printf("CKS: %u\nOFF: %u\n", dpb->cks, dpb->off);

	bsizkb = (SECSIZ << dpb->bsh) / 1024;	/* Block size in KBytes */
	dsizkb = (dpb->dsm + 1) * bsizkb;	/* Disk capacity in KBytes */

	/* Compute KBbytes allocated to direntry space */
	bits = sum_bits(dpb->al0, 1);
	bits += sum_bits(dpb->al1, 1);
	dirkb = bits * bsizkb;				/* KBytes allocated */
/*
	printf("\nbsizkb: %d\n", bsizkb);
	printf("dsizkb: %d\n", dsizkb);
	printf("dirkb:  %d\n", dirkb);
*/
	printf("\nALV (Block Allocation Table):\n");
	j = dpb->dsm / 8 + 1;

	for(i = bits = 0; i < j; alv++)
	{
		if(!(++i & 0x07))
			printf("%08b\n", *alv);
		else
			printf("%08b|", *alv);

		bits += sum_bits(*alv, 0);
	}

	dirents = j = 0;
	printf("\n\nDrive %c: files:\n", 'A' + bdos(GETCURDSK));

	setfcb(fcb, "S");
	fcb[0] = '?';
	fcb[1] = ' ';

	dma = 0x80;
	bdos(SETDMA, dma);
	i = bdos(SRCHFIRST, fcb);
	k = 0;

	while(i != 255)
	{
		alv = dma + (i << 5);

		if(*(alv + 12) > 1 || DELETED == *alv || LABEL == *alv)
		{
			if(DELETED != *alv)
				dirents++;

			goto snext;
		}

		printf("%2u", *alv);
		putchar('/');

		for(j = 1; j < 12; j++)
			putchar(alv[j] & 0x7f);

		putchar(' ');

		if(!(++k % 5))
			printf("\n");
snext:
		i = bdos(SRCHNEXT, fcb);
	}

	if(k % 5)
		printf("\n");

	printf("----------------\n");
	printf("%d directory entries allocated, %d entries remain\n",
		dirents, dpb->drm + 1 - dirents);

	/* Compute disk useable capacity and free space */
	kbfree = bits * bsizkb;
	printf("Disk capacity:   %3d KBytes\n", dsizkb - dirkb);
	printf("Disk free space: %3d KBytes\n", kbfree);

	bdos(SELDSK, cur_drive);
	exit();
}
