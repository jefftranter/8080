/*
	RM.C: Generalized file removal utility
	Written by Leor Zolman, 9/86

	Usage:
		rm <list of files>

	Compile & Link:
		cc rm.c
		clink rm wildexp -n
*/

#include <stdio.h>
#include "tutils.h"

main(argc,argv)
char **argv;
{
	int i;
	FILESPEC fs;

	if(1 == argc)
	{
		puts("Usage: RM list_of_files\n");
		exit();
	}

	setmem(&fs, sizeof fs, 0);

	for(i = 1; i < argc; i++)
	{
		if(!fpath_ok(argv[i], &fs, FALSE))
		{
			printf("\007%s ?\n", argv[i]);
			exit();
		}
	}

	wildexp(&argc, &argv, 1);

	for (i = 1; i < argc; i++)
	{
		puts("Removing "); puts(argv[i]);

		if(unlink(argv[i]))
			printf(" - %s", errmsg(errno()));

		putchar('\n');
	}
}
		puts("Removing "); puts(argv[i]);

		if(unlink(argv[i]))
			p