/*
TTEST.C
Test program for the TCOMMON.C functions.
*/

#include <stdio.h>
#include "tutils.h"

main(argc, argv)
int argc;
char **argv;
{
	FILESPEC fs, *pfs;
	char arg[80];
	int  i;
	char c, *cp;

	iobreak(0);

	for(i = 0; i < 40; i++)
	{
		puts("furp!\n");
		psleep(5);

		if(CTRL_C == ctrlc_chk())
		{
			puts("^C\n");
			break;
		}
	}

	exit();

	setmem(&fs, sizeof fs, 0);
	fs.opts = 0;

	while(1)
	{
		scanf("%s", arg);

		if('Q' == toupper(arg[0]))
			break;

/*
		if(UORD_ERR == (c = uord_ok(arg)))
		{
			puts("\007furp!\n");
			continue;
		}

		printf("Return: %08b\nUser:   %s\nDrive:  %s\nOffset: %d\n",
		       c,
		       c & HAS_USR ? "yes" : "no",
		       c & HAS_DRV ? "yes" : "no",
		       UORD_LEN(c));
*/
		if(!fpath_ok(arg, &fs, TRUE))
		{
			puts("\007furp!\n");
			continue;
		}

		printf("FLAGS8:      %08b\n", fs.opts);
		printf("Is wildcard: %s\n", fs.iswcard ? "TRUE" : "FALSE");
		printf("Has user:    %s\n", fs.has_uord & HAS_USR ? "TRUE" : "FALSE");
		printf("Has drive:   %s\n", fs.has_uord & HAS_DRV? "TRUE" : "FALSE");
		printf("Filename:    %s\n", fs.name);
		printf("Full path:   %s\n", fs.path);
	}

	exit();
}

RV? "TRUE" : "FALSE");
		printf("Filename:    %s\n", f