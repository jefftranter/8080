/*
	CP.C	v2.1
	Written by Leor Zolman, 3/82
	Last revision 6/86 for BDS C v1.6

	CP copies files to and from different drives and user areas on
	a CP/M filesystem.

	Usages:

	A>cp <filename> <newname> [options] <cr>
	A>cp <list_of_files> <cr>
	A>cp <list_of_files> { <user#/> or <d:> or <.> } [options] <cr>

	Options:	-V	Verify by reading after writing
			-B	Backup: delete original after copying

	Wildcard expansion is performed only on files present in the
	currently logged drive and user area (the "current directory").

	Any filename may contain an optional disk designator prefix of
	the form <d:> and/or a user area number prefix of the form <n/>
	preceding the actual name. If both must be given, the user area
	prefix must come first.

	When a single file is to be copied, simply give the old and new
	filenames.

	To copy into the currently logged directory from anywhere else,
	the destination parameter may simply be omitted provided the file
	list consists of either a) a single unabmiguous filename, or b) a
	single ambiguous filename specifier.

	Examples:

	To copy *.COM from the current directory into user area 0 of drive C:
		cp  *.com  0/C:

	To copy FOO.* from user area 7 into the current user area:
		cp 7/foo.*

	To copy FOO.C and BAR.C into the current directory from user area 8:
		cp 8/FOO.C 8/BAR.C .



	Compilation and Linkage:
		cc cp.c
		clink cp wildexp -n
*/

#include <stdio.h>
#include "tutils.h"

main(argc,argv)
char **argv;
{
	int i,j,k,c,loop;
	int fd1,fd2;
	int bufsects;
	unsigned bufsize;
	unsigned corebuf;
	char cur_disk;			/* currently logged-in disk */
	char destname[30];
	char *lastarg;
	char verify, *vbuf;		/* true when verifying writes */
	char backupf;
	FILESPEC fs; setmem(&fs, sizeof fs, 0);

	backupf = verify = FALSE;

	i = 1;
	while (i < argc)
	{
		if (*argv[i] == '-')
		{
			switch(argv[i][1])
			{
				case 'V':
					verify = TRUE;
					break;
				case 'B':
					backupf = TRUE;
					break;
				default:
					printf("Unrecognized option: %s\n",
								argv[i]);
					goto usages;
			}
			argc--;
			for (j = i; j < argc; j++)
				argv[j] = argv[j+1];
		}
		else
			i++;
	}

	for(i = 1; i < argc; i++)
	{
		if(!fpath_ok(argv[i], &fs, FALSE))
		{
			printf("\007%s ?\n", argv[i]);
			exit();
		}
	}

	wildexp(&argc,&argv);
	cur_disk = 'A' + bdos(25);

	if (argc == 2)	  /* Handle implied current dir destination */
	{
		argv[2] = ".";
		argc++;
	}

	lastarg = argv[argc - 1];

	if (3 == argc && !strcmp(argv[1], argv[2]))
	{
		puts("Can't copy a file onto itself\n");
		exit();
	}

	if (argc < 3 || (argc > 3 &&
	    !((c = lastarg[strlen(lastarg)-1]) == '/' || c == ':' ||
					!strcmp(lastarg,"."))))
	{
usages:		printf("Usages: cp [u/][d:]filename [u/][d:]newname [-vb]\n");
		printf("        cp <list_of_files>  ");
			printf("{ <u/> or <d:> or <u/d:> or <.>} [-vb]\n");
		printf("	cp <ambiguous_file_name>\n");
		exit();
	}

	if (verify)
		vbuf =  sbrk(SECSIZ);

	corebuf = sbrk(SECSIZ);
	for (bufsize = SECSIZ; sbrk(SECSIZ) != ERROR; bufsize += SECSIZ)
		;
	bufsects = bufsize / SECSIZ;


	for (loop = 1; loop < argc-1; loop++)
	{
		if ((fd1 = open(argv[loop],0)) == ERROR) {
			printf("\nCan't open %s\n",argv[loop]);
			continue;	/* go on to next one anyway. */
		}

		strcpy(destname,lastarg);	/* create output filename */

		if ( (c = destname[strlen(destname) - 1])=='/' || c == ':' ||
		  				!strcmp(destname,"."))
		{
			if (!strcmp(destname,"."))
				*destname = '\0';
			for (i = strlen(argv[loop]) - 1; i >= 0; i--)
			{
				if (argv[loop][i] == '/' && isdigit(argv[loop][i-1]))
					break;
				if (argv[loop][i] == ':')
					break;				
			}
			strcat(destname,&argv[loop][i+1]);
		}

		if ((fd2 = creat(destname)) == ERROR) {
			printf("\nCan't create %s\n",destname);
			printf("Assuming out of directory space and aborting.\n");
			exit();	
		}

		if (loop != 1)
			putchar('\n');

		printf("\t Copying %s to %s...",argv[loop],destname);

		while (1)
		{
			if (kbhit()) getchar();
			if (!(i = read(fd1,corebuf,bufsects))) break;
			if (i == ERROR)
			{
				printf("\nRead error: tell(fd1) = %d, \"%s\"\n",
					tell(fd1), errmsg(errno()));
				break;
			}

			if (kbhit()) getchar();
			if (write(fd2,corebuf,i) != i) {
				printf("\nWrite error: %s\n",errmsg(errno()));
				exit();
			}
			if (verify)
			{
				puts("[v] ");
				seek(fd2, -i, 1);
				for (j = 0, k = corebuf; j < i; j++, k += SECSIZ)
				{
					if (read(fd2, vbuf, 1) != 1)
						printf("\nVerify read error on %s\n",
							destname);
					if (memcmp(vbuf, k, SECSIZ))
						continue;
					else
						printf("\nVerify error on %s\n",
							destname);
				}
			}
		}

		if (close(fd2) == ERROR) {
			printf("\nCan't close the output file.\n");;
			exit();
		}
		fabort(fd1);
		if (backupf)
			unlink(argv[loop]);
	}
}
