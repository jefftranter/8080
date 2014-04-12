/*
TCOMMON.C
Common functions for the Altair 8800micro TREAD, TWRITE, TDEL and TDIR
utility programs.

Garry Jordan, 03/2013
*/

#include <stdio.h>
#include "tutils.h"

/*
Attempt to connect with the Propeller Terminal to initiate a T* utility 
function.
*/
int prop_connect(port, popts)
char  port;		/* Each T function has a unique port ID */
FLAGS8 *popts;	/* Option flags for this port */
{
	char c;

	pputchar(ESC);
	pputchar('[');
	pputchar(port);
	pputchar('S');
	altsleep(5);		/* Wait 1/2 second before checking for response */

	if(pconstat() & 0x01)
	{
		c = pgetchar();

		if(CONID == c || ACK == c)
		{
			if(CONID == c)
			{
				*popts &= ~LEGACY;

				if(*popts & PROBE)
					pputchar(NAK);
				else
				{
					pputchar(ACK);
					pputchar(*popts);
				}
			}
			else
				*popts |= LEGACY;

			return TRUE;
		}
	}

	return FALSE;
}

/*
Probe the terminal T* utility port for services version.
Return:
  FALSE - A terminal connection could not be established.
  TRUE  - Terminal interface present. If the firmware interface
          version is prior to v5.2, the LEGACY bit will be set
          in the popts OUT parameter.
*/
int tport_probe(popts)
FLAGS8 *popts;
{
	*popts |= PROBE;

	if(!prop_connect(PORT_READ, popts))
		return FALSE;

	*popts &= ~PROBE;	/* Don't want PROBE bit left in OUT param */

	if(*popts & LEGACY)
	{
		/* For firmware prior to v5.2 need to close the query out
		   by sending a (hopefully) bogus filename to search for */
		if(NAK != send_fname("$_$._", *popts))
			return FALSE;
	}

	return TRUE;
}

/*
Scan a filespec for the "*" wildcard character.
*/
int has_wstar(fspec)
char *fspec;
{
	while(*fspec)
		if('*' == *fspec++)
			return TRUE;

	return FALSE;
}

/*
Scan a file path for the user/drive designators.
NOTE: This only scans for user/drive. Use the fpath_ok() function
      to verify a complete user/drive:filespec path.
The return value is a packed byte that describes the state of the
user/drive component. If a valid user/drive: component is found,
the upper nibble contains the length of the validated user/drive:
component. The constants used are defined in "tutils.h".
Return:
  0           - No user/drive: designators present.
  HAS_USR     - Bit0 set if valid user designator found.
  HAS_DRV     - Bit1 set if valid drive: designator found.
  UORD_LEN(x) - Use this macro on the return value to retrieve
                the user/drive: component length.  
  UORD_ERR    - Invalid user/drive: component.
*/
int uord_ok(path)
char *path;
{
	char *cp, fidx, uord;

	uord = fidx = 0;
	cp = path;

	/* Check for user/ designator */
	if(isdigit(*cp) &&
	   ('/' == *(cp + 1) || (isdigit(*(cp + 1)) && '/' == *(cp + 2))))
	{
		fidx = '/' == *(cp + 1) ? 2 : 3;
		cp += fidx;

		if(atoi(path) > 15)
			return UORD_ERR;

		uord |= HAS_USR;
	}

	/* Check for drive: designator */
	if(isalpha(*cp) && ':' == *(cp + 1))
	{
		fidx += 2;
		cp += 2;
		uord |= HAS_DRV;
	}

	/* If user/drive delimiters exist now, path is invalid */
	while(*cp)
	{
		if('/' == *cp || ':' == *cp)
			break;

		cp++;
	}

	if(*cp)
		return UORD_ERR;
		
	uord |= fidx << 4;

	return uord;
}

/*
Send a filename to the Propeller terminal. If it's downlevel
firmware it gets space padded to FNAME_LEN. Firmware 5.2 or
greater uses z-strings.
*/
char send_fname(fname, opts)
char   fname[];
FLAGS8 opts;
{
	int i;

	if(!(opts & LEGACY))
		pputchar(strlen(fname));

	for(i = 0; fname[i]; i++)
		pputchar(fname[i]);

	if(opts & LEGACY)
	{
		while(i < FNAME_LEN)
		{
			pputchar(' ');	/* Downlevel space pad to FNAME_LEN */
			i++;
		}
	}

	return pgetchar();	/* Wait for ACK/NAK response */
}

/*
Get filenames from the Propeller terminal as a stream of names. Terminal
firmware version 5.2 or greater is required. Filenames received are
guaranteed to be within 8.3 spec.
Parameters:
  entry    - Array of TDIRENT structures.
  maxent   - # of DIRENT elements in the array.
  psumsecs - If not NULL, the total # of sectors for all files.
Return:
  Count of DIRENT structures filled; otherwise ERROR.
*/
int get_fnames(entry, maxent, arg, psumsecs)
TDIRENT   entry[];		/* Has maxent elements */
int       maxent;
char      *arg;			/* Original arg used in DIR query */
unsigned  *psumsecs;	/* OUT - Aggregate sector count */
{
	char i, len;
	int  count;
	char tmpbuf[FPATH_BUFF];

	count = 0;

	if(psumsecs)
		*psumsecs = 0;

	while(TRUE)
	{
		if(count == maxent)
		{
			pputchar(CAN);
			break;
		}

		pputchar(ACK);	/* Signal ready to receive filename */
		len = pgetchar();

		if(!len)
			break;	/* All done */

		/* Get file size in SECSIZ units */
		entry[count].numsecs = pgetchar();
		entry[count].numsecs |= pgetchar() << 8;

		/* Followed by the filename */
		for(i = 0; i < len; i++)
			tmpbuf[i] = pgetchar();

		tmpbuf[i] = '\0';
		entry[count].argname = arg;
		entry[count].exists = TRUE;

		if(psumsecs)
			*psumsecs += entry[count].numsecs;

		if(ERROR == (entry[count].fname = sbrk(strlen(tmpbuf) + 1)))
			return ERROR;

		strcpy(entry[count++].fname, tmpbuf);
	}

	return count;
}

/*
Validate a user/drive:filespec path.
Return:
  TRUE if arg contains a well-formed CP/M u/d:filespec path.
  If FALSE, all members of the FILESPEC structure are invalid.
*/
int fpath_ok(arg, fspec, expstar)
char     *arg;		/* Potentially any length string */
FILESPEC *fspec;
FLAG     expstar;	/* If non-zero expand "*" wildcard into "?" sequence */
{
	FLAG dot, base, ext, star;
	char *cpr, *pout;
	char reserved[16];

	if(strlen(arg) > FPATH_BUFF)
		return FALSE;

	if(UORD_ERR == (fspec->has_uord = uord_ok(arg)))
		return FALSE;

	fspec->iswcard = 0;
	fspec->path[0] = '\0';

	if(fspec->has_uord)
	{
		base = UORD_LEN(fspec->has_uord);
		fspec->has_uord &= 0x0f;	/* Don't need user/drive length any more */
		strncpy(fspec->path, arg, base);
		fspec->name = &fspec->path[base];
		arg += base;
	}
	else
		fspec->name = fspec->path;

	pout = fspec->name;

	if('!' == *arg)
	{
		/* "!" first character trips "NOTMATCH" flag */
		fspec->opts |= NOTMATCH;
		arg++;
	}

	strcpy(reserved, " ,\/:;<>=[]'|\"");

	dot = base = ext = star = 0;
	for(; *arg; arg++)
	{
		/* Handle wildcard characters */
		if('?' == *arg)
			fspec->iswcard = TRUE;
		else if('*' == *arg)
		{
			fspec->iswcard = TRUE;

			/* Only 1 '*' allowed in base or ext */
			if(++star > 1)
				return FALSE;

			/* '*' wildcard must be last char of base or ext */
			if((!dot && (*(arg + 1) && '.' != *(arg + 1))) ||
			   (dot && *(arg + 1)))
			{
				return FALSE;
			}
			
			if(expstar)
			{
				if(!dot)
				{
					pout += expand_star(pout, 8 - base);
					base = 8;
				}
				else
				{
					pout += expand_star(pout, 3 - ext);
					ext = 3;
				}

				continue;
			}
		}
		else	/* Check for reserved characters */
		{
			for(cpr = reserved; *cpr; cpr++)
			{
				if(*arg == *cpr)
					return FALSE;
			}
		}

		/* Validate base/ext separator */
		if('.' == *arg)
		{
			if(++dot > 1 || 0 == base)
				return FALSE;

			star = 0;	/* Reset "*" tracking */
		}
		else
		{
			if(!dot)
				base++;
			else
				ext++;

			if(base > 8 || ext > 3)
				return FALSE;
		}

		*pout++ = *arg;
	}

	*pout = '\0';
	return TRUE;
}

/*
Expand "*" wildcard into CP/M "?" sequence.
Return:
  Count of "?" characters added to the filespec.
*/
int expand_star(pq, maxq)
char *pq;	/* Filespec to expand, pointing at "?" insertion point. */
int maxq;	/* Count of "?" characters to fill. */
{
	int idx;

	if(!maxq)
		return 0;

	for(idx = 0; idx < maxq; idx++)
		*pq++ = '?';

	return idx;
}

/*
Read a filespec in CP/M FCB format and convert it into an
8.3 DOS filespec.
*/
void get_fcbname(pfcb, fspec)
char *pfcb;
char *fspec;	/* Pointer to an FPATH_BUFF sized array */
{
	int i;

	for(++pfcb, i = 0; i < 8; i++, pfcb++)
	{
		if(' ' == *pfcb)
			break;

		*fspec++ = *pfcb;
	}

	*fspec++ = '.';

	for(i = 0; i < 3; i++, pfcb++)
	{
		*pfcb &= 0x7f;	/* Strip DIR attributes */

		if(' ' == *pfcb)
			break;

		*fspec++ = *pfcb;
	}

	*fspec = '\0';
}

/*
Wait for keypress. After the keypress is detected, let the caller
decide newline handling.
*/
void pressakey()
{
	char c;

	puts("Press a key...");
	while(!kbhit());
	c = getchar();
}

/*
Present a query prompt and return a single key response. Keypress
compares are case-insensitive and return value is always folded to
upper case.
*/
char keyprompt(keys, prompt)
char *keys;		/* String of characters to match */
char *prompt;	/* Prompt text */
{
	char *pkey, key;

	for(pkey = keys; *pkey; pkey++)
		*pkey = toupper(*pkey);

	puts(prompt);

	while(1)
	{
		key = pgetchar();

		if(CTRL_C == key)
			return key;		/* Always return Ctrl-C */

		for(pkey = keys; *pkey; pkey++)
		{
			if(*pkey == toupper(key))
			{
				pputchar(key);
				return *pkey;
			}
		}
	}
}

/*
Log file functions. These functions are all atomic operations;
i.e. the file is always opened, written and closed.
*/
char *init_tlog(basename)
char *basename;
{
	FILE *fp;
	char *name;

	if(ERROR == (name = sbrk(FPATH_BUFF)))
		return NULL;

	strcpy(name, basename);
	strcat(name, ".log");

	if(NULL != (fp = fopen(name, "w")))
	{
		fprintf(fp, "%s log started...\n");
		fclose(fp);
	}

	return name;
}

void tlog(name, msg)
char *name;
char *msg;
{
	FILE *fp;

	if(NULL != (fp = fopen(name, "a")))
	{
		fputs(msg, fp);
		fclose(fp);
	}
}

/*
Get the CP/M operating system version.
Return:
  Zero if MP/M or CP/M version prior to 2.0, otherwise the byte is
  interpreted as two hex digits:
    High nibble: Major version #.
    Low nibble:  Minor version #.
*/
int get_version()
{
	int  ver;
	char b;

	ver = bdos(GETVERSION);
	b = (ver & 0xff00) >> 8;

	if(!b || 0x0f == b)
		ver &= 0x00ff;
	else
		ver = 0;

	return ver;
}

/*
Get disk capacity and allocation information.
Parameters:
  drvnum - Drive # of target. Drive A: = 0..P: == 15.
Return:
  Pointer to a DSKSTAT struct if successful, otherwise NULL.
*/
DSKSTAT *dsk_stat(drvnum)
{
	int      curdrv;
	unsigned bsizkb, dirsizkb;
	int      i, alvsiz, bits;
	DSKPARMS *dpb;
	DSKSTAT  *dstat;
	char     *cp;
	char     fcb[36];
	unsigned *pu;

	if(ERROR == (dstat = sbrk(sizeof *dstat)))
		return NULL;

	curdrv = bdos(GETCURDSK);
	setmem(dstat, sizeof *dstat, 0);
	bdos(SETDMA, DEF_DMA);	/* Use default DMA area */
	bdos(SELDSK, drvnum);
	dpb = bdos(GETDSKPARMS);
	bits = sum_bits(dpb->al0, 1);
	bits += sum_bits(dpb->al1, 1);
	dstat->blksiz = SECSIZ << dpb->bsh;
	bsizkb = dstat->blksiz / 1024;
	dstat->dirmax = dstat->dirfree = dpb->drm + 1;	/* Max DIR entries */
	dirsizkb = bits * bsizkb;		/* DIR entry allocation */
	dstat->dsksizkb = (dpb->dsm + 1) * bsizkb;	/* Total KB disk capacity */

	if(get_version() >= 0x30)
	{
		/* CP/M 3 BDOS 46 returns disk free space in SECSIZ blocks. */
		if(bdos(GETDSKFREE, drvnum) || 0 != peek(DEF_DMA + 2))
			return NULL;
		else
		{
			pu = DEF_DMA;
			dstat->dskfreekb = *pu >> 3;	/* Convert to KByte blocks */
		}
	}
	else
	{
		/* Scan the disk allocation table to compute free space. */
		alvsiz = dpb->dsm / 8 + 1;
		cp = bdos(GETDSKALLOC);

		for(bits = i = 0; i < alvsiz; i++, cp++)
			bits += sum_bits(*cp, 0);

		dstat->dskfreekb = bits * bsizkb;
	}

	dstat->dsksizkb -= dirsizkb;	/* Adjust disk size for DIR alloc */

	/* Scan the disk directory to calculate DIR entries allocated */
	setfcb(fcb, "S");
	fcb[0] = '?';		/* Scan all DIR entries in all user areas */
	fcb[1] = ' ';
	i = bdos(SRCHFIRST, fcb);	/* Search first */

	while(i != 255)
	{
		cp = DEF_DMA + (i << 5);	/* DIR entry fcb starts here */

		if(DELETED != *cp)
			dstat->dirfree--;

		i = bdos(SRCHNEXT, fcb);	/* Search next */
	}

	bdos(SELDSK, curdrv);

	return dstat;
}

/*
Count the number of set bits within a byte.
*/
int sum_bits(byte, set)
char byte;
int  set;	/* If non-zero sum set bits; otherwise sum cleared bits */
{
	int num;

	if(!set)
		byte = ~byte;

	for(num = 0; byte; byte >>= 1)
	{
		if(byte & 0x01)
			num++;
	}

	return num;
}

/*
FIXME: Should move all str* functions to DEFF.CRL.
Compare two strings up to n length. FIXME: Should move to DEFF.CRL.
*/
int strncmp(s1, s2, n)
char *s1, *s2;
int  n;
{
	if(0 == n)
		return 0;

	do
	{
		if(*s1 != *s2++)
			return *s1 - *--s2;

		if(*s1++ == 0)
			break;
	} while(--n != 0);

	return 0;
}

/*
Copy s2 to s1, up to n characters of s2.
*/
char *strncpy(s1, s2, n)
char *s1, *s2;
int  n;
{
	char *tmp;

	if(n <= 0)
		return s1;

	tmp = s1;

	do
	{
		*s1++ = *s2++;
	} while(*s2 && --n);

	*s1 = '\0';
	return tmp;
}

/*
Concatenate s2 to s1, up to n characters of s2.
*/
char *strncat(s1, s2, n)
char *s1, *s2;
int  n;
{
	char *tmp;

	if(n <= 0)
		return s1;

	while(*s1) s1++;

	do
	{
		*s1++ = *s2++;
	} while(*s2 && --n);

	return tmp;
}
tmp;

	if(n <= 0)
		return s1;

