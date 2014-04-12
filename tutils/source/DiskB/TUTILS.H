/*
TUTILS.H
Garry Jordan, 03/2013

Constants and externals common to the Altair 8800micro TREAD,
TWRITE, TDIR and TDEL SDCard<->CP/M file transfer utilities.
*/

/*
Protocol ID that's passed between the Propeller terminal and
these utilities to ensure consistency between interface revisions.
*/
#define CONID 0x52

/*
Constants used for CP/M system stuff.
*/
#define DEF_DMA 0x0080
#define DEF_FCB 0x005c

/*
Directory entries.
*/
#define SYSDIRENTS  128	/* Maximum # of directory entries */
#define DIRENTSIZE   32	/* Size of directory entry */
#define LABEL      0x20	/* Entry is disk label */
#define TIMESTAMP  0x21	/* Entry is time stamp */
#define DELETED    0xe5	/* Entry is unused */

/*
Often-used BDOS functions. Functions > 40 are CP/M 3 only.
*/
#define GETVERSION  12
#define SELDSK      14
#define SRCHFIRST   17
#define SRCHNEXT    18
#define GETCURDSK   25
#define SETDMA      26
#define GETDSKALLOC 27
#define GETDSKPARMS 31
#define GETSETUSER  32
#define GETDSKFREE  46

/*
Flags used for user/drive:filespec validation.
*/
#define UORD_ERR 0xff
#define HAS_USR  0x01
#define HAS_DRV  0x02
#define UORD_LEN(x) ((x) >> 4)

/* Option flags sent to the Propeller terminal */
#define OVERWRT   0x01	/* Suppress Y/N file overwrite */
#define TEXTEOF   0x02	/* Ctrl-Z (ASCII SUB) char triggers EOF */
#define LEGACY    0x04	/* Read/write in T* version 1.x mode */
#define DIRSTREAM 0x08	/* Request to stream SDCard DIR filenames */
#define EXECDIR   0x10	/* Execute SDCard "DIR" or "DSKDIR" function */
#define EXECDEL   0x20	/* Execute the terminal "DEL" function */
#define NOTMATCH  0x40	/* Filter names that DO NOT match filespec */
#define PROBE     0x80	/* Ping the terminal for T* utility services */

/*
Propeller terminal ESC[pS "ports" for the T* utilities.
*/
#define PORT_READ  '3'
#define PORT_WRITE '4'
#define PORT_DIR   '6'
#define PORT_DEL   '6'
#define PORT_LDEL  '7'

/* File access modes for the C open() function. */
#define READ  0
#define WRITE 1
#define RDWR  2

/* File seek modes */
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

/*
Maximum filename length "8.3" on both CP/M and DOS file systems.
*/
#define FNAME_LEN  12
#define FPATH_BUFF 20	/* Room for user/drive:filename */

/*
Often used ASCII codes.
*/
#define CTRL_C 0x03
#define ACK    0x06
#define CTRL_Q 0x11
#define CTRL_S 0x13
#define NAK    0x15
#define CAN    0x18
#define CTRL_Z 0x1a
#define ESC    0x1b

/*
BIOS call constants.
*/
#define CONST  2	/* c = bios(2); */
#define CONIN  3	/* c = bios(3); */
#define CONOUT 4	/* bios(4, c); */

/*
Poor man's typedefs
*/
#define FLAG     char
#define FLAGS8   char
#define FLAGS16  unsigned
#define BLKALLOC union  _blkalloc
#define CDIRENT  struct _cdirent
#define TDIRENT  struct _tdirent
#define FILESPEC struct _filespec
#define DSKSTAT  struct _dskstat
#define DSKPARMS struct _dpb

BLKALLOC
{
	char     alb[16];
	unsigned alw[8];
};

/* The CP/M directory entry (a.k.a. extent) */
CDIRENT
{
	char     st;
	char     f0_f7[8];
	char     t0_t2[3];
	char     xl;
	char     bc;
	char     xh;
	char     rc;
	BLKALLOC blkptr;
};

/*
Used by the TDIR and TDSKDIR utilities for formatted DIR console output.
The structure is an amalgam of DOS and CP/M properties, as TDIR displays
DOS directory information, and TDSKDIR displays CP/M directory information.
*/
TDIRENT
{
	char     *argname;	/* Original un-expanded filespec (TDIR) */
	char     *fname;
	FLAG     exists;
	unsigned numsecs;	/* Size of file in CP/M sectors */
	CDIRENT  *cpment;	/* TDSKDIR uses this */
	unsigned extnum;	/* TDSKDIR calculated extent number */
};

/* Used to validate TDIR, TDEL and TREAD input filespecs */
FILESPEC
{
	FLAGS8 opts;		/* Terminal-side function options */
	FLAG   iswcard;
	FLAGS8 has_uord;	/* Details about user/drive: designators */
	char   *name;
	char   path[FPATH_BUFF];
};

/* Disk information */
DSKSTAT
{
	unsigned blksiz;
	unsigned dsksizkb;
	unsigned dskfreekb;
	unsigned dirmax;
	unsigned dirfree;
};
	
/* CP/M Disk Parameter Block */
DSKPARMS
{
	unsigned spt;
	char	 bsh;
	char	 blm;
	char	 exm;
	unsigned dsm;
	unsigned drm;
	char	 al0;
	char	 al1;
	unsigned cks;
	unsigned off;
};
 bsh;
	char	 blm