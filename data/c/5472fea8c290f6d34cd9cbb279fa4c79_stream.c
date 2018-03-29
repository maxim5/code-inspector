/*
 * stream.c
 *
 * Copyright (C) 2012-2014 Aerospike, Inc.
 *
 * Portions may be licensed to Aerospike, Inc. under one or more contributor
 * license agreements.
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see http://www.gnu.org/licenses/
 */
/*
 * This file implements stream parsing for rows.
 */

#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "ai_obj.h"
#include "bt.h"
#include "find.h"
#include "stream.h"

#include <citrusleaf/alloc.h>

extern r_tbl_t *Tbl;
extern r_ind_t *Index;

#define TWO_POW_7                 128
#define TWO_POW_14              16384
#define TWO_POW_29          536870912
#define TWO_POW_32         4294967296
#define TWO_POW_44     17592186044416
#define TWO_POW_59 576460752303423488

// the value 0 here is reserved for GHOST rows
#define COL_1BYTE_INT   1
#define COL_2BYTE_INT   2
#define COL_4BYTE_INT   4
#define COL_6BYTE_INT   8
#define COL_8BYTE_INT  16
#define COL_5BYTE_INT  32 /* NOTE: INT  ONLY - not used as bitmap */
#define COL_9BYTE_INT  32 /* NOTE: LONG ONLY - not used as bitmap */

void *row_malloc(bt *btr, int size) {
	return bt_malloc(btr, ((size + 7) / 8) * 8); /* round to 8-byte boundary */
}

void writeFloatCol(uchar **row, bool fflag, float fcol) {
	if (!fflag) return;
	memcpy(*row, &fcol, 4);
	*row  = *row + 4;
}
float streamFloatToFloat(uchar *data, uint32 *clen) {
	float val;
	if (clen) *clen  = 4;
	memcpy(&val, data, 4);
	return val;
}

// LRU LRU LRU LRU LRU LRU LRU LRU LRU LRU LRU LRU LRU LRU LRU LRU LRU LRU
inline uchar getLruSflag() {
	return COL_4BYTE_INT;
}
inline int cLRUcol(ulong l, uchar *sflag, ulong *col) { // updateLRU (UPDATE_1)
	*sflag = getLruSflag();
	*col = (l * 8) + 4;
	return 4; /* COL_4BYTE_INT */
}
inline uint32 streamLRUToUInt(uchar *data) {
	ulong val = (*(uint32 *)data);
	val -= 4;
	val /= 8;
	return val;                       /* COL_4BYTE_INT */
}
inline void overwriteLRUcol(uchar *row, ulong icol) {
	icol = (icol * 8) + 4;
	memcpy(row, &icol, 4);         /* COL_4BYTE_INT */
}

// LFU LFU LFU LFU LFU LFU LFU LFU LFU LFU LFU LFU LFU LFU LFU LFU LFU
inline uchar getLfuSflag() {
	return COL_8BYTE_INT;
}
inline int cLFUcol(ulong l, uchar *sflag, ulong *col) { // updateLFU (UPDATE_1)
	*sflag = getLfuSflag();
	*col = (l * 32) + 16;
	return 8; /* COL_8BYTE_INT */
}
inline ulong streamLFUToULong(uchar *data) {
	ulong val = (*(uint32 *)data);
	val -= 16;
	val /= 32;
	return val;                       /* COL_8BYTE_INT */
}
inline void overwriteLFUcol(uchar *row, ulong icol) {
	icol = (icol * 32) + 16;
	memcpy(row, &icol, 8);         /* COL_8BYTE_INT */
}

// INT+LONG INT+LONG INT+LONG INT+LONG INT+LONG INT+LONG INT+LONG INT+LONG
int getCSize(ulong l, bool isi) {
	if          (l < TWO_POW_7)  return 1;
	else if     (l < TWO_POW_14) return 2;
	else if     (l < TWO_POW_29) return 4;
	else {
		if      (isi)            return 5;
		else if (l < TWO_POW_44) return 6;
		else if (l < TWO_POW_59) return 8;
		else                     return 9;
	}
}

int cIcol(ulong l, uchar *sflag, ulong *col, bool isi) { // 0 -> GHOST row
	if        (l < TWO_POW_7) {
		if (sflag) *sflag = COL_1BYTE_INT;
		*col = (l * 2) + 1;
		return 1;
	} else if (l < TWO_POW_14) {
		if (sflag) *sflag = COL_2BYTE_INT;
		*col = (l * 4) + 2;
		return 2;
	} else if (l < TWO_POW_29) {
		if (sflag) *sflag = COL_4BYTE_INT;
		*col = (l * 8) + 4;
		return 4;
	} else {
		if (isi) {                                                   /* INT */
			if (sflag) *sflag = COL_5BYTE_INT;
			*col = l;
			return 5;
		} else if (l < TWO_POW_44) {                                /* LONG */
			if (sflag) *sflag = COL_6BYTE_INT;
			*col = (l * 16) + 8;
			return 6;
		} else if (l < TWO_POW_59) {
			if (sflag) *sflag = COL_8BYTE_INT;
			*col = (l * 32) + 16;
			return 8;
		} else {
			if (sflag) *sflag = COL_9BYTE_INT;
			*col = l;
			return 9;
		}
	}
}

#define INCR(x)     {x = x + 1;}
#define INCRBY(x,y) {x = x + y;}

static void wUCol(uchar **row, uchar sflag, ulong icol, bool isi) {
	if (!sflag) return;
	if        (sflag == COL_1BYTE_INT) {
		**row = (char)icol;
		INCR(*row);
	} else if (sflag == COL_2BYTE_INT) {
		memcpy(*row, &icol, 2);
		INCRBY(*row, 2);
	} else if (sflag == COL_4BYTE_INT) {
		memcpy(*row, &icol, 4);
		INCRBY(*row, 4);
	} else {
		if (isi) {                                   /* INT */
			**row = COL_5BYTE_INT;
			INCR(*row);
			memcpy(*row, &icol, 4);
			INCRBY(*row, 4);
		} else if (sflag == COL_6BYTE_INT) {         /* LONG */
			memcpy(*row, &icol, 6);
			INCRBY(*row, 6);
		} else if (sflag == COL_8BYTE_INT) {
			memcpy(*row, &icol, 8);
			INCRBY(*row, 8);
		} else {
			**row = COL_9BYTE_INT;
			INCR(*row);
			memcpy(*row, &icol, 8);
			INCRBY(*row, 8);
		}
	}
}
static ulong sI2I(uchar *data, uint32 *clen, bool isi) {
	ulong  val = 0;
	uchar  b1  = *data;
	if (b1 & COL_1BYTE_INT) {
		if (clen) *clen  = 1;
		val = (*(uchar *)data);
		val -= 1;
		val /= 2;
	} else if (b1 & COL_2BYTE_INT) {
		if (clen) *clen  = 2;
		val = (*(ushort16 *)data);
		val -= 2;
		val /= 4;
	} else if (b1 & COL_4BYTE_INT) {
		if (clen) *clen  = 4;
		val = (*(uint32 *)data);
		val -= 4;
		val /= 8;
	} else {
		if (isi) { /* INT -> COL_5BYTE_INT */
			if (clen) *clen  = 5;
			data++;
			val = (*(uint32 *)data);
		} else if (b1 & COL_6BYTE_INT) {
			if (clen) *clen  = 6;
			memcpy(&val, data, 6);
			val -= 8;
			val /= 16;
		} else if (b1 & COL_8BYTE_INT) {
			if (clen) *clen  = 8;
			val    = (*(ulong *)data);
			val -= 16;
			val /= 32;
		} else { /* LONG -> COL_9BYTE_INT */
			if (clen) *clen  = 9;
			data++;
			val = (*(ulong *)data);
		}
	}
	return val;
}

int cr8Icol(ulong l, uchar *sflag, ulong *col) {
	return cIcol(l, sflag, col, 1);
}
int cr8Lcol(ulong l, uchar *sflag, ulong *col) {
	return cIcol(l, sflag, col, 0);
}

void writeUIntCol(uchar **row,  uchar sflag, ulong icol) {
	wUCol(row, sflag, icol, 1);
}
void writeULongCol(uchar **row, uchar sflag, ulong icol) {
	wUCol(row, sflag, icol, 0);
}
uint32 streamIntToUInt(uchar *data,   uint32 *clen) {
	return (uint32)sI2I(data, clen, 1);
}
ulong  streamLongToULong(uchar *data, uint32 *clen) {
	return sI2I(data, clen, 0);
}

// STREAM_U128_COL STREAM_U128_COL STREAM_U128_COL STREAM_U128_COL
//TODO U128's can be packed as 2 StreamUlongs - probably not needed
void writeU128Col(uchar **row, uint128 xcol) {
	memcpy(*row, &xcol, 16);
	INCRBY(*row, 16);
}

uint128 streamToU128(uchar *data, uint32 *clen) {
	if (clen) *clen  = 16;
	uint128 val = (*(uint128 *)data);
	return val;
}
int cr8Xcol(uint128 x, uint128 *col) {
	*col = x;
	return 16;
}

// U160 U160 U160 U160 U160 U160 U160 U160 U160 U160 U160 U160 U160 U160
void writeU160Col(uchar **row, uint160 ycol) {
	memcpy(*row, &ycol, AS_DIGEST_KEY_SZ);
	INCRBY(*row, AS_DIGEST_KEY_SZ);
}

uint160 streamToU160(uchar *data, uint32 *clen) {
	if (clen) *clen  = AS_DIGEST_KEY_SZ;
	uint160 val;
	memcpy(&val, data, AS_DIGEST_KEY_SZ);
	return val;
}
int cr8Ycol(uint160 y, uint160 *col) {
	*col = y;
	return AS_DIGEST_KEY_SZ;
}

/* COMPARE COMPARE COMPARE COMPARE COMPARE COMPARE COMPARE COMPARE */
// INDEX_COMP INDEX_COMP INDEX_COMP INDEX_COMP INDEX_COMP INDEX_COMP INDEX_COMP
int uintCmp(void *s1, void *s2) {
	return ulongCmp(s1, s2);
}
int ulongCmp(void *s1, void *s2) {
	ulong l1  = (ulong)s1;
	ulong l2  = (ulong)s2;
	return l1 == l2 ? 0 : (l1 > l2) ? 1 : -1;
}
int u128Cmp(void *s1, void *s2) {
	uint128 x1, x2;
	memcpy(&x1, s1, 16);
	memcpy(&x2, s2, 16);
	return x1 == x2 ? 0 : (x1 > x2) ? 1 : -1;
}
int u160Cmp(void *s1, void *s2) {
	char *p1 = (char *)s1;
	char *p2 = (char *)s2;
	uint128 x1, x2;
	memcpy(&x1, p1 + 4, 16);
	memcpy(&x2, p2 + 4, 16);
	if (x1 == x2) {
		uint32 u1;
		memcpy(&u1, p1, 4);
		uint32 u2;
		memcpy(&u2, p2, 4);
		return u1 == u2 ? 0 : (u1 > u2) ? 1 : -1;
	} else return             (x1 > x2) ? 1 : -1;
}
// OTHER_BT_COMP OTHER_BT_COMP OTHER_BT_COMP OTHER_BT_COMP OTHER_BT_COMP
static inline int UCmp(void *s1, void *s2) { //struct: first arg is UINT
	ulk  *ul1 = (ulk *)s1;
	ulk  *ul2 = (ulk *)s2;
	long  l1  = ul1->key;
	long  l2  = ul2->key;
	return l1 == l2 ? 0 : (l1 > l2) ? 1 : -1;
}
int uuCmp(void *s1, void *s2) { //TODO can be done w/ bit-shifting
	return (int)(((long)s1 / UINT_MAX) - ((long)s2 / UINT_MAX));
}
int ulCmp(void *s1, void *s2) {
	return UCmp(s1, s2);
}
int uxCmp(void *s1, void *s2) {
	return UCmp(s1, s2);
}
int uyCmp(void *s1, void *s2) {
	return UCmp(s1, s2);
}
static inline int LCmp(void *s1, void *s2) { // struct: first arg is ULONG
	luk   *lu1 = (luk *)s1;
	luk   *lu2 = (luk *)s2;
	long   l1  = lu1->key;
	long   l2  = lu2->key;
	return l1 == l2 ? 0 : (l1 > l2) ? 1 : -1;
}
int luCmp(void *s1, void *s2) {
	return LCmp(s1, s2);
}
int llCmp(void *s1, void *s2) {
	return LCmp(s1, s2);
}
int lxCmp(void *s1, void *s2) {
	return LCmp(s1, s2);
}
int lyCmp(void *s1, void *s2) {
	return LCmp(s1, s2);
}
static inline int XCmp(void *s1, void *s2) { // struct: first arg is U128
	xuk     *xu1 = (xuk *)s1;
	xuk     *xu2 = (xuk *)s2;
	uint128  x1  = xu1->key;
	uint128  x2  = xu2->key;
	return x1 == x2 ? 0 : (x1 > x2) ? 1 : -1;
}
int xuCmp(void *s1, void *s2) {
	return XCmp(s1, s2);
}
int xlCmp(void *s1, void *s2) {
	return XCmp(s1, s2);
}
int xxCmp(void *s1, void *s2) {
	return XCmp(s1, s2);
}
int xyCmp(void *s1, void *s2) {
	return XCmp(s1, s2);
}
static inline int YCmp(void *s1, void *s2) {
	yuk     *yu1 = (yuk *)s1;
	yuk     *yu2 = (yuk *)s2;
	uint160  y1  = yu1->key;
	uint160  y2  = yu2->key;
	return u160Cmp(&y1, &y2);
}
int yuCmp(void *s1, void *s2) {
	return YCmp(s1, s2);
}
int ylCmp(void *s1, void *s2) {
	return YCmp(s1, s2);
}
int yxCmp(void *s1, void *s2) {
	return YCmp(s1, s2);
}
int yyCmp(void *s1, void *s2) {
	return YCmp(s1, s2);
}

// PK_COMP PK_COMP PK_COMP PK_COMP PK_COMP PK_COMP PK_COMP PK_COMP PK_COMP
static inline uchar getSflag(uchar b1) {
	return (b1 & 1) ? 1 : 0;
}
static inline uchar *getTString(uchar *s, uint32 *slen) {
	*slen = ((uchar) * s / 2);
	s++;
	return s;
}
static inline uchar *getString(uchar *s, uint32 *slen) {
	*slen = *((uint32 *)s) / 2;
	s += 4;
	return s;
}

static bool cr8BTKInt(ai_obj *akey, uint32 *ksize, uchar *btkey) {
	uchar sflag;
	ulong l = (ulong)akey->i;
	*ksize  = cr8Icol(l, &sflag, &l);
	if (l >= TWO_POW_32) return 0;
	writeUIntCol(&btkey, sflag, l);
	return 1;
}
static void cr8BTKLong(ai_obj *akey, uint32 *ksize, uchar *btkey) {
	uchar sflag;
	ulong l = akey->l;
	*ksize  = cr8Lcol(l, &sflag, &l);
	writeULongCol(&btkey, sflag, l);
}
static void cr8BTKU128(ai_obj *akey, uint32 *ksize, uchar *btkey) {
	uint128 x = akey->x;
	*ksize = cr8Xcol(x, &x);
	writeU128Col(&btkey, x);
}
static void cr8BTKU160(ai_obj *akey, uint32 *ksize, uchar *btkey) {
	uint160 y = akey->y;
	*ksize = cr8Ycol(y, &y);
	writeU160Col(&btkey, y);
}
static void cr8BTKFloat(ai_obj *akey, uint32 *ksize, uchar *btkey) {
	writeFloatCol(&btkey, 1, akey->f);
	*ksize = 4;
}
int btIntCmp(void *a, void *b) {                        //printf("btIntCmp\n");
	uint32 key1 = streamIntToUInt(a, NULL);
	uint32 key2 = streamIntToUInt(b, NULL);
	return key1 == key2 ? 0 : (key1 > key2) ? 1 : -1;
}
int btLongCmp(void *a, void *b) {                      //printf("btLongCmp\n");
	ulong key1 = streamLongToULong(a, NULL);
	ulong key2 = streamLongToULong(b, NULL);
	return key1 == key2 ? 0 : (key1 > key2) ? 1 : -1;
}
int btU128Cmp(void *a, void *b) {                      //printf("btU128Cmp\n");
	uint128  x1 = *((uint128 *)a);
	uint128  x2 = *((uint128 *)b);
	return x1 == x2 ? 0 : (x1 > x2) ? 1 : -1;
}
int btU160Cmp(void *a, void *b) {                      //printf("btU160Cmp\n");
	return u160Cmp(a, b);
}
int btFloatCmp(void *a, void *b) {                    //printf("btFloatCmp\n");
	float key1 = streamFloatToFloat(a, NULL);
	float key2 = streamFloatToFloat(b, NULL);
	float f    = key1 - key2;
	return (f == 0.0) ? 0 : ((f > 0.0) ? 1 : -1);
}
int btTextCmp(void *a, void *b) {                      //printf("btTextCmp\n");
	uint32 slen1, slen2;
	uchar  *s1     = (uchar *)a;
	uchar  *s2     = (uchar *)b;
	s1 = (getSflag(*s1)) ? getTString(s1, &slen1) : getString( s1, &slen1);
	s2 = (getSflag(*s2)) ? getTString(s2, &slen2) : getString( s2, &slen2);
	if (slen1 == slen2) return strncmp((char *)s1, (char *)s2, slen1);
	else {
		int i   = (slen1 < slen2) ? slen1 : slen2;
		int ret = strncmp((char *)s1, (char *)s2, i);
		return (ret == 0) ? ((slen1 < slen2) ? -1 : 1) : ret;
	}
}

void destroyBTKey(char *btkey, bool med) {
	if (med) cf_free(btkey);/* FREED 033 */
}

#define OBT_CR8_BTK(btkeyptr, ai_objpart)            \
  { btkeyptr.key = akey->ai_objpart; return (char *)&btkeyptr; }

char *createBTKey(ai_obj *akey, bool *med, uint32 *ksize, bt *btr, btk_t *btk) {
	*med   = 0;
	*ksize = VOIDSIZE;
	//printf("createBTKey: btr: %p ", btr); DEBUG_BT_TYPE(printf, btr);
	if      INODE_I(btr) return (char *) (long) akey->i;
	else if INODE_L(btr) return (char *)        akey->l;
	else if INODE_X(btr) return (char *)       &akey->x;// 2 big -> pass ref
	else if INODE_Y(btr) return (char *)       &akey->y;// 2 big -> pass ref
	else if UU     (btr) return (char *)((long)akey->i * UINT_MAX);
	else if UL     (btr) OBT_CR8_BTK(btk->UL, i)
	else if UX     (btr) OBT_CR8_BTK(btk->UX, i)
	else if UY     (btr) OBT_CR8_BTK(btk->UY, i) //TODO too many IF's
	else if LU     (btr) OBT_CR8_BTK(btk->LU, l)
	else if LL     (btr) OBT_CR8_BTK(btk->LL, l)
	else if LX     (btr) OBT_CR8_BTK(btk->LX, l)
	else if LY     (btr) OBT_CR8_BTK(btk->LY, l)
	else if XU     (btr) OBT_CR8_BTK(btk->XU, x)
	else if XL     (btr) OBT_CR8_BTK(btk->XL, x)
	else if XX     (btr) OBT_CR8_BTK(btk->XX, x)
	else if XY     (btr) OBT_CR8_BTK(btk->XY, x)
	else if YU     (btr) OBT_CR8_BTK(btk->YU, y)
	else if YL     (btr) OBT_CR8_BTK(btk->YL, y)
	else if YX     (btr) OBT_CR8_BTK(btk->YX, y)
	else if YY     (btr) OBT_CR8_BTK(btk->YY, y)
	
int     ktype = btr->s.ktype;
	uchar  *btkey = btk->btkeybuffer;
	if        (C_IS_S(ktype)) {
		uchar *key;
		if (akey->len < TWO_POW_7) { /* tiny STRING */
			*ksize     = akey->len + 1;
			if (*ksize >= BTK_BSIZE) {
				*med = 1;    //F033
				btkey = cf_malloc(*ksize);
			}
			*btkey     = (char)(uint32)(akey->len * 2 + 1); /* KLEN b(1)*/
			key        = btkey + 1;
		} else {                     /* STRING */
			*ksize     = akey->len + 4;
			if (*ksize >= BTK_BSIZE) {
				*med = 1;    //F033
				btkey = cf_malloc(*ksize);
			}
			uint32 len = (uint32)(akey->len * 2);           /* KLEN b(0)*/
			memcpy(btkey, &len, 4);
			key        = btkey + 4;
		}
		memcpy(key, akey->s, akey->len); /* after LEN, copy raw STRING */
	} else if (C_IS_L(ktype))        cr8BTKLong (akey, ksize, btkey);
	else if (C_IS_X(ktype))        cr8BTKU128 (akey, ksize, btkey);
	else if (C_IS_Y(ktype))        cr8BTKU160 (akey, ksize, btkey);
	else if (C_IS_F(ktype))        cr8BTKFloat(akey, ksize, btkey);
	else if (C_IS_I(ktype)) {
		if (!cr8BTKInt(akey, ksize, btkey)) return NULL;
	}
	return (char *)btkey;
}
static uint32 skipToVal(uchar **stream, uchar ktype) { //printf("skipToVal\n");
	uint32  klen  = 0;
	if      (C_IS_I(ktype)) streamIntToUInt   (*stream, &klen);
	else if (C_IS_L(ktype)) streamLongToULong (*stream, &klen);
	else if (C_IS_X(ktype)) streamToU128      (*stream, &klen);
	else if (C_IS_Y(ktype)) streamToU160      (*stream, &klen);
	else if (C_IS_F(ktype)) streamFloatToFloat(*stream, &klen);
	else {
		if (getSflag(**stream)) {
			getTString(*stream, &klen);
			klen++;
		}
		else                    {
			getString (*stream, &klen);
			klen += 4;
		}
	}
	*stream += klen;
	return klen;
}

#define DEBUG_PARSE_STREAM                                              \
  if (!server.loading) printf("parseStream: %p btr: %p ", stream, btr); \
  DEBUG_BT_TYPE(printf, btr);

uchar *parseStream(uchar *stream, bt *btr) {               //DEBUG_PARSE_STREAM
	if     (!stream || INODE(btr)) return NULL; //TODO too many IF's
	else if UU      (btr)          return (uchar *)((long)stream % UINT_MAX);
	else if UP      (btr)          return (uchar *)      (*(ulk *)(stream)).val;
	else if LUP     (btr)          return (uchar *)(long)(*(luk *)(stream)).val;
	else if LLP     (btr)          return (uchar *)      (*(llk *)(stream)).val;
	else if XUP     (btr)          return (uchar *)(long)(*(xuk *)(stream)).val;
	else if XLP     (btr)          return (uchar *)      (*(xlk *)(stream)).val;
	//TODO should XXP use xLk
	else if XXP     (btr)          return (uchar *)(long)(*(xxk *)(stream)).val;
	else if YLP     (btr)          return (uchar *)      (*(ylk *)(stream)).val;
	else if YYP     (btr)          return (uchar *)(long)(*(ylk *)(stream)).val;
	else if OTHER_BT(btr)          return stream;
	skipToVal(&stream, btr->s.ktype);
	if      (btr->s.btype == BTREE_TABLE)   return stream;
	else if (btr->s.btype == BTREE_INODE)   return NULL;
	else                  /* BTREE_INDEX */ return *((uchar **)stream);
}
#define OBT_CONV2STREAM(t, ai_objpart, cast) \
  {  key->type = key->enc = t;  key->ai_objpart = (*(cast *)(stream)).key; }

void convertStream2Key(uchar *stream, ai_obj *key, bt *btr) {
	//printf("convertStream2Key\n"); DEBUG_BT_TYPE(printf, btr);
	init_ai_obj(key);
	key->empty = 0;
	if        INODE_I(btr) {
		key->type = key->enc = COL_TYPE_INT;
		key->i    = INTVOID stream;
	} else if INODE_L(btr) {
		key->type = key->enc = COL_TYPE_LONG;
		key->l    = (ulong)stream;
	} else if INODE_X(btr) {
		key->type = key->enc = COL_TYPE_U128;
		memcpy(&key->x, stream, 16);
	} else if INODE_Y(btr) {
		key->type = key->enc = COL_TYPE_U160;
		memcpy(&key->y, stream, 20);
	} else if UU     (btr) {
		key->type = key->enc = COL_TYPE_INT;
		key->i    = (uint32)((long)stream / UINT_MAX);
	} else if UL     (btr) OBT_CONV2STREAM(COL_TYPE_INT,  i, ulk)
		else if UX     (btr) OBT_CONV2STREAM(COL_TYPE_INT,  i, uxk)
		else if UY     (btr) OBT_CONV2STREAM(COL_TYPE_INT,  i, uyk)//TODO
		else if LU     (btr) OBT_CONV2STREAM(COL_TYPE_LONG, l, luk)//too many IF's
		else if LL     (btr) OBT_CONV2STREAM(COL_TYPE_LONG, l, llk)
		else if LX     (btr) OBT_CONV2STREAM(COL_TYPE_LONG, l, lxk)
		else if LY     (btr) OBT_CONV2STREAM(COL_TYPE_LONG, l, lyk)
		else if XU     (btr) OBT_CONV2STREAM(COL_TYPE_U128, x, xuk)
		else if XL     (btr) OBT_CONV2STREAM(COL_TYPE_U128, x, xlk)
		else if XX     (btr) OBT_CONV2STREAM(COL_TYPE_U128, x, xxk)
		else if XY     (btr) OBT_CONV2STREAM(COL_TYPE_U128, x, xyk)
		else if YU     (btr) OBT_CONV2STREAM(COL_TYPE_U160, y, yuk)
		else if YL     (btr) OBT_CONV2STREAM(COL_TYPE_U160, y, ylk)
		else if YX     (btr) OBT_CONV2STREAM(COL_TYPE_U160, y, yxk)
		else if YY     (btr) OBT_CONV2STREAM(COL_TYPE_U160, y, yyk)
		else { /* NORM_BT */
			int ktype = btr->s.ktype;
			if        (C_IS_I(ktype)) {
				key->type = key->enc = COL_TYPE_INT;
				key->i    = streamIntToUInt(stream, NULL);
			} else if (C_IS_L(ktype)) {
				key->type = key->enc = COL_TYPE_LONG;
				key->l    = streamLongToULong(stream, NULL);
			} else if (C_IS_X(ktype)) {
				key->type = key->enc = COL_TYPE_U128;
				key->x    = streamToU128(stream, NULL);
			} else if (C_IS_Y(ktype)) {
				key->type = key->enc = COL_TYPE_U160;
				key->y    = streamToU160(stream, NULL);
			} else if (C_IS_F(ktype)) {
				key->type = key->enc = COL_TYPE_FLOAT;
				key->f    = streamFloatToFloat(stream, NULL);
			} else { /* COL_TYPE_STRING */
				if (getSflag(*stream)) {  // tiny STRING
					key->type = key->enc = COL_TYPE_STRING;
					key->s    = (char *)getTString(stream, &key->len);
				} else {                  // STRING
					key->type = key->enc = COL_TYPE_STRING;
					key->s    = (char *)getString(stream, &key->len);
				}
			}
		}
}
#define DEBUG_CREATE_STREAM                                      \
  printf("createStream: size: %u klen: %d vlen: %d btype: %d\n", \
          *size, klen, vlen, btr->s.btype);
#define DEBUG_DESTROY_STREAM \
  printf("destroyStream: size: %u btr: %p\n", size, btr);

// **************** Originally from "row.c" *******************
#define RFLAG_1BYTE_INT   1
#define RFLAG_2BYTE_INT   2
#define RFLAG_4BYTE_INT   4
#define RFLAG_SIZE_FLAG (RFLAG_1BYTE_INT + RFLAG_2BYTE_INT + RFLAG_4BYTE_INT)

#define RFLAG_HASH16_ROW 32
#define RFLAG_HASH32_ROW 64
#define RFLAG_HASH_ROW (RFLAG_HASH16_ROW + RFLAG_HASH32_ROW)

typedef struct ahash32_entry { // 16 BYTES
	uint32 n_colijns; /* num_collisions now, always <= max_colijns */
	uint32 key;
	ulong  val;
} ahash32_entry;

typedef struct ahash32 {
	uint32         nvalid;
	uint32         nentries;
	uint32         max_colijns;
	ahash32_entry *entries;
} ahash32;

// NOTE: hash16 is a SHAMELESS COPY hash32
typedef struct ahash16_entry { // 7 BYTES
	uchar  n_colijns; /* num_collisions now, always <= max_colijns */
	ushort16 key;
	uint32 val;
} __attribute__ ((packed)) ahash16_entry;

typedef struct ahash16 {
	uint32         nvalid;
	uint32         nentries;
	uint32         max_colijns;
	ahash16_entry *entries;
} ahash16;

uint32 ai_hash16_size(ahash16 *ht) {
	return (ht->nentries * sizeof(ahash16_entry)) + sizeof(ahash16);
}

uint32 ai_hash32_size(ahash32 *ht) {
	return (ht->nentries * sizeof(ahash32_entry)) + sizeof(ahash32);
}

static uchar *getRowPayload(uchar  *row,   uchar  *rflag,
							uint32 *ncols, uint32 *rlen) {
	uint32_t clen;
	uchar *o_row = row;
	*rflag       = *row;
	row++;       // GET rflag
	if (*rflag & RFLAG_HASH_ROW) {
		*rlen       = streamIntToUInt(row, &clen);
		row += clen; // GET rlen
		*ncols      = streamIntToUInt(row, &clen);
		row += clen; // GET ncols
		if (*rflag & RFLAG_HASH32_ROW) {
			ahash32 *ht = (ahash32 *)row;
			/* SKIP HASH32_TABLE */    row += ai_hash32_size(ht);
		} else { // RFLAG_HASH16_ROW
			ahash16 *ht = (ahash16 *)row;
			/* SKIP HASH16_TABLE */    row += ai_hash16_size(ht);
		}
	} else {
		char sflag = *rflag & RFLAG_SIZE_FLAG;
		*ncols     = streamIntToUInt(row, &clen);
		row += clen; // GET ncols
		/* SKIP cofsts */          row += (*ncols * sflag);
		if        (*rflag & RFLAG_1BYTE_INT) { // rlen = final cofst[FINAL]
			*rlen = (uint32) * ((uchar*)row - 1);
		} else if (*rflag & RFLAG_2BYTE_INT) {
			*rlen = (uint32)(*((unsigned short *)((uchar *)(row - 2))));
		} else {         /* RFLAG_4BYTE_INT */
			*rlen = (uint32)(*((uint32 *)        ((uchar *)(row - 4))));
		}
	}
	uint32  mlen = row - o_row;
	*rlen        = *rlen + mlen;
	return row;
}
uint32 getRowMallocSize(uchar *stream) { // used in stream.c also
	if (!stream) return sizeof(void *); // NULL will be stored IN-stream
	uchar rflag;
	uint32 rlen;
	uint32 ncols;
	getRowPayload(stream, &rflag, &ncols, &rlen);
	return rlen;
}
// **************************************************************

static uint32 getStreamVlen(bt *btr, uchar *stream) {
	if      (btr->s.btype == BTREE_TABLE)   return getRowMallocSize(stream);
	else if (btr->s.btype == BTREE_INODE)   return 0;
	else                  /* BTREE_INDEX */ return sizeof(void *);
}
uint32 getStreamMallocSize(bt *btr, uchar *stream) {
	if (INODE(btr) || OTHER_BT(btr)) return 0;
	int size  = skipToVal(&stream, btr->s.ktype) + getStreamVlen(btr, stream);
	return ((size + 7) / 8) * 8; /* round to 8-byte boundary */
}
uint32 getStreamRowSize(bt *btr, uchar *stream) { /* for rdbSaveAllRows() */
	if NORM_BT(btr) {
		return skipToVal(&stream, btr->s.ktype) + getStreamVlen(btr, stream);
	} else {
		if      INODE(btr) return 0;
		//TODO, this can be an array (too many IF's)
		else if UU   (btr) return UU_SIZE;
		else if UL   (btr) return UL_SIZE;
		else if UX   (btr) return UX_SIZE;
		else if UY   (btr) return UY_SIZE;
		else if LU   (btr) return LU_SIZE;
		else if LL   (btr) return LL_SIZE;
		else if LX   (btr) return LX_SIZE;
		else if LY   (btr) return LY_SIZE;
		else if XU   (btr) return XU_SIZE;
		else if XL   (btr) return XL_SIZE;
		else if XX   (btr) return XX_SIZE;
		else if XY   (btr) return XY_SIZE;
		else if YU   (btr) return YU_SIZE;
		else if YL   (btr) return YL_SIZE;
		else if YX   (btr) return YX_SIZE;
		else if YY   (btr) return YY_SIZE;
		assert(!"getStreamRowSize ERROR");
		return -1;
	}
}

#define OBT_CR8_STRM(tcast, sptr, vcast)        \
  { tcast *ul = (tcast *)btkey;                 \
    sptr.key  = ul->key; sptr.val = (vcast)val; \
    return &sptr; }
#define XOBT_CR8_STRM(tcast, sptr)                       \
  { tcast *ux = (tcast *)btkey;                          \
    sptr.key  = ux->key; sptr.val = ((tcast *)val)->val; \
    return &sptr; }

static void *OBT_createStream(bt *btr, void *val, char *btkey, crs_t *crs) {
    //printf("OBT_createStream\n"); DEBUG_BT_TYPE(printf, btr);
    if (OBYI(btr) || MCI_UNIQ(btr)) {
        if      UU(btr) return (void *)((long)btkey + (long)val); /* merge */
        else if UL(btr) XOBT_CR8_STRM(ulk, crs->UL_StreamPtr)
        else if UX(btr) XOBT_CR8_STRM(uxk, crs->UX_StreamPtr)
        else if UY(btr) XOBT_CR8_STRM(uyk, crs->UY_StreamPtr) //TODO
        else if LU(btr) XOBT_CR8_STRM(luk, crs->LU_StreamPtr) //  too many IF's
        else if LL(btr) XOBT_CR8_STRM(llk, crs->LL_StreamPtr)
        else if LX(btr) XOBT_CR8_STRM(lxk, crs->LX_StreamPtr)
        else if LY(btr) XOBT_CR8_STRM(lyk, crs->LY_StreamPtr)
        else if XU(btr) XOBT_CR8_STRM(xuk, crs->XU_StreamPtr)
        else if XL(btr) XOBT_CR8_STRM(xlk, crs->XL_StreamPtr)
        else if XX(btr) XOBT_CR8_STRM(xxk, crs->XX_StreamPtr)
        else if XY(btr) XOBT_CR8_STRM(xyk, crs->XY_StreamPtr)
        else if YU(btr) XOBT_CR8_STRM(yuk, crs->YU_StreamPtr)
        else if YL(btr) XOBT_CR8_STRM(ylk, crs->YL_StreamPtr)
        else if YX(btr) XOBT_CR8_STRM(yxk, crs->YX_StreamPtr)
        else if YY(btr) XOBT_CR8_STRM(yyk, crs->YY_StreamPtr)
        assert(!"OBT_createStream OBYI error"); return NULL;
    }
    if       UP(btr) OBT_CR8_STRM (ulk, crs->UL_StreamPtr, ulong)
	else if LUP(btr) OBT_CR8_STRM (luk, crs->LU_StreamPtr, /* Actually: uint32 */ ulong)
    else if LLP(btr) OBT_CR8_STRM (llk, crs->LL_StreamPtr, ulong)

    else if UU(btr) return (void *)((long)btkey + (long)val); /* merge */
    else if UL(btr) XOBT_CR8_STRM(ulk, crs->UL_StreamPtr)
    else if UX(btr) XOBT_CR8_STRM(uxk, crs->UX_StreamPtr)
    else if UY(btr) XOBT_CR8_STRM(uyk, crs->UY_StreamPtr) //TODO too many IF's
    else if LU(btr) XOBT_CR8_STRM(luk, crs->LU_StreamPtr)
    else if LL(btr) XOBT_CR8_STRM(llk, crs->LL_StreamPtr)
    else if LX(btr) XOBT_CR8_STRM(lxk, crs->LX_StreamPtr)
    else if LY(btr) XOBT_CR8_STRM(lyk, crs->LY_StreamPtr)
    else if XU(btr) XOBT_CR8_STRM(xuk, crs->XU_StreamPtr)
    else if XX(btr) XOBT_CR8_STRM(xxk, crs->XX_StreamPtr)
    else if XY(btr) XOBT_CR8_STRM(xyk, crs->XY_StreamPtr)
    else if YU(btr) XOBT_CR8_STRM(yuk, crs->YU_StreamPtr)
    else if YX(btr) XOBT_CR8_STRM(yxk, crs->YX_StreamPtr)
    else if YY(btr) XOBT_CR8_STRM(yyk, crs->YY_StreamPtr)
    else if XL(btr) { //NOTE: XL is special it can be XXP()
        xlk *xl               = (xlk *)btkey;
        crs->XL_StreamPtr.key = xl->key;
        crs->XL_StreamPtr.val = XXP(btr) ? (ulong)val : ((xlk *)val)->val;
        return &crs->XL_StreamPtr;
    } else if YL(btr) { //NOTE: YL is special it can be YYP()
        ylk *yl               = (ylk *)btkey;
        crs->YL_StreamPtr.key = yl->key;
        crs->YL_StreamPtr.val = YYP(btr) ? (ulong)val : ((ylk *)val)->val;
        return &crs->YL_StreamPtr;
    } else { assert(!"OBT_createStream ERROR"); return NULL; }
}
// btkey from createBTKey() - bit-packed-num or string-w-length
// row from writeRow() - [normal|hash]_row of [bit-packed-num|string-w-length]
// NOTE: rows (but NOT btkeys) can have compressed strings
// STREAM BINARY FORMAT: [btkey|row]
void *createStream(bt *btr, void *val, char *btkey, uint32 klen, uint32 *size,
				   crs_t *crs) {
	*size = 0;                                           // DEBUG_BT_TYPE(btr);
	if      INODE(btr)    return btkey;
	else if OTHER_BT(btr) return OBT_createStream(btr, val, btkey, crs);
	uint32  vlen      = getStreamVlen(btr, val);
	*size             = klen + vlen;                      //DEBUG_CREATE_STREAM
	char   *bt_val    = (btr->s.btype == BTREE_TABLE) ? row_malloc(btr, *size) :
						bt_malloc (btr, *size);
	char   *o_bt_val  = bt_val;
	memcpy(bt_val, btkey, klen);
	bt_val           += klen;
	uchar btype = btr->s.btype;
	if        (btype == BTREE_TABLE) {
		if (val) memcpy(bt_val, val, vlen);
		else     bzero (bt_val, sizeof(void *));
	} else if (btype != BTREE_INODE) memcpy(bt_val, &val, sizeof(void *));
	return o_bt_val; /* line above is for STRING & FLOAT INDEX */
}
bool destroyStream(bt *btr, uchar *ostream) {
	if (!ostream || INODE(btr) || OTHER_BT(btr)) return 0;
	uint32 size  = getStreamMallocSize(btr, ostream);    //DEBUG_DESTROY_STREAM
	bt_free(btr, ostream, size); /* mem-bookkeeping in ibtr */
	return 1;
}
