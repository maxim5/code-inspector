/* Implementation of the hash table lisp object type.
   Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995, 1996, 2002, 2004, 2010 Ben Wing.
   Copyright (C) 1997 Free Software Foundation, Inc.

This file is part of XEmacs.

XEmacs is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs.  If not, see <http://www.gnu.org/licenses/>. */

/* Synched up with: Not in FSF. */

/* Author: Lost in the mists of history.  At least back to Lucid 19.3,
   circa Sep 1992.  Early hash table implementation allowed only `eq' as a
   test -- other tests possible only when these objects were created from
   the C code.

   Expansion to allow general `equal'-test Lisp-creatable tables, and hash
   methods for the various Lisp objects in existence at the time, added
   during 19.12 I think (early 1995?), by Ben Wing.

   Weak hash tables added by Jamie (maybe?) early on, perhaps around 19.6,
   maybe earlier; again, only possible through the C code, and only
   supported fully weak hash tables.  Expansion to other kinds of weakness,
   and exporting of the interface to Lisp, by Ben Wing during 19.12
   (early-mid 1995) or maybe 19.13 cycle (mid 1995).

   Expansion to full Common Lisp spec and interface, redoing of the
   implementation, by Martin Buchholz, 1997? (Former hash table
   implementation used "double hashing", I'm pretty sure, and was weirdly
   tied into the generic hash.c code.  Martin completely separated them.)
*/

/* This file implements the hash table lisp object type.

   This implementation was mostly written by Martin Buchholz in 1997.

   The Lisp-level API (derived from Common Lisp) is almost completely
   compatible with GNU Emacs 21, even though the implementations are
   totally independent.

   The hash table technique used is "linear probing".  Collisions are
   resolved by putting the item in the next empty place in the array
   following the collision.  Finding a hash entry performs a linear
   search in the cluster starting at the hash value.

   On deletions from the hash table, the entries immediately following
   the deleted entry are re-entered in the hash table.  We do not have
   a special way to mark deleted entries (known as "tombstones").

   At the end of the hash entries ("hentries"), we leave room for an
   entry that is always empty (the "sentinel").

   The traditional literature on hash table implementation
   (e.g. Knuth) suggests that too much "primary clustering" occurs
   with linear probing.  However, this literature was written when
   locality of reference was not a factor.  The discrepancy between
   CPU speeds and memory speeds is increasing, and the speed of access
   to memory is highly dependent on memory caches which work best when
   there is high locality of data reference.  Random access to memory
   is up to 20 times as expensive as access to the nearest address
   (and getting worse).  So linear probing makes sense.

   But the representation doesn't actually matter that much with the
   current elisp engine.  Funcall is sufficiently slow that the choice
   of hash table implementation is noise.  */

#include <config.h>
#include "lisp.h"
#include "bytecode.h"
#include "elhash.h"
#include "gc.h"
#include "opaque.h"
#include "buffer.h"

Lisp_Object Qhash_tablep;
Lisp_Object Qeq, Qeql, Qequal, Qequalp;
Lisp_Object Qeq_hash, Qeql_hash, Qequal_hash, Qequalp_hash;

static Lisp_Object Qhashtable, Qhash_table, Qmake_hash_table;
static Lisp_Object Qweakness, Qvalue, Qkey_or_value, Qkey_and_value;
static Lisp_Object Vall_weak_hash_tables;
static Lisp_Object Qrehash_size, Qrehash_threshold;
static Lisp_Object Q_size, Q_weakness, Q_rehash_size, Q_rehash_threshold;
static Lisp_Object Vhash_table_test_eq, Vhash_table_test_eql;
static Lisp_Object Vhash_table_test_weak_list;

/* obsolete as of 19990901 in xemacs-21.2 */
static Lisp_Object Qweak, Qkey_weak, Qvalue_weak, Qkey_or_value_weak;
static Lisp_Object Qnon_weak;

/* A hash table test, with its associated hash function. equal_function may
   call lisp_equal_function, and hash_function similarly may call
   lisp_hash_function. */
struct Hash_Table_Test
{
  NORMAL_LISP_OBJECT_HEADER header;
  Lisp_Object name;
  hash_table_equal_function_t equal_function;
  hash_table_hash_function_t hash_function;
  Lisp_Object lisp_equal_function;
  Lisp_Object lisp_hash_function;
};

static Lisp_Object
mark_hash_table_test (Lisp_Object obj)
{
  Hash_Table_Test *http = XHASH_TABLE_TEST (obj);

  mark_object (http->name);
  mark_object (http->lisp_equal_function);
  mark_object (http->lisp_hash_function);

  return Qnil;
}

static const struct memory_description hash_table_test_description_1[] =
  {
    { XD_LISP_OBJECT, offsetof (struct Hash_Table_Test, name) },
    { XD_LISP_OBJECT, offsetof (struct Hash_Table_Test, lisp_equal_function) },
    { XD_LISP_OBJECT, offsetof (struct Hash_Table_Test, lisp_hash_function) },
    { XD_END }
  };

static const struct sized_memory_description hash_table_test_description =
  {
    sizeof (struct Hash_Table_Test),
    hash_table_test_description_1
  };

DEFINE_DUMPABLE_INTERNAL_LISP_OBJECT ("hash-table-test", hash_table_test,
				      mark_hash_table_test,
                                      hash_table_test_description_1,
                                      Hash_Table_Test);
/* A hash table. */

struct Lisp_Hash_Table
{
  NORMAL_LISP_OBJECT_HEADER header;
  Elemcount size;
  Elemcount count;
  Elemcount rehash_count;
  double rehash_size;
  double rehash_threshold;
  Elemcount golden_ratio;
  htentry *hentries;
  Lisp_Object test;
  enum hash_table_weakness weakness;
  Lisp_Object next_weak;     /* Used to chain together all of the weak
			        hash tables.  Don't mark through this. */
};

#define CLEAR_HTENTRY(htentry)   \
  ((*(EMACS_UINT*)(&((htentry)->key)))   = 0, \
   (*(EMACS_UINT*)(&((htentry)->value))) = 0)

#define HASH_TABLE_DEFAULT_SIZE 16
#define HASH_TABLE_DEFAULT_REHASH_SIZE 1.3
#define HASH_TABLE_MIN_SIZE 10
#define HASH_TABLE_DEFAULT_REHASH_THRESHOLD(size, test)   \
  (((size) > 4096 && EQ (Vhash_table_test_eq, test)) ? 0.7 : 0.6)

#define HASHCODE(key, ht, http)						\
  ((((!EQ (Vhash_table_test_eq, ht->test)) ?                            \
     (http)->hash_function (http, key) :                                \
     LISP_HASH (key)) * (ht)->golden_ratio) % (ht)->size)

#define KEYS_EQUAL_P(key1, key2, test, http)                      \
  (EQ (key1, key2) || ((!EQ (Vhash_table_test_eq, test) &&        \
                        (http->equal_function) (http, key1, key2))))

#define LINEAR_PROBING_LOOP(probe, entries, size)		\
  for (;							\
       !HTENTRY_CLEAR_P (probe) ||				\
	 (probe == entries + size ?				\
	  (probe = entries, !HTENTRY_CLEAR_P (probe)) : 0);	\
       probe++)

#ifdef ERROR_CHECK_STRUCTURES
static void
check_hash_table_invariants (Lisp_Hash_Table *ht)
{
  assert (ht->count < ht->size);
  assert (ht->count <= ht->rehash_count);
  assert (ht->rehash_count < ht->size);
  assert ((double) ht->count * ht->rehash_threshold - 1 <= (double) ht->rehash_count);
  assert (HTENTRY_CLEAR_P (ht->hentries + ht->size));
}
#else
#define check_hash_table_invariants(ht)
#endif

/* Return a suitable size for a hash table, with at least SIZE slots. */
static Elemcount
hash_table_size (Elemcount requested_size)
{
  /* Return some prime near, but greater than or equal to, SIZE.
     Decades from the time of writing, someone will have a system large
     enough that the list below will be too short... */
  static const Elemcount primes [] =
  {
    19, 29, 41, 59, 79, 107, 149, 197, 263, 347, 457, 599, 787, 1031,
    1361, 1777, 2333, 3037, 3967, 5167, 6719, 8737, 11369, 14783,
    19219, 24989, 32491, 42257, 54941, 71429, 92861, 120721, 156941,
    204047, 265271, 344857, 448321, 582821, 757693, 985003, 1280519,
    1664681, 2164111, 2813353, 3657361, 4754591, 6180989, 8035301,
    10445899, 13579681, 17653589, 22949669, 29834603, 38784989,
    50420551, 65546729, 85210757, 110774011, 144006217, 187208107,
    243370577, 316381771, 411296309, 534685237, 695090819, 903618083,
    1174703521, 1527114613, 1985248999 /* , 2580823717UL, 3355070839UL */
  };
  /* We've heard of binary search. */
  int low, high;
  for (low = 0, high = countof (primes) - 1; high - low > 1;)
    {
      /* Loop Invariant: size < primes [high] */
      int mid = (low + high) / 2;
      if (primes [mid] < requested_size)
	low = mid;
      else
	high = mid;
    }
  return primes [high];
}



static int
lisp_object_eql_equal (const Hash_Table_Test *UNUSED (http), Lisp_Object obj1,
                       Lisp_Object obj2)
{
  return EQ (obj1, obj2) ||
    (NON_FIXNUM_NUMBER_P (obj1) && internal_equal (obj1, obj2, 0));
}

static Hashcode
lisp_object_eql_hash (const Hash_Table_Test *UNUSED (http), Lisp_Object obj)
{
  return NON_FIXNUM_NUMBER_P (obj) ?
    internal_hash (obj, 0, 0) : LISP_HASH (obj);
}

static int
lisp_object_equal_equal (const Hash_Table_Test *UNUSED (http),
                         Lisp_Object obj1, Lisp_Object obj2)
{
  return internal_equal (obj1, obj2, 0);
}

static Hashcode
lisp_object_equal_hash (const Hash_Table_Test *UNUSED (http), Lisp_Object obj)
{
  return internal_hash (obj, 0, 0);
}

static Hashcode
lisp_object_equalp_hash (const Hash_Table_Test *UNUSED (http), Lisp_Object obj)
{
  return internal_hash (obj, 0, 1);
}

static int
lisp_object_equalp_equal (const Hash_Table_Test *UNUSED (http),
                          Lisp_Object obj1, Lisp_Object obj2)
{
  return internal_equalp (obj1, obj2, 0);
}

static Hashcode
lisp_object_general_hash (const Hash_Table_Test *http, Lisp_Object obj)
{
  struct gcpro gcpro1;
  Lisp_Object args[2] = { http->lisp_hash_function, obj }, res;
  
  /* Make sure any weakly referenced objects don't get collected before the
     funcall: */
  GCPRO1 (args[0]);
  gcpro1.nvars = countof (args);
  res = IGNORE_MULTIPLE_VALUES (Ffuncall (countof (args), args));
  UNGCPRO;

  if (FIXNUMP (res))
    {
      return (Hashcode) (XFIXNUM (res));
    }

#ifdef HAVE_BIGNUM
  if (BIGNUMP (res))
    {
      if (bignum_fits_emacs_int_p (XBIGNUM_DATA (res)))
        {
          return (Hashcode) bignum_to_emacs_int (XBIGNUM_DATA (res));
        }

      signal_error (Qrange_error, "Not a valid hash code", res);
    }
#endif

  dead_wrong_type_argument (Qintegerp, res);
}

static int
lisp_object_general_equal (const Hash_Table_Test *http, Lisp_Object obj1,
                           Lisp_Object obj2)
{
  struct gcpro gcpro1;
  Lisp_Object args[] = { http->lisp_equal_function, obj1, obj2 }, res;

  GCPRO1 (args[0]);
  gcpro1.nvars = countof (args);
  res = IGNORE_MULTIPLE_VALUES (Ffuncall (countof (args), args));
  UNGCPRO;

  return !(NILP (res));
}


static Lisp_Object
mark_hash_table (Lisp_Object obj)
{
  Lisp_Hash_Table *ht = XHASH_TABLE (obj);

  /* If the hash table is weak, we don't want to mark the keys and
     values (we scan over them after everything else has been marked,
     and mark or remove them as necessary).  */
  if (ht->weakness == HASH_TABLE_NON_WEAK)
    {
      htentry *e, *sentinel;

      for (e = ht->hentries, sentinel = e + ht->size; e < sentinel; e++)
	if (!HTENTRY_CLEAR_P (e))
	  {
	    mark_object (e->key);
	    mark_object (e->value);
	  }
    }

  mark_object (ht->test);

  return Qnil;
}

static int
nsubst_structures_map_hash_table (Lisp_Object key, Lisp_Object value,
                                  void *extra_arg)
{
  Lisp_Object number_table
    = ((nsubst_structures_info_t *) extra_arg)->number_table;
  Lisp_Object new_ = ((nsubst_structures_info_t *) extra_arg)->new_;
  Lisp_Object old = ((nsubst_structures_info_t *) extra_arg)->old;
  Lisp_Object hash_table
    = ((nsubst_structures_info_t *) extra_arg)->current_object;
  Boolint test_not_unboundp
    = ((nsubst_structures_info_t *) extra_arg)->test_not_unboundp;

  if (EQ (old, key) == test_not_unboundp)
    {
      Fremhash (key, hash_table);
      Fputhash (new_, value, hash_table);
    }
  else if (LRECORDP (key) &&
           HAS_OBJECT_METH_P (key, nsubst_structures_descend))
    {
      nsubst_structures_descend (new_, old, key, number_table,
                                 test_not_unboundp);
    }

  if (EQ (old, value) == test_not_unboundp)
    {
      Fputhash (key, new_, hash_table);
    }
  else if (LRECORDP (value) &&
           HAS_OBJECT_METH_P (value, nsubst_structures_descend))
    {
      nsubst_structures_descend (new_, old, value, number_table,
                                 test_not_unboundp);
    }

  return 0;
}

static void
hash_table_nsubst_structures_descend (Lisp_Object new_, Lisp_Object old,
				      Lisp_Object object,
				      Lisp_Object number_table,
				      Boolint test_not_unboundp)
{
  nsubst_structures_info_t nsubst_structures_info
    = { number_table, new_, old, object, test_not_unboundp };

  /* If we're happy with limiting nsubst_structures to use in the Lisp
     reader, we don't have to worry about the hash table test here, because
     the only point where NEW_ can be the test will be forms like so:
     #%d=#:SOME-GENSYM, in which case OLD will most definitively not include
     a hash table anywhere in its structure. */

  elisp_maphash (nsubst_structures_map_hash_table, object,
		 &nsubst_structures_info);
}

static int
print_preprocess_mapper (Lisp_Object key, Lisp_Object value, void *extra_arg)
{
  Lisp_Object print_number_table = ((preprocess_info_t *) extra_arg)->table;
  Elemcount *seen_number_count = ((preprocess_info_t *) extra_arg)->count;

  PRINT_PREPROCESS (key, print_number_table, seen_number_count);
  PRINT_PREPROCESS (value, print_number_table, seen_number_count);

  return 0;
}

static void
hash_table_print_preprocess (Lisp_Object obj, Lisp_Object number_table,
                             Elemcount *seen_object_count)
{
  preprocess_info_t preprocess_info = { number_table,
                                        seen_object_count };

  print_preprocess (XHASH_TABLE_TEST (XHASH_TABLE (obj)->test)->name,
                    number_table, seen_object_count);

  elisp_maphash_unsafe (print_preprocess_mapper, obj, &preprocess_info);
}

/* Equality of hash tables.  Two hash tables are equal when they are of
   the same weakness and test function, they have the same number of
   elements, and for each key in the hash table, the values are `equal'.

   This is similar to Common Lisp `equalp' of hash tables, with the
   difference that CL requires the keys to be compared with the test
   function, which we don't do.  Doing that would require consing, and
   consing is a bad idea in `equal'.  Anyway, our method should provide
   the same result -- if the keys are not equal according to the test
   function, then Fgethash() in hash_table_equal_mapper() will fail.  */
static int
hash_table_equal (Lisp_Object hash_table1, Lisp_Object hash_table2, int depth,
		  int foldcase)
{
  Lisp_Hash_Table *ht1 = XHASH_TABLE (hash_table1);
  Lisp_Hash_Table *ht2 = XHASH_TABLE (hash_table2);
  htentry *e, *sentinel;

  if (!(EQ (ht1->test, ht2->test)) ||
      (ht1->weakness      != ht2->weakness)   ||
      (ht1->count         != ht2->count))
    return 0;

  depth++;

  for (e = ht1->hentries, sentinel = e + ht1->size; e < sentinel; e++)
    if (!HTENTRY_CLEAR_P (e))
      /* Look up the key in the other hash table, and compare the values. */
      {
	Lisp_Object value_in_other = Fgethash (e->key, hash_table2, Qunbound);
	if (UNBOUNDP (value_in_other) ||
	    !internal_equal_0 (e->value, value_in_other, depth, foldcase))
	  return 0;		/* Give up */
      }

  return 1;
}

/* This is not a great hash function, but it _is_ correct and fast.
   Examining all entries is too expensive, and examining a random
   subset does not yield a correct hash function. */
static Hashcode
hash_table_hash (Lisp_Object hash_table, int UNUSED (depth),
                 int UNUSED (equalp))
{
  return XHASH_TABLE (hash_table)->count;
}

#ifdef MEMORY_USAGE_STATS

struct hash_table_stats
{
  struct usage_stats u;
  Bytecount hentries;
};

static void
hash_table_memory_usage (Lisp_Object hashtab,
			 struct generic_usage_stats *gustats)
{
  Lisp_Hash_Table *ht = XHASH_TABLE (hashtab);
  struct hash_table_stats *stats = (struct hash_table_stats *) gustats;
  stats->hentries +=
    malloced_storage_size (ht->hentries,
			   sizeof (htentry) * (ht->size + 1),
			   &stats->u);
}

#endif /* MEMORY_USAGE_STATS */


/* Printing hash tables.

   This is non-trivial, because we use a readable structure-style
   syntax for hash tables.  This means that a typical hash table will be
   readably printed in the form of:

   #s(hash-table :size 2 :data (key1 value1 key2 value2))

   The supported hash table structure keywords and their values are:
   `:test'             (eql (or nil), eq or equal)
   `:size'             (a natnum or nil)
   `:rehash-size'      (a float)
   `:rehash-threshold' (a float)
   `:weakness'         (nil, key, value, key-and-value, or key-or-value)
   `:data'             (a list)

   If `print-readably' is nil, then a simpler syntax is used, for example

   #<hash-table size 2/13 data (key1 value1 key2 value2) 0x874d>

   The data is truncated to four pairs, and the rest is shown with
   `...'.  This printer does not cons.  */


/* Print the data of the hash table.  This maps through a Lisp
   hash table and prints key/value pairs using PRINTCHARFUN.  */
static void
print_hash_table_data (Lisp_Hash_Table *ht, Lisp_Object printcharfun)
{
  int count = 0;
  htentry *e, *sentinel;

  write_ascstring (printcharfun, " :data (");

  for (e = ht->hentries, sentinel = e + ht->size; e < sentinel; e++)
    if (!HTENTRY_CLEAR_P (e))
      {
	if (count > 0)
	  write_ascstring (printcharfun, " ");
	if (!print_readably && count > 3)
	  {
	    write_ascstring (printcharfun, "...");
	    break;
	  }
	print_internal (e->key, printcharfun, 1);
	write_fmt_string_lisp (printcharfun, " %S", 1, e->value);
	count++;
      }

  write_ascstring (printcharfun, ")");
}

static void
print_hash_table (Lisp_Object obj, Lisp_Object printcharfun,
		  int UNUSED (escapeflag))
{
  Lisp_Hash_Table *ht = XHASH_TABLE (obj);
  Ascbyte pigbuf[350];

  write_ascstring (printcharfun,
		  print_readably ? "#s(hash-table" : "#<hash-table");

  if (!(EQ (ht->test, Vhash_table_test_eql)))
    {
      write_fmt_string_lisp (printcharfun, " :test %S",
                             1, XHASH_TABLE_TEST (ht->test)->name);
    }

  if (ht->count || !print_readably)
    {
      if (print_readably)
	write_fmt_string (printcharfun, " :size %ld", (long) ht->count);
      else
	write_fmt_string (printcharfun, " :size %ld/%ld", (long) ht->count,
			  (long) ht->size);
    }

  if (ht->weakness != HASH_TABLE_NON_WEAK)
    {
      write_fmt_string
	(printcharfun, " :weakness %s",
	 (ht->weakness == HASH_TABLE_WEAK	    ? "key-and-value" :
	  ht->weakness == HASH_TABLE_KEY_WEAK	    ? "key" :
	  ht->weakness == HASH_TABLE_VALUE_WEAK	    ? "value" :
	  ht->weakness == HASH_TABLE_KEY_VALUE_WEAK ? "key-or-value" :
	  "you-d-better-not-see-this"));
    }

  if (ht->rehash_size != HASH_TABLE_DEFAULT_REHASH_SIZE)
    {
      float_to_string (pigbuf, ht->rehash_size);
      write_fmt_string (printcharfun, " :rehash-size %s", pigbuf);
    }

  if (ht->rehash_threshold
      != HASH_TABLE_DEFAULT_REHASH_THRESHOLD (ht->size, ht->test))
    {
      float_to_string (pigbuf, ht->rehash_threshold);
      write_fmt_string (printcharfun, " :rehash-threshold %s", pigbuf);
    }

  if (ht->count)
    print_hash_table_data (ht, printcharfun);

  if (print_readably)
    write_ascstring (printcharfun, ")");
  else
    write_fmt_string (printcharfun, " 0x%x>", LISP_OBJECT_UID (obj));
}

#ifdef ERROR_CHECK_STRUCTURES
#define USED_IF_ERROR_CHECK_STRUCTURES(x) x
#else
#define USED_IF_ERROR_CHECK_STRUCTURES(x) UNUSED (x)
#endif

#ifndef NEW_GC
static void
free_hentries (htentry *hentries,
	       Elemcount USED_IF_ERROR_CHECK_STRUCTURES (size))
{
#ifdef ERROR_CHECK_STRUCTURES
  /* Ensure a crash if other code uses the discarded entries afterwards. */
  deadbeef_memory (hentries,
		   (Rawbyte *) (hentries + size) - (Rawbyte *) hentries);
#endif

  if (!DUMPEDP (hentries))
    xfree (hentries);
}

static void
finalize_hash_table (Lisp_Object obj)
{
  Lisp_Hash_Table *ht = XHASH_TABLE (obj);
  free_hentries (ht->hentries, ht->size);
  ht->hentries = 0;
}
#endif /* not NEW_GC */

static const struct memory_description htentry_description_1[] = {
  { XD_LISP_OBJECT, offsetof (htentry, key) },
  { XD_LISP_OBJECT, offsetof (htentry, value) },
  { XD_END }
};

static const struct sized_memory_description htentry_description = {
  sizeof (htentry),
  htentry_description_1
};

#ifdef NEW_GC
static const struct memory_description htentry_weak_description_1[] = {
  { XD_LISP_OBJECT, offsetof (htentry, key), 0, { 0 }, XD_FLAG_NO_KKCC},
  { XD_LISP_OBJECT, offsetof (htentry, value), 0, { 0 }, XD_FLAG_NO_KKCC},
  { XD_END }
};

static const struct sized_memory_description htentry_weak_description = {
  sizeof (htentry),
  htentry_weak_description_1
};

DEFINE_DUMPABLE_INTERNAL_LISP_OBJECT ("hash-table-entry", hash_table_entry,
				      0, htentry_description_1,
				      Lisp_Hash_Table_Entry);
#endif /* NEW_GC */

static const struct memory_description htentry_union_description_1[] = {
  /* Note: XD_INDIRECT in this table refers to the surrounding table,
     and so this will work. */
#ifdef NEW_GC
  { XD_INLINE_LISP_OBJECT_BLOCK_PTR, HASH_TABLE_NON_WEAK,
    XD_INDIRECT (0, 1), { &htentry_description } },
  { XD_INLINE_LISP_OBJECT_BLOCK_PTR, 0, XD_INDIRECT (0, 1),
    { &htentry_weak_description }, XD_FLAG_UNION_DEFAULT_ENTRY },
#else /* not NEW_GC */
  { XD_BLOCK_PTR, HASH_TABLE_NON_WEAK, XD_INDIRECT (0, 1),
    { &htentry_description } },
  { XD_BLOCK_PTR, 0, XD_INDIRECT (0, 1), { &htentry_description },
    XD_FLAG_UNION_DEFAULT_ENTRY | XD_FLAG_NO_KKCC },
#endif /* not NEW_GC */
  { XD_END }
};

static const struct sized_memory_description htentry_union_description = {
  sizeof (htentry *),
  htentry_union_description_1
};

const struct memory_description hash_table_description[] = {
  { XD_ELEMCOUNT,  offsetof (Lisp_Hash_Table, size) },
  { XD_INT,	   offsetof (Lisp_Hash_Table, weakness) },
  { XD_UNION,	   offsetof (Lisp_Hash_Table, hentries), XD_INDIRECT (1, 0),
    { &htentry_union_description } },
  { XD_LO_LINK,    offsetof (Lisp_Hash_Table, next_weak) },
  { XD_LISP_OBJECT,offsetof (Lisp_Hash_Table, test) },
  { XD_END }
};

DEFINE_DUMPABLE_LISP_OBJECT ("hash-table", hash_table,
			     mark_hash_table, print_hash_table,
			     IF_OLD_GC (finalize_hash_table),
			     hash_table_equal, hash_table_hash,
			     hash_table_description,
			     Lisp_Hash_Table);

static Lisp_Hash_Table *
xhash_table (Lisp_Object hash_table)
{
  /* #### What's going on here?  Why the gc_in_progress check? */
  if (!gc_in_progress)
    CHECK_HASH_TABLE (hash_table);
  check_hash_table_invariants (XHASH_TABLE (hash_table));
  return XHASH_TABLE (hash_table);
}


/************************************************************************/
/*			 Creation of Hash Tables			*/
/************************************************************************/

/* Creation of hash tables, without error-checking. */
static void
compute_hash_table_derived_values (Lisp_Hash_Table *ht)
{
  ht->rehash_count = (Elemcount)
    ((double) ht->size * ht->rehash_threshold);
  ht->golden_ratio = (Elemcount)
    ((double) ht->size * (.6180339887 / (double) sizeof (Lisp_Object)));
}

static htentry *
allocate_hash_table_entries (Elemcount size)
{
#ifdef NEW_GC
  return XHASH_TABLE_ENTRY (alloc_lrecord_array
			    (size, &lrecord_hash_table_entry));
#else /* not NEW_GC */
  return xnew_array_and_zero (htentry, size);
#endif /* not NEW_GC */
}

static Lisp_Object decode_hash_table_test (Lisp_Object obj);

Lisp_Object
make_general_lisp_hash_table (Lisp_Object test,
			      Elemcount size,
			      double rehash_size,
			      double rehash_threshold,
			      enum hash_table_weakness weakness)
{
  Lisp_Object hash_table = ALLOC_NORMAL_LISP_OBJECT (hash_table);
  Lisp_Hash_Table *ht = XHASH_TABLE (hash_table);

  assert (HASH_TABLE_TESTP (test));

  ht->test = test;
  ht->weakness = weakness;

  ht->rehash_size =
    rehash_size > 1.0 ? rehash_size : HASH_TABLE_DEFAULT_REHASH_SIZE;

  ht->rehash_threshold =
    rehash_threshold > 0.0 ? rehash_threshold :
    HASH_TABLE_DEFAULT_REHASH_THRESHOLD (size, ht->test);

  if (size < HASH_TABLE_MIN_SIZE)
    size = HASH_TABLE_MIN_SIZE;
  ht->size = hash_table_size ((Elemcount) (((double) size / ht->rehash_threshold)
					+ 1.0));
  ht->count = 0;

  compute_hash_table_derived_values (ht);

  /* We leave room for one never-occupied sentinel htentry at the end.  */
  ht->hentries = allocate_hash_table_entries (ht->size + 1);

  if (weakness == HASH_TABLE_NON_WEAK)
    ht->next_weak = Qunbound;
  else
    ht->next_weak = Vall_weak_hash_tables, Vall_weak_hash_tables = hash_table;

  return hash_table;
}

Lisp_Object
make_lisp_hash_table (Elemcount size, enum hash_table_weakness weakness,
                      Lisp_Object test)
{
  test = decode_hash_table_test (test);
  return make_general_lisp_hash_table (test, size, -1.0, -1.0, weakness);
}

/* Pretty reading of hash tables.

   Here we use the existing structures mechanism (which is,
   unfortunately, pretty cumbersome) for validating and instantiating
   the hash tables.  The idea is that the side-effect of reading a
   #s(hash-table PLIST) object is creation of a hash table with desired
   properties, and that the hash table is returned.  */

/* Validation functions: each keyword provides its own validation
   function.  The errors should maybe be continuable, but it is
   unclear how this would cope with ERRB.  */
static int
hash_table_size_validate (Lisp_Object UNUSED (keyword), Lisp_Object value,
			  Error_Behavior errb)
{
  if (NATNUMP (value))
    {
      if (BIGNUMP (value))
        {
          /* hash_table_size() can't handle excessively large sizes. */
          maybe_signal_error_1 (Qargs_out_of_range,
                                list3 (value, Qzero,
                                       make_integer (MOST_POSITIVE_FIXNUM)),
                                Qhash_table, errb);
          return 0;
        }
      else
        {
          return 1;
        }
    }
  else
    {
      maybe_signal_error_1 (Qwrong_type_argument, list2 (Qnatnump, value),
                            Qhash_table, errb);
    }

  return 0;
}

static Elemcount
decode_hash_table_size (Lisp_Object obj)
{
  return NILP (obj) ? HASH_TABLE_DEFAULT_SIZE : XFIXNUM (obj);
}

static int
hash_table_weakness_validate (Lisp_Object UNUSED (keyword), Lisp_Object value,
			      Error_Behavior errb)
{
  if (EQ (value, Qnil))			return 1;
  if (EQ (value, Qt))			return 1;
  if (EQ (value, Qkey))			return 1;
  if (EQ (value, Qkey_and_value))	return 1;
  if (EQ (value, Qkey_or_value))	return 1;
  if (EQ (value, Qvalue))		return 1;

#ifdef NEED_TO_HANDLE_21_4_CODE
  /* Following values are obsolete as of 19990901 in xemacs-21.2 */
  if (EQ (value, Qnon_weak))		return 1;
  if (EQ (value, Qweak))		return 1;
  if (EQ (value, Qkey_weak))		return 1;
  if (EQ (value, Qkey_or_value_weak))	return 1;
  if (EQ (value, Qvalue_weak))		return 1;
#endif

  maybe_invalid_constant ("Invalid hash table weakness",
			     value, Qhash_table, errb);
  return 0;
}

static enum hash_table_weakness
decode_hash_table_weakness (Lisp_Object obj)
{
  if (EQ (obj, Qnil))			return HASH_TABLE_NON_WEAK;
  if (EQ (obj, Qt))			return HASH_TABLE_WEAK;
  if (EQ (obj, Qkey_and_value))		return HASH_TABLE_WEAK;
  if (EQ (obj, Qkey))			return HASH_TABLE_KEY_WEAK;
  if (EQ (obj, Qkey_or_value))		return HASH_TABLE_KEY_VALUE_WEAK;
  if (EQ (obj, Qvalue))			return HASH_TABLE_VALUE_WEAK;

#ifdef NEED_TO_HANDLE_21_4_CODE
  /* Following values are obsolete as of 19990901 in xemacs-21.2 */
  if (EQ (obj, Qnon_weak))		return HASH_TABLE_NON_WEAK;
  if (EQ (obj, Qweak))			return HASH_TABLE_WEAK;
  if (EQ (obj, Qkey_weak))		return HASH_TABLE_KEY_WEAK;
  if (EQ (obj, Qkey_or_value_weak))	return HASH_TABLE_KEY_VALUE_WEAK;
  if (EQ (obj, Qvalue_weak))		return HASH_TABLE_VALUE_WEAK;
#endif

  invalid_constant ("Invalid hash table weakness", obj);
  RETURN_NOT_REACHED (HASH_TABLE_NON_WEAK);
}

static int
hash_table_test_validate (Lisp_Object UNUSED (keyword), Lisp_Object value,
			  Error_Behavior errb)
{
  Lisp_Object lookup;

  if (NILP (value))
    {
      return 1;
    }

  lookup = Fassq (value, XWEAK_LIST_LIST (Vhash_table_test_weak_list));
  if (NILP (lookup))
    {
      maybe_invalid_constant ("Invalid hash table test",
                              value, Qhash_table, errb);
    }

  return 1;
}

static Lisp_Object
decode_hash_table_test (Lisp_Object obj)
{
  Lisp_Object result;

  if (NILP (obj))
    {
      obj = Qeql;
    }

  result = Fassq (obj, XWEAK_LIST_LIST (Vhash_table_test_weak_list));
  if (NILP (result))
    {
      invalid_constant ("Invalid hash table test", obj);
    }
  
  return XCDR (result);
}

static int
hash_table_rehash_size_validate (Lisp_Object UNUSED (keyword),
				 Lisp_Object value, Error_Behavior errb)
{
  if (!FLOATP (value))
    {
      maybe_signal_error_1 (Qwrong_type_argument, list2 (Qfloatp, value),
			  Qhash_table, errb);
      return 0;
    }

  {
    double rehash_size = XFLOAT_DATA (value);
    if (rehash_size <= 1.0)
      {
	maybe_invalid_argument
	  ("Hash table rehash size must be greater than 1.0",
	   value, Qhash_table, errb);
	return 0;
      }
  }

  return 1;
}

static double
decode_hash_table_rehash_size (Lisp_Object rehash_size)
{
  /* -1.0 signals make_general_lisp_hash_table to use the default. */
  return NILP (rehash_size) ? -1.0 : XFLOAT_DATA (rehash_size);
}

static int
hash_table_rehash_threshold_validate (Lisp_Object UNUSED (keyword),
				      Lisp_Object value, Error_Behavior errb)
{
  if (!FLOATP (value))
    {
      maybe_signal_error_1 (Qwrong_type_argument, list2 (Qfloatp, value),
			  Qhash_table, errb);
      return 0;
    }

  {
    double rehash_threshold = XFLOAT_DATA (value);
    if (rehash_threshold <= 0.0 || rehash_threshold >= 1.0)
      {
	maybe_invalid_argument
	  ("Hash table rehash threshold must be between 0.0 and 1.0",
	   value, Qhash_table, errb);
	return 0;
      }
  }

  return 1;
}

static double
decode_hash_table_rehash_threshold (Lisp_Object rehash_threshold)
{
  /* -1.0 signals make_general_lisp_hash_table to use the default. */
  return NILP (rehash_threshold) ? -1.0 : XFLOAT_DATA (rehash_threshold);
}

static int
hash_table_data_validate (Lisp_Object UNUSED (keyword), Lisp_Object value,
			  Error_Behavior errb)
{
  int len;

  /* Check for improper lists while getting length. */
  GET_EXTERNAL_LIST_LENGTH (value, len);

  if (len & 1)
    {
      maybe_sferror
	("Hash table data must have alternating key/value pairs",
	 value, Qhash_table, errb);
      return 0;
    }
  
  return 1;
}

/* The actual instantiation of a hash table.  This does practically no
   error checking, because it relies on the fact that the paranoid
   functions above have error-checked everything to the last details.
   If this assumption is wrong, we will get a crash immediately (with
   error-checking compiled in), and we'll know if there is a bug in
   the structure mechanism.  So there.  */
static Lisp_Object
hash_table_instantiate (Lisp_Object plist)
{
  Lisp_Object hash_table;
  Lisp_Object test	       = Qnil;
  Lisp_Object size	       = Qnil;
  Lisp_Object rehash_size      = Qnil;
  Lisp_Object rehash_threshold = Qnil;
  Lisp_Object weakness	       = Qnil;
  Lisp_Object data	       = Qnil;

  if (KEYWORDP (Fcar (plist)))
    {
      PROPERTY_LIST_LOOP_3 (key, value, plist)
        {
          if      (EQ (key, Q_test))		    test	     = value;
          else if (EQ (key, Q_size))		    size	     = value;
          else if (EQ (key, Q_rehash_size))	    rehash_size	     = value;
          else if (EQ (key, Q_rehash_threshold)) rehash_threshold = value;
          else if (EQ (key, Q_weakness))	    weakness	     = value;
          else if (EQ (key, Q_data))		    data	     = value;
          else if (!KEYWORDP (key))
            signal_error (Qinvalid_read_syntax, 
                          "can't mix keyword and non-keyword hash table syntax",
                          key);
          else ABORT();
        }
    }
  else
    {
      PROPERTY_LIST_LOOP_3 (key, value, plist)
        {
          if      (EQ (key, Qtest))		    test	     = value;
          else if (EQ (key, Qsize))		    size	     = value;
          else if (EQ (key, Qrehash_size))	    rehash_size	     = value;
          else if (EQ (key, Qrehash_threshold)) rehash_threshold = value;
          else if (EQ (key, Qweakness))	    weakness	     = value;
          else if (EQ (key, Qdata))		    data	     = value;
#ifdef NEED_TO_HANDLE_21_4_CODE
          else if (EQ (key, Qtype))/*obsolete*/ weakness	     = value;
#endif
          else if (KEYWORDP (key))
            signal_error (Qinvalid_read_syntax, 
                          "can't mix keyword and non-keyword hash table syntax",
                          key);
          else ABORT();                   
        }
    }

  /* Create the hash table.  */
  hash_table = make_general_lisp_hash_table
    (decode_hash_table_test (test),
     decode_hash_table_size (size),
     decode_hash_table_rehash_size (rehash_size),
     decode_hash_table_rehash_threshold (rehash_threshold),
     decode_hash_table_weakness (weakness));

  /* This can GC with a user-specified test. */
  {
    struct gcpro gcpro1;
    GCPRO1 (hash_table);

    /* And fill it with data.  */
    while (!NILP (data))
      {
	Lisp_Object key, value;
	key   = XCAR (data); data = XCDR (data);
	value = XCAR (data); data = XCDR (data);
	Fputhash (key, value, hash_table);
      }
    UNGCPRO;
  }

  return hash_table;
}

static void
structure_type_create_hash_table_structure_name (Lisp_Object structure_name)
{
  struct structure_type *st;

  st = define_structure_type (structure_name, 0, hash_table_instantiate);

  /* First the keyword syntax: */
  define_structure_type_keyword (st, Q_test, hash_table_test_validate);
  define_structure_type_keyword (st, Q_size, hash_table_size_validate);
  define_structure_type_keyword (st, Q_rehash_size, hash_table_rehash_size_validate);
  define_structure_type_keyword (st, Q_rehash_threshold, hash_table_rehash_threshold_validate);
  define_structure_type_keyword (st, Q_weakness, hash_table_weakness_validate);
  define_structure_type_keyword (st, Q_data, hash_table_data_validate);

#ifdef NEED_TO_HANDLE_21_4_CODE
  /* Next the mutually exclusive, older, non-keyword syntax: */
  define_structure_type_keyword (st, Qtest, hash_table_test_validate);
  define_structure_type_keyword (st, Qsize, hash_table_size_validate);
  define_structure_type_keyword (st, Qrehash_size, hash_table_rehash_size_validate);
  define_structure_type_keyword (st, Qrehash_threshold, hash_table_rehash_threshold_validate);
  define_structure_type_keyword (st, Qweakness, hash_table_weakness_validate);
  define_structure_type_keyword (st, Qdata, hash_table_data_validate);

  /* obsolete as of 19990901 in xemacs-21.2 */
  define_structure_type_keyword (st, Qtype, hash_table_weakness_validate);
#endif
}

/* Create a built-in Lisp structure type named `hash-table'.
   We make #s(hashtable ...) equivalent to #s(hash-table ...),
   for backward compatibility.
   This is called from emacs.c.  */
void
structure_type_create_hash_table (void)
{
  structure_type_create_hash_table_structure_name (Qhash_table);
#ifdef NEED_TO_HANDLE_21_4_CODE
  structure_type_create_hash_table_structure_name (Qhashtable); /* compat */
#endif
}


/************************************************************************/
/*		Definition of Lisp-visible methods			*/
/************************************************************************/

DEFUN ("hash-table-p", Fhash_table_p, 1, 1, 0, /*
Return t if OBJECT is a hash table, else nil.
*/
       (object))
{
  return HASH_TABLEP (object) ? Qt : Qnil;
}

DEFUN ("make-hash-table", Fmake_hash_table, 0, MANY, 0, /*
Return a new empty hash table object.
Use Common Lisp style keywords to specify hash table properties.

Keyword :test can be `eq', `eql' (default), `equal' or `equalp'.
Comparison between keys is done using this function.  If speed is important,
consider using `eq'.  When storing strings in the hash table, you will
likely need to use `equal' or `equalp' (for case-insensitivity).  With other
objects, consider using a test function defined with
`define-hash-table-test', an emacs extension to this Common Lisp hash table
API.

Keyword :size specifies the number of keys likely to be inserted.
This number of entries can be inserted without enlarging the hash table.

Keyword :rehash-size must be a float greater than 1.0, and specifies
the factor by which to increase the size of the hash table when enlarging.

Keyword :rehash-threshold must be a float between 0.0 and 1.0,
and specifies the load factor of the hash table which triggers enlarging.

Non-standard keyword :weakness can be `nil' (default), `t', `key-and-value',
`key', `value' or `key-or-value'. `t' is an alias for `key-and-value'.

A key-and-value-weak hash table, also known as a fully-weak or simply
as a weak hash table, is one whose pointers do not count as GC
referents: for any key-value pair in the hash table, if the only
remaining pointer to either the key or the value is in a weak hash
table, then the pair will be removed from the hash table, and the key
and value collected.  A non-weak hash table (or any other pointer)
would prevent the object from being collected.

A key-weak hash table is similar to a fully-weak hash table except that
a key-value pair will be removed only if the key remains unmarked
outside of weak hash tables.  The pair will remain in the hash table if
the key is pointed to by something other than a weak hash table, even
if the value is not.

A value-weak hash table is similar to a fully-weak hash table except
that a key-value pair will be removed only if the value remains
unmarked outside of weak hash tables.  The pair will remain in the
hash table if the value is pointed to by something other than a weak
hash table, even if the key is not.

A key-or-value-weak hash table is similar to a fully-weak hash table except
that a key-value pair will be removed only if the value and the key remain
unmarked outside of weak hash tables.  The pair will remain in the
hash table if the value or key are pointed to by something other than a weak
hash table, even if the other is not.

arguments: (&key TEST SIZE REHASH-SIZE REHASH-THRESHOLD WEAKNESS)
*/
       (int nargs, Lisp_Object *args))
{
#ifndef NEED_TO_HANDLE_21_4_CODE
  PARSE_KEYWORDS (Fmake_hash_table, nargs, args, 5,
                  (test, size, rehash_size, rehash_threshold, weakness),
                  NULL);
#else
  PARSE_KEYWORDS (Fmake_hash_table, nargs, args, 6,
                  (test, size, rehash_size, rehash_threshold, weakness,
		   type), (type = Qunbound, weakness = Qunbound));

  if (EQ (weakness, Qunbound))
    {
      if (EQ (weakness, Qunbound) && !EQ (type, Qunbound))
        {
          weakness = type;
        }
      else
        {
          weakness = Qnil;
        }
    }
#endif

#define VALIDATE_VAR(var) \
if (!NILP (var)) hash_table_##var##_validate (Q##var, var, ERROR_ME);

  VALIDATE_VAR (test);
  VALIDATE_VAR (size);
  VALIDATE_VAR (rehash_size);
  VALIDATE_VAR (rehash_threshold);
  VALIDATE_VAR (weakness);

  return make_general_lisp_hash_table
    (decode_hash_table_test (test),
     decode_hash_table_size (size),
     decode_hash_table_rehash_size (rehash_size),
     decode_hash_table_rehash_threshold (rehash_threshold),
     decode_hash_table_weakness (weakness));
}

DEFUN ("copy-hash-table", Fcopy_hash_table, 1, 1, 0, /*
Return a new hash table containing the same keys and values as HASH-TABLE.
The keys and values will not themselves be copied.
*/
       (hash_table))
{
  const Lisp_Hash_Table *ht_old = xhash_table (hash_table);
  Lisp_Object obj = ALLOC_NORMAL_LISP_OBJECT (hash_table);
  Lisp_Hash_Table *ht = XHASH_TABLE (obj);
  copy_lisp_object (obj, hash_table);

  /* We leave room for one never-occupied sentinel htentry at the end.  */
  ht->hentries = allocate_hash_table_entries (ht_old->size + 1);
  memcpy (ht->hentries, ht_old->hentries, (ht_old->size + 1) * sizeof (htentry));

  if (! EQ (ht->next_weak, Qunbound))
    {
      ht->next_weak = Vall_weak_hash_tables;
      Vall_weak_hash_tables = obj;
    }

  return obj;
}

static void
resize_hash_table (Lisp_Hash_Table *ht, Elemcount new_size)
{
  htentry *old_entries, *new_entries, *sentinel, *e;
  Elemcount old_size;
  Hash_Table_Test *http = XHASH_TABLE_TEST (ht->test);

  old_size = ht->size;
  ht->size = new_size;

  old_entries = ht->hentries;

  /* We leave room for one never-occupied sentinel htentry at the end.  */
  ht->hentries = allocate_hash_table_entries (new_size + 1);
  new_entries = ht->hentries;

  compute_hash_table_derived_values (ht);

  for (e = old_entries, sentinel = e + old_size; e < sentinel; e++)
    if (!HTENTRY_CLEAR_P (e))
      {
	htentry *probe = new_entries + HASHCODE (e->key, ht, http);
	LINEAR_PROBING_LOOP (probe, new_entries, new_size)
	  ;
	*probe = *e;
      }

#ifndef NEW_GC
  free_hentries (old_entries, old_size);
#endif /* not NEW_GC */
}

/* After a hash table has been saved to disk and later restored by the
   portable dumper, it contains the same objects, but their addresses
   and thus their HASHCODEs have changed. */
void
pdump_reorganize_hash_table (Lisp_Object hash_table)
{
  const Lisp_Hash_Table *ht = xhash_table (hash_table);
  /* We leave room for one never-occupied sentinel htentry at the end.  */
  htentry *new_entries = allocate_hash_table_entries (ht->size + 1);
  htentry *e, *sentinel;
  Hash_Table_Test *http = XHASH_TABLE_TEST (ht->test);

  for (e = ht->hentries, sentinel = e + ht->size; e < sentinel; e++)
    if (!HTENTRY_CLEAR_P (e))
      {
	htentry *probe = new_entries + HASHCODE (e->key, ht, http);
	LINEAR_PROBING_LOOP (probe, new_entries, ht->size)
	  ;
	*probe = *e;
      }

  memcpy (ht->hentries, new_entries, ht->size * sizeof (htentry));

#ifndef NEW_GC
  xfree (new_entries);
#endif /* not NEW_GC */
}

static void
enlarge_hash_table (Lisp_Hash_Table *ht)
{
  Elemcount new_size =
    hash_table_size ((Elemcount) ((double) ht->size * ht->rehash_size));
  resize_hash_table (ht, new_size);
}

htentry *
find_htentry (Lisp_Object key, const Lisp_Hash_Table *ht)
{
  Lisp_Object test = ht->test;
  Hash_Table_Test *http = XHASH_TABLE_TEST (test);

  htentry *entries = ht->hentries;
  htentry *probe = entries + HASHCODE (key, ht, http);

  LINEAR_PROBING_LOOP (probe, entries, ht->size)
    if (KEYS_EQUAL_P (probe->key, key, test, http))
      break;

  return probe;
}

/* A version of Fputhash() that increments the value by the specified
   amount and dispenses with all error checks.  Assumes that tables does
   comparison using EQ.  Used by the profiling routines to avoid
   overhead -- profiling overhead was being recorded at up to 15% of the
   total time. */

htentry *
inchash_eq (Lisp_Object key, Lisp_Object table, EMACS_INT offset)
{
  Lisp_Hash_Table *ht = XHASH_TABLE (table);
  Hash_Table_Test *http = XHASH_TABLE_TEST (ht->test);
  htentry *entries = ht->hentries;
  htentry *probe = entries + HASHCODE (key, ht, http);

  LINEAR_PROBING_LOOP (probe, entries, ht->size)
    if (EQ (probe->key, key))
      break;

  if (!HTENTRY_CLEAR_P (probe))
    probe->value = make_fixnum (XFIXNUM (probe->value) + offset);
  else
    {
      probe->key   = key;
      probe->value = make_fixnum (offset);

      if (++ht->count >= ht->rehash_count)
        {
          enlarge_hash_table (ht);
          return NULL;
        }
    }

  return probe;
}

DEFUN ("gethash", Fgethash, 2, 3, 0, /*
Find hash value for KEY in HASH-TABLE.
If there is no corresponding value, return DEFAULT (which defaults to nil).
*/
       (key, hash_table, default_))
{
  const Lisp_Hash_Table *ht = xhash_table (hash_table);
  htentry *e = find_htentry (key, ht);

  return HTENTRY_CLEAR_P (e) ? default_ : e->value;
}

DEFUN ("puthash", Fputhash, 3, 3, 0, /*
Hash KEY to VALUE in HASH-TABLE, and return VALUE. 
*/
       (key, value, hash_table))
{
  Lisp_Hash_Table *ht = xhash_table (hash_table);
  htentry *e = find_htentry (key, ht);

  if (!HTENTRY_CLEAR_P (e))
    return e->value = value;

  e->key   = key;
  e->value = value;

  if (++ht->count >= ht->rehash_count)
    enlarge_hash_table (ht);

  return value;
}

/* Remove htentry pointed at by PROBE.
   Subsequent entries are removed and reinserted.
   We don't use tombstones - too wasteful.  */
static void
remhash_1 (Lisp_Hash_Table *ht, htentry *entries, htentry *probe)
{
  Hash_Table_Test *http = XHASH_TABLE_TEST (ht->test);
  Elemcount size = ht->size;
  CLEAR_HTENTRY (probe);
  probe++;
  ht->count--;

  LINEAR_PROBING_LOOP (probe, entries, size)
    {
      Lisp_Object key = probe->key;
      htentry *probe2 = entries + HASHCODE (key, ht, http);
      LINEAR_PROBING_LOOP (probe2, entries, size)
	if (EQ (probe2->key, key))
	  /* htentry at probe doesn't need to move. */
	  goto continue_outer_loop;
      /* Move htentry from probe to new home at probe2. */
      *probe2 = *probe;
      CLEAR_HTENTRY (probe);
    continue_outer_loop: continue;
    }
}

DEFUN ("remhash", Fremhash, 2, 2, 0, /*
Remove the entry for KEY from HASH-TABLE.
Do nothing if there is no entry for KEY in HASH-TABLE.
Return non-nil if an entry was removed.
*/
       (key, hash_table))
{
  Lisp_Hash_Table *ht = xhash_table (hash_table);
  htentry *e = find_htentry (key, ht);

  if (HTENTRY_CLEAR_P (e))
    return Qnil;

  remhash_1 (ht, ht->hentries, e);
  return Qt;
}

DEFUN ("clrhash", Fclrhash, 1, 1, 0, /*
Remove all entries from HASH-TABLE, leaving it empty.
Return HASH-TABLE.
*/
       (hash_table))
{
  Lisp_Hash_Table *ht = xhash_table (hash_table);
  htentry *e, *sentinel;

  for (e = ht->hentries, sentinel = e + ht->size; e < sentinel; e++)
    CLEAR_HTENTRY (e);
  ht->count = 0;

  return hash_table;
}

/************************************************************************/
/*			    Accessor Functions				*/
/************************************************************************/

DEFUN ("hash-table-count", Fhash_table_count, 1, 1, 0, /*
Return the number of entries in HASH-TABLE.
*/
       (hash_table))
{
  return make_fixnum (xhash_table (hash_table)->count);
}

DEFUN ("hash-table-test", Fhash_table_test, 1, 1, 0, /*
Return HASH-TABLE's test.

This can be one of `eq', `eql', `equal', `equalp', or some symbol supplied
as the NAME argument to `define-hash-table-test', which see.
*/
       (hash_table))
{
  CHECK_HASH_TABLE (hash_table);
  return XHASH_TABLE_TEST (XHASH_TABLE (hash_table)->test)->name;
}

DEFUN ("hash-table-size", Fhash_table_size, 1, 1, 0, /*
Return the size of HASH-TABLE.
This is the current number of slots in HASH-TABLE, whether occupied or not.
*/
       (hash_table))
{
  return make_fixnum (xhash_table (hash_table)->size);
}

DEFUN ("hash-table-rehash-size", Fhash_table_rehash_size, 1, 1, 0, /*
Return the current rehash size of HASH-TABLE.
This is a float greater than 1.0; the factor by which HASH-TABLE
is enlarged when the rehash threshold is exceeded.
*/
       (hash_table))
{
  return make_float (xhash_table (hash_table)->rehash_size);
}

DEFUN ("hash-table-rehash-threshold", Fhash_table_rehash_threshold, 1, 1, 0, /*
Return the current rehash threshold of HASH-TABLE.
This is a float between 0.0 and 1.0; the maximum `load factor' of HASH-TABLE,
beyond which the HASH-TABLE is enlarged by rehashing.
*/
       (hash_table))
{
  return make_float (xhash_table (hash_table)->rehash_threshold);
}

DEFUN ("hash-table-weakness", Fhash_table_weakness, 1, 1, 0, /*
Return the weakness of HASH-TABLE.
This can be one of `nil', `key-and-value', `key-or-value', `key' or `value'.
*/
       (hash_table))
{
  switch (xhash_table (hash_table)->weakness)
    {
    case HASH_TABLE_WEAK:		return Qkey_and_value;
    case HASH_TABLE_KEY_WEAK:		return Qkey;
    case HASH_TABLE_KEY_VALUE_WEAK:	return Qkey_or_value;
    case HASH_TABLE_VALUE_WEAK:		return Qvalue;
    default:				return Qnil;
    }
}

/* obsolete as of 19990901 in xemacs-21.2 */
DEFUN ("hash-table-type", Fhash_table_type, 1, 1, 0, /*
Return the type of HASH-TABLE.
This can be one of `non-weak', `weak', `key-weak' or `value-weak'.
*/
       (hash_table))
{
  switch (xhash_table (hash_table)->weakness)
    {
    case HASH_TABLE_WEAK:		return Qweak;
    case HASH_TABLE_KEY_WEAK:		return Qkey_weak;
    case HASH_TABLE_KEY_VALUE_WEAK:	return Qkey_or_value_weak;
    case HASH_TABLE_VALUE_WEAK:		return Qvalue_weak;
    default:				return Qnon_weak;
    }
}

/************************************************************************/
/*			    Mapping Functions				*/
/************************************************************************/

/* We need to be careful when mapping over hash tables because the
   hash table might be modified during the mapping operation:
   - by the mapping function
   - by gc (if the hash table is weak)

   So we make a copy of the hentries at the beginning of the mapping
   operation, and iterate over the copy.  Naturally, this is
   expensive, but not as expensive as you might think, because no
   actual memory has to be collected by our notoriously inefficient
   GC; we use an unwind-protect instead to free the memory directly.

   We could avoid the copying by having the hash table modifiers
   puthash and remhash check for currently active mapping functions.
   Disadvantages: it's hard to get right, and IMO hash mapping
   functions are basically rare, and no extra space in the hash table
   object and no extra cpu in puthash or remhash should be wasted to
   make maphash 3% faster.  From a design point of view, the basic
   functions gethash, puthash and remhash should be implementable
   without having to think about maphash.

   Note: We don't (yet) have Common Lisp's with-hash-table-iterator.
   If you implement this naively, you cannot have more than one
   concurrently active iterator over the same hash table.  The `each'
   function in perl has this limitation.

   Note: We GCPRO memory on the heap, not on the stack.  There is no
   obvious reason why this is bad, but as of this writing this is the
   only known occurrence of this technique in the code.

   -- Martin
*/

/* Ben disagrees with the "copying hentries" design, and says:

   Another solution is the same as I've already proposed -- when
   mapping, mark the table as "change-unsafe", and in this case, use a
   secondary table to maintain changes.  this could be basically a
   standard hash table, but with entries only for added or deleted
   entries in the primary table, and a marker like Qunbound to
   indicate a deleted entry.  puthash, gethash and remhash need a
   single extra check for this secondary table -- totally
   insignificant speedwise.  if you really cared about making
   recursive maphashes completely correct, you'd have to do a bit of
   extra work here -- when maphashing, if the secondary table exists,
   make a copy of it, and use the copy in conjunction with the primary
   table when mapping.  the advantages of this are

   [a] easy to demonstrate correct, even with weak hashtables.

   [b] no extra overhead in the general maphash case -- only when you
       modify the table while maphashing, and even then the overhead is
       very small.
*/

static Lisp_Object
maphash_unwind (Lisp_Object unwind_obj)
{
  void *ptr = (void *) get_opaque_ptr (unwind_obj);
  xfree (ptr);
  free_opaque_ptr (unwind_obj);
  return Qnil;
}

/* Return a malloced array of alternating key/value pairs from HT. */
static Lisp_Object *
copy_compress_hentries (const Lisp_Hash_Table *ht)
{
  Lisp_Object * const objs =
    /* If the hash table is empty, ht->count could be 0. */
    xnew_array (Lisp_Object, 2 * (ht->count > 0 ? ht->count : 1));
  const htentry *e, *sentinel;
  Lisp_Object *pobj;

  for (e = ht->hentries, sentinel = e + ht->size, pobj = objs; e < sentinel; e++)
    if (!HTENTRY_CLEAR_P (e))
      {
	*(pobj++) = e->key;
	*(pobj++) = e->value;
      }

  type_checking_assert (pobj == objs + 2 * ht->count);

  return objs;
}

DEFUN ("maphash", Fmaphash, 2, 2, 0, /*
Map FUNCTION over entries in HASH-TABLE, calling it with two args,
each key and value in HASH-TABLE.

FUNCTION must not modify HASH-TABLE, with the one exception that FUNCTION
may remhash or puthash the entry currently being processed by FUNCTION.
*/
       (function, hash_table))
{
  const Lisp_Hash_Table * const ht = xhash_table (hash_table);
  Lisp_Object * const objs = copy_compress_hentries (ht);
  Lisp_Object args[3];
  const Lisp_Object *pobj, *end;
  int speccount = specpdl_depth ();
  struct gcpro gcpro1;

  record_unwind_protect (maphash_unwind, make_opaque_ptr ((void *)objs));
  GCPRO1 (objs[0]);
  gcpro1.nvars = 2 * ht->count;

  args[0] = function;

  for (pobj = objs, end = pobj + 2 * ht->count; pobj < end; pobj += 2)
    {
      args[1] = pobj[0];
      args[2] = pobj[1];
      Ffuncall (countof (args), args);
    }

  unbind_to (speccount);
  UNGCPRO;

  return Qnil;
}

/* Map *C* function FUNCTION over the elements of a non-weak lisp hash table.
   FUNCTION must not modify HASH-TABLE, with the one exception that FUNCTION
   may puthash the entry currently being processed by FUNCTION.
   Mapping terminates if FUNCTION returns something other than 0. */
void
elisp_maphash_unsafe (maphash_function_t function,
	       Lisp_Object hash_table, void *extra_arg)
{
  const Lisp_Hash_Table *ht = XHASH_TABLE (hash_table);
  const htentry *e, *sentinel;

  for (e = ht->hentries, sentinel = e + ht->size; e < sentinel; e++)
    if (!HTENTRY_CLEAR_P (e))
      if (function (e->key, e->value, extra_arg))
	return;
}

/* Map *C* function FUNCTION over the elements of a lisp hash table.
   It is safe for FUNCTION to modify HASH-TABLE.
   Mapping terminates if FUNCTION returns something other than 0. */
void
elisp_maphash (maphash_function_t function,
	       Lisp_Object hash_table, void *extra_arg)
{
  const Lisp_Hash_Table * const ht = xhash_table (hash_table);
  Lisp_Object * const objs = copy_compress_hentries (ht);
  const Lisp_Object *pobj, *end;
  int speccount = specpdl_depth ();
  struct gcpro gcpro1;

  record_unwind_protect (maphash_unwind, make_opaque_ptr ((void *)objs));
  GCPRO1 (objs[0]);
  gcpro1.nvars = 2 * ht->count;

  for (pobj = objs, end = pobj + 2 * ht->count; pobj < end; pobj += 2)
    if (function (pobj[0], pobj[1], extra_arg))
      break;

  unbind_to (speccount);
  UNGCPRO;
}

/* Remove all elements of a lisp hash table satisfying *C* predicate PREDICATE.
   PREDICATE must not modify HASH-TABLE. */
void
elisp_map_remhash (maphash_function_t predicate,
		   Lisp_Object hash_table, void *extra_arg)
{
  const Lisp_Hash_Table * const ht = xhash_table (hash_table);
  Lisp_Object * const objs = copy_compress_hentries (ht);
  const Lisp_Object *pobj, *end;
  int speccount = specpdl_depth ();
  struct gcpro gcpro1;

  record_unwind_protect (maphash_unwind, make_opaque_ptr ((void *)objs));
  GCPRO1 (objs[0]);
  gcpro1.nvars = 2 * ht->count;

  for (pobj = objs, end = pobj + 2 * ht->count; pobj < end; pobj += 2)
    if (predicate (pobj[0], pobj[1], extra_arg))
      Fremhash (pobj[0], hash_table);

  unbind_to (speccount);
  UNGCPRO;
}


/************************************************************************/
/*		   garbage collecting weak hash tables			*/
/************************************************************************/
#ifdef USE_KKCC
#define MARK_OBJ(obj) do {				\
  Lisp_Object mo_obj = (obj);				\
  if (!marked_p (mo_obj))				\
    {							\
      kkcc_gc_stack_push_lisp_object_0 (mo_obj);	\
      did_mark = 1;					\
    }							\
} while (0)

#else /* NO USE_KKCC */

#define MARK_OBJ(obj) do {		\
  Lisp_Object mo_obj = (obj);		\
  if (!marked_p (mo_obj))		\
    {					\
      mark_object (mo_obj);		\
      did_mark = 1;			\
    }					\
} while (0)
#endif /*NO USE_KKCC */


/* Complete the marking for semi-weak hash tables. */
int
finish_marking_weak_hash_tables (void)
{
  Lisp_Object hash_table;
  int did_mark = 0;

  for (hash_table = Vall_weak_hash_tables;
       !NILP (hash_table);
       hash_table = XHASH_TABLE (hash_table)->next_weak)
    {
      const Lisp_Hash_Table *ht = XHASH_TABLE (hash_table);
      const htentry *e = ht->hentries;
      const htentry *sentinel = e + ht->size;

      if (! marked_p (hash_table))
	/* The hash table is probably garbage.  Ignore it. */
	continue;

      /* Now, scan over all the pairs.  For all pairs that are
	 half-marked, we may need to mark the other half if we're
	 keeping this pair. */
      switch (ht->weakness)
	{
	case HASH_TABLE_KEY_WEAK:
	  for (; e < sentinel; e++)
	    if (!HTENTRY_CLEAR_P (e))
	      if (marked_p (e->key))
		MARK_OBJ (e->value);
	  break;

	case HASH_TABLE_VALUE_WEAK:
	  for (; e < sentinel; e++)
	    if (!HTENTRY_CLEAR_P (e))
	      if (marked_p (e->value))
		MARK_OBJ (e->key);
	  break;

	case HASH_TABLE_KEY_VALUE_WEAK:
	  for (; e < sentinel; e++)
	    if (!HTENTRY_CLEAR_P (e))
	      {
		if (marked_p (e->value))
		  MARK_OBJ (e->key);
		else if (marked_p (e->key))
		  MARK_OBJ (e->value);
	      }
	  break;

	case HASH_TABLE_KEY_CAR_WEAK:
	  for (; e < sentinel; e++)
	    if (!HTENTRY_CLEAR_P (e))
	      if (!CONSP (e->key) || marked_p (XCAR (e->key)))
		{
		  MARK_OBJ (e->key);
		  MARK_OBJ (e->value);
		}
	  break;

	  /* We seem to be sprouting new weakness types at an alarming
	     rate. At least this is not externally visible - and in
	     fact all of these KEY_CAR_* types are only used by the
	     glyph code. */
	case HASH_TABLE_KEY_CAR_VALUE_WEAK:
	  for (; e < sentinel; e++)
	    if (!HTENTRY_CLEAR_P (e))
	      {
		if (!CONSP (e->key) || marked_p (XCAR (e->key)))
		  {
		    MARK_OBJ (e->key);
		    MARK_OBJ (e->value);
		  }
		else if (marked_p (e->value))
		  MARK_OBJ (e->key);
	      }
	  break;

	case HASH_TABLE_VALUE_CAR_WEAK:
	  for (; e < sentinel; e++)
	    if (!HTENTRY_CLEAR_P (e))
	      if (!CONSP (e->value) || marked_p (XCAR (e->value)))
		{
		  MARK_OBJ (e->key);
		  MARK_OBJ (e->value);
		}
	  break;

	default:
	  break;
	}
    }

  return did_mark;
}

void
prune_weak_hash_tables (void)
{
  Lisp_Object hash_table, prev = Qnil;
  for (hash_table = Vall_weak_hash_tables;
       !NILP (hash_table);
       hash_table = XHASH_TABLE (hash_table)->next_weak)
    {
      if (! marked_p (hash_table))
	{
	  /* This hash table itself is garbage.  Remove it from the list. */
	  if (NILP (prev))
	    Vall_weak_hash_tables = XHASH_TABLE (hash_table)->next_weak;
	  else
	    XHASH_TABLE (prev)->next_weak = XHASH_TABLE (hash_table)->next_weak;
	}
      else
	{
	  /* Now, scan over all the pairs.  Remove all of the pairs
	     in which the key or value, or both, is unmarked
	     (depending on the weakness of the hash table). */
	  Lisp_Hash_Table *ht = XHASH_TABLE (hash_table);
	  htentry *entries = ht->hentries;
	  htentry *sentinel = entries + ht->size;
	  htentry *e;

	  for (e = entries; e < sentinel; e++)
	    if (!HTENTRY_CLEAR_P (e))
	      {
	      again:
		if (!marked_p (e->key) || !marked_p (e->value))
		  {
		    remhash_1 (ht, entries, e);
		    if (!HTENTRY_CLEAR_P (e))
		      goto again;
		  }
	      }

	  prev = hash_table;
	}
    }
}

/* Return a hash value for an array of Lisp_Objects of size SIZE. */

Hashcode
internal_array_hash (Lisp_Object *arr, int size, int depth, Boolint equalp)
{
  int i;
  Hashcode hash = 0;
  depth++;

  if (size <= 5)
    {
      for (i = 0; i < size; i++)
	hash = HASH2 (hash, internal_hash (arr[i], depth, equalp));
      return hash;
    }

  /* just pick five elements scattered throughout the array.
     A slightly better approach would be to offset by some
     noise factor from the points chosen below. */
  for (i = 0; i < 5; i++)
    hash = HASH2 (hash, internal_hash (arr[i*size/5], depth, equalp));

  return hash;
}

/* This needs to be algorithmically the same as
   internal_array_hash(). Unfortunately, for strings with non-ASCII content,
   it has to be O(2N), I don't see a reasonable alternative to hashing
   sequence relying on their length. It is O(1) for pure ASCII strings,
   though. */

static Hashcode
string_equalp_hash (Lisp_Object string)
{
  Bytecount len = XSTRING_LENGTH (string),
    ascii_begin = (Bytecount) XSTRING_ASCII_BEGIN (string);
  const Ibyte *ptr = XSTRING_DATA (string), *pend = ptr + len;
  Charcount clen;
  Hashcode hash = 0;

  if (len == ascii_begin)
    {
      clen = len;
    }
  else
    {
      clen = string_char_length (string);
    }

  if (clen <= 5)
    {
      while (ptr < pend)
        {
          hash = HASH2 (hash,
                        LISP_HASH (make_char (CANONCASE (NULL,
                                                         itext_ichar (ptr)))));
          INC_IBYTEPTR (ptr);
        }
    }
  else
    {
      int ii;

      if (clen == len)
        {
          for (ii = 0; ii < 5; ii++)
            {
              hash = HASH2 (hash,
                            LISP_HASH (make_char
                                       (CANONCASE (NULL,
                                                   ptr[ii * clen / 5]))));
            }
        }
      else
        {
          Charcount this_char = 0, last_char = 0;
          for (ii = 0; ii < 5; ii++)
            {
              this_char = ii * clen / 5;
              ptr = itext_n_addr (ptr, this_char - last_char);
              last_char = this_char;

              hash = HASH2 (hash,
                            LISP_HASH (make_char
                                       (CANONCASE (NULL, itext_ichar (ptr)))));
            }
        }
    }

  return HASH2 (clen, hash);
}

/* Return a hash value for a Lisp_Object.  This is for use when hashing
   objects with the comparison being `equal' (for `eq', you can just
   use the Lisp_Object itself as the hash value).  You need to make a
   tradeoff between the speed of the hash function and how good the
   hashing is.  In particular, the hash function needs to be FAST,
   so you can't just traipse down the whole tree hashing everything
   together.  Most of the time, objects will differ in the first
   few elements you hash.  Thus, we only go to a short depth (5)
   and only hash at most 5 elements out of a vector.  Theoretically
   we could still take 5^5 time (a big big number) to compute a
   hash, but practically this won't ever happen. */

Hashcode
internal_hash (Lisp_Object obj, int depth, Boolint equalp)
{
  if (depth > 5)
    return 0;

  if (CONSP (obj)) 
    {
      Hashcode hash, h;
      int s;

      depth += 1;

      if (!CONSP (XCDR (obj)))
	{
	  /* special case for '(a . b) conses */
	  return HASH2 (internal_hash (XCAR(obj), depth, equalp),
			internal_hash (XCDR (obj), depth, equalp));
	}

      /* Don't simply tail recurse; we want to hash lists with the
	 same contents in distinct orders differently. */
      hash = internal_hash (XCAR (obj), depth, equalp);

      obj = XCDR (obj);
      for (s = 1; s < 6 && CONSP (obj); obj = XCDR (obj), s++)
	{
	  h = internal_hash (XCAR (obj), depth, equalp);
	  hash = HASH3 (hash, h, s);
	}

      return hash;
    }
  if (STRINGP (obj))
    {
      if (equalp)
        {
          return string_equalp_hash (obj);
        }

      return hash_string (XSTRING_DATA (obj), XSTRING_LENGTH (obj));
    }
  if (LRECORDP (obj))
    {
      const struct lrecord_implementation
	*imp = XRECORD_LHEADER_IMPLEMENTATION (obj);
      if (imp->hash)
	return imp->hash (obj, depth, equalp);
    }

  if (equalp)
    {
      if (CHARP (obj))
        {
          /* Characters and numbers of the same numeric value hash
             differently, which is fine, they're not equalp. */
          return LISP_HASH (make_char (CANONCASE (NULL, XCHAR (obj))));
        }

      if (FIXNUMP (obj))
        {
          return FLOAT_HASHCODE_FROM_DOUBLE ((double) (XFIXNUM (obj)));
        }
    }

  return LISP_HASH (obj);
}

DEFUN ("eq-hash", Feq_hash, 1, 1, 0, /*
Return a hash value for OBJECT appropriate for use with `eq.'

If OBJECT is not immediate (it is not a fixnum or character) this hash value
will be unique among currently-reachable objects, and is appropriate for
implementing the Common Lisp PRINT-OBJECT protocol.
*/
       (object))
{
  return make_integer ((EMACS_INT) XPNTRVAL (object));
}

DEFUN ("eql-hash", Feql_hash, 1, 1, 0, /*
Return a hash value for OBJECT appropriate for use with `eql.'
*/
       (object))
{
  EMACS_INT hashed = lisp_object_eql_hash (NULL, object);
  return make_integer (hashed);
}

DEFUN ("equal-hash", Fequal_hash, 1, 1, 0, /*
Return a hash value for OBJECT appropriate for use with `equal.'
\(equal obj1 obj2) implies (= (equal-hash obj1) (equal-hash obj2)).
*/
       (object))
{
  EMACS_INT hashed = internal_hash (object, 0, 0);
  return make_integer (hashed);
}

DEFUN ("equalp-hash", Fequalp_hash, 1, 1, 0, /*
Return a hash value for OBJECT appropriate for use with `equalp.'
*/
       (object))
{
  EMACS_INT hashed = internal_hash (object, 0, 1);
  return make_integer (hashed);
}

static Lisp_Object
make_hash_table_test (Lisp_Object name,
                      hash_table_equal_function_t equal_function,
                      hash_table_hash_function_t hash_function,
                      Lisp_Object lisp_equal_function,
                      Lisp_Object lisp_hash_function)
{
  Lisp_Object result = ALLOC_NORMAL_LISP_OBJECT (hash_table_test);
  Hash_Table_Test *http = XHASH_TABLE_TEST (result);

  http->name = name;
  http->equal_function = equal_function;
  http->hash_function = hash_function;
  http->lisp_equal_function = lisp_equal_function;
  http->lisp_hash_function = lisp_hash_function;

  return result;
}

Lisp_Object
define_hash_table_test (Lisp_Object name,
                        hash_table_equal_function_t equal_function,
                        hash_table_hash_function_t hash_function,
                        Lisp_Object lisp_equal_function,
                        Lisp_Object lisp_hash_function)
{
  Lisp_Object result = make_hash_table_test (name, equal_function,
                                             hash_function,
                                             lisp_equal_function,
                                             lisp_hash_function);
  XWEAK_LIST_LIST (Vhash_table_test_weak_list)
    = Fcons (Fcons (name, result),
             XWEAK_LIST_LIST (Vhash_table_test_weak_list));

  return result;
}

DEFUN ("define-hash-table-test", Fdefine_hash_table_test, 3, 3, 0, /*
Define a new hash table test with name NAME, a symbol.

In a hash table created with NAME as its test, use EQUAL-FUNCTION to compare
keys, and HASH-FUNCTION for computing hash codes of keys.

EQUAL-FUNCTION must be a function taking two arguments and returning non-nil
if both arguments are the same.  HASH-FUNCTION must be a function taking one
argument and returning an integer that is the hash code of the argument.

Computation should use the whole value range of the underlying machine long
type.  In XEmacs this will necessitate bignums for values above
`most-positive-fixnum' but below (1+ (* most-positive-fixnum 2)) and
analogous values below `most-negative-fixnum'.  Relatively poor hashing
performance is guaranteed in a build without bignums.

This function returns t if successful, and errors if NAME
cannot be defined as a hash table test.
*/
       (name, equal_function, hash_function))
{
  Lisp_Object min, max, lookup;

  CHECK_SYMBOL (name);

  lookup = Fassq (name, XWEAK_LIST_LIST (Vhash_table_test_weak_list));

  if (!NILP (lookup))
    {
      invalid_change ("Cannot redefine existing hash table test", name);
    }

  min = Ffunction_min_args (equal_function);
  max = Ffunction_max_args (equal_function);

  if (!((XFIXNUM (min) <= 2) && (NILP (max) || 2 <= XFIXNUM (max))))
    {
      signal_wrong_number_of_arguments_error (equal_function, 2);
    }

  min = Ffunction_min_args (hash_function);
  max = Ffunction_max_args (hash_function);

  if (!((XFIXNUM (min) <= 1) && (NILP (max) || 1 <= XFIXNUM (max))))
    {
      signal_wrong_number_of_arguments_error (hash_function, 1);
    }

  define_hash_table_test (name, lisp_object_general_equal,
                          lisp_object_general_hash, equal_function,
                          hash_function);
  return Qt;
}

DEFUN ("valid-hash-table-test-p", Fvalid_hash_table_test_p, 1, 1, 0, /*
Return t if OBJECT names a hash table test, nil otherwise.

A valid hash table test is one of the symbols `eq', `eql', `equal',
`equalp', or some symbol passed as the NAME argument to
`define-hash-table-test'.  As a special case, `nil' is regarded as
equivalent to `eql'.
*/
       (object))
{
  Lisp_Object lookup;

  if (NILP (object))
    {
      return Qt;
    }

  lookup = Fassq (object, XWEAK_LIST_LIST (Vhash_table_test_weak_list));

  if (!NILP (lookup))
    {
      return Qt;
    }

  return Qnil;
}

DEFUN ("hash-table-test-list", Fhash_table_test_list, 0, 0, 0, /*
Return a list of symbols naming valid hash table tests.
These can be passed as the value of the TEST keyword to `make-hash-table'.
This list does not include nil, regarded as equivalent to `eql' by
`make-hash-table'.
*/
       ())
{
  Lisp_Object result = Qnil;

  LIST_LOOP_2 (test, XWEAK_LIST_LIST (Vhash_table_test_weak_list))
    {
      if (!UNBOUNDP (XCAR (test)))
        {
          result = Fcons (XCAR (test), result);
        }
    }

  return result;
}

DEFUN ("hash-table-test-equal-function",
       Fhash_table_test_equal_function, 1, 1, 0, /*
Return the comparison function used for hash table test TEST.
See `define-hash-table-test' and `make-hash-table'.
*/
       (test))
{
  Lisp_Object lookup;

  if (NILP (test))
    {
      test = Qeql;
    }

  lookup = Fassq (test, XWEAK_LIST_LIST (Vhash_table_test_weak_list));
  if (NILP (lookup))
    {
      invalid_argument ("Not a defined hash table test", test);
    }

  return XHASH_TABLE_TEST (XCDR (lookup))->lisp_equal_function;
}

DEFUN ("hash-table-test-hash-function",
       Fhash_table_test_hash_function, 1, 1, 0, /*
Return the hash function used for hash table test TEST.
See `define-hash-table-test' and `make-hash-table'.
*/
       (test))
{
  Lisp_Object lookup;

  if (NILP (test))
    {
      test = Qeql;
    }

  lookup = Fassq (test, XWEAK_LIST_LIST (Vhash_table_test_weak_list));
  if (NILP (lookup))
    {
      invalid_argument ("Not a defined hash table test", test);
    }

  return XHASH_TABLE_TEST (XCDR (lookup))->lisp_hash_function;
}

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
hash_table_objects_create (void)
{
#ifdef MEMORY_USAGE_STATS
  OBJECT_HAS_METHOD (hash_table, memory_usage);
#endif
  OBJECT_HAS_METHOD (hash_table, print_preprocess);
  OBJECT_HAS_METHOD (hash_table, nsubst_structures_descend);
}

void
syms_of_elhash (void)
{
  DEFSUBR (Fhash_table_p);
  DEFSUBR (Fmake_hash_table);
  DEFSUBR (Fcopy_hash_table);
  DEFSUBR (Fgethash);
  DEFSUBR (Fremhash);
  DEFSUBR (Fputhash);
  DEFSUBR (Fclrhash);
  DEFSUBR (Fmaphash);
  DEFSUBR (Fhash_table_count);
  DEFSUBR (Fhash_table_test);
  DEFSUBR (Fhash_table_size);
  DEFSUBR (Fhash_table_rehash_size);
  DEFSUBR (Fhash_table_rehash_threshold);
  DEFSUBR (Fhash_table_weakness);
  DEFSUBR (Fhash_table_type); /* obsolete */

  DEFSUBR (Feq_hash);
  DEFSUBR (Feql_hash);
  DEFSUBR (Fequal_hash);
  Ffset (intern ("sxhash"), intern ("equal-hash"));
  DEFSUBR (Fequalp_hash);

  DEFSUBR (Fdefine_hash_table_test);
  DEFSUBR (Fvalid_hash_table_test_p);
  DEFSUBR (Fhash_table_test_list);
  DEFSUBR (Fhash_table_test_equal_function);
  DEFSUBR (Fhash_table_test_hash_function);

  DEFSYMBOL_MULTIWORD_PREDICATE (Qhash_tablep);

  DEFSYMBOL (Qhash_table);
  DEFSYMBOL (Qhashtable);
  DEFSYMBOL (Qmake_hash_table);
  DEFSYMBOL (Qweakness);
  DEFSYMBOL (Qvalue);
  DEFSYMBOL (Qkey_or_value);
  DEFSYMBOL (Qkey_and_value);
  DEFSYMBOL (Qrehash_size);
  DEFSYMBOL (Qrehash_threshold);

  DEFSYMBOL (Qweak);             /* obsolete */
  DEFSYMBOL (Qkey_weak);     /* obsolete */
  DEFSYMBOL (Qkey_or_value_weak);    /* obsolete */
  DEFSYMBOL (Qvalue_weak); /* obsolete */
  DEFSYMBOL (Qnon_weak);     /* obsolete */

  DEFKEYWORD (Q_data);
  DEFKEYWORD (Q_size);
  DEFKEYWORD (Q_rehash_size);
  DEFKEYWORD (Q_rehash_threshold);
  DEFKEYWORD (Q_weakness);
}

void
vars_of_elhash (void)
{
  Lisp_Object weak_list_list = XWEAK_LIST_LIST (Vhash_table_test_weak_list);

  /* This var was staticpro'd and initialised in
     init_elhash_once_early, but its Vall_weak_lists isn't sane, since
     that was done before vars_of_data() was called. Create a sane
     weak list object now, set its list appropriately, assert that our
     data haven't been garbage collected. */
  assert (!NILP (Fassq (Qeq, weak_list_list)));
  assert (!NILP (Fassq (Qeql, weak_list_list)));
  assert (!NILP (Fassq (Qequal, weak_list_list)));
  assert (!NILP (Fassq (Qequalp, weak_list_list)));
  assert (4 == XFIXNUM (Flength (weak_list_list)));

  Vhash_table_test_weak_list = make_weak_list (WEAK_LIST_KEY_ASSOC);
  XWEAK_LIST_LIST (Vhash_table_test_weak_list) = weak_list_list;

#ifdef MEMORY_USAGE_STATS
  OBJECT_HAS_PROPERTY
    (hash_table, memusage_stats_list, list1 (intern ("hash-entries")));
#endif /* MEMORY_USAGE_STATS */
}

void
init_elhash_once_early (void)
{
  INIT_LISP_OBJECT (hash_table);
  INIT_LISP_OBJECT (hash_table_test);

#ifdef NEW_GC
  INIT_LISP_OBJECT (hash_table_entry);
#endif /* NEW_GC */

  /* init_elhash_once_early() is called very early, we can't have these
     DEFSYMBOLs in syms_of_elhash(), unfortunately. */

  DEFSYMBOL (Qeq);
  DEFSYMBOL (Qeql);
  DEFSYMBOL (Qequal);
  DEFSYMBOL (Qequalp);

  DEFSYMBOL (Qeq_hash);
  DEFSYMBOL (Qeql_hash);
  DEFSYMBOL (Qequal_hash);
  DEFSYMBOL (Qequalp_hash);

  /* This must NOT be staticpro'd */
  Vall_weak_hash_tables = Qnil;
  dump_add_weak_object_chain (&Vall_weak_hash_tables);
 
  staticpro (&Vhash_table_test_weak_list);
  Vhash_table_test_weak_list = make_weak_list (WEAK_LIST_KEY_ASSOC);

  staticpro (&Vhash_table_test_eq);
  Vhash_table_test_eq = define_hash_table_test (Qeq, NULL, NULL, Qeq, Qeq_hash);
  staticpro (&Vhash_table_test_eql);
  Vhash_table_test_eql
    = define_hash_table_test (Qeql, lisp_object_eql_equal,
                              lisp_object_eql_hash, Qeql, Qeql_hash);
  (void) define_hash_table_test (Qequal, lisp_object_equal_equal,
                                 lisp_object_equal_hash, Qequal, Qequal_hash);
  (void) define_hash_table_test (Qequalp, lisp_object_equalp_equal,
                                 lisp_object_equalp_hash, Qequalp, Qequalp_hash);
}
