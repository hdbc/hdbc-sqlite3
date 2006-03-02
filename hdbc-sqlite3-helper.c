#include <sqlite3.h>
#include <stdio.h>
#include <stdlib.h>
#include "hdbc-sqlite3-helper.h"

int sqlite3_bind_text2(sqlite3_stmt* a, int b, const char *c, int d) {
    return sqlite3_bind_text(a, b, c, d, SQLITE_TRANSIENT);
}

/* Sqlite things can't finalize more than once.  
We'd like to let people call them from the app to get the error, if any.
Yet we'd also like to be able to have a ForeignPtr finalize them.

So, here's a little wrapper for things. */
  

int sqlite3_open2(const char *filename, finalizeonce **ppo) {
  sqlite3 *ppDb;
  finalizeonce *newobj;
  int res;

  res = sqlite3_open(filename, &ppDb);
  newobj = malloc(sizeof(finalizeonce));
  if (newobj == NULL) {
    fprintf(stderr, "\nhdbc sqlite internal error: couldn't malloc memory for newobj\n");
    return -999;
  }
  newobj->encapobj = (void *) ppDb;
  newobj->isfinalized = 0;
  newobj->refcount = 1;
  newobj->parent = NULL;
  *ppo = newobj;
#ifdef DEBUG_HDBC_SQLITE3
  fprintf(stderr, "\nAllocated db at %p %p\n", newobj, newobj->encapobj);
#endif
  return res;
}

int sqlite3_close_app(finalizeonce *ppdb) {
  int res;
  if (ppdb->isfinalized) {
#ifdef DEBUG_HDBC_SQLITE3
    fprintf(stderr, "\nclose_app on already finalized %p\n", ppdb);
#endif
    return SQLITE_OK;
  }
#ifdef DEBUG_HDBC_SQLITE3
  fprintf(stderr, "\nclose_app on non-finalized %p\n", ppdb);
#endif
  res = sqlite3_close((sqlite3 *) (ppdb->encapobj));
  ppdb->isfinalized = 1;
  return res;
}

void sqlite3_close_finalizer(finalizeonce *ppdb) {
#ifdef DEBUG_HDBC_SQLITE3
  fprintf(stderr, "\nclose_finalizer on %p: %d\n", ppdb, ppdb->isfinalized);
#endif
  (ppdb->refcount)--;
  sqlite3_conditional_finalizer(ppdb);
}

void sqlite3_conditional_finalizer(finalizeonce *ppdb) {
#ifdef DEBUG_HDBC_SQLITE3
  fprintf(stderr, "\ncond finalizer on %p: refcount %d\n", ppdb, ppdb->refcount);
#endif
  if (ppdb->refcount < 1) {
    sqlite3_close_app(ppdb);
    free(ppdb);
  }
}

int sqlite3_prepare2(finalizeonce *fdb, const char *zSql,
                     int nBytes, finalizeonce **ppo,
                     const char **pzTail) {

  sqlite3_stmt *ppst;
  sqlite3 *db;
  finalizeonce *newobj;
  int res;

  db = (sqlite3 *) fdb->encapobj;

#ifdef DEBUG_HDBC_SQLITE3
  fprintf(stderr, "\nCalling prepare on %p", db);
#endif
  res = sqlite3_prepare(db, zSql, nBytes, &ppst,
                        pzTail);
  /* We don't try to deallocate this in Haskell if there
     was an error. */
  if (res != SQLITE_OK) {
    if (ppst != NULL) {
      sqlite3_finalize(ppst);
    }
    return res;
   
  }

  newobj = malloc(sizeof(finalizeonce));
  if (newobj == NULL) {
    fprintf(stderr, "\nhdbc sqlite3 internal error: couldn't malloc memory for newobj\n");
    return -999;
  }
  newobj->encapobj = (void *) ppst;
  newobj->isfinalized = 0;
  newobj->parent = fdb;
  newobj->refcount = 1;
  (fdb->refcount)++;
  *ppo = newobj;
#ifdef DEBUG_HDBC_SQLITE3
  fprintf(stderr, "\nAllocated stmt at %p %p\n", newobj, newobj->encapobj);
#endif
  return res;
}

int sqlite3_finalize_app(finalizeonce *ppst) {
  int res;
  if (ppst->isfinalized) {
#ifdef DEBUG_HDBC_SQLITE3
    fprintf(stderr, "\nfinalize_app on already finalized %p\n", ppst);
#endif
    return SQLITE_OK;
  }
#ifdef DEBUG_HDBC_SQLITE3
  fprintf(stderr, "\nfinalize_app on non-finalized %p\n", ppst);
#endif
  res = sqlite3_finalize((sqlite3_stmt *) (ppst->encapobj));
  ppst->isfinalized = 1;
  return res;
}

void sqlite3_finalize_finalizer(finalizeonce *ppst) {
#ifdef DEBUG_HDBC_SQLITE3
  fprintf(stderr, "\nfinalize_finalizer on %p: %d\n", ppst, ppst->isfinalized);
#endif
  sqlite3_finalize_app(ppst);
  (ppst->refcount)--;           /* Not really important since no children use 
                                   us */
  /* Now decrement the refcount for the parent */
  (ppst->parent->refcount)--;
  sqlite3_conditional_finalizer(ppst->parent);
  free(ppst);
}
