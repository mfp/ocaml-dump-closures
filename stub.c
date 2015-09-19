
#include <stdlib.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <stdio.h>

/* taken from OCaml sources for 64-bit platforms */

#ifdef ARCH_SIXTYFOUR

#define Not_in_heap 0
#define In_heap 1
#define In_young 2
#define In_static_data 4
#define In_code_area 8

typedef struct {
  void *block;           /* address of the malloced block this chunk live in */
  size_t alloc;         /* in bytes, used for compaction */
  size_t size;          /* in bytes */
  char *next;
} heap_chunk_head;

#define Chunk_size(c) (((heap_chunk_head *) (c)) [-1]).size
#define Chunk_alloc(c) (((heap_chunk_head *) (c)) [-1]).alloc
#define Chunk_next(c) (((heap_chunk_head *) (c)) [-1]).next
#define Chunk_block(c) (((heap_chunk_head *) (c)) [-1]).block

CAMLextern char *caml_heap_start;

/* 64 bits: Represent page table as a sparse hash table */
int caml_page_table_lookup(void * addr);
#define Classify_addr(a) (caml_page_table_lookup((void *)(a)))

#define Is_in_value_area(a) \
  (Classify_addr(a) & (In_heap | In_young | In_static_data))

void capture_params(value v, value **closures2, long *nclosures2, long *capa2, int depth)
{
 int i;
 header_t hd = Hd_val(v);

 if(depth >= 10) { return; }

 for(i = 0; i < Wosize_hd(hd); i++) {
     if(Is_block(Field(v, i)) && Is_in_value_area(Field(v, i))) {
         if(*nclosures2 >= *capa2) {
             *capa2 *= 2;
             void *p = realloc(*closures2, *capa2 * sizeof(value));
             if(!p) caml_failwith("dump_closure_addresses: could not alloc result array");
             *closures2 = p;
         }
         (*closures2)[(*nclosures2)++] = Val_long(Field(v, i));
         if(Tag_hd(Hd_val(Field(v, i))) == Closure_tag) {
             capture_params(Field(v, i), closures2, nclosures2, capa2, depth + 1);
         }
     }
 }
}

CAMLprim value dump_closure_addresses(value unit)
{
 CAMLparam0();
 CAMLlocal4(v, ret, addresses, params);
 char *chunk, *limit, *hp;
 header_t hd;
 mlsize_t size;
 long count = 0;
 long bytes = 0;
 long capa = 32768, capa2 = 32768, nclosures = 0, nclosures2 = 0;
 value *closures = malloc(32768 * sizeof(value));
 value *closures2 = malloc(32768 * sizeof(value));
 int i;

 if(!closures || !closures) {
     if(closures) free(closures);
     if(closures2) free(closures2);
     caml_failwith("dump_closure_addresses: could not alloc result array");
 }

 chunk = caml_heap_start;
 limit = chunk + Chunk_size(chunk);

 do {
       hp = chunk;
       do {
           hd = Hd_hp(hp);
           size = Bhsize_hd(hd);
           v = Val_hp(hp);
           /* printf("v: %p, size: %zd\n", hp, size); */
           if(size == 0) break;
           /* printf("tag: %d\n", Tag_hd(hd)); */
           if(Tag_hd(hd) == Closure_tag) {
               if(nclosures >= capa) {
                   capa *= 2;
                   void *p = realloc(closures, capa * sizeof(value));
                   if(!p) {
                       free(closures);
                       caml_failwith("dump_closure_addresses: could not alloc result array");
                   }
                   closures = p;
               }
               closures[nclosures++] = Val_long(Code_val(Val_hp(hp)));

               /* comment out to disable param capture */
               capture_params(v, &closures2, &nclosures2, &capa2, 0);
               count++;
               bytes += size;
           }
           hp += Bhsize_hd(hd);
       } while(hp < limit);

       chunk = Chunk_next(chunk);
       if(!chunk) break;
       limit = chunk + Chunk_size(chunk);
 } while(chunk);

 addresses = caml_alloc(nclosures, 0);
 params = caml_alloc(nclosures2, 0);
 for(i = 0; i < nclosures; i++) {
     Field(addresses, i) = closures[i];
 }
 for(i = 0; i < nclosures2; i++) {
     Field(params, i) = closures2[i];
 }
 ret = caml_alloc(4, 0);
 Store_field(ret, 0, Val_long(count));
 Store_field(ret, 1, Val_long(bytes));
 Store_field(ret, 2, addresses);
 Store_field(ret, 3, params);

 free(closures);
 free(closures2);

 CAMLreturn(ret);
}

#else /* ARCH_SIXTYFOUR */

CAMLprim value dump_closure_addresses(value unit)
{
 CAMLparam0();
 CAMLlocal2(v, ret);

 v = caml_alloc(0, 0);
 ret = caml_alloc(4, 0);
 Store_field(ret, 0, Val_long(0));
 Store_field(ret, 1, Val_long(0));
 Store_field(ret, 2, v);
 Store_field(ret, 3, v);

 CAMLreturn(ret);
}

#endif

