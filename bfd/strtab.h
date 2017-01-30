#include "bfd.h"

struct strtab;

struct strtab *strtab_new(bfd *abfd);
void strtab_free (struct strtab *st);
int strtab_size(struct strtab const *st);
int strtab_add(struct strtab *st, void *s);
void *strtab_lookup(struct strtab *st, int i);
