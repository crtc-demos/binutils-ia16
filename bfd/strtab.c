#include "sysdep.h"
#include "bfd.h"
#include "strtab.h"
#include "libbfd.h"
#include "strtab.h"

#define STRTAB_INIT_SIZE 256

struct strtab {
  bfd *owner;
  void **strings;
  int strings_used;
  int strings_size;
};

struct strtab *
strtab_new(bfd *abfd)
{
  struct strtab *st;

  st = bfd_malloc (sizeof (*st));
  if (st == NULL)
    return NULL;
  st->owner = abfd;
  st->strings = bfd_malloc2 (STRTAB_INIT_SIZE, sizeof (*st->strings));
  if (st->strings == NULL)
    {
      free (st);
      return NULL;
    }
  st->strings_used = 0;
  st->strings_size = STRTAB_INIT_SIZE;

  return st;
}

void
strtab_free (struct strtab *st ATTRIBUTE_UNUSED)
{
  free (st->strings);
  free (st);
}

int strtab_size(struct strtab const *st)
{
  return (st->strings_used);
}

int strtab_add(struct strtab *st, void *s)
{
  if (st->strings_used >= st->strings_size)
    {
      void **newstrings;
      int newsize = st->strings_size * 2;

      newstrings = bfd_realloc2 (st->strings, newsize, sizeof (*st->strings));
      if (newstrings == NULL)
	{
	  (*_bfd_error_handler) ("strings_used %d >= strings_size %d",
				 st->strings_used, st->strings_size);
	  abort();
	}

      st->strings = newstrings;
      st->strings_size = newsize;
    }

  st->strings[st->strings_used++] = s;

  return (st->strings_used - 1);
}

void *
strtab_lookup(struct strtab *st, int i)
{
  if (i >= st->strings_used)
    return NULL;

  return (st->strings[i]);
}
