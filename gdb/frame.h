/* Definitions for dealing with stack frames, for GDB, the GNU debugger.

   Copyright 1986, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1996,
   1997, 1998, 1999, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.

   This file is part of GDB.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#if !defined (FRAME_H)
#define FRAME_H 1

struct symtab_and_line;
struct frame_unwind;
struct frame_base;
struct block;
struct gdbarch;
struct ui_file;

/* A legacy unwinder to prop up architectures using the old style
   saved regs array.  */
extern const struct frame_unwind *legacy_saved_regs_unwind;

/* The frame object.  */

struct frame_info;

/* The frame object's ID.  This provides a per-frame unique identifier
   that can be used to relocate a `struct frame_info' after a target
   resume or a frame cache destruct.  It of course assumes that the
   inferior hasn't unwound the stack past that frame.  */

struct frame_id
{
  /* The frame's stack address.  This shall be constant through out
     the lifetime of a frame.  Note that this requirement applies to
     not just the function body, but also the prologue and (in theory
     at least) the epilogue.  Since that value needs to fall either on
     the boundary, or within the frame's address range, the frame's
     outer-most address (the inner-most address of the previous frame)
     is used.  Watch out for all the legacy targets that still use the
     function pointer register or stack pointer register.  They are
     wrong.  */
  /* NOTE: cagney/2002-11-16: The ia64 has two stacks and hence two
     frame bases.  This will need to be expanded to accomodate that.  */
  CORE_ADDR stack_addr;
  /* The frame's code address.  This shall be constant through out the
     lifetime of the frame.  While the PC (a.k.a. resume address)
     changes as the function is executed, this code address cannot.
     Typically, it is set to the address of the entry point of the
     frame's function (as returned by frame_func_unwind().  */
  CORE_ADDR code_addr;
};

/* Methods for constructing and comparing Frame IDs.

   NOTE: Given frameless functions A and B, where A calls B (and hence
   B is inner-to A).  The relationships: !eq(A,B); !eq(B,A);
   !inner(A,B); !inner(B,A); all hold.  This is because, while B is
   inner to A, B is not strictly inner to A (being frameless, they
   have the same .base value).  */

/* For convenience.  All fields are zero.  */
extern const struct frame_id null_frame_id;

/* Construct a frame ID.  The first parameter is the frame's constant
   stack address (typically the outer-bound), and the second the
   frame's constant code address (typically the entry point) (or zero,
   to indicate a wild card).  */
extern struct frame_id frame_id_build (CORE_ADDR stack_addr,
				       CORE_ADDR code_addr);

/* Returns non-zero when L is a valid frame (a valid frame has a
   non-zero .base).  */
extern int frame_id_p (struct frame_id l);

/* Returns non-zero when L and R identify the same frame, or, if
   either L or R have a zero .func, then the same frame base.  */
extern int frame_id_eq (struct frame_id l, struct frame_id r);

/* Returns non-zero when L is strictly inner-than R (they have
   different frame .bases).  Neither L, nor R can be `null'.  See note
   above about frameless functions.  */
extern int frame_id_inner (struct frame_id l, struct frame_id r);

/* Write the internal representation of a frame ID on the specified
   stream.  */
extern void fprint_frame_id (struct ui_file *file, struct frame_id id);


/* For every stopped thread, GDB tracks two frames: current and
   selected.  Current frame is the inner most frame of the selected
   thread.  Selected frame is the one being examined by the the GDB
   CLI (selected using `up', `down', ...).  The frames are created
   on-demand (via get_prev_frame()) and then held in a frame cache.  */
/* FIXME: cagney/2002-11-28: Er, there is a lie here.  If you do the
   sequence: `thread 1; up; thread 2; thread 1' you loose thread 1's
   selected frame.  At present GDB only tracks the selected frame of
   the current thread.  But be warned, that might change.  */
/* FIXME: cagney/2002-11-14: At any time, only one thread's selected
   and current frame can be active.  Switching threads causes gdb to
   discard all that cached frame information.  Ulgh!  Instead, current
   and selected frame should be bound to a thread.  */

/* On demand, create the inner most frame using information found in
   the inferior.  If the inner most frame can't be created, throw an
   error.  */
extern struct frame_info *get_current_frame (void);

/* Invalidates the frame cache (this function should have been called
   invalidate_cached_frames).

   FIXME: cagney/2002-11-28: The only difference between
   flush_cached_frames() and reinit_frame_cache() is that the latter
   explicitly sets the selected frame back to the current frame there
   isn't any real difference (except that one delays the selection of
   a new frame).  Code can instead simply rely on get_selected_frame()
   to reinit's the selected frame as needed.  As for invalidating the
   cache, there should be two methods one that reverts the thread's
   selected frame back to current frame (for when the inferior
   resumes) and one that does not (for when the user modifies the
   target invalidating the frame cache).  */
extern void flush_cached_frames (void);
extern void reinit_frame_cache (void);

/* On demand, create the selected frame and then return it.  If the
   selected frame can not be created, this function throws an error.  */
/* FIXME: cagney/2002-11-28: At present, when there is no selected
   frame, this function always returns the current (inner most) frame.
   It should instead, when a thread has previously had its frame
   selected (but not resumed) and the frame cache invalidated, find
   and then return that thread's previously selected frame.  */
extern struct frame_info *get_selected_frame (void);

/* Select a specific frame.  NULL, apparently implies re-select the
   inner most frame.  */
extern void select_frame (struct frame_info *);

/* Given a FRAME, return the next (more inner, younger) or previous
   (more outer, older) frame.  */
extern struct frame_info *get_prev_frame (struct frame_info *);
extern struct frame_info *get_next_frame (struct frame_info *);

/* Given a frame's ID, relocate the frame.  Returns NULL if the frame
   is not found.  */
extern struct frame_info *frame_find_by_id (struct frame_id id);

/* Base attributes of a frame: */

/* The frame's `resume' address.  Where the program will resume in
   this frame.

   This replaced: frame->pc; */
extern CORE_ADDR get_frame_pc (struct frame_info *);

/* Following on from the `resume' address.  Return the entry point
   address of the function containing that resume address, or zero if
   that function isn't known.  */
extern CORE_ADDR frame_func_unwind (struct frame_info *fi);
extern CORE_ADDR get_frame_func (struct frame_info *fi);

/* Closely related to the resume address, various symbol table
   attributes that are determined by the PC.  Note that for a normal
   frame, the PC refers to the resume address after the return, and
   not the call instruction.  In such a case, the address is adjusted
   so that it (approximatly) identifies the call site (and not return
   site).

   NOTE: cagney/2002-11-28: The frame cache could be used to cache the
   computed value.  Working on the assumption that the bottle-neck is
   in the single step code, and that code causes the frame cache to be
   constantly flushed, caching things in a frame is probably of little
   benefit.  As they say `show us the numbers'.

   NOTE: cagney/2002-11-28: Plenty more where this one came from:
   find_frame_block(), find_frame_partial_function(),
   find_frame_symtab(), find_frame_function().  Each will need to be
   carefully considered to determine if the real intent was for it to
   apply to the PC or the adjusted PC.  */
extern void find_frame_sal (struct frame_info *frame,
			    struct symtab_and_line *sal);

/* Return the frame base (what ever that is) (DEPRECATED).

   Old code was trying to use this single method for two conflicting
   purposes.  Such code needs to be updated to use either of:

   get_frame_id: A low level frame unique identifier, that consists of
   both a stack and a function address, that can be used to uniquely
   identify a frame.  This value is determined by the frame's
   low-level unwinder, the stack part [typically] being the
   top-of-stack of the previous frame, and the function part being the
   function's start address.  Since the correct identification of a
   frameless function requires both the a stack and function address,
   the old get_frame_base method was not sufficient.

   get_frame_base_address: get_frame_locals_address:
   get_frame_args_address: A set of high-level debug-info dependant
   addresses that fall within the frame.  These addresses almost
   certainly will not match the stack address part of a frame ID (as
   returned by get_frame_base).

   This replaced: frame->frame; */

extern CORE_ADDR get_frame_base (struct frame_info *);

/* Return the per-frame unique identifer.  Can be used to relocate a
   frame after a frame cache flush (and other similar operations).  If
   FI is NULL, return the null_frame_id.  */
extern struct frame_id get_frame_id (struct frame_info *fi);

/* Assuming that a frame is `normal', return its base-address, or 0 if
   the information isn't available.  NOTE: This address is really only
   meaningful to the frame's high-level debug info.  */
extern CORE_ADDR get_frame_base_address (struct frame_info *);

/* Assuming that a frame is `normal', return the base-address of the
   local variables, or 0 if the information isn't available.  NOTE:
   This address is really only meaningful to the frame's high-level
   debug info.  Typically, the argument and locals share a single
   base-address.  */
extern CORE_ADDR get_frame_locals_address (struct frame_info *);

/* Assuming that a frame is `normal', return the base-address of the
   parameter list, or 0 if that information isn't available.  NOTE:
   This address is really only meaningful to the frame's high-level
   debug info.  Typically, the argument and locals share a single
   base-address.  */
extern CORE_ADDR get_frame_args_address (struct frame_info *);

/* The frame's level: 0 for innermost, 1 for its caller, ...; or -1
   for an invalid frame).  */
extern int frame_relative_level (struct frame_info *fi);

/* Return the frame's type.  Some are real, some are signal
   trampolines, and some are completly artificial (dummy).  */

enum frame_type
{
  /* The frame's type hasn't yet been defined.  This is a catch-all
     for legacy code that uses really strange technicques, such as
     deprecated_set_frame_type, to set the frame's type.  New code
     should not use this value.  */
  UNKNOWN_FRAME,
  /* A true stack frame, created by the target program during normal
     execution.  */
  NORMAL_FRAME,
  /* A fake frame, created by GDB when performing an inferior function
     call.  */
  DUMMY_FRAME,
  /* In a signal handler, various OSs handle this in various ways.
     The main thing is that the frame may be far from normal.  */
  SIGTRAMP_FRAME
};
extern enum frame_type get_frame_type (struct frame_info *);

/* FIXME: cagney/2002-11-10: Some targets want to directly mark a
   frame as being of a specific type.  This shouldn't be necessary.
   PC_IN_SIGTRAMP() indicates a SIGTRAMP_FRAME and
   DEPRECATED_PC_IN_CALL_DUMMY() indicates a DUMMY_FRAME.  I suspect
   the real problem here is that get_prev_frame() only sets
   initialized after DEPRECATED_INIT_EXTRA_FRAME_INFO as been called.
   Consequently, some targets found that the frame's type was wrong
   and tried to fix it.  The correct fix is to modify get_prev_frame()
   so that it initializes the frame's type before calling any other
   functions.  */
extern void deprecated_set_frame_type (struct frame_info *,
				       enum frame_type type);

/* Unwind the stack frame so that the value of REGNUM, in the previous
   (up, older) frame is returned.  If VALUEP is NULL, don't
   fetch/compute the value.  Instead just return the location of the
   value.  */
extern void frame_register_unwind (struct frame_info *frame, int regnum,
				   int *optimizedp, enum lval_type *lvalp,
				   CORE_ADDR *addrp, int *realnump,
				   void *valuep);

/* More convenient interface to frame_register_unwind().  */
/* NOTE: cagney/2002-09-13: Return void as one day these functions may
   be changed to return an indication that the read succeeded.  */

extern void frame_unwind_register (struct frame_info *frame,
				   int regnum, void *buf);

extern void frame_unwind_signed_register (struct frame_info *frame,
					  int regnum, LONGEST *val);

extern void frame_unwind_unsigned_register (struct frame_info *frame,
					    int regnum, ULONGEST *val);

/* Get the value of the register that belongs to this FRAME.  This
   function is a wrapper to the call sequence ``frame_unwind_register
   (get_next_frame (FRAME))''.  As per frame_register_unwind(), if
   VALUEP is NULL, the registers value is not fetched/computed.  */

extern void frame_register (struct frame_info *frame, int regnum,
			    int *optimizedp, enum lval_type *lvalp,
			    CORE_ADDR *addrp, int *realnump,
			    void *valuep);

/* More convenient interface to frame_register().  */
/* NOTE: cagney/2002-09-13: Return void as one day these functions may
   be changed to return an indication that the read succeeded.  */

extern void frame_read_register (struct frame_info *frame, int regnum,
				 void *buf);

extern void frame_read_signed_register (struct frame_info *frame,
					int regnum, LONGEST *val);

extern void frame_read_unsigned_register (struct frame_info *frame,
					  int regnum, ULONGEST *val);

/* Map between a frame register number and its name.  A frame register
   space is a superset of the cooked register space --- it also
   includes builtin registers.  If NAMELEN is negative, use the NAME's
   length when doing the comparison.  */

extern int frame_map_name_to_regnum (const char *name, int namelen);
extern const char *frame_map_regnum_to_name (int regnum);

/* Unwind the PC.  Strictly speaking return the resume address of the
   calling frame.  For GDB, `pc' is the resume address and not a
   specific register.  */

extern CORE_ADDR frame_pc_unwind (struct frame_info *frame);

/* Discard the specified frame.  Restoring the registers to the state
   of the caller.  */
extern void frame_pop (struct frame_info *frame);

/* Values for the source flag to be used in print_frame_info_base(). */
enum print_what
  { 
    /* Print only the source line, like in stepi. */
    SRC_LINE = -1, 
    /* Print only the location, i.e. level, address (sometimes)
       function, args, file, line, line num. */
    LOCATION,
    /* Print both of the above. */
    SRC_AND_LOC, 
    /* Print location only, but always include the address. */
    LOC_AND_ADDRESS 
  };

/* Allocate additional space for appendices to a struct frame_info.
   NOTE: Much of GDB's code works on the assumption that the allocated
   saved_regs[] array is the size specified below.  If you try to make
   that array smaller, GDB will happily walk off its end. */

#ifdef SIZEOF_FRAME_SAVED_REGS
#error "SIZEOF_FRAME_SAVED_REGS can not be re-defined"
#endif
#define SIZEOF_FRAME_SAVED_REGS \
        (sizeof (CORE_ADDR) * (NUM_REGS+NUM_PSEUDO_REGS))

/* Allocate zero initialized memory from the frame cache obstack.
   Appendices to the frame info (such as the unwind cache) should
   allocate memory using this method.  */

extern void *frame_obstack_zalloc (unsigned long size);
#define FRAME_OBSTACK_ZALLOC(TYPE) ((TYPE *) frame_obstack_zalloc (sizeof (TYPE)))
#define FRAME_OBSTACK_CALLOC(NUMBER,TYPE) ((TYPE *) frame_obstack_zalloc ((NUMBER) * sizeof (TYPE)))

/* If legacy_frame_chain_valid() returns zero it means that the given
   frame is the outermost one and has no caller.

   This method has been superseeded by the per-architecture
   frame_unwind_pc() (returns 0 to indicate an invalid return address)
   and per-frame this_id() (returns a NULL frame ID to indicate an
   invalid frame).  */
extern int legacy_frame_chain_valid (CORE_ADDR, struct frame_info *);

extern void generic_save_dummy_frame_tos (CORE_ADDR sp);

extern struct block *get_frame_block (struct frame_info *,
                                      CORE_ADDR *addr_in_block);

/* Return the `struct block' that belongs to the selected thread's
   selected frame.  If the inferior has no state, return NULL.

   NOTE: cagney/2002-11-29:

   No state?  Does the inferior have any execution state (a core file
   does, an executable does not).  At present the code tests
   `target_has_stack' but I'm left wondering if it should test
   `target_has_registers' or, even, a merged target_has_state.

   Should it look at the most recently specified SAL?  If the target
   has no state, should this function try to extract a block from the
   most recently selected SAL?  That way `list foo' would give it some
   sort of reference point.  Then again, perhaphs that would confuse
   things.

   Calls to this function can be broken down into two categories: Code
   that uses the selected block as an additional, but optional, data
   point; Code that uses the selected block as a prop, when it should
   have the relevant frame/block/pc explicitly passed in.

   The latter can be eliminated by correctly parameterizing the code,
   the former though is more interesting.  Per the "address" command,
   it occures in the CLI code and makes it possible for commands to
   work, even when the inferior has no state.  */

extern struct block *get_selected_block (CORE_ADDR *addr_in_block);

extern struct symbol *get_frame_function (struct frame_info *);

extern CORE_ADDR frame_address_in_block (struct frame_info *);

extern CORE_ADDR get_pc_function_start (CORE_ADDR);

extern int frameless_look_for_prologue (struct frame_info *);

extern void print_frame_args (struct symbol *, struct frame_info *,
			      int, struct ui_file *);

extern struct frame_info *find_relative_frame (struct frame_info *, int *);

extern void show_and_print_stack_frame (struct frame_info *fi, int level,
					int source);

extern void print_stack_frame (struct frame_info *, int, int);

extern void show_stack_frame (struct frame_info *);

extern void print_frame_info (struct frame_info *, int, int, int);

extern void show_frame_info (struct frame_info *, int, int, int);

extern struct frame_info *block_innermost_frame (const struct block *);

/* NOTE: cagney/2002-09-13: There is no need for this function.
   Instead either of frame_unwind_signed_register() or
   frame_unwind_unsigned_register() can be used.  */
extern CORE_ADDR deprecated_read_register_dummy (CORE_ADDR pc,
						 CORE_ADDR fp, int);
extern void generic_push_dummy_frame (void);
extern void generic_pop_current_frame (void (*)(struct frame_info *));
extern void generic_pop_dummy_frame (void);

extern int generic_pc_in_call_dummy (CORE_ADDR pc,
				     CORE_ADDR sp, CORE_ADDR fp);

/* NOTE: cagney/2002-06-26: Targets should no longer use this
   function.  Instead, the contents of a dummy frames registers can be
   obtained by applying: frame_register_unwind to the dummy frame; or
   frame_register_unwind() to the next outer frame.  */

extern char *deprecated_generic_find_dummy_frame (CORE_ADDR pc, CORE_ADDR fp);

void generic_unwind_get_saved_register (char *raw_buffer,
				        int *optimizedp,
				        CORE_ADDR *addrp,
				        struct frame_info *frame,
				        int regnum,
				        enum lval_type *lvalp);

/* The function generic_get_saved_register() has been made obsolete.
   DEPRECATED_GET_SAVED_REGISTER now defaults to the recursive
   equivalent - generic_unwind_get_saved_register() - so there is no
   need to even set DEPRECATED_GET_SAVED_REGISTER.  Architectures that
   need to override the register unwind mechanism should modify
   frame->unwind().  */
extern void deprecated_generic_get_saved_register (char *, int *, CORE_ADDR *,
						   struct frame_info *, int,
						   enum lval_type *);

extern void generic_save_call_dummy_addr (CORE_ADDR lo, CORE_ADDR hi);

/* FIXME: cagney/2003-02-02: Should be deprecated or replaced with a
   function called frame_read_register_p().  This slightly weird (and
   older) variant of frame_read_register() returns zero (indicating
   the register is unavailable) if either: the register isn't cached;
   or the register has been optimized out.  Problem is, neither check
   is exactly correct.  A register can't be optimized out (it may not
   have been saved as part of a function call); The fact that a
   register isn't in the register cache doesn't mean that the register
   isn't available (it could have been fetched from memory).  */

extern int frame_register_read (struct frame_info *frame, int regnum,
				void *buf);

/* From stack.c.  */
extern void args_info (char *, int);

extern void locals_info (char *, int);

extern void (*selected_frame_level_changed_hook) (int);

extern void return_command (char *, int);


/* NOTE: cagney/2002-11-27:

   You might think that the below global can simply be replaced by a
   call to either get_selected_frame() or select_frame().

   Unfortunatly, it isn't that easy.

   The relevant code needs to be audited to determine if it is
   possible (or pratical) to instead pass the applicable frame in as a
   parameter.  For instance, DEPRECATED_DO_REGISTERS_INFO() relied on
   the deprecated_selected_frame global, while its replacement,
   PRINT_REGISTERS_INFO(), is parameterized with the selected frame.
   The only real exceptions occure at the edge (in the CLI code) where
   user commands need to pick up the selected frame before proceeding.

   This is important.  GDB is trying to stamp out the hack:

   saved_frame = deprecated_selected_frame;
   deprecated_selected_frame = ...;
   hack_using_global_selected_frame ();
   deprecated_selected_frame = saved_frame;

   Take care!  */

extern struct frame_info *deprecated_selected_frame;


/* Create a frame using the specified BASE and PC.  */

extern struct frame_info *create_new_frame (CORE_ADDR base, CORE_ADDR pc);


/* Create/access the frame's `extra info'.  The extra info is used by
   older code to store information such as the analyzed prologue.  The
   zalloc() should only be called by the INIT_EXTRA_INFO method.  */

extern struct frame_extra_info *frame_extra_info_zalloc (struct frame_info *fi,
							 long size);
extern struct frame_extra_info *get_frame_extra_info (struct frame_info *fi);

/* Create/access the frame's `saved_regs'.  The saved regs are used by
   older code to store the address of each register (except for
   SP_REGNUM where the value of the register in the previous frame is
   stored).  */
extern CORE_ADDR *frame_saved_regs_zalloc (struct frame_info *);
extern CORE_ADDR *get_frame_saved_regs (struct frame_info *);

/* FIXME: cagney/2002-12-06: Has the PC in the current frame changed?
   "infrun.c", Thanks to DECR_PC_AFTER_BREAK, can change the PC after
   the initial frame create.  This puts things back in sync.

   This replaced: frame->pc = ....; */
extern void deprecated_update_frame_pc_hack (struct frame_info *frame,
					     CORE_ADDR pc);

/* FIXME: cagney/2002-12-18: Has the frame's base changed?  Or to be
   more exact, whas that initial guess at the frame's base as returned
   by deprecated_read_fp() wrong.  If it was, fix it.  This shouldn't
   be necessary since the code should be getting the frame's base
   correct from the outset.

   This replaced: frame->frame = ....; */
extern void deprecated_update_frame_base_hack (struct frame_info *frame,
					       CORE_ADDR base);

/* FIXME: cagney/2003-01-04: Explicitly set the frame's saved_regs
   and/or extra_info.  Target code is allocating a fake frame and than
   initializing that to get around the problem of, when creating the
   inner most frame, there is no where to cache information such as
   the prologue analysis.  This is fixed by the new unwind mechanism -
   even the inner most frame has somewhere to store things like the
   prolog analysis (or at least will once the frame overhaul is
   finished).  */
extern void deprecated_set_frame_saved_regs_hack (struct frame_info *frame,
						  CORE_ADDR *saved_regs);
extern void deprecated_set_frame_extra_info_hack (struct frame_info *frame,
						  struct frame_extra_info *extra_info);

/* FIXME: cagney/2003-01-04: Allocate a frame from the heap (rather
   than the frame obstack).  Targets do this as a way of saving the
   prologue analysis from the inner most frame before that frame has
   been created.  By always creating a frame, this problem goes away.  */
extern struct frame_info *deprecated_frame_xmalloc (void);

/* FIXME: cagney/2003-01-05: Allocate a frame, along with the
   saved_regs and extra_info.  Set up cleanups for all three.  Same as
   for deprecated_frame_xmalloc, targets are calling this when
   creating a scratch `struct frame_info'.  The frame overhaul makes
   this unnecessary since all frame queries are parameterized with a
   common cache parameter and a frame.  */
extern struct frame_info *deprecated_frame_xmalloc_with_cleanup (long sizeof_saved_regs,
								 long sizeof_extra_info);

/* FIXME: cagney/2003-01-07: These are just nasty.  Code shouldn't be
   doing this.  I suspect it dates back to the days when every field
   of an allocated structure was explicitly initialized.  */
extern void deprecated_set_frame_next_hack (struct frame_info *fi,
					    struct frame_info *next);
extern void deprecated_set_frame_prev_hack (struct frame_info *fi,
					    struct frame_info *prev);

/* FIXME: cagney/2003-01-07: Instead of the dwarf2cfi having its own
   dedicated `struct frame_info . context' field, the code should use
   the per frame `unwind_cache' that is passed to the
   frame_pc_unwind(), frame_register_unwind() and frame_id_unwind()
   methods.

   See "dummy-frame.c" for an example of how a cfi-frame object can be
   implemented using this.  */
extern struct context *deprecated_get_frame_context (struct frame_info *fi);
extern void deprecated_set_frame_context (struct frame_info *fi,
					  struct context *context);

/* Return non-zero if the architecture is relying on legacy frame
   code.  */
extern int legacy_frame_p (struct gdbarch *gdbarch);

#endif /* !defined (FRAME_H)  */
