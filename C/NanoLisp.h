//
//  NanoLogo.h
//  nanoLogo
//
//  Created by Jean-Pierre De Soza on 15/07/2021.
//

#ifndef NanoLogo_h
#define NanoLogo_h

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define BANNER "NanoLisp-V0.0.1 - by jpds - 2021\n"

#define MAX_STRING_LENGTH 255
#define TAB     0x09
#define LF      0x10
#define CR      0x13
#define SPC     0x20
#define QUOTE_S 0x27
#define PAREN_L '('
#define PAREN_R ')'

// Macros
#define DBG_TRACE(x) if (TRACE) (x)
#define F_CAR(s) ((!NULLP(s)) ? (LISTP(s) ? (s->unode.list->car): ((NUMBERP(s))? s : NIL)): NIL)
#define F_CDR(s) (!NULLP(s) ? (LISTP(s) ? (s->unode.list->cdr) : NIL) : NIL)

#define NULLP(S) ((S) == NIL)
#define QUOTEP(S) ((S) == QUOTE)
#define ATOMP(S) ((NULLP(S) || (S)->kind == ATOM))
#define LISTP(S) ((!NULLP(S) && (S)->kind == LIST))
#define NUMBERP(S) (isNumeric(NAME_OF((S))))

#define NAME_OF(s) ((s)->unode.atom->name)
#define VALUE_OF(s) ((s)->unode.atom->val)
#define TRACE GLOBAL_ENV._trace
#define ERROR GLOBAL_ENV._error
#define DONE GLOBAL_ENV._done
#define OBLIST  GLOBAL_ENV._oblist
#define T       GLOBAL_ENV._T
#define NIL     GLOBAL_ENV._NIL
#define QUOTE   GLOBAL_ENV._QUOTE
#define LAMBDA  GLOBAL_ENV._LAMBDA

#define BUFFER  GLOBAL_ENV._buffer
#define INDEX   GLOBAL_ENV._index
#define S_CONSOLE GLOBAL_ENV._CONSOLE
#define CONSOLE NAME_OF(S_CONSOLE)

#define PROMPT1 "\tR> "
#define PROMPT2 "E> "
#define PROMPT3 "P> "

typedef unsigned char KindOfNode;
typedef struct struct_object* PtObList;
typedef struct struct_node* Sexp;
typedef struct struct_atom* Atom;
typedef struct struct_list* List;
typedef struct struct_env* Env;
typedef char* String;

enum KindOfNode {ATOM, LIST};
enum KindOfToken {L_PAREN, R_PAREN, S_QUOTE, SYMBOL};

typedef struct struct_atom {
    String name;
    Sexp val;
} atom_t;

typedef struct struct_list {
    Sexp car;
    Sexp cdr;
} list_t;

typedef struct struct_node {
    PtObList plist;
    enum KindOfNode kind;
    union {
        Atom atom;
        List list;
    } unode;
} node_t;

typedef struct struct_object {
    Sexp info;
    PtObList next;
} object_t ;

// Globales
typedef struct struct_env {
    bool _error;
    bool _done;
    bool _trace;
    PtObList _oblist;
    unsigned int _index;
    char _buffer[MAX_STRING_LENGTH];
    Sexp _NIL;
    Sexp _T;
    Sexp _QUOTE;
    Sexp _LAMBDA;
    Sexp _CONSOLE;
} env_t;
extern env_t GLOBAL_ENV;

// Function prototypes
void setup(void);

Sexp f_error(String message, Sexp origin);

PtObList new_atom(PtObList position, String name);
Sexp find_sexp2(const PtObList position, String name);
Sexp find_sexp(String name);
Sexp f_cons(Sexp s1, Sexp s2);

// Display function
void f_print(Sexp s);
void print1(Sexp s, String format);
void print_atom(Sexp s, String format);
void f_oblist(void);
void obprint(const PtObList start);

// Input function
Sexp read_atom(char buffer[], enum KindOfToken *token, FILE *source);
Sexp read1(char buffer[], bool list_in_progress, FILE *source);
Sexp f_read(FILE * source);
Sexp f_load(Sexp filename);
Sexp f_eval(Sexp s);
Sexp f_apply(Sexp fn, Sexp args);
#endif /* NanoLogo_h */
