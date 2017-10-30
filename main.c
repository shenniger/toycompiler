#include "prg.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void printList(struct LE *list, int depth) {
  struct LE *l;
  int i;
  for (l = list; l; l = l->N) {
    switch (l->T) {
    case tyIdent:
      fprintf(stderr, "%s:", formatSourceLoc(*l));
      for (i = 0; i < depth; ++i) {
        fputc(' ', stderr);
      }
      fprintf(stderr, "\"%s\"\n", l->V.S);
      break;
    case tyInt:
      fprintf(stderr, "%s:", formatSourceLoc(*l));
      for (i = 0; i < depth; ++i) {
        fputc(' ', stderr);
      }
      fprintf(stderr, "%li\n", l->V.I);
      break;
    case tyFloat:
      fprintf(stderr, "%s:", formatSourceLoc(*l));
      for (i = 0; i < depth; ++i) {
        fputc(' ', stderr);
      }
      fprintf(stderr, "%f\n", l->V.F);
      break;
    case tyString:
      fprintf(stderr, "%s:", formatSourceLoc(*l));
      for (i = 0; i < depth; ++i) {
        fputc(' ', stderr);
      }
      fprintf(stderr, "\"%s\" (quoted)\n", l->V.S);
      break;
    case tyList:
      printList(l->V.L, depth + 2);
      break;
    }
  }
}

static char *readFile(const char *name, size_t *len) {
  FILE *in;
  char *s;
  in = fopen(name, "rb");
  if (!in) {
    return NULL;
  }
  fseek(in, 0, SEEK_END);
  *len = ftell(in);
  fseek(in, 0, SEEK_SET);
  s = getMem(*len + 1);
  if (fread(s, 1, *len, in) != *len) {
    fclose(in);
    return NULL;
  }
  fclose(in);
  s[*len] = 0;
  return s;
}

static const char *sourcefile; /* TODO: array */
static const char *sourcefilename;

const char *formatSourceLoc(struct LE l) {
  unsigned line;
  const char *lastline;
  const char *stop;
  const char *s;
  line = 1;
  lastline = sourcefile;
  stop = sourcefile + l.CharIdx;
  for (s = sourcefile; *s && s != stop; ++s) {
    if (*s == '\n') {
      ++line;
      lastline = s;
    }
  }
  return printToMem("%s:%i:%i", sourcefilename, line, stop - lastline);
}

void compileError(struct LE l, const char *fmt, ...) {
  va_list va;
  fprintf(stderr, "%s: ERR: ", formatSourceLoc(l));
  va_start(va, fmt);
  vfprintf(stderr, fmt, va);
  va_end(va);
  puts("");
  exit(1);
}

void compileHint(struct LE l, const char *fmt, ...) {
  va_list va;
  fprintf(stderr, "%s: hint: ", formatSourceLoc(l));
  va_start(va, fmt);
  vfprintf(stderr, fmt, va);
  va_end(va);
  puts("");
}

int main(int argc, char **argv) {
  /*char testfile[] =
      "(defun fnA (x y z) (+ x y z 4 (if 0 1 (+ 2 2))))\n"
      "(defun main () ((print (+ (* 12 4) (- 5 5))) "
      "  (defun fnB () ((print 6) 5)) "
      "(print (+ 10 1))\n((var a) (set a 10) (set a (+ a 5)) "
      "(print a)) (print (+ "
      "23 2 (fnA 12 1 (+ 3 4)))) (if 1 ((print 1))) ((var i) (set "
      "i 0); Test\n (while 1 "
      "((print i) (set; Test 2\n i (; Test 3\n+ i 1))))) (- "
      "(fnB) 10 2 3)))\n";*/
  /*  char testfile[] = "(alias MyInt i64)\n"
                      "(alias MyInt2 MyInt) (alias MyInt3 MyInt2)\n"
                      "(defun fnA ((MyInt a) (i64 b)) MyInt3 (+ a b))\n"
                      "(defun main () i32 (\n"
                      "  (var i64 x)\n"
                      "  (set x 10)\n"
                      "  (print (fnA 10 x))\n"
                      "  4))";*/
  /*char testfile[] =
      "(defun (static inline) add2 ((i8 a) (i32 b) (i32 c d)) i32 (+ a b))"
      "(alias add add2) (defun (static inline) add3 ((i32 a b c) "
      ") i32 (+ a b c))"
      "(defun add2_64 ((i64 a b)) i64 (+ a b))"
      "(alias add add2_64) (alias add add3)"
      "(defun addf ((float a) (float b)) float (+ a b))"
      "(alias add addf)\n"
      "(funproto sin ((float a)) float)"
      "(struct Point ((i64 x y)))"
      "(defun (static) test2 () void ())"
      "(global (static) i32 glvar)"
      "(defun main () i32 ("
      "  (add 5 10)"
      "  (defun test () i32 5)"
      "  (add 10 11 (test))"
      "  (var (i64) x)"
      "  (var (i64) y 10) (add y y)"
      "  (var (ref i64) t (ptrto x))"
      "  (add 1.2 3.4)\n"
      "  (+ 12 t)"
      "  (var (ptr const i8) s \"Hallo Welt.\")"
      "  (add 1 2 (cast i8 (sin 4.5)))"
      "  (var Point pt) (set (memb pt x) 10)"
      "  (var Point pt2) (set (memb pt2 y) 12)"
      "  (if (== pt pt2) (set x 0))"
      "  (add (cast i8 1) (cast i8 2) 3 4)"
      "  (var (funptr i64 (i64 i64)) f add2_64)"
      "  (funcall f 4 10)"
      "  (test2)"
      ""
      "  (var (array 4 i32) a)"
      "  (set (ptr-deref (+ a 1)) 10)"
      "  (set glvar 1234)"
      "  glvar"
      "))";*/
  /*"(static (var x 9.12)) (defun main () float (static (quasiquote "
  "(+ 1 (static x) (quasiunquote (+ 6 4))))))";*/
  /*char testfile[] =
      "(static-run (var stuff (scope (\n"
      "  (var i 1000000)\n"
      "  (var l (quote ()))\n"
      "  (while (!= i 0) (\n"
      "    (set l (append-first l (quasiquote ((print (quasiunquote i))))))\n"
      "    (set i (- i 1))))\n"
      "  l))))\n"
      "(static-run (debug-print (ident-to-string (quote stuff))))\n"
      "(defun main () i32 ((static stuff) 0))\n";*/
  char *srcfile;
  struct LE *list;
  size_t len;
  if (argc != 2) {
    fputs("USAGE: [exec] [FILE]\n", stderr);
    return 1;
  }
  initAlloc();
  sourcefilename = argv[1];
  sourcefile = readFile(sourcefilename, &len);
  srcfile = getMem(len + 1);
  memcpy(srcfile, sourcefile, len);
  list = NULL;
  readList(srcfile, &list, srcfile, 0);
  initCodegen();
  initParser();
  initEvaluator();
  parseSrc(list);
  finalizeCodegen();
  return 0;
}
