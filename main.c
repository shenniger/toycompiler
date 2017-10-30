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

struct SourceFile {
  const char *Name;
  const char *Content;
  struct SourceFile *Next;
};
static struct SourceFile *firstsource, *lastsource;
static unsigned short atfile;

static struct SourceFile getSourceFile(unsigned short idx) {
  struct SourceFile *s;
  unsigned short i;
  if (!idx) {
    struct SourceFile a;
    a.Name = "<gen>";
    a.Content = NULL;
    return a;
  }
  for (i = 1, s = firstsource; s && i != idx; ++i, s = s->Next)
    ;
  return *s;
}

const char *formatSourceLoc(struct LE l) {
  unsigned line;
  const char *lastline;
  const char *stop;
  const char *s;
  struct SourceFile file;
  file = getSourceFile(l.FileIdx);
  line = 1;
  lastline = file.Content;
  stop = file.Content + l.CharIdx;
  if (file.Content) {
    for (s = file.Content; *s && s != stop; ++s) {
      if (*s == '\n') {
        ++line;
        lastline = s;
      }
    }
  }
  return printToMem("%s:%i:%i", file.Name, line, stop - lastline);
}

struct LE *readFileAsList(const char *name) {
  struct SourceFile **n;
  size_t len;
  char *mutcopy;
  struct LE *list;
  n = lastsource ? &lastsource->Next : &lastsource;
  *n = getMem(sizeof(struct SourceFile));
  lastsource = *n;
  lastsource->Name = name;
  lastsource->Content = readFile(name, &len);
  if (!lastsource->Content) {
    struct LE li;
    memset(&li, 0, sizeof(struct LE));
    compileError(li, "can not find file: %s", lastsource->Name);
  }
  if (!firstsource) {
    firstsource = lastsource;
  }
  mutcopy = getMem(len + 1);
  memcpy(mutcopy, lastsource->Content, len);
  list = getMem(sizeof(struct LE));
  list->T = tyList;
  readList(mutcopy, &list->V.L, mutcopy, ++atfile);
  return list;
}

int main(int argc, char **argv) {
  struct LE *list;
  if (argc != 2) {
    fputs("USAGE: [exec] [FILE]\n", stderr);
    return 1;
  }
  initAlloc();
  initCodegen();
  initParser();
  initEvaluator();
  list = readFileAsList(argv[1]);
  parseSrc(list);
  finalizeCodegen();
  return 0;
}
