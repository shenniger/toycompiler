#include "prg.h"

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define PART_SIZE (1048576)

struct MemAllocator {
  char *At;
  long Left;
  char **Mem;
  long MemSize;
};

static struct MemAllocator memalloc;

void initAlloc() {
  memalloc.Mem = malloc(sizeof(char *));
  assert(!!memalloc.Mem);
  *memalloc.Mem = calloc(PART_SIZE, 1);
  assert(!!*memalloc.Mem);
  memalloc.At = *memalloc.Mem;
  memalloc.Left = PART_SIZE;
  memalloc.MemSize = 1;
}
void *getMem(long s) {
  if (memalloc.Left < s) {
    char *a;
    a = calloc(PART_SIZE, 1);
    assert(!!a);
    memalloc.At = a;
    memalloc.Left = PART_SIZE;
    memalloc.MemSize++;
    memalloc.Mem = realloc(memalloc.Mem, sizeof(char *) * memalloc.MemSize);
    assert(!!memalloc.Mem);
  }
  memalloc.Left -= s;
  memalloc.At += s;
  return memalloc.At - s;
}
void *moreMem(void *before, long beforesize, long addedsize) {
  if (!before) {
    return getMem(addedsize);
  }
  assert(memalloc.At == (char *)before + beforesize);
  if (memalloc.Left < addedsize) {
    void *r;
    r = getMem(beforesize + addedsize);
    memcpy(r, before, beforesize);
    return r;
  }
  memalloc.Left -= addedsize;
  memalloc.At += addedsize;
  return before;
}
char *printToMem(const char *fmt, ...) {
  va_list va, va2;
  char *str;
  int len;
  va_start(va, fmt);
  va_copy(va2, va);
  str = memalloc.At;
  len = vsnprintf(str, memalloc.Left, fmt, va);
  if (len >= memalloc.Left) {
    str = getMem(len + 1);
    vsprintf(str, fmt, va2);
  } else {
    memalloc.At += len + 1;
    memalloc.Left -= len + 1;
  }
  va_end(va2);
  va_end(va);
  return str;
}

void readList(char *str, struct LE **l, const char *filebegin,
              unsigned short fileidx) {
  char *s;
  s = str;
  while (*s) {
    switch (*s) {
    case ' ':
    case '\t':
    case '\n':
      ++s;
      break;
    case '(': {
      struct LE **n;
      char *a;
      int parentheses;
      *l = getMem(sizeof(struct LE));
      (*l)->T = tyList;
      (*l)->CharIdx = str - filebegin;
      (*l)->FileIdx = fileidx;
      n = &((*l)->V.L);
      a = s + 1;
      parentheses = 0;
      for (;;) {
        if (!*s) {
          compileError(**l, "missing closing parentheses");
          return;
        }
        if (*s == '(') {
          ++parentheses;
        }
        if (*s == ')') {
          if (!--parentheses) {
            break;
          }
        }
        ++s;
      }
      *s = 0;
      ++s;
      l = &((*l)->N);
      readList(a, n, filebegin, fileidx);
    } break;
    case '"':
      *l = getMem(sizeof(struct LE));
      (*l)->T = tyString;
      (*l)->CharIdx = str - filebegin;
      (*l)->FileIdx = fileidx;
      ++s;
      (*l)->V.S = s;
      for (; *s != '"'; ++s) {
        if (!*s) {
          compileError(**l, "missing closing '\"'");
          return;
        }
      }
      *s = 0;
      ++s;
      l = &((*l)->N);
      break;
    default:
      if (*s >= '0' && *s <= '9') {
        int isFloat;
        char *a;
        isFloat = 0;
        for (a = s; *a && *a != ' ' && *a != ')'; ++a) {
          if (*a == '.') {
            isFloat = 1;
          }
        }
        *l = getMem(sizeof(struct LE));
        (*l)->T = isFloat ? tyFloat : tyInt;
        (*l)->CharIdx = str - filebegin;
        (*l)->FileIdx = fileidx;
        if (isFloat) {
          (*l)->V.F = strtod(s, NULL);
        } else {
          (*l)->V.I = strtol(s, NULL, 10);
        }
        l = &((*l)->N);
        s = a;
      } else {
        *l = getMem(sizeof(struct LE));
        (*l)->T = tyIdent;
        (*l)->CharIdx = str - filebegin;
        (*l)->FileIdx = fileidx;
        (*l)->V.S = s;
        l = &((*l)->N);
        for (; *s != ')' && *s != ' '; ++s) {
          if (!*s) {
            return;
          }
        }
        *s = 0;
        ++s;
      }
    }
  }
}