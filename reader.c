#include "prg.h"

#include <stdlib.h>
#include <string.h>

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
    case ';':
      for (; *s != '\n' && *s; ++s)
        ;
      break;
    default:
      if (*s >= '0' && *s <= '9') {
        int isFloat;
        char *a;
        isFloat = 0;
        for (a = s; *a && *a != ' ' && *a != ')' && *a != '\n' && *a != '\t' &&
                    *a != ';';
             ++a) {
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
        for (; *s != ')' && *s != ' ' && *s != '\t' && *s != '\n' && *s != ';';
             ++s) {
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

struct LE *copyList(struct LE *l) {
  struct LE *cp;
  if (!l) {
    return NULL;
  }
  cp = getMem(sizeof(struct LE));
  *cp = *l;
  if (cp->T == tyList) {
    cp->V.L = copyList(cp->V.L);
  }
  cp->N = copyList(cp->N);
  return cp;
}
