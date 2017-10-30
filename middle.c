#include "prg.h"

#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

enum { tyMiddleEndType = tyEnd, tyFun };

struct MFun {
  struct LE *Args;
  struct LE *L;
};

struct MVar {
  unsigned long HashedName;
  const char *Name;
  struct LE Value;
  struct MVar *Last;
};

struct MScope {
  struct MVar *Vars;
  struct MScope *Parent;
};

static struct MScope firstscope;
static struct MScope *curmscope;

enum {
  bmNoBuiltin,
  bmCar,
  bmCdr,
  bmList,
  bmCons,
  bmQuote,
  bmQuasiquote,
  bmQuasiunquote,
  bmArm,
  bmCmp,
  bmEqual,
  bmUnary,
  bmRound,
  bmTrunc,
  bmCeil,
  bmIf,
  bmWhile,
  bmVar,
  bmLet,
  bmSet,
  bmScope,
  bmLambda,
  bmDefun,
  bmEval,
  bmAppend,
  bmAppendFirst,
  bmReadFile,
  bmStringToInt,
  bmStringToFloat,
  bmNumToString,
  bmIsList,
  bmIsString,
  bmIsIdent,
  bmIsInt,
  bmIsFloat,
  bmToIdent,
  bmToString,
  bmDebugPrint
};
struct BuiltinMacro {
  const char *Name;
  unsigned long HashedName;
  int BuiltinCode;
};
static struct BuiltinMacro builtinmacros[54];

void initEvaluator() {
  unsigned i;
  i = 0;

  curmscope = &firstscope;

#define ADDM(a, b)                                                             \
  builtinmacros[i].Name = a;                                                   \
  builtinmacros[i].BuiltinCode = b;                                            \
  i++;

  /* DO NOT ADD MORE MACROS THAN ARRAY ELEMENTS */

  ADDM("car", bmCar)
  ADDM("cdr", bmCdr)
  ADDM("list", bmList)
  ADDM("cons", bmCons)
  ADDM("quote", bmQuote)
  ADDM("quasiquote", bmQuasiquote)
  ADDM("quasiunquote", bmQuasiunquote)
  ADDM("if", bmIf)
  ADDM("while", bmWhile)
  ADDM("var", bmVar)
  ADDM("let", bmLet)
  ADDM("set", bmSet)
  ADDM("scope", bmScope)
  ADDM("append", bmAppend)
  ADDM("append-first", bmAppendFirst)
  ADDM("debug-print", bmDebugPrint)
  ADDM("lambda", bmLambda)
  ADDM("defun", bmDefun)
  ADDM("eval", bmEval)
  ADDM("read-file", bmReadFile)
  ADDM("+", bmArm)
  ADDM("-", bmArm)
  ADDM("*", bmArm)
  ADDM("/", bmArm)
  ADDM("&&", bmArm)
  ADDM("||", bmArm)
  ADDM("<<", bmArm)
  ADDM(">>", bmArm)
  ADDM("&", bmArm)
  ADDM("|", bmArm)
  ADDM("%", bmArm)
  ADDM("==", bmCmp)
  ADDM("!=", bmCmp)
  ADDM("<", bmCmp)
  ADDM(">", bmCmp)
  ADDM("<=", bmCmp)
  ADDM(">=", bmCmp)
  ADDM("!", bmUnary)
  ADDM("~", bmUnary)
  ADDM("round", bmRound)
  ADDM("trunc", bmTrunc)
  ADDM("ceil", bmCeil)
  ADDM("string-to-int", bmStringToInt)
  ADDM("string-to-float", bmStringToFloat)
  ADDM("num-to-string", bmNumToString)
  ADDM("int-to-string", bmNumToString)
  ADDM("float-to-string", bmNumToString)
  ADDM("is-list", bmIsList)
  ADDM("is-string", bmIsString)
  ADDM("is-ident", bmIsIdent)
  ADDM("is-int", bmIsInt)
  ADDM("is-float", bmIsFloat)
  ADDM("string-to-ident", bmToIdent)
  ADDM("ident-to-string", bmToString)

#undef ADDM

  assert(i == sizeof(builtinmacros) / sizeof(struct BuiltinMacro));

  for (i = 0; i < sizeof(builtinmacros) / sizeof(struct BuiltinMacro); ++i) {
    builtinmacros[i].HashedName = hashName(builtinmacros[i].Name);
  }
}

static void checkArgs(struct LE *l, int n) {
  int i;
  struct LE *li;
  li = l->N;
  for (i = 0; i < n; ++i) {
    if (!l) {
      compileError(*li, "ctfe: %s: found %i arguments, expected %i", l->V.S, i,
                   n);
    }
    li = li->N;
  }
  if (li) {
    compileError(*li, "ctfe: %s: found too many arguments, expected %i", l->V.S,
                 i, n);
  }
}

static void handleQuasiquote(struct LE **li) {
  struct LE **l;
  if ((*li)->T != tyList || !(*li)->V.L) {
    return;
  }
  if ((*li)->V.L->T == tyIdent &&
      strcmp("quasiunquote", (*li)->V.L->V.S) == 0) {
    struct LE *n, *oldl;
    n = (*li)->N;
    oldl = *li;
    *li = evalList((*li)->V.L->N);
    if (!*li) {
      compileError(*oldl, "ctfe: invalid quasiquote");
    }
    (*li)->N = n;
    return;
  }
  for (l = &(*li)->V.L; *l; l = &(*l)->N) {
    handleQuasiquote(l);
  }
}

static struct LE *listCmd(struct LE *li) {
  struct LE *r;
  if (!li) {
    return NULL;
  }
  r = evalList(li);
  r->N = evalList(li->N);
  return r;
}

static struct LE evalOp(const char *op, struct LE a, struct LE b) {
  struct LE l;
  memset(&l, 0, sizeof(l));
  if (a.T == tyInt && b.T == tyFloat) {
    return evalOp(op, b, a);
  }
  if (a.T == tyString && b.T == tyString) {
    l.T = tyInt;
    if (strcmp(op, "==") == 0) {
      l.V.I = strcmp(a.V.S, b.V.S) == 0;
    }
    if (strcmp(op, "!=") == 0) {
      l.V.I = strcmp(a.V.S, b.V.S) != 0;
    }
    if (strcmp(op, "+") == 0) {
      l.T = tyString;
      l.V.S = printToMem("%s%s", a.V.S, b.V.S);
    }
    compileError(a, "ctfe: %s: unsupported op with string", op);
  }
#define OP(o, c)                                                               \
  if (strcmp(o, op) == 0) {                                                    \
    if (a.T == tyInt && b.T == tyInt) {                                        \
      l.T = tyInt;                                                             \
      l.V.I = a.V.I c b.V.I;                                                   \
    } else if (a.T == tyFloat && b.T == tyFloat) {                             \
      l.T = tyFloat;                                                           \
      l.V.F = a.V.F c b.V.F;                                                   \
    } else if (a.T == tyFloat && b.T == tyInt) {                               \
      l.T = tyFloat;                                                           \
      l.V.F = a.V.F c b.V.I;                                                   \
    } else {                                                                   \
      compileError(a, "ctfe: %s: unsupported types", op);                      \
    }                                                                          \
    return l;                                                                  \
  }
  OP("+", +)
  OP("-", -)
  OP("*", *)
  OP("/", /)
  OP("&&", &&)
  OP("||", ||)
  OP("==", ==)
  OP("!=", !=)
  OP("<", <)
  OP(">", >)
  OP("<=", <=)
  OP(">=", >=)

#undef OP

/* int-only operators */
#define OP(o, c)                                                               \
  if (strcmp(op, o) == 0) {                                                    \
    if (a.T == tyInt && b.T == tyInt) {                                        \
      l.T = tyInt;                                                             \
      l.V.I = a.V.I c b.V.I;                                                   \
    } else {                                                                   \
      compileError(a, "ctfe: %s: unsupported types", op);                      \
    }                                                                          \
    return l;                                                                  \
  }

  OP("<<", <<)
  OP(">>", >>)
  OP("&", &)
  OP("|", |)
  OP("%", %)

#undef OP

  compileError(a, "ctfe: %s: unsupported op", op);
  return l; /* never reached */
}

static struct LE *evalCmp(struct LE *li) {
  struct LE *l, *a, *b;
  checkArgs(li, 2);
  a = evalList(li->N);
  b = evalList(li->N->N);
  l = getMem(sizeof(struct LE));
  *l = evalOp(li->V.S, *a, *b);
  return l;
}

static struct LE *evalArm(struct LE *li) {
  struct LE *l, *a, *b, *list;
  if (!li->N || !li->N->N) {
    compileError(*li, "ctfe: %s: too few arguments");
  }
  a = evalList(li->N);
  b = evalList(li->N->N);
  l = getMem(sizeof(struct LE));
  *l = evalOp(li->V.S, *a, *b);
  for (list = li->N->N->N; list; list = list->N) {
    b = evalList(list);
    *l = evalOp(li->V.S, *l, *b);
  }
  return l;
}

static int evalCond(struct LE *li) {
  switch (li->T) {
  case tyEmpty:
    return 0;
  case tyInt:
    return !!li->V.I;
  case tyFloat:
    return li->V.F != 0.0;
  case tyString:
    return !!*li->V.S;
  case tyList:
    return !!li->V.L;
  }
  return 1;
}

static struct MFun *evalLambda(struct LE *li) {
  struct MFun *f;
  f = getMem(sizeof(struct MFun));
  f->Args = li;
  f->L = li->N;
  /* TODO: fail */
  /* TODO: perhaps include variable values? */
  return f;
}

static struct LE *callFun(struct MFun *f, struct LE *args) {
  struct MScope scope;
  struct LE *r;
  scope.Parent = curmscope;
  curmscope = &scope;
  if (f->Args->T == tyList) {
    struct LE *l, *la;
    struct MVar *v;
    scope.Vars = NULL;
    for (l = f->Args->V.L, la = args; 1; l = l->N, la = la->N) {
      if (!l != !la) {
        compileError(
            *la,
            "ctfe: too many or too few arguments in function call"); /* TODO: be
                                                                  more
                                                                  specific */
        break;
      }
      if (!l && !la) {
        break;
      }
      v = getMem(sizeof(struct MVar));
      v->Last = scope.Vars;
      scope.Vars = v;
      v->HashedName = hashName(l->V.S);
      v->Name = l->V.S;
      v->Value = *evalList(la);
    }
  } else {
    scope.Vars = getMem(sizeof(struct MVar));
    scope.Vars->HashedName = hashName(f->Args->V.S);
    scope.Vars->Name = f->Args->V.S;
    scope.Vars->Value.T = tyList;
    scope.Vars->Value.V.L = args;
  }
  r = evalList(f->L);
  curmscope = scope.Parent;
  return r;
}

static struct LE *evalFuncall(struct LE *li) {
  unsigned long hash;
  int builtin;
  unsigned i;
  hash = hashName(li->V.S);
  builtin = bmNoBuiltin;
  for (i = 0; i < sizeof(builtinmacros) / sizeof(struct BuiltinMacro); ++i) {
    if (builtinmacros[i].HashedName == hash &&
        strcmp(builtinmacros[i].Name, li->V.S) == 0) {
      builtin = builtinmacros[i].BuiltinCode;
      break;
    }
  }
  switch (builtin) {
  case bmCar: {
    struct LE *l;
    checkArgs(li, 1);
    l = evalList(li->N);
    if (l->T != tyList) {
      compileError(*li, "ctfe: %s: not a list", li->V.S);
    }
    return l->V.L;
  }
  case bmCdr: {
    struct LE *l, *lt;
    checkArgs(li, 1);
    l = getMem(sizeof(struct LE));
    l->T = tyList;
    lt = evalList(li->N);
    if (lt->T != tyList) {
      compileError(*li, "ctfe: %s: not a list", li->V.S);
    }
    if (!lt->V.L) {
      compileError(*li, "ctfe: %s: cdr on empty list", li->V.S);
    }
    l->V.L = lt->V.L->N;
    return l;
  }
  case bmList:
    return listCmd(li->N);
  case bmCons: {
    struct LE *l, *lt1, *lt2;
    checkArgs(li, 2);
    lt1 = evalList(li->N);
    lt2 = evalList(li->N->N);
    l = getMem(sizeof(struct LE));
    l->T = tyList;
    l->V.L = lt1;
    lt1->N = lt2;
    return l;
  }
  case bmQuote:
    checkArgs(li, 1);
    return copyList(li->N);
  case bmQuasiquote:
    checkArgs(li, 1);
    handleQuasiquote(&li->N);
    return copyList(li->N);
  case bmQuasiunquote:
    compileError(*li, "ctfe: %s outside of quasiquote", li->V.S);
    break;
  case bmUnary: {
    struct LE *l;
    long x;
    checkArgs(li, 1);
    l = evalList(li->N);
    if (l->T != tyInt) {
      compileError(*li, "ctfe: expected int in unary operator");
    }
    x = l->V.I;
    l = getMem(sizeof(struct LE));
    l->T = tyInt;
    switch (*li->V.S) {
    case '!':
      l->V.I = !x;
      break;
    case '~':
      l->V.I = ~x;
      break;
    }
    return l;
  }
  case bmCmp:
    return evalCmp(li);
  case bmArm:
    return evalArm(li);
  case bmRound: {
    struct LE *r; /* TODO: error */
    checkArgs(li, 1);
    r = getMem(sizeof(struct LE));
    r->T = tyInt;
    r->V.I = round(evalList(li->N)->V.F);
    return r;
  }
  case bmTrunc: {
    struct LE *r; /* TODO: error */
    checkArgs(li, 1);
    r = getMem(sizeof(struct LE));
    r->T = tyInt;
    r->V.I = trunc(evalList(li->N)->V.F);
    return r;
  }
  case bmCeil: {
    struct LE *r; /* TODO: error */
    checkArgs(li, 1);
    r = getMem(sizeof(struct LE));
    r->T = tyInt;
    r->V.I = ceil(evalList(li->N)->V.F);
    return r;
  }
  case bmLambda: {
    struct LE *r;
    checkArgs(li, 2);
    r = getMem(sizeof(struct LE));
    r->T = tyFun;
    r->V.D = evalLambda(li->N);
    return r;
  }
  case bmDefun: {
    struct MVar *a;
    checkArgs(li, 3);
    a = getMem(sizeof(struct MVar));
    a->HashedName = hashName(li->N->V.S); /* TODO: error */
    a->Name = li->N->V.S;
    a->Last = firstscope.Vars;
    firstscope.Vars = a;
    a->Value.T = tyFun;
    a->Value.V.D = evalLambda(li->N->N);
    return NULL;
  }
  case bmEval:
    checkArgs(li, 1);
    return evalList(evalList(li->N));
  case bmReadFile:
    checkArgs(li, 1);
    /* TODO: error */
    return readFileAsList(evalList(li->N)->V.S);
  case bmStringToInt: {
    struct LE *r; /* TODO: error */
    checkArgs(li, 1);
    r = getMem(sizeof(struct LE));
    r->T = tyInt;
    r->V.I = strtol(evalList(li->N)->V.S, NULL, 10);
    return r;
  }
  case bmStringToFloat: {
    struct LE *r; /* TODO: error */
    checkArgs(li, 1);
    r = getMem(sizeof(struct LE));
    r->T = tyFloat;
    r->V.I = strtod(evalList(li->N)->V.S, NULL);
    return r;
  }
  case bmNumToString: {
    struct LE *r, *l;
    checkArgs(li, 1);
    r = getMem(sizeof(struct LE));
    r->T = tyString;
    l = evalList(li->N);
    if (l->T == tyInt) {
      r->V.S = printToMem("%i", l->V.I);
    } else if (li->N->T == tyFloat) {
      r->V.S = printToMem("%f", l->V.F);
    } else {
      compileError(*li->N, "ctfe: %s: wrong type", li->V.S);
    }
    return r;
  }
  case bmIsList: {
    struct LE *r;
    checkArgs(li, 1);
    r = getMem(sizeof(struct LE));
    r->T = tyInt;
    r->V.I = evalList(li->N)->T == tyList;
    return r;
  }
  case bmIsString: {
    struct LE *r;
    checkArgs(li, 1);
    r = getMem(sizeof(struct LE));
    r->T = tyInt;
    r->V.I = evalList(li->N)->T == tyString;
    return r;
  }
  case bmIsIdent: {
    struct LE *r;
    checkArgs(li, 1);
    r = getMem(sizeof(struct LE));
    r->T = tyInt;
    r->V.I = evalList(li->N)->T == tyIdent;
    return r;
  }
  case bmIsInt: {
    struct LE *r;
    checkArgs(li, 1);
    r = getMem(sizeof(struct LE));
    r->T = tyInt;
    r->V.I = evalList(li->N)->T == tyInt;
    return r;
  }
  case bmIsFloat: {
    struct LE *r;
    checkArgs(li, 1);
    r = getMem(sizeof(struct LE));
    r->T = tyInt;
    r->V.I = evalList(li->N)->T == tyFloat;
    return r;
  }
  case bmToIdent: {
    struct LE *r, *l;
    checkArgs(li, 1);
    r = getMem(sizeof(struct LE));
    r->T = tyIdent;
    l = evalList(li->N);
    if (l->T != tyString) {
      compileError(*li->N, "ctfe: %s: wrong type (expected string)", li->V.S);
    }
    /* TODO: check whether it contains special characters (like ' ' or ')' */
    r->V.S = l->V.S;
    return r;
  }
  case bmToString: {
    struct LE *r, *l;
    checkArgs(li, 1);
    r = getMem(sizeof(struct LE));
    r->T = tyString;
    l = evalList(li->N);
    if (l->T != tyIdent) {
      compileError(*li->N, "ctfe: %s: wrong type (expected ident)", li->V.S);
    }
    r->V.S = l->V.S;
    return r;
  }
  case bmIf:
    checkArgs(li, 3);
    if (evalCond(evalList(li->N))) {
      return evalList(li->N->N);
    }
    return evalList(li->N->N->N);
  case bmWhile:
    checkArgs(li, 2);
    while (evalCond(evalList(li->N))) {
      evalList(li->N->N);
    }
    return NULL;
  case bmVar: {
    struct MVar *v;
    checkArgs(li, 2);
    v = getMem(sizeof(struct MVar));
    v->Last = curmscope->Vars;
    curmscope->Vars = v;
    v->HashedName = hashName(li->N->V.S);
    v->Name = li->N->V.S;
    v->Value = *evalList(li->N->N);
    return &v->Value;
  }
  case bmLet: {
    /* TODO */
  }
  case bmSet: {
    struct LE *l;
    checkArgs(li, 2);
    l = evalList(li->N);
    *l = *evalList(li->N->N);
    return l;
  }
  case bmScope: {
    struct MScope scope;
    struct LE *ret;
    struct LE *newret;
    checkArgs(li, 1);
    scope.Vars = NULL;
    scope.Parent = curmscope;
    curmscope = &scope;
    ret = evalList(li->N);
    newret = getMem(sizeof(struct LE));
    *newret = *ret;
    curmscope = scope.Parent;
    return newret;
  }
  case bmAppend: {
    struct LE *lt;
    struct LE **lt1, *lt2;
    checkArgs(li, 2);
    lt = evalList(li->N);
    lt2 = evalList(li->N->N);
    lt1 = &(lt->V.L);    /* TODO: error */
    lt2 = copyList(lt2); /* do we really need to copy here? */
    if (!*lt1) {
      *lt1 = lt2;
      return lt;
    }
    if (!lt2) {
      return lt;
    }
    while ((*lt1)->N) {
      lt1 = &((*lt1)->N);
    }
    (*lt1)->N = lt2;
    return lt;
  }
  case bmAppendFirst: {
    struct LE *lt;
    struct LE *lt2;
    checkArgs(li, 2);
    lt = evalList(li->N);
    lt2 = evalList(li->N->N);
    /* TODO: error */
    lt2 = copyList(lt2); /* do we really need to copy here? */
    if (!lt2) {
      return lt;
    }
    lt2->N = lt->V.L;
    lt->V.L = lt2;
    return lt;
  }
  case bmDebugPrint:
    checkArgs(li, 1);
    compileHint(*li->N, "debug print requested: \"\"");
    printList(evalList(li->N), 0);
    /* TODO: proper printList */
    return NULL;
  case bmNoBuiltin: {
    struct MVar *v;
    struct MScope *sc;
    unsigned long hash;
    hash = hashName(li->V.S);
    for (sc = curmscope; sc; sc = sc->Parent) {
      for (v = sc->Vars; v; v = v->Last) {
        if (v->Value.T == tyFun && v->HashedName == hash &&
            strcmp(li->V.S, v->Name) == 0) {
          return callFun((struct MFun *)v->Value.V.D, li->N);
        }
      }
    }
    compileError(*li, "ctfe: function not found (compile-time): %s", li->V.S);
  }
  }
  assert(0);   /* never reached */
  return NULL; /* also never reached */
}

struct LE *evalList(struct LE *li) {
  if (!li) {
    return NULL;
  }
  switch (li->T) {
  case tyEmpty:
    return NULL;
  case tyInt:
  case tyFloat:
  case tyString:
    return li;
  case tyIdent: {
    struct MVar *v;
    struct MScope *sc;
    unsigned long hash;
    hash = hashName(li->V.S);
    for (sc = curmscope; sc; sc = sc->Parent) {
      for (v = sc->Vars; v; v = v->Last) {
        if (v->HashedName == hash && strcmp(v->Name, li->V.S) == 0) {
          return &v->Value;
        }
      }
    }
    compileError(*li, "ctfe: unknown variable: %s", li->V.S);
  }
  case tyList: {
    struct LE *l;
    l = li->V.L;
    if (!l) {
      return NULL;
    }
    if (l->T == tyList) { /* list */
      for (; l && l->N; l = l->N) {
        evalList(l);
      }
      return evalList(l);
    } else if (l->T == tyIdent) { /* function call */
      return evalFuncall(l);
    } else {
      compileError(*l, "ctfe: invalid list in code mode");
    }
  } break;
  }
  return NULL;
}
