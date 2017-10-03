#include "prg.h"

#include <assert.h>
#include <string.h>

struct FParm {
  unsigned long HashedName;
  const char *Name;
  struct BVar *Backend;
  /* TODO: Type */
};

struct FVar {
  unsigned long HashedName;
  const char *Name;
  struct BVar *Backend;
  /* TODO: Type */

  struct FVar *Next;
};

struct FLocalFun {
  struct FFunction *Fn;
  struct FLocalFun *Last;
};

struct FScope {
  /* on adding new members: add zero initialization in "parse" */
  struct FVar *Vars;
  struct FLocalFun *Funs;

  struct FScope *Parent;
};

struct FFunction {
  unsigned long HashedName;
  const char *Name;
  unsigned long NParms;
  struct FParm *Parms;
  struct BFunction *Backend;
  struct FFunction *Parent;
  struct FScope *Scope;

  struct FFunction *Last;
};

static struct FFunction *functions;
static struct FFunction *currentffunction;
static struct FScope *curscope;

static unsigned long hashName(const char *s) {
  unsigned long res;
  while (*s++) {
    res = *s * 11;
  }
  return res;
}

/* TODO: closures */
static void parseDefun(struct LE *li, struct FScope *scope, int lvl) {
  /* TODO: error */
  struct FFunction *fn, *lastfn;
  struct LE *l;
  unsigned i;
  beginFnPrototype(li->V.S);
  /* TODO */
  fn = getMem(sizeof(struct FFunction));
  fn->HashedName = hashName(li->V.S);
  fn->Name = li->V.S;
  fn->Parent = currentffunction;
  for (l = li->N->V.L; l; l = l->N) {
    fn->Parms = moreMem(fn->Parms, sizeof(struct FParm) * fn->NParms,
                        sizeof(struct FParm));
    fn->Parms[fn->NParms].Name = l->V.S;
    fn->Parms[fn->NParms].HashedName = hashName(l->V.S);
    fn->NParms++;
  }
  for (i = 0; i < fn->NParms; ++i) {
    fn->Parms[i].Backend = addParameter(fn->Parms[i].Name);
  }
  fn->Last = functions;
  functions = fn;
  lastfn = currentffunction;
  currentffunction = fn;
  fn->Backend = endFnPrototype(1 /* add body */);

  endFnBody(parse(li->N->N, lvFun));

  if (lvl & lvFun) {
    struct FLocalFun *f;

    functions = fn->Last;
    currentffunction = lastfn;

    f = scope->Funs;
    scope->Funs = getMem(sizeof(struct FLocalFun));
    scope->Funs->Last = f;
    scope->Funs->Fn = fn;
  }
}

static struct BExpr *parseArm(struct LE *li, const char *op) {
  struct BExpr *e;
  struct LE *l;
  e = arithmeticOp(op, parse(li, lvFun), parse(li->N, lvFun)); /* TODO: error */
  for (l = li->N->N; l; l = l->N) {
    e = arithmeticOp(op, e, parse(l, lvFun));
  }
  return e;
}

static struct BExpr *parseIf(struct LE *li, int lvl) {
  /* TODO: errors */
  struct FScope newscope;
  struct BExpr *e;
  newscope.Vars = NULL;
  newscope.Parent = curscope;
  curscope = &newscope;
  if (li->N->N) {
    /* if - else */
    void *l;
    beginIfElseStmt(parse(li, lvl));
    l = elseIfStmt(parse(li->N, lvl));
    e = endIfElseStmt(l, parse(li->N->N, lvl));
  } else {
    /* if without else */
    beginIfStmt(parse(li, lvl));
    endIfStmt(parse(li->N, lvl));
    e = NULL;
  }
  curscope = curscope->Parent;
  return e;
}
static struct BExpr *parseWhile(struct LE *li, int lvl) {
  struct FScope newscope;
  newscope.Vars = NULL;
  newscope.Parent = curscope;
  curscope = &newscope;
  beginWhileLoop(parse(li, lvl));
  endWhileLoop(parse(li->N, lvl | lvLoop));
  curscope = curscope->Parent;
  return NULL;
}

enum {
  bcNoBuiltin,
  bcDefun,
  bcArm,
  bcUnary,
  bcIf,
  bcWhile,
  bcBreak,
  bcContinue,
  bcTODO_Print,
  bcTODO_Set,
  bcTODO_Var
};
struct BuiltinCommand {
  const char *Name;
  unsigned long HashedName;
  int BuiltinCode;
};
static struct BuiltinCommand builtincommands[24];

void initParser() {
  unsigned i;
  i = 0;

#define ADDCMD(a, b)                                                           \
  builtincommands[i].Name = a;                                                 \
  builtincommands[i].BuiltinCode = b;                                          \
  i++;

  /* DO NOT ADD MORE COMMANDS THAN ARRAY ELEMENTS */

  ADDCMD("defun", bcDefun)
  ADDCMD("+", bcArm)
  ADDCMD("-", bcArm)
  ADDCMD("*", bcArm)
  ADDCMD("/", bcArm)
  ADDCMD("&&", bcArm)
  ADDCMD("||", bcArm)
  ADDCMD("<<", bcArm)
  ADDCMD(">>", bcArm)
  ADDCMD("==", bcArm)
  ADDCMD("!=", bcArm)
  ADDCMD("<", bcArm)
  ADDCMD(">", bcArm)
  ADDCMD("<=", bcArm)
  ADDCMD(">=", bcArm)
  ADDCMD("!", bcUnary)
  ADDCMD("~", bcUnary)
  ADDCMD("if", bcIf)
  ADDCMD("while", bcWhile)
  ADDCMD("break", bcBreak)
  ADDCMD("continue", bcContinue)
  ADDCMD("print", bcTODO_Print)
  ADDCMD("set", bcTODO_Set)
  ADDCMD("var", bcTODO_Var)

#undef ADDCMD

  assert(i == sizeof(builtincommands) / sizeof(struct BuiltinCommand));

  for (i = 0; i < sizeof(builtincommands) / sizeof(struct BuiltinCommand);
       ++i) {
    builtincommands[i].HashedName = hashName(builtincommands[i].Name);
  }
}

static int lookupBuiltin(const char *name, unsigned long hash) {
  unsigned i;
  for (i = 0; i < sizeof(builtincommands) / sizeof(struct BuiltinCommand);
       ++i) {
    if (hash == builtincommands[i].HashedName &&
        (strcmp(name, builtincommands[i].Name) == 0)) {
      return builtincommands[i].BuiltinCode;
    }
  }
  return bcNoBuiltin;
}

struct FFunction *findFunction(const char *name, unsigned long hash,
                               struct FScope *scope) {
  struct FScope *sc;
  struct FFunction *fn;
  for (sc = scope; sc; sc = sc->Parent) {
    struct FLocalFun *fn;
    for (fn = sc->Funs; fn; fn = fn->Last) {
      if (fn->Fn->HashedName == hash && strcmp(fn->Fn->Name, name) == 0) {
        return fn->Fn;
      }
    }
  }
  for (fn = functions; fn; fn = fn->Last) {
    if (fn->HashedName == hash && strcmp(fn->Name, name) == 0) {
      return fn;
    }
  }
  return NULL;
}

struct BExpr *parseFuncall(struct LE *li, unsigned long hash,
                           struct FScope *scope) {
  struct FFunction *fn;
  struct BIncompleteFuncall *c;
  struct LE *l;
  unsigned argsLen;
  fn = findFunction(li->V.S, hash, scope);
  if (!fn) {
    compileError(*li, "unknown function: \"%s\"", li->V.S);
  }
  c = beginFuncall(fn->Backend);
  argsLen = 0;
  for (l = li->N; l; l = l->N) {
    if (argsLen == fn->NParms) {
      compileError(*li, "too many arguments for function \"%s\"", li->V.S);
    }
    addArg(c, parse(l, lvFun));
    ++argsLen;
  }
  if (argsLen < fn->NParms) {
    compileError(*li, "too few arguments for function \"%s\"", li->V.S);
  }
  return endFuncall(c);
}

struct BExpr *parse(struct LE *l, int lvl) {
  if (!l) {
    return NULL;
  }
  switch (l->T) {
  case tyInt:
    return intLiteral(l->V.I);
  case tyString:
    return stringLiteral(l->V.S);
  case tyIdent: {
    unsigned i;
    unsigned long hash;
    struct FVar *v;
    struct FScope *s;
    hash = hashName(l->V.S);
    for (s = curscope; s; s = s->Parent) {
      for (v = s->Vars; v; v = v->Next) {
        if (v->HashedName == hash && strcmp(v->Name, l->V.S) == 0) {
          return varUsage(v->Backend);
        }
      }
    }
    for (i = 0; i < currentffunction->NParms; ++i) {
      if (currentffunction->Parms[i].HashedName == hash &&
          strcmp(currentffunction->Parms[i].Name, l->V.S) == 0) {
        return varUsage(currentffunction->Parms[i].Backend);
      }
    }
    compileError(*l, "unknown identifier: \"%s\"", l->V.S);
  }
  case tyList: {
    if (!l->V.L) {
      compileError(*l, "empty list in code mode");
    }
    if (l->V.L->T == tyIdent) {
      struct LE *li;
      int builtin;
      unsigned long hash;
      li = l->V.L;
      hash = hashName(li->V.S);
      builtin = lookupBuiltin(li->V.S, hash);
      switch (builtin) {
      case bcNoBuiltin:
        return parseFuncall(li, hash, curscope);
      case bcDefun:
        parseDefun(li->N, curscope, lvl);
        break;
      case bcTODO_Print:
        TODO_print(parse(li->N, lvl));
        break;
      case bcTODO_Set:
        return setVar(parse(li->N, lvl), parse(li->N->N, lvl));
      case bcTODO_Var: {
        struct FVar *last;
        last = curscope->Vars;
        curscope->Vars = getMem(sizeof(struct FVar));
        curscope->Vars->Next = last;
        curscope->Vars->Name = li->N->V.S;
        curscope->Vars->HashedName = hashName(li->N->V.S);
        curscope->Vars->Backend = addVariable(li->N->V.S);
        return varUsage(curscope->Vars->Backend);
      } break;
      case bcArm:
        return parseArm(li->N, li->V.S);
      case bcUnary:
        return unaryOp(*li->V.S, parse(li->N, lvl));
      case bcIf:
        return parseIf(li->N, lvl);
      case bcWhile:
        return parseWhile(li->N, lvl);
      case bcBreak:
        if (!(lvl & lvLoop)) {
          compileError(*li, "break/continue outside of a loop");
        }
        breakLoop();
        return NULL;
      case bcContinue:
        if (!(lvl & lvLoop)) {
          compileError(*li, "break/continue outside of a loop");
        }
        continueLoop();
        return NULL;
      }
    } else if (l->V.L->T == tyList) { /* group expression */
      if (lvl & lvFun) {
        struct FScope newscope;
        struct LE *li;
        struct BExpr *e;
        newscope.Vars = NULL;
        newscope.Funs = NULL;
        newscope.Parent = curscope;
        curscope = &newscope;
        for (li = l->V.L; li && li->N; li = li->N) {
          addEvaluation(parse(li, lvl));
        }
        e = parse(li, lvl);
        curscope = curscope->Parent;
        return e;
      } else if (lvl & lvTop) {
        struct LE *li;
        for (li = l->V.L; li && li->N; li = li->N) {
          parse(li, lvl);
        }
        return parse(li, lvl);
      }
    } else {
      compileError(*l, "a list (in code mode) must start with either another "
                       "list or an identifier (function call)");
    }
    break;
  }
  }

  return NULL;
}
