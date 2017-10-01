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

struct FScope {
  struct FVar *Vars;

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
static void parseDefun(struct LE *li) {
  /* TODO: error */
  struct FFunction *fn;
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
  currentffunction = fn;
  fn->Backend = endFnPrototype(1 /* add body */);

  if (!li->N->N->V.L) {
    compileError(*li->N->N->V.L, "empty list as function body");
  }
  if (li->N->N->V.L->T != tyList) {
    endFnBody(parse(li->N->N, lvFun));
  } else {
    for (l = li->N->N->V.L; l && l->N; l = l->N) {
      addEvaluation(parse(l, lvFun));
    }
    endFnBody(parse(l, lvFun));
  }
}

static struct BExpr *parseArm(struct LE *li, char op) {
  struct BExpr *e;
  struct LE *l;
  e = arithmeticOp(op, parse(li, lvFun), parse(li->N, lvFun)); /* TODO: error */
  for (l = li->N->N; l; l = l->N) {
    e = arithmeticOp(op, e, parse(l, lvFun));
  }
  return e;
}

static struct BExpr *parseIf(struct LE *li) {
  /* TODO: errors */
  return ifStmt(parse(li, lvFun), parse(li->N, lvFun),
                li->N->N ? parse(li->N->N, lvFun) : NULL);
}

enum {
  bcNoBuiltin,
  bcDefun,
  bcArm,
  bcIf,
  bcTODO_Print,
  bcTODO_Set,
  bcTODO_Var
};
struct BuiltinCommand {
  const char *Name;
  unsigned long HashedName;
  int BuiltinCode;
};
static struct BuiltinCommand builtincommands[8];

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
  ADDCMD("if", bcIf)
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

struct BExpr *parseFuncall(struct LE *li, unsigned long hash) {
  struct FFunction *fn;
  for (fn = functions; fn; fn = fn->Last) {
    if (fn->HashedName == hash && strcmp(fn->Name, li->V.S) == 0) {
      struct BIncompleteFuncall *c;
      struct LE *l;
      unsigned argsLen;
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
  }
  compileError(*li, "unknown function: \"%s\"", li->V.S);
  return NULL;
}

struct BExpr *parse(struct LE *l, enum ParserLevel lvl) {
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
      case bcNoBuiltin: {
        return parseFuncall(li, hash);
      }
      case bcDefun:
        parseDefun(li->N);
        break;
      case bcTODO_Print:
        TODO_print(parse(li->N, lvFun));
        break;
      case bcTODO_Set:
        return setVar(parse(li->N, lvFun), parse(li->N->N, lvFun));
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
        return parseArm(li->N, *li->V.S);
      case bcIf:
        return parseIf(li->N);
      }
    } else if (l->V.L->T == tyList) { /* group expression */
      if (lvl == lvFun) {
        struct FScope newscope;
        struct LE *li;
        struct BExpr *e;
        struct BScope *backend;
        newscope.Vars = NULL;
        newscope.Parent = curscope;
        curscope = &newscope;
        backend = beginScope();
        for (li = l->V.L; li && li->N; li = li->N) {
          addEvaluation(parse(li, lvFun));
        }
        e = endScope(backend, parse(li, lvFun));
        curscope = curscope->Parent;
        return e;
      } else if (lvl == lvTop) {
        struct LE *li;
        for (li = l->V.L; li && li->N; li = li->N) {
          parse(li, lvTop);
        }
        return parse(li, lvTop);
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
