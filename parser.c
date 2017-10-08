#include "prg.h"

#include <assert.h>
#include <string.h>

enum { lvTop = 0x1, lvFun = 0x2, lvLoop = 0x4 };
static struct FExpr parse(struct LE *l, int lvl);

enum { ttNone, ttVoid, ttInt };

struct FIntType {
  unsigned char IntSize; /* bytes */
  unsigned char Signed;
};

union FTypeData {
  struct FIntType Int;
};

struct FType {
  unsigned char Type;
  unsigned char Flags;
  union FTypeData Data;
  struct BType *Backend;
  struct FTypeAlias *AliasUsed;
};

struct FExpr {
  struct BExpr *Backend;
  struct FType Type;
};

struct FParm {
  unsigned long HashedName;
  const char *Name;
  struct BVar *Backend;
  struct FType Type;
  struct LE *L; /* stupid hack; ignore! */
};

struct FVar {
  unsigned long HashedName;
  const char *Name;
  struct BVar *Backend;
  struct FType Type;

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
  struct FType RetType;

  struct FFunction *Last;
};

struct FTypeAlias {
  unsigned long HashedName;
  const char *Name;
  struct FType T;
  struct FTypeAlias *Last;
};

static struct FFunction *functions;
static struct FFunction *currentffunction;
static struct FScope *curscope;
static struct FTypeAlias *typealiases;

static unsigned long hashName(const char *s) {
  unsigned long res;
  while (*s++) {
    res = *s * 11;
  }
  return res;
}

static struct FExpr voidExpr() {
  struct FExpr e;
  e.Type.Type = ttVoid;
  e.Type.Flags = 0;
  e.Backend = NULL;
  e.Type.Backend = voidType();
  return e;
}

static struct FExpr makeExpr(struct BExpr *backend, struct FType type) {
  struct FExpr e;
  e.Backend = backend;
  e.Type = type;
  return e;
}

static const char *printType(struct FType t) {
  if (t.AliasUsed) {
    return printToMem("%s (aka %s)", t.AliasUsed->Name,
                      printType(t.AliasUsed->T));
  }
  switch (t.Type) {
  case ttVoid:
    return "<void>";
  case ttInt:
    return printToMem("%c%i", t.Data.Int.Signed ? 'i' : 'u',
                      t.Data.Int.IntSize * 8);
  }
  assert(0); /* should never be reached */
}

/* try to implicitly convert to a given type; fail if not possible */
static struct FExpr convertType(struct FExpr e, struct FType want,
                                struct LE *l) {
  if (want.Type == ttInt && e.Type.Type == ttInt) {
    if (e.Type.Data.Int.Signed == want.Data.Int.Signed &&
        want.Data.Int.IntSize == e.Type.Data.Int.IntSize) {
      return e;
    }
    /*
     * Signed   -> Unsigned => NEVER WORKS
     * Unsigned -> Signed   => Works if WantedSize >  HaveSize
     * Same     -> Same     => Works if WantedSize >= HaveSize
     */
    if (((!e.Type.Data.Int.Signed == !want.Data.Int.Signed) ||
         (!e.Type.Data.Int.Signed && want.Data.Int.Signed)) &&
        want.Data.Int.IntSize > e.Type.Data.Int.IntSize) {
      e.Type = want;
      e.Backend = castInt(e.Backend, want.Backend);
      return e;
    }
  }
  compileError(*l, "incompatible types: found %s; expected %s",
               printType(e.Type), printType(want));
  return voidExpr(); /* should never be reached; silences compiler warning */
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
  bcAlias,
  bcTODO_Print,
  bcTODO_Set,
  bcTODO_Var
};
struct BuiltinCommand {
  const char *Name;
  unsigned long HashedName;
  int BuiltinCode;
};
static struct BuiltinCommand builtincommands[25];

enum {
  btNoBuiltin,
  btInt8,
  btInt16,
  btInt32,
  btInt64,
  btUint8,
  btUint16,
  btUint32,
  btUint64
};
struct BuiltinType {
  const char *Name;
  unsigned long HashedName;
  int BuiltinCode;
};
static struct BuiltinType builtintypes[8];

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
  ADDCMD("alias", bcAlias)
  ADDCMD("print", bcTODO_Print)
  ADDCMD("set", bcTODO_Set)
  ADDCMD("var", bcTODO_Var)

#undef ADDCMD

  assert(i == sizeof(builtincommands) / sizeof(struct BuiltinCommand));

  for (i = 0; i < sizeof(builtincommands) / sizeof(struct BuiltinCommand);
       ++i) {
    builtincommands[i].HashedName = hashName(builtincommands[i].Name);
  }

  /* types */

  i = 0;

#define ADDTY(a, b)                                                            \
  builtintypes[i].Name = a;                                                    \
  builtintypes[i].BuiltinCode = b;                                             \
  i++;

  /* DO NOT ADD MORE TYPES THAN ARRAY ELEMENTS */

  ADDTY("i8", btInt8)
  ADDTY("i16", btInt16)
  ADDTY("i32", btInt32)
  ADDTY("i64", btInt64)
  ADDTY("u8", btUint8)
  ADDTY("u16", btUint16)
  ADDTY("u32", btUint32)
  ADDTY("u64", btUint64)

#undef ADDTY

  assert(i == sizeof(builtintypes) / sizeof(struct BuiltinType));

  for (i = 0; i < sizeof(builtintypes) / sizeof(struct BuiltinType); ++i) {
    builtintypes[i].HashedName = hashName(builtintypes[i].Name);
  }
}

static int lookupBuiltinFn(const char *name, unsigned long hash) {
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

static int lookupBuiltinType(const char *name, unsigned long hash) {
  unsigned i;
  for (i = 0; i < sizeof(builtintypes) / sizeof(struct BuiltinType); ++i) {
    if (hash == builtintypes[i].HashedName &&
        (strcmp(name, builtintypes[i].Name) == 0)) {
      return builtintypes[i].BuiltinCode;
    }
  }
  return btNoBuiltin;
}

static struct FType parseType(struct LE *li) {
  int builtincode;
  unsigned long hash;
  struct FType r;
  r.Flags = 0;
  r.AliasUsed = NULL;
  hash = hashName(li->V.S); /* TODO: error */
  builtincode = lookupBuiltinType(li->V.S, hash);
  switch (builtincode) {
  case btInt8:
    r.Type = ttInt;
    r.Data.Int.IntSize = 1;
    r.Data.Int.Signed = 1;
    break;
  case btInt16:
    r.Type = ttInt;
    r.Data.Int.IntSize = 2;
    r.Data.Int.Signed = 1;
    break;
  case btInt32:
    r.Type = ttInt;
    r.Data.Int.IntSize = 4;
    r.Data.Int.Signed = 1;
    break;
  case btInt64:
    r.Type = ttInt;
    r.Data.Int.IntSize = 8;
    r.Data.Int.Signed = 1;
    break;
  case btUint8:
    r.Type = ttInt;
    r.Data.Int.IntSize = 1;
    r.Data.Int.Signed = 0;
    break;
  case btUint16:
    r.Type = ttInt;
    r.Data.Int.IntSize = 2;
    r.Data.Int.Signed = 0;
    break;
  case btUint32:
    r.Type = ttInt;
    r.Data.Int.IntSize = 4;
    r.Data.Int.Signed = 0;
    break;
  case btUint64:
    r.Type = ttInt;
    r.Data.Int.IntSize = 8;
    r.Data.Int.Signed = 0;
    break;
  case btNoBuiltin: /* (fallthrough intended) */
  default: {
    struct FTypeAlias *a;
    for (a = typealiases; a; a = a->Last) {
      if (a->HashedName == hash && strcmp(a->Name, li->V.S) == 0) {
        r = a->T;
        r.AliasUsed = a;
        return r;
      }
    }
  }
    compileError(*li, "unknown type: %s", li->V.S);
  }
  switch (r.Type) {
  case ttInt:
    r.Backend = intType(r.Data.Int.Signed, r.Data.Int.IntSize);
    break;
  }
  return r;
}

/* TODO: closures */
static void parseDefun(struct LE *li, struct FScope *scope, int lvl) {
  /* TODO: error */
  struct FFunction *fn, *lastfn;
  struct LE *l;
  struct FExpr ret;
  unsigned i;
  /* TODO */
  fn = getMem(sizeof(struct FFunction));
  fn->HashedName = hashName(li->V.S);
  fn->Name = li->V.S;
  fn->Parent = currentffunction;
  fn->RetType = parseType(li->N->N);
  beginFnPrototype(li->V.S, fn->RetType.Backend);
  for (l = li->N->V.L; l; l = l->N) {
    fn->Parms = moreMem(fn->Parms, sizeof(struct FParm) * fn->NParms,
                        sizeof(struct FParm));
    fn->Parms[fn->NParms].Name = l->V.L->N->V.S;
    fn->Parms[fn->NParms].HashedName = hashName(l->V.L->N->V.S);
    fn->Parms[fn->NParms].L = l->V.L;
    fn->NParms++;
  }
  for (i = 0; i < fn->NParms; ++i) {
    fn->Parms[i].Type = parseType(fn->Parms[i].L);
  }
  for (i = 0; i < fn->NParms; ++i) {
    fn->Parms[i].Backend =
        addParameter(fn->Parms[i].Name, fn->Parms[i].Type.Backend);
  }
  fn->Last = functions;
  functions = fn;
  lastfn = currentffunction;
  currentffunction = fn;
  fn->Backend = endFnPrototype(1 /* add body */);

  ret = convertType(parse(li->N->N->N, lvFun), fn->RetType, li->N->N->N);
  endFnBody(ret.Backend);

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

static struct FType resultTypeOfArmExpr(struct FIntType a, struct FIntType b) {
  struct FType r;
  r.Type = ttInt;
  r.Flags = 0;
  r.AliasUsed = NULL;
  r.Data.Int.Signed = a.Signed || b.Signed;
  r.Data.Int.IntSize = a.IntSize > b.IntSize ? a.IntSize : b.IntSize;
  return r;
}

static struct FExpr parseArm(struct LE *li, const char *op) {
  struct FExpr e, left, right;
  struct LE *l;
  left = parse(li, lvFun);
  right = parse(li->N, lvFun);
  if (left.Type.Type != ttInt) {
    compileError(*li,
                 "mathematical operation with something other than integers");
  }
  if (right.Type.Type != ttInt) {
    compileError(*li->N,
                 "mathematical operation with something other than integers");
  }
  e.Type = resultTypeOfArmExpr(left.Type.Data.Int, right.Type.Data.Int);
  e.Backend =
      arithmeticOp(op, left.Backend, right.Backend, e.Type.Data.Int.Signed,
                   e.Type.Data.Int.IntSize); /* TODO: error */
  for (l = li->N->N; l; l = l->N) {
    right = parse(l, lvFun);
    if (right.Type.Type != ttInt) {
      compileError(*li->N,
                   "mathematical operation with something other than integers");
    }
    e.Type = resultTypeOfArmExpr(e.Type.Data.Int, right.Type.Data.Int);
    e.Backend = arithmeticOp(op, e.Backend, right.Backend,
                             e.Type.Data.Int.Signed, e.Type.Data.Int.IntSize);
  }
  return e;
}

int typeWorksAsCondition(struct FType t) { return t.Type == ttInt; }

static struct FExpr parseIf(struct LE *li, int lvl) {
  /* TODO: errors */
  struct FScope newscope;
  struct FExpr e;
  struct FExpr cond, ifpart, elsepart;
  newscope.Vars = NULL;
  newscope.Parent = curscope;
  curscope = &newscope;
  if (li->N->N) {
    /* if - else */
    void *l;
    cond = parse(li, lvl);
    if (!typeWorksAsCondition(cond.Type)) {
      compileError(*li, "ill typed condition");
    }
    beginIfElseStmt(cond.Backend);
    ifpart = parse(li->N, lvl);
    l = elseIfStmt(ifpart.Backend,
                   ifpart.Type.Backend); /* TODO: smarter typing */
    elsepart = parse(li->N->N, lvl);
    e.Backend = endIfElseStmt(l, elsepart.Backend);
    e.Type = ifpart.Type; /* TODO: check type compatibility */
  } else {
    /* if without else */
    cond = parse(li, lvl);
    if (!typeWorksAsCondition(cond.Type)) {
      compileError(*li, "ill typed condition");
    }
    beginIfStmt(cond.Backend);
    ifpart = parse(li->N, lvl);
    endIfStmt(ifpart.Backend);
    e = voidExpr();
  }
  curscope = curscope->Parent;
  return e;
}
static void parseWhile(struct LE *li, int lvl) {
  struct FScope newscope;
  struct FExpr cond;
  newscope.Vars = NULL;
  newscope.Parent = curscope;
  curscope = &newscope;
  cond = parse(li, lvl);
  if (!typeWorksAsCondition(cond.Type)) {
    compileError(*li, "ill typed condition");
  }
  beginWhileLoop(cond.Backend);
  addEvaluation(parse(li->N, lvl | lvLoop).Backend);
  endWhileLoop();
  curscope = curscope->Parent;
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

struct FExpr parseFuncall(struct LE *li, unsigned long hash,
                          struct FScope *scope) {
  struct FFunction *fn;
  struct BIncompleteFuncall *c;
  struct LE *l;
  struct FExpr f;
  struct FExpr r;
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
    f = convertType(parse(l, lvFun), fn->Parms[argsLen].Type, l);
    addArg(c, f.Backend);
    ++argsLen;
  }
  if (argsLen < fn->NParms) {
    compileError(*li, "too few arguments for function \"%s\"", li->V.S);
  }
  r.Backend = endFuncall(c);
  r.Type = fn->RetType;
  return r;
}

static void parseAlias(struct LE *li) {
  struct FTypeAlias *b;
  b = getMem(sizeof(struct FTypeAlias));
  b->HashedName = hashName(li->V.S);
  b->Name = li->V.S;
  b->T = parseType(li->N);
  b->Last = typealiases;
  typealiases = b;
}

static struct FExpr parse(struct LE *l, int lvl) {
  if (!l) {
    return voidExpr();
  }
  switch (l->T) {
  case tyInt: {
    struct FType t;
    t.Type = ttInt;
    t.AliasUsed = NULL;
    t.Flags = 0;
    t.Data.Int.IntSize = 4;
    t.Data.Int.Signed = 1;
    return makeExpr(intLiteral(l->V.I), t);
  }
  /*
case tyString:
  return stringLiteral(l->V.S);
  */
  case tyIdent: {
    unsigned i;
    unsigned long hash;
    struct FVar *v;
    struct FScope *s;
    hash = hashName(l->V.S);
    for (s = curscope; s; s = s->Parent) {
      for (v = s->Vars; v; v = v->Next) {
        if (v->HashedName == hash && strcmp(v->Name, l->V.S) == 0) {
          return makeExpr(varUsage(v->Backend), v->Type);
        }
      }
    }
    for (i = 0; i < currentffunction->NParms; ++i) {
      if (currentffunction->Parms[i].HashedName == hash &&
          strcmp(currentffunction->Parms[i].Name, l->V.S) == 0) {
        return makeExpr(varUsage(currentffunction->Parms[i].Backend),
                        currentffunction->Parms[i].Type);
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
      builtin = lookupBuiltinFn(li->V.S, hash);
      switch (builtin) {
      case bcNoBuiltin:
        return parseFuncall(li, hash, curscope);
      case bcDefun:
        parseDefun(li->N, curscope, lvl);
        break;
      case bcTODO_Print:
        TODO_print(parse(li->N, lvl).Backend);
        break;
      case bcTODO_Set: {
        struct FExpr lhs, rhs;
        lhs = parse(li->N, lvl);
        rhs = parse(li->N->N, lvl);
        rhs = convertType(rhs, lhs.Type, li->N->N);
        return makeExpr(setVar(lhs.Backend, rhs.Backend), lhs.Type);
      }
      case bcTODO_Var: {
        struct FVar *last;
        last = curscope->Vars;
        curscope->Vars = getMem(sizeof(struct FVar));
        curscope->Vars->Next = last;
        curscope->Vars->Type = parseType(li->N);
        curscope->Vars->Name = li->N->N->V.S;
        curscope->Vars->HashedName = hashName(li->N->N->V.S);
        curscope->Vars->Backend =
            addVariable(li->N->N->V.S, curscope->Vars->Type.Backend);
        return makeExpr(varUsage(curscope->Vars->Backend),
                        curscope->Vars->Type);
      } break;
      case bcArm:
        /* TODO: check type */
        return parseArm(li->N, li->V.S);
      case bcUnary: {
        struct FExpr a;
        a = parse(li->N, lvl);
        /* TODO: check type */
        return makeExpr(unaryOp(*li->V.S, a.Backend, a.Type.Data.Int.Signed,
                                a.Type.Data.Int.IntSize),
                        a.Type);
      }
      case bcIf:
        return parseIf(li->N, lvl);
      case bcWhile:
        parseWhile(li->N, lvl);
        break;
      case bcBreak:
        if (!(lvl & lvLoop)) {
          compileError(*li, "break/continue outside of a loop");
        }
        breakLoop();
        break;
      case bcContinue:
        if (!(lvl & lvLoop)) {
          compileError(*li, "break/continue outside of a loop");
        }
        continueLoop();
        break;
      case bcAlias:
        parseAlias(li->N);
        break;
      }
    } else if (l->V.L->T == tyList) { /* group expression */
      if (lvl & lvFun) {
        struct FScope newscope;
        struct LE *li;
        struct FExpr e;
        newscope.Vars = NULL;
        newscope.Funs = NULL;
        newscope.Parent = curscope;
        curscope = &newscope;
        for (li = l->V.L; li && li->N; li = li->N) {
          addEvaluation(parse(li, lvl).Backend);
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

  return voidExpr();
}

void parseSrc(struct LE *list) {
  struct LE l;
  l.T = tyList;
  l.V.L = list;
  parse(&l, lvTop);
}
