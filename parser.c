#include "prg.h"

#include <alloca.h>
#include <assert.h>
#include <string.h>

enum { lvTop = 0x1, lvFun = 0x2, lvLoop = 0x4 };
enum { ffExplicitCast = ffLast << 1, ffImplicitCast = ffLast << 2 };
static struct FExpr parse(struct LE *l, int lvl);

enum {
  ttVoid,
  ttInt,
  ttFloat,
  ttStruct,
  ttPointer,
  ttFunPointer,
  ttArray,
  ttOpaque
};

struct FIntType {
  unsigned char IntSize; /* bytes */
  unsigned char Flags;
};

struct FStructType {
  struct FStruct *S;
};

enum { ptRaw = 1, ptRef };
enum { pfVolatile = 0x1 };
struct FPtrType {
  struct FType *Pointee;
  int Type, Flags;
};

struct FFunPtrType {
  struct FType *RetType;
  struct FType *Parms;
  int NParms;
};

struct FArrayType {
  struct FType *Pointee;
  int Size;
};

struct FOpaqueType {
  struct FOpaque *O;
};

union FTypeData {
  struct FIntType Int;
  int FloatSize;
  struct FStructType Struct;
  struct FPtrType Ptr;
  struct FFunPtrType FunPtr;
  struct FArrayType Array;
  struct FOpaqueType Opaque;
};

enum { tfConst = 0x1 };

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

enum { vfVolatile = 0x1 };

struct FVar {
  unsigned long HashedName;
  const char *Name;
  struct BVar *Backend;
  struct FType Type;
  int Flags;

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
  int Flags;

  struct FFunction *Last;
};

struct FTypeAlias {
  unsigned long HashedName;
  const char *Name;
  struct FType T;
  struct FTypeAlias *Last;
};

struct FFnAliasEntry {
  struct FFunction *F;
  struct FFnAliasEntry *N;
};

struct FFnAlias {
  unsigned long HashedName;
  const char *Name;
  struct FFnAliasEntry *Entry;
  struct FFnAlias *Last;
};

struct FStructMember {
  unsigned long HashedName;
  const char *Name;
  struct BStructMember *Backend;
  struct FType Type;
  struct LE *L; /* stupid hack */
};

struct FMemberMacro {
  unsigned long HashedName;
  const char *Name;
  struct MMemberMacro *Middleend;
  struct FMemberMacro *Last;
};

struct FStruct {
  unsigned long HashedName;
  const char *Name;
  struct BStruct *Backend;
  struct FStructMember *Members;
  unsigned long NMembers;
  struct FMemberMacro *Macros;
  struct FStruct *Last;
};

struct FOpaque {
  unsigned long HashedName;
  const char *Name;
  struct FType *T;
  struct FOpaque *Last;
};

struct FGlobal {
  unsigned long HashedName;
  const char *Name;
  struct BVar *Backend;
  struct FType Type;
  int Flags;
  struct FGlobal *Last;
};

static struct FFunction *functions;
static struct FFunction *currentffunction;
static struct FScope *curscope;
static struct FTypeAlias *typealiases;
static struct FFnAlias *fnaliases;
static struct FStruct *structs;
static struct FGlobal *globals;
static struct FOpaque *opaques;

static unsigned countLen(struct LE *l) {
  unsigned i;
  for (i = 0; l; l = l->N) {
    ++i;
  }
  return i;
}

unsigned long hashName(const char *s) {
  unsigned long res;
  res = 0;
  while (*s) {
    res = (res + *s) * 11;
    ++s;
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
  const char *r;
  r = NULL;
  if (t.AliasUsed) {
    t.AliasUsed = t.AliasUsed->T.AliasUsed;
    return printToMem("%s [aka %s]", t.AliasUsed->Name, printType(t));
  }
  switch (t.Type) {
  case ttVoid:
    r = "<void>";
    break;
  case ttInt: {
    /* TODO: new int types */
    if (t.Data.Int.Flags & ifChar) {
      r = "char";
      break;
    }
    if (t.Data.Int.Flags & ifCType) {
      const char *const uns =
          (t.Data.Int.Flags & ifSigned) ? "signed " : "unsigned ";
      switch (t.Data.Int.IntSize) {
      case 1:
        r = printToMem("%schar", uns);
        break;
      case 2:
        r = printToMem("%sshort int", uns);
        break;
      case 4:
        r = printToMem("%sint", uns);
        break;
      case 8:
        r = printToMem("%slong long int", uns);
        break;
      }
    } else {
      r = printToMem("%sint%i_t", (t.Data.Int.Flags & ifSigned) ? "" : "u",
                     t.Data.Int.IntSize * 8);
    }
  } break;
  case ttFloat:
    r = t.Data.FloatSize == 8 ? "float" : "sfloat";
    break;
  case ttPointer: {
    switch (t.Data.Ptr.Type) {
    case ptRaw:
      if (t.Data.Ptr.Pointee->Type == ttOpaque) {
        r = printToMem("%sopaque-ptr %s",
                       t.Data.Ptr.Flags & pfVolatile ? "volatile-" : "",
                       t.Data.Ptr.Pointee->Data.Opaque.O->Name);
        break;
      }
      r = printToMem("%sptr %s",
                     t.Data.Ptr.Flags & pfVolatile ? "volatile-" : "",
                     printType(*t.Data.Ptr.Pointee));
      break;
    case ptRef:
      r = printToMem("%sref %s",
                     t.Data.Ptr.Flags & pfVolatile ? "volatile-" : "",
                     printType(*t.Data.Ptr.Pointee));
      break;
    default:
      assert(0); /* hopefully never reached */
      break;
    }
  } break;
  case ttFunPointer: {
    int i;
    r = printToMem("(funptr %s (", printType(*t.Data.FunPtr.RetType));
    for (i = 0; i < t.Data.FunPtr.NParms; ++i) {
      r = printToMem("%s (%s)", r, printType(t.Data.FunPtr.Parms[i]));
    }
    r = printToMem("%s))", r);
  } break;
  case ttArray:
    r = printToMem("(array %i %s)", t.Data.Array.Size,
                   printType(*t.Data.Array.Pointee));
    break;
  case ttStruct:
    r = t.Data.Struct.S->Name;
    break;
  case ttOpaque:
    assert(0 && "opaque type alone can not be printed");
    break;
  default:
    assert(0); /* should never be reached */
  }
  if (t.Flags & tfConst) {
    r = printToMem("const %s", r);
  }
  return r;
}

static int typeEquals(struct FType a, struct FType b) {
  if (a.Type != b.Type || a.Flags != b.Flags) {
    return 0;
  }
  switch (a.Type) {
  case ttVoid:
    return 1;
  case ttInt:
    return a.Data.Int.IntSize == b.Data.Int.IntSize &&
           a.Data.Int.Flags == b.Data.Int.Flags;
  case ttFloat:
    return a.Data.FloatSize == b.Data.FloatSize;
  case ttStruct:
    return a.Data.Struct.S == b.Data.Struct.S;
  case ttPointer:
    return a.Data.Ptr.Type == b.Data.Ptr.Type &&
           a.Data.Ptr.Flags == b.Data.Ptr.Flags &&
           typeEquals(*a.Data.Ptr.Pointee, *b.Data.Ptr.Pointee);
  case ttFunPointer: {
    int i;
    if (!typeEquals(*a.Data.FunPtr.RetType, *b.Data.FunPtr.RetType) ||
        a.Data.FunPtr.NParms != b.Data.FunPtr.NParms) {
      return 0;
    }
    for (i = 0; i < a.Data.FunPtr.NParms; ++i) {
      if (!typeEquals(a.Data.FunPtr.Parms[i], b.Data.FunPtr.Parms[i])) {
        return 0;
      }
    }
    return 1;
  }
  case ttArray:
    return a.Data.Array.Size == b.Data.Array.Size &&
           typeEquals(*a.Data.Array.Pointee, *b.Data.Array.Pointee);
  case ttOpaque:
    return a.Data.Opaque.O == b.Data.Opaque.O;
  }
  return 0; /* should never be reached */
}

static struct FType pointerTo(struct FType a, int isvolatile) {
  struct FType r;
  r.Type = ttPointer;
  r.Data.Ptr.Type = ptRaw;
  r.Data.Ptr.Pointee = getMem(sizeof(struct FType));
  *r.Data.Ptr.Pointee = a;
  r.Backend = ptrType(a.Backend, isvolatile);
  return r;
}

/* try to implicitly convert to a given type; fail if not possible */
static int tryConvertType(struct FExpr e, struct FType want, struct FExpr *r) {
  if (typeEquals(e.Type, want)) {
    if (r) {
      *r = e;
    }
    return 0;
  }
  if (want.Type == ttPointer && want.Data.Ptr.Type == ptRef &&
      typeEquals(*want.Data.Ptr.Pointee, e.Type)) {
    if (r) {
      r->Type = want;
      r->Backend = refof(e.Backend);
    }
    return 0;
  }
  if (e.Type.Type == ttPointer && e.Type.Data.Ptr.Type == ptRaw &&
      e.Type.Data.Ptr.Pointee->Type == ttOpaque &&
      e.Type.Data.Ptr.Pointee->Data.Opaque.O->T) {
    return tryConvertType(
        makeExpr(
            castPtr(e.Backend,
                    ptrType(e.Type.Data.Ptr.Pointee->Data.Opaque.O->T->Backend,
                            e.Type.Data.Ptr.Flags & pfVolatile)),
            pointerTo(*e.Type.Data.Ptr.Pointee->Data.Opaque.O->T,
                      e.Type.Data.Ptr.Flags & pfVolatile)),
        want, r);
  }
  if (want.Type == ttPointer && want.Data.Ptr.Type == ptRaw &&
      want.Data.Ptr.Pointee->Type == ttOpaque &&
      want.Data.Ptr.Pointee->Data.Opaque.O->T) {
    return tryConvertType(e, pointerTo(*want.Data.Ptr.Pointee->Data.Opaque.O->T,
                                       want.Data.Ptr.Flags & pfVolatile),
                          r);
  }
  if (want.Type == ttInt && e.Type.Type == ttInt) {
    want.Flags = e.Type.Flags & want.Type;
    /*
     * Signed   -> Unsigned => NEVER WORKS
     * Unsigned -> Signed   => Works if WantedSize >  HaveSize
     * Same     -> Same     => Works if WantedSize >= HaveSize
     */
    if (((!(e.Type.Data.Int.Flags & ifSigned) ==
          !(want.Data.Int.Flags & ifSigned)) ||
         (!(e.Type.Data.Int.Flags & ifSigned) &&
          (want.Data.Int.Flags & ifSigned))) &&
        want.Data.Int.IntSize > e.Type.Data.Int.IntSize) {
      if (r) {
        e.Type = want;
        e.Backend =
            castInt(e.Backend, want.Backend, e.Type.Data.Int.Flags & ifSigned);
        *r = e;
      }
      return 0;
    }
  } else if (want.Type == ttFloat && e.Type.Type == ttFloat) {
    want.Flags = e.Type.Flags & want.Type;
    if (e.Type.Data.FloatSize < want.Data.FloatSize) {
      if (r) {
        e.Type = want;
        e.Backend = castFloat(e.Backend, want.Backend);
        *r = e;
      }
      return 0;
    }
  }
  {
    struct FFunction *f, *chosen;
    struct FExpr fake;
    fake.Backend = NULL;
    chosen = NULL;
    for (f = currentffunction; f; f = f->Last) {
      if (f->Flags & ffImplicitCast) {
        fake.Type = f->RetType;
        if (!tryConvertType(e, f->Parms[0].Type, NULL) &&
            !tryConvertType(fake, want, NULL)) {
          if (chosen) {
            chosen = NULL;
            break;
            /* TODO: find a better solution */
          }
          chosen = f;
        }
      }
    }
    if (chosen && r) {
      struct FExpr funval;
      struct BIncompleteFuncall *call;
      funval = e;
      assert(!tryConvertType(funval, chosen->Parms[0].Type, &funval));
      e.Type = chosen->RetType;
      call = beginFuncall(chosen->Backend);
      addArg(call, funval.Backend);
      e.Backend = endFuncall(call);
      assert(!tryConvertType(e, want, &e));
      *r = e;
    }
    return chosen == NULL;
  }
  return 1;
}

static struct FExpr convertType(struct FExpr e, struct FType want,
                                struct LE *l) {
  if (tryConvertType(e, want, &e)) {
    compileError(*l, "incompatible types: found %s; expected %s",
                 printType(e.Type), printType(want));
  }
  return e;
}

static struct FExpr parseExplicitCast(struct FExpr have, struct FType want,
                                      struct LE *li) {
  struct FExpr e;
  e = have;
  want.Flags = e.Type.Flags;
  if (e.Type.Type == ttInt && want.Type == ttInt) {
    e.Type = want;
    e.Backend =
        castInt(e.Backend, want.Backend, e.Type.Data.Int.Flags & ifSigned);
    return e;
  }
  if (e.Type.Type == ttFloat && want.Type == ttFloat) {
    e.Type = want;
    e.Backend = castFloat(e.Backend, want.Backend);
    return e;
  }
  if (e.Type.Type == ttInt && want.Type == ttFloat) {
    e.Type = want;
    e.Backend = castIntToFloat(e.Backend, want.Backend,
                               e.Type.Data.Int.Flags & ifSigned);
    return e;
  }
  if (e.Type.Type == ttFloat && want.Type == ttInt) {
    e.Type = want;
    e.Backend =
        castFloatToInt(e.Backend, want.Backend, want.Data.Int.Flags & ifSigned);
    return e;
  }
  if (e.Type.Type == ttPointer && want.Type == ttPointer &&
      e.Type.Data.Ptr.Type == ptRaw && want.Data.Ptr.Type == ptRaw) {
    e.Type = want;
    e.Backend = castPtr(e.Backend, want.Backend);
    return e;
  }
  if (e.Type.Type == ttFunPointer && want.Type == ttFunPointer) {
    e.Type = want;
    e.Backend = castPtr(e.Backend, want.Backend);
    return e;
  }
  {
    struct FFunction *f, *chosen;
    struct FExpr fake;
    fake.Backend = NULL;
    chosen = NULL;
    for (f = currentffunction; f; f = f->Last) {
      if (f->Flags & ffExplicitCast) {
        fake.Type = f->RetType;
        if (!tryConvertType(e, f->Parms[0].Type, NULL) &&
            !tryConvertType(fake, want, NULL)) {
          if (chosen) {
            /* TODO: better diagnostics: give information about implicit casts
             * that are done and list all possiblities (not just two) */
            compileHint(*li, "found an ambiguous explicit cast:");
            compileHint(*li, " first function: %s", chosen->Name);
            compileHint(*li, " second function: %s", f->Name);
            compileError(*li, "ambiguous explicit cast (see above)");
          }
          chosen = f;
        }
      }
    }
    if (chosen) {
      struct FExpr funval;
      struct BIncompleteFuncall *call;
      funval = have;
      assert(!tryConvertType(funval, chosen->Parms[0].Type, &funval));
      e.Type = chosen->RetType;
      call = beginFuncall(chosen->Backend);
      addArg(call, funval.Backend);
      e.Backend = endFuncall(call);
      assert(!tryConvertType(e, want, &e));
      return e;
    }
  }
  compileError(*li, "don't know how to explicitly cast %s to %s",
               printType(e.Type), printType(want));
  return e; /* never reached; used to silence compiler warning */
}

enum {
  bcNoBuiltin,
  bcStatic,
  bcStaticRun,
  bcDefun,
  bcFunProto,
  bcCast,
  bcStruct,
  bcMemb,
  bcArm,
  bcCmp,
  bcUnary,
  bcIf,
  bcWhile,
  bcBreak,
  bcContinue,
  bcAlias,
  bcPtrDeref,
  bcPtrRefof,
  bcFuncall,
  bcGlobal,
  bcTODO_Set,
  bcTODO_Var
};
struct BuiltinCommand {
  const char *Name;
  unsigned long HashedName;
  int BuiltinCode;
};
static struct BuiltinCommand builtincommands[35];

enum {
  btNoBuiltin,
  btInt8,
  btInt16,
  btInt32,
  btInt64,
  btUint8,
  btUint16,
  btUint32,
  btUint64,
  btChar,
  btFloat,
  btShortFloat,
  btConst,
  btPtr,
  btRef,
  btVoid,
  btFunPtr,
  btArray,
  btCType,
  btOpaquePtr,
  btVolatileOpaquePtr,
  btVolatilePtr,
  btVolatileRef
};
struct BuiltinType {
  const char *Name;
  unsigned long HashedName;
  int BuiltinCode;
};
static struct BuiltinType builtintypes[22];

void initParser() {
  unsigned i;
  i = 0;

#define ADDCMD(a, b)                                                           \
  builtincommands[i].Name = a;                                                 \
  builtincommands[i].BuiltinCode = b;                                          \
  i++;

  /* DO NOT ADD MORE COMMANDS THAN ARRAY ELEMENTS */

  ADDCMD("static", bcStatic)
  ADDCMD("static-run", bcStaticRun)
  ADDCMD("defun", bcDefun)
  ADDCMD("funproto", bcFunProto)
  ADDCMD("cast", bcCast)
  ADDCMD("memb", bcMemb)
  ADDCMD("struct", bcStruct)
  ADDCMD("global", bcGlobal)
  ADDCMD("+", bcArm)
  ADDCMD("-", bcArm)
  ADDCMD("*", bcArm)
  ADDCMD("/", bcArm)
  ADDCMD("&&", bcArm)
  ADDCMD("||", bcArm)
  ADDCMD("<<", bcArm)
  ADDCMD(">>", bcArm)
  ADDCMD("%", bcArm)
  ADDCMD("==", bcCmp)
  ADDCMD("!=", bcCmp)
  ADDCMD("<", bcCmp)
  ADDCMD(">", bcCmp)
  ADDCMD("<=", bcCmp)
  ADDCMD(">=", bcCmp)
  ADDCMD("!", bcUnary)
  ADDCMD("~", bcUnary)
  ADDCMD("if", bcIf)
  ADDCMD("while", bcWhile)
  ADDCMD("break", bcBreak)
  ADDCMD("continue", bcContinue)
  ADDCMD("alias", bcAlias)
  ADDCMD("ptrto", bcPtrRefof)
  ADDCMD("ptr-deref", bcPtrDeref)
  ADDCMD("funcall", bcFuncall)
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
  ADDTY("char", btChar)
  ADDTY("float", btFloat)
  ADDTY("sfloat", btShortFloat)
  ADDTY("const", btConst)
  ADDTY("ptr", btPtr)
  ADDTY("ref", btRef)
  ADDTY("void", btVoid)
  ADDTY("funptr", btFunPtr)
  ADDTY("array", btArray)
  ADDTY("c-type", btCType)
  ADDTY("opaque-ptr", btOpaquePtr)
  ADDTY("volatile-opaque-ptr", btVolatileOpaquePtr)
  ADDTY("volatile-ptr", btVolatilePtr)
  ADDTY("volatile-ref", btVolatileRef)

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

static struct FType parseType(struct LE *li, int waslist) {
  int builtincode;
  unsigned long hash;
  struct FType r;
  memset(&r, 0, sizeof(struct FType));
  r.Flags = 0;
  r.AliasUsed = NULL;
  if (li->T == tyList) {
    waslist = 1;
    li = li->V.L;
  }
  hash = hashName(li->V.S); /* TODO: error */
  builtincode = lookupBuiltinType(li->V.S, hash);
  switch (builtincode) {
  case btInt8:
    r.Type = ttInt;
    r.Data.Int.IntSize = 1;
    r.Data.Int.Flags = ifSigned;
    break;
  case btInt16:
    r.Type = ttInt;
    r.Data.Int.IntSize = 2;
    r.Data.Int.Flags = ifSigned;
    break;
  case btInt32:
    r.Type = ttInt;
    r.Data.Int.IntSize = 4;
    r.Data.Int.Flags = ifSigned;
    break;
  case btInt64:
    r.Type = ttInt;
    r.Data.Int.IntSize = 8;
    r.Data.Int.Flags = ifSigned;
    break;
  case btUint8:
    r.Type = ttInt;
    r.Data.Int.IntSize = 1;
    r.Data.Int.Flags = 0;
    break;
  case btUint16:
    r.Type = ttInt;
    r.Data.Int.IntSize = 2;
    r.Data.Int.Flags = 0;
    break;
  case btUint32:
    r.Type = ttInt;
    r.Data.Int.IntSize = 4;
    r.Data.Int.Flags = 0;
    break;
  case btUint64:
    r.Type = ttInt;
    r.Data.Int.IntSize = 8;
    r.Data.Int.Flags = 0;
    break;
  case btChar:
    r.Type = ttInt;
    r.Data.Int.IntSize = 1;
    r.Data.Int.Flags = ifChar;
    break;
  case btFloat:
    r.Type = ttFloat;
    r.Data.FloatSize = 8;
    break;
  case btShortFloat:
    r.Type = ttFloat;
    r.Data.FloatSize = 4;
    break;
  case btConst:
    if (!waslist) {
      compileError(*li, "\"const\" requires arguments");
    }
    r = parseType(li->N, 1);
    r.Flags |= tfConst;
    constType(r.Backend);
    break;
  case btPtr:
    r.Type = ttPointer;
    r.Data.Ptr.Type = ptRaw;
    r.Data.Ptr.Pointee = getMem(sizeof(struct FType));
    if (waslist) {
      *r.Data.Ptr.Pointee = parseType(li->N, 1);
    } else {
      *r.Data.Ptr.Pointee = voidExpr().Type;
    }
    r.Backend = ptrType(r.Data.Ptr.Pointee->Backend, 0);
    break;
  case btVolatilePtr:
    r.Type = ttPointer;
    r.Data.Ptr.Type = ptRaw;
    r.Data.Ptr.Flags = pfVolatile;
    r.Data.Ptr.Pointee = getMem(sizeof(struct FType));
    if (waslist) {
      *r.Data.Ptr.Pointee = parseType(li->N, 1);
    } else {
      *r.Data.Ptr.Pointee = voidExpr().Type;
    }
    r.Backend = ptrType(r.Data.Ptr.Pointee->Backend, 1);
    break;
  case btRef:
    if (!waslist) {
      compileError(*li, "\"ref\" requires arguments");
    }
    r.Type = ttPointer;
    r.Data.Ptr.Type = ptRef;
    r.Data.Ptr.Pointee = getMem(sizeof(struct FType));
    *r.Data.Ptr.Pointee = parseType(li->N, 1);
    r.Backend = ptrType(r.Data.Ptr.Pointee->Backend, 0);
    break;
  case btVolatileRef:
    if (!waslist) {
      compileError(*li, "\"ref\" requires arguments");
    }
    r.Type = ttPointer;
    r.Data.Ptr.Type = ptRef;
    r.Data.Ptr.Flags = pfVolatile;
    r.Data.Ptr.Pointee = getMem(sizeof(struct FType));
    *r.Data.Ptr.Pointee = parseType(li->N, 1);
    r.Backend = ptrType(r.Data.Ptr.Pointee->Backend, 1);
    break;
  case btFunPtr: {
    struct LE *l;
    int len;
    int i;
    struct BType **parms;
    if (!waslist) {
      compileError(*li, "\"funptr\" requires arguments");
    }
    r.Type = ttFunPointer;
    r.Data.FunPtr.RetType = getMem(sizeof(struct FType));
    *r.Data.FunPtr.RetType = parseType(li->N, 1); /* TODO: error */
    len = countLen(li->N->N->V.L);
    r.Data.FunPtr.Parms = getMem(sizeof(struct FType) * len);
    parms = alloca(sizeof(struct BType *) * len);
    r.Data.FunPtr.NParms = len;
    i = 0;
    for (l = li->N->N->V.L; l; l = l->N) {
      r.Data.FunPtr.Parms[i] = parseType(l, 0);
      parms[i] = r.Data.FunPtr.Parms[i].Backend;
      ++i;
    }
    r.Backend = fnPtrType(r.Data.FunPtr.RetType->Backend, len, parms);
    return r;
  }
  case btArray:
    /* TODO: error */
    if (!waslist) {
      compileError(*li, "\"array\" requires arguments");
    }
    r.Type = ttArray;
    r.Data.Array.Pointee = getMem(sizeof(struct FType));
    *r.Data.Array.Pointee = parseType(li->N->N, 1);
    r.Data.Array.Size = li->N->V.I;
    r.Backend = arrayType(r.Data.Ptr.Pointee->Backend, r.Data.Array.Size);
    break;
  case btCType: {
    struct LE *l;
    int size, flags, issigned;
    if (!waslist) {
      compileError(*li, "\"c-type\" requires arguments");
    }
    size = 4;
    flags = ifCType;
    issigned = 1;
    for (l = li->N; l; l = l->N) {
      if (l->T != tyIdent) {
        compileError(*li, "\"c-type\" requires only identifiers");
      }
      if (strcmp(l->V.S, "signed") == 0) {
        issigned = 1;
      } else if (strcmp(l->V.S, "unsigned") == 0) {
        issigned = 0;
      } else if (strcmp(l->V.S, "char") == 0) {
        size = 1;
      } else if (strcmp(l->V.S, "long") == 0) {
        size *= 2;
      } else if (strcmp(l->V.S, "short") == 0) {
        size /= 2;
      } else if (strcmp(l->V.S, "int") != 0) {
        compileError(*l, "unknown c-type identifier");
      }
    }
    if (issigned) {
      flags |= ifSigned;
    }
    if (size == 16) /* long long */ {
      size = 8;
    }
    if (size == 0 || size > 8) {
      compileError(*li, "invalid c-type");
    }
    r.Type = ttInt;
    r.Data.Int.IntSize = size;
    r.Data.Int.Flags = flags;
  } break;
  case btOpaquePtr:
  case btVolatileOpaquePtr: {
    struct FOpaque *oq, *oqt;
    struct FType *t;
    unsigned long hash;
    if (!waslist) {
      compileError(*li, "\"opaque-ptr\" requires arguments");
    }
    hash = hashName(li->N->V.S);

    oqt = NULL;
    for (oq = opaques; oq; oq = oq->Last) {
      if (oq->HashedName == hash && strcmp(oq->Name, li->N->V.S) == 0) {
        oqt = oq;
        break;
      }
    }
    if (!oqt) {
      struct FOpaque *old;
      old = opaques;
      opaques = getMem(sizeof(struct FOpaque));
      opaques->Last = old;
      opaques->HashedName = hash;
      opaques->Name = li->N->V.S;
      oqt = opaques;
    }

    t = getMem(sizeof(struct FType));
    t->Type = ttOpaque;
    t->Data.Opaque.O = oqt;
    r.Type = ttPointer;
    r.Data.Ptr.Type = ptRaw;
    if (builtincode == btVolatileOpaquePtr) {
      r.Data.Ptr.Flags = pfVolatile;
    }
    r.Data.Ptr.Pointee = t;
    r.Backend = ptrType(voidType(), r.Data.Ptr.Flags & pfVolatile);
    return r;
  }
  case btVoid:
    return voidExpr().Type;
  case btNoBuiltin: /* (fallthrough intended) */
  default: {
    struct FTypeAlias *a;
    struct FStruct *st;
    for (a = typealiases; a; a = a->Last) {
      if (a->HashedName == hash && strcmp(a->Name, li->V.S) == 0) {
        r = a->T;
        r.AliasUsed = a;
        return r;
      }
    }
    for (st = structs; st; st = st->Last) {
      if (st->HashedName == hash && strcmp(st->Name, li->V.S) == 0) {
        r.Type = ttStruct;
        r.Data.Struct.S = st;
        r.Backend = structType(st->Backend);
        return r;
      }
    }
  }
    compileError(*li, "unknown type: %s", li->V.S);
  }
  switch (r.Type) {
  case ttInt:
    r.Backend = intType(r.Data.Int.Flags, r.Data.Int.IntSize);
    break;
  case ttFloat:
    r.Backend = floatType(r.Data.FloatSize);
    break;
  }
  return r;
}

/* TODO: closures */
static void parseDefun(struct LE *li, struct FScope *scope, int lvl,
                       int proto) {
  /* TODO: error */
  struct FFunction *fn, *lastfn;
  struct LE *l, *list;
  struct FExpr ret;
  unsigned i;
  int flags;
  /* TODO */
  flags = lvl & lvFun ? ffStatic : 0;
  if (li->T == tyList) {
    for (l = li->V.L; l; l = l->N) {
      if (strcmp("static", l->V.S) == 0) {
        flags |= ffStatic;
      } else if (strcmp("inline", l->V.S) == 0) {
        flags |= ffInline;
      } else if (strcmp("explicit-cast", l->V.S) == 0) {
        flags |= ffExplicitCast;
      } else if (strcmp("implicit-cast", l->V.S) == 0) {
        flags |= ffImplicitCast;
      } else {
        compileError(*l, "unknown function property: \"%s\"", l->V.S);
      }
    }
    li = li->N;
  }
  fn = getMem(sizeof(struct FFunction));
  fn->HashedName = hashName(li->V.S);
  fn->Name = li->V.S;
  fn->Parent = currentffunction;
  fn->RetType = parseType(li->N->N, 0);
  fn->Flags = flags;
  /* TODO: check functions which are casts */
  beginFnPrototype(li->V.S, fn->RetType.Backend, flags);
  for (l = li->N->V.L; l; l = l->N) {
    for (list = l->V.L->N; list; list = list->N) {
      fn->Parms = moreMem(fn->Parms, sizeof(struct FParm) * fn->NParms,
                          sizeof(struct FParm));
      fn->Parms[fn->NParms].Name = list->V.S;
      fn->Parms[fn->NParms].HashedName = hashName(list->V.S);
      fn->Parms[fn->NParms].L = l->V.L;
      fn->NParms++;
    }
  }
  for (i = 0; i < fn->NParms; ++i) {
    fn->Parms[i].Type = parseType(fn->Parms[i].L, 0);
  }
  for (i = 0; i < fn->NParms; ++i) {
    fn->Parms[i].Backend =
        addParameter(fn->Parms[i].Name, fn->Parms[i].Type.Backend);
  }
  fn->Last = functions;
  functions = fn;
  lastfn = currentffunction;
  currentffunction = fn;
  fn->Backend = endFnPrototype(!proto /* add body */);

  if (!proto) {
    for (i = 0; i < fn->NParms; ++i) {
      fn->Parms[i].Backend = updateParameter(fn->Parms[i].Backend);
    }
    ret = parse(li->N->N->N, lvFun);
    if (fn->RetType.Type != ttVoid) {
      ret = convertType(ret, fn->RetType, li->N->N->N);
      endFnBody(ret.Backend);
    } else {
      addEvaluation(ret.Backend);
      endFnBody(NULL);
    }
  }

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

static struct FType resultTypeOfArmExpr(struct FType a, struct FType b,
                                        struct LE *l) {
  struct FType r;
  if (a.Type != ttInt && a.Type != ttFloat && a.Type != ttPointer) {
    compileError(*l,
                 "mathematical operation with something other than numbers");
  }
  if (b.Type != ttInt && b.Type != ttFloat) {
    compileError(*l,
                 "mathematical operation with something other than numbers");
  }
  if (b.Type == ttPointer) {
    compileError(*l, "pointer must be on the left hand side");
  }
  if ((a.Type == ttFloat || b.Type == ttFloat) &&
      (a.Type == ttPointer || b.Type == ttPointer)) {
    compileError(*l, "cannot mix calculations with floats and pointers");
  }
  if (a.Type == ttPointer && a.Data.Ptr.Type != ptRaw) {
    compileError(*l, "pointer arithmetic with non-raw pointer");
  }
  r.Type = a.Type == ttFloat || b.Type == ttFloat ? ttFloat : ttInt;
  r.Type = a.Type == ttPointer ? ttPointer : r.Type;
  r.Flags = 0;
  r.AliasUsed = NULL;
  switch (r.Type) {
  case ttInt:
    r.Data.Int.Flags =
        (a.Data.Int.Flags & ifSigned) || (b.Data.Int.Flags & ifSigned)
            ? ifSigned
            : 0;
    r.Data.Int.IntSize = a.Data.Int.IntSize > b.Data.Int.IntSize
                             ? a.Data.Int.IntSize
                             : b.Data.Int.IntSize;
    r.Backend = intType(r.Data.Int.Flags, r.Data.Int.IntSize);
    break;
  case ttFloat:
    r.Data.FloatSize = 8;
    if (a.Type == ttFloat && b.Type == ttFloat && a.Data.FloatSize == 4 &&
        b.Data.FloatSize == 4) {
      r.Data.FloatSize = 4;
    }
    r.Backend = floatType(r.Data.FloatSize);
    break;
  case ttPointer:
    r.Data.Ptr = a.Data.Ptr;
    r.Backend = a.Backend;
    break;
  }
  return r;
}

static struct FExpr parseArm(struct LE *li, const char *op) {
  struct FExpr e, left, right;
  struct LE *l;
  left = parse(li, lvFun);
  right = parse(li->N, lvFun);
  e.Type = resultTypeOfArmExpr(left.Type, right.Type, li);
  if (e.Type.Type == ttInt) {
    e.Backend =
        arithmeticOp(op, left.Backend, right.Backend, e.Type.Data.Int.Flags,
                     e.Type.Data.Int.IntSize, 0); /* TODO: error */
  } else if (e.Type.Type == ttPointer) {
    e.Backend = arithmeticOp(op, left.Backend, right.Backend, 0, 0,
                             1); /* TODO: error */
  } else if (e.Type.Type == ttFloat) {
    e.Backend =
        arithmeticFPOp(op, left.Backend, right.Backend, e.Type.Data.FloatSize);
  }
  for (l = li->N->N; l; l = l->N) {
    right = parse(l, lvFun);
    if (right.Type.Type != ttInt && right.Type.Type != ttFloat) {
      compileError(*li->N,
                   "mathematical operation with something other than numbers");
    }
    e.Type = resultTypeOfArmExpr(e.Type, right.Type, l);
    if (e.Type.Type == ttInt) {
      e.Backend =
          arithmeticOp(op, e.Backend, right.Backend, e.Type.Data.Int.Flags,
                       e.Type.Data.Int.IntSize, 0);
    } else if (e.Type.Type == ttPointer) {
      e.Backend = arithmeticOp(op, e.Backend, right.Backend, 0, 0, 1);
    } else if (e.Type.Type == ttFloat) {
      e.Backend =
          arithmeticFPOp(op, e.Backend, right.Backend, e.Type.Data.FloatSize);
    }
  }
  return e;
}

static struct FExpr parseCmp(const char *op, struct FExpr a, struct FExpr b,
                             struct LE *l) {
  struct FExpr r;
  r.Type.Type = ttInt;
  r.Type.AliasUsed = NULL;
  r.Type.Flags = 0;
  r.Type.Backend = intType(1, 1);
  r.Type.Data.Int.IntSize = 1;
  r.Type.Data.Int.Flags = ifSigned;
  if ((a.Type.Type == ttInt || a.Type.Type == ttFloat ||
       a.Type.Type == ttPointer) &&
      (b.Type.Type == ttInt || b.Type.Type == ttFloat ||
       b.Type.Type == ttPointer)) {
    r.Backend = arithmeticOp(op, a.Backend, b.Backend, 1, 1, 0);
  } else if (a.Type.Type == ttStruct && b.Type.Type == ttStruct &&
             a.Type.Data.Struct.S == b.Type.Data.Struct.S) {
    /* struct comparison */
    struct BExpr *last;
    struct FStruct *st;
    struct BTemporary *tmp1, *tmp2;
    unsigned i;
    last = NULL;
    st = a.Type.Data.Struct.S;
    /* TODO: maybe use a pointer instead of copying the struct? */
    tmp1 = addTemporary(a.Backend, a.Type.Backend);
    tmp2 = addTemporary(b.Backend, b.Type.Backend);
    for (i = 0; i < a.Type.Data.Struct.S->NMembers; ++i) {
      struct BExpr *e;
      e = arithmeticOp(
          op,
          structMemb(tmpInstance(tmp1), st->Backend, st->Members[i].Backend),
          structMemb(tmpInstance(tmp2), st->Backend, st->Members[i].Backend), 1,
          1, 0);
      /* TODO: OTHER COMPARISONS THAN arithmeticOp!!! */
      if (last) {
        last = arithmeticOp("&&", last, e, 1, 1, 0);
      } else {
        last = e;
      }
    }
    r.Backend = last ? last : castInt(intLiteral(1), r.Type.Backend, 1);
  } else {
    compileError(*l, "don't know how to compare %s and %s", printType(a.Type),
                 printType(b.Type));
  }
  return r;
}

int typeWorksAsCondition(struct FType t) { return t.Type == ttInt; }

static struct FExpr parseIf(struct LE *li, int lvl) {
  /* TODO: errors */
  struct FScope newscope;
  struct FExpr e;
  struct FExpr cond, ifpart, elsepart;
  newscope.Vars = NULL;
  newscope.Funs = NULL;
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
  newscope.Funs = NULL;
  curscope = &newscope;
  beginWhileLoopCond();
  cond = parse(li, lvl);
  if (!typeWorksAsCondition(cond.Type)) {
    compileError(*li, "ill typed condition");
  }
  beginWhileLoopBody(cond.Backend);
  addEvaluation(parse(li->N, lvl | lvLoop).Backend);
  endWhileLoop();
  curscope = curscope->Parent;
}

struct FFunction *findFunctionOverload(struct FFnAliasEntry *e,
                                       struct FExpr *args, unsigned argsLen,
                                       int givehints, struct LE *li) {
  for (; e; e = e->N) {
    unsigned i;
    int success;
    if (e->F->NParms != argsLen) {
      if (givehints) {
        compileHint(*li,
                    "function \"%s\" does not work: wrong number of arguments "
                    "(%i expected, %i seen)",
                    e->F->Name, e->F->NParms, argsLen);
      }
      continue;
    }
    /* go through all arguments */
    success = 1;
    for (i = 0; i < e->F->NParms; ++i) {
      if (tryConvertType(args[i], e->F->Parms[i].Type, NULL)) {
        success = 0;
        if (givehints) {
          compileHint(*li, "function \"%s\" does not work: argument %i has an "
                           "incompatible type (%s expected, %s seen)",
                      e->F->Name, i + 1, printType(e->F->Parms[i].Type),
                      printType(args[i].Type));
        }
        continue;
      }
    }
    if (!success) {
      continue;
    }
    assert(!givehints);
    return e->F;
  }
  return NULL;
}

struct FFunction *findFunction(const char *name, unsigned long hash,
                               struct FScope *scope, struct FExpr *args,
                               unsigned argsLen, struct LE *li) {
  struct FScope *sc;
  struct FFunction *fn;
  struct FFnAlias *al;
  for (sc = scope; sc; sc = sc->Parent) {
    struct FLocalFun *fn;
    for (fn = sc->Funs; fn; fn = fn->Last) {
      if (fn->Fn->HashedName == hash && strcmp(fn->Fn->Name, name) == 0) {
        if (argsLen < fn->Fn->NParms) {
          compileError(*li, "too few arguments for local function \"%s\"",
                       name);
        }
        if (argsLen > fn->Fn->NParms) {
          compileError(*li, "too many arguments for local function \"%s\"",
                       name);
        }
        return fn->Fn;
      }
    }
  }
  for (fn = functions; fn; fn = fn->Last) {
    if (fn->HashedName == hash && strcmp(fn->Name, name) == 0) {
      if (argsLen < fn->NParms) {
        compileError(*li, "too few arguments for function \"%s\"", name);
      }
      if (argsLen > fn->NParms) {
        compileError(*li, "too many arguments for function \"%s\"", name);
      }
      return fn;
    }
  }
  for (al = fnaliases; al; al = al->Last) {
    if (al->HashedName == hash && strcmp(al->Name, name) == 0) {
      struct FFunction *r;
      r = findFunctionOverload(al->Entry, args, argsLen, 0, li);
      if (!r) {
        compileHint(*li, "none of the overloads viable for function \"%s\"",
                    name);
        findFunctionOverload(al->Entry, args, argsLen, 1, li);
        compileError(*li, "overloaded function calls not viable for function "
                          "\"%s\" (see above)",
                     name);
      }
      return r;
    }
  }
  return NULL;
}

struct FFunction *findSimpleFunction(const char *name, unsigned long hash,
                                     struct FScope *scope, struct LE *li) {
  struct FScope *sc;
  struct FFunction *fn;
  (void)li;
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
  struct FExpr *argsparsed;
  unsigned i, argsLen;
  argsLen = countLen(li->N);
  argsparsed = alloca(sizeof(struct FExpr) * argsLen);
  for (l = li->N, i = 0; l; l = l->N, ++i) {
    assert(i < argsLen);
    argsparsed[i] = parse(l, lvFun);
  }
  fn = findFunction(li->V.S, hash, scope, argsparsed, argsLen, li);
  if (!fn) {
    return parse(tryCallAsMacro(li->V.S, li->N), lvFun);
  }
  c = beginFuncall(fn->Backend);
  for (l = li->N, i = 0; l; l = l->N, ++i) {
    f = convertType(argsparsed[i], fn->Parms[i].Type, l);
    addArg(c, f.Backend);
  }
  r.Backend = endFuncall(c);
  r.Type = fn->RetType;
  return r;
}

static void parseAlias(struct LE *li) {
  unsigned long hash;
  hash = hashName(li->V.S);
  goto fnAlias; /* try function alias first */
fnAlias : {
  struct FFunction *fn, *f;
  struct FFnAlias *a, *alias;
  struct FFnAliasEntry *entry;
  unsigned long rhash;
  rhash = hashName(li->N->V.S);
  f = NULL;
  for (fn = functions; fn; fn = fn->Last) {
    if (fn->HashedName == rhash && strcmp(fn->Name, li->N->V.S) == 0) {
      f = fn;
    }
  }
  if (!f) {
    goto typeAlias;
  }
  alias = NULL;
  for (a = fnaliases; a; a = a->Last) {
    if (a->HashedName == hash && strcmp(a->Name, li->V.S) == 0) {
      alias = a;
    }
  }
  entry = getMem(sizeof(struct FFnAliasEntry));
  if (!alias) {
    alias = getMem(sizeof(struct FFnAlias));
    alias->HashedName = hash;
    alias->Name = li->V.S;
    alias->Last = fnaliases;
    fnaliases = alias;
  }
  entry->N = alias->Entry;
  alias->Entry = entry;
  entry->F = f;
  return;
}
typeAlias : {
  struct FTypeAlias *b;
  struct FOpaque *oq;
  b = getMem(sizeof(struct FTypeAlias));
  b->HashedName = hash;
  b->Name = li->V.S;
  b->T = parseType(li->N, 0);
  b->Last = typealiases;
  typealiases = b;
  for (oq = opaques; oq; oq = oq->Last) {
    if (oq->HashedName == b->HashedName && strcmp(oq->Name, b->Name) == 0) {
      if (oq->T) {
        compileError(*li, "opaque type \"%s\" has already been cleared",
                     b->Name);
        /* TODO: Language? "been cleared"? */
      }
      oq->T = getMem(sizeof(struct FType));
      *oq->T = b->T;
      oq->T->AliasUsed = b;
      break;
    }
  }
}
}

static void parseStruct(struct LE *li) {
  struct FStruct *st;
  struct LE *l, *list;
  struct FOpaque *oq;
  unsigned i;
  st = getMem(sizeof(struct FStruct));
  st->Name = li->V.S;
  st->HashedName = hashName(li->V.S);
  st->Backend = beginStruct(li->V.S);
  for (l = li->N->V.L; l; l = l->N) {
    for (list = l->V.L->N; list; list = list->N) {
      st->Members =
          moreMem(st->Members, sizeof(struct FStructMember) * st->NMembers,
                  sizeof(struct FStructMember));
      st->Members[st->NMembers].Name = list->V.S;
      st->Members[st->NMembers].HashedName = hashName(list->V.S);
      st->Members[st->NMembers].L = l->V.L;
      st->NMembers++;
    }
  }
  for (i = 0; i < st->NMembers; ++i) {
    st->Members[i].Type = parseType(st->Members[i].L, 0);
  }
  for (i = 0; i < st->NMembers; ++i) {
    st->Members[i].Backend = addToStruct(st->Backend, st->Members[i].Name,
                                         st->Members[i].Type.Backend);
  }
  endStruct(st->Backend);
  st->Last = structs;
  structs = st;
  for (oq = opaques; oq; oq = oq->Last) {
    if (oq->HashedName == st->HashedName && strcmp(oq->Name, st->Name) == 0) {
      if (oq->T) {
        compileError(*li, "opaque type \"%s\" has already been cleared",
                     oq->Name);
        /* TODO: Language? "been cleared"? */
      }
      oq->T = getMem(sizeof(struct FType));
      oq->T->Type = ttStruct;
      oq->T->Backend = structType(st->Backend);
      oq->T->Data.Struct.S = st;
      break;
    }
  }
}
static struct FExpr parseMemb(struct LE *li, struct FExpr st, struct LE *stli,
                              int lvl) {
  /* TODO: error */
  struct FStructMember *smemb, *endmemb;
  unsigned long hash;
  struct FExpr r;
  struct FMemberMacro *mac;
  if (st.Type.Type != ttStruct) {
    compileError(*li, "member access of non-struct");
  }
  hash = hashName(li->N->V.S);
  endmemb = st.Type.Data.Struct.S->Members + st.Type.Data.Struct.S->NMembers;
  for (smemb = st.Type.Data.Struct.S->Members; smemb != endmemb; ++smemb) {
    if (smemb->HashedName == hash && strcmp(smemb->Name, li->N->V.S) == 0) {
      r.Backend = structMemb(st.Backend, st.Type.Data.Struct.S->Backend,
                             smemb->Backend);
      r.Type = smemb->Type;
      return r;
    }
  }
  for (mac = st.Type.Data.Struct.S->Macros; mac; mac = mac->Last) {
    if (mac->HashedName == hash && strcmp(mac->Name, li->N->V.S) == 0) {
      return parse(callMemberMacro(mac->Middleend, stli, li->N->V.S, li), lvl);
    }
  }
  compileError(*li->N, "member not found (in %s): \"%s\"", printType(st.Type),
               li->N->V.S);
  r.Type.Type = ttVoid;
  return r; /* never reached; used to silence compiler warning */
}

static struct FExpr handleRef(struct FExpr e) {
  if (e.Type.Type == ttPointer && e.Type.Data.Ptr.Type == ptRef) {
    e.Backend = derefPtr(e.Backend, e.Type.Data.Ptr.Flags & pfVolatile);
    e.Type = *e.Type.Data.Ptr.Pointee;
  } else if (e.Type.Type == ttArray) {
    struct FType t;
    t.Flags = 0;
    t.AliasUsed = NULL;
    t.Type = ttPointer;
    t.Data.Ptr.Type = ptRaw;
    t.Data.Ptr.Pointee = e.Type.Data.Array.Pointee;
    t.Backend = ptrType(t.Data.Ptr.Pointee->Backend,
                        0 /* TODO: correctly handle volatile arrays */);
    /* TODO: prevent user from setting that */
    return makeExpr(pointerToArray(e.Backend), t);
  }
  return e;
}

static struct FExpr parseFunPtrCall(struct FExpr fun, struct LE *li) {
  struct LE *l;
  struct BIncompleteFuncall *fnc;
  int i;
  if (fun.Type.Type != ttFunPointer) {
    compileError(*li, "expected function pointer");
  }
  fnc = beginFunPtrCall(fun.Backend);
  i = 0;
  for (l = li->N; l; l = l->N) {
    if (i == fun.Type.Data.FunPtr.NParms) {
      compileError(*li, "this function pointer requires %i arguments, found %i",
                   fun.Type.Data.FunPtr.NParms, i + 1);
    }
    addArg(
        fnc,
        convertType(parse(l, lvFun), fun.Type.Data.FunPtr.Parms[i], l).Backend);
    ++i;
  }
  if (i != fun.Type.Data.FunPtr.NParms) {
    compileError(*li, "this function pointer requires %i arguments, found %i",
                 fun.Type.Data.FunPtr.NParms, i);
  }
  return makeExpr(endFuncall(fnc), *fun.Type.Data.FunPtr.RetType);
}

static void parseGlobal(struct LE *l) {
  /* TODO: error */
  int flags;
  struct FGlobal *gl;
  flags = 0;
  if (l->T == tyList) {
    struct LE *li;
    for (li = l->V.L; li; li = li->N) {
      if (strcmp(li->V.S, "extern") == 0) {
        flags |= gfExtern;
      } else if (strcmp(li->V.S, "static") == 0) {
        flags |= gfStatic;
      } else if (strcmp(li->V.S, "volatile") == 0) {
        flags |= gfVolatile;
      }
    }
    l = l->N;
  }
  gl = getMem(sizeof(struct FGlobal));
  gl->Last = globals;
  gl->HashedName = hashName(l->V.S);
  gl->Name = l->V.S;
  gl->Type = parseType(l->N, 0);
  gl->Flags = flags;
  gl->Backend = addGlobal(gl->Name, gl->Type.Backend, gl->Flags);
  globals = gl;
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
    t.Data.Int.Flags = ifSigned;
    t.Backend = intType(ifSigned, 4);
    return makeExpr(intLiteral(l->V.I), t);
  }
  case tyFloat: {
    struct FType t;
    t.Type = ttFloat;
    t.AliasUsed = NULL;
    t.Flags = 0;
    t.Data.FloatSize = 8;
    t.Backend = floatType(8);
    return makeExpr(floatLiteral(l->V.F), t);
  }
  case tyString: {
    struct FType t;
    t.Type = ttPointer;
    t.AliasUsed = NULL;
    t.Flags = 0;
    t.Data.Ptr.Type = ptRaw;
    t.Data.Ptr.Pointee = getMem(sizeof(struct FType));
    t.Data.Ptr.Pointee->Type = ttInt;
    t.Data.Ptr.Pointee->AliasUsed = NULL;
    t.Data.Ptr.Pointee->Flags = tfConst;
    t.Data.Ptr.Pointee->Data.Int.IntSize = 1;
    t.Data.Ptr.Pointee->Data.Int.Flags = ifChar;
    return makeExpr(stringLiteral(l->V.S), t);
  }
  case tyIdent: {
    unsigned i;
    unsigned long hash;
    struct FVar *v;
    struct FScope *s;
    struct FGlobal *gl;
    struct FFunction *fn;
    struct BType **parms;
    struct FType t;
    hash = hashName(l->V.S);
    for (s = curscope; s; s = s->Parent) {
      for (v = s->Vars; v; v = v->Next) {
        if (v->HashedName == hash && strcmp(v->Name, l->V.S) == 0) {
          return handleRef(
              makeExpr(varUsage(v->Backend, v->Flags & vfVolatile), v->Type));
        }
      }
    }
    for (i = 0; i < currentffunction->NParms; ++i) {
      if (currentffunction->Parms[i].HashedName == hash &&
          strcmp(currentffunction->Parms[i].Name, l->V.S) == 0) {
        return handleRef(
            makeExpr(varUsage(currentffunction->Parms[i].Backend, 0),
                     currentffunction->Parms[i].Type));
      }
    }
    for (gl = globals; gl; gl = gl->Last) {
      if (gl->HashedName == hash && strcmp(gl->Name, l->V.S) == 0) {
        return handleRef(
            makeExpr(varUsage(gl->Backend, gl->Flags & gfVolatile), gl->Type));
      }
    }
    fn = findSimpleFunction(l->V.S, hashName(l->V.S), curscope, l);
    if (!fn) {
      compileError(*l, "unknown identifier: \"%s\"", l->V.S);
    }
    parms = alloca(sizeof(struct BType *) * fn->NParms);
    t.Type = ttFunPointer;
    t.Flags = 0;
    t.AliasUsed = NULL;
    t.Data.FunPtr.Parms = getMem(sizeof(struct FType) * fn->NParms);
    t.Data.FunPtr.NParms = fn->NParms;
    t.Data.FunPtr.RetType = &fn->RetType;
    for (i = 0; i < fn->NParms; ++i) {
      t.Data.FunPtr.Parms[i] = fn->Parms[i].Type;
      parms[i] = fn->Parms[i].Type.Backend;
    }
    t.Backend = fnPtrType(fn->RetType.Backend, fn->NParms, parms);
    return makeExpr(fnRefof(fn->Backend), t);
  }
  case tyList: {
    if (!l->V.L) {
      return voidExpr();
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
      case bcStatic:
        return parse(evalList(li->N), lvl);
      case bcStaticRun:
        evalList(li->N);
        break;
      case bcDefun:
        parseDefun(li->N, curscope, lvl, 0);
        break;
      case bcFunProto:
        parseDefun(li->N, curscope, lvl, 1);
        break;
      case bcCast:
        /* TODO: error */
        return parseExplicitCast(parse(li->N->N, lvl), parseType(li->N, 0), li);
      case bcTODO_Set: {
        struct FExpr lhs, rhs;
        lhs = parse(li->N, lvl);
        rhs = parse(li->N->N, lvl);
        rhs = convertType(rhs, lhs.Type, li->N->N);
        if (lhs.Type.Flags & tfConst) {
          compileError(*li->N, "const variables cannot be assigned to");
        }
        return makeExpr(setVar(lhs.Backend, rhs.Backend), lhs.Type);
      }
      case bcTODO_Var: {
        struct FVar *last;
        int flags;
        if (li->N->N->T == tyList) {
          /* TODO: this is a syntactic irregularity */
          struct LE *l;
          for (l = li->N->N->V.L; l; l = l->N) {
            if (strcmp("volatile", l->V.S) == 0) {
              flags = vfVolatile;
            }
          }
          li->N->N = li->N->N->N;
        }
        last = curscope->Vars;
        curscope->Vars = getMem(sizeof(struct FVar));
        curscope->Vars->Next = last;
        curscope->Vars->Type = parseType(li->N, 0);
        curscope->Vars->Name = li->N->N->V.S;
        curscope->Vars->HashedName = hashName(li->N->N->V.S);
        curscope->Vars->Flags = flags;
        curscope->Vars->Backend = addVariable(
            li->N->N->V.S, curscope->Vars->Type.Backend, flags & vfVolatile);
        if (curscope->Vars->Type.Type == ttPointer &&
            curscope->Vars->Type.Data.Ptr.Type == ptRef) {
          /* TODO: PROPER TYPE CHECKING; SOLVE THIS ENTIRE THING BETTER!!! */
          struct FType t;
          t = curscope->Vars->Type;
          t.Data.Ptr.Type = ptRaw;
          addEvaluation(setVar(
              varUsage(curscope->Vars->Backend, flags & vfVolatile),
              convertType(parse(li->N->N->N, lvl), t, li->N->N->N).Backend));
          return makeExpr(
              derefPtr(varUsage(curscope->Vars->Backend, flags & vfVolatile),
                       curscope->Vars->Type.Data.Ptr.Flags & pfVolatile),
              *curscope->Vars->Type.Data.Ptr.Pointee);
        }
        if (li->N->N->N) {
          /* for constant variables */
          addEvaluation(
              setVar(varUsage(curscope->Vars->Backend, flags & vfVolatile),
                     convertType(parse(li->N->N->N, lvl), curscope->Vars->Type,
                                 li->N->N->N)
                         .Backend));
        }
        return makeExpr(varUsage(curscope->Vars->Backend, flags & vfVolatile),
                        curscope->Vars->Type);
      }
      case bcArm:
        /* TODO: check type */
        return parseArm(li->N, li->V.S);
      case bcCmp:
        /* TODO: error */
        return parseCmp(li->V.S, parse(li->N, lvl), parse(li->N->N, lvl), li);
      case bcUnary: {
        struct FExpr a;
        a = parse(li->N, lvl);
        /* TODO: check type; POINTERS!!! */
        return makeExpr(unaryOp(*li->V.S, a.Backend, a.Type.Data.Int.Flags,
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
      case bcStruct:
        parseStruct(li->N);
        break;
      case bcGlobal:
        parseGlobal(li->N);
        break;
      case bcPtrRefof: {
        struct FExpr a;
        struct FType *t, ty;
        a = parse(li->N, lvl);
        t = getMem(sizeof(struct FType));
        *t = a.Type;
        ty.Type = ttPointer;
        ty.Flags = 0;
        ty.Data.Ptr.Type = ptRaw;
        ty.Data.Ptr.Pointee = t;
        ty.AliasUsed = NULL;
        return makeExpr(refof(a.Backend), ty);
      }
      case bcPtrDeref: {
        struct FExpr a;
        a = parse(li->N, lvl);
        if (a.Type.Type != ttPointer) {
          compileError(*li->N, "\"ptr-deref\" used on non-pointer");
        }
        if (a.Type.Data.Ptr.Type != ptRaw) {
          compileError(*li->N, "cannot \"ptr-deref\" non-raw pointer");
        }
        if (a.Type.Data.Ptr.Pointee &&
            a.Type.Data.Ptr.Pointee->Type == ttOpaque) {
          struct FOpaque *oq;
          oq = a.Type.Data.Ptr.Pointee->Data.Opaque.O;
          if (!oq->T) {
            compileError(*li->N, "cannot \"ptr-deref\" opaque pointer \"%s\" "
                                 "because it is unknown",
                         oq->Name);
          }
          a.Type = *oq->T;
          a.Backend = derefPtr(
              castPtr(a.Backend, ptrType(oq->T->Backend,
                                         a.Type.Data.Ptr.Flags & pfVolatile)),
              a.Type.Data.Ptr.Flags & pfVolatile);
        } else {
          a.Backend = derefPtr(a.Backend, a.Type.Data.Ptr.Flags & pfVolatile);
          a.Type = *a.Type.Data.Ptr.Pointee;
        }
        return a;
      }
      case bcMemb:
        return parseMemb(li->N, parse(li->N, lvl), li->N, lvl);
      case bcFuncall:
        return parseFunPtrCall(parse(li->N, lvl), li->N);
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
  memset(&l, 0, sizeof(struct LE));
  l.T = tyList;
  l.V.L = list;
  parse(&l, lvTop);
}

/* introspection */
struct IStruct *tryLookupStruct(const char *name) {
  struct FStruct *s;
  unsigned long hash;
  hash = hashName(name);
  for (s = structs; s; s = s->Last) {
    if (s->HashedName == hash && strcmp(s->Name, name) == 0) {
      return (struct IStruct *)s;
    }
  }
  return NULL;
}
const char *getStructName(struct IStruct *s) {
  return ((struct FStruct *)s)->Name;
}
struct IStructMemberIt {
  struct FStructMember *A;
  unsigned long MembersLeft;
};
struct IStructMemberIt *getStructMembers(struct IStruct *s) {
  struct IStructMemberIt *r;
  if (!s) {
    return NULL;
  }
  if (!((struct FStruct *)s)->NMembers) {
    return NULL;
  }
  r = getMem(sizeof(struct IStructMemberIt));
  r->A = ((struct FStruct *)s)->Members;
  r->MembersLeft = ((struct FStruct *)s)->NMembers - 1;
  return r;
}
struct IStructMemberIt *nextStructMember(struct IStructMemberIt *it) {
  /* we invalidate the iterator in our argument */
  if (!it) {
    return NULL;
  }
  if (!it->MembersLeft) {
    return NULL;
  }
  it->MembersLeft--;
  ++it->A;
  return it;
}
const char *getStructMemberName(struct IStructMemberIt *it) {
  return it->A->Name;
}
struct IType *getStructMemberType(struct IStructMemberIt *it) {
  return (struct IType *)&it->A->Type;
}
void addMemberMacro(struct IStruct *s, const char *name,
                    struct MMemberMacro *m) {
  struct FMemberMacro *f;
  f = getMem(sizeof(struct FMemberMacro));
  f->HashedName = hashName(name);
  f->Name = name;
  f->Middleend = m;
  f->Last = ((struct FStruct *)s)->Macros;
  ((struct FStruct *)s)->Macros = f;
}
