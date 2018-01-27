#include "prg.h"

#include <stdio.h>
#include <string.h>

static char nullStr;
#define NULLSTR (&nullStr)

struct StringList {
  const char *S;
  struct StringList *L;
};
struct StringListContainer {
  struct StringList *First;
  struct StringList *Last;
};
static void appendString(struct StringListContainer *sl, const char *s) {
  if (sl->Last) {
    sl->Last->L = getMem(sizeof(struct StringList));
    sl->Last->L->S = s;
    sl->Last = sl->Last->L;
  } else {
    sl->Last = getMem(sizeof(struct StringList));
    sl->Last->S = s;
    sl->First = sl->Last;
  }
}
static void appendStringList(struct StringListContainer *sl,
                             struct StringListContainer l) {
  if (sl->First == l.First || !l.First) {
    return;
  }
  if (sl->Last) {
    sl->Last->L = l.First;
    sl->Last = l.Last;
  } else {
    *sl = l;
  }
}
static void printAll(struct StringListContainer c) {
  struct StringList *sl;
  for (sl = c.First; sl; sl = sl->L) {
    puts(sl->S);
  }
}
static char *toOneString(struct StringListContainer c) {
  struct StringList *s;
  char *r;
  unsigned len, i, l;
  len = 0;
  for (s = c.First; s; s = s->L) {
    len += strlen(s->S);
  }
  r = getMem(len + 1);
  r[len] = 0;
  i = 0;
  for (s = c.First; s; s = s->L) {
    l = strlen(s->S);
    memcpy(r + i, s->S, l);
    i += l;
  }
  return r;
}

void initCodegen() {
  puts("/* A generated C file. */");
  /*puts("#include <stdio.h>");*/
  puts("#include <stdint.h>");
}
void finalizeCodegen() { puts("/* EOF */"); }

static int uniqueIntAt;
static int uniqueInt() { return ++uniqueIntAt; }

enum BExprFlags { efNonSelfSufficient = 0x1 };
struct BExpr {
  const char *A;
  struct StringListContainer Before;
  struct StringListContainer Var;
  int Flags;
};
static struct BExpr *makeSimpleExpr(const char *a, const char *before,
                                    const char *var, int flags) {
  struct BExpr *r;
  r = getMem(sizeof(struct BExpr));
  r->A = a;
  if (before != NULLSTR) {
    r->Before.Last = getMem(sizeof(struct StringList));
    r->Before.Last->S = before;
    r->Before.First = r->Before.Last;
  }
  if (var != NULLSTR) {
    r->Var.Last = getMem(sizeof(struct StringList));
    r->Var.Last->S = var;
    r->Var.First = r->Var.Last;
  }
  r->Flags = flags;
  return r;
}

struct BType {
  const char *A;
};
static struct BType *makeSimpleType(const char *a) {
  struct BType *r;
  r = getMem(sizeof(struct BType));
  r->A = a;
  return r;
}

struct BVar {
  const char *Name;
};

struct BStruct {
  const char *Name;
};

struct BStructMember {
  const char *Name;
};

struct BStruct *beginStruct(const char *name) {
  struct BStruct *r;
  r = getMem(sizeof(struct BStruct));
  r->Name = name;
  printf("struct %s {\n", name);
  return r;
}
struct BStructMember *addToStruct(struct BStruct *st, const char *name,
                                  struct BType *type) {
  struct BStructMember *r;
  (void)st;
  printf("  %s %s;\n", type->A, name);
  r = getMem(sizeof(struct BStructMember));
  r->Name = name;
  return r;
}
void endStruct(struct BStruct *st) { (void)st, puts("};"); }

struct BExpr *structMemb(struct BExpr *st, struct BStruct *type,
                         struct BStructMember *memb) {
  (void)type;
  st->A = printToMem("%s.%s", st->A, memb->Name);
  return st;
}

struct BExpr *intLiteral(int val) {
  return makeSimpleExpr(printToMem("%i", val), NULLSTR, NULLSTR,
                        efNonSelfSufficient);
}
struct BExpr *floatLiteral(double val) {
  return makeSimpleExpr(printToMem("%f", val), NULLSTR, NULLSTR,
                        efNonSelfSufficient);
}
struct BExpr *stringLiteral(const char *val) {
  return makeSimpleExpr(printToMem("\"%s\"", val), NULLSTR, NULLSTR,
                        efNonSelfSufficient);
}

struct BType *voidType() {
  return makeSimpleType("void");
}
struct BType *intType(int flags, int size) { /* size in bytes */
  if (flags & ifChar) {
    return makeSimpleType("char"); /* TODO: other character lengths */
  }
  if (flags & ifCType) {
    const char *const uns = (flags & ifSigned) ? "signed " : "unsigned ";
    switch (size) {
    case 1:
      return makeSimpleType(printToMem("%schar", uns));
    case 2:
      return makeSimpleType(printToMem("%sshort int", uns));
    case 4:
      return makeSimpleType(printToMem("%sint", uns));
    case 8:
      return makeSimpleType(printToMem("%slong long int", uns));
    }
  }
  return makeSimpleType(
      printToMem("%sint%i_t", (flags & ifSigned) ? "" : "u", size * 8));
}
struct BType *floatType(int size) { /* size in bytes */
  return makeSimpleType(size == 8 ? "double" : "float");
}
struct BType *structType(struct BStruct *st) {
  return makeSimpleType(printToMem("struct %s", st->Name));
}
void constType(struct BType *t) {
  (void)t;
  /*t->A = printToMem("const %s", t->A);*/ /* TODO */
}
struct BType *ptrType(struct BType *t, int isvolatile) {
  return makeSimpleType(
      printToMem("%s%s*", t->A, isvolatile ? "volatile " : ""));
}
struct BType *fnPtrType(struct BType *rettype, int nparms,
                        struct BType **parms) {
  /* TODO: this is a bad solution */
  unsigned u;
  int i;
  u = uniqueInt();
  printf("typedef %s (*helper_%i)(", rettype->A, u);
  for (i = 0; i < nparms - 1; ++i) {
    printf("%s, ", parms[i]->A);
  }
  printf("%s);\n", nparms ? parms[nparms - 1]->A : "");
  return makeSimpleType(printToMem("helper_%i", u));
}
struct BType *arrayType(struct BType *t, int size) {
  /* TODO: this is a bad solution */
  unsigned u;
  u = uniqueInt();
  printf("typedef %s helper_%i[%i];\n", t->A, u, size);
  return makeSimpleType(printToMem("helper_%i", u));
}

struct BExpr *pointerToArray(struct BExpr *r) {
  return r;
}

struct BExpr *derefPtr(struct BExpr *e, int isvolatileptr) {
  (void)isvolatileptr;
  e->A = printToMem("*(%s)", e->A);
  return e;
}
struct BExpr *refof(struct BExpr *e) {
  e->A = printToMem("&(%s)", e->A);
  return e;
}

struct BExpr *castInt(struct BExpr *a, struct BType *t, int issigned) {
  (void)issigned;
  a->A = printToMem("((%s)%s)", t->A, a->A);
  return a;
}
struct BExpr *castFloat(struct BExpr *a, struct BType *t) {
  return castInt(a, t, 0); /* same cast in C */
}
struct BExpr *castPtr(struct BExpr *a, struct BType *t) {
  return castInt(a, t, 0); /* same cast in C */
}
struct BExpr *castIntToFloat(struct BExpr *a, struct BType *t, int issigned) {
  (void)issigned;
  return castInt(a, t, 0); /* same cast in C */
}
struct BExpr *castFloatToInt(struct BExpr *a, struct BType *t, int issigned) {
  (void)issigned;
  return castInt(a, t, 0); /* same cast in C */
}
struct BExpr *castIntToPtr(struct BExpr *a, struct BType *t) {
  return castInt(a, t, 0); /* same cast in C */
}
struct BExpr *castPtrToInt(struct BExpr *a, struct BType *t) {
  return castInt(a, t, 0); /* same cast in C */
}

struct BExpr *arithmeticOp(const char *op, struct BExpr *a, struct BExpr *b,
                           int result_flags, int result_size, int ptr) {
  struct BExpr *r;
  r = getMem(sizeof(struct BExpr));
  if (!ptr && (*op == '+' || *op == '-' || *op == '*')) {
    int u;
    u = uniqueInt();
    r->Var = b->Var;
    appendStringList(&r->Var, a->Var);
    r->Before = b->Before;
    appendStringList(&r->Before, a->Before);
    appendString(&r->Var, printToMem(" %s temporary__%i;",
                                     intType(result_flags, result_size)->A, u));
    appendString(&r->Before,
                 printToMem("  if(__builtin_%s_overflow(%s, %s, "
                            "&temporary__%i)) { /* TODO */ }",
                            *op == '+' ? "add" : *op == '-' ? "sub" : "mul",
                            a->A, b->A, u));
    r->A = printToMem("temporary__%i", u);
  } else {
    appendStringList(&a->Var, b->Var);
    appendStringList(&r->Var, a->Var);
    appendStringList(&a->Before, b->Before);
    appendStringList(&r->Before, a->Before);
    r->Flags = efNonSelfSufficient;
    r->A = printToMem("(%s %s %s)", a->A, op, b->A);
  }
  return r;
}
struct BExpr *unaryOp(char op, struct BExpr *a, int result_flags,
                      int result_size) {
  struct BExpr *r;
  (void)result_flags, (void)result_size;
  r = getMem(sizeof(struct BExpr));
  appendStringList(&r->Var, a->Var);
  appendStringList(&r->Before, a->Before);
  r->Flags = efNonSelfSufficient;
  r->A = printToMem("(%c%s)", op, a->A);
  return r;
}
struct BExpr *arithmeticFPOp(const char *op, struct BExpr *a, struct BExpr *b,
                             int floatsize) {
  struct BExpr *r;
  (void)floatsize;
  r = getMem(sizeof(struct BExpr));
  appendStringList(&a->Var, b->Var);
  appendStringList(&r->Var, a->Var);
  appendStringList(&a->Before, b->Before);
  appendStringList(&r->Before, a->Before);
  r->Flags = efNonSelfSufficient;
  r->A = printToMem("(%s %s %s)", a->A, op, b->A);
  return r;
}

struct BParm {
  struct BVar V;
  struct BType *T;
};

struct BFunction {
  const char *Name;
  struct BParm *Parms;
  long NParms;
  struct BFunction *Parent;
  struct StringListContainer Var;
  struct StringListContainer Body;
  struct BType *RetType;
  int Flags;
};
struct BFunction *curfn;

struct BExpr *fnRefof(struct BFunction *f) {
  return makeSimpleExpr(printToMem("&%s", f->Name), NULLSTR, NULLSTR,
                        efNonSelfSufficient);
}

void beginFnPrototype(const char *name, struct BType *rettype, int flags) {
  struct BFunction *last;
  last = curfn;
  curfn = getMem(sizeof(struct BFunction));
  curfn->Name = name;
  curfn->Parent = last;
  curfn->RetType = rettype;
  curfn->Flags = flags;
}
/* prints prototype; DOES NOT PRINT ');' or ') {'!!! */
static void printProto(struct BFunction *f) {
  unsigned i;
  printf("%s%s%s%s %s(", f->Flags & ffStatic ? "static " : NULLSTR,
         f->Flags & ffInline ? "inline " : NULLSTR,
         f->Flags & ffNoReturn ? "noreturn " : "", f->RetType->A, f->Name);
  for (i = 0; i < f->NParms; ++i) {
    if (i) {
      fputs(", ", stdout);
    }
    printf("%s %s", f->Parms[i].T->A, f->Parms[i].V.Name);
  }
}
struct BFunction *endFnPrototype(int addBody) {
  struct BFunction *r;
  r = curfn;
  if (!addBody) {
    printProto(curfn);
    puts(");");
    curfn = curfn->Parent;
  }
  return r;
}
void endFnBody(struct BExpr *e) {
  printProto(curfn);
  puts(") {");
  printAll(curfn->Var);
  if (e)
    printAll(e->Var);
  printAll(curfn->Body);
  if (e)
    printAll(e->Before);
  if (e)
    printf("  return %s;\n", e->A);
  puts("}");
  curfn = curfn->Parent;
}

struct BVar *addVariable(const char *name, struct BType *type, int isvolatile) {
  struct BVar *a;
  a = getMem(sizeof(struct BVar));
  a->Name = name;
  appendString(
      &curfn->Var,
      printToMem("  %s%s %s;", isvolatile ? "volatile " : "", type->A, name));
  return a;
}
struct BVar *addParameter(const char *name, struct BType *type) {
  curfn->Parms = moreMem(curfn->Parms, sizeof(struct BParm) * curfn->NParms,
                         sizeof(struct BParm));
  curfn->Parms[curfn->NParms].V.Name = name;
  curfn->Parms[curfn->NParms].T = type;
  return &curfn->Parms[curfn->NParms++].V;
}
struct BVar *updateParameter(struct BVar *a) {
  return a;
}
struct BVar *addGlobal(const char *name, struct BType *t, int flags) {
  struct BVar *a;
  a = getMem(sizeof(struct BVar));
  a->Name = name;
  printf("%s%s%s %s;\n",
         flags & gfExtern ? "extern " : (flags & gfStatic ? "static " : ""),
         flags & gfVolatile ? "volatile " : "", t->A, name);
  return a;
}
struct BExpr *varUsage(struct BVar *p, int isvolatilevar) {
  (void)isvolatilevar;
  return makeSimpleExpr(p->Name, NULLSTR, NULLSTR, efNonSelfSufficient);
}

struct BExpr *setVar(struct BExpr *lhs, struct BExpr *rhs) {
  appendStringList(&lhs->Var, rhs->Var);
  appendStringList(&lhs->Before, rhs->Before);
  appendString(&lhs->Before, printToMem("  %s = %s;", lhs->A, rhs->A));
  return lhs;
}

struct BTemporary *addTemporary(struct BExpr *e, struct BType *t) {
  unsigned u;
  const char *r;
  u = uniqueInt();
  r = printToMem("tmp_%i", u);
  appendString(&curfn->Var, printToMem("  %s %s;", t->A, r));
  appendStringList(&curfn->Var, e->Var);
  appendStringList(&curfn->Body, e->Before);
  appendString(&curfn->Body, printToMem("  %s = %s;", r, e->A));
  return (struct BTemporary *)r;
}
struct BExpr *tmpInstance(struct BTemporary *tmp) {
  return makeSimpleExpr((const char *)tmp, NULLSTR, NULLSTR, 0);
}

struct BIncompleteFuncall {
  const char *Name;
  struct StringListContainer Args;
  struct StringListContainer Var;
  struct StringListContainer Before;
  int LenArgs;
};

struct BIncompleteFuncall *beginFuncall(struct BFunction *f) {
  struct BIncompleteFuncall *c;
  c = getMem(sizeof(struct BIncompleteFuncall));
  c->Name = f->Name;
  return c;
}
struct BIncompleteFuncall *beginFunPtrCall(struct BExpr *e) {
  struct BIncompleteFuncall *c;
  c = getMem(sizeof(struct BIncompleteFuncall));
  c->Name = printToMem("(%s)", e->A);
  appendStringList(&c->Var, e->Var);
  appendStringList(&c->Before, e->Before);
  return c;
}
void addArg(struct BIncompleteFuncall *c, struct BExpr *val) {
  appendStringList(&c->Var, val->Var);
  appendStringList(&c->Before, val->Before);
  if (c->LenArgs) {
    appendString(&c->Args, printToMem(", %s", val->A));
  } else {
    appendString(&c->Args, val->A);
  }
  ++c->LenArgs;
}
struct BExpr *endFuncall(struct BIncompleteFuncall *c) {
  struct BExpr *r;
  r = getMem(sizeof(struct BExpr));
  r->A = printToMem("%s(%s)", c->Name, toOneString(c->Args));
  r->Var = c->Var;
  r->Before = c->Before;
  r->Flags = 0;
  return r;
}

void beginIfStmt(struct BExpr *cond) {
  appendStringList(&curfn->Var, cond->Var);
  appendStringList(&curfn->Body, cond->Before);
  appendString(&curfn->Body, printToMem("if(%s) {", cond->A));
}
void endIfStmt(struct BExpr *r) {
  addEvaluation(r);
  appendString(&curfn->Body, "}");
}
void beginIfElseStmt(struct BExpr *cond) {
  appendStringList(&curfn->Var, cond->Var);
  appendStringList(&curfn->Body, cond->Before);
  appendString(&curfn->Body, printToMem("if(%s) {", cond->A));
}
void *elseIfStmt(struct BExpr *r, struct BType *rettype) {
  if (r) {
    unsigned u;
    u = uniqueInt();
    appendStringList(&curfn->Var, r->Var);
    appendStringList(&curfn->Body, r->Before);
    appendString(&curfn->Var, printToMem("  %s tmp_if_%i;", rettype->A, u));
    appendString(&curfn->Body,
                 printToMem("  tmp_if_%i = %s;\n} else {", u, r->A));
    return (void *)u;
  } else {
    appendString(&curfn->Body, "  } else {");
    return NULL;
  }
}
struct BExpr *endIfElseStmt(void *last, struct BExpr *r) {
  unsigned u;
  u = (unsigned)last;
  if (u && r) {
    appendStringList(&curfn->Var, r->Var);
    appendStringList(&curfn->Body, r->Before);
    appendString(&curfn->Body, printToMem("  tmp_if_%i = %s;\n}", u, r->A));
    return makeSimpleExpr(printToMem("tmp_if_%i", u), NULLSTR, NULLSTR,
                          efNonSelfSufficient);
  } else {
    appendString(&curfn->Body, "  }");
    return NULL;
  }
}

void beginWhileLoopCond() {}
void beginWhileLoopBody(struct BExpr *cond) {
  /* TODO: cond->Before!!! */
  appendStringList(&curfn->Var, cond->Var);
  appendStringList(&curfn->Body, cond->Before);
  appendString(&curfn->Body, printToMem("while(%s) {", cond->A));
}
void endWhileLoop() { appendString(&curfn->Body, "}"); }

void breakLoop() { appendString(&curfn->Body, "  break;"); }
void continueLoop() { appendString(&curfn->Body, "  continue;"); }

void addEvaluation(struct BExpr *a) {
  if (!a) {
    return;
  }

  appendStringList(&curfn->Var, a->Var);
  appendStringList(&curfn->Body, a->Before);
  if (!(a->Flags & efNonSelfSufficient)) {
    appendString(&curfn->Body, printToMem("  %s;", a->A));
  }
}

struct BExpr *sizeofType(struct BType *t) {
  return makeSimpleExpr(printToMem("sizeof(%s)", t->A), NULLSTR, NULLSTR, 0);
}
