#include "prg.h"

#include <stdio.h>
#include <string.h>

static char nullStr;
#define NULLSTR (&nullStr)

struct StringList {
  const char *S;
  struct StringList *L;
};
static void appendString(struct StringList **sl, const char *s) {
  while (*sl) {
    sl = &((*sl)->L);
  }
  *sl = getMem(sizeof(struct StringList));
  (*sl)->S = s;
}
static void appendStringList(struct StringList **sl, struct StringList *l) {
  while (*sl) {
    sl = &((*sl)->L);
  }
  *sl = l;
}
static void printAll(struct StringList *sl) {
  for (; sl; sl = sl->L) {
    if (sl->S != NULLSTR) {
      puts(sl->S);
    }
  }
}
static char *toOneString(struct StringList *sl) {
  struct StringList *s;
  char *r;
  unsigned len, i, l;
  len = 0;
  for (s = sl; s; s = s->L) {
    len += strlen(s->S);
  }
  r = getMem(len + 1);
  r[len] = 0;
  i = 0;
  for (s = sl; s; s = s->L) {
    l = strlen(s->S);
    memcpy(r + i, s->S, l);
    i += l;
  }
  return r;
}

void initCodegen() {
  puts("/* A generated C file. */");
  puts("#include <stdio.h>");
  puts("#include <stdint.h>");
}
void finalizeCodegen() { puts("/* EOF */"); }

static int uniqueIntAt;
static int uniqueInt() { return ++uniqueIntAt; }

struct BType {
  const char *A;
};
static struct BType *makeSimpleType(const char *a) {
  struct BType *r;
  r = getMem(sizeof(struct BType));
  r->A = a;
  return r;
}
struct BType *voidType() {
  return makeSimpleType("void");
}
struct BType *intType(int issigned, int size) { /* size in bytes */
  return makeSimpleType(printToMem("%sint%i_t", issigned ? "" : "u", size * 8));
}

enum BExprFlags { efNonSelfSufficient = 0x1 };
struct BExpr {
  const char *A;
  struct StringList *Before;
  struct StringList *Var;
  int Flags;
};
static struct BExpr *makeSimpleExpr(const char *a, const char *before,
                                    const char *var, int flags) {
  struct BExpr *r;
  r = getMem(sizeof(struct BExpr));
  r->A = a;
  r->Before = getMem(sizeof(struct StringList));
  r->Before->S = before;
  r->Var = getMem(sizeof(struct StringList));
  r->Var->S = var;
  r->Flags = flags;
  return r;
}

struct BExpr *intLiteral(int val) {
  return makeSimpleExpr(printToMem("%i", val), NULLSTR, NULLSTR,
                        efNonSelfSufficient);
}
struct BExpr *stringLiteral(const char *val) {
  return makeSimpleExpr(printToMem("\"%s\"", val), NULLSTR, NULLSTR,
                        efNonSelfSufficient);
}

struct BExpr *castInt(struct BExpr *a, struct BType *t) {
  a->A = printToMem("((%s)%s)", t->A, a->A);
  return a;
}

struct BExpr *arithmeticOp(const char *op, struct BExpr *a, struct BExpr *b,
                           int result_signed, int result_size) {
  struct BExpr *r;
  r = getMem(sizeof(struct BExpr));
  if (*op == '+' || *op == '-' || *op == '*') {
    int u;
    u = uniqueInt();
    r->Var = b->Var;
    appendStringList(&r->Var, a->Var);
    r->Before = b->Before;
    appendStringList(&r->Before, a->Before);
    appendString(&r->Var,
                 printToMem("  %sint%i_t temporary__%i;",
                            result_signed ? "" : "u", result_size * 8, u));
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
    return r;
  }
  return r;
}
struct BExpr *unaryOp(char op, struct BExpr *a, int result_signed,
                      int result_size) {
  struct BExpr *r;
  (void)result_signed, (void)result_size;
  r = getMem(sizeof(struct BExpr));
  appendStringList(&r->Var, a->Var);
  appendStringList(&r->Before, a->Before);
  r->Flags = efNonSelfSufficient;
  r->A = printToMem("(%c%s)", op, a->A);
  return r;
}

struct BVar {
  const char *Name;
};

struct BParm {
  struct BVar V;
  struct BType *T;
};

struct BFunction {
  const char *Name;
  struct BParm *Parms;
  long NParms;
  struct BFunction *Parent;
  struct StringList *Var;
  struct StringList *Body;
  struct BType *RetType;
};
struct BFunction *curfn;

void beginFnPrototype(const char *name, struct BType *rettype) {
  struct BFunction *last;
  last = curfn;
  curfn = getMem(sizeof(struct BFunction));
  curfn->Name = name;
  curfn->Parent = last;
  curfn->RetType = rettype;
}
/* prints prototype; DOES NOT PRINT ');' or ') {'!!! */
static void printProto(struct BFunction *f) {
  unsigned i;
  printf("%s %s(", f->RetType->A, f->Name);
  for (i = 0; i < f->NParms; ++i) {
    if (i) {
      fputs(", ", stdout);
    }
    printf("%s %s", f->Parms[i].T->A, f->Parms[i].V.Name);
  }
}
struct BFunction *endFnPrototype(int addBody) {
  if (!addBody) {
    printProto(curfn);
    puts(");");
    curfn = curfn->Parent;
  }
  return curfn;
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

struct BVar *addVariable(const char *name, struct BType *type) {
  struct BVar *a;
  a = getMem(sizeof(struct BVar));
  a->Name = name;
  appendString(&curfn->Var, printToMem("  %s %s;", type->A, name));
  return a;
}
struct BVar *addParameter(const char *name, struct BType *type) {
  curfn->Parms = moreMem(curfn->Parms, sizeof(struct BParm) * curfn->NParms,
                         sizeof(struct BParm));
  curfn->Parms[curfn->NParms].V.Name = name;
  curfn->Parms[curfn->NParms].T = type;
  return &curfn->Parms[curfn->NParms++].V;
}
struct BExpr *varUsage(struct BVar *p) {
  return makeSimpleExpr(printToMem("%s", p->Name), NULLSTR, NULLSTR,
                        efNonSelfSufficient);
}

struct BExpr *setVar(struct BExpr *lhs, struct BExpr *rhs) {
  appendStringList(&lhs->Var, rhs->Var);
  appendStringList(&lhs->Before, rhs->Before);
  appendString(&lhs->Before, printToMem("  %s = %s;", lhs->A, rhs->A));
  return lhs;
}

struct BIncompleteFuncall {
  struct BFunction *Fn;
  struct StringList *Args;
  struct StringList *Var;
  struct StringList *Before;
  unsigned LenArgs;
};

struct BIncompleteFuncall *beginFuncall(struct BFunction *f) {
  struct BIncompleteFuncall *c;
  c = getMem(sizeof(struct BIncompleteFuncall));
  c->Fn = f;
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
  r->A = printToMem("%s(%s)", c->Fn->Name, toOneString(c->Args));
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

void beginWhileLoop(struct BExpr *cond) {
  appendStringList(&curfn->Var, cond->Var);
  appendStringList(&curfn->Body, cond->Before);
  appendString(&curfn->Body, printToMem("while(%s) {", cond->A));
}
void endWhileLoop() { appendString(&curfn->Body, "}"); }

void breakLoop() { appendString(&curfn->Body, "  break;"); }
void continueLoop() { appendString(&curfn->Body, "  continue;"); }

void TODO_print(struct BExpr *a) {
  appendStringList(&curfn->Var, a->Var);
  appendStringList(&curfn->Body, a->Before);
  appendString(&curfn->Body,
               printToMem("  printf(\"VAL: %%i\\n\", %s);", a->A));
}

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
