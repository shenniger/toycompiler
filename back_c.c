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
}
void finalizeCodegen() { puts("/* EOF */"); }

static int uniqueIntAt;
static int uniqueInt() { return uniqueIntAt++; }

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
struct BExpr *arithmeticOp(char op, struct BExpr *a, struct BExpr *b) {
  int u;
  struct BExpr *r;
  if (op == '+' || op == '-' || op == '*') {
    u = uniqueInt();
    r = getMem(sizeof(struct BExpr));
    r->Var = b->Var;
    appendStringList(&r->Var, a->Var);
    r->Before = b->Before;
    appendStringList(&r->Before, a->Before);
    appendString(&r->Var, printToMem("  int temporary__%i;", u));
    appendString(&r->Before,
                 printToMem("  if(__builtin_%s_overflow(%s, %s, "
                            "&temporary__%i)) { /* TODO */ }",
                            op == '+' ? "add" : op == '-' ? "sub" : "mul", a->A,
                            b->A, u));
    r->A = printToMem("temporary__%i", u);
  }
  return r;
}
struct BExpr *stringLiteral(const char *val) {
  return makeSimpleExpr(printToMem("\"%s\"", val), NULLSTR, NULLSTR,
                        efNonSelfSufficient);
}

struct BVar {
  const char *Name;
};

struct BParm {
  struct BVar V;
};

struct BScope {
  struct StringList *Var;
  struct StringList *Body;
  struct BScope *Parent;
};
struct BFunction {
  const char *Name;
  struct BParm *Parms;
  long NParms;
  struct BFunction *Parent;
  struct BScope Scope;
};
struct BFunction *curfn;
struct BScope *curscope;

struct BScope *beginScope() {
  struct BScope *cur;
  cur = curscope;
  curscope = getMem(sizeof(struct BScope));
  curscope->Parent = cur;
  appendString(&curscope->Var, "{");
  return curscope; /* never used */
}
struct BExpr *endScope(struct BScope *scope, struct BExpr *e) {
  unsigned u;
  struct BExpr *expr;
  (void)scope;
  u = uniqueInt();
  expr = getMem(sizeof(struct BExpr));
  appendString(&expr->Var, printToMem("int tmp_scoped_%i;", u));
  if (e) {
    appendStringList(&curscope->Var, e->Var);
    appendStringList(&curscope->Body, e->Before);
    appendString(&curscope->Body, printToMem("tmp_scoped_%i = %s;", u, e->A));
  }
  appendString(&curscope->Body, "}");

  appendStringList(&curscope->Var, curscope->Body);
  expr->Before = curscope->Var;
  expr->A = printToMem("tmp_scoped_%i", u);
  expr->Flags = efNonSelfSufficient;
  curscope = curscope->Parent;
  return expr;
}

void beginFnPrototype(const char *name) {
  struct BFunction *last;
  last = curfn;
  curfn = getMem(sizeof(struct BFunction));
  curfn->Name = name;
  curfn->Parent = last;
}
/* prints prototype; DOES NOT PRINT ');' or ') {'!!! */
static void printProto(struct BFunction *f) {
  unsigned i;
  printf("int %s(", curfn->Name);
  for (i = 0; i < f->NParms; ++i) {
    if (i) {
      fputs(", ", stdout);
    }
    printf("int %s", f->Parms[i].V.Name);
  }
}
struct BFunction *endFnPrototype(int addBody) {
  if (!addBody) {
    printProto(curfn);
    puts(");");
    curfn = curfn->Parent;
  } else {
    curfn->Scope.Parent = curscope;
    curscope = &curfn->Scope;
  }
  return curfn;
}
void endFnBody(struct BExpr *e) {
  printProto(curfn);
  puts(") {");
  printAll(curfn->Scope.Var);
  printAll(e->Var);
  printAll(curfn->Scope.Body);
  printAll(e->Before);
  printf("  return %s;\n", e->A);
  puts("}");
  curfn = curfn->Parent;
  curscope = curscope->Parent;
}

struct BVar *addVariable(const char *name) {
  struct BVar *a;
  a = getMem(sizeof(struct BVar));
  a->Name = name;
  appendString(&curscope->Var, printToMem("int %s;", name));
  return a;
}
struct BVar *addParameter(const char *name) {
  curfn->Parms = moreMem(curfn->Parms, sizeof(struct BParm) * curfn->NParms,
                         sizeof(struct BParm));
  curfn->Parms[curfn->NParms].V.Name = name;
  return &curfn->Parms[curfn->NParms++].V;
}
struct BExpr *varUsage(struct BVar *p) {
  return makeSimpleExpr(printToMem("%s", p->Name), NULLSTR, NULLSTR,
                        efNonSelfSufficient);
}

struct BExpr *setVar(struct BExpr *lhs, struct BExpr *rhs) {
  appendStringList(&lhs->Var, rhs->Var);
  appendStringList(&lhs->Before, rhs->Before);
  return makeSimpleExpr(printToMem("(%s = %s)", lhs->A, rhs->A),
                        toOneString(lhs->Before), toOneString(lhs->Var), 0);
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

struct BExpr *ifStmt(struct BExpr *cond, struct BExpr *iftrue,
                     struct BExpr *iffalse) {
  struct BExpr *r;
  unsigned u;
  u = uniqInt();
  r = getMem(sizeof(struct BExpr));
  r->Flags = efNonSelfSufficient;
  /* TODO */
  r->A = printToMem("tmp_if_%i", u);
  return r;
}

void TODO_print(struct BExpr *a) {
  appendStringList(&curscope->Var, a->Var);
  appendStringList(&curscope->Body, a->Before);
  appendString(&curscope->Body,
               printToMem("  printf(\"VAL: %%i\\n\", %s);", a->A));
}

void addEvaluation(struct BExpr *a) {
  if (!a) {
    return;
  }

  appendStringList(&curscope->Var, a->Var);
  appendStringList(&curscope->Body, a->Before);
  if (!(a->Flags & efNonSelfSufficient)) {
    appendString(&curscope->Body, printToMem("  %s;", a->A));
  }
}
