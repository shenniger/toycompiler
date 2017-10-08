#ifndef PRG_H
#define PRG_H

void initAlloc();
void *getMem(long s);
void *moreMem(void *before, long beforesize, long addedsize);
char *printToMem(const char *fmt, ...);

/* reader */
enum { tyEmpty, tyInt, tyFloat, tyString, tyIdent, tyList };
union Val {
  long I;
  double F;
  char *S;
  struct LE *L;
};
struct LE {
  union Val V;
  struct LE *N;
  unsigned char T;
  unsigned int CharIdx;
  unsigned short FileIdx;
};
void readList(char *s, struct LE **l, const char *filebegin,
              unsigned short fileidx);

const char *formatSourceLoc(struct LE l);
void compileError(struct LE l, const char *fmt, ...);

/* parser */
void initParser();
void parseSrc(struct LE *list);

/* code generator */
void initCodegen();
void finalizeCodegen();

struct BType *voidType();
struct BType *intType(int issigned, int size); /* size in bytes */

struct BExpr *intLiteral(int val);
struct BExpr *stringLiteral(const char *val);

struct BExpr *castInt(struct BExpr *a, struct BType *t);

struct BExpr *arithmeticOp(const char *op, struct BExpr *a, struct BExpr *b,
                           int result_signed, int result_size);
struct BExpr *unaryOp(char op, struct BExpr *a, int result_signed,
                      int result_size);

void beginFnPrototype(const char *name, struct BType *rettype);
struct BFunction *endFnPrototype(int addBody);
void endFnBody(struct BExpr *e);

struct BVar *addVariable(const char *name, struct BType *t);
struct BVar *addParameter(const char *name, struct BType *t);

struct BExpr *varUsage(struct BVar *p);
struct BExpr *setVar(struct BExpr *lhs,
                     struct BExpr *rhs); /* TODO: dangerous (see impl) */

struct BIncompleteFuncall *beginFuncall(struct BFunction *f);
void addArg(struct BIncompleteFuncall *c, struct BExpr *val);
struct BExpr *endFuncall(struct BIncompleteFuncall *c);

void beginIfStmt(struct BExpr *cond);
void endIfStmt(struct BExpr *r);
void beginIfElseStmt(struct BExpr *cond);
void *elseIfStmt(struct BExpr *r, struct BType *rettype);
struct BExpr *endIfElseStmt(void *last, struct BExpr *r);

void beginWhileLoop(struct BExpr *cond);
void endWhileLoop();
void breakLoop();
void continueLoop();

void TODO_print(struct BExpr *a);

void addEvaluation(struct BExpr *a);

#endif
