#ifndef PRG_H
#define PRG_H

void initAlloc();
void *getMem(long s);
void *moreMem(void *before, long beforesize, long addedsize);
char *printToMem(const char *fmt, ...);

/* reader */
enum { tyEmpty, tyInt, tyFloat, tyString, tyIdent, tyList, tyEnd };
union Val {
  long I;
  double F;
  char *S;
  struct LE *L;

  /* middle end */
  void *D;
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
void compileHint(struct LE l, const char *fmt, ...);

void printList(struct LE *l, int depth);
unsigned long hashName(const char *s);
struct LE *copyList(struct LE *l);

/* parser */
void initParser();
void parseSrc(struct LE *list);

/* middle end (ctfe) */
void initEvaluator();
struct LE *evalList(struct LE *li);

/* code generator */
void initCodegen();
void finalizeCodegen();

struct BStruct *beginStruct(const char *name);
struct BType; /* silences compiler warning */
struct BStructMember *addToStruct(struct BStruct *st, const char *name,
                                  struct BType *type);
void endStruct(struct BStruct *st);

struct BExpr *structMemb(struct BExpr *st, struct BStructMember *memb);

struct BType *voidType();
struct BType *intType(int issigned, int size); /* size in bytes */
struct BType *floatType(int size);             /* size in bytes */
struct BType *structType(struct BStruct *st);
struct BType *ptrType(struct BType *t);
struct BType *fnPtrType(struct BType *rettype, int nparms,
                        struct BType **parms);
struct BType *arrayType(struct BType *t, int size);

struct BExpr *pointerToArray(struct BExpr *r);

void constType(struct BType *t);

struct BExpr *derefPtr(struct BExpr *e);
struct BExpr *refof(struct BExpr *e);
struct BFunction; /* silences compiler warning */
struct BExpr *fnRefof(struct BFunction *f);

struct BExpr *intLiteral(int val);
struct BExpr *floatLiteral(double val);
struct BExpr *stringLiteral(const char *val);

struct BExpr *castNum(struct BExpr *a, struct BType *t);
struct BExpr *castPtr(struct BExpr *a, struct BType *t);

struct BExpr *arithmeticOp(const char *op, struct BExpr *a, struct BExpr *b,
                           int result_signed, int result_size, int ptr);
struct BExpr *unaryOp(char op, struct BExpr *a, int result_signed,
                      int result_size);
struct BExpr *arithmeticFPOp(const char *op, struct BExpr *a, struct BExpr *b,
                             int floatsize);

struct BTemporary *addTemporary(struct BExpr *e, struct BType *t);
struct BExpr *tmpInstance(struct BTemporary *tmp);

enum { ffStatic = 0x1, ffInline = 0x2 };
void beginFnPrototype(const char *name, struct BType *rettype, int flags);
struct BFunction *endFnPrototype(int addBody);
void endFnBody(struct BExpr *e);

struct BVar *addVariable(const char *name, struct BType *t);
struct BVar *addParameter(const char *name, struct BType *t);
enum { gfStatic = 0x1, gfExtern = 0x2 };
struct BVar *addGlobal(const char *name, struct BType *t, int flags);

struct BExpr *varUsage(struct BVar *p);
struct BExpr *setVar(struct BExpr *lhs,
                     struct BExpr *rhs); /* TODO: dangerous (see impl) */

struct BIncompleteFuncall *beginFuncall(struct BFunction *f);
struct BIncompleteFuncall *beginFunPtrCall(struct BExpr *e);
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
