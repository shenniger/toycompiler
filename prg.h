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
enum ParserLevel { lvTop, lvFun };
struct BExpr *parse(struct LE *list, enum ParserLevel lvl);

/* code generator */
void initCodegen();
void finalizeCodegen();

struct BExpr *intLiteral(int val);
struct BExpr *arithmeticOp(char op, struct BExpr *a, struct BExpr *b);
struct BExpr *stringLiteral(const char *val);

void beginFnPrototype(const char *name);
struct BFunction *endFnPrototype(int addBody);
void endFnBody(struct BExpr *e);

struct BVar *addVariable(const char *name);
struct BVar *addParameter(const char *name);
struct BExpr *varUsage(struct BVar *p);

struct BScope *beginScope();
struct BExpr *endScope(struct BScope *scope, struct BExpr *e);

struct BExpr *setVar(struct BExpr *lhs, struct BExpr *rhs);

struct BIncompleteFuncall *beginFuncall(struct BFunction *f);
void addArg(struct BIncompleteFuncall *c, struct BExpr *val);
struct BExpr *endFuncall(struct BIncompleteFuncall *c);

struct BExpr *ifStmt(struct BExpr *cond, struct BExpr *iftrue,
                     struct BExpr *iffalse);

void TODO_print(struct BExpr *a);

void addEvaluation(struct BExpr *a);

#endif
