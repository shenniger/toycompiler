extern "C" {
/* clang-format off */
#define _Noreturn [[noreturn]] /* pointless C/C++ incompatibility %(/$)(/&$%/ */
/* clang-format on */
/*
 * clang-format needs to be disabled here because it removes the whitespace
 * between macro name and expansion.
 *
 */
#include "prg.h"
}

#include <assert.h>

#include <iostream>
#include <iterator>
#include <map>
#include <memory>
#include <stack>
#include <utility>

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

using namespace llvm;

using InsertPoint = std::pair<llvm::BasicBlock *, llvm::BasicBlock::iterator>;

struct Singleton {
  LLVMContext C;
  IRBuilder<> B;
  std::unique_ptr<Module> M;
  std::map<std::string, Value *> NamedValues;
  std::stack<InsertPoint> InsertPoints;
  Singleton() : B(C) { M = llvm::make_unique<Module>("test", C); }
};

static Singleton *llvmdata;

void initCodegen() { llvmdata = new Singleton; }
void finalizeCodegen() {
  llvmdata->M->print(llvm::outs(), nullptr);
  delete llvmdata;
}

static inline struct BExpr *toExpr(Value *v) { return (struct BExpr *)v; }
static inline Value *fromExpr(struct BExpr *v) { return (Value *)v; }
static inline struct BVar *toVar(Value *v) { return (struct BVar *)v; }
static inline Value *fromVar(struct BVar *v) { return (Value *)v; }
static inline struct BType *toType(Type *v) { return (struct BType *)v; }
static inline Type *fromType(struct BType *v) { return (Type *)v; }
static inline struct BFunction *toFunction(Function *v) {
  return (struct BFunction *)v;
}
static inline Function *fromFunction(struct BFunction *v) {
  return (Function *)v;
}

struct BStruct { /* NOT POD!!! */
  StructType *Type;
  const char *Name;
  std::vector<llvm::Type *> Content;
};

struct BStruct *beginStruct(const char *name) {
  struct BStruct *s = (BStruct *)getMem(sizeof(struct BStruct));
  s->Name = name;
  return s;
}
struct BStructMember *addToStruct(struct BStruct *st, const char *name,
                                  struct BType *type) {
  (void)name; /* in LLVM, struct members are not named */
  st->Content.push_back(fromType(type));
  return (struct BStructMember *)st->Content.size();
}
void endStruct(struct BStruct *st) {
  st->Type = StructType::create(llvmdata->C, st->Content, st->Name);
}

struct BExpr *structMemb(struct BExpr *st, struct BStruct *type,
                         struct BStructMember *memb) {
  unsigned n = (unsigned)(unsigned long long)memb;
  Value *l = fromExpr(st);
  if (isa<LoadInst>(l)) {
    /* TODO: this is a terrible workaround, solve better */
    // Value *old = l;
    l = cast<LoadInst>(l)->getPointerOperand();
    // static_cast<LoadInst *>(old)->eraseFromParent();
    // TODO: SOLVE THIS BETTER! This creates an unnecessary `load`!
  }
  return derefPtr(toExpr(static_cast<Value *>(llvmdata->B.CreateStructGEP(
                      NULL, l, n - 1, "structmemb"))),
                  0);
}

struct BExpr *intLiteral(int val) {
  return toExpr(ConstantInt::get(
      static_cast<Type *>(Type::getInt32Ty(llvmdata->C)), val, true));
}
struct BExpr *floatLiteral(double val) {
  return toExpr(ConstantFP::get(llvmdata->C, APFloat(val)));
}
struct BExpr *stringLiteral(const char *val) {
  /* TODO: use CreateGlobalStringPtr */
  Constant *str = ConstantDataArray::getString(llvmdata->C, val);
  GlobalVariable *global = new GlobalVariable(
      *llvmdata->M, str->getType(), true,
      GlobalValue::LinkageTypes::PrivateLinkage, str, "strliteral");
  global->setAlignment(1);
  std::vector<Value *> idxlist;
  idxlist.resize(2);
  idxlist[0] = ConstantInt::get(
      static_cast<Type *>(Type::getInt64Ty(llvmdata->C)), 0, true);
  idxlist[1] = idxlist[0];
  return toExpr(static_cast<Value *>(
      llvmdata->B.CreateInBoundsGEP(global, idxlist, "strgep")));
}

struct BType *voidType() {
  return toType(Type::getVoidTy(llvmdata->C));
}
struct BType *intType(int flags, int size) { /* size in bytes */
  /* LLVM does not care about the flags */
  switch (size) {
  case 1:
    return toType(static_cast<Type *>(Type::getInt8Ty(llvmdata->C)));
  case 2:
    return toType(static_cast<Type *>(Type::getInt16Ty(llvmdata->C)));
  case 4:
    return toType(static_cast<Type *>(Type::getInt32Ty(llvmdata->C)));
  case 8:
    return toType(static_cast<Type *>(Type::getInt64Ty(llvmdata->C)));
  }
  assert(0);
  return NULL; /* never reached */
}
struct BType *floatType(int size) { /* size in bytes */
  switch (size) {
  case 4:
    return toType(Type::getFloatTy(llvmdata->C));
  case 8:
    return toType(Type::getDoubleTy(llvmdata->C));
  }
  assert(0);
  return NULL; /* never reached */
}
struct BType *structType(struct BStruct *st) {
  return toType(st->Type);
}
void constType(struct BType *t) {
  (void)t; /* LLVM does not care about constness */
}
struct BType *ptrType(struct BType *t, int isvolatile) {
  (void)isvolatile;
  if (fromType(t)->isVoidTy()) {
    /* LLVM does not like void pointers */
    return toType(static_cast<Type *>(PointerType::getUnqual(
        static_cast<Type *>(Type::getInt8Ty(llvmdata->C)))));
  }
  return toType(static_cast<Type *>(PointerType::getUnqual(fromType(t))));
}
struct BType *fnPtrType(struct BType *rettype, int nparms,
                        struct BType **parms) {
  std::vector<Type *> args(nparms);
  int i;
  for (i = 0; i < nparms; ++i) {
    args[i] = fromType(parms[i]);
  }
  return toType(static_cast<Type *>(PointerType::getUnqual(
      FunctionType::get(fromType(rettype), args, false))));
}
struct BType *arrayType(struct BType *t, int size) {
  return toType(static_cast<Type *>(ArrayType::get(fromType(t), size)));
}

struct BExpr *pointerToArray(struct BExpr *r, struct BType *ptrtype) {
  Value *l = fromExpr(r);
  if (isa<LoadInst>(l)) {
    /* TODO: this is a terrible workaround, solve better */
    Value *old = l;
    l = cast<LoadInst>(l)->getPointerOperand();
    static_cast<LoadInst *>(old)->eraseFromParent();
  }
  return toExpr(llvmdata->B.CreateBitCast(l, fromType(ptrtype), "arraydecay"));
}

struct BExpr *derefPtr(struct BExpr *e, int isvolatileptr) {
  return toExpr(llvmdata->B.CreateLoad(fromExpr(e), isvolatileptr, "loadtmp"));
}
struct BExpr *refof(struct BExpr *e) {
  return e; /* not necessary in LLVM */
}

struct BExpr *castInt(struct BExpr *a, struct BType *t, int issigned) {
  return toExpr(llvmdata->B.CreateIntCast(fromExpr(a), fromType(t), issigned));
}
struct BExpr *castFloat(struct BExpr *a, struct BType *t) {
  return toExpr(llvmdata->B.CreateFPCast(fromExpr(a), fromType(t)));
}
struct BExpr *castIntToFloat(struct BExpr *a, struct BType *t, int issigned) {
  if (issigned) {
    return toExpr(llvmdata->B.CreateSIToFP(fromExpr(a), fromType(t)));
  }
  return toExpr(llvmdata->B.CreateUIToFP(fromExpr(a), fromType(t)));
}
struct BExpr *castFloatToInt(struct BExpr *a, struct BType *t, int issigned) {
  if (issigned) {
    return toExpr(llvmdata->B.CreateFPToSI(fromExpr(a), fromType(t)));
  }
  return toExpr(llvmdata->B.CreateFPToUI(fromExpr(a), fromType(t)));
}
struct BExpr *castPtr(struct BExpr *a, struct BType *t) {
  return toExpr(llvmdata->B.CreatePointerCast(fromExpr(a), fromType(t)));
}
struct BExpr *castIntToPtr(struct BExpr *a, struct BType *t) {
  return toExpr(llvmdata->B.CreateIntToPtr(fromExpr(a), fromType(t)));
}
struct BExpr *castPtrToInt(struct BExpr *a, struct BType *t) {
  return toExpr(llvmdata->B.CreatePtrToInt(fromExpr(a), fromType(t)));
}

struct BExpr *arithmeticOp(const char *op, struct BExpr *a, struct BExpr *b,
                           int result_flags, int result_size, int ptr) {
  (void)result_size;
  (void)result_flags;
  if (ptr && strcmp(op, "+") == 0) {
    std::vector<Value *> idxlist;
    idxlist.resize(1);
    idxlist[0] = fromExpr(b);
    return toExpr(static_cast<Value *>(
        llvmdata->B.CreateInBoundsGEP(fromExpr(a), idxlist, "ptradd")));
  }
#define OP(str, fun)                                                           \
  if (strcmp(op, str) == 0) {                                                  \
    return toExpr(llvmdata->B.fun(fromExpr(a), fromExpr(b), "armtmp"));        \
  }
  OP("+", CreateAdd)
  OP("-", CreateSub)
  OP("*", CreateMul)
  OP("/", CreateSDiv)
  OP("&&", CreateAnd)
  OP("||", CreateOr)
  OP("<<", CreateShl)
  OP(">>", CreateAShr)
  OP("%", CreateSRem)
  OP("==", CreateICmpEQ)
  OP("!=", CreateICmpNE)
  OP("<", CreateICmpSLT)
  OP(">", CreateICmpSGT)
  OP("<=", CreateICmpSLE)
  OP(">=", CreateICmpSGE)

#undef OP
  assert(!"arithmetic operation unknown to the LLVM backend");
  return NULL; /* never reached, used to silence compiler warning */
}
struct BExpr *unaryOp(char op, struct BExpr *a, int result_flags,
                      int result_size) {
  /* TODO: POINTERS! */
  (void)result_size;
  (void)result_flags;
  switch (op) {
  case '!':
    return toExpr(llvmdata->B.CreateNot(fromExpr(a), "unarytmp"));
  case '~':
    return toExpr(llvmdata->B.CreateXor(fromExpr(a), -1, "unarytmp"));
  }
  assert(0);
  return NULL; /* never reached */
}
struct BExpr *arithmeticFPOp(const char *op, struct BExpr *a, struct BExpr *b,
                             int floatsize) {
  (void)floatsize;
#define OP(str, fun)                                                           \
  if (strcmp(op, str) == 0) {                                                  \
    return toExpr(llvmdata->B.fun(fromExpr(a), fromExpr(b), "armtmp"));        \
  }
  OP("+", CreateFAdd)
  OP("-", CreateFDiv)
  OP("*", CreateFMul)
  OP("/", CreateFDiv)
  OP("%", CreateFRem)
  OP("==", CreateFCmpOEQ)
  OP("!=", CreateFCmpONE)
  OP("<", CreateFCmpOLT)
  OP(">", CreateFCmpOGT)
  OP("<=", CreateFCmpOLE)
  OP(">=", CreateFCmpOGE)

#undef OP
  assert(0);
  return NULL; /* never reached */
}

struct BExpr *fnRefof(struct BFunction *f) {
  return toExpr(static_cast<Value *>(fromFunction(f)));
}

struct BIncompleteParm {
  const char *Name;
  Type *T;
};
struct BIncompleteFunction {
  const char *Name;
  struct BType *RetType;
  int Flags;
  struct BIncompleteParm *Parms;
  int NParms;
  Function *F;
  struct BIncompleteFunction *Parent;
};
struct BIncompleteFunction *curfn;

void beginFnPrototype(const char *name, struct BType *rettype, int flags) {
  struct BIncompleteFunction *a =
      (struct BIncompleteFunction *)getMem(sizeof(BIncompleteFunction));
  a->Name = name;
  a->RetType = rettype;
  a->Flags = flags;
  a->Parent = curfn;
  curfn = a;
}

struct BFunction *endFnPrototype(int addBody) {
  std::vector<Type *> args(curfn->NParms);
  int i;
  for (i = 0; i < curfn->NParms; ++i) {
    args[i] = curfn->Parms[i].T;
  }
  FunctionType *ft = FunctionType::get(fromType(curfn->RetType), args, false);
  Function *f = Function::Create(ft, Function::ExternalLinkage, curfn->Name,
                                 llvmdata->M.get());
  /* TODO: Flags */
  i = 0;
  for (auto &arg : f->args()) {
    arg.setName(curfn->Parms[i].Name);
    ++i;
  }
  curfn->F = f;
  if (addBody) {
    if (llvmdata->B.GetInsertBlock()) {
      llvmdata->InsertPoints.push(InsertPoint{llvmdata->B.GetInsertBlock(),
                                              llvmdata->B.GetInsertPoint()});
    }
    BasicBlock *block = BasicBlock::Create(llvmdata->C, "entry", curfn->F);
    llvmdata->B.SetInsertPoint(block);
    return toFunction(curfn->F);
  } else {
    struct BIncompleteFunction *f = curfn;
    curfn = curfn->Parent;
    return toFunction(f->F);
  }
}
void endFnBody(struct BExpr *e) {
  if (e) {
    llvmdata->B.CreateRet(fromExpr(e));
  } else {
    llvmdata->B.CreateRetVoid();
  }
  verifyFunction(*curfn->F, &llvm::errs());

  if (llvmdata->InsertPoints.size()) {
    const auto &top = llvmdata->InsertPoints.top();
    llvmdata->B.SetInsertPoint(top.first, top.second);
    llvmdata->InsertPoints.pop();
  } else {
    llvmdata->B.ClearInsertionPoint();
  }

  curfn = curfn->Parent;
}

struct BVar *addVariable(const char *name, struct BType *type, int) {
  IRBuilder<> b(&curfn->F->getEntryBlock(), curfn->F->getEntryBlock().begin());
  return toVar(b.CreateAlloca(fromType(type), 0, name));
}
struct BVar *addParameter(const char *name, struct BType *type) {
  curfn->Parms = (struct BIncompleteParm *)moreMem(
      curfn->Parms, sizeof(struct BIncompleteParm) * curfn->NParms,
      sizeof(struct BIncompleteParm));
  curfn->Parms[curfn->NParms].Name = name;
  curfn->Parms[curfn->NParms].T = fromType(type);
  return (struct BVar *)curfn->NParms++;
}
struct BVar *updateParameter(struct BVar *v) {
  Function::arg_iterator a = curfn->F->arg_begin();
  std::advance(a, (unsigned long)v);
  IRBuilder<> b(&curfn->F->getEntryBlock(), curfn->F->getEntryBlock().begin());
  Value *p = b.CreateAlloca(a->getType(), 0, a->getName());
  llvmdata->B.CreateStore(&(*a), p);
  return toVar(p);
}
struct BVar *addGlobal(const char *name, struct BType *t, int flags) {
  GlobalVariable *global = new GlobalVariable(
      *llvmdata->M, fromType(t), false, /* TODO: const globals */
      GlobalValue::LinkageTypes::ExternalLinkage, NULL, name);
  (void)flags;          /* TODO: flags */
  return toVar(global); /* TODO: alignment */
}
struct BExpr *varUsage(struct BVar *p, int isvolatilevar) {
  Value *v = fromVar(p);
  if (isa<AllocaInst>(v) /* && !isa<ArrayType>(v->getType())*/) {
    if (!isa<StructType>(static_cast<AllocaInst *>(v)->getAllocatedType())) {
      return toExpr(llvmdata->B.CreateLoad(v, isvolatilevar, "varusage"));
    }
  }
  return toExpr(v);
}

struct BExpr *setVar(struct BExpr *lhs, struct BExpr *rhs) {
  Value *l = fromExpr(lhs);
  int isvolatile = 0;
  if (isa<LoadInst>(l)) {
    /* TODO: this is a terrible workaround, solve better */
    Value *old = l;
    isvolatile = cast<LoadInst>(l)->isVolatile();
    l = cast<LoadInst>(l)->getPointerOperand();
    static_cast<LoadInst *>(old)->eraseFromParent();
  }
  llvmdata->B.CreateStore(fromExpr(rhs), l, isvolatile);
  return lhs;
}

struct BTemporary *addTemporary(struct BExpr *e, struct BType *t) {
  (void)t;
  /* in LLVM, expressions can be reused, no need for temporaries! */
  return (struct BTemporary *)e;
}
struct BExpr *tmpInstance(struct BTemporary *tmp) {
  return (struct BExpr *)tmp;
}

struct BIncompleteFuncall { /* not POD!!! */
  std::vector<Value *> Args;
  struct BFunction *F;
  struct BExpr *FunPtr;
};
struct BIncompleteFuncall *beginFuncall(struct BFunction *f) {
  struct BIncompleteFuncall *r =
      (struct BIncompleteFuncall *)getMem(sizeof(struct BIncompleteFuncall));
  new (r) struct BIncompleteFuncall;
  r->F = f;
  return r;
}
struct BIncompleteFuncall *beginFunPtrCall(struct BExpr *e) {
  struct BIncompleteFuncall *r =
      (struct BIncompleteFuncall *)getMem(sizeof(struct BIncompleteFuncall));
  new (r) struct BIncompleteFuncall;
  r->FunPtr = e;
  return r;
}
void addArg(struct BIncompleteFuncall *c, struct BExpr *val) {
  c->Args.push_back(fromExpr(val));
}
struct BExpr *endFuncall(struct BIncompleteFuncall *c) {
  struct BExpr *r;
  if (c->F) {
    r = toExpr(llvmdata->B.CreateCall(fromFunction(c->F), c->Args));
  } else {
    r = toExpr(llvmdata->B.CreateCall(fromExpr(c->FunPtr), c->Args));
  }
  return r;
}

static Value *toCond(Value *a) {
  return llvmdata->B.CreateIntCast(
      a, Type::getInt1Ty(llvmdata->C),
      false /* for a condition, signedness does not matter; TODO: other
               types as conditions (POINTERS!) */
      ,
      "cond");
}
struct BIf {
  BasicBlock *IfTrue, *IfFalse, *AfterIf;
  struct BIf *Parent;
};
static struct BIf *lastif;
void beginIfStmt(struct BExpr *cond) {
  struct BIf *cnt;
  cnt = (struct BIf *)getMem(sizeof(struct BIf));
  cnt->IfTrue = BasicBlock::Create(llvmdata->C, "iftrue", curfn->F);
  cnt->AfterIf = BasicBlock::Create(llvmdata->C, "afterif", curfn->F);
  llvmdata->B.CreateCondBr(toCond(fromExpr(cond)), cnt->IfTrue, cnt->AfterIf);
  cnt->Parent = lastif;
  lastif = cnt;
  llvmdata->B.SetInsertPoint(cnt->IfTrue);
}
void endIfStmt(struct BExpr *r) {
  llvmdata->B.CreateBr(lastif->AfterIf);
  lastif->IfTrue = llvmdata->B.GetInsertBlock();
  llvmdata->B.SetInsertPoint(lastif->AfterIf);
  lastif = lastif->Parent;
}
void beginIfElseStmt(struct BExpr *cond) {
  struct BIf *cnt;
  cnt = (struct BIf *)getMem(sizeof(struct BIf));
  cnt->IfTrue = BasicBlock::Create(llvmdata->C, "iftrue", curfn->F);
  cnt->IfFalse = BasicBlock::Create(llvmdata->C, "iffalse", curfn->F);
  cnt->AfterIf = BasicBlock::Create(llvmdata->C, "afterif", curfn->F);
  llvmdata->B.CreateCondBr(toCond(fromExpr(cond)), cnt->IfTrue, cnt->IfFalse);
  cnt->Parent = lastif;
  lastif = cnt;
  llvmdata->B.SetInsertPoint(cnt->IfTrue);
}
void *elseIfStmt(struct BExpr *r, struct BType *rettype) {
  llvmdata->B.CreateBr(lastif->AfterIf);
  lastif->IfTrue = llvmdata->B.GetInsertBlock();
  llvmdata->B.SetInsertPoint(lastif->IfFalse);
  (void)rettype; /* not necessary, Value is typed */
  return (void *)fromExpr(r);
}
struct BExpr *endIfElseStmt(void *last, struct BExpr *r) {
  llvmdata->B.CreateBr(lastif->AfterIf);
  lastif->IfFalse = llvmdata->B.GetInsertBlock();
  llvmdata->B.SetInsertPoint(lastif->AfterIf);
  Value *a = (Value *)last;
  if (a) {
    Value *b = fromExpr(r);
    PHINode *phi = llvmdata->B.CreatePHI(a->getType(), 2, "iftmp");
    phi->addIncoming(a, lastif->IfTrue);
    phi->addIncoming(b, lastif->IfFalse);
    lastif = lastif->Parent;
    return toExpr(phi);
  }
  lastif = lastif->Parent;
  return NULL;
}

struct BWhile {
  BasicBlock *Cond, *Body, *After;
  struct BWhile *Parent;
};
struct BWhile *lastwhile;
void beginWhileLoopCond() {
  struct BWhile *cnt;
  cnt = (struct BWhile *)getMem(sizeof(struct BWhile));
  cnt->Cond = BasicBlock::Create(llvmdata->C, "condloop", curfn->F);
  cnt->Body = BasicBlock::Create(llvmdata->C, "loopbody", curfn->F);
  cnt->After = BasicBlock::Create(llvmdata->C, "afterloop", curfn->F);
  llvmdata->B.CreateBr(cnt->Cond);
  cnt->Parent = lastwhile;
  lastwhile = cnt;
  llvmdata->B.SetInsertPoint(cnt->Cond);
}
void beginWhileLoopBody(struct BExpr *cond) {
  llvmdata->B.CreateCondBr(toCond(fromExpr(cond)), lastwhile->Body,
                           lastwhile->After);
  lastwhile->Cond = llvmdata->B.GetInsertBlock();
  llvmdata->B.SetInsertPoint(lastwhile->Body);
}
void endWhileLoop() {
  llvmdata->B.CreateBr(lastwhile->Cond);
  lastwhile->Body = llvmdata->B.GetInsertBlock();
  llvmdata->B.SetInsertPoint(lastwhile->After);
  lastwhile = lastwhile->Parent;
}

void breakLoop() { llvmdata->B.CreateBr(lastwhile->After); }
void continueLoop() { llvmdata->B.CreateBr(lastwhile->Cond); }

void addEvaluation(struct BExpr *a) { (void)a; /* not necessary in LLVM */ }

struct BExpr *sizeofType(struct BType *t) {
  return intLiteral(
      llvm::DataLayout(llvmdata->M.get()).getTypeAllocSize(fromType(t)));
}
