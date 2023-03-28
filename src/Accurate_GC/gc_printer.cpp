#include "llvm/CodeGen/GCMetadataPrinter.h"
#include "llvm/Support/Compiler.h"

using namespace llvm;

namespace {
  class LLVM_LIBRARY_VISIBILITY GCPrinter : public GCMetadataPrinter {
  public:
    virtual void beginAssembly(AsmPrinter &AP);

    virtual void finishAssembly(AsmPrinter &AP);
  };

  GCMetadataPrinterRegistry::Add<MyGCPrinter>
  X("gc", "The bespoken garbage collector.");
}