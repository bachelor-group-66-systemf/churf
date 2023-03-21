// TODO: include these properly
#include "llvm/CodeGen/GCStrategy.h"
#include "llvm/CodeGen/GCMetadata.h"
#include "llvm/Support/Compiler.h"

using namespace llvm;

namespace {
  class LLVM_LIBRARY_VISIBILITY GC : public GCStrategy {
  public:
    GC() {}
  };

  GCRegistry::Add<GC>
  X("gc", "The bespoken garbage collector.");
}