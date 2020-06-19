/*
void kprintf(Module *mod, BasicBlock *bb, const char *format, ...)
  {
      Function *func_printf = mod->getFunction("printf");
      if (!func_printf) {
          PointerType *Pty = PointerType::get(IntegerType::get(mod->getContext(), 8), 0);
          FunctionType *FuncTy9 = FunctionType::get(IntegerType::get(mod->getContext(), 32), true);

          func_printf = Function::Create(FuncTy9, GlobalValue::ExternalLinkage, "printf", mod);
          func_printf->setCallingConv(CallingConv::C);

          AttrListPtr func_printf_PAL;
          func_printf->setAttributes(func_printf_PAL);
      }

      IRBuilder <> builder(mod->getContext());
      builder.SetInsertPoint(bb);

      Value *str = builder.CreateGlobalStringPtr(format);
      std::vector <Value *> int32_call_params;
      int32_call_params.push_back(str);

      va_list ap;
      va_start(ap, format);

      char *str_ptr = va_arg(ap, char*);
      Value *format_ptr = builder.CreateGlobalStringPtr(str_ptr);
      int32_call_params.push_back(format_ptr);

      std::vector<llvm::Value*> extra;
      do {
          llvm::Value *op = va_arg(ap, llvm::Value*);
          if (op) {
              int32_call_params.push_back(op);
          } else {
              break;
          }
      } while (1);
      va_end(ap);

      CallInst * int32_call = CallInst::Create(func_printf, int32_call_params, "call", bb);
  }
  #define oprintf(...) kprintf(__VA_ARGS__)
  #define llvm_printf(...) oprintf(mod, bb, __VA_ARGS__, NULL)

  llvm_printf("Output: 0x%08X %f %d\n", 0x12345678, 3.1415926, 12345);
  */
