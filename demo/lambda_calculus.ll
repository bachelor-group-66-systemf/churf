target triple = "x86_64-pc-linux-gnu"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
@.str = private unnamed_addr constant [2 x i8] c"%i", align 1
@.new_line = private unnamed_addr constant [1 x i8] c"
", align 1
@.non_exhaustive_patterns = private unnamed_addr constant [41 x i8] c"Non-exhaustive patterns in case at %i:%i
"
@.char_print = private unnamed_addr constant [2 x i8] c"%c"
@.char_print_no_nl = private unnamed_addr constant [3 x i8] c"%c "
@.int_print_no_nl = private unnamed_addr constant [3 x i8] c"%i "
declare i32 @printf(ptr noalias nocapture, ...)
declare i32 @exit(i32 noundef)
declare ptr @malloc(i32 noundef)
declare external void @cheap_init()
declare external ptr @cheap_alloc(i64)
declare external void @cheap_dispose()
declare external ptr @cheap_the()
declare external void @cheap_set_profiler(ptr, i1)
declare external void @cheap_profiler_log_options(ptr, i64)
%Cons$List.Pair.Char.Val = type { i8, %PairCharVal*, %ListPairCharVal* }
%Cxt = type { i8, [15 x i8] }
%Cxt$Cxt = type { i8, %ListPairCharVal* }
%EAbs$Exp = type { i8, i8, %Exp* }
%EAdd$Exp = type { i8, %Exp*, %Exp* }
%EApp$Exp = type { i8, %Exp*, %Exp* }
%EInt$Exp = type { i8, i64 }
%EVar$Exp = type { i8, i8 }
%Exp = type { i8, [23 x i8] }
%ListPairCharVal = type { i8, [23 x i8] }
%Nil$List.Pair.Char.Val = type { i8 }
%Pair$Pair.Char.Val = type { i8, i8, %Val* }
%PairCharVal = type { i8, [23 x i8] }
%VClosure$Val = type { i8, %Cxt*, i8, %Exp* }
%VInt$Val = type { i8, i64 }
%Val = type { i8, [31 x i8] }

; Ident "main": ECase (EApp (EApp (EVar (Ident "eval$Cxt_Exp_Val"),TFun (TLit (Ident "Cxt")) (TFun (TLit (Ident "Exp")) (TLit (Ident "Val")))) (EApp (EVar (Ident "Cxt$Cxt"),TFun (TLit (Ident "ListPairCharVal")) (TLit (Ident "Cxt"))) (EVar (Ident "Nil$List.Pair.Char.Val"),TLit (Ident "ListPairCharVal")),TLit (Ident "Cxt")),TFun (TLit (Ident "Exp")) (TLit (Ident "Val"))) (EVar (Ident "exp$Exp"),TLit (Ident "Exp")),TLit (Ident "Val")) [Branch (PInj (Ident "VInt$Val") [(PVar (Ident "$41i"),TLit (Ident "Int"))],TLit (Ident "Val")) (EVar (Ident "$41i"),TLit (Ident "Int"))]
define fastcc i64 @main() {
	call void @cheap_init()
	%1 = call fastcc %ListPairCharVal @Nil$List.Pair.Char.Val(ptr null)
	; CustomType (Ident "Cxt")
	%2 = call fastcc %Cxt @Cxt$Cxt(ptr null, %ListPairCharVal %1)
	%3 = call fastcc %Exp @exp$Exp(ptr null)
	; CustomType (Ident "Val")
	%4 = call fastcc %Val @eval$Cxt_Exp_Val(ptr null, %Cxt %2, %Exp %3)
	%5 = alloca i64 
	; Inj
	%6 = extractvalue %Val %4, 0
	%7 = icmp eq i8 %6, 1
	br i1 %7, label %lbl_success_3, label %lbl_failed_2
	
lbl_success_3:
	%8 = alloca %Val 
	store %Val %4, ptr %8
	%9 = load %VInt$Val, ptr %8
	; ident i64
	%$41i = extractvalue %VInt$Val %9, 1
	store i64 %$41i, ptr %5
	br label %lbl_escape_1
	
lbl_failed_2:
	call i32 (ptr, ...) @printf(ptr noundef @.non_exhaustive_patterns, i64 noundef 9, i64 noundef 6)
	call void @cheap_dispose()
	call i32 @exit(i32 noundef 1)
	br label %lbl_escape_1
	
lbl_escape_1:
	%12 = load i64, ptr %5
	call i32 (ptr, ...) @printf(ptr noundef @.str, i64 noundef %12)
	call i32 (ptr, ...) @printf(ptr noundef @.new_line)
	call void @cheap_dispose()
	ret i64 0
}

; Ident "lookup$Char_Cxt_Val": ECase (EVar (Ident "$9cxt"),TLit (Ident "Cxt")) [Branch (PInj (Ident "Cxt$Cxt") [(PVar (Ident "$10ps"),TLit (Ident "ListPairCharVal"))],TLit (Ident "Cxt")) (ECase (EVar (Ident "$10ps"),TLit (Ident "ListPairCharVal")) [Branch (PInj (Ident "Cons$List.Pair.Char.Val") [(PVar (Ident "$11p"),TLit (Ident "PairCharVal")),(PVar (Ident "$12ps"),TLit (Ident "ListPairCharVal"))],TLit (Ident "ListPairCharVal")) (ECase (EVar (Ident "$11p"),TLit (Ident "PairCharVal")) [Branch (PInj (Ident "Pair$Pair.Char.Val") [(PVar (Ident "$13y"),TLit (Ident "Char")),(PVar (Ident "$14v"),TLit (Ident "Val"))],TLit (Ident "PairCharVal")) (ECase (EApp (EApp (EVar (Ident "$equals$$equals$$Int_Int_Bool"),TFun (TLit (Ident "Int")) (TFun (TLit (Ident "Int")) (TLit (Ident "Bool")))) (EApp (EVar (Ident "asciiCode$Char_Int"),TFun (TLit (Ident "Char")) (TLit (Ident "Int"))) (EVar (Ident "$8x"),TLit (Ident "Char")),TLit (Ident "Int")),TFun (TLit (Ident "Int")) (TLit (Ident "Bool"))) (EApp (EVar (Ident "asciiCode$Char_Int"),TFun (TLit (Ident "Char")) (TLit (Ident "Int"))) (EVar (Ident "$13y"),TLit (Ident "Char")),TLit (Ident "Int")),TLit (Ident "Bool")) [Branch (PEnum (Ident "True$Bool"),TLit (Ident "Bool")) (EVar (Ident "$14v"),TLit (Ident "Val")),Branch (PEnum (Ident "False$Bool"),TLit (Ident "Bool")) (EApp (EApp (EVar (Ident "lookup$Char_Cxt_Val"),TFun (TLit (Ident "Char")) (TFun (TLit (Ident "Cxt")) (TLit (Ident "Val")))) (EVar (Ident "$8x"),TLit (Ident "Char")),TFun (TLit (Ident "Cxt")) (TLit (Ident "Val"))) (EApp (EVar (Ident "Cxt$Cxt"),TFun (TLit (Ident "ListPairCharVal")) (TLit (Ident "Cxt"))) (EVar (Ident "$12ps"),TLit (Ident "ListPairCharVal")),TLit (Ident "Cxt")),TLit (Ident "Val"))],TLit (Ident "Val"))],TLit (Ident "Val"))],TLit (Ident "Val"))]
define fastcc %Val @lookup$Char_Cxt_Val(ptr %cxt, i8 %$8x, %Cxt %$9cxt) {
	%1 = alloca %Val 
	; Inj
	%2 = extractvalue %Cxt %$9cxt, 0
	%3 = icmp eq i8 %2, 0
	br i1 %3, label %lbl_success_6, label %lbl_failed_5
	
lbl_success_6:
	%4 = alloca %Cxt 
	store %Cxt %$9cxt, ptr %4
	%5 = load %Cxt$Cxt, ptr %4
	; ident %ListPairCharVal
	%6 = extractvalue %Cxt$Cxt %5, 1
	%$10ps = load %ListPairCharVal, ptr %6
	%7 = alloca %Val 
	; Inj
	%8 = extractvalue %ListPairCharVal %$10ps, 0
	%9 = icmp eq i8 %8, 0
	br i1 %9, label %lbl_success_9, label %lbl_failed_8
	
lbl_success_9:
	%10 = alloca %ListPairCharVal 
	store %ListPairCharVal %$10ps, ptr %10
	%11 = load %Cons$List.Pair.Char.Val, ptr %10
	; ident %PairCharVal
	%12 = extractvalue %Cons$List.Pair.Char.Val %11, 1
	%$11p = load %PairCharVal, ptr %12
	; ident %ListPairCharVal
	%13 = extractvalue %Cons$List.Pair.Char.Val %11, 2
	%$12ps = load %ListPairCharVal, ptr %13
	%14 = alloca %Val 
	; Inj
	%15 = extractvalue %PairCharVal %$11p, 0
	%16 = icmp eq i8 %15, 0
	br i1 %16, label %lbl_success_12, label %lbl_failed_11
	
lbl_success_12:
	%17 = alloca %PairCharVal 
	store %PairCharVal %$11p, ptr %17
	%18 = load %Pair$Pair.Char.Val, ptr %17
	; ident i8
	%$13y = extractvalue %Pair$Pair.Char.Val %18, 1
	; ident %Val
	%19 = extractvalue %Pair$Pair.Char.Val %18, 2
	%$14v = load %Val, ptr %19
	; I64
	%20 = call fastcc i64 @asciiCode$Char_Int(ptr null, i8 %$8x)
	; I64
	%21 = call fastcc i64 @asciiCode$Char_Int(ptr null, i8 %$13y)
	; I1
	%22 = call fastcc i1 @$equals$$equals$$Int_Int_Bool(ptr null, i64 %20, i64 %21)
	%23 = alloca %Val 
	; Plit
	%24 = icmp eq i1 %22, 1
	br i1 %24, label %lbl_success_15, label %lbl_failed_14
	
lbl_success_15:
	store %Val %$14v, ptr %23
	br label %lbl_escape_13
	
lbl_failed_14:
	; Plit
	%25 = icmp eq i1 %22, 0
	br i1 %25, label %lbl_success_17, label %lbl_failed_16
	
lbl_success_17:
	; CustomType (Ident "Cxt")
	%26 = call fastcc %Cxt @Cxt$Cxt(ptr null, %ListPairCharVal %$12ps)
	; CustomType (Ident "Val")
	%27 = call fastcc %Val @lookup$Char_Cxt_Val(ptr null, i8 %$8x, %Cxt %26)
	store %Val %27, ptr %23
	br label %lbl_escape_13
	
lbl_failed_16:
	call i32 (ptr, ...) @printf(ptr noundef @.non_exhaustive_patterns, i64 noundef 27, i64 noundef 6)
	call void @cheap_dispose()
	call i32 @exit(i32 noundef 1)
	br label %lbl_escape_13
	
lbl_escape_13:
	%30 = load %Val, ptr %23
	store %Val %30, ptr %14
	br label %lbl_escape_10
	
lbl_failed_11:
	call i32 (ptr, ...) @printf(ptr noundef @.non_exhaustive_patterns, i64 noundef 30, i64 noundef 6)
	call void @cheap_dispose()
	call i32 @exit(i32 noundef 1)
	br label %lbl_escape_10
	
lbl_escape_10:
	%33 = load %Val, ptr %14
	store %Val %33, ptr %7
	br label %lbl_escape_7
	
lbl_failed_8:
	call i32 (ptr, ...) @printf(ptr noundef @.non_exhaustive_patterns, i64 noundef 33, i64 noundef 6)
	call void @cheap_dispose()
	call i32 @exit(i32 noundef 1)
	br label %lbl_escape_7
	
lbl_escape_7:
	%36 = load %Val, ptr %7
	store %Val %36, ptr %1
	br label %lbl_escape_4
	
lbl_failed_5:
	call i32 (ptr, ...) @printf(ptr noundef @.non_exhaustive_patterns, i64 noundef 36, i64 noundef 6)
	call void @cheap_dispose()
	call i32 @exit(i32 noundef 1)
	br label %lbl_escape_4
	
lbl_escape_4:
	%39 = load %Val, ptr %1
	ret %Val %39
}

; Ident "insert$Char_Val_Cxt_Cxt": ECase (EVar (Ident "$17cxt"),TLit (Ident "Cxt")) [Branch (PInj (Ident "Cxt$Cxt") [(PVar (Ident "$18ps"),TLit (Ident "ListPairCharVal"))],TLit (Ident "Cxt")) (EApp (EVar (Ident "Cxt$Cxt"),TFun (TLit (Ident "ListPairCharVal")) (TLit (Ident "Cxt"))) (EApp (EApp (EVar (Ident "Cons$List.Pair.Char.Val"),TFun (TLit (Ident "PairCharVal")) (TFun (TLit (Ident "ListPairCharVal")) (TLit (Ident "ListPairCharVal")))) (EApp (EApp (EVar (Ident "Pair$Pair.Char.Val"),TFun (TLit (Ident "Char")) (TFun (TLit (Ident "Val")) (TLit (Ident "PairCharVal")))) (EVar (Ident "$15x"),TLit (Ident "Char")),TFun (TLit (Ident "Val")) (TLit (Ident "PairCharVal"))) (EVar (Ident "$16v"),TLit (Ident "Val")),TLit (Ident "PairCharVal")),TFun (TLit (Ident "ListPairCharVal")) (TLit (Ident "ListPairCharVal"))) (EVar (Ident "$18ps"),TLit (Ident "ListPairCharVal")),TLit (Ident "ListPairCharVal")),TLit (Ident "Cxt"))]
define fastcc %Cxt @insert$Char_Val_Cxt_Cxt(ptr %cxt, i8 %$15x, %Val %$16v, %Cxt %$17cxt) {
	%1 = alloca %Cxt 
	; Inj
	%2 = extractvalue %Cxt %$17cxt, 0
	%3 = icmp eq i8 %2, 0
	br i1 %3, label %lbl_success_20, label %lbl_failed_19
	
lbl_success_20:
	%4 = alloca %Cxt 
	store %Cxt %$17cxt, ptr %4
	%5 = load %Cxt$Cxt, ptr %4
	; ident %ListPairCharVal
	%6 = extractvalue %Cxt$Cxt %5, 1
	%$18ps = load %ListPairCharVal, ptr %6
	; CustomType (Ident "PairCharVal")
	%7 = call fastcc %PairCharVal @Pair$Pair.Char.Val(ptr null, i8 %$15x, %Val %$16v)
	; CustomType (Ident "ListPairCharVal")
	%8 = call fastcc %ListPairCharVal @Cons$List.Pair.Char.Val(ptr null, %PairCharVal %7, %ListPairCharVal %$18ps)
	; CustomType (Ident "Cxt")
	%9 = call fastcc %Cxt @Cxt$Cxt(ptr null, %ListPairCharVal %8)
	store %Cxt %9, ptr %1
	br label %lbl_escape_18
	
lbl_failed_19:
	call i32 (ptr, ...) @printf(ptr noundef @.non_exhaustive_patterns, i64 noundef 9, i64 noundef 6)
	call void @cheap_dispose()
	call i32 @exit(i32 noundef 1)
	br label %lbl_escape_18
	
lbl_escape_18:
	%12 = load %Cxt, ptr %1
	ret %Cxt %12
}

; Ident "exp$Exp": EApp (EApp (EVar (Ident "EApp$Exp"),TFun (TLit (Ident "Exp")) (TFun (TLit (Ident "Exp")) (TLit (Ident "Exp")))) (EApp (EApp (EVar (Ident "EAbs$Exp"),TFun (TLit (Ident "Char")) (TFun (TLit (Ident "Exp")) (TLit (Ident "Exp")))) (ELit (LChar 'x'),TLit (Ident "Char")),TFun (TLit (Ident "Exp")) (TLit (Ident "Exp"))) (EApp (EApp (EVar (Ident "EAdd$Exp"),TFun (TLit (Ident "Exp")) (TFun (TLit (Ident "Exp")) (TLit (Ident "Exp")))) (EApp (EVar (Ident "EVar$Exp"),TFun (TLit (Ident "Char")) (TLit (Ident "Exp"))) (ELit (LChar 'x'),TLit (Ident "Char")),TLit (Ident "Exp")),TFun (TLit (Ident "Exp")) (TLit (Ident "Exp"))) (EApp (EVar (Ident "EVar$Exp"),TFun (TLit (Ident "Char")) (TLit (Ident "Exp"))) (ELit (LChar 'x'),TLit (Ident "Char")),TLit (Ident "Exp")),TLit (Ident "Exp")),TLit (Ident "Exp")),TFun (TLit (Ident "Exp")) (TLit (Ident "Exp"))) (EApp (EVar (Ident "EInt$Exp"),TFun (TLit (Ident "Int")) (TLit (Ident "Exp"))) (ELit (LInt 200),TLit (Ident "Int")),TLit (Ident "Exp"))
define fastcc %Exp @exp$Exp(ptr %cxt) {
	; CustomType (Ident "Exp")
	%1 = call fastcc %Exp @EVar$Exp(ptr null, i8 120)
	; CustomType (Ident "Exp")
	%2 = call fastcc %Exp @EVar$Exp(ptr null, i8 120)
	; CustomType (Ident "Exp")
	%3 = call fastcc %Exp @EAdd$Exp(ptr null, %Exp %1, %Exp %2)
	; CustomType (Ident "Exp")
	%4 = call fastcc %Exp @EAbs$Exp(ptr null, i8 120, %Exp %3)
	; CustomType (Ident "Exp")
	%5 = call fastcc %Exp @EInt$Exp(ptr null, i64 200)
	; CustomType (Ident "Exp")
	%6 = call fastcc %Exp @EApp$Exp(ptr null, %Exp %4, %Exp %5)
	ret %Exp %6
}

; Ident "eval$Cxt_Exp_Val": ECase (EVar (Ident "$20exp"),TLit (Ident "Exp")) [Branch (PInj (Ident "EVar$Exp") [(PVar (Ident "$21x"),TLit (Ident "Char"))],TLit (Ident "Exp")) (ECase (EApp (EApp (EVar (Ident "lookup$Char_Cxt_Val"),TFun (TLit (Ident "Char")) (TFun (TLit (Ident "Cxt")) (TLit (Ident "Val")))) (EVar (Ident "$21x"),TLit (Ident "Char")),TFun (TLit (Ident "Cxt")) (TLit (Ident "Val"))) (EVar (Ident "$19cxt"),TLit (Ident "Cxt")),TLit (Ident "Val")) [Branch (PInj (Ident "VInt$Val") [(PVar (Ident "$22i"),TLit (Ident "Int"))],TLit (Ident "Val")) (EApp (EVar (Ident "VInt$Val"),TFun (TLit (Ident "Int")) (TLit (Ident "Val"))) (EVar (Ident "$22i"),TLit (Ident "Int")),TLit (Ident "Val")),Branch (PInj (Ident "VClosure$Val") [(PVar (Ident "$23delta"),TLit (Ident "Cxt")),(PVar (Ident "$24x"),TLit (Ident "Char")),(PVar (Ident "$25e"),TLit (Ident "Exp"))],TLit (Ident "Val")) (EApp (EApp (EVar (Ident "eval$Cxt_Exp_Val"),TFun (TLit (Ident "Cxt")) (TFun (TLit (Ident "Exp")) (TLit (Ident "Val")))) (EVar (Ident "$23delta"),TLit (Ident "Cxt")),TFun (TLit (Ident "Exp")) (TLit (Ident "Val"))) (EVar (Ident "$25e"),TLit (Ident "Exp")),TLit (Ident "Val"))],TLit (Ident "Val")),Branch (PInj (Ident "EAbs$Exp") [(PVar (Ident "$26x"),TLit (Ident "Char")),(PVar (Ident "$27e"),TLit (Ident "Exp"))],TLit (Ident "Exp")) (EApp (EApp (EApp (EVar (Ident "VClosure$Val"),TFun (TLit (Ident "Cxt")) (TFun (TLit (Ident "Char")) (TFun (TLit (Ident "Exp")) (TLit (Ident "Val"))))) (EVar (Ident "$19cxt"),TLit (Ident "Cxt")),TFun (TLit (Ident "Char")) (TFun (TLit (Ident "Exp")) (TLit (Ident "Val")))) (EVar (Ident "$26x"),TLit (Ident "Char")),TFun (TLit (Ident "Exp")) (TLit (Ident "Val"))) (EVar (Ident "$27e"),TLit (Ident "Exp")),TLit (Ident "Val")),Branch (PInj (Ident "EApp$Exp") [(PVar (Ident "$28e1"),TLit (Ident "Exp")),(PVar (Ident "$29e2"),TLit (Ident "Exp"))],TLit (Ident "Exp")) (ECase (EApp (EApp (EVar (Ident "eval$Cxt_Exp_Val"),TFun (TLit (Ident "Cxt")) (TFun (TLit (Ident "Exp")) (TLit (Ident "Val")))) (EVar (Ident "$19cxt"),TLit (Ident "Cxt")),TFun (TLit (Ident "Exp")) (TLit (Ident "Val"))) (EVar (Ident "$28e1"),TLit (Ident "Exp")),TLit (Ident "Val")) [Branch (PInj (Ident "VClosure$Val") [(PVar (Ident "$30delta"),TLit (Ident "Cxt")),(PVar (Ident "$31x"),TLit (Ident "Char")),(PVar (Ident "$32f"),TLit (Ident "Exp"))],TLit (Ident "Val")) (ELet (Bind (Ident "$33v",TLit (Ident "Val")) [] (EApp (EApp (EApp (EVar (Ident "VClosure$Val"),TFun (TLit (Ident "Cxt")) (TFun (TLit (Ident "Char")) (TFun (TLit (Ident "Exp")) (TLit (Ident "Val"))))) (EVar (Ident "$19cxt"),TLit (Ident "Cxt")),TFun (TLit (Ident "Char")) (TFun (TLit (Ident "Exp")) (TLit (Ident "Val")))) (EVar (Ident "$31x"),TLit (Ident "Char")),TFun (TLit (Ident "Exp")) (TLit (Ident "Val"))) (EVar (Ident "$29e2"),TLit (Ident "Exp")),TLit (Ident "Val"))) (EApp (EApp (EVar (Ident "eval$Cxt_Exp_Val"),TFun (TLit (Ident "Cxt")) (TFun (TLit (Ident "Exp")) (TLit (Ident "Val")))) (EApp (EApp (EApp (EVar (Ident "insert$Char_Val_Cxt_Cxt"),TFun (TLit (Ident "Char")) (TFun (TLit (Ident "Val")) (TFun (TLit (Ident "Cxt")) (TLit (Ident "Cxt"))))) (EVar (Ident "$31x"),TLit (Ident "Char")),TFun (TLit (Ident "Val")) (TFun (TLit (Ident "Cxt")) (TLit (Ident "Cxt")))) (EVar (Ident "$33v"),TLit (Ident "Val")),TFun (TLit (Ident "Cxt")) (TLit (Ident "Cxt"))) (EVar (Ident "$30delta"),TLit (Ident "Cxt")),TLit (Ident "Cxt")),TFun (TLit (Ident "Exp")) (TLit (Ident "Val"))) (EVar (Ident "$32f"),TLit (Ident "Exp")),TLit (Ident "Val")),TLit (Ident "Val"))],TLit (Ident "Val")),Branch (PInj (Ident "EInt$Exp") [(PVar (Ident "$34i"),TLit (Ident "Int"))],TLit (Ident "Exp")) (EApp (EVar (Ident "VInt$Val"),TFun (TLit (Ident "Int")) (TLit (Ident "Val"))) (EVar (Ident "$34i"),TLit (Ident "Int")),TLit (Ident "Val")),Branch (PInj (Ident "EAdd$Exp") [(PVar (Ident "$35e1"),TLit (Ident "Exp")),(PVar (Ident "$36e2"),TLit (Ident "Exp"))],TLit (Ident "Exp")) (ELet (Bind (Ident "$37i1",TLit (Ident "Int")) [] (ECase (EApp (EApp (EVar (Ident "eval$Cxt_Exp_Val"),TFun (TLit (Ident "Cxt")) (TFun (TLit (Ident "Exp")) (TLit (Ident "Val")))) (EVar (Ident "$19cxt"),TLit (Ident "Cxt")),TFun (TLit (Ident "Exp")) (TLit (Ident "Val"))) (EVar (Ident "$35e1"),TLit (Ident "Exp")),TLit (Ident "Val")) [Branch (PInj (Ident "VInt$Val") [(PVar (Ident "$38i"),TLit (Ident "Int"))],TLit (Ident "Val")) (EVar (Ident "$38i"),TLit (Ident "Int"))],TLit (Ident "Int"))) (ELet (Bind (Ident "$39i2",TLit (Ident "Int")) [] (ECase (EApp (EApp (EVar (Ident "eval$Cxt_Exp_Val"),TFun (TLit (Ident "Cxt")) (TFun (TLit (Ident "Exp")) (TLit (Ident "Val")))) (EVar (Ident "$19cxt"),TLit (Ident "Cxt")),TFun (TLit (Ident "Exp")) (TLit (Ident "Val"))) (EVar (Ident "$36e2"),TLit (Ident "Exp")),TLit (Ident "Val")) [Branch (PInj (Ident "VInt$Val") [(PVar (Ident "$40i"),TLit (Ident "Int"))],TLit (Ident "Val")) (EVar (Ident "$40i"),TLit (Ident "Int"))],TLit (Ident "Int"))) (EApp (EVar (Ident "VInt$Val"),TFun (TLit (Ident "Int")) (TLit (Ident "Val"))) (EApp (EApp (EVar (Ident "$plus$$Int_Int_Int"),TFun (TLit (Ident "Int")) (TFun (TLit (Ident "Int")) (TLit (Ident "Int")))) (EVar (Ident "$37i1"),TLit (Ident "Int")),TFun (TLit (Ident "Int")) (TLit (Ident "Int"))) (EVar (Ident "$39i2"),TLit (Ident "Int")),TLit (Ident "Int")),TLit (Ident "Val")),TLit (Ident "Val")),TLit (Ident "Val"))]
define fastcc %Val @eval$Cxt_Exp_Val(ptr %cxt, %Cxt %$19cxt, %Exp %$20exp) {
	%1 = alloca %Val 
	; Inj
	%2 = extractvalue %Exp %$20exp, 0
	%3 = icmp eq i8 %2, 4
	br i1 %3, label %lbl_success_23, label %lbl_failed_22
	
lbl_success_23:
	%4 = alloca %Exp 
	store %Exp %$20exp, ptr %4
	%5 = load %EVar$Exp, ptr %4
	; ident i8
	%$21x = extractvalue %EVar$Exp %5, 1
	; CustomType (Ident "Val")
	%6 = call fastcc %Val @lookup$Char_Cxt_Val(ptr null, i8 %$21x, %Cxt %$19cxt)
	%7 = alloca %Val 
	; Inj
	%8 = extractvalue %Val %6, 0
	%9 = icmp eq i8 %8, 1
	br i1 %9, label %lbl_success_26, label %lbl_failed_25
	
lbl_success_26:
	%10 = alloca %Val 
	store %Val %6, ptr %10
	%11 = load %VInt$Val, ptr %10
	; ident i64
	%$22i = extractvalue %VInt$Val %11, 1
	; CustomType (Ident "Val")
	%12 = call fastcc %Val @VInt$Val(ptr null, i64 %$22i)
	store %Val %12, ptr %7
	br label %lbl_escape_24
	
lbl_failed_25:
	; Inj
	%13 = extractvalue %Val %6, 0
	%14 = icmp eq i8 %13, 0
	br i1 %14, label %lbl_success_28, label %lbl_failed_27
	
lbl_success_28:
	%15 = alloca %Val 
	store %Val %6, ptr %15
	%16 = load %VClosure$Val, ptr %15
	; ident %Cxt
	%17 = extractvalue %VClosure$Val %16, 1
	%$23delta = load %Cxt, ptr %17
	; ident i8
	%$24x = extractvalue %VClosure$Val %16, 2
	; ident %Exp
	%18 = extractvalue %VClosure$Val %16, 3
	%$25e = load %Exp, ptr %18
	; CustomType (Ident "Val")
	%19 = call fastcc %Val @eval$Cxt_Exp_Val(ptr null, %Cxt %$23delta, %Exp %$25e)
	store %Val %19, ptr %7
	br label %lbl_escape_24
	
lbl_failed_27:
	call i32 (ptr, ...) @printf(ptr noundef @.non_exhaustive_patterns, i64 noundef 19, i64 noundef 6)
	call void @cheap_dispose()
	call i32 @exit(i32 noundef 1)
	br label %lbl_escape_24
	
lbl_escape_24:
	%22 = load %Val, ptr %7
	store %Val %22, ptr %1
	br label %lbl_escape_21
	
lbl_failed_22:
	; Inj
	%23 = extractvalue %Exp %$20exp, 0
	%24 = icmp eq i8 %23, 0
	br i1 %24, label %lbl_success_30, label %lbl_failed_29
	
lbl_success_30:
	%25 = alloca %Exp 
	store %Exp %$20exp, ptr %25
	%26 = load %EAbs$Exp, ptr %25
	; ident i8
	%$26x = extractvalue %EAbs$Exp %26, 1
	; ident %Exp
	%27 = extractvalue %EAbs$Exp %26, 2
	%$27e = load %Exp, ptr %27
	; CustomType (Ident "Val")
	%28 = call fastcc %Val @VClosure$Val(ptr null, %Cxt %$19cxt, i8 %$26x, %Exp %$27e)
	store %Val %28, ptr %1
	br label %lbl_escape_21
	
lbl_failed_29:
	; Inj
	%29 = extractvalue %Exp %$20exp, 0
	%30 = icmp eq i8 %29, 2
	br i1 %30, label %lbl_success_32, label %lbl_failed_31
	
lbl_success_32:
	%31 = alloca %Exp 
	store %Exp %$20exp, ptr %31
	%32 = load %EApp$Exp, ptr %31
	; ident %Exp
	%33 = extractvalue %EApp$Exp %32, 1
	%$28e1 = load %Exp, ptr %33
	; ident %Exp
	%34 = extractvalue %EApp$Exp %32, 2
	%$29e2 = load %Exp, ptr %34
	; CustomType (Ident "Val")
	%35 = call fastcc %Val @eval$Cxt_Exp_Val(ptr null, %Cxt %$19cxt, %Exp %$28e1)
	%36 = alloca %Val 
	; Inj
	%37 = extractvalue %Val %35, 0
	%38 = icmp eq i8 %37, 0
	br i1 %38, label %lbl_success_35, label %lbl_failed_34
	
lbl_success_35:
	%39 = alloca %Val 
	store %Val %35, ptr %39
	%40 = load %VClosure$Val, ptr %39
	; ident %Cxt
	%41 = extractvalue %VClosure$Val %40, 1
	%$30delta = load %Cxt, ptr %41
	; ident i8
	%$31x = extractvalue %VClosure$Val %40, 2
	; ident %Exp
	%42 = extractvalue %VClosure$Val %40, 3
	%$32f = load %Exp, ptr %42
	; CustomType (Ident "Val")
	%43 = call fastcc %Val @VClosure$Val(ptr null, %Cxt %$19cxt, i8 %$31x, %Exp %$29e2)
	%44 = alloca %Val 
	store %Val %43, ptr %44
	%$33v = load %Val, ptr %44
	; CustomType (Ident "Cxt")
	%45 = call fastcc %Cxt @insert$Char_Val_Cxt_Cxt(ptr null, i8 %$31x, %Val %$33v, %Cxt %$30delta)
	; CustomType (Ident "Val")
	%46 = call fastcc %Val @eval$Cxt_Exp_Val(ptr null, %Cxt %45, %Exp %$32f)
	store %Val %46, ptr %36
	br label %lbl_escape_33
	
lbl_failed_34:
	call i32 (ptr, ...) @printf(ptr noundef @.non_exhaustive_patterns, i64 noundef 46, i64 noundef 6)
	call void @cheap_dispose()
	call i32 @exit(i32 noundef 1)
	br label %lbl_escape_33
	
lbl_escape_33:
	%49 = load %Val, ptr %36
	store %Val %49, ptr %1
	br label %lbl_escape_21
	
lbl_failed_31:
	; Inj
	%50 = extractvalue %Exp %$20exp, 0
	%51 = icmp eq i8 %50, 3
	br i1 %51, label %lbl_success_37, label %lbl_failed_36
	
lbl_success_37:
	%52 = alloca %Exp 
	store %Exp %$20exp, ptr %52
	%53 = load %EInt$Exp, ptr %52
	; ident i64
	%$34i = extractvalue %EInt$Exp %53, 1
	; CustomType (Ident "Val")
	%54 = call fastcc %Val @VInt$Val(ptr null, i64 %$34i)
	store %Val %54, ptr %1
	br label %lbl_escape_21
	
lbl_failed_36:
	; Inj
	%55 = extractvalue %Exp %$20exp, 0
	%56 = icmp eq i8 %55, 1
	br i1 %56, label %lbl_success_39, label %lbl_failed_38
	
lbl_success_39:
	%57 = alloca %Exp 
	store %Exp %$20exp, ptr %57
	%58 = load %EAdd$Exp, ptr %57
	; ident %Exp
	%59 = extractvalue %EAdd$Exp %58, 1
	%$35e1 = load %Exp, ptr %59
	; ident %Exp
	%60 = extractvalue %EAdd$Exp %58, 2
	%$36e2 = load %Exp, ptr %60
	; CustomType (Ident "Val")
	%61 = call fastcc %Val @eval$Cxt_Exp_Val(ptr null, %Cxt %$19cxt, %Exp %$35e1)
	%62 = alloca i64 
	; Inj
	%63 = extractvalue %Val %61, 0
	%64 = icmp eq i8 %63, 1
	br i1 %64, label %lbl_success_42, label %lbl_failed_41
	
lbl_success_42:
	%65 = alloca %Val 
	store %Val %61, ptr %65
	%66 = load %VInt$Val, ptr %65
	; ident i64
	%$38i = extractvalue %VInt$Val %66, 1
	store i64 %$38i, ptr %62
	br label %lbl_escape_40
	
lbl_failed_41:
	call i32 (ptr, ...) @printf(ptr noundef @.non_exhaustive_patterns, i64 noundef 66, i64 noundef 6)
	call void @cheap_dispose()
	call i32 @exit(i32 noundef 1)
	br label %lbl_escape_40
	
lbl_escape_40:
	%69 = load i64, ptr %62
	%70 = alloca i64 
	store i64 %69, ptr %70
	%$37i1 = load i64, ptr %70
	; CustomType (Ident "Val")
	%71 = call fastcc %Val @eval$Cxt_Exp_Val(ptr null, %Cxt %$19cxt, %Exp %$36e2)
	%72 = alloca i64 
	; Inj
	%73 = extractvalue %Val %71, 0
	%74 = icmp eq i8 %73, 1
	br i1 %74, label %lbl_success_45, label %lbl_failed_44
	
lbl_success_45:
	%75 = alloca %Val 
	store %Val %71, ptr %75
	%76 = load %VInt$Val, ptr %75
	; ident i64
	%$40i = extractvalue %VInt$Val %76, 1
	store i64 %$40i, ptr %72
	br label %lbl_escape_43
	
lbl_failed_44:
	call i32 (ptr, ...) @printf(ptr noundef @.non_exhaustive_patterns, i64 noundef 76, i64 noundef 6)
	call void @cheap_dispose()
	call i32 @exit(i32 noundef 1)
	br label %lbl_escape_43
	
lbl_escape_43:
	%79 = load i64, ptr %72
	%80 = alloca i64 
	store i64 %79, ptr %80
	%$39i2 = load i64, ptr %80
	; I64
	%81 = add i64 %$37i1, %$39i2
	; CustomType (Ident "Val")
	%82 = call fastcc %Val @VInt$Val(ptr null, i64 %81)
	store %Val %82, ptr %1
	br label %lbl_escape_21
	
lbl_failed_38:
	call i32 (ptr, ...) @printf(ptr noundef @.non_exhaustive_patterns, i64 noundef 82, i64 noundef 6)
	call void @cheap_dispose()
	call i32 @exit(i32 noundef 1)
	br label %lbl_escape_21
	
lbl_escape_21:
	%85 = load %Val, ptr %1
	ret %Val %85
}

; Ident "asciiCode$Char_Int": ECase (EVar (Ident "$59x"),TLit (Ident "Char")) [Branch (PLit (LChar 'a'),TLit (Ident "Char")) (ELit (LInt 97),TLit (Ident "Int")),Branch (PLit (LChar 'b'),TLit (Ident "Char")) (ELit (LInt 98),TLit (Ident "Int")),Branch (PLit (LChar 'c'),TLit (Ident "Char")) (ELit (LInt 99),TLit (Ident "Int")),Branch (PLit (LChar 'd'),TLit (Ident "Char")) (ELit (LInt 100),TLit (Ident "Int")),Branch (PLit (LChar 'e'),TLit (Ident "Char")) (ELit (LInt 101),TLit (Ident "Int")),Branch (PLit (LChar 'f'),TLit (Ident "Char")) (ELit (LInt 102),TLit (Ident "Int")),Branch (PLit (LChar 'g'),TLit (Ident "Char")) (ELit (LInt 103),TLit (Ident "Int")),Branch (PLit (LChar 'h'),TLit (Ident "Char")) (ELit (LInt 104),TLit (Ident "Int")),Branch (PLit (LChar 'i'),TLit (Ident "Char")) (ELit (LInt 105),TLit (Ident "Int")),Branch (PLit (LChar 'j'),TLit (Ident "Char")) (ELit (LInt 106),TLit (Ident "Int")),Branch (PLit (LChar 'k'),TLit (Ident "Char")) (ELit (LInt 107),TLit (Ident "Int")),Branch (PLit (LChar 'l'),TLit (Ident "Char")) (ELit (LInt 108),TLit (Ident "Int")),Branch (PLit (LChar 'm'),TLit (Ident "Char")) (ELit (LInt 109),TLit (Ident "Int")),Branch (PLit (LChar 'n'),TLit (Ident "Char")) (ELit (LInt 110),TLit (Ident "Int")),Branch (PLit (LChar 'o'),TLit (Ident "Char")) (ELit (LInt 111),TLit (Ident "Int")),Branch (PLit (LChar 'p'),TLit (Ident "Char")) (ELit (LInt 112),TLit (Ident "Int")),Branch (PLit (LChar 'q'),TLit (Ident "Char")) (ELit (LInt 113),TLit (Ident "Int")),Branch (PLit (LChar 's'),TLit (Ident "Char")) (ELit (LInt 114),TLit (Ident "Int")),Branch (PLit (LChar 't'),TLit (Ident "Char")) (ELit (LInt 115),TLit (Ident "Int")),Branch (PLit (LChar 'u'),TLit (Ident "Char")) (ELit (LInt 116),TLit (Ident "Int")),Branch (PLit (LChar 'v'),TLit (Ident "Char")) (ELit (LInt 117),TLit (Ident "Int")),Branch (PLit (LChar 'w'),TLit (Ident "Char")) (ELit (LInt 118),TLit (Ident "Int")),Branch (PLit (LChar 'x'),TLit (Ident "Char")) (ELit (LInt 119),TLit (Ident "Int")),Branch (PLit (LChar 'y'),TLit (Ident "Char")) (ELit (LInt 120),TLit (Ident "Int")),Branch (PLit (LChar 'z'),TLit (Ident "Char")) (ELit (LInt 121),TLit (Ident "Int"))]
define fastcc i64 @asciiCode$Char_Int(ptr %cxt, i8 %$59x) {
	%1 = alloca i64 
	; Plit
	%2 = icmp eq i8 %$59x, 97
	br i1 %2, label %lbl_success_48, label %lbl_failed_47
	
lbl_success_48:
	store i64 97, ptr %1
	br label %lbl_escape_46
	
lbl_failed_47:
	; Plit
	%3 = icmp eq i8 %$59x, 98
	br i1 %3, label %lbl_success_50, label %lbl_failed_49
	
lbl_success_50:
	store i64 98, ptr %1
	br label %lbl_escape_46
	
lbl_failed_49:
	; Plit
	%4 = icmp eq i8 %$59x, 99
	br i1 %4, label %lbl_success_52, label %lbl_failed_51
	
lbl_success_52:
	store i64 99, ptr %1
	br label %lbl_escape_46
	
lbl_failed_51:
	; Plit
	%5 = icmp eq i8 %$59x, 100
	br i1 %5, label %lbl_success_54, label %lbl_failed_53
	
lbl_success_54:
	store i64 100, ptr %1
	br label %lbl_escape_46
	
lbl_failed_53:
	; Plit
	%6 = icmp eq i8 %$59x, 101
	br i1 %6, label %lbl_success_56, label %lbl_failed_55
	
lbl_success_56:
	store i64 101, ptr %1
	br label %lbl_escape_46
	
lbl_failed_55:
	; Plit
	%7 = icmp eq i8 %$59x, 102
	br i1 %7, label %lbl_success_58, label %lbl_failed_57
	
lbl_success_58:
	store i64 102, ptr %1
	br label %lbl_escape_46
	
lbl_failed_57:
	; Plit
	%8 = icmp eq i8 %$59x, 103
	br i1 %8, label %lbl_success_60, label %lbl_failed_59
	
lbl_success_60:
	store i64 103, ptr %1
	br label %lbl_escape_46
	
lbl_failed_59:
	; Plit
	%9 = icmp eq i8 %$59x, 104
	br i1 %9, label %lbl_success_62, label %lbl_failed_61
	
lbl_success_62:
	store i64 104, ptr %1
	br label %lbl_escape_46
	
lbl_failed_61:
	; Plit
	%10 = icmp eq i8 %$59x, 105
	br i1 %10, label %lbl_success_64, label %lbl_failed_63
	
lbl_success_64:
	store i64 105, ptr %1
	br label %lbl_escape_46
	
lbl_failed_63:
	; Plit
	%11 = icmp eq i8 %$59x, 106
	br i1 %11, label %lbl_success_66, label %lbl_failed_65
	
lbl_success_66:
	store i64 106, ptr %1
	br label %lbl_escape_46
	
lbl_failed_65:
	; Plit
	%12 = icmp eq i8 %$59x, 107
	br i1 %12, label %lbl_success_68, label %lbl_failed_67
	
lbl_success_68:
	store i64 107, ptr %1
	br label %lbl_escape_46
	
lbl_failed_67:
	; Plit
	%13 = icmp eq i8 %$59x, 108
	br i1 %13, label %lbl_success_70, label %lbl_failed_69
	
lbl_success_70:
	store i64 108, ptr %1
	br label %lbl_escape_46
	
lbl_failed_69:
	; Plit
	%14 = icmp eq i8 %$59x, 109
	br i1 %14, label %lbl_success_72, label %lbl_failed_71
	
lbl_success_72:
	store i64 109, ptr %1
	br label %lbl_escape_46
	
lbl_failed_71:
	; Plit
	%15 = icmp eq i8 %$59x, 110
	br i1 %15, label %lbl_success_74, label %lbl_failed_73
	
lbl_success_74:
	store i64 110, ptr %1
	br label %lbl_escape_46
	
lbl_failed_73:
	; Plit
	%16 = icmp eq i8 %$59x, 111
	br i1 %16, label %lbl_success_76, label %lbl_failed_75
	
lbl_success_76:
	store i64 111, ptr %1
	br label %lbl_escape_46
	
lbl_failed_75:
	; Plit
	%17 = icmp eq i8 %$59x, 112
	br i1 %17, label %lbl_success_78, label %lbl_failed_77
	
lbl_success_78:
	store i64 112, ptr %1
	br label %lbl_escape_46
	
lbl_failed_77:
	; Plit
	%18 = icmp eq i8 %$59x, 113
	br i1 %18, label %lbl_success_80, label %lbl_failed_79
	
lbl_success_80:
	store i64 113, ptr %1
	br label %lbl_escape_46
	
lbl_failed_79:
	; Plit
	%19 = icmp eq i8 %$59x, 115
	br i1 %19, label %lbl_success_82, label %lbl_failed_81
	
lbl_success_82:
	store i64 114, ptr %1
	br label %lbl_escape_46
	
lbl_failed_81:
	; Plit
	%20 = icmp eq i8 %$59x, 116
	br i1 %20, label %lbl_success_84, label %lbl_failed_83
	
lbl_success_84:
	store i64 115, ptr %1
	br label %lbl_escape_46
	
lbl_failed_83:
	; Plit
	%21 = icmp eq i8 %$59x, 117
	br i1 %21, label %lbl_success_86, label %lbl_failed_85
	
lbl_success_86:
	store i64 116, ptr %1
	br label %lbl_escape_46
	
lbl_failed_85:
	; Plit
	%22 = icmp eq i8 %$59x, 118
	br i1 %22, label %lbl_success_88, label %lbl_failed_87
	
lbl_success_88:
	store i64 117, ptr %1
	br label %lbl_escape_46
	
lbl_failed_87:
	; Plit
	%23 = icmp eq i8 %$59x, 119
	br i1 %23, label %lbl_success_90, label %lbl_failed_89
	
lbl_success_90:
	store i64 118, ptr %1
	br label %lbl_escape_46
	
lbl_failed_89:
	; Plit
	%24 = icmp eq i8 %$59x, 120
	br i1 %24, label %lbl_success_92, label %lbl_failed_91
	
lbl_success_92:
	store i64 119, ptr %1
	br label %lbl_escape_46
	
lbl_failed_91:
	; Plit
	%25 = icmp eq i8 %$59x, 121
	br i1 %25, label %lbl_success_94, label %lbl_failed_93
	
lbl_success_94:
	store i64 120, ptr %1
	br label %lbl_escape_46
	
lbl_failed_93:
	; Plit
	%26 = icmp eq i8 %$59x, 122
	br i1 %26, label %lbl_success_96, label %lbl_failed_95
	
lbl_success_96:
	store i64 121, ptr %1
	br label %lbl_escape_46
	
lbl_failed_95:
	call i32 (ptr, ...) @printf(ptr noundef @.non_exhaustive_patterns, i64 noundef 26, i64 noundef 6)
	call void @cheap_dispose()
	call i32 @exit(i32 noundef 1)
	br label %lbl_escape_46
	
lbl_escape_46:
	%29 = load i64, ptr %1
	ret i64 %29
}

; Ident "$plus$$Int_Int_Int": ELit (LInt 0)
define fastcc i64 @$plus$$Int_Int_Int(ptr %cxt, i64 %$46x, i64 %$47y) {
	ret i64 0
}

; Ident "$langle$$Int_Int_Bool": ECase (EVar (Ident "$42x"),TLit (Ident "Int")) [Branch (PCatch,TLit (Ident "Int")) (EVar (Ident "True$Bool"),TLit (Ident "Bool")),Branch (PCatch,TLit (Ident "Int")) (EVar (Ident "False$Bool"),TLit (Ident "Bool"))]
define fastcc i1 @$langle$$Int_Int_Bool(ptr %cxt, i64 %$42x, i64 %$43y) {
	%1 = alloca i1 
	; Pcatch
	store i1 1, ptr %1
	br label %lbl_escape_97
	
lbl_failed_98:
	; Pcatch
	store i1 0, ptr %1
	br label %lbl_escape_97
	
lbl_failed_99:
	call i32 (ptr, ...) @printf(ptr noundef @.non_exhaustive_patterns, i64 noundef 1, i64 noundef 6)
	call void @cheap_dispose()
	call i32 @exit(i32 noundef 1)
	br label %lbl_escape_97
	
lbl_escape_97:
	%4 = load i1, ptr %1
	ret i1 %4
}

; Ident "$equals$$equals$$Int_Int_Bool": ECase (EApp (EApp (EVar (Ident "$langle$$Int_Int_Bool"),TFun (TLit (Ident "Int")) (TFun (TLit (Ident "Int")) (TLit (Ident "Bool")))) (EVar (Ident "$48a"),TLit (Ident "Int")),TFun (TLit (Ident "Int")) (TLit (Ident "Bool"))) (EVar (Ident "$49b"),TLit (Ident "Int")),TLit (Ident "Bool")) [Branch (PEnum (Ident "False$Bool"),TLit (Ident "Bool")) (ECase (EApp (EApp (EVar (Ident "$langle$$Int_Int_Bool"),TFun (TLit (Ident "Int")) (TFun (TLit (Ident "Int")) (TLit (Ident "Bool")))) (EVar (Ident "$49b"),TLit (Ident "Int")),TFun (TLit (Ident "Int")) (TLit (Ident "Bool"))) (EVar (Ident "$48a"),TLit (Ident "Int")),TLit (Ident "Bool")) [Branch (PEnum (Ident "False$Bool"),TLit (Ident "Bool")) (EVar (Ident "True$Bool"),TLit (Ident "Bool")),Branch (PEnum (Ident "True$Bool"),TLit (Ident "Bool")) (EVar (Ident "False$Bool"),TLit (Ident "Bool"))],TLit (Ident "Bool")),Branch (PEnum (Ident "True$Bool"),TLit (Ident "Bool")) (EVar (Ident "False$Bool"),TLit (Ident "Bool"))]
define fastcc i1 @$equals$$equals$$Int_Int_Bool(ptr %cxt, i64 %$48a, i64 %$49b) {
	; I1
	%1 = icmp slt i64 %$48a, %$49b
	%2 = alloca i1 
	; Plit
	%3 = icmp eq i1 %1, 0
	br i1 %3, label %lbl_success_102, label %lbl_failed_101
	
lbl_success_102:
	; I1
	%4 = icmp slt i64 %$49b, %$48a
	%5 = alloca i1 
	; Plit
	%6 = icmp eq i1 %4, 0
	br i1 %6, label %lbl_success_105, label %lbl_failed_104
	
lbl_success_105:
	store i1 1, ptr %5
	br label %lbl_escape_103
	
lbl_failed_104:
	; Plit
	%7 = icmp eq i1 %4, 1
	br i1 %7, label %lbl_success_107, label %lbl_failed_106
	
lbl_success_107:
	store i1 0, ptr %5
	br label %lbl_escape_103
	
lbl_failed_106:
	call i32 (ptr, ...) @printf(ptr noundef @.non_exhaustive_patterns, i64 noundef 7, i64 noundef 6)
	call void @cheap_dispose()
	call i32 @exit(i32 noundef 1)
	br label %lbl_escape_103
	
lbl_escape_103:
	%10 = load i1, ptr %5
	store i1 %10, ptr %2
	br label %lbl_escape_100
	
lbl_failed_101:
	; Plit
	%11 = icmp eq i1 %1, 1
	br i1 %11, label %lbl_success_109, label %lbl_failed_108
	
lbl_success_109:
	store i1 0, ptr %2
	br label %lbl_escape_100
	
lbl_failed_108:
	call i32 (ptr, ...) @printf(ptr noundef @.non_exhaustive_patterns, i64 noundef 11, i64 noundef 6)
	call void @cheap_dispose()
	call i32 @exit(i32 noundef 1)
	br label %lbl_escape_100
	
lbl_escape_100:
	%14 = load i1, ptr %2
	ret i1 %14
}

define fastcc %ListPairCharVal @Cons$List.Pair.Char.Val(ptr %cxt, %PairCharVal %arg_0, %ListPairCharVal %arg_1) {
	%1 = alloca %ListPairCharVal 
	%2 = getelementptr %ListPairCharVal, %ListPairCharVal* %1, i64 0, i32 0
	store i8 0, i8* %2
	%3 = bitcast %ListPairCharVal* %1 to %Cons$List.Pair.Char.Val*
	; %PairCharVal arg_0 1
	%4 = getelementptr %Cons$List.Pair.Char.Val, %Cons$List.Pair.Char.Val* %3, i64 0, i32 1
	; Malloc and store
	%5 = call ptr @cheap_alloc(i64 24)
	store %PairCharVal %arg_0, ptr %5
	store %PairCharVal* %5, ptr %4
	; %ListPairCharVal arg_1 2
	%6 = getelementptr %Cons$List.Pair.Char.Val, %Cons$List.Pair.Char.Val* %3, i64 0, i32 2
	; Malloc and store
	%7 = call ptr @cheap_alloc(i64 24)
	store %ListPairCharVal %arg_1, ptr %7
	store %ListPairCharVal* %7, ptr %6
	; Return the newly constructed value
	%8 = load %ListPairCharVal, ptr %1
	ret %ListPairCharVal %8
}

define fastcc %Cxt @Cxt$Cxt(ptr %cxt, %ListPairCharVal %arg_0) {
	%1 = alloca %Cxt 
	%2 = getelementptr %Cxt, %Cxt* %1, i64 0, i32 0
	store i8 0, i8* %2
	%3 = bitcast %Cxt* %1 to %Cxt$Cxt*
	; %ListPairCharVal arg_0 1
	%4 = getelementptr %Cxt$Cxt, %Cxt$Cxt* %3, i64 0, i32 1
	; Malloc and store
	%5 = call ptr @cheap_alloc(i64 24)
	store %ListPairCharVal %arg_0, ptr %5
	store %ListPairCharVal* %5, ptr %4
	; Return the newly constructed value
	%6 = load %Cxt, ptr %1
	ret %Cxt %6
}

define fastcc %Exp @EAbs$Exp(ptr %cxt, i8 %arg_0, %Exp %arg_1) {
	%1 = alloca %Exp 
	%2 = getelementptr %Exp, %Exp* %1, i64 0, i32 0
	store i8 0, i8* %2
	%3 = bitcast %Exp* %1 to %EAbs$Exp*
	; i8 arg_0 1
	%4 = getelementptr %EAbs$Exp, %EAbs$Exp* %3, i64 0, i32 1
	; Just store
	store i8 %arg_0, ptr %4
	; %Exp arg_1 2
	%5 = getelementptr %EAbs$Exp, %EAbs$Exp* %3, i64 0, i32 2
	; Malloc and store
	%6 = call ptr @cheap_alloc(i64 24)
	store %Exp %arg_1, ptr %6
	store %Exp* %6, ptr %5
	; Return the newly constructed value
	%7 = load %Exp, ptr %1
	ret %Exp %7
}

define fastcc %Exp @EAdd$Exp(ptr %cxt, %Exp %arg_0, %Exp %arg_1) {
	%1 = alloca %Exp 
	%2 = getelementptr %Exp, %Exp* %1, i64 0, i32 0
	store i8 1, i8* %2
	%3 = bitcast %Exp* %1 to %EAdd$Exp*
	; %Exp arg_0 1
	%4 = getelementptr %EAdd$Exp, %EAdd$Exp* %3, i64 0, i32 1
	; Malloc and store
	%5 = call ptr @cheap_alloc(i64 24)
	store %Exp %arg_0, ptr %5
	store %Exp* %5, ptr %4
	; %Exp arg_1 2
	%6 = getelementptr %EAdd$Exp, %EAdd$Exp* %3, i64 0, i32 2
	; Malloc and store
	%7 = call ptr @cheap_alloc(i64 24)
	store %Exp %arg_1, ptr %7
	store %Exp* %7, ptr %6
	; Return the newly constructed value
	%8 = load %Exp, ptr %1
	ret %Exp %8
}

define fastcc %Exp @EApp$Exp(ptr %cxt, %Exp %arg_0, %Exp %arg_1) {
	%1 = alloca %Exp 
	%2 = getelementptr %Exp, %Exp* %1, i64 0, i32 0
	store i8 2, i8* %2
	%3 = bitcast %Exp* %1 to %EApp$Exp*
	; %Exp arg_0 1
	%4 = getelementptr %EApp$Exp, %EApp$Exp* %3, i64 0, i32 1
	; Malloc and store
	%5 = call ptr @cheap_alloc(i64 24)
	store %Exp %arg_0, ptr %5
	store %Exp* %5, ptr %4
	; %Exp arg_1 2
	%6 = getelementptr %EApp$Exp, %EApp$Exp* %3, i64 0, i32 2
	; Malloc and store
	%7 = call ptr @cheap_alloc(i64 24)
	store %Exp %arg_1, ptr %7
	store %Exp* %7, ptr %6
	; Return the newly constructed value
	%8 = load %Exp, ptr %1
	ret %Exp %8
}

define fastcc %Exp @EInt$Exp(ptr %cxt, i64 %arg_0) {
	%1 = alloca %Exp 
	%2 = getelementptr %Exp, %Exp* %1, i64 0, i32 0
	store i8 3, i8* %2
	%3 = bitcast %Exp* %1 to %EInt$Exp*
	; i64 arg_0 1
	%4 = getelementptr %EInt$Exp, %EInt$Exp* %3, i64 0, i32 1
	; Just store
	store i64 %arg_0, ptr %4
	; Return the newly constructed value
	%5 = load %Exp, ptr %1
	ret %Exp %5
}

define fastcc %Exp @EVar$Exp(ptr %cxt, i8 %arg_0) {
	%1 = alloca %Exp 
	%2 = getelementptr %Exp, %Exp* %1, i64 0, i32 0
	store i8 4, i8* %2
	%3 = bitcast %Exp* %1 to %EVar$Exp*
	; i8 arg_0 1
	%4 = getelementptr %EVar$Exp, %EVar$Exp* %3, i64 0, i32 1
	; Just store
	store i8 %arg_0, ptr %4
	; Return the newly constructed value
	%5 = load %Exp, ptr %1
	ret %Exp %5
}

define fastcc %ListPairCharVal @Nil$List.Pair.Char.Val(ptr %cxt) {
	%1 = alloca %ListPairCharVal 
	%2 = getelementptr %ListPairCharVal, %ListPairCharVal* %1, i64 0, i32 0
	store i8 1, i8* %2
	%3 = bitcast %ListPairCharVal* %1 to %Nil$List.Pair.Char.Val*
	; Return the newly constructed value
	%4 = load %ListPairCharVal, ptr %1
	ret %ListPairCharVal %4
}

define fastcc %PairCharVal @Pair$Pair.Char.Val(ptr %cxt, i8 %arg_0, %Val %arg_1) {
	%1 = alloca %PairCharVal 
	%2 = getelementptr %PairCharVal, %PairCharVal* %1, i64 0, i32 0
	store i8 0, i8* %2
	%3 = bitcast %PairCharVal* %1 to %Pair$Pair.Char.Val*
	; i8 arg_0 1
	%4 = getelementptr %Pair$Pair.Char.Val, %Pair$Pair.Char.Val* %3, i64 0, i32 1
	; Just store
	store i8 %arg_0, ptr %4
	; %Val arg_1 2
	%5 = getelementptr %Pair$Pair.Char.Val, %Pair$Pair.Char.Val* %3, i64 0, i32 2
	; Malloc and store
	%6 = call ptr @cheap_alloc(i64 32)
	store %Val %arg_1, ptr %6
	store %Val* %6, ptr %5
	; Return the newly constructed value
	%7 = load %PairCharVal, ptr %1
	ret %PairCharVal %7
}

define fastcc %Val @VClosure$Val(ptr %cxt, %Cxt %arg_0, i8 %arg_1, %Exp %arg_2) {
	%1 = alloca %Val 
	%2 = getelementptr %Val, %Val* %1, i64 0, i32 0
	store i8 0, i8* %2
	%3 = bitcast %Val* %1 to %VClosure$Val*
	; %Cxt arg_0 1
	%4 = getelementptr %VClosure$Val, %VClosure$Val* %3, i64 0, i32 1
	; Malloc and store
	%5 = call ptr @cheap_alloc(i64 16)
	store %Cxt %arg_0, ptr %5
	store %Cxt* %5, ptr %4
	; i8 arg_1 2
	%6 = getelementptr %VClosure$Val, %VClosure$Val* %3, i64 0, i32 2
	; Just store
	store i8 %arg_1, ptr %6
	; %Exp arg_2 3
	%7 = getelementptr %VClosure$Val, %VClosure$Val* %3, i64 0, i32 3
	; Malloc and store
	%8 = call ptr @cheap_alloc(i64 24)
	store %Exp %arg_2, ptr %8
	store %Exp* %8, ptr %7
	; Return the newly constructed value
	%9 = load %Val, ptr %1
	ret %Val %9
}

define fastcc %Val @VInt$Val(ptr %cxt, i64 %arg_0) {
	%1 = alloca %Val 
	%2 = getelementptr %Val, %Val* %1, i64 0, i32 0
	store i8 1, i8* %2
	%3 = bitcast %Val* %1 to %VInt$Val*
	; i64 arg_0 1
	%4 = getelementptr %VInt$Val, %VInt$Val* %3, i64 0, i32 1
	; Just store
	store i64 %arg_0, ptr %4
	; Return the newly constructed value
	%5 = load %Val, ptr %1
	ret %Val %5
}

