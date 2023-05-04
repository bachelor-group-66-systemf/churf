target triple = "x86_64-pc-linux-gnu"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
@.str = private unnamed_addr constant [3 x i8] c"%i
", align 1
@.non_exhaustive_patterns = private unnamed_addr constant [41 x i8] c"Non-exhaustive patterns in case at %i:%i
"
declare i32 @printf(ptr noalias nocapture, ...)
declare i32 @exit(i32 noundef)
declare ptr @malloc(i32 noundef)
%List = type { i8, [23 x i8] }
%Cons = type { i8, i64, %List* }
%Nil = type { i8 }
; NYTT: kontexttyp
%Closure_sc_0 = type { i64 (i64)*, i64 } 

; Ident "sum$List_Int": (ECase (EVar (Ident "$4xs"),TLit (Ident "List")) [Branch (PEnum (Ident "Nil"),TLit (Ident "List")) (ELit (LInt 0),TLit (Ident "Int")),Branch (PInj (Ident "Cons") [PVar (Ident "$5x",TLit (Ident "Int")),PVar (Ident "$6xs",TLit (Ident "List"))],TLit (Ident "List")) (EAdd (EVar (Ident "$5x"),TLit (Ident "Int")) (EApp (EVar (Ident "sum$List_Int"),TFun (TLit (Ident "List")) (TLit (Ident "Int"))) (EVar (Ident "$6xs"),TLit (Ident "List")),TLit (Ident "Int")),TLit (Ident "Int"))],TLit (Ident "Int"))
define fastcc i64 @sum$List_Int(%List %$4xs) {
	%1 = alloca i64 
	; Penum
	%2 = extractvalue %List %$4xs, 0
	%3 = icmp eq i8 %2, 1
	br i1 %3, label %lbl_success_3, label %lbl_failed_2
	
lbl_success_3:
	%4 = alloca %List 
	store %List %$4xs, ptr %4
	%5 = load %Nil, ptr %4
	store i64 0, ptr %1
	br label %lbl_escape_1
	
lbl_failed_2:
	; Inj
	%6 = extractvalue %List %$4xs, 0
	%7 = icmp eq i8 %6, 0
	br i1 %7, label %lbl_success_5, label %lbl_failed_4
	
lbl_success_5:
	%8 = alloca %List 
	store %List %$4xs, ptr %8
	%9 = load %Cons, ptr %8
	; ident i64
	%$5x = extractvalue %Cons %9, 1
	; ident %List
	%10 = extractvalue %Cons %9, 2
	%$6xs = load %List, ptr %10
	; TLit (Ident "Int")
	%11 = call fastcc i64 @sum$List_Int(%List %$6xs)
	%12 = add i64 %$5x, %11
	store i64 %12, ptr %1
	br label %lbl_escape_1
	
lbl_failed_4:
	call i32 (ptr, ...) @printf(ptr noundef @.non_exhaustive_patterns, i64 noundef 12, i64 noundef 6)
	call i32 @exit(i32 noundef 1)
	br label %lbl_escape_1
	
lbl_escape_1:
	%15 = load i64, ptr %1
	ret i64 %15
}

; Ident "sc_0$Int_Int": (EAdd (EVar (Ident "$7x"),TLit (Ident "Int")) (ELit (LInt 10),TLit (Ident "Int")),TLit (Ident "Int"))
; ÄNDRAT: lägg till kontextpekare
define fastcc i64 @sc_0$Int_Int(ptr %closure_sc_0, i64 %$7x) {
    ; NYTT: Ladda alla fria variabler
    %fri_variabel_ptr = getelementptr inbounds %Closure_sc_0, ptr %closure_sc_0, i32 0, i32 1
    %fri_variabel = load i64, ptr %fri_variabel_ptr
    ; ÄNDRAT: %fri_variabel istället för 2
	%1 = add i64 %$7x, %fri_variabel
	ret i64 %1
}

; Ident "map$Int_Int_List_List": (ECase (EVar (Ident "$1xs"),TLit (Ident "List")) [Branch (PEnum (Ident "Nil"),TLit (Ident "List")) (EVar (Ident "Nil"),TLit (Ident "List")),Branch (PInj (Ident "Cons") [PVar (Ident "$2x",TLit (Ident "Int")),PVar (Ident "$3xs",TLit (Ident "List"))],TLit (Ident "List")) (EApp (EApp (EVar (Ident "Cons"),TFun (TLit (Ident "Int")) (TFun (TLit (Ident "List")) (TLit (Ident "List")))) (EApp (EVar (Ident "$0f"),TFun (TLit (Ident "Int")) (TLit (Ident "Int"))) (EVar (Ident "$2x"),TLit (Ident "Int")),TLit (Ident "Int")),TFun (TLit (Ident "List")) (TLit (Ident "List"))) (EApp (EApp (EVar (Ident "map$Int_Int_List_List"),TFun (TFun (TLit (Ident "Int")) (TLit (Ident "Int"))) (TFun (TLit (Ident "List")) (TLit (Ident "List")))) (EVar (Ident "$0f"),TFun (TLit (Ident "Int")) (TLit (Ident "Int"))),TFun (TLit (Ident "List")) (TLit (Ident "List"))) (EVar (Ident "$3xs"),TLit (Ident "List")),TLit (Ident "List")),TLit (Ident "List"))],TLit (Ident "List"))
; ÄNDRAT: ptr istället för i64 (i64)* 
define fastcc %List @map$Int_Int_List_List(ptr %$0f, %List %$1xs) {
    ; NYTT: ta fram funktionspekaren
    %$0f_deref = load i64(i64)*, ptr %$0f

	%1 = alloca %List 
	; Penum
	%2 = extractvalue %List %$1xs, 0
	%3 = icmp eq i8 %2, 1
	br i1 %3, label %lbl_success_8, label %lbl_failed_7
	
lbl_success_8:
	%4 = alloca %List 
	store %List %$1xs, ptr %4
	%5 = load %Nil, ptr %4
	%6 = call fastcc %List @Nil()
	store %List %6, ptr %1
	br label %lbl_escape_6
	
lbl_failed_7:
	; Inj
	%7 = extractvalue %List %$1xs, 0
	%8 = icmp eq i8 %7, 0
	br i1 %8, label %lbl_success_10, label %lbl_failed_9
	
lbl_success_10:
	%9 = alloca %List 
	store %List %$1xs, ptr %9
	%10 = load %Cons, ptr %9
	; ident i64
	%$2x = extractvalue %Cons %10, 1
	; ident %List
	%11 = extractvalue %Cons %10, 2
	%$3xs = load %List, ptr %11
	; TLit (Ident "Int")
    ; ÄNDRAT använd deref
	%12 = call fastcc i64 %$0f_deref(ptr %$0f, i64 %$2x)
	; TLit (Ident "List")
    ; ÄNDRAT ptr istället för 64 (64)* och skicka med ptr
	%13 = call fastcc %List @map$Int_Int_List_List(ptr %$0f, %List %$3xs)
	; TLit (Ident "List")
	%14 = call fastcc %List @Cons(i64 %12, %List %13)
	store %List %14, ptr %1
	br label %lbl_escape_6
	
lbl_failed_9:
	call i32 (ptr, ...) @printf(ptr noundef @.non_exhaustive_patterns, i64 noundef 14, i64 noundef 6)
	call i32 @exit(i32 noundef 1)
	br label %lbl_escape_6
	
lbl_escape_6:
	%17 = load %List, ptr %1
	ret %List %17
}


; Ident "main": (EApp (EVar (Ident "sum$List_Int"),TFun (TLit (Ident "List")) (TLit (Ident "Int"))) (EApp (EApp (EVar (Ident "map$Int_Int_List_List"),TFun (TFun (TLit (Ident "Int")) (TLit (Ident "Int"))) (TFun (TLit (Ident "List")) (TLit (Ident "List")))) (EVar (Ident "sc_0$Int_Int"),TFun (TLit (Ident "Int")) (TLit (Ident "Int"))),TFun (TLit (Ident "List")) (TLit (Ident "List"))) (EApp (EApp (EVar (Ident "Cons"),TFun (TLit (Ident "Int")) (TFun (TLit (Ident "List")) (TLit (Ident "List")))) (ELit (LInt 1),TLit (Ident "Int")),TFun (TLit (Ident "List")) (TLit (Ident "List"))) (EApp (EApp (EVar (Ident "Cons"),TFun (TLit (Ident "Int")) (TFun (TLit (Ident "List")) (TLit (Ident "List")))) (ELit (LInt 2),TLit (Ident "Int")),TFun (TLit (Ident "List")) (TLit (Ident "List"))) (EVar (Ident "Nil"),TLit (Ident "List")),TLit (Ident "List")),TLit (Ident "List")),TLit (Ident "List")),TLit (Ident "Int"))
define fastcc i64 @main() {
	%1 = call fastcc %List @Nil()
	; TLit (Ident "List")
	%2 = call fastcc %List @Cons(i64 2, %List %1)
	; TLit (Ident "List")
	%3 = call fastcc %List @Cons(i64 1, %List %2)
	; TLit (Ident "List")

    ; NYTT: spara funktionspekaren och 100 i kontexten 
    %closure_sc_0 = alloca %Closure_sc_0
    store i64(i64)* @sc_0$Int_Int, ptr %closure_sc_0
    %fri_variabel_ptr = getelementptr inbounds %Closure_sc_0, ptr %closure_sc_0, i32 0, i32 1
    store i64 100, ptr %fri_variabel_ptr


    ; store %Closure_sc_0 {i64 (i64)* @sc_0$Int_Int, 100}, ptr %closure_sc_0

    ; ÄNDRAT ptr %closure_sc_0 istället för i64 (i64)* @sc_0$Int_Int
	%4 = call fastcc %List @map$Int_Int_List_List(ptr %closure_sc_0, %List %3)
	; TLit (Ident "Int")
	%5 = call fastcc i64 @sum$List_Int(%List %4)
	call i32 (ptr, ...) @printf(ptr noundef @.str, i64 noundef %5)
	ret i64 0
}

define fastcc %List @Cons(i64 %arg_0, %List %arg_1) {
	%1 = alloca %List 
	%2 = getelementptr %List, %List* %1, i64 0, i32 0
	store i8 0, i8* %2
	%3 = bitcast %List* %1 to %Cons*
	; i64 arg_0 1
	%4 = getelementptr %Cons, %Cons* %3, i64 0, i32 1
	; Just store
	store i64 %arg_0, ptr %4
	; %List arg_1 2
	%5 = getelementptr %Cons, %Cons* %3, i64 0, i32 2
	; Malloc and store
	%6 = call ptr @malloc(i64 24)
	store %List %arg_1, ptr %6
	store %List* %6, ptr %5
	; Return the newly constructed value
	%7 = load %List, ptr %1
	ret %List %7
}

define fastcc %List @Nil() {
	%1 = alloca %List 
	%2 = getelementptr %List, %List* %1, i64 0, i32 0
	store i8 1, i8* %2
	%3 = bitcast %List* %1 to %Nil*
	; Return the newly constructed value
	%4 = load %List, ptr %1
	ret %List %4
}

