@.str = private unnamed_addr constant [3 x i8] c"%i
", align 1
declare i32 @printf(ptr noalias nocapture, ...)

; Ident "main": EAdd (TMono (Ident "Int")) (ELit (TMono (Ident "Int")) (LInt 3)) (EApp (TMono (Ident "Int")) (EId (Ident "sc_0",TArr (TPol (Ident "t1")) (TPol (Ident "t1")))) (ELit (TMono (Ident "Int")) (LInt 3)))
define i64 @main() {
	%1 = call i64 @sc_0(i64 3)
	%2 = add i64 3, %1
	call i32 (ptr, ...) @printf(ptr noundef @.str, i64 noundef %2)
	ret i64 0
}

; Ident "sc_0": EId (Ident "x_0",TPol (Ident "t1"))
define "TPol (Ident "t1")" @sc_0("TPol (Ident "t1")" %x_0) {
	ret i64 %x_0
}
