@.str = private unnamed_addr constant [3 x i8] c"%i
", align 1
declare i32 @printf(ptr noalias nocapture, ...)

; Ident "main": EAdd (TMono (Ident "Int")) (ELit (TMono (Ident "Int")) (LInt 3)) (ELit (TMono (Ident "Int")) (LInt 3))
define i64 @main() {
	%1 = add i64 3, 3
	call i32 (ptr, ...) @printf(ptr noundef @.str, i64 noundef %1)
	ret i64 0
}
