; External declaration of the puts function
declare i32 @puts(i8* nocapture) nounwind
declare [21 x i8] @i64ToString(i64)

; Definition of main function
define i32 @main() { ; i32()*
    ; %val = add i64 -123456789, 0
    %val = add i64 -133780085, 0

    %print_res = call [21 x i8] @i64ToString(i64 %val)
    %ptr = alloca [21 x i8]
    store [21 x i8] %print_res, [21 x i8]* %ptr  

    %printable = bitcast [21 x i8]* %ptr to i8* 
    call i32 @puts(i8* %printable)

    ret i32 0
}
