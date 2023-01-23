; External declaration of the puts function
declare i32 @puts(i8* nocapture) nounwind
declare i64 @llvm.abs.i64(i64, i1 immarg) 

define [21 x i8] @i64ToString(i64 %val_org) {
    %val = alloca i64
    store i64 %val_org, i64* %val
    %val_copy = add i64 0, %val_org
    ; https://stackoverflow.com/a/7123710
    ; an algorithm for translating ints to strings
    ; s = ''
    ; sign = ''
    ; if i < 0:
    ;     sign = '-'
    ;     i = -i
    ; while True:
    ;     remainder = i % 10
    ;     i = i / 10
    ;     s = chr(ord('0') + remainder) + s
    ;     if i == 0:
    ;         break
    ; return sign + s

    ; allocate memory for the string, and store the temp variable into it
    %string_ptr = alloca [21 x i8] 
    store [21 x i8] c"\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00", ptr %string_ptr  

    ; create a pointer to the array
    %array_index_ptr = alloca i32
    store i32 0, i32* %array_index_ptr

    br label %while_point
    while_point:
        %val.tmp = load i64, i64* %val; load i64, i64* %val
        %val.1 = call i64 @llvm.abs.i64(i64 %val.tmp, i1 true)
        %tmp = load i32, ptr %array_index_ptr
        %array_pointer.1 = getelementptr [21 x i8], [21 x i8]* %string_ptr, i32 0, i32 %tmp
        %array_index_ptr.1 = add i32 %tmp, 1
        store i32 %array_index_ptr.1, ptr %array_index_ptr

        ; this should not work, but it does
        %remainder = srem i64 %val.1, 10
        %remainder_tmp = alloca i64
        store i64 %remainder, i64* %remainder_tmp
        %remainder_tmp8 = bitcast i64* %remainder_tmp to i8*
        %remainder8 = load i8, i8* %remainder_tmp8
        
        %i = sdiv i64 %val.1, 10
        %char = add i8 48, %remainder8
        store i8 %char, i8* %array_pointer.1 ; update string!
        store i64 %i, i64* %val
        %while_condition = icmp eq i64 %i, 0

        br i1 %while_condition, label %while_break, label %while_point
    while_break:

    ; get last_pointer
    %array_index_ptr.0 = load i32, i32* %array_index_ptr
    %array_pointer.0 = getelementptr [21 x i8], [21 x i8]* %string_ptr, i32 0, i32 %array_index_ptr.0

    ; check if p is below 0
    %condition = icmp slt i64 %val_copy, 0
    br i1 %condition, label %negative_check_true, label %negative_check_false
    negative_check_true:
        store i8 45, i8* %array_pointer.0
        br label %negative_check_done
    negative_check_false:
        ; store i8 43, i8* %array_pointer.0
        br label %negative_check_done
    negative_check_done:
    %noop = add i32 0, 0

    ; load the result and return it
    %res = load [21 x i8],[21 x i8]* %string_ptr
    ret [21 x i8] %res
}

; Definition of main function
define i32 @main() { ; i32()*
    ; %val = add i64 -123456789, 0
    %val = add i64 -1234, 0

    %print_res = call [21 x i8] @i64ToString(i64 %val)
    %ptr = alloca [21 x i8]
    store [21 x i8] %print_res, ptr %ptr  

    call i32 @puts(i8* %ptr)

    ret i32 0
}
