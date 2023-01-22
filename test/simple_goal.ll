; External declaration of the puts function
declare i32 @puts(i8* nocapture) nounwind

define [22 x i8] @i64ToString(i64 %val) {
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
    %string_ptr = alloca [22 x i8] 
    store [22 x i8] c"Tjena banane hellosa\0A\00", ptr %string_ptr  

    ; create a pointer to the array
    %array_index_ptr = alloca i32
    store i32 0, ptr %array_index_ptr

    ; get the value of the first element in the array
    %array_index_ptr.0 = load i32, i32* %array_index_ptr
    %p = getelementptr [22 x i8], [22 x i8]* %string_ptr, i32 0, i32 %array_index_ptr.0
    
    ; check if p is below 0
    %condition = icmp slt i64 %val, 0
    br i1 %condition, label %negative_check_true, label %negative_check_false
    negative_check_true:
        store i8 45, i8* %p
        br label %negative_check_done
    negative_check_false:
        store i8 43, i8* %p
        br label %negative_check_done
    negative_check_done:

    ; iterate over the next nums
    

    ; load the result and return it
    %res = load [22 x i8],[22 x i8]* %string_ptr
    ret [22 x i8] %res
}

; Definition of main function
define i32 @main() { ; i32()*
    %val = add i64 1, 0

    %print_res = call [22 x i8] @i64ToString(i64 %val)
    %ptr = alloca [22 x i8]
    store [22 x i8] %print_res, ptr %ptr  

    call i32 @puts(i8* %ptr)

    ret i32 0
}
