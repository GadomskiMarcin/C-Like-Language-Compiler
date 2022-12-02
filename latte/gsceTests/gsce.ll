declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatString__(i8*, i8*)
declare i1 @__cmpString__(i8*, i8*)
declare i8* @__llvmMemcpy__(i32)


define i32 @main() {
L0:
  %r0 = call i32 @readInt()
  %r1 = mul i32 %r0, 2
  %r2 = add i32 3, %r0
  %r3 = call i32 @readInt()
  %r4 = mul i32 %r3, 100
  %r5 = icmp sgt i32 %r0, 10
  br i1 %r5, label %L1, label %L2
L1:
  br label %L3
L2:
  br label %L3
L3:
  %r11 = phi i32 [ %r1, %L1 ], [ %r2, %L2 ]
  call void @printInt(i32 %r4)
  call void @printInt(i32 %r11)
  ret i32 0
}

