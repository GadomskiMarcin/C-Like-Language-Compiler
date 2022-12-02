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
  %r1 = call i32 @readInt()
  %r2 = mul i32 %r1, 3
  %r3 = add i32 123, %r1
  br label %L1
L1:
  %r7 = phi i32 [ 0, %L0 ], [ %r2, %L5 ]
  %r5 = phi i32 [ %r0, %L0 ], [ %r11, %L5 ]
  %r10 = icmp sgt i32 %r5, 0
  br i1 %r10, label %L2, label %L6
L2:
  %r11 = sub i32 %r5, 1
  %r12 = icmp sgt i32 %r11, 10
  br i1 %r12, label %L3, label %L4
L3:
  br label %L5
L4:
  br label %L5
L5:
  br label %L1
L6:
  call void @printInt(i32 %r7)
  call void @printInt(i32 %r3)
  ret i32 0
}

