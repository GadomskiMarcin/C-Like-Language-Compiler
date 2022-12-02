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
  call void @printInt(i32 1)
  br label %L1
L1:
  %r1 = phi i32 [ 1, %L0 ], [ %r5, %L2 ]
  %r0 = phi i32 [ 1, %L0 ], [ %r4, %L2 ]
  %r3 = icmp slt i32 %r0, 5000000
  br i1 %r3, label %L2, label %L3
L2:
  call void @printInt(i32 %r0)
  %r4 = add i32 %r1, %r0
  %r5 = sub i32 %r4, %r1
  br label %L1
L3:
  ret i32 0
}

