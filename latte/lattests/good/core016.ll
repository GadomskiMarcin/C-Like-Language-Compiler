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
  br label %L1
L1:
  %r0 = phi i32 [ 17, %L0 ], [ %r2, %L2 ]
  %r1 = icmp sgt i32 %r0, 0
  br i1 %r1, label %L2, label %L3
L2:
  %r2 = sub i32 %r0, 2
  br label %L1
L3:
  %r3 = icmp slt i32 %r0, 0
  br i1 %r3, label %L4, label %L5
L4:
  call void @printInt(i32 0)
  ret i32 0
L5:
  call void @printInt(i32 1)
  ret i32 0
}

