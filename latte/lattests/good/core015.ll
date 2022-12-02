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
  %r0 = call i32 @ev(i32 17)
  call void @printInt(i32 %r0)
  ret i32 0
}

define i32 @ev(i32 %y) {
L1:
  %r1 = icmp sgt i32 %y, 0
  br i1 %r1, label %L2, label %L3
L2:
  %r2 = sub i32 %y, 2
  %r3 = call i32 @ev(i32 %r2)
  ret i32 %r3
L3:
  %r4 = icmp slt i32 %y, 0
  br i1 %r4, label %L4, label %L5
L4:
  ret i32 0
L5:
  ret i32 1
}

