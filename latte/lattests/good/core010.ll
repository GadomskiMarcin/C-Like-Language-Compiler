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
  %r0 = call i32 @fac(i32 5)
  call void @printInt(i32 %r0)
  ret i32 0
}

define i32 @fac(i32 %a) {
L1:
  br label %L2
L2:
  %r4 = phi i32 [ 1, %L1 ], [ %r6, %L3 ]
  %r3 = phi i32 [ %a, %L1 ], [ %r7, %L3 ]
  %r5 = icmp sgt i32 %r3, 0
  br i1 %r5, label %L3, label %L4
L3:
  %r6 = mul i32 %r4, %r3
  %r7 = sub i32 %r3, 1
  br label %L2
L4:
  ret i32 %r4
}

