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
  %r0 = call i32 @f(i32 1, i32 -1)
  call void @printInt(i32 %r0)
  ret i32 0
}

define i32 @f(i32 %a, i32 %b) {
L1:
  %r1 = icmp sgt i32 %a, 0
  br i1 %r1, label %L2, label %L3
L2:
  %r2 = icmp sgt i32 %b, 0
  br label %L3
L3:
  %r3 = phi i1 [ %r1, %L1 ], [ %r2, %L2 ]
  br i1 %r3, label %L7, label %L4
L4:
  %r4 = icmp slt i32 %a, 0
  br i1 %r4, label %L5, label %L6
L5:
  %r5 = icmp slt i32 %b, 0
  br label %L6
L6:
  %r6 = phi i1 [ %r4, %L4 ], [ %r5, %L5 ]
  br label %L7
L7:
  %r7 = phi i1 [ %r3, %L3 ], [ %r6, %L6 ]
  br i1 %r7, label %L8, label %L9
L8:
  ret i32 7
L9:
  ret i32 42
}

