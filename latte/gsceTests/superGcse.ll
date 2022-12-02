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
  call void @f(i32 3, i32 4, i32 5)
  ret i32 0
}

define void @f(i32 %a, i32 %b, i32 %c) {
L1:
  br label %L2
L2:
  %r3 = phi i32 [ %c, %L1 ], [ %r15, %L6 ]
  %r2 = phi i32 [ %b, %L1 ], [ %r13, %L6 ]
  %r1 = phi i32 [ %a, %L1 ], [ %r16, %L6 ]
  %r4 = icmp sgt i32 %r3, 0
  br i1 %r4, label %L3, label %L7
L3:
  %r5 = sub i32 %r3, 1
  %r6 = icmp sgt i32 %r1, %r2
  br i1 %r6, label %L4, label %L5
L4:
  %r8 = add i32 %r1, 1
  br label %L6
L5:
  %r9 = sub i32 %r2, 1
  %r10 = add i32 %r1, 1
  br label %L6
L6:
  %r12 = phi i32 [ %r5, %L4 ], [ %r3, %L5 ]
  %r13 = phi i32 [ %r9, %L5 ], [ %r2, %L4 ]
  %r15 = sub i32 %r12, 1
  %r16 = add i32 %r1, 1
  br label %L2
L7:
  %r17 = sub i32 %r3, 1
  %r18 = sub i32 %r2, 1
  %r19 = add i32 %r1, 1
  call void @printInt(i32 %r19)
  call void @printInt(i32 %r18)
  call void @printInt(i32 %r17)
  ret void
}

