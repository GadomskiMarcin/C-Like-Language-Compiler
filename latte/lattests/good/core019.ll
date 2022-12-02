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
  call void @printInt(i32 78)
  br label %L1
L1:
  %r0 = phi i32 [ 78, %L0 ], [ %r2, %L2 ]
  %r1 = icmp sgt i32 %r0, 76
  br i1 %r1, label %L2, label %L3
L2:
  %r2 = sub i32 %r0, 1
  call void @printInt(i32 %r2)
  %r3 = add i32 %r2, 7
  call void @printInt(i32 %r3)
  br label %L1
L3:
  call void @printInt(i32 %r0)
  %r4 = icmp sgt i32 %r0, 4
  br i1 %r4, label %L4, label %L5
L4:
  call void @printInt(i32 4)
  br label %L6
L5:
  call void @printString(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @__string__0, i64 0, i64 0))
  br label %L6
L6:
  call void @printInt(i32 %r0)
  ret i32 0
}

@__string__0 = private constant [4 x i8] c"foo\00"
