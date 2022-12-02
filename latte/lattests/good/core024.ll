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
  call void @f(i32 1, i32 2)
  ret i32 0
}

define void @f(i32 %x, i32 %y) {
L1:
  %r0 = icmp sgt i32 %y, %x
  br i1 %r0, label %L3, label %L2
L2:
  %r1 = call i1 @e()
  br label %L3
L3:
  %r2 = phi i1 [ %r0, %L1 ], [ %r1, %L2 ]
  br i1 %r2, label %L4, label %L5
L4:
  call void @printString(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @__string__0, i64 0, i64 0))
  br label %L5
L5:
  ret void
}

define i1 @e() {
L6:
  call void @printString(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @__string__1, i64 0, i64 0))
  ret i1 false
}

@__string__0 = private constant [4 x i8] c"yes\00"
@__string__1 = private constant [5 x i8] c"NOOO\00"
