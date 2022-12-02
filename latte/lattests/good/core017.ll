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
  call void @printBool(i1 true)
  call void @printBool(i1 true)
  call void @printBool(i1 false)
  call void @printBool(i1 true)
  %r0 = call i1 @implies(i1 false, i1 false)
  call void @printBool(i1 %r0)
  %r1 = call i1 @implies(i1 false, i1 true)
  call void @printBool(i1 %r1)
  %r2 = call i1 @implies(i1 true, i1 false)
  call void @printBool(i1 %r2)
  %r3 = call i1 @implies(i1 true, i1 true)
  call void @printBool(i1 %r3)
  ret i32 0
}

define i1 @dontCallMe(i32 %x) {
L1:
  call void @printInt(i32 %x)
  ret i1 true
}

define void @printBool(i1 %b) {
L2:
  br i1 %b, label %L3, label %L4
L3:
  call void @printString(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @__string__0, i64 0, i64 0))
  br label %L5
L4:
  call void @printString(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @__string__1, i64 0, i64 0))
  br label %L5
L5:
  ret void
}

define i1 @implies(i1 %x, i1 %y) {
L6:
  %r4 = xor i1 1, %x
  br i1 %r4, label %L8, label %L7
L7:
  %r5 = icmp eq i1 %x, %y
  br label %L8
L8:
  %r6 = phi i1 [ %r4, %L6 ], [ %r5, %L7 ]
  ret i1 %r6
}

@__string__0 = private constant [5 x i8] c"true\00"
@__string__1 = private constant [6 x i8] c"false\00"
