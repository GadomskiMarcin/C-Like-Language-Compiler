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
  call void @printInt(i32 33)
  call void @printInt(i32 79)
  call void @printInt(i32 -1288)
  call void @printInt(i32 22)
  call void @printInt(i32 0)
  call void @printBool(i1 true)
  call void @printBool(i1 false)
  %r0 = call i8* @__concatString__(i8* getelementptr inbounds ([7 x i8], [7 x i8]* @__string__0, i64 0, i64 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @__string__1, i64 0, i64 0))
  %r1 = call i8* @__concatString__(i8* %r0, i8* getelementptr inbounds ([14 x i8], [14 x i8]* @__string__2, i64 0, i64 0))
  call void @printString(i8* %r1)
  ret i32 0
}

define void @printBool(i1 %b) {
L1:
  br i1 %b, label %L2, label %L3
L2:
  call void @printString(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @__string__3, i64 0, i64 0))
  ret void
L3:
  call void @printString(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @__string__4, i64 0, i64 0))
  ret void
}

@__string__0 = private constant [7 x i8] c"string\00"
@__string__1 = private constant [2 x i8] c" \00"
@__string__2 = private constant [14 x i8] c"concatenation\00"
@__string__3 = private constant [5 x i8] c"true\00"
@__string__4 = private constant [6 x i8] c"false\00"
