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
  call void @f(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @__string__0, i64 0, i64 0))
  ret i32 0
}

define void @f(i8* %arg) {
L1:
  call void @printString(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @__string__1, i64 0, i64 0))
  ret void
}

@__string__0 = private constant [4 x i8] c"bad\00"
@__string__1 = private constant [5 x i8] c"good\00"
