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
  %r0 = call i32 @readInt()
  %r1 = call i8* @readString()
  %r2 = call i8* @readString()
  %r3 = sub i32 %r0, 5
  call void @printInt(i32 %r3)
  %r4 = call i8* @__concatString__(i8* %r1, i8* %r2)
  call void @printString(i8* %r4)
  ret i32 0
}

