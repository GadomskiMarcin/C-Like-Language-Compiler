declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatString__(i8*, i8*)
declare i1 @__cmpString__(i8*, i8*)
declare i8* @__llvmMemcpy__(i32)

%class.list = type {i32, %class.list*}
%class.notlist = type {i32}

define i32 @main() {
L0:
  %r0 = getelementptr %class.list, %class.list* null, i32 1
  %r1 = ptrtoint %class.list* %r0 to i32
  %r2 = call i8* @__llvmMemcpy__(i32 %r1)
  %r3 = bitcast i8* %r2 to %class.list*
  %r4 = getelementptr %class.notlist, %class.notlist* null, i32 1
  %r5 = ptrtoint %class.notlist* %r4 to i32
  %r6 = call i8* @__llvmMemcpy__(i32 %r5)
  %r7 = bitcast i8* %r6 to %class.notlist*
  %r8 = getelementptr %class.list, %class.list* %r3, i32 0, i32 0
  store i32 123, i32* %r8, align 8
  %r9 = getelementptr %class.notlist, %class.notlist* %r7, i32 0, i32 0
  store i32 321, i32* %r9, align 8
  %r10 = getelementptr %class.list, %class.list* %r3, i32 0, i32 0
  %r11 = load i32, i32* %r10 , align 8
  %r12 = getelementptr %class.notlist, %class.notlist* %r7, i32 0, i32 0
  %r13 = load i32, i32* %r12 , align 8
  %r14 = add i32 %r11, %r13
  call void @printInt(i32 %r14)
  ret i32 0
}

