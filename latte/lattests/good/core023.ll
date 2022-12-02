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
  %r0 = call i32 @foo(i32 1, i32 2, i32 1, i32 2, i32 1, i32 2, i32 1, i32 2, i32 1, i32 2, i32 1, i32 2, i32 1, i32 2)
  ret i32 %r0
}

define i32 @foo(i32 %a, i32 %b, i32 %c, i32 %d, i32 %e, i32 %f, i32 %g, i32 %h, i32 %i, i32 %j, i32 %k, i32 %l, i32 %m, i32 %n) {
L1:
  %r1 = mul i32 2, %a
  %r2 = sdiv i32 %b, 2
  %r3 = add i32 %r1, %r2
  %r4 = add i32 %r3, %c
  %r5 = add i32 %r4, %d
  %r6 = add i32 %r5, %e
  %r7 = add i32 %r6, %f
  %r8 = add i32 %r7, %g
  %r9 = add i32 %r8, %h
  %r10 = add i32 %r9, %i
  %r11 = sdiv i32 %j, 2
  %r12 = add i32 %r10, %r11
  %r13 = add i32 %r12, %k
  %r14 = add i32 %r13, %l
  %r15 = add i32 %r14, %m
  %r16 = add i32 %r15, %n
  %r17 = srem i32 %r16, 10
  call void @printInt(i32 %r17)
  ret i32 %r17
}

