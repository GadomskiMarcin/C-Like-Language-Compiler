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
  %r0 = call i32 @fac(i32 10)
  call void @printInt(i32 %r0)
  %r1 = call i32 @rfac(i32 10)
  call void @printInt(i32 %r1)
  %r2 = call i32 @mfac(i32 10)
  call void @printInt(i32 %r2)
  %r3 = call i32 @ifac(i32 10)
  call void @printInt(i32 %r3)
  br label %L1
L1:
  %r5 = phi i32 [ 1, %L0 ], [ %r7, %L2 ]
  %r4 = phi i32 [ 10, %L0 ], [ %r8, %L2 ]
  %r6 = icmp sgt i32 %r4, 0
  br i1 %r6, label %L2, label %L3
L2:
  %r7 = mul i32 %r5, %r4
  %r8 = sub i32 %r4, 1
  br label %L1
L3:
  call void @printInt(i32 %r5)
  %r9 = call i8* @repStr(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @__string__1, i64 0, i64 0), i32 60)
  call void @printString(i8* %r9)
  call void @printString(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @__string__2, i64 0, i64 0))
  call void @printString(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @__string__3, i64 0, i64 0))
  ret i32 0
}

define i32 @fac(i32 %a) {
L4:
  br label %L5
L5:
  %r13 = phi i32 [ 1, %L4 ], [ %r15, %L6 ]
  %r12 = phi i32 [ %a, %L4 ], [ %r16, %L6 ]
  %r14 = icmp sgt i32 %r12, 0
  br i1 %r14, label %L6, label %L7
L6:
  %r15 = mul i32 %r13, %r12
  %r16 = sub i32 %r12, 1
  br label %L5
L7:
  ret i32 %r13
}

define i32 @rfac(i32 %n) {
L8:
  %r17 = icmp eq i32 %n, 0
  br i1 %r17, label %L9, label %L10
L9:
  ret i32 1
L10:
  %r18 = sub i32 %n, 1
  %r19 = call i32 @rfac(i32 %r18)
  %r20 = mul i32 %n, %r19
  ret i32 %r20
}

define i32 @mfac(i32 %n) {
L12:
  %r21 = icmp eq i32 %n, 0
  br i1 %r21, label %L13, label %L14
L13:
  ret i32 1
L14:
  %r22 = sub i32 %n, 1
  %r23 = call i32 @nfac(i32 %r22)
  %r24 = mul i32 %n, %r23
  ret i32 %r24
}

define i32 @nfac(i32 %n) {
L16:
  %r25 = icmp ne i32 %n, 0
  br i1 %r25, label %L17, label %L18
L17:
  %r26 = sub i32 %n, 1
  %r27 = call i32 @mfac(i32 %r26)
  %r28 = mul i32 %r27, %n
  ret i32 %r28
L18:
  ret i32 1
}

define i32 @ifac(i32 %n) {
L20:
  %r29 = call i32 @ifac2f(i32 1, i32 %n)
  ret i32 %r29
}

define i32 @ifac2f(i32 %l, i32 %h) {
L21:
  %r30 = icmp eq i32 %l, %h
  br i1 %r30, label %L22, label %L23
L22:
  ret i32 %l
L23:
  %r31 = icmp sgt i32 %l, %h
  br i1 %r31, label %L24, label %L25
L24:
  ret i32 1
L25:
  %r32 = add i32 %l, %h
  %r33 = sdiv i32 %r32, 2
  %r34 = call i32 @ifac2f(i32 %l, i32 %r33)
  %r35 = add i32 %r33, 1
  %r36 = call i32 @ifac2f(i32 %r35, i32 %h)
  %r37 = mul i32 %r34, %r36
  ret i32 %r37
}

define i8* @repStr(i8* %s, i32 %n) {
L26:
  br label %L27
L27:
  %r41 = phi i8* [ getelementptr inbounds ([1 x i8], [1 x i8]* @__string__4, i64 0, i64 0), %L26 ], [ %r44, %L28 ]
  %r39 = phi i32 [ 0, %L26 ], [ %r45, %L28 ]
  %r43 = icmp slt i32 %r39, %n
  br i1 %r43, label %L28, label %L29
L28:
  %r44 = call i8* @__concatString__(i8* %r41, i8* %s)
  %r45 = add i32 %r39, 1
  br label %L27
L29:
  ret i8* %r41
}

@__string__0 = private constant [1 x i8] c"\00"
@__string__1 = private constant [2 x i8] c"=\00"
@__string__2 = private constant [9 x i8] c"hello */\00"
@__string__3 = private constant [9 x i8] c"/* world\00"
@__string__4 = private constant [1 x i8] c"\00"
