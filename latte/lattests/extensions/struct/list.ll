declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatString__(i8*, i8*)
declare i1 @__cmpString__(i8*, i8*)
declare i8* @__llvmMemcpy__(i32)

%class.list = type {i32, %class.list*}

define i32 @main() {
L0:
  %r0 = call %class.list* @fromTo(i32 1, i32 50)
  %r1 = call i32 @length(%class.list* %r0)
  call void @printInt(i32 %r1)
  %r2 = call %class.list* @fromTo(i32 1, i32 100)
  %r3 = call i32 @length2(%class.list* %r2)
  call void @printInt(i32 %r3)
  ret i32 0
}

define i32 @head(%class.list* %xs) {
L1:
  %r4 = getelementptr %class.list, %class.list* %xs, i32 0, i32 0
  %r5 = load i32, i32* %r4 , align 8
  ret i32 %r5
}

define %class.list* @cons(i32 %x, %class.list* %xs) {
L2:
  %r6 = getelementptr %class.list, %class.list* null, i32 1
  %r7 = ptrtoint %class.list* %r6 to i32
  %r8 = call i8* @__llvmMemcpy__(i32 %r7)
  %r9 = bitcast i8* %r8 to %class.list*
  %r10 = getelementptr %class.list, %class.list* %r9, i32 0, i32 0
  store i32 %x, i32* %r10, align 8
  %r11 = getelementptr %class.list, %class.list* %r9, i32 0, i32 1
  store %class.list* %xs, %class.list** %r11, align 8
  ret %class.list* %r9
}

define i32 @length(%class.list* %xs) {
L3:
  %r12 = icmp eq %class.list* %xs, null
  br i1 %r12, label %L4, label %L5
L4:
  ret i32 0
L5:
  %r13 = getelementptr %class.list, %class.list* %xs, i32 0, i32 1
  %r14 = load %class.list*, %class.list** %r13 , align 8
  %r15 = call i32 @length(%class.list* %r14)
  %r16 = add i32 1, %r15
  ret i32 %r16
}

define %class.list* @fromTo(i32 %m, i32 %n) {
L7:
  %r17 = icmp sgt i32 %m, %n
  br i1 %r17, label %L8, label %L9
L8:
  ret %class.list* null
L9:
  %r18 = add i32 %m, 1
  %r19 = call %class.list* @fromTo(i32 %r18, i32 %n)
  %r20 = call %class.list* @cons(i32 %m, %class.list* %r19)
  ret %class.list* %r20
}

define i32 @length2(%class.list* %xs) {
L11:
  br label %L12
L12:
  %r23 = phi %class.list* [ %xs, %L11 ], [ %r27, %L13 ]
  %r22 = phi i32 [ 0, %L11 ], [ %r25, %L13 ]
  %r24 = icmp ne %class.list* %r23, null
  br i1 %r24, label %L13, label %L14
L13:
  %r25 = add i32 %r22, 1
  %r26 = getelementptr %class.list, %class.list* %r23, i32 0, i32 1
  %r27 = load %class.list*, %class.list** %r26 , align 8
  br label %L12
L14:
  ret i32 %r22
}

