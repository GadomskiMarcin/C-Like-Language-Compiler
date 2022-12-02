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
  call void @printString(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @__string__0, i64 0, i64 0))
  %r0 = call i1 @test(i32 -1)
  br i1 %r0, label %L1, label %L2
L1:
  %r1 = call i1 @test(i32 0)
  br label %L2
L2:
  %r2 = phi i1 [ %r0, %L0 ], [ %r1, %L1 ]
  call void @printBool(i1 %r2)
  %r3 = call i1 @test(i32 -2)
  br i1 %r3, label %L3, label %L4
L3:
  %r4 = call i1 @test(i32 1)
  br label %L4
L4:
  %r5 = phi i1 [ %r3, %L2 ], [ %r4, %L3 ]
  call void @printBool(i1 %r5)
  %r6 = call i1 @test(i32 3)
  br i1 %r6, label %L5, label %L6
L5:
  %r7 = call i1 @test(i32 -5)
  br label %L6
L6:
  %r8 = phi i1 [ %r6, %L4 ], [ %r7, %L5 ]
  call void @printBool(i1 %r8)
  %r9 = call i1 @test(i32 234234)
  br i1 %r9, label %L7, label %L8
L7:
  %r10 = call i1 @test(i32 21321)
  br label %L8
L8:
  %r11 = phi i1 [ %r9, %L6 ], [ %r10, %L7 ]
  call void @printBool(i1 %r11)
  call void @printString(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @__string__1, i64 0, i64 0))
  %r12 = call i1 @test(i32 -1)
  br i1 %r12, label %L10, label %L9
L9:
  %r13 = call i1 @test(i32 0)
  br label %L10
L10:
  %r14 = phi i1 [ %r12, %L8 ], [ %r13, %L9 ]
  call void @printBool(i1 %r14)
  %r15 = call i1 @test(i32 -2)
  br i1 %r15, label %L12, label %L11
L11:
  %r16 = call i1 @test(i32 1)
  br label %L12
L12:
  %r17 = phi i1 [ %r15, %L10 ], [ %r16, %L11 ]
  call void @printBool(i1 %r17)
  %r18 = call i1 @test(i32 3)
  br i1 %r18, label %L14, label %L13
L13:
  %r19 = call i1 @test(i32 -5)
  br label %L14
L14:
  %r20 = phi i1 [ %r18, %L12 ], [ %r19, %L13 ]
  call void @printBool(i1 %r20)
  %r21 = call i1 @test(i32 234234)
  br i1 %r21, label %L16, label %L15
L15:
  %r22 = call i1 @test(i32 21321)
  br label %L16
L16:
  %r23 = phi i1 [ %r21, %L14 ], [ %r22, %L15 ]
  call void @printBool(i1 %r23)
  call void @printString(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @__string__2, i64 0, i64 0))
  call void @printBool(i1 true)
  call void @printBool(i1 false)
  ret i32 0
}

define void @printBool(i1 %b) {
L17:
  %r24 = xor i1 1, %b
  br i1 %r24, label %L18, label %L19
L18:
  call void @printString(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @__string__3, i64 0, i64 0))
  br label %L20
L19:
  call void @printString(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @__string__4, i64 0, i64 0))
  br label %L20
L20:
  ret void
}

define i1 @test(i32 %i) {
L21:
  call void @printInt(i32 %i)
  %r25 = icmp sgt i32 %i, 0
  ret i1 %r25
}

@__string__0 = private constant [3 x i8] c"&&\00"
@__string__1 = private constant [3 x i8] c"||\00"
@__string__2 = private constant [2 x i8] c"!\00"
@__string__3 = private constant [6 x i8] c"false\00"
@__string__4 = private constant [5 x i8] c"true\00"
