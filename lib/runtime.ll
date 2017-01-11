; ModuleID = 'predefinedFuncs.c'
;source_filename = "predefinedFuncs.c"
;target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
;target triple = "x86_64-apple-macosx10.12.0"

%struct.__sFILE = type { i8*, i32, i32, i16, i16, %struct.__sbuf, i32, i8*, i32 (i8*)*, i32 (i8*, i8*, i32)*, i64 (i8*, i64, i32)*, i32 (i8*, i8*, i32)*, %struct.__sbuf, %struct.__sFILEX*, i32, [3 x i8], [1 x i8], %struct.__sbuf, i32, i64 }
%struct.__sFILEX = type opaque
%struct.__sbuf = type { i8*, i32 }

@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@.str.1 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@.str.2 = private unnamed_addr constant [15 x i8] c"runtime error\0A\00", align 1
@.str.3 = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@__stdinp = external global %struct.__sFILE*, align 8

; Function Attrs: nounwind ssp uwtable
define void @printInt(i32) #0 {
  %2 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  %3 = load i32, i32* %2, align 4
  %4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 %3)
  ret void
}

declare i32 @printf(i8*, ...) #1

; Function Attrs: nounwind ssp uwtable
define void @printString(i8*) #0 {
  %2 = alloca i8*, align 8
  store i8* %0, i8** %2, align 8
  %3 = load i8*, i8** %2, align 8
  %4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.1, i32 0, i32 0), i8* %3)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define void @error() #0 {
  %1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.2, i32 0, i32 0))
  call void @exit(i32 1) #5
  unreachable
                                                  ; No predecessors!
  ret void
}

; Function Attrs: noreturn
declare void @exit(i32) #2

; Function Attrs: nounwind ssp uwtable
define i32 @readInt() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i8, align 1
  %4 = call i32 (i8*, ...) @scanf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.3, i32 0, i32 0), i32* %1)
  store i32 %4, i32* %2, align 4
  %5 = load i32, i32* %2, align 4
  %6 = icmp eq i32 %5, -1
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %0
  call void @error()
  br label %8

; <label>:8                                       ; preds = %7, %0
  br label %9

; <label>:9                                       ; preds = %25, %8
  %10 = load %struct.__sFILE*, %struct.__sFILE** @__stdinp, align 8
  %11 = call i32 @getc(%struct.__sFILE* %10)
  %12 = trunc i32 %11 to i8
  store i8 %12, i8* %3, align 1
  %13 = sext i8 %12 to i32
  %14 = icmp eq i32 %13, 32
  br i1 %14, label %23, label %15

; <label>:15                                      ; preds = %9
  %16 = load i8, i8* %3, align 1
  %17 = sext i8 %16 to i32
  %18 = icmp eq i32 %17, 9
  br i1 %18, label %23, label %19

; <label>:19                                      ; preds = %15
  %20 = load i8, i8* %3, align 1
  %21 = sext i8 %20 to i32
  %22 = icmp eq i32 %21, 10
  br label %23

; <label>:23                                      ; preds = %19, %15, %9
  %24 = phi i1 [ true, %15 ], [ true, %9 ], [ %22, %19 ]
  br i1 %24, label %25, label %26

; <label>:25                                      ; preds = %23
  br label %9

; <label>:26                                      ; preds = %23
  %27 = load i8, i8* %3, align 1
  %28 = sext i8 %27 to i32
  %29 = load %struct.__sFILE*, %struct.__sFILE** @__stdinp, align 8
  %30 = call i32 @ungetc(i32 %28, %struct.__sFILE* %29)
  %31 = load i32, i32* %1, align 4
  ret i32 %31
}

declare i32 @scanf(i8*, ...) #1

declare i32 @getc(%struct.__sFILE*) #1

declare i32 @ungetc(i32, %struct.__sFILE*) #1

; Function Attrs: nounwind ssp uwtable
define i8* @readString() #0 {
  %1 = alloca i8*, align 8
  %2 = alloca i64, align 8
  store i8* null, i8** %1, align 8
  %3 = load %struct.__sFILE*, %struct.__sFILE** @__stdinp, align 8
  %4 = call i64 @getline(i8** %1, i64* %2, %struct.__sFILE* %3)
  %5 = icmp eq i64 %4, -1
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  call void @error()
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i8*, i8** %1, align 8
  %9 = call i64 @strlen(i8* %8)
  %10 = sub i64 %9, 1
  %11 = load i8*, i8** %1, align 8
  %12 = getelementptr inbounds i8, i8* %11, i64 %10
  store i8 0, i8* %12, align 1
  %13 = load i8*, i8** %1, align 8
  ret i8* %13
}

declare i64 @getline(i8**, i64*, %struct.__sFILE*) #1

declare i64 @strlen(i8*) #1

; Function Attrs: nounwind ssp uwtable
define i8* @concat(i8*, i8*) #0 {
  %3 = alloca i8*, align 8
  %4 = alloca i8*, align 8
  %5 = alloca i8*, align 8
  store i8* %0, i8** %3, align 8
  store i8* %1, i8** %4, align 8
  %6 = load i8*, i8** %3, align 8
  %7 = call i64 @strlen(i8* %6)
  %8 = load i8*, i8** %4, align 8
  %9 = call i64 @strlen(i8* %8)
  %10 = add i64 %7, %9
  %11 = add i64 %10, 1
  %12 = call i8* @malloc(i64 %11)
  store i8* %12, i8** %5, align 8
  %13 = load i8*, i8** %5, align 8
  %14 = load i8*, i8** %3, align 8
  %15 = load i8*, i8** %5, align 8
  %16 = call i64 @llvm.objectsize.i64.p0i8(i8* %15, i1 false)
  %17 = call i8* @__strcpy_chk(i8* %13, i8* %14, i64 %16) #6
  %18 = load i8*, i8** %4, align 8
  %19 = call i8* @__strcat_chk(i8* %17, i8* %18, i64 -1) #6
  ret i8* %19
}

declare i8* @malloc(i64) #1

; Function Attrs: nounwind
declare i8* @__strcat_chk(i8*, i8*, i64) #3

; Function Attrs: nounwind
declare i8* @__strcpy_chk(i8*, i8*, i64) #3

; Function Attrs: nounwind readnone
declare i64 @llvm.objectsize.i64.p0i8(i8*, i1) #4

attributes #0 = { nounwind ssp uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { noreturn "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+mmx,+sse,+sse2,+sse3,+sse4.1,+ssse3" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { nounwind readnone }
attributes #5 = { noreturn }
attributes #6 = { nounwind }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"PIC Level", i32 2}
!1 = !{!"Apple LLVM version 8.0.0 (clang-800.0.42.1)"}
