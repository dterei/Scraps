; ModuleID = 'sections.c'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-f80:128:128-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32"
target triple = "i686-pc-win32"

%0 = type { i32, i32, i32 }

@a_info_tb = constant %0 { i32 -1, i32 1, i32 231 }, section ".text 0", align 4 ; <%0*> [#uses=0]
@b_info_tb = constant %0 { i32 -2, i32 0, i32 952 }, section ".text 1", align 4 ; <%0*> [#uses=0]

define i32 @a_entry() nounwind section ".text 3" {
  %1 = alloca i32, align 4                        ; <i32*> [#uses=2]
  store i32 -1, i32* %1
  %2 = load i32* %1                               ; <i32> [#uses=1]
  ret i32 %2
}

define i32 @b_entry() nounwind section ".text 2" {
  %1 = alloca i32, align 4                        ; <i32*> [#uses=2]
  store i32 1, i32* %1
  %2 = load i32* %1                               ; <i32> [#uses=1]
  ret i32 %2
}

define i32 @main() nounwind {
  %1 = alloca i32, align 4                        ; <i32*> [#uses=3]
  %a = alloca i32, align 4                        ; <i32*> [#uses=2]
  %b = alloca i32, align 4                        ; <i32*> [#uses=2]
  store i32 0, i32* %1
  %2 = call i32 @a_entry()                        ; <i32> [#uses=1]
  store i32 %2, i32* %a
  %3 = call i32 @b_entry()                        ; <i32> [#uses=1]
  store i32 %3, i32* %b
  %4 = load i32* %a                               ; <i32> [#uses=1]
  %5 = load i32* %b                               ; <i32> [#uses=1]
  %6 = add nsw i32 %4, %5                         ; <i32> [#uses=1]
  store i32 %6, i32* %1
  %7 = load i32* %1                               ; <i32> [#uses=1]
  ret i32 %7
}
