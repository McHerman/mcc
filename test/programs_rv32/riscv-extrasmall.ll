; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

define void @__impl_riscv_kernel_0(ptr %0, ptr %1, i32 %2, i32 %3, i32 %4, i32 %5, i32 %6, ptr %7, ptr %8, i32 %9, i32 %10, i32 %11, i32 %12, i32 %13, i8 %14) {
  br label %16

16:                                               ; preds = %16, %15
  %17 = load i32, ptr inttoptr (i32 16452 to ptr), align 4
  %18 = icmp eq i32 %17, 8
  br i1 %18, label %19, label %16

19:                                               ; preds = %16
  %20 = atomicrmw add ptr inttoptr (i32 16452 to ptr), i32 -8 acquire, align 4
  br label %21

21:                                               ; preds = %21, %19
  %22 = load i32, ptr inttoptr (i32 16440 to ptr), align 4
  %23 = icmp eq i32 %22, 8
  br i1 %23, label %24, label %21

24:                                               ; preds = %21
  %25 = atomicrmw add ptr inttoptr (i32 16440 to ptr), i32 -8 acquire, align 4
  br label %26

26:                                               ; preds = %44, %24
  %27 = phi i32 [ 0, %24 ], [ %45, %44 ]
  %28 = icmp slt i32 %27, 1
  br i1 %28, label %29, label %46

29:                                               ; preds = %26
  br label %30

30:                                               ; preds = %33, %29
  %31 = phi i32 [ 0, %29 ], [ %43, %33 ]
  %32 = icmp slt i32 %31, 8
  br i1 %32, label %33, label %44

33:                                               ; preds = %30
  %34 = mul nuw nsw i32 %27, 8
  %35 = add nuw nsw i32 %34, %31
  %36 = getelementptr inbounds nuw i8, ptr %8, i32 %35
  %37 = load i8, ptr %36, align 1
  %38 = icmp sgt i8 %37, %14
  %39 = select i1 %38, i8 %37, i8 %14
  %40 = mul nuw nsw i32 %27, 8
  %41 = add nuw nsw i32 %40, %31
  %42 = getelementptr inbounds nuw i8, ptr %1, i32 %41
  store i8 %39, ptr %42, align 1
  %43 = add i32 %31, 1
  br label %30

44:                                               ; preds = %30
  %45 = add i32 %27, 1
  br label %26

46:                                               ; preds = %26
  %47 = atomicrmw add ptr inttoptr (i32 16448 to ptr), i32 8 release, align 4
  %48 = atomicrmw add ptr inttoptr (i32 16444 to ptr), i32 8 release, align 4
  ret void
}

define void @riscv_kernel_0() {
  call void @__impl_riscv_kernel_0(ptr null, ptr null, i32 0, i32 1, i32 8, i32 8, i32 1, ptr inttoptr (i32 64 to ptr), ptr inttoptr (i32 64 to ptr), i32 0, i32 1, i32 8, i32 8, i32 1, i8 0)
  ret void
}

define void @main() {
  call void @riscv_kernel_0()
  ret void
}

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}
