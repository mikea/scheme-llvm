
#ifndef RUNTIME_H
#define RUNTIME_H

#include "runtime_shared.h"

#define DATA %struct.Data
#define CONS %struct.Cons
#define LAMBDA %struct.Lambda

DATA = type { i8*, i8 }
CONS = type { DATA*, DATA* }
LAMBDA = type { i32, i8* }

@car = external global DATA
@cdr = external global DATA
@add = external global DATA


declare DATA* @display(DATA* %d) 
declare void @init_symbol(i8* %str, DATA* %data)
declare DATA* @get_env(DATA* %symbol) 
declare void @llvm.memcpy.i32(i8* %dst, i8* %src, i32 %size, i32 %align)

declare DATA* @call0(DATA* %d)
declare DATA* @call1(DATA* %d, DATA* %p1)
declare DATA* @call2(DATA* %d, DATA* %p1, DATA* %p2)
declare DATA* @call3(DATA* %d, DATA* %p1, DATA* %p2, DATA* %p3)
declare DATA* @call4(DATA* %d, DATA* %p1, DATA* %p2, DATA* %p3, DATA* %p4)
declare DATA* @call5(DATA* %d, DATA* %p1, DATA* %p2, DATA* %p3, DATA* %p4, DATA* %p5)
declare DATA* @call6(DATA* %d, DATA* %p1, DATA* %p2, DATA* %p3, DATA* %p4, DATA* %p5, 
		     DATA* %p6)
declare DATA* @call7(DATA* %d, DATA* %p1, DATA* %p2, DATA* %p3, DATA* %p4, DATA* %p5, 
		     DATA* %p6, DATA* %p7)
declare DATA* @call8(DATA* %d, DATA* %p1, DATA* %p2, DATA* %p3, DATA* %p4, DATA* %p5, 
		     DATA* %p6, DATA* %p7, DATA* %p8)
declare DATA* @call9(DATA* %d, DATA* %p1, DATA* %p2, DATA* %p3, DATA* %p4, DATA* %p5, 
		     DATA* %p6, DATA* %p7, DATA* %p8, DATA* %p9)

#endif

