
#ifndef RUNTIME_H
#define RUNTIME_H

#include "runtime_shared.h"

#define DATA %struct.Data
#define CONS %struct.Cons

DATA = type { i8*, i8 }
CONS = type { DATA*, DATA* }

declare DATA* @car(DATA* %d)
declare DATA* @cdr(DATA* %d) 
declare DATA* @display(DATA* %d) 
declare DATA* @string_to_symbol(i8* %str) 
declare DATA* @get_env(DATA* %symbol) 
declare void @llvm.memcpy.i32(i8* %dst, i8* %src, i32 %size, i32 %align)

#endif

