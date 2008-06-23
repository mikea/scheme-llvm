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

#define INIT_DATA(d, value, value_type, data_type) \
	%data_addr = getlementptr DATA* d, i32 0 \
	%data_value = bitcast value_type value to i8* \
	store i8* %data_value, %data_addr \
	%type_addr = getelementptr DATA* d, i32 1 \
	store i8 data_type, %type_addr

#define SET_CAR(cons, car)   \
	%car_addr = getelementptr CONS* cons, i32 0 \
	store car, %car_addr

#define SET_CDR(cons, cdr)   \
	%cdr_addr = getelementptr CONS* cons, i32 1 \
	store cdr, %cdr_addr

#endif

