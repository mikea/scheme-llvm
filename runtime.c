#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "runtime_shared.h"

typedef struct {
  void* data;
  char type;
} Data;

typedef struct {
  Data* car;
  Data* cdr;
} Cons;

#define CHECK(b) assert(b)
#define CHECK_IS_CONS(d) CHECK(d->type == T_CONS)
#define CHECK_IS_INT(d) CHECK(d->type == T_INT)
#define CHECK_IS_SYMBOL(d) CHECK(d->type == T_SYMBOL)

#define CONS(d) ((Cons*)(d->data))
#define CHARP(d) ((char*)(d->data))
#define INT(d) ((int)(d->data))

Data* car(Data* d) {
  CHECK_IS_CONS(d);
  return CONS(d)->car;
}

Data* cdr(Data* d) {
  CHECK_IS_CONS(d);
  return CONS(d)->cdr;
}

void display_int(Data* d) {
  CHECK_IS_INT(d);
  printf("%d", INT(d));
}

void display_symbol(Data* d) {
  CHECK_IS_SYMBOL(d);
  printf("%s", CHARP(d));
}

Data* display(Data* d) {
  if (!d) {
    printf("()");
    return 0;
  }
  switch (d->type) {
  case T_INT : 
    display_int(d);
    break;
  case T_SYMBOL :
    display_symbol(d);
    break;
  default : assert(0);
  }
  return 0;
}

Data* symbols;
int symbols_size;
int symbols_count;

void init_symbols() {
  symbols_size = 100;
  symbols = malloc(sizeof(Data) * symbols_size);
  memset(symbols, 0, sizeof(Data) * symbols_size);
  symbols_count = 0;
}

Data* string_to_symbol(char* str) {
  int i;
  for (i = 0; i < symbols_count; i++) {
    if (!strcmp(str, (char*)symbols[i].data)) {
      return &symbols[i];
    }
  }

  // not found, should add new one

  if (symbols_count == symbols_size) {
    // reallocate
    Data* old = symbols;
    symbols = malloc(sizeof(Data) * symbols_size * 2);
    memset(symbols, 0, sizeof(Data) * symbols_size * 2);
    memcpy(symbols, old, sizeof(Data) * symbols_size);
    symbols_size *= 2;
  }

  Data *d = &symbols[symbols_count];
  symbols_count++;

  d->type = T_SYMBOL;
  int len = strlen(str);
  d->data = malloc(len + 1);
  memcpy(d->data, str, len + 1);
  return d;
}

void add_to_env(Data* symbol,
		Data* value,
		Cons** env,
		int* env_size,
		int* env_count) {
  assert(*env_count < *env_size);

  Cons* c = &((*env)[*env_count]);
  *env_count++;
  c->car = symbol;
  c->cdr = value;
}

Data* new_builtin(void* f) {
  Data* d = malloc(sizeof(Data));
  d->type = T_BUILTIN;
  d->data = f;
  return d;
}

Cons* env;
int env_size;
int env_count;

void register_builtin(char* name, void* f) {
  add_to_env(string_to_symbol(name), new_builtin(f), &env, &env_size, &env_count);
}

void init_environment() {
  env_size = 100;
  env_count = 0;
  env = malloc(sizeof(Cons) * env_size);
  memset(env, 0, sizeof(Cons) * env_size);

  register_builtin("car", car);
  register_builtin("cdr", cdr);
}

Data* get_env(Data* symbol) {
  int i;
  for (i = 0; i < env_count; i++) {
    if (env[i].car == symbol) return env[i].cdr;
  }

  assert(0);
}

void scheme_main(void);
void scheme_init(void);

int main(int argc, char** argv) {
  init_symbols();
  init_environment();
  scheme_init();
  scheme_main();
  return 0;
}
