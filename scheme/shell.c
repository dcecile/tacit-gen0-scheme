#include <stdio.h>

#include "tinyscheme/scheme.h"
#include "tinyscheme/scheme-private.h"

pointer write_binary(scheme *sc, pointer args) {
  if (args == sc->NIL
      || !sc->vptr->is_string(sc->vptr->pair_car(args))) {
    printf("first string\n");
    return sc->NIL; }
  char *filename = sc->vptr->string_value(sc->vptr->pair_car(args));
  FILE *file = fopen(filename, "wb");
  long written = 0;
  if (file == NULL) {
    printf("fopen\n");
    return sc->NIL; }
  args = sc->vptr->pair_cdr(args);
  if (args == sc->NIL) {
    printf("second list\n");
    return sc->NIL; }
  args = sc->vptr->pair_car(args);
  while (args != sc->NIL) {
    if (!sc->vptr->is_integer(sc->vptr->pair_car(args))) {
      return sc->NIL; }
    long byte = sc->vptr->ivalue(sc->vptr->pair_car(args));
    if (byte != (0xFF & byte)) {
      printf("byte\n");
      return sc->NIL; }
    int status = fputc((int)(byte), file);
    if (status == EOF) {
      printf("fputc\n");
      return sc->NIL; }
    written++;
    args = sc->vptr->pair_cdr(args); }
  int status = fclose(file);
  if (status == EOF) {
    printf("fclose\n");
    return sc->NIL; }
  return sc->vptr->mk_integer(sc, written); }

//pointer get_stdin(scheme *sc, pointer args) {

int main(int argc, char *argv[]) {
  scheme vsc;
  scheme *sc = &vsc;
  if (argc != 2) {
    fprintf(stderr, "Wrong arguments\n");
    return 1; }
  if (!scheme_init(sc)) {
    fprintf(stderr, "Cannot init\n");
    return 1; }
  sc->vptr->scheme_define(
    sc,
    sc->global_env,
    sc->vptr->mk_symbol(sc,"write-binary"),
    sc->vptr->mk_foreign_func(sc, write_binary));
  scheme_set_output_port_file(sc, stdout);
  FILE *file = fopen(argv[1], "r");
  if (file == NULL) {
    fprintf(stderr, "Cannot open file\n");
    return 1; }
  scheme_load_named_file(sc, file, argv[1]);
  fclose(file);
  scheme_set_input_port_file(sc, stdin);
  scheme_apply0(sc, "main");
  int retcode = sc->retcode;
  scheme_deinit(sc);
  return retcode; }
