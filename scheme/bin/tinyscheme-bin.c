#include <stdio.h>
#include <string.h>

#include "tinyscheme-src/scheme.h"
#include "tinyscheme-src/scheme-private.h"

// Binary writer
pointer write_binary(scheme *sc, pointer args) {

  // Argument check
  if (args == sc->NIL
      || !sc->vptr->is_string(sc->vptr->pair_car(args))) {
    printf("first string\n");
    return sc->NIL; }

  // File open
  char *filename = sc->vptr->string_value(sc->vptr->pair_car(args));
  FILE *file = fopen(filename, "wb");
  long written = 0;
  if (file == NULL) {
    printf("fopen\n");
    return sc->NIL; }

  // Loop through bytes
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

    // Write the byte
    int status = fputc((int)(byte), file);
    if (status == EOF) {
      printf("fputc\n");
      return sc->NIL; }
    written++;
    args = sc->vptr->pair_cdr(args); }

  // Close the file
  int status = fclose(file);
  if (status == EOF) {
    printf("fclose\n");
    return sc->NIL; }
  return sc->vptr->mk_integer(sc, written); }

// Currently loading file
pointer currently_loading_file(scheme *sc, pointer args) {
  return sc->vptr->mk_string(sc,
    sc->load_stack[sc->file_i].rep.stdio.filename);
}

// Entry point
int main(int argc, char *argv[]) {

  // Initialize
  scheme vsc;
  scheme *sc = &vsc;
  if (!scheme_init(sc)) {
    fprintf(stderr, "Cannot init\n");
    return 1; }

  // Set up native functions
  sc->vptr->scheme_define(
    sc,
    sc->global_env,
    sc->vptr->mk_symbol(sc, "write-binary"),
    sc->vptr->mk_foreign_func(sc, write_binary));
  sc->vptr->scheme_define(
    sc,
    sc->global_env,
    sc->vptr->mk_symbol(sc, "currently-loading-file"),
    sc->vptr->mk_foreign_func(sc, currently_loading_file));

  // Load Scheme code
  scheme_set_output_port_file(sc, stdout);
  for (int i = 1; i < argc; i++) {

    // Evaluate a string
    if (strcmp(argv[i], "-e") == 0) {
      scheme_load_string(sc, argv[++i]); }

    // Load a file
    else {
      FILE *file = fopen(argv[i], "r");
      if (file == NULL) {
        fprintf(stderr, "Cannot open file\n");
        return 1; }
      scheme_load_named_file(sc, file, argv[i]);
      fclose(file); } }

  // Exit
  int retcode = sc->retcode;
  scheme_deinit(sc);
  return retcode; }
