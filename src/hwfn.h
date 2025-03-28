#ifndef ZAKAROUF_HAYWIRE_HWFN_H
#define ZAKAROUF_HAYWIRE_HWFN_H

#include "def.h"

#define CAT2(X_Y) X##_##Y
#define CAT(X_Y) CAT2(X, Y)

enum hwfn_InterfaceID {
      hwfn_InterfaceID_new                    // &self
    , hwfn_InterfaceID_delete                 // &self
    , hwfn_InterfaceID_newFrom_string         // &self, &lexer
    , hwfn_InterfaceID_newFrom_deserialize    // &self, &index, bytearr
    , hwfn_InterfaceID_to_string              // &self, &String
    , hwfn_InterfaceID_to_info                // &self, &String
    , hwfn_InterfaceID_to_serialize           // &self, &bytearr
};

/**
 * Section ARR_EXPORT
 */
#define DEFN(NAME)\
    void hwfn_##NAME (        \
        hw_State    *th       \
      , hw_Var      *args     \
      , hw_byte     *tid      \
      , hw_uint const count)

/**
 * DECLARATION
 */
/**/
DEFN(VarFn_UNREACHABLE); // Raise an error;

/* String */
DEFN(String_new);
DEFN(String_delete);
DEFN(String_newFrom_data);
DEFN(String_newFrom_file);
DEFN(String_newFrom_fmt);
DEFN(String_append_bytes);

/* List */
DEFN(VarList_new);
DEFN(VarList_newFrom_fmt);
DEFN(VarList_delete);
DEFN(VarList_reserve);
DEFN(VarList_expand);
DEFN(VarList_push_shallow);

/* Array */
DEFN(VarArr_newFrom_conf);
DEFN(VarArr_delete);
DEFN(VarArr_push);
DEFN(VarArr_get);

/* SArr */
DEFN(SArr_newFrom_conf);
DEFN(SArr_delete);
DEFN(SArr_push);
DEFN(SArr_pop);
DEFN(SArr_get);

/* Symtable */
DEFN(SymTable_new);
DEFN(SymTable_newFrom_fmt);
DEFN(SymTable_delete);
DEFN(SymTable_set);
DEFN(SymTable_get);
DEFN(SymTable_reset);

/* ByteArr */
DEFN(byteArr_new);
DEFN(byteArr_delete);
DEFN(byteArr_pushstr);

/* Module */
DEFN(Module_newFrom_deserialize);   // &self, &index, bytearr
DEFN(Module_to_serialize);          // &self, &bytearray

#undef DEFN
#undef CAT
#undef CAT2

#endif

