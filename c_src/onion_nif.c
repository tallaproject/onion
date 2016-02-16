// Copyright (c) 2015 The Talla Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "onion.h"
#include "onion_nif_rsa.h"

static ErlNifFunc nif_functions[] = {
    { "rsa_generate_private_key", 2, onion_nif_rsa_generate_private_key, ERL_NIF_DIRTY_JOB_CPU_BOUND }
};

static int on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

static int on_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

ERL_NIF_TERM make_error_tuple(ErlNifEnv *env, char *error)
{
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, error));
}

ERL_NIF_INIT(onion_nif, nif_functions, on_load, /* reload */ NULL, on_upgrade, /* unload */ NULL);
