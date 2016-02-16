// Copyright (c) 2015 The Talla Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GUARD_ONION_NIF_RSA_H
#define GUARD_ONION_NIF_RSA_H 1

#include "onion.h"

ERL_NIF_TERM onion_nif_rsa_generate_private_key(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#endif
