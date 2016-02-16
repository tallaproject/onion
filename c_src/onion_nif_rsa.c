// Copyright (c) 2015 The Talla Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <openssl/bn.h>
#include <openssl/rsa.h>
#include <openssl/pem.h>

#include "onion_nif_rsa.h"

ERL_NIF_TERM onion_nif_rsa_generate_private_key(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int bits = 0;
    unsigned int e = 0;

    BIGNUM *b_e = NULL;
    RSA *rsa = NULL;
    BIO *bio = NULL;
    int length = 0;

    ErlNifBinary pem_key;
    ERL_NIF_TERM result;

    if (argc != 2 || ! enif_get_uint(env, argv[0], &bits) || ! enif_get_uint(env, argv[1], &e))
        return enif_make_badarg(env);

    do
    {
        // Set e.
        b_e = BN_new();

        if (b_e == NULL)
        {
            result = make_error_tuple(env, "out_of_memory");
            continue;
        }

        if (! BN_set_word(b_e, e))
        {
            result = make_error_tuple(env, "bn_set_word");
            continue;
        }

        // Create RSA key.
        rsa = RSA_new();

        if (rsa == NULL)
        {
            result = make_error_tuple(env, "rsa_new");
            continue;
        }

        if (RSA_generate_key_ex(rsa, bits, b_e, NULL) == -1)
        {
            result = make_error_tuple(env, "rsa_error");
            continue;
        }

        // Initialize BIO.
        bio = BIO_new(BIO_s_mem());

        if (bio == NULL)
        {
            result = make_error_tuple(env, "out_of_memory");
            continue;
        }

        PEM_write_bio_RSAPrivateKey(bio, rsa, NULL, NULL, 0, NULL, NULL);
        length = BIO_pending(bio);

        if (! enif_alloc_binary(length, &pem_key))
        {
            result = make_error_tuple(env, "alloc_binary");
            continue;
        }

        BIO_read(bio, pem_key.data, length);

        result = enif_make_binary(env, &pem_key);
    } while (0);

    // Clean up.
    if (b_e != NULL)
        BN_clear_free(b_e);

    if (rsa != NULL)
        RSA_free(rsa);

    if (bio != NULL)
        BIO_free_all(bio);

    return result;
}
