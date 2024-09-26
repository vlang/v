/**
 *  Modular bignum functions
 *
 *  Copyright The Mbed TLS Contributors
 *  SPDX-License-Identifier: Apache-2.0
 *
 *  Licensed under the Apache License, Version 2.0 (the "License"); you may
 *  not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 *  WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

#include "common.h"

#if defined(MBEDTLS_BIGNUM_C)

#include <string.h>

#include "mbedtls/platform_util.h"
#include "mbedtls/error.h"
#include "mbedtls/bignum.h"

#include "mbedtls/platform.h"

#include "bignum_core.h"
#include "bignum_mod.h"
#include "bignum_mod_raw.h"
#include "constant_time_internal.h"

int mbedtls_mpi_mod_residue_setup( mbedtls_mpi_mod_residue *r,
                                   const mbedtls_mpi_mod_modulus *m,
                                   mbedtls_mpi_uint *p,
                                   size_t p_limbs )
{
    if( p_limbs != m->limbs || !mbedtls_mpi_core_lt_ct( p, m->p, m->limbs ) )
        return( MBEDTLS_ERR_MPI_BAD_INPUT_DATA );

    r->limbs = m->limbs;
    r->p = p;

    return( 0 );
}

void mbedtls_mpi_mod_residue_release( mbedtls_mpi_mod_residue *r )
{
    if( r == NULL )
        return;

    r->limbs = 0;
    r->p = NULL;
}

void mbedtls_mpi_mod_modulus_init( mbedtls_mpi_mod_modulus *m )
{
    if( m == NULL )
        return;

    m->p = NULL;
    m->limbs = 0;
    m->bits = 0;
    m->int_rep = MBEDTLS_MPI_MOD_REP_INVALID;
}

void mbedtls_mpi_mod_modulus_free( mbedtls_mpi_mod_modulus *m )
{
    if( m == NULL )
        return;

    switch( m->int_rep )
    {
        case MBEDTLS_MPI_MOD_REP_MONTGOMERY:
            if (m->rep.mont.rr != NULL)
            {
                mbedtls_platform_zeroize( (mbedtls_mpi_uint *) m->rep.mont.rr,
                                           m->limbs );
                mbedtls_free( (mbedtls_mpi_uint *)m->rep.mont.rr );
                m->rep.mont.rr = NULL;
            }
            m->rep.mont.mm = 0;
            break;
        case MBEDTLS_MPI_MOD_REP_OPT_RED:
            mbedtls_free( m->rep.ored );
            break;
        case MBEDTLS_MPI_MOD_REP_INVALID:
            break;
    }

    m->p = NULL;
    m->limbs = 0;
    m->bits = 0;
    m->int_rep = MBEDTLS_MPI_MOD_REP_INVALID;
}

static int set_mont_const_square( const mbedtls_mpi_uint **X,
                                  const mbedtls_mpi_uint *A,
                                  size_t limbs )
{
    int ret = MBEDTLS_ERR_ERROR_CORRUPTION_DETECTED;
    mbedtls_mpi N;
    mbedtls_mpi RR;
    *X = NULL;

    mbedtls_mpi_init( &N );
    mbedtls_mpi_init( &RR );

    if( A == NULL || limbs == 0 || limbs >= ( MBEDTLS_MPI_MAX_LIMBS / 2 ) - 2 )
        goto cleanup;

    if( mbedtls_mpi_grow( &N, limbs ) )
        goto cleanup;

    memcpy( N.p, A, sizeof(mbedtls_mpi_uint) * limbs );

    ret = mbedtls_mpi_core_get_mont_r2_unsafe(&RR, &N);

    if( ret == 0 )
    {
        *X = RR.p;
        RR.p = NULL;
    }

cleanup:
    mbedtls_mpi_free(&N);
    mbedtls_mpi_free(&RR);
    ret = ( ret != 0 ) ? MBEDTLS_ERR_ERROR_CORRUPTION_DETECTED : 0;
    return( ret );
}

int mbedtls_mpi_mod_modulus_setup( mbedtls_mpi_mod_modulus *m,
                                   const mbedtls_mpi_uint *p,
                                   size_t p_limbs,
                                   mbedtls_mpi_mod_rep_selector int_rep )
{
    int ret = 0;

    m->p = p;
    m->limbs = p_limbs;
    m->bits = mbedtls_mpi_core_bitlen( p, p_limbs );

    switch( int_rep )
    {
        case MBEDTLS_MPI_MOD_REP_MONTGOMERY:
            m->int_rep = int_rep;
            m->rep.mont.mm = mbedtls_mpi_core_montmul_init( m->p );
            ret = set_mont_const_square( &m->rep.mont.rr, m->p, m->limbs );
            break;
        case MBEDTLS_MPI_MOD_REP_OPT_RED:
            m->int_rep = int_rep;
            m->rep.ored = NULL;
            break;
        default:
            ret = MBEDTLS_ERR_MPI_BAD_INPUT_DATA;
            goto exit;
    }

exit:

    if( ret != 0 )
    {
        mbedtls_mpi_mod_modulus_free( m );
    }

    return( ret );
}

/* BEGIN MERGE SLOT 1 */

/* END MERGE SLOT 1 */

/* BEGIN MERGE SLOT 2 */

/* END MERGE SLOT 2 */

/* BEGIN MERGE SLOT 3 */
int mbedtls_mpi_mod_sub( mbedtls_mpi_mod_residue *X,
                         const mbedtls_mpi_mod_residue *A,
                         const mbedtls_mpi_mod_residue *B,
                         const mbedtls_mpi_mod_modulus *N )
{
    if( X->limbs != N->limbs || A->limbs != N->limbs || B->limbs != N->limbs )
        return( MBEDTLS_ERR_MPI_BAD_INPUT_DATA );

    mbedtls_mpi_mod_raw_sub( X->p, A->p, B->p, N );

    return( 0 );
}
/* END MERGE SLOT 3 */

/* BEGIN MERGE SLOT 4 */

/* END MERGE SLOT 4 */

/* BEGIN MERGE SLOT 5 */

/* END MERGE SLOT 5 */

/* BEGIN MERGE SLOT 6 */

/* END MERGE SLOT 6 */

/* BEGIN MERGE SLOT 7 */
int mbedtls_mpi_mod_read( mbedtls_mpi_mod_residue *r,
                          const mbedtls_mpi_mod_modulus *m,
                          const unsigned char *buf,
                          size_t buflen,
                          mbedtls_mpi_mod_ext_rep ext_rep )
{
    int ret = MBEDTLS_ERR_MPI_BAD_INPUT_DATA;

    /* Do our best to check if r and m have been set up */
    if( r->limbs == 0 || m->limbs == 0 )
        goto cleanup;
    if( r->limbs != m->limbs )
        goto cleanup;

    ret = mbedtls_mpi_mod_raw_read( r->p, m, buf, buflen, ext_rep );
    if( ret != 0 )
        goto cleanup;

    r->limbs = m->limbs;

    if( m->int_rep == MBEDTLS_MPI_MOD_REP_MONTGOMERY )
       ret = mbedtls_mpi_mod_raw_to_mont_rep( r->p, m );

cleanup:
    return ( ret );
}

int mbedtls_mpi_mod_write( const mbedtls_mpi_mod_residue *r,
                           const mbedtls_mpi_mod_modulus *m,
                           unsigned char *buf,
                           size_t buflen,
                           mbedtls_mpi_mod_ext_rep ext_rep )
{
    int ret = MBEDTLS_ERR_MPI_BAD_INPUT_DATA;

    /* Do our best to check if r and m have been set up */
    if( r->limbs == 0 || m->limbs == 0 )
        goto cleanup;
    if( r->limbs != m->limbs )
        goto cleanup;

    if( m->int_rep == MBEDTLS_MPI_MOD_REP_MONTGOMERY )
    {
        ret = mbedtls_mpi_mod_raw_from_mont_rep( r->p, m );
        if( ret != 0 )
            goto cleanup;
    }

    ret = mbedtls_mpi_mod_raw_write( r->p, m, buf, buflen, ext_rep );

    if( m->int_rep == MBEDTLS_MPI_MOD_REP_MONTGOMERY )
    {
        /* If this fails, the value of r is corrupted and we want to return
         * this error (as opposed to the error code from the write above) to
         * let the caller know. If it succeeds, we want to return the error
         * code from write above. */
        int conv_ret = mbedtls_mpi_mod_raw_to_mont_rep( r->p, m );
        if( ret == 0 )
            ret = conv_ret;
    }

cleanup:

    return ( ret );
}
/* END MERGE SLOT 7 */

/* BEGIN MERGE SLOT 8 */

/* END MERGE SLOT 8 */

/* BEGIN MERGE SLOT 9 */

/* END MERGE SLOT 9 */

/* BEGIN MERGE SLOT 10 */

/* END MERGE SLOT 10 */

#endif /* MBEDTLS_BIGNUM_C */
