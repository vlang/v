/**
 *  Modular bignum functions
 *
 * This module implements operations on integers modulo some fixed modulus.
 */

/*
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

#ifndef MBEDTLS_BIGNUM_MOD_H
#define MBEDTLS_BIGNUM_MOD_H

#include "common.h"

#if defined(MBEDTLS_BIGNUM_C)
#include "mbedtls/bignum.h"
#endif

/* Skip 1 as it is slightly easier to accidentally pass to functions. */
typedef enum
{
    MBEDTLS_MPI_MOD_REP_INVALID    = 0,
    MBEDTLS_MPI_MOD_REP_MONTGOMERY = 2,
    MBEDTLS_MPI_MOD_REP_OPT_RED
} mbedtls_mpi_mod_rep_selector;

/* Make mbedtls_mpi_mod_rep_selector and mbedtls_mpi_mod_ext_rep disjoint to
 * make it easier to catch when they are accidentally swapped. */
typedef enum
{
    MBEDTLS_MPI_MOD_EXT_REP_INVALID = 0,
    MBEDTLS_MPI_MOD_EXT_REP_LE      = 8,
    MBEDTLS_MPI_MOD_EXT_REP_BE
} mbedtls_mpi_mod_ext_rep;

typedef struct
{
    mbedtls_mpi_uint *p;
    size_t limbs;
} mbedtls_mpi_mod_residue;

typedef struct {
    mbedtls_mpi_uint const *rr;  /* The residue for 2^{2*n*biL} mod N */
    mbedtls_mpi_uint mm;         /* Montgomery const for -N^{-1} mod 2^{ciL} */
} mbedtls_mpi_mont_struct;

typedef void *mbedtls_mpi_opt_red_struct;

typedef struct {
    const mbedtls_mpi_uint *p;
    size_t limbs;                            // number of limbs
    size_t bits;                             // bitlen of p
    mbedtls_mpi_mod_rep_selector int_rep;    // selector to signal the active member of the union
    union rep
    {
        mbedtls_mpi_mont_struct mont;
        mbedtls_mpi_opt_red_struct ored;
    } rep;
} mbedtls_mpi_mod_modulus;

/** Setup a residue structure.
 *
 * The residue will be set up with the buffer \p p and modulus \p m.
 *
 * The memory pointed to by \p p will be used by the resulting residue structure.
 * The value at the pointed-to memory will be the initial value of \p r and must
 * hold a value that is less than the modulus. This value will be used as-is
 * and interpreted according to the value of the `m->int_rep` field.
 *
 * The modulus \p m will be the modulus associated with \p r. The residue \p r
 * should only be used in operations where the modulus is \p m.
 *
 * \param[out] r    The address of the residue to setup.
 * \param[in] m     The address of the modulus related to \p r.
 * \param[in] p     The address of the limb array containing the value of \p r.
 *                  The memory pointed to by \p p will be used by \p r and must
 *                  not be modified in any way until after
 *                  mbedtls_mpi_mod_residue_release() is called. The data
 *                  pointed to by \p p must be less than the modulus (the value
 *                  pointed to by `m->p`) and already in the representation
 *                  indicated by `m->int_rep`.
 * \param p_limbs   The number of limbs of \p p. Must be the same as the number
 *                  of limbs in the modulus \p m.
 *
 * \return      \c 0 if successful.
 * \return      #MBEDTLS_ERR_MPI_BAD_INPUT_DATA if \p p_limbs is less than the
 *              limbs in \p m or if \p p is not less than \p m.
 */
int mbedtls_mpi_mod_residue_setup( mbedtls_mpi_mod_residue *r,
                                   const mbedtls_mpi_mod_modulus *m,
                                   mbedtls_mpi_uint *p,
                                   size_t p_limbs );

/** Unbind elements of a residue structure.
 *
 * This function removes the reference to the limb array that was passed to
 * mbedtls_mpi_mod_residue_setup() to make it safe to free or use again.
 *
 * This function invalidates \p r and it must not be used until after
 * mbedtls_mpi_mod_residue_setup() is called on it again.
 *
 * \param[out] r     The address of residue to release.
 */
void mbedtls_mpi_mod_residue_release( mbedtls_mpi_mod_residue *r );

/** Initialize a modulus structure.
 *
 * \param[out] m     The address of the modulus structure to initialize.
 */
void mbedtls_mpi_mod_modulus_init( mbedtls_mpi_mod_modulus *m );

/** Setup a modulus structure.
 *
 * \param[out] m    The address of the modulus structure to populate.
 * \param[in] p     The address of the limb array storing the value of \p m.
 *                  The memory pointed to by \p p will be used by \p m and must
 *                  not be modified in any way until after
 *                  mbedtls_mpi_mod_modulus_free() is called.
 * \param p_limbs   The number of limbs of \p p.
 * \param int_rep   The internal representation to be used for residues
 *                  associated with \p m (see #mbedtls_mpi_mod_rep_selector).
 *
 * \return      \c 0 if successful.
 * \return      #MBEDTLS_ERR_MPI_BAD_INPUT_DATA if \p int_rep is invalid.
 */
int mbedtls_mpi_mod_modulus_setup( mbedtls_mpi_mod_modulus *m,
                                   const mbedtls_mpi_uint *p,
                                   size_t p_limbs,
                                   mbedtls_mpi_mod_rep_selector int_rep );

/** Free elements of a modulus structure.
 *
 * This function frees any memory allocated by mbedtls_mpi_mod_modulus_setup().
 *
 * \warning This function does not free the limb array passed to
 *          mbedtls_mpi_mod_modulus_setup() only removes the reference to it,
 *          making it safe to free or to use it again.
 *
 * \param[in,out] m     The address of the modulus structure to free.
 */
void mbedtls_mpi_mod_modulus_free( mbedtls_mpi_mod_modulus *m );

/* BEGIN MERGE SLOT 1 */

/* END MERGE SLOT 1 */

/* BEGIN MERGE SLOT 2 */

/* END MERGE SLOT 2 */

/* BEGIN MERGE SLOT 3 */
/**
 * \brief Perform a fixed-size modular subtraction.
 *
 * Calculate `A - B modulo N`.
 *
 * \p A, \p B and \p X must all have the same number of limbs as \p N.
 *
 * \p X may be aliased to \p A or \p B, or even both, but may not overlap
 * either otherwise.
 *
 * \note This function does not check that \p A or \p B are in canonical
 *       form (that is, are < \p N) - that will have been done by
 *       mbedtls_mpi_mod_residue_setup().
 *
 * \param[out] X    The address of the result MPI. Must be initialized.
 *                  Must have the same number of limbs as the modulus \p N.
 * \param[in]  A    The address of the first MPI.
 * \param[in]  B    The address of the second MPI.
 * \param[in]  N    The address of the modulus. Used to perform a modulo
 *                  operation on the result of the subtraction.
 *
 * \return          \c 0 if successful.
 * \return          #MBEDTLS_ERR_MPI_BAD_INPUT_DATA if the given MPIs do not
 *                  have the correct number of limbs.
 */
int mbedtls_mpi_mod_sub( mbedtls_mpi_mod_residue *X,
                         const mbedtls_mpi_mod_residue *A,
                         const mbedtls_mpi_mod_residue *B,
                         const mbedtls_mpi_mod_modulus *N );
/* END MERGE SLOT 3 */

/* BEGIN MERGE SLOT 4 */

/* END MERGE SLOT 4 */

/* BEGIN MERGE SLOT 5 */

/* END MERGE SLOT 5 */

/* BEGIN MERGE SLOT 6 */

/* END MERGE SLOT 6 */

/* BEGIN MERGE SLOT 7 */
/** Read a residue from a byte buffer.
 *
 * The residue will be automatically converted to the internal representation
 * based on the value of the `m->int_rep` field.
 *
 * The modulus \p m will be the modulus associated with \p r. The residue \p r
 * should only be used in operations where the modulus is \p m or a modulus
 * equivalent to \p m (in the sense that all their fields or memory pointed by
 * their fields hold the same value).
 *
 * \param[out] r    The address of the residue. It must have exactly the same
 *                  number of limbs as the modulus \p m.
 * \param[in] m     The address of the modulus.
 * \param[in] buf   The input buffer to import from.
 * \param buflen    The length in bytes of \p buf.
 * \param ext_rep   The endianness of the number in the input buffer.
 *
 * \return       \c 0 if successful.
 * \return       #MBEDTLS_ERR_MPI_BUFFER_TOO_SMALL if \p r isn't
 *               large enough to hold the value in \p buf.
 * \return       #MBEDTLS_ERR_MPI_BAD_INPUT_DATA if \p ext_rep
 *               is invalid or the value in the buffer is not less than \p m.
 */
int mbedtls_mpi_mod_read( mbedtls_mpi_mod_residue *r,
                          const mbedtls_mpi_mod_modulus *m,
                          const unsigned char *buf,
                          size_t buflen,
                          mbedtls_mpi_mod_ext_rep ext_rep );

/** Write a residue into a byte buffer.
 *
 * The modulus \p m must be the modulus associated with \p r (see
 * mbedtls_mpi_mod_residue_setup() and mbedtls_mpi_mod_read()).
 *
 * The residue will be automatically converted from the internal representation
 * based on the value of `m->int_rep` field.
 *
 * \warning     If the buffer is smaller than `m->bits`, the number of
 *              leading zeroes is leaked through timing. If \p r is
 *              secret, the caller must ensure that \p buflen is at least
 *              (`m->bits`+7)/8.
 *
 * \param[in] r     The address of the residue. It must have the same number of
 *                  limbs as the modulus \p m. (\p r is an input parameter, but
 *                  its value will be modified during execution and restored
 *                  before the function returns.)
 * \param[in] m     The address of the modulus associated with \r.
 * \param[out] buf  The output buffer to export to.
 * \param buflen    The length in bytes of \p buf.
 * \param ext_rep   The endianness in which the number should be written into
 *                  the output buffer.
 *
 * \return       \c 0 if successful.
 * \return       #MBEDTLS_ERR_MPI_BUFFER_TOO_SMALL if \p buf isn't
 *               large enough to hold the value of \p r (without leading
 *               zeroes).
 * \return       #MBEDTLS_ERR_MPI_BAD_INPUT_DATA if \p ext_rep is invalid.
 * \return       #MBEDTLS_ERR_MPI_ALLOC_FAILED if couldn't allocate enough
 *               memory for conversion. Can occur only for moduli with
 *               MBEDTLS_MPI_MOD_REP_MONTGOMERY.
 */
int mbedtls_mpi_mod_write( const mbedtls_mpi_mod_residue *r,
                           const mbedtls_mpi_mod_modulus *m,
                           unsigned char *buf,
                           size_t buflen,
                           mbedtls_mpi_mod_ext_rep ext_rep );
/* END MERGE SLOT 7 */

/* BEGIN MERGE SLOT 8 */

/* END MERGE SLOT 8 */

/* BEGIN MERGE SLOT 9 */

/* END MERGE SLOT 9 */

/* BEGIN MERGE SLOT 10 */

/* END MERGE SLOT 10 */

#endif /* MBEDTLS_BIGNUM_MOD_H */
