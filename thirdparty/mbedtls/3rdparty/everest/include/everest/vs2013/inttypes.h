/*
 *  Custom inttypes.h for VS2010 KreMLin requires these definitions,
 *  but VS2010 doesn't provide them.
 *
 *  Copyright 2016-2018 INRIA and Microsoft Corporation
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
 *
 *  This file is part of Mbed TLS (https://tls.mbed.org)
 */

#ifndef _INTTYPES_H_VS2010
#define _INTTYPES_H_VS2010

#include <stdint.h>

#ifdef _MSC_VER
#define inline __inline
#endif

/* VS2010 unsigned long == 8 bytes */

#define PRIu64 "I64u"

#endif
