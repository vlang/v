// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module quic

// QUIC protocol implementation for HTTP/3
//
// This module provides two implementations:
// 1. ngtcp2-based (production-ready, requires ngtcp2 library)
// 2. Placeholder (fallback when ngtcp2 is not available)
//
// The implementation is automatically selected based on whether
// ngtcp2 is available at compile time.
//
// To use the ngtcp2 implementation:
// - macOS: brew install ngtcp2
// - Ubuntu: apt-get install libngtcp2-dev
// - Build from source: See QUIC_LIBRARY_EVALUATION.md

// Re-export the implementation from quic_ngtcp2.v
// If ngtcp2 is not available, this will use the placeholder implementation

// Note: The actual implementation is in quic_ngtcp2.v
// This file serves as the public API entry point
