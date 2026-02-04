#!/bin/bash
# Integration Test Runner for HTTP/2 and HTTP/3
# This script runs all integration tests and generates a report

echo "=== HTTP/2 & HTTP/3 Integration Test Suite ==="
echo ""

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test results
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Function to run a test
run_test() {
	local test_name=$1
	local test_command=$2

	echo -n "Running ${test_name}... "
	TOTAL_TESTS=$((TOTAL_TESTS + 1))

	if eval "$test_command" >/tmp/test_output.log 2>&1; then
		echo -e "${GREEN}✓ PASSED${NC}"
		PASSED_TESTS=$((PASSED_TESTS + 1))
	else
		echo -e "${RED}✗ FAILED${NC}"
		FAILED_TESTS=$((FAILED_TESTS + 1))
		echo "  Error output:"
		cat /tmp/test_output.log | head -10
	fi
}

# Check prerequisites
echo "Checking prerequisites..."
echo ""

# Check V compiler
if ! command -v v &>/dev/null; then
	echo -e "${RED}✗ V compiler not found${NC}"
	exit 1
fi
echo -e "${GREEN}✓ V compiler found${NC}"

# Check ngtcp2
if pkg-config --exists libngtcp2 2>/dev/null; then
	echo -e "${GREEN}✓ ngtcp2 found${NC}"
else
	echo -e "${YELLOW}⚠ ngtcp2 not found (HTTP/3 tests will be skipped)${NC}"
fi

# Check OpenSSL
if command -v openssl &>/dev/null; then
	echo -e "${GREEN}✓ OpenSSL found${NC}"
else
	echo -e "${YELLOW}⚠ OpenSSL not found${NC}"
fi

echo ""
echo "=== Running Unit Tests ==="
echo ""

# HTTP/2 unit tests
run_test "HTTP/2 Frame Tests" "v test vlib/net/http/v2/frame_test.v"
run_test "HTTP/2 HPACK Tests" "v test vlib/net/http/v2/hpack_test.v"

# HTTP/3 unit tests
run_test "HTTP/3 Tests" "v test vlib/net/http/v3/v3_test.v"

# Version negotiation tests
run_test "Version Negotiation Tests" "v test vlib/net/http/version_test.v"

# QUIC tests
run_test "QUIC ngtcp2 Tests" "v test vlib/net/quic/ngtcp2_test.v"

echo ""
echo "=== Running Integration Tests ==="
echo ""

# HTTP/2 integration tests
run_test "HTTP/2 Client Integration" "v test vlib/net/http/v2/integration_test.v"
run_test "HTTP/2 Server Integration" "v test vlib/net/http/v2/server_integration_test.v"

echo ""
echo "=== Running Performance Tests ==="
echo ""

# Performance profiler
echo "Running performance profiler..."
v run vlib/net/http/performance_profiler.v >/tmp/performance_report.txt 2>&1
if [ $? -eq 0 ]; then
	echo -e "${GREEN}✓ Performance profiling completed${NC}"
	echo "  Report saved to /tmp/performance_report.txt"
else
	echo -e "${RED}✗ Performance profiling failed${NC}"
fi

echo ""
echo "=== Test Summary ==="
echo ""
echo "Total tests: ${TOTAL_TESTS}"
echo -e "Passed: ${GREEN}${PASSED_TESTS}${NC}"
echo -e "Failed: ${RED}${FAILED_TESTS}${NC}"

if [ $FAILED_TESTS -eq 0 ]; then
	echo ""
	echo -e "${GREEN}=== All tests passed! ===${NC}"
	exit 0
else
	echo ""
	echo -e "${RED}=== Some tests failed ===${NC}"
	exit 1
fi
