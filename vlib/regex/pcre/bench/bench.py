#!/usr/bin/env python3
import re
import time

ITERATIONS = 10000
WARMUP = 100

short_text = "The quick brown fox jumps over the lazy dog 12345"
medium_text = (
    "The quick brown fox jumps over the lazy dog 12345. "
    "Pack my box with five dozen liquor jugs. "
    "How vexingly quick daft zebras jump! "
    "The five boxing wizards jump quickly. "
    "Sphinx of black quartz, judge my vow. "
    "Two driven jocks help fax my big quiz."
)
long_text = medium_text * 20
email_text = (
    "Contact us at user@example.com or admin@test.org for info. "
    "Also try support@company.co.uk and test.user+tag@domain.com please."
)


def bench(name, func):
    for _ in range(WARMUP):
        func()
    start = time.perf_counter_ns()
    for _ in range(ITERATIONS):
        func()
    elapsed = time.perf_counter_ns() - start
    ns_per_op = elapsed // ITERATIONS
    ops_sec = 1e9 / ns_per_op if ns_per_op > 0 else 0
    return (name, ns_per_op, ops_sec)


results = []

# 1. Literal
p1 = re.compile(r"quick")
results.append(bench("1. Literal match (short)", lambda: p1.search(short_text)))

# 2. Char class
p2 = re.compile(r"[a-zA-Z]+")
results.append(bench("2. Char class [a-zA-Z]+", lambda: p2.search(medium_text)))

# 3. Alternation
p3 = re.compile(r"fox|dog|cat|bird")
results.append(bench("3. Alternation (4 words)", lambda: p3.search(short_text)))

# 4. Find all
p4 = re.compile(r"\d+")
results.append(bench("4. Find all digits", lambda: p4.findall(medium_text)))

# 5. Groups
p5 = re.compile(r"(\w+)\s+(\w+)")
results.append(bench("5. Groups (\\w+)\\s+(\\w+)", lambda: p5.search(medium_text)))

# 6. Email
p6 = re.compile(r"[\w.]+@[\w]+\.[\w]+")
results.append(bench("6. Email pattern", lambda: p6.findall(email_text)))

# 7. Long text
p7 = re.compile(r"quickly")
results.append(bench("7. Long text literal scan", lambda: p7.findall(long_text)))

# 8. Replace
p8 = re.compile(r"\d+")
results.append(bench("8. Replace all digits", lambda: p8.sub("NUM", medium_text)))

# 9. Dot-star
p9 = re.compile(r".*fox.*dog")
results.append(bench("9. Dot-star greedy", lambda: p9.match(short_text)))

# 10. Compile
results.append(bench("10. Compile simple", lambda: re.compile(r"\w+@\w+\.\w+")))

# 11. Lookahead
p11 = re.compile(r"\w+(?=\.)")
results.append(bench("11. Lookahead \\w+(?=\\.)", lambda: p11.search(email_text)))

# 12. Backreference
doubled = "the the quick quick fox fox jumps over the lazy dog"
p12 = re.compile(r"(\w+)\s+\1")
results.append(bench("12. Backref (\\w+)\\s+\\1", lambda: p12.findall(doubled)))

print("=" * 70)
print(f"{'':>15}PYTHON REGEX BENCHMARK (re module)")
print("=" * 70)
print(f"{'Benchmark':<35} {'ns/op':>12} {'ops/sec':>14}")
print("-" * 70)
for name, ns, ops in results:
    print(f"{name:<35} {ns:>12} {ops:>14.0f}")
print("=" * 70)
