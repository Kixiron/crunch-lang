sudo apt install build-essential binutils-dev libunwind-dev libblocksruntime-dev
cargo install honggfuzz
HFUZZ_RUN_ARGS="--threads=8 --linux_perf_instr --linux_perf_branch --timeout=2 --tmout_sigvtalrm --verifier"
cargo hfuzz run fuzz_parser --color=always --features fuzz
