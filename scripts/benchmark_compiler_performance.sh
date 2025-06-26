#!/bin/bash

# Swamp Compiler Performance Benchmark
# Tests how fast different optimized versions of the Swamp compiler compile Swamp source code
# Usage: ./scripts/benchmark_compiler_performance.sh

set -e

echo "🚀 Swamp Compiler Performance Benchmark"
echo "========================================"
echo ""

# Configuration
TARGET_DIR="/Users/peter/cargo-target"
TEST_PATH="crates/swamp/tests/fixtures/basic"
ITERATIONS=5

# Ensure test path exists
if [ ! -d "$TEST_PATH" ]; then
    echo "❌ Test path $TEST_PATH not found!"
    exit 1
fi

echo "📄 Test path: $TEST_PATH"
echo "🔄 Iterations per test: $ITERATIONS"
echo ""

# Function to benchmark Swamp compilation performance
benchmark_swamp_compiler() {
    local profile=$1
    local executable_path="$TARGET_DIR/$profile/swamp-build"
    
    if [ ! -f "$executable_path" ]; then
        echo "❌ $profile/swamp-build not found, skipping..."
        echo ""
        return
    fi
    
    echo "📊 Testing $profile profile:"
    echo "   Executable: $executable_path"
    echo "   Size: $(ls -lah "$executable_path" | awk '{print $5}')"
    echo "   Running $ITERATIONS iterations..."
    
    local times=()
    local total=0
    
    for i in $(seq 1 $ITERATIONS); do
        # Run the compiler and measure time
        local start_time=$(date +%s.%N)
        "$executable_path" --path "$TEST_PATH" > /dev/null 2>&1
        local end_time=$(date +%s.%N)
        
        local duration=$(echo "$end_time - $start_time" | bc -l)
        times+=($duration)
        total=$(echo "$total + $duration" | bc -l)
        
        printf "     Run %d: %.4fs\n" $i $duration
    done
    
    local average=$(echo "scale=4; $total / $ITERATIONS" | bc -l)
    local min_time=$(printf '%s\n' "${times[@]}" | sort -n | head -1)
    local max_time=$(printf '%s\n' "${times[@]}" | sort -n | tail -1)
    
    echo "   Results:"
    printf "     Average: %.4fs\n" $average
    printf "     Minimum: %.4fs\n" $min_time
    printf "     Maximum: %.4fs\n" $max_time
    
    # Calculate speedup compared to debug if this isn't debug
    if [ "$profile" != "debug" ] && [ -f "$TARGET_DIR/debug/swamp-build" ]; then
        # Get debug average (simplified - just run once for comparison)
        local debug_start=$(date +%s.%N)
        "$TARGET_DIR/debug/swamp-build" --path "$TEST_PATH" > /dev/null 2>&1
        local debug_end=$(date +%s.%N)
        local debug_time=$(echo "$debug_end - $debug_start" | bc -l)
        
        local speedup=$(echo "scale=2; $debug_time / $average" | bc -l)
        printf "     Speedup: %.2fx faster than debug\n" $speedup
    fi
    
    echo ""
}

echo "🎯 Testing with comprehensive Swamp test suite: $TEST_PATH"
echo "=========================================================="

# Test each profile
benchmark_swamp_compiler "debug"
benchmark_swamp_compiler "release"
benchmark_swamp_compiler "fast"
benchmark_swamp_compiler "ultra-fast"
benchmark_swamp_compiler "min-size"
benchmark_swamp_compiler "ultra-min"

echo ""
echo "📈 Summary & Recommendations:"
echo "============================="
echo "• DEBUG: Slowest compilation, largest binary (development)"
echo "• RELEASE: Standard Rust optimizations"
echo "• FAST: Optimized for speed, good balance"
echo "• ULTRA-FAST: Maximum speed optimizations"
echo "• MIN-SIZE: Smallest binary, still fast compilation"
echo "• ULTRA-MIN: Absolute minimum binary size"
echo ""
echo "🏆 For production Swamp compilation: Use --profile ultra-fast"
echo "💾 For distribution: Use --profile ultra-min"
echo "🔧 For development: Use debug profile" 