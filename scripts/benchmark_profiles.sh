#!/bin/bash

# Simple benchmark script to compare different optimization profiles
# Usage: ./scripts/benchmark_profiles.sh

set -e

echo "🚀 Benchmarking different optimization profiles for swamp-build"
echo "================================================================="

# Test file - using a simple example
TEST_FILE="examples/main.swamp"

if [ ! -f "$TEST_FILE" ]; then
    echo "Warning: Test file $TEST_FILE not found, creating a simple one..."
    mkdir -p examples
    cat > "$TEST_FILE" << 'EOF'
let main = 42
EOF
fi

# Function to benchmark a specific executable
benchmark_executable() {
    local profile=$1
    local executable=$2
    local target_dir="/Users/peter/cargo-target"
    
    if [ ! -f "$target_dir/$profile/$executable" ]; then
        echo "❌ $profile/$executable not found, skipping..."
        return
    fi
    
    echo "📊 Testing $profile profile ($executable):"
    echo "   Size: $(ls -lah $target_dir/$profile/$executable | awk '{print $5}')"
    
    # Run 3 times and take average
    local total_time=0
    for i in {1..3}; do
        local start_time=$(date +%s.%N)
        "$target_dir/$profile/$executable" "$TEST_FILE" > /dev/null 2>&1 || true
        local end_time=$(date +%s.%N)
        local duration=$(echo "$end_time - $start_time" | bc -l)
        total_time=$(echo "$total_time + $duration" | bc -l)
    done
    
    local avg_time=$(echo "scale=4; $total_time / 3" | bc -l)
    echo "   Avg time: ${avg_time}s"
    echo ""
}

# Test different profiles
echo "Testing swamp-build executable:"
echo "==============================="
benchmark_executable "debug" "swamp-build"
benchmark_executable "release" "swamp-build"
benchmark_executable "fast" "swamp-build"
benchmark_executable "ultra-fast" "swamp-build"
benchmark_executable "min-size" "swamp-build"
benchmark_executable "ultra-min" "swamp-build"

echo "Summary:"
echo "========"
echo "For maximum SPEED: use --profile ultra-fast"
echo "For minimum SIZE:  use --profile ultra-min"
echo "For development:   use debug (default)"
echo "For production:    use --profile fast (good balance)" 