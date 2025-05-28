#!/bin/bash

TARGET_DIR="."

if [ -n "$1" ]; then
    TARGET_DIR="$1"
fi

if [ ! -d "$TARGET_DIR" ]; then
    echo "Error: Directory '$TARGET_DIR' not found or is not a directory." >&2
    exit 1
fi

find "$TARGET_DIR" -maxdepth 1 -type f -name "*.swamp" | sort | while read -r file; do
    if [ -f "$file" ]; then
				echo "// ---------------------------------"
        echo "// $(basename "$file")"
				echo "// ---------------------------------"
        cat "$file"
        echo ""
    fi
done