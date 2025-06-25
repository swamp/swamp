#!/bin/bash

echo "Checking which crates have never been published to crates.io:"
echo "============================================================"

cargo metadata --format-version 1 | jq -r '.workspace_members[]' | while read -r member; do
    # Extract everything after the # symbol
    after_hash=$(echo "$member" | cut -d'#' -f2)

    # Check if it contains @ (format: name@version)
    if [[ "$after_hash" == *"@"* ]]; then
        # Extract name part before @
        crate_name=$(echo "$after_hash" | cut -d'@' -f1)
    else
        # It's just a version number, extract crate name from path
        # Get the directory name from the path part
        path_part=$(echo "$member" | cut -d'#' -f1)
        crate_name=$(basename "$path_part")
    fi

    echo "Checking: $crate_name"

    http_status=$(curl -s -o /dev/null -w "%{http_code}" "https://crates.io/api/v1/crates/$crate_name")

    if [ "$http_status" = "404" ]; then
        echo "❌ $crate_name - Never published"
    elif [ "$http_status" = "200" ]; then
        # echo "✅ $crate_name - Published"
    else
        echo "⚠️  $crate_name - API error (HTTP $http_status)"
    fi
done