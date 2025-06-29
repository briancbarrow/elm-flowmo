#!/bin/bash

echo "Watching for Elm file changes... Press Ctrl+C to stop"
echo "Compiling initially..."
elm make src/Main.elm --output=elm.js

LAST_MODIFIED=""

while true; do
    # Get the modification time of all .elm files
    CURRENT_MODIFIED=$(find src -name "*.elm" -exec stat -f "%m %N" {} \; | sort -nr | head -1)
    
    if [ "$CURRENT_MODIFIED" != "$LAST_MODIFIED" ]; then
        if [ ! -z "$LAST_MODIFIED" ]; then
            echo "Elm file changed, recompiling..."
            elm make src/Main.elm --output=elm.js
            if [ $? -eq 0 ]; then
                echo "✅ Compilation successful at $(date)"
            else
                echo "❌ Compilation failed at $(date)"
            fi
        fi
        LAST_MODIFIED="$CURRENT_MODIFIED"
    fi
    
    sleep 1
done
