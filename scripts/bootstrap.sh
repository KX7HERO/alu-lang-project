#!/usr/bin/env bash
set -euo pipefail

# Pre-fetch all Maven dependencies (Rascal + TypePal) so later commands are fast.
mvn -q -e dependency:go-offline
