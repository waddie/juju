#!/usr/bin/env bash
# juju installation script.
#
# Copies the Scheme plugin into ~/.steel/cogs/juju/ - the same location Steel's
# forge package manager uses - so `(require "juju/juju.scm")` resolves the same
# way whether installed via forge or this script. juju shells out to git/jj and
# has no native component, so there is no dylib build or download step
# (cf. nrepl.hx; this mirrors the pure-Scheme paredit.hx).

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

error()   { echo -e "${RED}Error: $1${NC}" >&2; exit 1; }
success() { echo -e "${GREEN}✓ $1${NC}"; }
info()    { echo -e "${YELLOW}→ $1${NC}"; }

# Run from the repository root.
[ -f "juju.scm" ]  || error "juju.scm not found. Run this from the juju repository root."
[ -d "cogs/juju" ] || error "cogs/juju not found. Run this from the juju repository root."

DEST="$HOME/.steel/cogs/juju"
HELIX_DIR="$HOME/.config/helix"

info "Installing into $DEST..."
mkdir -p "$DEST/cogs"
cp juju.scm "$DEST/"
cp -r cogs/juju "$DEST/cogs/"
success "Installed juju.scm and cogs/juju"

# Check for git/jj on PATH.
command -v git >/dev/null 2>&1 && success "git found on PATH" || info "git not on PATH (the git backend will be unavailable)"
command -v jj  >/dev/null 2>&1 && success "jj found on PATH"  || info "jj not on PATH (the jj backend will be unavailable)"

INIT_SCM="$HELIX_DIR/init.scm"
if [ -f "$INIT_SCM" ] && grep -q 'juju/juju.scm' "$INIT_SCM"; then
    success "init.scm already requires juju"
else
    echo ""
    info "Add this line to $INIT_SCM:"
    echo ""
    echo "    (require \"juju/juju.scm\")"
    echo ""
fi

echo ""
info "For suggested keybindings, see keybindings-example.scm"
echo ""

success "Installation complete. Restart Helix, then run :juju"
