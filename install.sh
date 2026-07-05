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

# juju requires the shared ui-utils.hx library; its modules must be in
# ~/.steel/cogs/ui-utils.hx/ or juju fails to load. Install from a sibling
# checkout (override with UI_UTILS_DIR), else a shallow clone.
if [ ! -d "$HOME/.steel/cogs/ui-utils.hx" ]; then
    UI_UTILS_DIR="${UI_UTILS_DIR:-../ui-utils.hx}"
    if [ -d "$UI_UTILS_DIR" ]; then
        info "Installing ui-utils.hx from $UI_UTILS_DIR..."
        (cd "$UI_UTILS_DIR" && ./install.sh) >/dev/null
    else
        TMP_DIR=$(mktemp -d)
        info "Cloning ui-utils.hx..."
        git clone --depth 1 https://github.com/waddie/ui-utils.hx "$TMP_DIR/ui-utils.hx" >/dev/null 2>&1 \
            || error "ui-utils.hx is not installed and could not be cloned. Set UI_UTILS_DIR to a checkout and re-run."
        (cd "$TMP_DIR/ui-utils.hx" && ./install.sh) >/dev/null
        rm -rf "$TMP_DIR"
    fi
    success "Installed ui-utils.hx"
else
    success "ui-utils.hx already installed"
fi

# juju also requires the shared run-command library (the spawn/capture core
# behind run-vcs); its module must be in ~/.steel/cogs/run-command/ or juju
# fails to load. It ships no install.sh, so copy its files into place. Install
# from a sibling checkout (override with RUN_COMMAND_DIR), else a shallow clone.
# The destination directory is `run-command` (the require path), not the repo
# name `run-command.scm`.
if [ ! -d "$HOME/.steel/cogs/run-command" ]; then
    RUN_COMMAND_DIR="${RUN_COMMAND_DIR:-../run-command.scm}"
    if [ -d "$RUN_COMMAND_DIR" ]; then
        info "Installing run-command from $RUN_COMMAND_DIR..."
        SRC="$RUN_COMMAND_DIR"
    else
        TMP_DIR=$(mktemp -d)
        info "Cloning run-command..."
        git clone --depth 1 https://github.com/waddie/run-command.scm "$TMP_DIR/run-command.scm" >/dev/null 2>&1 \
            || error "run-command is not installed and could not be cloned. Set RUN_COMMAND_DIR to a checkout and re-run."
        SRC="$TMP_DIR/run-command.scm"
    fi
    mkdir -p "$HOME/.steel/cogs/run-command"
    cp "$SRC/run-command.scm" "$SRC/cog.scm" "$HOME/.steel/cogs/run-command/"
    [ -n "${TMP_DIR:-}" ] && rm -rf "$TMP_DIR"
    unset TMP_DIR
    success "Installed run-command"
else
    success "run-command already installed"
fi

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
