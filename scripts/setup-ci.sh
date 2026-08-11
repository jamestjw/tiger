#!/bin/sh -eux

# Make sure opam is setup in your environment.
eval `opam config env`
opam update

# Install the repository's development and test dependencies. This keeps the
# generated _opam source copy out of dependency discovery.
opam install . --deps-only --with-test -y
eval `opam config env`

# Extra dependencies
opam install ocamlformat -y

# Download the prebuilt test tools produced by the release workflow.
export RISCV=/opt/riscv
export PATH="$RISCV/bin:$RISCV/riscv64-unknown-elf/bin:$PATH"

if [ -x "$RISCV/bin/riscv64-unknown-elf-gcc" ] \
  && [ -x "$RISCV/bin/spike" ] \
  && [ -x "$RISCV/riscv64-unknown-elf/bin/pk" ]; then
    echo "RISC-V test tools already installed."
else
    : "${RISC_V_TOOLS_URL:?RISC_V_TOOLS_URL must point to the RISC-V tools release asset}"
    curl --fail --location "$RISC_V_TOOLS_URL" | sudo tar -xz -C /opt
fi
sudo ln -sf /opt/riscv/bin/riscv64-unknown-elf-gcc /usr/local/bin/riscv64-unknown-elf-gcc
sudo ln -sf /opt/riscv/bin/spike /usr/local/bin/spike
