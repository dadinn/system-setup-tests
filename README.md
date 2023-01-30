# system-setup-tests

End-to-end tests for Linux system-setup scripts, using Qemu/KVM virtual machines.

## Dependencies

Requires working Qemu and KVM installation, with `qemu-system-x86_64`
binary accessible on the `$PATH`.

Also, requires paths to OVMF firmware when testing UEFI bootable systems.

Requires `curl` or `transmission-cli` binaries accessible on `$PATH` to
potentially download Live ISO images for the Linux distributions under test.

Supports using package repository mirrors (currently apt / Debian only),
instead of network connection to official/built-in package repositories.

## Configuration

Test specifications are by default found under [./resources/specs](./resources/specs) directory.
An alternate path to the test-specs directory can be specified via CLI options.

Test specification group aliases can be defined by placing or symlinking specs in a directory (eg. for [current](./resources/specs/current) specs)

Specifications for downloading Live ISO images can be found at [./resources/isos.scm](./resources/isos.scm).

## Usage

The `run.scm` script is the main entrypoint for executing test specifications.

The `--data-path` stores static data, including downloaded Live ISO images, and synchronized repository mirrors.

The `--temp-path` stores temporary files created for each test executed for the given test run session.
This includes logs, Qemu disk images, and copy of OVMF vars file used to store UEFI boot options.

Network connection for installing packages can be enabled with `--use-network` CLI option,
but by default attempts to use a local mirror as package repository (currently apt/Debian only).

To synchronize package repository mirrors use the `--sync-mirror` CLI option.

For UEFI booting, the required OVMF firmare and vars files can be specified with the `--ovmf-code-file` and `--ovmf-vars-file` CLI options. By default expects to find these files under the `/usr/share/OVMF` directory.

To just execute the verification on an already installed VM image, reference as argument the given RUNID with the `--verify` CLI option.

Tests can be executed in parallel, on multiple VMs, using the `--parallel` CLI option. This obviously requires that resources are available on the host for running all the VM instances together.
The outputs are then only printed into log files, and nothing appears on standard output except messages indicating completions of individual test.
